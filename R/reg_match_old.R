example = function() {
  parent.dir = "~/statabox/supp"
  repbox_match_all(parent.dir)

  project.dir = "/home/rstudio/statabox/supp/aejapp_vol_6_issue_3_article_7"
  res = repbox_match(project.dir,force = TRUE)
  sapply(names(res), function(na) {
    paste0(na, " ", format(object.size(res[[na]]), units="MB"))
  })

  dirs = list.dirs(path=parent.dir, full.names = TRUE,recursive = FALSE)
  for (dir in dirs) {
    if (file.exists(paste0(dir,"/repbox/matched_tabs.Rds")))
      cat("\n",dir)
  }

}


old.reg.match = function(project.dir, verbose=TRUE) {
  restore.point("repbox.reg.match")
  repbox.dir = paste0(project.dir,"/repbox")

  pdf.tabs = readRDS(paste0(repbox.dir, "/arttab.Rds"))
  pdf.tabs = add.reg.df.to.tabs.df(pdf.tabs)
  pdf.tabs = filter(pdf.tabs, has.reg.df)
  if (NROW(pdf.tabs)==0) {
    cat(paste0("\n\tNo regression tables found in PDF of project ", project.dir))
    return(NULL)
  }

  regtab = readRDS(paste0(repbox.dir, "/stata/regtab.Rds"))
  if (NROW(regtab)==0) {
    if (verbose) {
      cat(paste0("\n\tNo regressions were run for project ", project.dir))
      return(NULL)
    }
  }

  regtab$creg.num = seq_len(NROW(regtab))
  creg.df = regtab %>%
    tidyr::unnest(c(ct)) %>%
    mutate(creg.row = seq_len(n()))
  if (NROW(creg.df)==0) {
    if (verbose) {
      cat(paste0("\n\tNo regression results obtained when running supplement for project ", project.dir))
      return(invisible())
    }
  }

  preg.df = bind_rows(pdf.tabs$reg.df) %>%
    ungroup() %>%
    mutate(
      preg.row = seq_len(n())
    ) %>%
    group_by(tabid, col) %>%
    mutate(col.num.coef = n()) %>%
    ungroup() %>%
    left_join(select(pdf.tabs, tabid, table.title, tabname, tpname), by="tabid") %>%
    group_by(tabid, col) %>%
    mutate(col.num.coef = n()) %>%
    ungroup()



  mc = find.all.coef.matchings(preg.df, creg.df) %>% as.data.frame()

  # Map all matched coefficients from pdf and supplement
  mdf =
    left_join(preg.df, mc, by=c("preg.row")) %>%
    left_join(creg.df, by="creg.row")

  # 3 Versions for interpretation of value in ( ): se, t or p
  mdf = bind_rows(
    mutate(mdf, par.type = "se", creg.par = se),
    mutate(mdf, par.type = "t", creg.par = t),
    mutate(mdf, par.type = "se", creg.par = p)
  )

  # Compute match of coeficient and parenthesis
  mdf = mdf %>% mutate(
    coef.diff = abs(pdf.coef-coef),
    round.coef.diff = round(coef.diff, coef.deci),
    par.diff = abs(pdf.par-creg.par),
    round.par.diff = round(par.diff, par.deci)
  )

  # Keep only perfect matches
  mdf = filter(mdf, round.coef.diff==0, round.par.diff==0)

  # Store for each pdf regression (tabid, col) and possible match (creg.num, par.type)
  # information of how many rows (coefficient & parenthesis) match
  col.mdf = mdf %>%
    group_by(tabid, col, creg.num, par.type) %>%
    summarize(
      col.num.coef = first(col.num.coef),
      col.num.match = n_distinct(preg.row),
      col.share.match = col.num.match / col.num.coef
    ) %>%
    group_by(tabid, col) %>%
    mutate(
      col.best.num.match = suppressWarnings(max(col.num.match, na.rm = TRUE)),
      col.best.share.match = suppressWarnings(max(col.share.match, na.rm = TRUE))
    ) %>%
    ungroup()

  # Map match information for complete regression table column back
  mdf = mdf %>%
    left_join(select(col.mdf, tabid, col,creg.num,par.type, col.share.match, col.num.match, col.best.num.match, col.best.share.match),by=c("tabid","col","creg.num", "par.type"))


  best.col.row.df = mdf %>%
    group_by(tabid, col, preg.row) %>%
    summarize(
      col.best.num.match = first(col.best.num.match),
      col.best.share.match = first(col.best.share.match)
    )

  preg.df = preg.df %>%
    left_join(best.col.row.df, by=c("tabid","col","preg.row"))


  # Best match info
  best.col.mdf = filter(col.mdf,col.num.match == col.best.num.match)
  mreg.df = best.col.mdf %>%
    left_join(select(pdf.tabs, tabid,tabname,tpname,table.title), by="tabid") %>%
    left_join(regtab, by=c("creg.num"))


  # Add mct column to mreg.df. An coef table that contains the matches
  # between preg.df rows from the PDF table and ct rows from the code output
  i = 1
  mreg.df$mct = lapply(seq_len(NROW(mreg.df)), function(i) {
    matches = semi_join(mdf,mreg.df[i,], by=c("tabid","col"))
    #cat(paste0('"', colnames(matches),'"', collapse=", "))
    cols = c("xlabel", "var", "coef", "se", "t", "p", "ci_low", "ci_up","row"
             ,"col", "pdf.coef", "pdf.par", "par.type", "coef.big.str", "coef.deci", "round.coef.diff","round.par.diff", "tabid")
    matches[, cols]
  })

  mreg.df$num.code.coef = sapply(seq_len(NROW(mreg.df)), function(i) NROW(mreg.df$ct[[i]]))
  mreg.df$num.matched.coef = sapply(seq_len(NROW(mreg.df)), function(i) n_distinct(mreg.df$mct[[i]]$var))

  res = list(creg.df = creg.df, preg.df=preg.df, mdf=mdf, col.mdf=col.mdf, pdf.tabs=pdf.tabs, regtab=regtab, mreg.df=mreg.df)



  return(res)
}


old.find.all.coef.matchings = function(preg.df, creg.df) {
  restore.point("find.all.coef.matchings")

  empty.res = cbind(preg.row=integer(0),creg.row=integer(0),match.ind=integer(0))

  preg.val = preg.df[["pdf.coef"]]
  pdeci = preg.df[["coef.deci"]]
  creg.val = creg.df[["coef"]]

  rows = seq_along(preg.val)

  match.coef = do.call(rbind,lapply(rows, function(i) {
    #restore.point("ksjkdsdjdsjklm")
    match = which(round(creg.val, pdeci[i])==preg.val[i])
    if (length(match)==0) return(empty.res)
    cbind(preg.row=i, creg.row=match, match.ind = seq_along(match))
  }))
  match.coef
}


