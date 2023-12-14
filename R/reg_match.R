example = function() {
  project.dir = standardizePath("~/repbox/projects_reg/aejapp_13_3_7")
  res = reg.match(project.dir)

  mreg.df = res$mreg.df

  aregs = make.artreg.rds(project.dir)
  aregs = load.or.make.artreg(project.dir)



}

load.or.create.reg.match = function(project.dir) {
  rema = load.reg.match(project.dir)
  if (is.null(rema)) {
    rema = create.reg.match(project.dir)
  }
  rema
}

create.reg.match = function(project.dir) {
  file = file.path(project.dir,"repbox/regmatch.Rds")
  res = reg.match(project.dir)
  rema = res$mreg.df
  if (!is.null(mreg.df)) {
    saveRDS(rema, file)
  }
  invisible(rema)
}

load.reg.match = function(project.dir) {
  file = file.path(project.dir,"repbox/regmatch.Rds")
  if (!file.exists(file)) return(NULL)
  readRDS(file)
}

load.or.make.artreg = function(project.dir) {
  artreg.file = file.path(project.dir,"repbox/artreg.Rds")
  if (file.exists(artreg.file)) {
    return(readRDS(artreg.file))
  }

  return(make.artreg.rds(project.dir))
}

make.artreg.rds = function(project.dir) {
  arttab.file = file.path(project.dir,"repbox/arttab.Rds")
  if (!file.exists(arttab.file)) {
    return(NULL)
  }
  tab.df = readRDS(arttab.file)
  tab.df = add.reg.info.to.tab.df(tab.df)
  reg.df = tab.df.to.reg.df(tab.df)
  reg.df$project = basename(project.dir)
  saveRDS(reg.df, file.path(project.dir,"repbox/artreg.Rds"))
  invisible(reg.df)
}


reg.match = function(project.dir, verbose=TRUE) {
  restore.point("repbox.reg.match")
  repbox.dir = paste0(project.dir,"/repbox")

  # aregs = article regressions extracted from pdf
  # cregs = code regressions extracted from running supplement

  aregs = load.or.make.artreg(project.dir)

  # for debugging
  # aregs = filter(aregs, ar_num == 13)

  if (NROW(aregs)==0) {
    cat(paste0("\n\tNo regression tables found in article of project ", project.dir))
    return(NULL)
  }

  cregs = readRDS(paste0(repbox.dir, "/stata/regtab.Rds"))
  if (NROW(cregs)==0) {
    if (verbose) {
      cat(paste0("\n\tNo regressions were run for project ", project.dir))
      return(NULL)
    }
  }

  cregs$cr_num = seq_len(NROW(cregs))
  ccoef.df = cregs %>%
    tidyr::unnest(c(ct)) %>%
    mutate(ccoef.row = seq_len(n()))
  if (NROW(ccoef.df)==0) {
    if (verbose) {
      cat(paste0("\n\tNo regression results obtained when running supplement for project ", project.dir))
      return(invisible())
    }
  }

  acoef.df = aregs %>%
    select(ar_num, tabid, reg_col, obs, ct, tpname) %>%
    unnest(ct) %>%
    mutate(acoef.row = 1:n()) %>%
    group_by(ar_num) %>%
    mutate(ar_num_coef = n()) %>%
    ungroup() %>%
    mutate(coef_deci_step =10^-coef_deci, par_deci_step = 10^-par_deci)

  mc = find.all.coef.matchings(acoef.df, ccoef.df) %>% as.data.frame()

  # Map all matched coefficients from article and supplement
  mdf =
    left_join(acoef.df, mc, by=c("acoef.row")) %>%
    left_join(ccoef.df, by="ccoef.row")

  # 3 Versions for interpretation of value in ( ): se, t or p
  mdf = bind_rows(
    mutate(mdf, par_type = "se", creg.par = se),
    mutate(mdf, par_type = "t", creg.par = t),
    mutate(mdf, par_type = "p", creg.par = p)
  )

  # Compute match of coeficient and parentheses
  mdf = mdf %>% mutate(
    coef_diff = abs(art_coef-coef),
    round_coef_diff = round(coef_diff, coef_deci),
    par_diff = abs(art_par-creg.par),
    round_par_diff = round(par_diff, par_deci)
  )

  # Old version: keep only perfect matches
  #mdf = filter(mdf, round_coef_diff==0, round_par_diff==0)

  # New version: also assign points for partial matches, e.g. due to
  # rounding errors...
  mdf = mdf %>% mutate(
    row_coef_match_score = case_when(
      round_coef_diff==0 ~ 1,
      round_coef_diff<=coef_deci_step ~ 0.75
    ),
    row_match_score = case_when(
      round_coef_diff==0 & round_par_diff==0 ~ 1,
      # likely just a rounding error
      round_coef_diff<=coef_deci_step & round_par_diff==0 ~ 0.75,
      round_coef_diff==0 & round_par_diff<=par_deci_step ~ 0.75,
      # rounding error for both coef and par seems less likely
      round_coef_diff<=coef_deci_step & round_par_diff<=par_deci_step ~ 0.1,
      # only one number matches exactly
      round_coef_diff ==0 ~ 0.1,
      round_par_diff == 0 ~ 0.1
    )
  )

  # Only keep best matches for every
  # ar-cr par_type coef combi
  mdf = mdf %>%
    group_by(tabid, ar_num, cr_num, par_type, acoef.row) %>%
    filter(row_match_score == max(row_match_score)) %>%
    ungroup()


  # Store for each article regression (ar_num)
  # and possible match (cr_num, par_type)
  # information of how many rows (coefficient & parentheses) match
  ar_mdf = mdf %>%
    # only keep for each ar-cr par_type coef combi one matching row
    # so that we can later sum up the row_match_score
    group_by(tabid, ar_num, cr_num, par_type, acoef.row) %>%
    slice(1) %>%
    group_by(tabid, ar_num, cr_num, par_type, ar_num_coef) %>%
    summarize(
      #ar_num_match = n_distinct(acoef.row),
      ar_num_match = sum(row_match_score),
      ar_share_match = first(ar_num_match / ar_num_coef),
      ar_coef_share_match =  sum(row_coef_match_score) / ar_num_coef
    ) %>%
    group_by(ar_num) %>%
    mutate(
      ar_best_num_match = suppressWarnings(max(ar_num_match, na.rm = TRUE)),
      ar_best_share_match = suppressWarnings(max(ar_share_match, na.rm = TRUE))
    ) %>%
    # compute best fit for parenthesis
    group_by(tabid, par_type) %>%
    mutate(
      tab_par_match_score = sum(ar_num_match)
    ) %>%
    group_by(tabid) %>%
    mutate(is_best_par_type = tab_par_match_score == max(tab_par_match_score)) %>%
    ungroup()


  # Map match information from complete article regression back to mdf (each coefficient)
  mdf = mdf %>%
    left_join(select(ar_mdf, ar_num, cr_num,par_type, ar_share_match, ar_num_match, ar_coef_share_match, ar_best_num_match, ar_best_share_match, is_best_par_type),by=c("ar_num","cr_num", "par_type"))

  # Only keep best parenthesis match for each table
  mdf = filter(mdf, is_best_par_type)
  ar_mdf = filter(ar_mdf, is_best_par_type)

  # Best match info
  best_ar_mdf = filter(ar_mdf,ar_num_match == ar_best_num_match)
  mreg.df = best_ar_mdf %>%
    left_join(select(aregs,ar_num, reg_col, tabname,tpname,table_title), by="ar_num") %>%
    left_join(cregs, by=c("cr_num"))

  # Match best match info back to acoef.df
  # if of interest later

  # uni_best_ar_mdf = best_ar_mdf %>%
  #   select(ar_num,ar_best_num_match, ar_best_share_match) %>%
  #   unique()
  #
  # acoef.df = acoef.df %>%
  #   left_join(uni_best_ar_mdf, by="ar_num")


  # Add mct column to mreg.df. An coef table that contains the matches
  # between acoef.df rows from the PDF table and ct rows from the code output
  i = 13
  mreg.df$mct = lapply(seq_len(NROW(mreg.df)), function(i) {
    matches = semi_join(mdf,mreg.df[i,], by=c("ar_num"))
    #cat(paste0('"', colnames(matches),'"', collapse=", "))
    cols = c("xlabel", "var", "coef", "se", "t", "p", "ci_low", "ci_up","row"
             ,"art_coef", "art_par", "par_type", "coef_big_str", "coef_deci", "round_coef_diff","round_par_diff", "ar_num", "row_match_score", "row_coef_match_score")
    matches[, cols]
  })

  mreg.df$cr_num_coef = sapply(seq_len(NROW(mreg.df)), function(i) NROW(mreg.df$ct[[i]]))
  #mreg.df$matched_num_coef = sapply(seq_len(NROW(mreg.df)), function(i) n_distinct(mreg.df$mct[[i]]$var))

  cols = c("ar_num","cr_num","tabname", "reg_col", "doid","ar_share_match","mct", "ar_coef_share_match", "par_type","ar_num_coef","cr_num_coef","ar_num_match")
  mreg.df = mreg.df[,c(cols, setdiff(colnames(mreg.df),cols))]

  res = list(ccoef.df = ccoef.df, acoef.df=acoef.df, mdf=mdf, ar_mdf=ar_mdf, aregs=aregs, cregs=cregs, mreg.df=mreg.df)



  return(res)
}


# Finds for every coefficient in acoef.df (from article regression tables)
# matched coefficients in ccoef.df (from supplement regression output)
# we don't yet perform any aggregation on regression level
find.all.coef.matchings = function(acoef.df, ccoef.df, just.exact=FALSE) {
  restore.point("find.all.coef.matchings")

  empty.res = cbind(acoef.row=integer(0),ccoef.row=integer(0),match.ind=integer(0))

  areg.val = acoef.df[["art_coef"]]
  adeci = acoef.df[["coef_deci"]]
  creg.val = ccoef.df[["coef"]]

  rows = seq_along(areg.val)

  if (just.exact) {
    match.coef = do.call(rbind,lapply(rows, function(i) {
      match = which(round(creg.val, adeci[i])==areg.val[i])
      if (length(match)==0) return(empty.res)
      cbind(acoef.row=i, ccoef.row=match, match.ind = seq_along(match))
    }))

  } else {
    tol = acoef.df[["coef_deci_step"]]
    match.coef = do.call(rbind,lapply(rows, function(i) {
      match = which( abs(round(creg.val, adeci[i])-areg.val[i])<=tol[i])
      if (length(match)==0) return(empty.res)
      cbind(acoef.row=i, ccoef.row=match, match.ind = seq_along(match))
    }))

  }
  match.coef
}


