
do.overview.html = function(project.dir, su, ma) {
  restore.point("do.overview.html")

  sup.dir = file.path(project.dir,"mod")
  # Do files overview
  do.files = get.project.do.files(project.dir)
  #do.files = list.files(sup.dir,glob2rx("*.do"),full.names = TRUE,recursive = TRUE)
  #do.files = do.files[!startsWith(basename(do.files),"repbox_")]

  do.df = tibble(file = do.files, doid = tools::file_path_sans_ext(basename(do.files)))



  h = "<h4>Summary of do file analysis</h4>\n"
  if (NROW(do.df)==0) {
    h = paste0(h,"<p>The project has no do files.<p>")
    return(h)
  }

  dotab = su$dotab
  if (is.null(dotab)) {
    tab.html = repbox_html_table(id="do-overview-tab",
                                 transmute(do.df, Do=paste0(doid,".do"), Analyzed="NO")
    )
    h = paste0(h, tab.html)
    return(h)
  }



  do.df = left_join(select(do.df, doid),dotab, by="doid")
  do.df$analyzed = !is.na(do.df$dofile)
  do.df = arrange(do.df, desc(analyzed), desc(!is.na(runtime)))

  run.df = su$run.df

  runs.df = run.df %>%
    group_by(donum) %>%
    summarize(
      runs = n(),
      runs.with.data = sum(has.data),
      runs.no.data = sum(!has.data),
      runs.err = sum(runerr),
      runs.err.with.data = sum(has.data & runerr)
    )

  reg.df = filter(run.df, is.regcmd)
  mdf = ma$mdf
  if (!is.null(mdf)) {
    mdf.has = mdf %>%
      group_by(donum, line, counter) %>%
      summarize(matched = TRUE)

    reg.df = reg.df %>%
      left_join(mdf.has, by = c("donum","line","counter")) %>%
      mutate(matched = na.val(matched, FALSE))

  } else {
    reg.df$matched = rep(FALSE, NROW(reg.df))
  }

  regruns.df = reg.df %>%
    group_by(donum) %>%
    summarize(
      rruns = n(),
      rruns.with.data = sum(has.data),
      rruns.no.data = sum(!has.data),
      rruns.err = sum(runerr),
      rruns.err.with.data = sum(has.data & runerr),
      matched = sum(matched)
    )

  tab = su$tab

  if (!is.null(su$regtab)) {
    norunregs = su$regtab %>%
      anti_join(run.df, by=c("donum","line")) %>%
      group_by(donum) %>%
      summarize(norun.reg.lines = n())
  } else {
    norunregs = su$tab %>%
      filter(is.regcmd) %>%
      group_by(donum) %>%
      summarize(norun.reg.lines = sum(errruns + runs == 0))
  }

  do.df = do.df %>%
    left_join(runs.df, by="donum") %>%
    left_join(regruns.df, by="donum") %>%
    left_join(norunregs, by="donum")


  if (!has.col(do.df,"is.included"))
    do.df$is.included = FALSE

  do.df = do.df %>% mutate(
    info = case_when(
      is.true(parse.err) & is.true(timeout) ~ "timeout",
      is.true(parse.err) ~ "parsing error",
      is.true(timeout) ~ "timeout",
      TRUE ~ ""
    )) %>%
    mutate(
      info = paste0(case_when(
        !analyzed~"not analyzed",
        is.included~"called in other do",
        is.na(runtime) & is.true(timeout) ~ "not run, global ",
        is.na(runtime) ~ "just parsed ",
        TRUE ~ ""
      ), info)
    )

  if (NROW(do.df)>1) {
    total = lapply(do.df, function(val) {
      if (is.numeric(val)) return(sum(val, na.rm=TRUE))
      return(NA)
    }) %>% as_tibble()
    total$analyzed = FALSE
    total$do.file = "TOTAL"
    total$info = ""
    do.df = bind_rows(total, do.df)
    do.df$is.total = c(TRUE, rep(FALSE,NROW(do.df)-1))
  } else {
    do.df$is.total = logical(NROW(do.df))
  }

  format.share = function(x) {
    x = ifelse(is.finite(x),
               paste0(round(100*x,1),"%"),
               "-"
    )
    x
  }

  head.html = '
<tr>
  <th style="border: 0; padding-bottom: 0;"></th>
  <th style="border: 0; padding-bottom: 0;">Runtime</th>
  <th style="border: 0; padding-bottom: 0;" colspan="4" align="center">Regressions</th>
  <th colspan="3" style="border: 0; padding-bottom: 0;">All commands</th>
  <th style="border: 0; padding-bottom: 0;">Info</th>
</tr><tr>
  <th style="border: 0"></th>
  <th style="border: 0">(sec.)</th>

  <th style="border: 0">Runs</th>
  <th style="border: 0">Matched</th>
  <th style="border: 0">No data</th>
  <th style="border: 0">Error</th>

  <th style="border: 0">Runs</th>
  <th style="border: 0">No data</th>
  <th style="border: 0">Error</th>
  <th style="border: 0"></th>

</tr>
  '
  do.df = mutate(do.df, row.html = paste0(
    '<tr', ifelse(is.total,' class="total-row"',""),'>','<td>',ifelse(analyzed,
                                                                      paste0('<a href="',do.line.link(donum),'">',dofile,"</a>"),
                                                                      do.file),'</td>',
    '<td style="text-align: right;">',na.val(round(runtime),"-"),'</td>',

    '<td style="text-align: right;">',na.val(rruns,"-"),'</td>',
    '<td style="text-align: right;">',na.val(matched,"-"),'</td>',
    '<td style="text-align: right;">',na.val(rruns.no.data,"-"),'</td>',
    '<td style="text-align: right;">',na.val(rruns.err.with.data,"-"),'</td>',

    '<td style="text-align: right;">',na.val(runs,"-"),'</td>',
    '<td style="text-align: right;">',na.val(runs.no.data,"-"),'</td>',
    '<td style="text-align: right;">',na.val(runs.err.with.data,"-"),'</td>',
    '<td>',info,'</td>',
    '</tr>'
  ))
  tbody = paste0(do.df$row.html, collapse="\n")

  tab.html = paste0('\n<table id="do-overview-tab" class="table-mini table table-striped">
  <thead>',head.html, "</thead><tbody>\n",tbody,"</tbody></table>")
  h = paste0(h, tab.html)
  return(h)

}


# Code with regression tables overview
tab.overview.html = function(project.dir, su=NULL, ma=NULL, show.figure=FALSE, show.unknown=TRUE, show.reg=FALSE) {
  restore.point("tab.overview.html")
  adf = readRDS.or.null(paste0(project.dir, "/repbox/arttab.Rds"))

  if (!show.figure & !is.null(adf)) {
    adf = filter(adf, !startsWith(tabname,"Figure"))
  }
  if (!show.unknown & !is.null(adf)) {
    adf = filter(adf, !startsWith(tabname,"Unknown"))
  }

  pdf.df = ma$pdf.df
  h = "<h4>Extracted Tables from PDF</h4>"
  if (is.null(adf)) {
    h = paste0(h, "<p>No tables extracted from PDF.</p>")
    files = list.files(paste0(project.dir,"/pdf"),glob2rx("*.pdf"),ignore.case = TRUE)
    if (length(files)==0) {
      h = paste0(h, "<p>Likely reason: no pdf file was copied to:<br>", paste0(project.dir,"/pdf"), "</p>")
    }
    return(h)
  }

  df = adf %>%
    mutate(
      tablink = paste0('<a href="', arttab.link(tabid),'">',tpname,'</a>')
    )

  # Match regression tables
  if (!is.null(pdf.df)) {
    col.grid = df %>%
      mutate(col = lapply(loc.df, function(loc)
        setdiff(sort(unique(loc$col)),0)
      )) %>%
      select(tabid, col) %>%
      tidyr::unnest(cols=col)

    perc.info = col.grid %>%
      left_join(select(pdf.df, tabid, col, col.best.share.match), by=c("tabid","col")) %>%
      mutate(col.best.share.match = na.val(col.best.share.match,0)) %>%
      group_by(tabid) %>%
      summarize(match.perc = mean(col.best.share.match))

    df = left_join(df, perc.info, by="tabid") %>%
      mutate(reg.match.str = paste0(round(match.perc*100),"%"))
  } else {
    df$reg.match.str = "0%"
  }

  # Match general numbers
  if (!is.null(ma$num.match.agg)) {
    df = left_join(df, select(ma$num.match.agg,-tpname), by="tabid") %>%
      left_join(select(su$dotab,donum,doid), by="donum") %>%
      mutate(
        do.str = paste0('<a href="', do.line.link(donum),'&tabid=',tabid,'">',doid,'.do</a>'),
        num.count.str = tab.num.count,
        do.match.str = paste0(round(match.share*100),"%"),
        glob.match.str = paste0(round(glob.match.share*100),"%")
      )
  } else {
    df$do.str = ""
    df$do.match.str = ""
    df$num.count.str = ""
    df$match.share = ""
    df$glob.match.share = ""
    df$glob.match.str = ""
  }


  head.html = paste0('
<tr>
  <th>Table</th>
  <th>Numbers</th>
  <th>Best matching do</th>
  <th>Matches</th>
  <th>Matches (all do)</th>
', if (show.reg) '<th>Matches (regressions)</th>','
</tr>')
  df = df %>% mutate(row.html = paste0(
    '<tr>',
    '<td>',tablink,'</td>',
    '<td style="text-align: right">',num.count.str,'</td>',
    '<td>',do.str,'</td>',
    '<td style="text-align: right">',do.match.str,'</td>',
    '<td style="text-align: right">',glob.match.str,'</td>',
    if (show.reg) paste0(
      '<td style="text-align: right">',reg.match.str,'</td>'),
    '</tr>')
  )

  tbody = paste0(df$row.html, collapse="\n")

  tab.html = paste0('\n<table class="table-mini table table-striped">
  <thead>',head.html, "</thead><tbody>\n",tbody,"</tbody></table>")
  h = paste0(h, tab.html)
  return(h)
}
project.tab.tabs.html = function(project.dir,  ma = readRDS.or.null(paste0(project.dir,"/repbox/matched_tabs.Rds")), su = readRDS.or.null(paste0(project.dir,"/repbox/stata/repbox_results.Rds")), show.figure=FALSE, show.unknown=TRUE, show.reg = FALSE) {
  restore.point("project.tab.tabs.html")

  tabs.df = readRDS.or.null(paste0(project.dir,"/repbox/arttab.Rds"))

  if (NROW(tabs.df)==0) {
    return("<p>No table has been extracted from the PDF.</p>")
  }

  ma = init.reg.table.matches(ma, su)

  # Overwrite default color spans if we have matching
  # Then a grey background indicates non-matches
  if (!is.null(ma$tab.span.txt)) {
    tabs.df$color.tab.txt = ma$tab.span.txt
  }

  if (!show.figure) {
    tabs.df = filter(tabs.df, !startsWith(tabname,"Figure"))
  }
  if (!show.unknown) {
    tabs.df = filter(tabs.df, !startsWith(tabname,"Unknown"))
  }

  if (NROW(tabs.df)==0) {
    return("<p>No table has been extracted from the PDF.</p>")
  }



  tabids = tabs.df$tabid
  tabid = 1
  contents = lapply(tabids, function(tabid) {
    tab = tabs.df[tabs.df$tabid == tabid, ]
    if (isTRUE(tab$has.reg.df) & !is.null(ma$pdf.df) & show.reg) {
      reg.tab.html(tab, su, ma)
    } else {
      noreg.tab.html(tab,ma)
    }
  })
  link.menus.html = reg.tab.link.menus(su,ma)

  html = paste0(
    repboxTabSetPanel(
      id=paste0("tabtabs"),
      tabnames = paste0(tabs.df$tpname),
      tabids = paste0("tabtab",tabids),
      contents = contents, type="pills"
    ),"\n",
    '<div id="link-menu"></div>',
    link.menus.html
  )
  html
}




reg.tab.link.menus = function(su, ma) {
  restore.point("reg.tab.link.menus")
  mdf = ma$mdf
  if (is.null(mdf)) {
    return(NULL)
  }

  short.rp.df = ma$rp.df %>%
    group_by(rp.regnum) %>%
    filter(1:n() == 1)

  col.df = ma$col.mdf
  col.df = left_join(col.df, select(short.rp.df, rp.regnum, donum, orgline), by="rp.regnum")


  # <div id="come-1-1">
  #   <div class="item">Col 1 A</div>
  #   <div class="item">Col 1 B</div>
  # </div>
  menu.df = col.df %>%
    add.donum(su$dotab) %>%
    group_by(tabid, col) %>%
    arrange(desc(col.best.num.match)) %>%
    filter(1:n() <= 12 | col.best.share.match==1) %>%
    summarize(
      menu.html = paste0('<div id="link-menu-',tabid,'-',col,'">',
                         paste0('<div class="item reg-link" data-donum="',donum,'" data-line="', orgline,'">', round(100*col.share.match),"% ",donum,".do:",orgline," ",'</div>', collapse="\n"),
                         '</div>'
      )
    )

  html = paste0('
<div style = "display: none">
',paste0(menu.df$menu.html, collapse="\n\n"),
                "\n</div>")
}



reg.tab.html = function(tab, su, ma, min.col.share = 0.3) {
  restore.point("reg.tab.html")

  dat = tab$reg.df[[1]]
  if (is.null(ma$pdf.df)) {
    dat$col.num.coef = NA
    dat$col.best.num.match = NA
    dat$col.best.num.match = NA
  } else {
    dat = left_join(dat, select(ma$pdf.df, tabid, col, row, col.num.coef, col.best.num.match, col.best.share.match), by=c("tabid","col","row"))
  }
  tab$reg.df[[1]] = dat

  main.html = tab.reg.main.html(tab, ma)

  source.html = source.tab.html(tab,ma)


  # No match
  tabid = tab$tabid
  if (is.null(ma$col.mdf)) {
    html = repboxTabSetPanel(
      id=paste0("tab_subtabs",tabid),
      tabnames = c("Main", "Source"),
      tabids = c(paste0("tabmain",tabid),paste0("tabsource",tabid) ),
      contents = list(main.html, source.html)
    )
    return(html)

  }


  col.mdf = ma$col.mdf
  .tabid = tabid
  cols = unique(filter(col.mdf, tabid==.tabid, col.share.match >= min.col.share)$col)

  cols.html = lapply(cols, function(col) {
    tab.col.matched.html(tab, col,su=su,ma=ma)
  })


  html = repboxTabSetPanel(id=paste0("tab_subtabs",tabid),
                           tabnames = c("Main",if (NROW(cols)>0) paste0("Col. ",cols),"Source"),
                           tabids = c(paste0("tabmain",tabid),if (NROW(cols)>0) paste0("tabcol", tabid,"_",cols),paste0("tabsource",tabid)),
                           contents = c(list(main.html), cols.html, source.html)
  )
  return(html)
}


tab.reg.main.html = function(tab, ma, min.col.share=0.3) {
  restore.point("tab.reg.main.html")

  dat = tab$reg.df[[1]]



  dat = dat %>%
    group_by(tabid,col) %>%
    mutate(
      has.match = !is.na(col.best.num.match) & is.true(col.best.share.match > min.col.share),
      num.match = sum(has.match),
      unknown.match = is.true(num.match > col.best.num.match) & has.match
    ) %>%
    ungroup() %>%
    arrange(tabid, row)

  xlabels = unique(dat$xlabel)

  dat = dat %>%
    arrange(tabid, col, row)
  cols = unique(dat$col)

  dat = dat %>% mutate(
    color = case_when(
      num.match == 0 ~ "#000000",
      # we know that this row has a match
      has.match & !unknown.match ~ "#0000aa",
      # unknown which row has match
      has.match & unknown.match ~ "#aa00aa",
      TRUE ~ "#880000"
    )
  )



  na.to.empty = function(x) {
    x[is.na(x)] = ""
    x
  }
  tabtitle = na.to.empty(tab$table.title)

  gr = tidyr::expand_grid(col=cols,xlabel=xlabels) %>%
    left_join(dat, by=c("col","xlabel")) %>%
    mutate(coef.str = na.to.empty(coef.big.str), par.str = na.to.empty(par.big.str))

  var.rows = match(xlabels, dat$xlabel)
  #lab.df = tibble(xlabel = xlabels, var = dat$var[var.rows])
  lab.df = tibble(xlabel = xlabels)

  col.rows = match(cols, dat$col)
  col.df = tibble(col=cols)


  str = paste0("<td title='","'>",lab.df$xlabel,"</td>")
  for (col in cols) {
    d = gr[gr$col == col,]
    d$class = ifelse(is.true(d$has.match),"maincoef-cell context-link","")
    #title = paste0("coef = ", signif(d$coef,5),"\nse = ",signif(d$se,5),"\np=",round(d$p,6))
    title = ""
    str = paste0(str, "<td class='",d$class,"' title='", title, "' data-col='",col,"'>",
                 "<span style='color: ", d$color,"'>", d$coef.str,"</span>","<br>",
                 "<span style='color: ", d$color,"'>", d$par.str,"</span>","</td>")
  }
  inner.html = paste0("<tr>",str,"</tr>",collapse="\n")

  head1 = paste0("<th>(",col.df$col,")</th>", collapse="\n")
  head = paste0("<tr><th></th>",head1,"</tr>")
  foot = ""


  html = paste0(
    "<h4>",tabtitle,"</h4>",
    "<table class='table-mini table table-striped maincoef-table' data-tabid='", tab$tabid,"'>",
    head,
    inner.html,
    foot,
    "</table>"
  )
  html
}


tab.col.matched.html = function(tab, col,su=NULL,ma=NULL, mdf=ma$mdf, rp.df=ma$rp.df, pdf.df=ma$pdf.df, max.code.char=30) {
  restore.point("tab.col.matched.html")

  tabid = tab$tabid
  p = pdf.df[pdf.df$tabid==tabid & pdf.df$col==col,]
  m = mdf[mdf$tabid==tabid & mdf$col==col,]
  m = arrange(m,desc(col.num.match))

  if (NROW(m)==0 | NROW(p)==0) return(NULL)

  max.row = max(p$row)
  regnums = unique(m$rp.regnum)


  regnum = regnums[1]

  rp.li = vector("list",length(regnums))

  nr = max.row
  i = 1
  for (i in seq_along(regnums)) {
    regnum = regnums[i]
    r = rp.df[rp.df$rp.regnum == regnum,]
    mr = m[m$rp.regnum == regnum,]
    r$row = mr$row[match(r$rp.row, mr$rp.row)]
    r$row[duplicated(r$row)] = NA

    num.na = sum(is.na(r$row))
    if (num.na>0) {
      r$row[is.na(r$row)] = max.row+1:num.na
    }
    r = arrange(r, is.na(r$row), r$row)
    rp.li[[i]] = r
    nr = max(nr, max.row+num.na)
  }

  # Make inner cells
  empty = rep("",nr)

  inner = empty
  # Column 1 and 2 from pdf.df
  s = empty
  rows = p$row
  s[rows] = p$xlabel
  inner = paste0("<td>",s,"</td>")

  s = empty
  rows = p$row
  s[rows] = paste0("<span>", p$coef.big.str,"</span>","<br>",
                   "<span>", p$par.big.str,"</span>","</td>")
  inner = paste0(inner,"<td>",s,"</td>")

  for (i in seq_along(rp.li)) {
    r = rp.li[[i]]
    s = empty
    rows = r$row
    s[rows] = r$var
    inner = paste0(inner,"<td>",s,"</td>")

    s = empty
    s[rows] = paste0("<span>", r$coef,"</span>","<br>",
                     "<span>(", r$se,")</span>")
    inner = paste0(inner,"<td>",s,"</td>")
  }
  inner.html = paste0("<tr>",inner,"</tr>",collapse="\n")

  head = "<th colspan='2'>Table from PDF</th>"
  i = 1

  for (i in seq_along(rp.li)) {
    r = rp.li[[i]]

    rows = match(r$donum, su$dotab$donum)
    r$donum = su$dotab$donum[rows]
    share.match = round(100*sum(r$row <= max.row) / NROW(p),1)
    head = paste0(head, "<th colspan='2'>",
                  "<a href='", do.line.link(first(r$donum), first(r$orgline)),"'>",
                  first(paste0(r$doid,".do"))," ",share.match,  "%<br>",
                  "<span style='font-size: 1rem; font-family: Consolas;'>",
                  shorten.str(first(r$code),50),
                  "</span></a></th>")
  }

  foot = NULL
  tabtitle = first(p$table.title)
  html = paste0("<h5>",tabtitle,"</h5><table class='table-mini table table-striped '>",
                head,
                inner.html,
                foot,
                "</table>")
  html

}


init.reg.table.matches = function(ma, su) {
  restore.point("init.reg.table.matches")
  mdf = ma$mdf
  rp.df = ma$rp.df
  tab = su$tab
  if (!is.null(rp.df)) {
    if (!is.null(tab)) {
      rp.df = rp.df %>%
        left_join(select(su$tab,line,donum,orgline),by=c("donum","line"))
    } else {
      rp.df$orgline = NA_integer_
    }
  }
  ma$rp.df = rp.df

  col.mdf = ma$col.mdf
  pdf.df = ma$pdf.df

  if (is.null(pdf.df)) {
    ma$tab.sum = NULL
    return(ma)
  }

  ma$tab.sum = pdf.df %>%
    group_by(tabid, col, col.num.coef, tpname) %>%
    summarize(
      col.best.num.match = first(col.best.num.match) %>% na.val(0),
      col.perc = col.best.num.match / col.num.coef
    ) %>%
    group_by(tabid) %>%
    summarize(
      tpname = first(tpname),
      perc = mean(col.perc),
      tabname = paste0(str.right.of(tpname,"Table ")," (", round(perc*100),"% of ",sum(col.num.coef),")")
    )

  return(ma)
}


