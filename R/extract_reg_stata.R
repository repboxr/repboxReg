# Will be called from repboxStata
#
# Extracts specified regression information
#
extract.stata.regs = function(project_dir, run.df=NULL, dotab=NULL, save=TRUE) {
  restore.point("extract.stata.regs")

  project = basename(project_dir)
  #++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1. Extract TSV information stored by esttab
  #++++++++++++++++++++++++++++++++++++++++++++++++++

  res.dir = file.path(project_dir,"repbox/stata/tsv")
  files = list.files(res.dir,glob2rx(paste0("*.dta")),full.names = TRUE)

  bfiles = basename(files)
  donum = str.left.of(bfiles, "_") %>% as_integer()
  str = str.right.of(bfiles,"_")
  line = str.left.of(str, "_") %>% as_integer()
  str = str.right.of(str,"_")
  counter = str.remove.ends(str, right=4) %>% as_integer()

  regtab = tibble(regresfile=files,donum=donum,line=line,counter=counter) %>%
    arrange(donum, line, counter) %>%
    group_by(donum, line) %>%
    mutate(run = seq_len(n())) %>%
    ungroup()

  regtab$ct = lapply(regtab$regresfile, function(file) {
    restore.point("inner.read.regres")
    regres = haven::read_dta(file)
    old.cols = c("parm","label","estimate","stderr","dof", "z","p","min95","max95")
    new.cols = c("var","label", "coef","se","dof", "t","p","ci_low","ci_up")
    regres = rename.cols(regres, old.cols, new.cols)
    regres = regres[,intersect(new.cols, colnames(regres))]
    regres
  })
  regtab = select(regtab, -regresfile)



  #++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2. Extract regression information stored in logs
  #++++++++++++++++++++++++++++++++++++++++++++++++++

  dir = file.path(project_dir, "repbox/stata/logs")
  log.files = list.files(dir,glob2rx("log_*.log"),full.names = TRUE)

  reg.log = lapply(log.files, function(file) {
    log.txt = readLines(file,warn=FALSE)  %>% enc2utf8()
    bdf = extract.inject.blocks(log.txt, type="REG_ERETURN")
    bdf$er = lapply(bdf$str,parse.ereturn.injection)
    bdf
  }) %>% bind_rows()

  regtab = left_join(regtab, select(reg.log, donum, line, counter, er), by=c("donum","line","counter"))

  if (is.null(run.df)) {
    return(regtab)
  }

  #++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3. Merge with run.df
  #++++++++++++++++++++++++++++++++++++++++++++++++++

  regtab = left_join(regtab,run.df, by=c("donum","line","counter"))%>%
    left_join(select(dotab, donum, doid), by=c("donum"))

  # UPDATE: Only consider regression where missing_data = FALSE
  # Otherwise we likely have faulty regressions that use an earlier data set
  # Also ignore regression results with run error
  regtab = regtab[regtab$has.data & !regtab$runerr,]


  regtab$project = project
  #colnames(regtab)
  cols = c("project", "donum", "doid", "line", "counter","cmd", "cmdline","ct","er", "datasig", "timevar" ,"panelvar", "tdelta",        "runerr",        "runerrcode",    "runerrmsg",     "runsec", "orgline", "in.program", "has.data")
  regtab = regtab[,cols]


  if (save) {
    saveRDS(regtab, file.path(project_dir,"repbox/stata/regtab.Rds"))
  }

  regtab
}


# Used to extract additional regression information
parse.ereturn.injection = function(str) {
  restore.point("parse.ereturn.injection")

  # scalars
  start = which(str=="scalars:")+1
  if (length(start)>0) {
    end = min(which(str=="" & 1:NROW(str)>start))-1
    svec = trimws(str[start:end])
    var = trimws(str.left.of(svec,"=")) %>% str.remove.ends(2,1)
    val = suppressWarnings(as.numeric(str.right.of(svec,"=") %>% trimws()))
    scalars = as.list(val)
    names(scalars) = var
  } else {
    scalars = list()
  }

  # macros (contains e.g. depvar)
  start = which(str=="macros:")+1
  if (length(start)>0) {
    end = min(which((str=="" | startsWith(str, "#~# END INJECT")) & 1:NROW(str)>start))-1
    svec = trimws(str[start:end])
    var = trimws(str.left.of(svec,":")) %>% str.remove.ends(2,1)
    val = str.right.of(svec,":") %>% trimws()%>% str.remove.ends(1,1)
    macros = as.list(val)
    names(macros) = var
  } else {
    macros = list()
  }

  as_tibble(c(scalars,macros))
}


# Still uses tsv
extract.stata.regs.old = function(project_dir, run.df=NULL, dotab=NULL, save=TRUE) {
  restore.point("extract.stata.regs")


  #++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1. Extract TSV information stored by esttab
  #++++++++++++++++++++++++++++++++++++++++++++++++++

  tsv.dir = file.path(project_dir,"repbox/stata/tsv")
  files = list.files(tsv.dir,glob2rx(paste0("*.tsv")),full.names = TRUE)

  bfiles = basename(files)
  donum = str.left.of(bfiles, "_") %>% as_integer()
  str = str.right.of(bfiles,"_")
  line = str.left.of(str, "_") %>% as_integer()
  str = str.right.of(str,"_")
  counter = str.remove.ends(str, right=4) %>% as_integer()

  regtab = tibble(tsvfile=files,donum=donum,line=line,counter=counter) %>%
    arrange(donum, line, counter) %>%
    group_by(donum, line) %>%
    mutate(run = seq_len(n())) %>%
    ungroup()

  regtab$ct = lapply(regtab$tsvfile, function(file) {
    restore.point("inner.read.reg.tsv")
    tsv.txt = readLines(file,warn=FALSE)
    if (length(tsv.txt)>2) {
      df = data.table::fread(text=tsv.txt, sep="\t", na.strings=".", skip=2, header=FALSE) %>% as_tibble()
      colnames(df) = c("var","coef","se","t","p","ci_low","ci_up")
    } else {
      df = NULL
    }
    df
  })
  regtab = select(regtab, -tsvfile)



  #++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2. Extract regression information stored in logs
  #++++++++++++++++++++++++++++++++++++++++++++++++++

  dir = file.path(project_dir, "repbox/stata/logs")
  log.files = list.files(dir,glob2rx("log_*.log"),full.names = TRUE)

  reg.log = lapply(log.files, function(file) {
    log.txt = readLines(file,warn=FALSE)  %>% enc2utf8()
    bdf = extract.inject.blocks(log.txt, type="REG_ERETURN")
    bdf$er = lapply(bdf$str,parse.ereturn.injection)
    bdf
  }) %>% bind_rows()

  regtab = left_join(regtab, select(reg.log, donum, line, counter, er), by=c("donum","line","counter"))

  if (is.null(run.df)) {
    return(regtab)
  }

  #++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3. Merge with run.df
  #++++++++++++++++++++++++++++++++++++++++++++++++++

  regtab = left_join(regtab,run.df, by=c("donum","line","counter"))%>%
    left_join(select(dotab, donum, doid), by=c("donum"))
  regtab$project = project
  #colnames(regtab)
  cols = c("project", "donum", "doid", "line", "counter","cmd", "cmdline","ct","er", "datasig", "timevar" ,"panelvar", "tdelta",        "runerr",        "runerrcode",    "runerrmsg",     "runsec", "orgline", "in.program", "has.data")
  regtab = regtab[,cols]


  if (save) {
    saveRDS(regtab, file.path(project_dir,"repbox/stata/regtab.Rds"))
  }

  regtab
}


