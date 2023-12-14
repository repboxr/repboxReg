examples = function() {
  library(repboxMain)
  library(repboxReg)

  options(warn=2)

  project = "aejapp_13_3_7"
  project = "aer_103_3_14"
  project = "aer_105_6_5"
  project = "testsupp"
  project = "aejmac_11_3_1"
  project = "aejapp_3_2_2"

  project.dir = file.path("~/repbox/projects_reg",project)

  dap_and_cache_remove_from_project(project.dir)
  #copy.ejd.project.to.reg(project)
  update.repbox.project(project.dir,run.lang = "stata",make.matching = FALSE, make.html = FALSE)

  run.project.with.reg(project.dir)

  options(warn = 2)
  mr = mr_base_run_study(project.dir, stata_version=17)


  rstudioapi::filesPaneNavigate(paste0(project.dir,"/repbox"))
  rstudioapi::filesPaneNavigate("~/repbox/repboxReg/R")
  rstudioapi::filesPaneNavigate("~/repbox/repboxStata/R")
}

run.project.with.reg = function(project.dir,store.data.caches=TRUE, timeout = 60*5, make.matching = FALSE, make.html=FALSE) {
  restore.point("run.project.with.reg")
  library(repboxMain)

  dap = get.project.dap(project.dir, make.if.missing = TRUE)

  if (store.data.caches) {
    cache.dir = file.path(project.dir, "metareg/dap/stata/cache")
    if (!dir.exists(cache.dir)) dir.create(cache.dir,recursive = TRUE)
    store.data = dap.to.store.data(dap, cache.dir)
  } else {
    store.data = NULL
  }

  stata.opts = repbox.stata.opts(report.inside.program = TRUE,all.do.timeout = timeout,timeout = timeout,extract.reg.info = TRUE, store.data = store.data)


  cat("\nInit repbox for ", project.dir)
  init.repbox.project(project.dir)

  cat("\nExtract article tables.")
  #repbox.extract.art.tabs(project.dir)

  cat("\nUpdate repbox...")
  update.repbox.project(project.dir,run.lang = "stata", make.matching = make.matching, make.html=make.html, make.ejd.html=FALSE, make.report.html = FALSE, make.rstudio.html = FALSE, stata.opts = stata.opts)

}

load.reg.dta = function(project.dir, reg=NULL, donum=reg$donum, line=reg$line, counter=reg$counter) {
  file = file.path(project.dir, "repbox/stata/dta", paste0("reg_", donum,"_",line,"_",counter,".dta"))
  if (!file.exists(file)) {
    cat("\nNo dta file for this regression stored.\n")
    return(NULL)
  }
  rio::import(file)
}

create.repbox.reg.project.from.ejd = function(project=NULL) {

  if (is.null(project)) {
    pro.df <- readRDS("~/repbox/repbox_ejd_infos.Rds")
    ejd_projects = list.files("~/repbox/projects_ejd")

    pro.df = pro.df %>%
      filter(share.runs.no.error >= 0.99 & runtime < 30 & runtime > 3 & data.missing == 0) %>%
      filter(id %in% ejd_projects) %>%
      arrange(runtime)

    row = 27
    project = pro.df$id[row]
    cat(project)
  }

  copy.ejd.project.to.reg(project)

  # Run one time without DAP
  if (!file.exists(paste0(project.dir,"/repbox/metareg_was_run.txt"))) {
    run.project.with.reg(project.dir, make.dap = FALSE)
    writeLines("yes",paste0(project.dir,"/repbox/metareg_was_run.txt"))

  }
  run.project.with.reg(project.dir,overwrite.dap = TRUE, make.dap = TRUE)
  writeLines("yes",paste0(project.dir,"/repbox/metareg_was_run.txt"))

  mr = mr_base_run_study(project.dir, run.stata=TRUE)

  if (FALSE) {
    plot.dap(mr)
    agg = mr$agg
    comp_stata = agg$comp_stata
    comp = agg$comp_stata_r
    # How many rows should we look at to detect likely new errors?
    agg$lookat
    agg$lookat.se


    agg$stata_ok
    agg$stata.r.coef.same
    agg$stata.r.same

    rstudioapi::filesPaneNavigate(paste0(project.dir,"/metareg"))
    rstudioapi::filesPaneNavigate("~/repbox/repboxReg/R")


    dap = get.project.dap(project.dir)
    plot.dap(dap)
  }
}

copy.ejd.project.to.reg = function(project=NULL) {


  ejd.projects.dir = "~/repbox/projects_ejd"
  reg.projects.dir = "~/repbox/projects_reg"
  project.dir = file.path("~/repbox/projects_reg",project)

  copy.dir(file.path(ejd.projects.dir,project), file.path(reg.projects.dir,project))




}

