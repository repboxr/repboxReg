example = function() {
  project = "testsupp"
  #project = "aejapp_13_3_7"

  project_dir = file.path("~/repbox/projects_reg",project)

  dap = get.project.dap(project_dir,add.run.df = TRUE)
  #plot.dap(dap)
  step.df = dap$step.df

  tidy_reg = function(reg, type) {
    restore.point("tidy_reg")
    res = bind_rows(
      tidy_sandwich(reg, "default"),
      tidy_sandwich(reg, "HC1"),
      tidy_sandwich(reg, "HC2"),
      tidy_sandwich(reg, "HC3"),
      tidy_sandwich(reg, "HC4"),
      tidy_sandwich(reg, "HC4m"),
      tidy_sandwich(reg, "HC5")
    )
    cbind(reg_type = type, res)
  }

  ana.fun = function(dat, reg, mr, step,...) {
    restore.point("step_run_fun")
    library(sandwich)
    vi = reg$vi[[1]]
    reg_call = make_lm_iv_call(reg)
    reg = eval_reg(reg_call, data=dat)
    ct = tidy_reg(reg, reg_call$command)

    return(as_tibble(ct))

    library(fixest)
    fml = vi_to_fixest_formula(vi, min_fe_level = 9)
    fix = feols(fml,dat)
  }
  mr = mr_init_study(project_dir,metaid="HC", fun=ana.fun, dap=dap)

  mr_write_path_rcode(mr, code.file = file.path(mr$out_dir,"path_code.R"),add.line.info = TRUE)
  mr_run(mr, asteps = sample(mr_get_asteps(mr),2))
  mr_run(mr)
  res = mr_run_single_path(mr, 42)

  term = "palm"

  temp = res %>% arrange(term)
  res$term
}

# TBD: A meta study analysis different supplements / articles
mr_init_metastudy = function(project.dirs, ...) {

}

# A study refers to a single supplement / article
mr_init_study = function(project_dir,  metaid=NULL,artid = basename(project_dir), version=NULL, dap = get.project.dap(project_dir,make.if.missing = TRUE), opts=mr_opts(), step_run_fun = NULL, study_agg_fun = NULL,stata_code_fun = NULL, stata_agg_fun = NULL, main_metaid = metaid, stata_version=NA) {
  restore.point("mr_init_study")
  if (!is_valid_metaid(metaid)) {
    stop("Please provide a valid analyis id (metaid). It shall contain only letters, digits and _ and not start with _.")
  }


  if (is.null(version)) {
    stop("Please provide a numeric version of the study type. If you are still in the development phase you may just set version = 0. The design of a metastudy may change in the future, but somewhere data of old runs may be stored. To determine whether an old run should be updated, meta study designers should always assign a new version when a metareg specification changes.")
  }

  mr = list()
  class(mr) = c("metareg_study","list")

  mr$metaid = metaid
  mr$artid = artid
  mr$project_dir = standardizePath(project_dir)
  mr$version = version
  mr$stata_version = stata_version
  mr$total_runtime = NA

  mr$main_metaid = main_metaid

  mr$dap_version = dap$version
  mr$step.df = dap$step.df
  mr$path.df = dap$path.df
  mr$res.li = vector("list", NROW(mr$step.df))
  mr$run.df = dap$run.df

  mr$opts = opts
  if (dir.exists(file.path(mr$project_dir,"repbox/stata"))) {
    mr$project.type = "repbox"
    try(mr$dotab <- readRDS(file.path(mr$project_dir,"repbox/stata/dotab.Rds")))
  } else {
    mr$project.type = "metareg"
  }
  mr$out_dir = file.path(mr$project_dir, "metareg", metaid)
  if (!dir.exists(mr$out_dir)) {
    dir.create(mr$out_dir,recursive = TRUE)
  }
  mr$step.dir = file.path(mr$project_dir, "metareg", metaid, "step_results")
  if (!dir.exists(mr$step.dir)) {
    dir.create(mr$step.dir,recursive = TRUE)
  }

  if (metaid!="base") {
    mr$repdb_out_dir = file.path(mr$project_dir, "metareg", metaid, "repdb")
  } else {
    mr$repdb_out_dir = file.path(mr$project_dir, "repdb")
  }
  if (!dir.exists(mr$repdb_out_dir)) {
    dir.create(mr$repdb_out_dir,recursive = TRUE)
  }

  mr$step_run_fun = step_run_fun
  mr$stata_code_fun = stata_code_fun
  mr$stata_agg_fun = stata_agg_fun
  mr$study_agg_fun = study_agg_fun

  clear_problem_files
  check = dap_check_all_cache_files_exist(project_dir, mr$step.df)
  if (!check$ok) {
    mr = mr_set_problem(mr, "missing_cache", paste0(
"Cannot perform the metastudy for ", mr$project_dir, " since not all data cache files exist. The owner of the project has to repeat the base analysis again in order to repair the project. Reasons for the corruption could be i) A new DAP was generated but the cache refers to an older DAP or ii) When generating the DAP the Stata code did not work as expected, e.g. because it was run on a slimified folder where data sets where removed."))
    return(mr)
  }



  if (mr$opts$pass.repdb.info & NROW(mr$step.df)>0) {
    mr = mr_load_parcels(mr, c("base_core","base_regvar",if (mr$opts$create.regxvar.cols) "base_regxvar"))
  }


  mr
}

#' Options
#' @param save.each.step If TRUE saves the results of each analysis step in a default .Rds file. Set to FALSE if your analysis steps saves the results itself.
#' @param extra.cache If the R code for a mod step has an error run the stata code instead and store the resulting .dta file for this step in project_dir/metareg/extra_cache. This file will then be used for the following analysis steps. We call it extra cache since we did not yet detect at the parsing stage that a cache will be required here.
#' @param load.extra.cache Should a previously generated extra.cache file for a step directly be loaded?
#' @param extra.infeasible An alternative to extra.cache. If the R code in a modification step throws an error just generate a flag by storing a file in metareg/extra_infeasible. No cache will be generated. Yet, when running the whole project again, DAP will mark any extra infeasible step as infeasible and thus generate cache files. This approach will sometimes generate more efficient caches than extra.cache = TRUE, but takes longer since the complete repbox project must be run again. Best set either extra.cache=TRUE or extra.infeasible=TRUE.


mr_opts = function(save.each.step = TRUE,extra.cache=TRUE,load.extra.cache=TRUE, extra.infeasible=FALSE, stop.on.error=FALSE, pass.repdb.info=TRUE, pass.internal.info = FALSE, repdb.tabs = c("reg","regvar","regxvar","regcoef","regcheck", "cmdpart","colstat_dummy","colstat_factor","colstat_numeric"), save.header=TRUE, stata.preserve.always=TRUE, create.regxvar.cols=pass.repdb.info,  ...) {
  list(
    save.each.step = save.each.step,
    extra.cache = extra.cache,
    extra.infeasible = extra.infeasible,
    load.extra.cache = load.extra.cache,
    stop.on.error = stop.on.error,
    pass.repdb.info = pass.repdb.info,
    pass.internal.info = pass.internal.info,
    repdb.tabs = repdb.tabs,
    save.header = save.header,
    stata.preserve.always = stata.preserve.always,
    create.regxvar.cols=create.regxvar.cols,
    ...
  )
}

is_valid_metaid = function(metaid) {
  if (is.null(metaid)) return(FALSE)
  rx = "^[a-zA-Z0-9][a-zA-Z0-9_]*$"
  grepl(rx, metaid)
}

