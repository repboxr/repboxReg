# Functions related to data cache handling



example = function() {
  project = "testsupp"
  project.dir = file.path("~/repbox/projects_reg",project)

  mr_get_project_cache_files(project.dir)
}

dap_and_cache_check_outdated = function(project.dir, sup.dir = file.path(project.dir,"org")) {
  restore.point("dap_and_cache_check_outdated")
  do.files = list.files(sup.dir, glob2rx("*.do"), full.names = TRUE,recursive = TRUE)

  cache.dir = paste0(project.dir,"/metareg/dap/stata/cache")
  extra.cache.dir = paste0(project.dir, "/metareg/dap/stata/extra_cache")

  cache.files = list.files(c(cache.dir, extra.cache.dir),glob2rx("*.dta"), full.names = TRUE)

  dap.file = file.path(project.dir,"/metareg/dap/stata/dap.Rds")

  dap.time = file.mtime(dap.file)
  do.max.time = suppressWarnings(max(file.mtime(do.files)))
  cache.min.time = suppressWarnings(min(file.mtime(cache.files)))

  if (isTRUE(do.max.time > dap.time)) {
    return(list(ok=FALSE, msg="There is a do file newer than the DAP. Please clear DAP and cache and run all new: 1. repox without reg (to create run.df), 2. repbox with reg (to create DAP and caches) 3. Your metastudy"))
  }
  if (isTRUE(cache.min.time < dap.time)) {
    return(list(ok=FALSE, msg="There is a cache file older than the DAP. Please clear DAP and cache and run new: 1. repbox with reg and 2. metastudy"))
  }

  return(list(ok=TRUE))
}

dap_and_cache_remove_from_project = function(project.dir) {
  restore.point("dap_and_cache_remove_from_project")
  dap.file = file.path(project.dir,"metareg/dap/stata/dap.Rds")
  extra.cache.dir = paste0(project.dir, "/metareg/dap/stata/extra_cache")
  cache.dir = paste0(project.dir,"/metareg/dap/stata/cache")

  if (file.exists(dap.file)) {
    file.remove(dap.file)
  }
  cache.files = list.files(cache.dir,full.names = TRUE)
  if (length(cache.files)>0) file.remove(cache.files)
  cache.files = list.files(extra.cache.dir,full.names = TRUE)
  if (length(cache.files)>0) file.remove(cache.files)
}

dap_and_cache_remove_from_project = function(project.dir) {
  restore.point("dap_and_cache_remove_from_project")
  dap.file = file.path(project.dir,"metareg/dap/stata/dap.Rds")
  extra.cache.dir = paste0(project.dir, "/metareg/dap/stata/extra_cache")
  cache.dir = paste0(project.dir,"/metareg/dap/stata/cache")

  if (file.exists(dap.file)) {
    file.remove(dap.file)
  }
  cache.files = list.files(cache.dir,full.names = TRUE)
  if (length(cache.files)>0) file.remove(cache.files)
  cache.files = list.files(extra.cache.dir,full.names = TRUE)
  if (length(cache.files)>0) file.remove(cache.files)

}


# Will be called at the beginning of mr_run
dap_check_all_cache_files_exist = function(project.dir, step.df) {
  restore.point("dap_check_all_cache_files_exist")
  if (NROW(step.df)==0) return(list(ok=TRUE))
  cache.rows = which(step.df$cache)
  cache_df = mr_get_cache_df(project.dir)
  ok = all(step.df$step[cache.rows] %in% cache_df$step)
  if (ok) {
    return(list(ok=TRUE))
  }

  list(ok=FALSE, has.any.cache.files=NROW(cache_df)>0)
}



# TO DO: Improve cache interface in repboxReg
mr_get_cache_df = function(project.dir) {
  extra.cache.dir = file.path(project.dir, "/metareg/dap/stata/extra_cache")
  extra.cache.files = list.files(extra.cache.dir,glob2rx("step_*.dta"), full.names = TRUE)


  cache.dir = paste0(project.dir,"/metareg/dap/stata/cache")
  cache.files = list.files(cache.dir, "*.dta",full.names = TRUE)

  df = bind_rows(
    tibble(extra_cache=rep(TRUE, length(extra.cache.files)),file = extra.cache.files),
    tibble(extra_cache=rep(FALSE, length(cache.files)), file = cache.files)
  ) %>%
    mutate(
      base = basename(file),
      step = str.between(base,"step_",".dta") %>% as.integer()
    ) %>%
    select(step, everything())

  df
}


mr_get_cache_file = function(project.dir=project_dir, step, check.exists=FALSE) {
  restore.point("mr_get_cache_file")
  project.dir = standardizePath(project.dir)

  extra.cache.file = paste0(project.dir, "/metareg/dap/stata/extra_cache/step_", step,".dta")
  if (file.exists(extra.cache.file)) {
    return(extra.cache.file)
  }

  cache.file = paste0(project.dir, "/metareg/dap/stata/cache/step_", step,".dta")
  if (file.exists(cache.file)) {
    return(cache.file)
  }
  stop(paste0("No data cache file exists for ", project.dir, " step ", step))
}




# Make extra cache assuming the Stata path code works
# but the R code does not run (e.g.  due to )

mr_make_extra_cache = function(mr, step) {
  restore.point("make_extra_cache")
  code.file = write_extra_cache_code(mr, step)
  run.stata.do(code.file)

  cache.dir = paste0(mr$project.dir,"/metareg/dap/stata/extra_cache")
  cache.file = file.path(cache.dir, paste0("step_", step, ".dta"))
  if (file.exists(cache.file)) {
    return(cache.file)
  }
  return(NULL)
}

mr_load_extra_cache = function(mr, step, cache.file = NULL) {
  if (is.null(cache.file)) {
    cache.file = paste0(mr$project.dir,"/metareg/extra_cache/step_", step, ".dta")
  }
  if (file.exists(cache.file)) {
    return(haven::read_dta(cache.file))
  }
  return(NULL)
}

write_extra_cache_code = function(mr, step, code.file=paste0(mr$project.dir,"/metareg/dap/stata/extra_cache/step_",step,".do"), ...) {
  restore.point("mr_write_path_stata_code")

  dap = mr
  txt = ""
  astep = first(dap$path.df$astep[dap$path.df$step == step])

  path = dap$path.df[dap$path.df$astep == astep & dap$path.df$step <= step,]

  # 1. Get data file
  file = mr_get_cache_file(mr$project.dir, path$step[1])
  txt = paste0(txt,'\nuse "', file,'", clear\n')

  # 2. code for all data modification steps
  steps = path$step
  txt = paste0(txt,"\n", paste0(mr$step.df$stata_code[steps], collapse = "\n"))

  # 3. store extra cache
  cache.dir = paste0(mr$project.dir,"/metareg/dap/stata/extra_cache")
  if (!dir.exists(cache.dir)) {
    dir.create(cache.dir,recursive = TRUE)
  }
  cache.file = file.path(cache.dir, paste0("step_", step, ".dta"))
  txt = paste0(txt,'\nsave "', cache.file,'", replace\n')

  writeLines(txt, code.file)
  invisible(code.file)
}

mr_get_extra_infeasible_steps = function(project.dir=NULL, inf.dir = paste0(mr$project.dir,"/metareg/extra_infeasible")) {
  restore.point("mr_get_extra_infeasible_steps")
  if (!dir.exists(inf.dir)) return(NULL)
  files = c(
    list.files(inf.dir, glob2rx("*.inf"),full.names = TRUE)
  )
  base = basename(files)
  steps = str.between(base,"step_",".")
  as.integer(steps)
}



mr_get_extra_cache_files = function(project.dir=NULL, cache.dir = paste0(mr$project.dir,"/metareg/dap/stata/extra_cache")) {
  restore.point("mr_get_extra_cache_files")
  if (!dir.exists(cache.dir)) return(NULL)
  files = c(
    list.files(cache.dir, glob2rx("*.dta"),full.names = TRUE)
  )
  base = basename(files)
  steps = str.between(base,"step_",".") %>% as.integer()
  tibble(step = step, cache_file = files)
}
