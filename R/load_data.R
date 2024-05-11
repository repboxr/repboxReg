# Functions to load the data of a regression
# This is a simplified version of the mr_run apparatus

example = function() {
  project_dir = "~/repbox/projects_gha/aejpol_10_3_13"
  step = 3

  res = load_reg_data(project_dir, step)
}

load_reg_data = function(project_dir, step, parcels=list(), just_org_dat = FALSE) {
  restore.point("load_reg_data")
  dap = repboxReg::get.project.dap(project_dir)

  # 1. Get path steps
  path.df = dap$path.df
  path.df = path.df[path.df$astep==step,]

  cache_df = find_closest_cache_file(project_dir, path.df$step)
  if (NROW(cache_df)==0) {
    cat(paste0("\nNo cache store to create data set for step ", step, " in ", project_dir))
    return(NULL)
  }

  path.df = path.df[path.df$step >= cache_df$step,]

  inds = seq_len(NROW(path.df))

  # 2. Load data cache
  ind = 1
  source_step = path.df$step[ind]

  dat = haven::read_dta(cache_df$cache_file)

  # 2. Modification steps
  mod_inds = setdiff(inds, c(1,max(inds)))

  ind = 2
  for (ind in mod_inds) {
    cur_step = path.df$step[ind]
    env = new.env(parent = METAREG_STATA_ENV)

    env$. = NA
    env$dat = dat

    s = dap$step.df[dap$step.df$step == cur_step,]

    rcode = stata.to.r(sep.lines(s$stata_code))
    res = try(parse_eval(rcode, env))
    if (is(res, "try-error")) {
      cat("\nError when modifying data set.")
      return(NULL)
    }
    dat = env$dat
  }

  if (just_org_dat) return(dat)

  # 3. Last analysis step can be skipped...

  astep = max(path.df$astep)
  reg = mr_get_reg_info(dap, astep,dat)
  org_dat = dat
  dat = mr_adapt_data_for_reg(project_dir, astep, reg, dat)

  list(dat=dat, org_dat=org_dat, reg=reg)
}

find_closest_cache_file = function(project_dir, steps) {
  restore.point("find_closest_cache_file")
  cache_file = mr_get_cache_file(project_dir, min(steps))
  if (file.exists(cache_file)) {
    cache_tibble = tibble(step = min(steps), cache_file = cache_file)
  } else {
    cache_tibble = NULL
  }

  cache_df = bind_rows(
    cache_tibble,
    mr_get_extra_cache_files(project_dir,cache.dir = paste0(project_dir,"/metareg/dap/stata/extra_cache")),
    mr_get_extra_cache_files(project_dir,cache.dir = paste0(project_dir,"/metareg/extra_cache"))
  )
  if (NROW(cache_df)==0) return(NULL)
  cache_df = cache_df[cache_df$step %in% steps,]
  if (NROW(cache_df)==0) return(NULL)

  cache_df = cache_df[cache_df$step == max(cache_df$step),]
  cache_df = cache_df[1,]

}
