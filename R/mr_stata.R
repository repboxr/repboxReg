# Run Metareg analyses that use stata code

example.metareg.stata = function() {
  project = "aejapp_13_3_7"

  project = "testsupp"
  project_dir = file.path("~/repbox/projects_reg",project)

  dap = get.project.dap(project_dir,add.run.df = TRUE)
  plot.dap(dap)
  step.df = dap$step.df

  stata_code_fun = function(mr, step, stata_code, ...) {
    restore.point("stata_code_fun")

    outfile = paste0(mr$step.dir, "/reg_", step, ".tsv")
    code = paste0(
      stata_code,
      '\n\nestout . using "',outfile,'", cells("b se t p ci_l ci_u") replace'
    )
    code
  }

  agg.fun = function(mr, ...) {
    restore.point("agg.fun")
    stata.res = mr_agg_estout_tsv(mr,prefix = "reg_")
    saveRDS(stata.res, file.path(mr$out_dir,"results.Rds"))
    stata.res
  }

  mr = mr_init_study(project_dir,metaid="base", stata_code_fun=stata_code_fun,study_agg_fun = agg.fun, dap=dap)
  set.seed(123)
  mr = mr_make_all_stata_code(mr, stata_code_fun, asteps = sample(mr_get_asteps(mr),1))
  mr = mr_run_all_stata_code(mr)

  res = agg.fun(mr)
}

# Runs the metareg Stata analysis with code generated by
# the study's stata_code_fun
# Unlike the original repbox runs, we typically want to
# stop the Stata run upon an error, which indicates
# a problem in the generated Stata code and should be known
# and possibly be debugged by the author.
mr_run_all_stata_code = function(mr, nostop = FALSE) {
  library(repboxStata)
  restore.point("mr_run_all_stata_code")
  code.dir = first.non.null(mr$stata_code.dir, file.path(mr$out_dir,"stata_code"))
  do.files = list.files(code.dir, glob2rx("*.do"), full.names = TRUE)
  for (do.file in do.files) {
    #cat("\nRun", do.file,"\n")
    run_stata_do(do.file, nostop = FALSE)
  }

  file = first(do.files)
  stata_check_df = lapply(do.files, function(file) {
    base = basename(file)
    step = str.between(base, "path_",".do") %>% as.integer()
    log_file = paste0(dirname(file),"/path_", step,".log")
    mr_check_stata_log_error(log_file, step=step)
  }) %>% bind_rows()

  mr$stata_check_df = stata_check_df

  mr
}

dap_scalar_df_to_stata_step_code = function(scalar.df, steps) {
  restore.point("dap_scalar_df_to_step_code")
  if (is.null(scalar.df)) return(rep("", length(steps)))
  code_df = scalar.df %>%
    filter(step %in% steps) %>%
    mutate(
      scalar_code = paste0("\nscalar ", scalar_var, " = ", ifelse(is_num, scalar_num_val, paste0('"', scalar_val,'"')), "\n")
    ) %>%
    group_by(step) %>%
    summarize(
      scalar_code = paste0(scalar_code, collapse="")
    )

  code = rep("", length(steps))
  inds = match(code_df$step, steps)
  code[inds] = code_df$scalar_code
  code
}


mr_make_all_stata_code = function(mr, stata_code_fun=mr$stata_code_fun, asteps = mr_get_asteps(mr), changes.data = TRUE, stata.preserve.always = mr$opts$stata.preserve.always) {
  restore.point("mr_make_all_stata_code")

  metaid = mr$metaid
  mr$stata.changes.data = changes.data

  path.df = mr$path.df %>%
    filter(astep %in% asteps) %>%
    group_by(astep) %>%
    mutate(path.len=n()) %>%
    ungroup()

  astep.df = path.df %>%
    select(astep) %>%
    unique() %>%
    mutate(parent.step =  mr$step.df$parent_step[astep])


  # Update step.df stata_code
  step.df = mr$step.df

  step.df$load_code = ""

  # Load data code
  source_steps = unique(path.df$source_step)
  load_code = sapply(source_steps, function(step) {
    file = mr_get_cache_file(mr$project_dir, step)
    paste0('use "', file,'", clear',"\n")
  })
  step.df$load_code[source_steps] = load_code
  rows = source_steps[step.df$step_type[source_steps]!="a"]
  step.df$stata_code[rows] = ""


  step = 4
  # Analysis code
  acode = sapply(asteps, function(step) {
    if (mr$opts$pass.repdb.info) {
      res = stata_code_fun(mr=mr, step=step, stata_code=step.df$stata_code[[step]], reg = mr_get_reg(mr, step))
    } else {
      res = stata_code_fun(mr=mr, step=step, stata_code=step.df$stata_code[[step]])
    }
    res = merge.lines(res)

    res = paste0("
******** Analysis step ", step," ************
", res,"
******** End analysis step ", step," ********
")
  })
  step.df$stata_code[asteps] = acode


  # Code that regenerates Stata scalars that are used in steps
  # we may duplicate the scalar generation if a particular scalar
  # is used in several steps. But that does not seem to be a big
  # problem
  step.df$scalar_code = dap_scalar_df_to_stata_step_code(mr[["scalar.df"]], step.df$step)

  #step.df$stata_code[source_steps] = load_code


  # Make or clear path.code.dir
  mr$stata_code.dir = path.code.dir = file.path(mr$out_dir,"stata_code")
  if (!dir.exists(path.code.dir)) {
    dir.create(path.code.dir)
  } else {
    path.code.files = list.files(path.code.dir,glob2rx("*.do"),full.names = TRUE)
    file.remove(path.code.files)
  }
  # Add adopath code
  #ado.code = adopath.injection.code(mr$project_dir, ado.dirs = get.ado.dirs())
  ado.code = adopath.injection.code(mr$project_dir)
  while(NROW(path.df) > 0) {
    # Pick (a) longest path from path.df
    .astep = first(path.df$astep[path.df$path.len == max(path.df$path.len)])
    steps = path.df$step[path.df$astep == .astep]

    casteps = astep.df$astep[astep.df$parent.step %in% steps]

    cat(paste0("\nWrite path_", .astep,".do with ", NROW(casteps), " analysis steps."))

    steps = sort(union(steps, casteps))

    # Add preserve restore around all but last analysis step
    # if analysis changes data set
    if (changes.data) {
      restore.steps  = setdiff(casteps, max(casteps))
      if (stata.preserve.always) {
        pr.steps = casteps
      } else {
        pr.steps = restore.steps
      }
      step.df$stata_code[pr.steps] = paste0(
"\npreserve\n",step.df$stata_code[pr.steps]
      )
      step.df$stata_code[restore.steps] = paste0(
        step.df$stata_code[restore.steps],"\nrestore\n"
      )
    }

    all.code = paste0(step.df$scalar_code[steps],step.df$load_code[steps],step.df$stata_code[steps])

    all.code = c(ado.code, all.code)

    # Save code file
    code.file = file.path(path.code.dir,paste0("path_", .astep,".do"))
    writeLines(all.code, code.file)


    # Remove used paths from path.df and astep.df
    path.df = path.df %>%
      filter(!astep %in% casteps)

    astep.df = astep.df %>%
      filter(!astep %in% casteps)
  }

  return(mr)
}

mr_write_path_stata_code = function(mr, astep=first(mr$path.df$astep), code.file=paste0(mr$project_dir,"/metareg/step_",astep,".do"), add.line.info = !is.null(run.df), run.df=mr$run.df,...) {
  restore.point("mr_write_path_stata_code")

  txt = "\n\nset more off\n"
  path = dap$path.df[dap$path.df$astep == astep,]
  # 1. Get data file
  step = path$step[1]
  file = mr_get_cache_file(mr$project_dir, step)
  txt = paste0(txt,'\nuse "', file,'", clear\n')

  # 2. code for all other steps data modification steps
  steps = path$step[-1]
  scalar_code = dap_scalar_df_to_stata_step_code(mr$scalar.df, steps)

  txt = paste0(txt,"\n", paste0(scalar_code, mr$step.df$stata_code[steps], collapse = "\n"))

  writeLines(txt, code.file)
  invisible(txt)
}

mr_check_stata_log_error = function(log_file, step=NA) {
  restore.point("mr_check_stata_log_error")
  #log_file = "~/repbox/projects_reg/testsupp/metareg/base/stata_code/path_5.log"
  if (!file.exists(log_file)) {
    return(tibble(step=step, stata_ok=TRUE, stata_err_code=-1, stata_err_msg="missing log file"))
  }
  txt = read_lines_from_tail(log_file, 10) %>% rev()
  str = txt[1]
  has_err = startsWith(str,"r(")
  if (!has_err) {
    return(tibble(step=step, stata_ok=TRUE, stata_err_code=NA_integer_, stata_err_msg=""))
  }
  err_code = str.between(str, "(",")") %>% as.integer()
  err_lines = which(txt==txt[1])
  if (length(err_lines)>1) {
    msg_lines = rev(intersect(err_lines[2]+(1:2),1:10))
  } else {
    msg_lines = NULL
  }
  if (length(msg_lines)>0) {
    err_msg = paste0(txt[msg_lines], collapse="\n")
  } else {
    err_msg = ""
  }
  tibble(step=step, stata_ok=FALSE,stata_err_code=err_code, stata_err_msg=err_msg)
}

# The function below is not currently used since
# when we save caches as .dta files, the xtset and tsset
# structure will be saved as part of the dta file
# We this don't need to add an xtset or tsset command.
make_xtset_code_from_step_df = function(step.df) {
  # Add xtset code
  step.df = (step.df[source_steps,]) %>%
    mutate(
      xtset_code = case_when(
        timevar != "" & panelvar != "" & !tdelta %in% c("","1") ~
          paste0("\nxtset ", panelvar," ", timevar,", delta(",tdelta,")"),
        timevar != "" & panelvar != "" ~
          paste0("\nxtset ", panelvar," ", timevar),
        panelvar != "" ~
          paste0("\nxtset ", panelvar),
        timevar != ""  & !tdelta %in% c("","1") ~
          paste0("\ntsset ", timevar,", delta(",tdelta,")"),
        timevar != "" ~
          paste0("\ntsset ", timevar),
        TRUE ~ ""
      )
    )
  step.df$xtset_code
}
