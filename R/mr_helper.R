# Some metareg helper functions


mr_base_variant = function(mr) {
  mr$repdb$reg
}

mr_uses_no_reg = function(mr) {
  asteps = mr_get_asteps(mr)
  length(asteps)==0
}

#' Specify which analysis steps are considered in this metareg study
#'
#' @param x is a data frame that
#'    has columns "step" and a logical column "use_step".
#'    Such a data frame will typically be a
#'    mutated version of reg_df where the colum "use_reg" is added.
mr_specify_used_reg = function(mr, reg_df) {
  if (NROW(reg_df)==0) {
    mr$used_steps = integer(0)
    return(mr)
  }
  if (!all(c("step","use_reg") %in% names(reg_df))) {
    stop("If you provide a data frame it must have the columns 'step' and 'use_reg' (TRUE / FALSE).")
  }
  mr$used_steps = reg_df$step[reg_df$use_reg]
  mr
}

mr_projects_have_file = function(parent.dir, files) {
  restore.point("mr_projects_have_file")

  mr_for_all_projects(parent.dir, verbose=FALSE, function(project_dir) {
    res = tibble(project = basename(project_dir))
    for (file in files) {
      base = basename(file)
      res[[base]] = file.exists(file.path(project_dir, file))
    }
    res
  }) %>% bind_rows()

}


#' Call a function for projects in parent.dir
mr_for_all_projects = function(parent.dir, fun, project.dirs, verbose=TRUE, stop.on.error = TRUE, store.results=TRUE, just.num = NULL) {
  if (missing(project.dirs)) {
    project.dirs = list.dirs(parent.dir,recursive = FALSE)
  }
  if (!is.null(just.num)) {
    project.dirs = project.dirs[just.num]
  }

  res = lapply(seq_along(project.dirs), function(i) {
    project_dir = project.dirs[i]
    if (verbose) {
      cat("\n", i, " of ", length(project.dirs), " ", basename(project), "\n")
    }
    if (stop.on.error) {
      res = fun(project_dir)
    } else {
      res = try(fun(project_dir))
    }
    if (store.results) return(res)
    return(NULL)
  })
  if (store.results) {
    names(res) = basename(project.dirs)
    return(res)
  }
  return(NULL)

}
