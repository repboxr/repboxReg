# Key idea: mr_finish is a helper function
# that generally finishes up a meta_study

mr_finish = function(mr, show_msg=TRUE, save_header = mr$opts$save.header) {
  restore.point("mr_finish")

  if (mr_has_problem(mr)) {
    # TO DO: Possibly use repbox_problem instead
    problem.file = file.path(mr$out_dir,"problem.txt")
    writeLines(mr[["problem"]], problem.file)
    return(mr)
  }

  header = mr_get_header(mr)
  if (is.null(header) & !isTRUE(mr$did_run)) {
    mr = mr_set_no_run_header(mr)
    header = mr_get_header(mr)
  } else if (is.null(header)) {
    stop("No header was found in mr. Make sure that you call mr_set_header, e.g. in your study_agg_fun or afterward before you run mr_finish.")
  }

  msg = repdb_get_check_msg_from_header(header)

  if (header$num_problem >0) {
    repbox_problem(type="reg_problem",msg=msg, fail_action="msg")
  } else if (show_msg) {
    cat(paste0("\n",msg),"\n")
  }

  if (save_header) {
    mr_save_header(mr)
  }

  invisible(mr)
}
