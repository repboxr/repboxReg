
mr_save_parcels = function(mr, parcels) {
  mr$saved_parcels = parcels
  repdb_save_parcels(parcels, mr$repdb_out_dir)
  mr
}

mr_set_step_result = function(mr, step, ...) {
  args = list(...)
  restore.point("mr_set_step_result")
  if (is.null(mr$step_results)) {
    mr$step_results = tibble(step=mr_get_asteps(mr))
  }
  for (var in names(args)) {
    if (!var %in% names(mr$step_results)) {
      mr$step_results[[var]] = vector("list",NROW(mr$step_results))
    }
    step_row = match(step, mr$step_result$step)
    mr$step_results[[var]][[step_row]] = args[[var]]
  }
  mr
}

mr_set_result = function(mr, ...) {
  args = list(...)
  restore.point("mr_set_result")
  if (is.null(mr[["results"]])) {
    mr$results = list()
  }
  for (var in names(args)) {
    mr$results[[var]] = args[[var]]
  }
  mr
}

mr_get_result = function(mr, name) {
  mr$results[[name]]
}

mr_get_steps_result = function(mr, name, add_step=as_df, as_df=TRUE) {
  restore.point("mr_get_steps_result")
  if (!as_df) {
    return(mr$step_results[[name]])
  }
  if (add_step) {
    df = bind_rows_with_parent_fields(mr$step_results, name, "step")
  } else {
    df = bind_rows(mr$step_results[[name]])
  }
  df

}
