quantreg_with_summary = function(command, formula, tau=0.5, data, se=NA, summary.args = list(), weights=NULL, call.summary = FALSE, no.warnings=TRUE) {
  restore.point("quantreg_with_summary")
  library(quantreg)
  quantreg.args = c(list(formula = formula,tau=tau,data=data))
  if (!is.null(weights)) {
    quantreg.args$weights = weights
  }

  if (no.warnings) {
    reg = suppressWarnings(do.call(command, quantreg.args))
  } else {
    reg = do.call(command, quantreg.args)
  }

  if (!call.summary) return(reg)

  summary.args$object=reg
  sum = suppressWarnings(do.call("summary", summary.args))
  return(sum)
}

make_quantreg_call = function(reg, call.summary = FALSE, ...) {
  restore.point("make_quantreg_call")

  formula = vi_to_lm_iv_formula(reg$vi[[1]])
  se.info = reg$se.info[[1]]
  command = "rq"

  weights = NULL
  if (!is_empty(reg$weights_var)) {
    weights = as.formula(paste0("~", reg$weights_var))
  }
  summary.args = list()

  opt = get.reg.opt(reg, "quantile")
  if (!is.null(opt)) {
    tau = as.numeric(opt$opt_args %>% trimws())
  } else {
    tau = 0.5
  }
  ti = reg_translation_info(reg$cmd, command)

  list(
    ti = ti,
    command = "quantreg_with_summary",
    args = list(command = command, formula = formula, tau = tau, se=NA, call.summary = call.summary, summary.args = summary.args)

  )
}


