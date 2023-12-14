# GLM and variants

# Extended glm
mr_glm = function(formula, data, type=c("", "probit","logit")[[1]], se.info, glm.args = list(), margins.args = list(), margins=FALSE) {
  restore.point("mr_glm")
  library(fixest)
  glm.args = c(list(formula = formula,data=data), glm.args)

  if (type =="probit") {
    glm.args$family = binomial(link="probit")
  } else if (type=="logit") {
    glm.args$family = binomial(link="logit")
  }

  reg = do.call("glm", glm.args)

  use.sandwich = !se.info$iid_se
  if (use.sandwich) {
    library(sandwich)
    sandwich_args = c(list(x=reg, data=data), se.info$sandwich_opts[[1]])
    vcov = do.call(se.info$sandwich_fun, sandwich_args)
  }
  if (margins) {
    margins.args$model = reg
    if (use.sandwich) {
      margins.args$.vcov = vcov
    }
    res = do.call("margins", margins.args)
  } else if (use.sandwich) {
    res = lmtest::coeftest(reg,vcov. = vcov)
  } else {
    res = reg
  }
  res
}


make_glm_call = function(reg, margins=reg$cmd %in% c("dprobit"),...) {
  restore.point("make_glm_call")
  reg_call = make_default_reg_call(reg,"mr_glm")
  if (reg$cmd=="dprobit") {
    type = "probit"
  }
  reg_call$args = set.fields(reg_call$args, list(glm.args=list(), type=type, se="sandwich", se.info = reg$se.info[[1]]))
  reg_call

}

