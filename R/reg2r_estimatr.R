# Convert to calls from estimatr package

vi_to_estimatr_call = function(vi, clustervar, se_type = "default", min_fe_level = Inf, as.character = FALSE) {
  restore.point("vi_to_estimatr_call")

  is.iv = any(vi$role == "instr")
  depvars = vi$var[vi$role=="dep"]
  form = paste0(paste0(depvars, collapse=" + "), " ~ ")

  terms = vi_to_lm_felm_terms(vi, min_fe_level = min_fe_level)

  # Exogeneous x that are no FE
  rows = which(terms$role == "exo" & terms$as_fe == FALSE)
  if (sum(rows)>0) {
    form = paste0(form, paste0(terms$x_expr[rows], collapse= " + "))
  } else {
    form = "0"
  }

  # Exogeneous x as FE
  rows = which(terms$role == "exo" & terms$as_fe == TRUE)
  if (sum(rows)>0) {
    form = paste0(form, " | ",paste0(terms$fe_expr[rows], collapse= " + "))
  } else {
    form = paste0(form, " | 0")
  }

  # endogenous instruments (never FE)
  rows = which(terms$role == "endo")
  if (sum(rows)>0) {
    form = paste0(form, " | (",paste0(terms$x_expr[rows], collapse= " | "))
  }  else {
    form = paste0(form," | 0")
  }
  rows = which(terms$role == "instr")
  if (sum(rows)>0) {
    form = paste0(form, " ~ ",paste0(terms$x_expr[rows], collapse= " + ")," ) ")
  }

  if (!is.null(clustervar)) {
    form = paste0(form, paste0(clustervar, collapse = " + "))
  }

  if (as.character) return(form)
  as.formula(form)

}

