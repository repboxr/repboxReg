
vi_to_felm_formula = function(vi, clustervar=NULL, min_fe_level = 9, as.character = FALSE) {
  restore.point("vi_to_felm_formula")
  depvars = vi$var[vi$role=="dep"]
  form = paste0(paste0(depvars, collapse=" + "), " ~ ")

  terms = vi_to_lm_iv_felm_terms(vi, min_fe_level=min_fe_level)

  cols = c("role", "is_ia", "main_pos", "as_fe","x_expr","fe_expr")
  terms = bind_rows(ia[,cols], no_ia[cols]) %>%
    arrange(main_pos)

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
