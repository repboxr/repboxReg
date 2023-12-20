# Convert to calls from lm and iv and general helper function
# to create R regression commands and execute them

reg.cmds.without.r.translation = function() {
  c("dprobit","tobit")
}

make_reg_call = function(reg,...) {
  if (reg$cmd %in% reg.cmds.without.r.translation()) {
    return(list(
      dont_run = TRUE,
      ti = reg_translation_info(s_cmd=reg$cmd, feasible=FALSE, problem="not_implemented")
    ))
  }

  if (reg$cmd == "qreg") {
    reg_call = make_quantreg_call(reg, ...)
  } else if (reg$cmd %in% c("dprobit")) {
    reg_call = make_mfx_call(reg,...)
  } else if (reg$cmd %in% "tobit") {
    reg_call = make_tobit_call(reg,...)
  } else {
    # As default use fixest
    reg_call = make_fixest_call(reg,...)
  }
  restore.point("stata.to.r.reg.call")
  reg_call
}

get.reg.opt = function(reg, opt) {
  row = which(reg$opts.df[[1]]$opt == opt)
  if (length(row)==0) return(NULL)
  reg$opts.df[[1]][row,]
}

eval_reg = function(call.li, ..., envir=parent.frame()) {
  restore.point("eval_reg_call")
  extra.args = list(...)
  args = call.li$args
  args[names(extra.args)] = extra.args
  do.call(call.li$command, args, envir = envir)
}





make_default_reg_call = function(reg,command, allow.robust=TRUE, max.cluster=0, allow.weights=TRUE, robust.arg = "robust", cluster.arg = "cluster", ...) {
  restore.point("make_default_reg_call")

  ti = reg_translation_info(reg$cmd, command)
  vi = reg$vi[[1]]
  formula = vi_to_lm_iv_formula(vi)
  args = list(formula = formula)
  if (!is_empty(reg$weights_var)) {
    if (!allow.weights) {
      ti$weights_ok = FALSE
    } else {
      args$weights = as.formula(paste0("~", reg$weights_var))
    }
  }

  se.info = reg$se.info[[1]]
  if (se.info$stata_se=="robust") {
    if (!allow.robust) {
      ti$se_ok=FALSE
      ti$se_type="robust"
    } else if (!is.null(robust.arg)) {
      args$robust = TRUE
    }
  } else if (se.info$stata_se=="cluster") {
    cluster = se.info$clustervar[[1]]
    if (length(cluster) > max.cluster) {
      ti$se_ok=FALSE
      ti$se_type = "cluster"
    } else if (!is.null(cluster.arg)) {
      args[[cluster.arg]] = cluster
    }
  }
  list(
    dont_run = isTRUE(!ti$feasible),
    command = command,
    args=args,
    ti = ti
  )
}

reg_translation_info = function(s_cmd, r_cmd="",feasible = TRUE, problem="", problem_descr="", se_ok=TRUE, se_type="", se_descr="", weights_ok=TRUE) {
  tibble(s_cmd, r_cmd, feasible, problem, problem_descr, se_ok, se_type, se_descr, weights_ok)
}




make_lm_call = function(reg, command="lm") {
  restore.point("make_lm_call")
  vi = reg$vi[[1]]
  reg_call = make_default_reg_call(reg, command)
  reg_call
}

make_lm_iv_call = function(reg) {
  restore.point("make_lm_iv_call")
  vi = reg$vi[[1]]
  is_iv = any(vi$role == "instr")
  command = if(is_iv) "ivreg" else "lm"
  reg_call = make_default_reg_call(reg, command)
  reg_call
}

vi_to_lm_iv_formula = function(vi, min_fe_level = Inf, as.character = FALSE) {
  restore.point("vi_to_lm_iv_formula")

  # Add time series prefixes like L2. or d. to variable name
  # those variables were manually added
  rows = which(toupper(substring(vi$prefix,1,1)) %in% c("D","L","F","S"))
  vi$var[rows] = paste0(vi$prefix[rows],vi$var[rows])

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

  # instruments
  rows = which(terms$role == "instr")
  if (sum(rows)>0) {
    form = paste0(form, " | ",paste0(terms$x_expr[c(rows, exo.rows)], collapse= " + "))
  }

  if (as.character) return(form)
  as.formula(form)

}


# Deal with factor variables and interaction effects
# in default manner
vi_to_lm_felm_terms = function(vi, min_fe_level = Inf) {
  restore.point("vi_to_lm_iv_terms")

  # In stata x variables starting with o. like o.var will be omitted
  vi = vi %>%
    filter(!is.true(prefix=="o"))


  ia = vi %>%
    filter(is_ia) %>%
    vi.add.ia.type() %>%
    group_by(is_ia, ia_cterm, role, ia_num, main_pos) %>%
    summarize(
      ia_type = first(ia_type),
      fe_expr = case_when(
        ia_type %in% c("dummies","numeric") ~ NA_character_,
        ia_type == "fe" ~ paste0(var, collapse="#"),
        ia_type == "fe_numeric" ~ paste0(var[1],"#",var[2]),
        ia_type == "unknown" ~ NA_character_,
        TRUE ~ NA_character_
      )[1],
      x_expr = paste0(ifelse(!is_fe, var, paste0("factor(",var,")")), collapse= if (isTRUE(first(add_main_effects))) "*" else "#"),
      # x_expr = case_when(
      #   ia_type %in% c("dummies","numeric") ~ paste0("I(", paste0(var, collapse="*"),")"),
      #   ia_type == "fe" ~ paste0(ifelse(is_factor, var, paste0("fe(",var,")")), collapse="*"),
      #   ia_type == "fe_numeric" ~ paste0(ifelse(is_factor[1], var[1], paste0("fe(",var[1],")")),"#",var[2]),
      #   ia_type == "unknown" ~ NA_character_
      # )[1],
      as_fe = case_when(
        role != "exo" ~ FALSE,
        is.na(fe_expr) ~ FALSE,
        ia_type=="fe_numeric" & distinct_num[1] < min_fe_level ~ FALSE,
        ia_distinct_num < min_fe_level ~ FALSE,
        TRUE ~ TRUE
      )[1]
    ) %>%
    ungroup()

  no_ia = vi %>%
    filter(!is_ia) %>%
    mutate(
      fe_expr = var,
      x_expr = case_when(
        is_fe & !is_factor ~ paste0("fe(", var,")"),
        TRUE ~ var
      ),
      as_fe = case_when(
        !is_fe ~ FALSE,
        role != "exo" ~ FALSE,
        distinct_num < min_fe_level ~ FALSE,
        TRUE ~ TRUE
      )
    )

  cols = c("role", "is_ia", "main_pos", "as_fe","x_expr","fe_expr")
  terms = bind_rows(ia[,cols], no_ia[cols]) %>%
    arrange(main_pos)
}


# Does not yet work nicely
make_tobit_call = function(reg, ...) {
  restore.point("make_tobit_call")
  library(AER)
  reg_call = make_default_reg_call(reg,"tobit", max.cluster=1)
  reg_call
}

tidy.tobit = function(tobit, ...) {
  mat = coeftest(tobit)
  tibble(term = rownames(mat), estimate=mat[,1], std.error = mat[,2], statistic=mat[,3], p.value = mat[,4])
}


# Does not yet work nicely
make_mfx_call = function(reg,...) {
  restore.point("make_mfx_call")

  if (reg$cmd=="dprobit") {
    command = "probitmfx"
  }
  reg_call = make_default_reg_call(reg, command, max.cluster = 2, cluster.arg = NULL)
  cluster = reg$se.info$clustervar[[1]]
  if (length(cluster)>=1) {
    reg_call$args$clustervar1 = cluster[1]
  }
  if (length(cluster)>=2) {
    reg_call$args$clustervar2 = cluster[2]
  }

  reg_call$args$atmean = TRUE
  reg_call
}



