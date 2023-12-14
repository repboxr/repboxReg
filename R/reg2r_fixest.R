mr_fixest = function(command, fml, data, se.info, fixest.args = list(), summary.args = list(), call.summary = TRUE, use.sandwich=NA) {
  restore.point("mr_fixest")
  library(fixest)
  vcov = "iid"
  if (is.na(use.sandwich))
    use.sandwich = se.info$stata_se %in% c("newey","##robust","hc0","hc1","hc2","hc3","hc4","hc5")
  if (use.sandwich) {
    library(sandwich)
    vcov = "iid"
    call.summary = TRUE
  } else if (se.info$stata_se == "robust") {
    vcov = "HC1"
  } else if (se.info$stata_se == "cluster") {
    # Specify in summary
    vcov = as.formula(paste0("cluster ~ ", paste0(se.info$clustervar[[1]], collapse = "+")))
  }
  fixest.args = c(list(fml = fml,data=data, vcov=vcov), fixest.args)

  reg = do.call(command, fixest.args)
  if (!call.summary) return(reg)
  if (use.sandwich) {
    sandwich.args = c(list(x=reg, data=data), se.info$sandwich_opts[[1]])
    vcov = do.call(se.info$sandwich_fun, sandwich.args)
    summary.args = c(list(.vcov=vcov), summary.args)
  }
  summary.args = c(list(object=reg), summary.args)

  # if (se.info$stata_se == "cluster") {
  #   summary.args = c(list(vcov=vcov_cluster(reg, cluster = se.info$clustervar[[1]])), summary.args)
  # }
  sum = do.call("summary", summary.args)
  sum
}



# Got problems with standard errors in summary
# Also used old fixest version: se is now deprecated
mr_fixest_old = function(command, fml, data, se.info, fixest.args = list(), summary.args = list(), call.summary = TRUE, use.sandwich=NA) {
  restore.point("mr_fixest_old")
  library(fixest)
  fixest.args = c(list(fml = fml,data=data), fixest.args)
  if (!has.col(fixest.args,"se") & !has.col(fixest.args,"vcov")) {
    fixest.args$vcov="hetero"
  }
  reg = do.call(command, fixest.args)
  if (!call.summary) return(reg)
  if (is.na(use.sandwich))
    use.sandwich = se.info$stata_se %in% c("newey","##robust")
  if (use.sandwich) {
    library(sandwich)
    sandwich.args = c(list(x=reg, data=data), se.info$sandwich_opts[[1]])
    vcov = do.call(se.opts$sandwich_fun, sandwich.args)
    summary.args = c(list(object=reg,.vcov=vcov), summary.args)
  } else if (se.info$stata_se == "robust") {
    summary.args = c(list(object=reg,se="hetero", summary.args))
  } else if (se.info$stata_se == "cluster") {
    summary.args = c(list(object=reg,se="cluster", cluster = se.info$clustervar[[1]]), summary.args)
  } else {
    summary.args = c(list(object=reg), summary.args)
  }
  if (has.col(summary.args,"se")) {
    summary.args = rename.col(summary.args,"se","vcov")
  }
  sum = do.call("summary", summary.args)
  sum
}

make_fixest_call = function(reg, min_fe_level = 50, call.summary = TRUE,...) {
  restore.point("make_fixest_call")
  library(fixest)
  fml = vi_to_fixest_formula(reg$vi[[1]], min_fe_level)
  se.info = reg$se.info[[1]]
  command = "feols"

  fixest.args = list()
  if (reg$cmd == "ppmlhdfe") {
    command = "fepos"
  } else if (reg$cmd %in% c("logit","xtlogit")) {
    command = "feglm"
    fixest.args = list(family=binomial())
  } else if (reg$cmd %in% c("probit","xtprobit")) {
    command = "feglm"
    fixest.args = list(family=binomial(link = "probit"))
  }
  if (!is.empty(reg$weights_var)) {
    fixest.args$weights = as.formula(paste0("~", reg$weights_var))
  }
  list(
    command = "mr_fixest",
    args = list(command = command, fml = fml, se.info=se.info, call.summary = call.summary, fixest.args = fixest.args),
    ti = reg_translation_info(reg$cmd, command)
  )
}

vi_to_fixest_formula = function(vi, min_fe_level = 9, as.character = FALSE) {
  restore.point("vi_to_fixest_formula")

  # Add time series prefixes like L2. or d. to variable name
  # those variables were manually added
  rows = which(toupper(substring(vi$prefix,1,1)) %in% c("D","L","F","S"))
  vi$var[rows] = paste0(vi$prefix[rows],".",vi$var[rows])

  depvars = vi$var[vi$role=="dep"]
  form = paste0(paste0(depvars, collapse=" + "), " ~ ")
  # In stata x variables starting with o. like o.var will be omitted
  vi = vi %>%
    filter(!is.true(prefix=="o"))


  # TO DO: Specify whether in interaction A*B also A and B
  #        should be included or not.
  ia = vi %>%
    filter(is_ia) %>%
    group_by(is_ia, ia_cterm, role, ia_num, main_pos) %>%
    arrange(desc(is_fe), desc(class=="dummy")) %>%
    summarize(
      ia_distinct_num = prod(distinct_num),
      ia_type = case_when(
        all(!is_fe & class=="dummy") ~ "dummies",
        all(!is_fe) ~ "numeric",
        all(is_fe | class=="dummy") ~ "fe",
        ia_num == 2 & class[1] == "dummy" & (!is_fe[2] & class[2] != "dummy") ~ "dummy_numeric",
        ia_num == 2 & is_fe[1] & !is_fe[2] ~ "fe_numeric",
        TRUE ~ "unknown"
      )[1],
      fe_expr = case_when(
        ia_type %in% c("dummies","numeric") ~ NA_character_,
        ia_type == "fe" ~ paste0(var, collapse="^"),
        ia_type == "fe_numeric" ~ paste0(var[1],"[",var[2],"]"),
        ia_type == "unknown" ~ NA_character_,
        TRUE ~ NA_character_
      )[1],
      x_expr = paste0(ifelse(!is_fe, var, paste0("factor(",var,")")), collapse= if (isTRUE(first(add_main_effects))) "*" else ":"),
      # x_expr = case_when(
      #   ia_type %in% c("dummies","numeric") ~ paste0("I(", paste0(var, collapse="*"),")"),
      #   ia_type == "fe" & ia_num==2 ~ paste0("i(",var[1],",i.",var[2],")"),
      #   ia_type == "fe_numeric" ~ paste0("i(",var[1],",",var[2],")"),
      #   ia_type == "fe" ~ paste0(ifelse(is_factor, var, paste0("factor(",var,")")), collapse="*"),
      #   ia_type == "unknown" ~ NA_character_
      # )[1],
      as_fe = case_when(
        role != "exo" ~ FALSE,
        is.na(fe_expr) ~ FALSE,
        option %in% c("absorb","fe") ~ TRUE,
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
        is_fe ~ paste0("factor(", var,")"),
        TRUE ~ var
      ),
      as_fe = case_when(
        !is_fe ~ FALSE,
        role != "exo" ~ FALSE,
        option %in% c("absorb","fe") ~ TRUE,
        distinct_num < min_fe_level ~ FALSE,
        TRUE ~ TRUE
      )
    )

  cols = c("role", "is_ia", "main_pos", "as_fe","x_expr","fe_expr")
  terms = bind_rows(ia[,cols], no_ia[cols]) %>%
    arrange(main_pos)

  # Exogeneous x that are no FE
  rows = which(terms$role == "exo" & terms$as_fe == FALSE)
  if (sum(rows)>0) {
    form = paste0(form, paste0(terms$x_expr[rows], collapse= " + "))
  }

  # Exogeneous x as FE
  rows = which(terms$role == "exo" & terms$as_fe == TRUE)
  if (sum(rows)>0) {
    form = paste0(form, " | ",paste0(terms$fe_expr[rows], collapse= " + "))
  }

  # Endogeneous x and instruments (never FE)
  rows = which(terms$role == "endo")
  if (sum(rows)>0) {
    form = paste0(form, " | ",paste0(terms$x_expr[rows], collapse= " + "))
  }
  rows = which(terms$role == "instr")
  if (sum(rows)>0) {
    form = paste0(form, " ~ ",paste0(terms$x_expr[rows], collapse= " + "))
  }

  if (as.character) return(form)
  as.formula(form)
}

