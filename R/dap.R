# Determine Data Save Points in Stata Code
# Used by Metareg
#
# Goal: Find trade-off between time to prepare
#       data sets and number of save points.

# Current notation: node corresponds to an analysis row in run.df
# run.row to an arbitrary row in run.df
# step to a row in step.df
# stepid to a unique ID for each step, which can be computed before we know the index in step.d

example = function() {
  project = "aejapp_13_3_7"
  project = "testsupp"
  project_dir = file.path("~/repbox/projects_reg",project)

  #dirs = repboxMain::get.ejd.projects(has.repbox.dir = TRUE)
  #project_dir = sample(dirs,1)
  #project_dir

  rr = readRDS(file.path(project_dir,"repbox/stata/repbox_results.Rds"))
  run.df = rr$run.df
  #set.seed(1234)
  dap = make.dap(run.df)
  plot.dap(dap)
  store.data = dap.to.store.data(dap)
}


# Not all regressions did successfully generate output
# We will reduce the dap to only contain successful regressions
# Otherwise the unsuccessful regressions will cause problems later
# reasons for unsuccessful regressions could be missing data
dap_add_reg_ok = function(project_dir, dap=NULL) {
  dap.file = file.path(project_dir,"metareg/dap/stata/dap.Rds")
  if (is.null(dap)) {
    dap = readRDS(dap.file)
  }

  regtab.file = file.path(project_dir,"repbox/stata/regtab.Rds")
  org_regs = readRDS.or.null(regtab.file)

  org_regs = left_join(org_regs,select(dap$step.df, step, donum, line, counter), by=c("donum","line","counter"))

  dap$step.df = dap$step.df %>%
    mutate(
      reg_ok = case_when(
        step_type!="a" ~ NA,
        step %in% org_regs$step ~ TRUE,
        TRUE ~ FALSE
      )
    )
  saveRDS(dap, dap.file)
  invisible(dap)
}


get.project.dap = function(project_dir,make.if.missing=FALSE, add.run.df=FALSE) {
  restore.point("get.project.dap")
  dap.file = file.path(project_dir,"metareg/dap/stata/dap.Rds")
  if (file.exists(dap.file)) {
    dap = readRDS(dap.file)
    if (is.null(dap$version)) {
      stop("Invalid DAP without version. Please remove DAP and cache file from project ", project_dir, " and create them again.")
    }

    if (!add.run.df) return(dap)

    rr.file = file.path(project_dir,"repbox/stata/repbox_results.Rds")
    if (!file.exists(rr.file)) {
      cat("\nNo repbox results exists. Cannot make DAP.\n")
      return(NULL)
    }
    rr = readRDS(rr.file)
    run.df = rr$run.df
    dap$run.df = run.df
    return(dap)
  }
  if (!make.if.missing) return(NULL)

  # No dap file exists: make a new one
  rr.file = file.path(project_dir,"repbox/stata/repbox_results.Rds")
  if (!file.exists(rr.file)) {
    cat("\nNo repbox results exists. Cannot make DAP.\n")
    return(NULL)
  }

  rr = readRDS(rr.file)
  run.df = rr$run.df



  dotab = readRDS.or.null(file.path(project_dir,"repbox/stata/dotab.Rds"))

  restore.point("get.project.dap2")
  #if (all(run.df$timevar == "")) stop()


  dap = make.dap(run.df,dotab=dotab, extra.inf.dir = file.path(project_dir,"metareg/extra_infeasible"))
  if (is.null(dap)) return(NULL)

  dap.dir = dirname(dap.file)
  if (!dir.exists(dap.dir)) dir.create(dap.dir,recursive = TRUE)
  saveRDS(dap, dap.file)
  if (add.run.df)
    dap$run.df = run.df
  return(dap)
}

# Update only generate DAP for regressions that do not have missing data
make.dap = function(run.df, reg.nodes = which(run.df$is.regcmd & run.df$has.data), set.random.time=FALSE, find.infeasible.steps.fun = default.find.infeasible.steps, dotab=NULL, extra.inf.dir = NULL) {
  restore.point("make.dap")
  path.df = make.data.path.df(run.df,nodes = reg.nodes)

  if (is.null(path.df)) return(NULL)

  # Add lineid to detect
  # rows = path.df$step
  # path.df$lineid = paste0(run.df$donum[rows],":",run.df$line[rows])

  path.df = adapt_data_path_for_mod_regs(path.df, run.df)

  #plot.path.df(path.df)
  dap = path.df.to.dap(path.df, run.df)
  run.df$runid = seq_len(NROW(run.df))

  if (!is.null(dotab)) {
    run.df$dofile = dotab$dofile[run.df$donum]
  } else {
    run.df$dofile = NA
  }
  dap$step.df = left_join_overwrite(dap$step.df, select(run.df, runid, dofile, orgline, cmd, cmdline, timevar, panelvar, tdelta, donum, line, counter), by=c("donum","line", "counter"))


  dap$step.df$infeasible = find.infeasible.steps.fun(dap$step.df, path.df)

  # If extra cache files exist for some steps then
  # mark them as infeasible, so that the cache will be used
  if (!is.null(extra.inf.dir)) {
    extra.inf.steps = mr_get_extra_infeasible_steps(inf.dir=extra.inf.dir)
    dap$step.df$infeasible[extra.inf.steps] = TRUE
  }

  if (set.random.time) {
    warning("make.dap was called with set.random.time=TRUE. This should only be done for debugging purposes!!!!")
    dap$step.df$time = 10^runif(NROW(dap$step.df),-2,3.5)
  }
  dap = dap.add.cache.cost(dap)
  dap = determine.data.caches(dap)
  dap = dap.adapt.paths.to.caches(dap)

  dap$time_stamp = Sys.time()
  dap$version = 0
  dap
}


# Sometimes a regression command modifies the data set in a way that is required for later regressions.
# Example:
#
# xi: reg y i.i1
# reg y _Ii1*
#
# The 1st regression generates the _I* dummy variables
# The 2nd regression only works after the 1st is run
#
# Here we will add the 1st regression as a data modification step. But we will also mark it as always_cache
adapt_data_path_for_mod_regs = function(path.df, run.df) {
  restore.point("adapt_data_path_for_mod_regs")

  I.rows = which(path.df$step_type == "node" & has.substr(path.df$stata_code," _I"))
  if (length(I.rows)==0) {
    return(path.df)
  }
  xi.rows = which(stri_detect_regex(path.df$stata_code,"(^|[ \\:])[ ]*xi[ \\:]"))

  # If there is already an xi command in the path
  # of a regression tha uses _I variables
  # nothing needs to be done
  xi.nodes = unique(path.df$node[xi.rows])
  has.xi = path.df$node[I.rows] %in% xi.nodes
  I.rows = I.rows[!has.xi]

  if (length(I.rows)==0) return(path.df)

  # We now insert the last command with xi
  # into the paths for _I regressions
  Inodes = path.df$node[I.rows]
  path.df$.ROW = seq_rows(path.df)

  path_li = split(path.df, path.df$node)

  xi.steps = path.df$step[xi.rows]
  pdf = last(path_li)
  new_li = lapply(path_li, function(pdf) {
    node = first(pdf$node)
    if (!node %in% Inodes) {
      return(pdf)
    }
    valid.xi.steps = xi.steps[xi.steps > min(pdf$step) & xi.steps<node]
    if (length(valid.xi.steps)==0) return(pdf)
    xi.step = max(valid.xi.steps)
    xi.row = which(path.df$step == xi.step)
    rows = c(xi.row,pdf$.ROW)
    res_pdf = path.df[rows,] %>% arrange(step)
    xi.prow = which(res_pdf$.ROW==xi.row)
    #res_pdf$step_type[xi.prow] = "mod"
    res_pdf$node[xi.prow] = node
    res_pdf
  })
  res = bind_rows(new_li)
  select(res, -.ROW)
}

default.find.infeasible.steps = function(step.df, path.df=NULL) {
  restore.point("default.find.infeasible.steps")

  # We cannot translate stata commands that use r(...)
  # Because they rely on previously computed scalars
  # But we cannot match all scalars

  mod.rows = which(step.df$step_type=="mod")
  infeasible = rep(FALSE, NROW(step.df))

  # Find all mod steps that call a Stata function
  # except for functions we can modify

  #strings <- c("a", "test( sum(", "asum(", "sum(", "=sum(", "This is a sum(","amean(","=mean(haa")
  strings = step.df$stata_code[mod.rows]

  # 1. replace known function calls like mean(
  #known.fun = c("mean","max","sum","min","median")
  known.fun = METAREG_STATA_FUN_NAMES

  repl.pattern = paste0("(",paste0("\\b", known.fun,"\\(", collapse="|"),")")
  strings = gsub(repl.pattern,"REPL", strings)

  # 2. check if any function calls remain
  # those function calls cannot be translated
  fun.rx = "[a-zA-Z][a-zA-Z_0-9]*\\("
  found = grepl(fun.rx, strings,perl = TRUE)


  # # Also ignore commands that use xi:
  # found = found | stri_detect_regex(strings,"(^|[ \\:])[ ]*xi[ \\:]")


  # Old code: just searches for r( calls
  #rx = "(?<![a-zA-Z_0-9])r\\("
  #found = grepl(rx, step.df$stata_code[mod.rows],perl = TRUE)



  infeasible[mod.rows[found]] = TRUE


  # Also add regression nodes that use _I variables
  # rows = step.df$step_type=="a" & stri_detect_regex(step.df$stata_code, " _I[a-zA-Z]")
  # infeasible[rows] = TRUE

  # Also add regression nodes that are used as modification
  # nodes
  stepids = unique(path.df$step[path.df$step_type=="node" & path.df$node !=path.df$step])
  if (length(stepids)>0) {
    rows = match(stepids, step.df$stepid)
    infeasible[rows] = TRUE
  }


  infeasible
}

dap.add.cache.cost = function(dap, fixed_cost=10, factor=3) {
  restore.point("add.cache.cost.to.path.df")
  step.df = dap$step.df
  step.df$load_time = NA
  load.steps = which(step.df$step_type == "load")

  step.df$load_time[load.steps] = step.df$time[load.steps]
  step.df$load_time = step.df$load_time[step.df$root_step]
  step.df$cache_cost = fixed_cost + step.df$load_time*factor
  #step.df$cache_cost[step.df$need_cache] = -1
  dap$step.df = step.df
  dap

}

determine.data.caches = function(dap) {
  restore.point("determine.data.caches")

  step.df = dap$step.df
  step.df = step.df %>%
    mutate(
      cache_benefit = time,
      acc_benefit = 0,
      acc = FALSE,
      always_cache = infeasible | ifelse(need_cache | (cache_benefit > cache_cost & num.a.child > 0), TRUE, FALSE),
      cache = always_cache
    )

  # o stands for outer

  # Starting leaves: all terminal mod nodes
  oleaves = which(step.df$num.mod.child==0)

  while(TRUE) {
    oroots = setdiff(unique(step.df$parent_step[oleaves]), c(0,oleaves))
    if (length(oroots)==0) break
    # Only consider roots for which all children are oleaves
    use.root = sapply(oroots, function(oroot) all(step.df$children[[oroot]] %in% oleaves))
    oroots = oroots[use.root]
    if (length(oroots)==0) {
      restore.point("khsdhskd")
      stop("We did not find a valid oroot. But if oroot is not empty there should also be at least one valid oroot, i.e. it only has oleaves as children.")
      break
    }
    #cat("\nSolve oroots ", paste0(oroots, collapse=", "))

    # Now determine optimal data cache placement for each oroot
    # under the assumption that each regression leaf must be
    # served by a data cache.
    oroot = oroots[1]

    for (oroot in oroots) {
      # If oroot has no direct regression leaf
      # determine whether it would be better to cache all
      # children i.e. set acc=TRUE
      # or to place the cache at oroot instead (acc=FALSE)
      if (step.df$num.a.child[oroot]==0) {
        children = step.df$children[[oroot]]
        not_always_children = children[!step.df$always_cache[children]]
        # All children always cache.
        if (length(not_always_children)==0) {
          step.df$always_cache[oroot] = TRUE
          # But acc=TRUE means that only children cache
          #step.df$cache[oroot] = TRUE
          acc_bonus = step.df$cache_cost[oroot]
        } else {
          acc_bonus = sum(step.df$cache_benefit[not_always_children]-step.df$cache_cost[not_always_children]) + step.df$cache_cost[oroot]
        }
        if (isTRUE(acc_bonus > 0)) {
          step.df$acc[oroot] = TRUE
          step.df$acc_benefit[oroot] = acc_bonus
          step.df$cache_benefit[oroot] = step.df$time[oroot] + step.df$acc_benefit[oroot]
        }
      }
    }

    # The solved oroots now become leaves
    oleaves = c(oleaves, oroots)
  }

  # Now given the computed information in step.df,
  # we should move from inner to outer to determine the caches



  inner.to.outer.recursion = function(children, parent.acc, tree.dt) {
    #restore.point("huhsfsf")
    #stop()
    if (parent.acc) {
      #tree.dt$cache[children] = !tree.dt$acc[children]
      data.table::set(tree.dt,i=children, j="cache", value = !tree.dt$acc[children])
    }
    for (ind in children) {
      inner.to.outer.recursion(step.df$children[[ind]], step.df$acc[ind], tree.dt)
    }
  }
  tree.dt = as.data.table(step.df)
  roots = which(step.df$parent_step==0)
  inner.to.outer.recursion(roots, parent.acc=TRUE, tree.dt)

  step.df = as.data.frame(tree.dt)


  # Experimental: üossibly delete again
  # Code added for dealing with _I regressions
  rows = step.df$step_type=="a" & step.df$cache
  step.df$parent_step[rows] = step.df$step[rows]
  # end experimental

  dap$step.df = step.df
  dap
}

dap.adapt.paths.to.caches = function(dap) {
  restore.point("dap.adapt.paths.to.caches")
  step.df = dap$step.df
  cache.steps = which(step.df$cache)
  path.df = dap$path.df

  apath.df = path.df %>%
    group_by(astep) %>%
    mutate(
      source_ind = max(which(step %in% cache.steps))
    ) %>%
    mutate(source_step = step[source_ind]) %>%
    filter(1:n() >= source_ind) %>%
    select(astep, step, source_step) %>%
    ungroup()

  dap$path.df = apath.df
  dap
}

path.df.to.dap = function(path.df, run.df) {
  restore.point("path.df.to.dap")

  need_cache_cmds = stata.data.cmd.types()$need_cache

  org.path.df = path.df

  # path_df still has the old notation
  #
  # step = runid
  #
  # dstep is either run_id or if not NA the dataid_num
  #       equal data sets that are loaded twice will
  #       thus be loaded only once
  #
  # node  the runid of the analysis step
  # snode runid of the data loading step


  # step.df will combine the same steps from path_df
  #         later we will aggregate subsequent
  #         modification steps to single steps
  # NEW:    Only perform aggregation if the complete
  #         step group is in all paths.
  org.step.df = step.df = path.df %>%
    group_by(node) %>%  # by runid of analysis step
    mutate(
      next_step = lead(step),
      next_step_type = lead(step_type),
      cmd = run.df$cmd[step]
    ) %>%
    group_by(dstep, step, step_type, steplab) %>%
    summarize(
      cmd = first(cmd),
      need_cache = cmd %in% need_cache_cmds,
      time = first(time),
      next_step = first(next_step),
      next_step_type = first(next_step_type),
      num_next_step = n_distinct(next_step, na.rm=TRUE),
      add_next_step = first(is.true(step_type == "mod" & next_step_type == "mod" & num_next_step==1)),
      stata_code = first(stata_code),
      # New check in which paths the step is contained
      step_paths = paste0(sort(node), collapse="|")
    ) %>%
    ungroup() %>%
    mutate(
      # Updated: Only form a step_group if
      # all steps are contained in the same set of paths
      # otherwise we get problems in certain preserve/restore constructions
      add_next_step = add_next_step & is.true(lead(step) == next_step) & is.true(lead(step_paths)==step_paths),
      add_prev_step = is.true(lag(add_next_step)),
      step_group = cumsum(1-add_prev_step)
    ) %>%
    group_by(step_group) %>%
    mutate(cstep = max(step)) %>%
    ungroup()

  # To know which rows will be removed from path.df
  rem.df = filter(step.df, cstep != step)

  # Now compress mod and node steps on cstep
  mstep.df = org.step.df %>%
    filter(dstep >= 0) %>%
    group_by(cstep, step_type) %>%
    summarize(
      dstep = first(cstep),
      num_run_rows = n(),
      run_rows = list(step),
      max_run_row = max(step),
      need_cache = any(need_cache),
      time = sum(time),
      steplab = paste0(steplab, collapse="\n"),
      stata_code = paste0(stata_code, collapse="\n")
    ) %>%
    ungroup() %>%
    mutate(step_type = ifelse(step_type=="node","a",step_type))

  # Now compress load steps on dstep
  lstep.df = org.step.df %>%
    filter(dstep < 0)

  if (NROW(lstep.df) > 0) {
    lstep.df = lstep.df %>%
      group_by(dstep, step_type) %>%
      summarize(
        cstep = first(cstep),
        num_run_rows = n(),
        run_rows = list(step),
        max_run_row = suppressWarnings(max(step)),
        need_cache = any(need_cache),
        time = first(time),
        steplab = first(steplab),
        stata_code = first(stata_code)
      ) %>%
      ungroup()
  } else {
    lstep.df = suppressWarnings(lstep.df %>%
      group_by(dstep, step_type) %>%
      summarize(
        cstep = first(cstep),
        num_run_rows = n(),
        run_rows = list(step),
        max_run_row = suppressWarnings(max(step)),
        need_cache = any(need_cache),
        time = first(time),
        steplab = first(steplab),
        stata_code = first(stata_code)
      ) %>%
      ungroup())
  }

  step.df = bind_rows(lstep.df, mstep.df) %>%
    rename(stepid=dstep) %>%
    mutate(step = seq_len(n())) %>%
    select(-cstep) %>%
    select(step, step_type, everything()) %>%
    # store info to map to run.df
    # e.g. used to copy cached .dta files
    mutate(
      donum = run.df$donum[max_run_row],
      line = run.df$line[max_run_row],
      counter = run.df$counter[max_run_row]
    )

  path.df = org.path.df %>%
    select(node, stepid=dstep) %>%
    filter(stepid %in% step.df$stepid) %>%
    left_join(select(step.df, stepid, step), by="stepid") %>%
    group_by(node) %>%
    mutate(
      astep = step[n()]
    ) %>%
    ungroup() %>%
    select(astep, step, stepid)


  # We will now add tree info step.df
  tree.info = path.df %>%
    left_join(select(step.df, step, step_type),by="step") %>%
    group_by(astep) %>%
    mutate(
      root_step = first(step),
      parent_step = lag(step),
      next_step = lead(step),
      next_step_type = lead(step_type)
    ) %>%
    group_by(step, step_type) %>%
    summarize(
      root_step = first(root_step),
      parent_step = na.val(first(parent_step),0),
      num.a.child = sum(next_step_type=="a", na.rm=TRUE),
      num.mod.child = n_distinct(next_step[next_step_type=="mod"], na.rm=TRUE)
    ) %>%
    ungroup()


  tree.info$children = lapply(tree.info$step, function(step) which(tree.info$parent_step == step))

  step.df = step.df %>%
    left_join(select(tree.info, -step_type), by="step")

  dap = list(step.df=step.df, path.df = path.df)
}



plot.dap = function(dap, step.df = dap$step.df, error.steps=NULL) {

  blues.col = c("#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD")
  reds.col = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")
  greens.col = c("#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45")


  # lila for error
  error.col = c("#aa22aa")

  if (!"cache" %in% colnames(step.df)) {
    step.df$cache = FALSE
  }
  if ("reg_ok" %in% colnames(step.df)) {
    error.steps = union(error.steps, step.df$step[is.true(!step.df$reg_ok)])
  }

  nodes = step.df %>%
    mutate(
      time_int = 1 + (time>1) + (time > 10) + (time > 100) + (time > 1000),
      id = step,
      shape = ifelse(step_type=="load","dot","box"),
      color = case_when(
        step %in% error.steps ~ error.col,
        cache ~ blues.col[time_int],
        step_type == "a" ~ greens.col[time_int],
        TRUE ~ reds.col[time_int]
      ),
      label = paste0(step, ": ",signif(time,2)," sec.","\n", shorten.str(steplab,30)),
      title = paste0(shorten.str(gsub("\n","<br>",steplab, fixed=TRUE),100),"<br>",signif(time,3)," sec.","<br>step = ",step,"<br>", dofile, "<br>Org. line ",orgline,"<br>Line / Counter ", line," / ", counter)
    )

  edges = step.df %>%
    transmute(
      from = parent_step,
      to = step
    ) %>%
    ungroup() %>%
    filter(!is.na(from)) %>%
    distinct() %>%
    mutate(arrows = "to")

  title = paste0("Data-Regression Network")
  library(visNetwork)
  vis = visNetwork(nodes, edges,
                   main = list(text=title,style="font-family: Helvetia, sans-serif;font-weight:bold;font-size:20px; text-align: center;")
  )
  vis

  # library(RColorBrewer)
  # RColorBrewer::display.brewer.pal(7,"Greens")
  # RColorBrewer::brewer.pal(7,"Greens")
  # greens.col = c("#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45")
  #
  # RColorBrewer::display.brewer.pal(5,"YlOrRd")
  # RColorBrewer::brewer.pal(5,"YlOrRd")
  # reds.col = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")
  #
   # RColorBrewer::display.brewer.pal(6,"Blues")
   # RColorBrewer::brewer.pal(6,"Blues")
   # blues.col = c("#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD")



}




stata.data.cmd.types = function() {
  #
  # IMPORTANT: IF YOU ADD A COMMAND HERE
  # ALSO ADD IT IN stata.data.cmd.types.vec
  # Otherwise strange errors occur
  #
  list(
    load = c("u","us", "use","insheet","infix","import","sysuse","guse","gzuse"),
    preserve = c("preserve"),
    restore = c("restore"),
    # Commands that modify data and can be translated to R
    mod = c("g","ge","gen","generate","replace","drop","keep","rename","merge","egen","xtset","tsset", "xtile","pctile","append","tabulate","tabul","tabu", "tab","ta","encode","predict","xi", "collapse", "sort","so","tab1", "winsor"),
    # Commands that modify data but cannot be translated to R
    # predict also needs cache because it may use
    # results from previous regression
    need_cache = c("merge","xtile","pctile","append","tabulate","tabul","tabu", "tab","ta","encode","predict","xi","collapse","sort","so","tab1", "winsor"),

    # Commands that under certain conditions modify the data
    # tabulate creates variables if called with the gen option
    possible_mod = c("tabulate","tabul","tabu", "tab","ta","tab1")

  )
}

stata.data.cmd.types.vec = function() {
  return(c(u='load', us='load',use = 'load', insheet = 'load', infix = 'load', import = 'load', sysuse = 'load', merge = 'mod', append='mod', preserve = 'preserve', restore = 'restore', g = 'mod', ge = 'mod', gen = 'mod', generate = 'mod', replace = 'mod', egen="mod", drop = 'mod', keep = 'mod', rename = 'mod', xtset="mod",tsset="mod", xtile="mod",pctile="mod", tabulate="mod", tabu="mod", tabul="mod", tab="mod",ta="mod",encode="mod",predict="mod",xi="mod", collapse="mod", sort="mod",so="mod", tab1="mod", winsor="mod"))

  temp = stata.data.cmd.types()
  vec = NULL
  for (i in seq_along(temp)) {
    new = rep(names(temp)[i], length(temp[[i]]))
    names(new) = temp[[i]]
    vec = c(vec, new)
  }
  cat(paste0(names(vec)," = '", vec,"'", collapse=", "))

}

are.possible.mod.cmd.real.mod = function(run.df) {
  restore.point("are.possible.mod.cmd.real.mod.cmd")
  is.mod = rep(TRUE, NROW(run.df))

  tab.rows = which(run.df$cmd %in% c("tabulate","tabul","tabu", "tab","ta"))
  if (length(tab.rows)>0) {
    opts = str.right.of(run.df$cmdline[tab.rows],",")
    opts = trimws_around(opts,"\\(")
    rx = paste0("(", c("gen","gene","gener","generate"),"\\()", collapse="|")
    is.gen = grepl(rx, opts)
    is.mod[tab.rows] = is.gen
  }
  is.mod
}

dap.to.store.data = function(dap, cache.dir) {
  restore.point("dap.to.store.data.df")
  if (is.null(dap)) return(NULL)
  step.df = dap$step.df
  store.data = step.df %>%
    filter(cache) %>%
    select(step, donum, line, counter) %>%
    mutate(
      file = paste0(cache.dir,"/step_",step,".dta"),
      xtset_file = paste0(cache.dir,"/xtset_",step,".csv")
    )
}

# Matches stored stata scalar values
# with DAP steps
dap_create_stata_scalar_info = function(project_dir, dap, scalar_df=NULL, save_dap=TRUE) {
  restore.point("dap_create_stata_scalar_info")
  if (is.null(scalar_df)) {
    scalar_df = readRDS.or.null(file.path(project_df,"repdb/stata_scalar.Rds"))$stata_scalar$stata_scalar
    if (is.null(scalar_df)) {
      return(dap)
    }
  }

  step.df = dap$step.df

  scalars = unique(scalar_df$scalar_var)

  # First restrict attention to possibly relevant scalars
  all_code = paste0(unique(dap$step.df$cmdline), collapse="\n")
  rx = paste0("(?<![:alnum:_])", scalars, "(?![:alnum:_])")
  found = stri_detect_regex(all_code, rx)
  scalars = scalars[found]
  if (length(scalars)==0) {
    return(dap)
  }

  scalar_df = scalar_df[scalar_df$scalar_var %in% scalars,]

  smap_df = lapply(scalars, function(s) {
    rx = paste0("(?<![:alnum:_])", s, "(?![:alnum:_])")
    found = stri_detect_regex(step.df$cmdline, rx)
    as_tibble(list(step = step.df$step[found],scalar_var = rep(s, sum(found))  ))
  }) %>% bind_rows()

  scalar.df = full_join(smap_df, scalar_df %>% rename(scalar_runid=runid), by="scalar_var") %>%
    left_join(step.df %>% select(step, step_runid=runid), by = "step") %>%
    # Filter the last scalar definition
    # for each step
    filter(scalar_runid <= step_runid) %>%
    arrange(step, scalar_runid) %>%
    group_by(step) %>%
    slice(n()) %>%
    ungroup() %>%
    select(step, scalar_var, scalar_val, scalar_num_val) %>%
    mutate(is_num = !is.na(scalar_num_val))

  dap$scalar.df = scalar.df

  if (save_dap) {
    dap.file = file.path(project_dir,"metareg/dap/stata/dap.Rds")
    saveRDS(dap, dap.file)

    # Need to reset timestamps of cache files
    # to avoid error msg
    cache.files =list.files(file.path(project_dir,"metareg/dap/stata/cache"), full.names = TRUE)
    try(touchFile(cache.files))

  }
  dap
}
