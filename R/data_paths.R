# Part of DAP function
#
# Here we make the first part: extraction of data paths
# Notation is a bit old and not the same as in dap.R
#
# step: refers to a row in run.df
# node: refers to an analysis step (astep)

make.data.path.df = function(run.df, nodes = which(run.df$is.regcmd)) {
  restore.point("make.data.path.df")
  type.vec = stata.data.cmd.types.vec()

  run.df = run.df %>%
    mutate(
      step = seq_len(n()),
      cmd.type = type.vec[cmd],
      dataid = ifelse(foundfile!="" & is.true(cmd.type=="load"), paste0(basename(foundfile),":",datasig), ""),
      datalab = basename(foundfile)
    )
  dataids = setdiff(unique(run.df$dataid),"")

  run.df$dataid_num = -match(run.df$dataid, dataids)

  # Split run.df by rootdonum and compute path.df
  # Then merge path.df again for all run.df
  srun.li = split(run.df,run.df$rootdonum)
  path.li = lapply(srun.li, find.srun.df.data.paths, nodes=nodes)
  path.df = bind_rows(path.li)

  if (NROW(path.df)==0) {
    cat("\nNo regression command found.")
    return(NULL)
  }

  # dstep is either step or if not NA the dataid_num
  # this allows us to detect identical data sets
  # from different rootdonum
  path.df = path.df %>% mutate(
    dataid_num = run.df$dataid_num[step],
    dstep = ifelse(is.na(dataid_num), step, dataid_num),
    cmdlab = substring(run.df$cmdline[step],1,100),
    datalab = run.df$datalab[step],
    steplab = ifelse(dstep < 0, datalab, cmdlab)
  ) %>%
    dplyr::select(-cmdlab, -datalab) %>%
    left_join(select(run.df,step, stata_code = cmdline), by=c("step"))

  return(path.df)
}

extract.run.tree.from.path.df = function(root, path.df) {
  restore.point("extract.tree.from.path.df")
  tree.df = path.df %>%
    group_by(node) %>%
    mutate(
      step.ind = 1:n(),
      root.ind = match(root, dstep),
      del = is.na(root.ind) | step.ind < root.ind
    ) %>%
    ungroup() %>%
    filter(!del) %>%
    select(-step.ind, root.ind, del)
}



find.srun.df.data.paths = function(srun.df, nodes) {
  restore.point("find.srun.df.data.paths")

  srun.df$.ROW = seq_len(NROW(srun.df))
  srun.df = add.load.blocks.to.run.df(srun.df)

  nodes.ind = match(nodes, srun.df$step)
  snodes = nodes.ind[!is.na(nodes.ind)]
  snodes.org = nodes[!is.na(nodes.ind)]

  path.df = bind_rows(lapply(snodes, find.data.run.path, srun.df = srun.df, nodes=nodes))
  if (NROW(path.df)==0) return(NULL)
  path.df = path.df %>%
    mutate(
      snode = node,
      sstep = step,
      step = srun.df$step[sstep],
      node = srun.df$step[snode]
    )
  path.df
}

# srun.df is a part of run.df with a single node
find.data.run.path = function(node, srun.df, nodes=NULL) {
  restore.point("find.data.run.path")

  path = which(srun.df$load.block == srun.df$load.block[node] & srun.df$.ROW <= node)
  # If we start with a restore command
  # then jump to previous preserve and then
  # add all rows with the same load.block
  while (TRUE) {
    if (srun.df$cmd[path[1]] == "restore") {
      pr.row = srun.df$preserve.row[path[1]]
      new.path = which(srun.df$load.block == srun.df$load.block[pr.row] & srun.df$.ROW < pr.row)
      path = c(new.path, path[-1])
      next
    }
    break
  }
  #return(path)

  # Adapt path
  cmd.types = stata.data.cmd.types()

  allow = c(cmd.types$merge, cmd.types$mod, nodes)
  keep = seq_along(path) %in% c(1, length(path)) |
    srun.df$cmd[path] %in% allow

  path = path[keep]

  # Check in more detail whether possible mods
  # are indeed mods
  pos.keep = are.possible.mod.cmd.real.mod(srun.df[path,])
  path = path[pos.keep]


  cmds = srun.df$cmd[path]
  type.vec = stata.data.cmd.types.vec()

  path.type = type.vec[cmds]
  path.type[is.na(path.type)] = "node"
  path.time = c(rev(cumsum(srun.df$runsec[rev(path)[-1]])),0)
  tibble(node=node, step=path, step_type=path.type, time=path.time)
}



# Only works for run.df that have a single rootdonum
add.load.blocks.to.run.df = function(run.df) {
  restore.point("run.df.make.load.blocks")
  load.cmds = stata.data.cmd.types()$load

  run.df$load.block = cumsum(run.df$cmd %in% c(load.cmds,"restore"))

  # Deal with preserve / restore
  run.df$preserve.row = NA
  pr.rows = which(run.df$cmd %in% c("preserve","restore"))
  cur.preserve = NA
  for (r in pr.rows) {
    if (run.df$cmd[r]== "preserve") {
      cur.preserve = r
    } else if (!is.na(cur.preserve)) {
      run.df$preserve.row[r] = cur.preserve
      #rows = which(run.df$load.block == run.df$load.block[r])
      #run.df$load.block[rows] = run.df$load.block[cur.preserve]
    } else {
      warning("Restore command without previous preserve command is encountered!")
    }
  }
  run.df
}


plot.path.df = function(path.df) {

  blues.col = c("#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C")
  reds.col = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")
  greens.col = c("#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45")

  if (!"cache" %in% colnames(path.df)) {
    path.df$cache = FALSE
  }

  nodes = path.df %>%
    group_by(dstep) %>%
    summarize(
      cache = first(cache),
      time = first(time),
      step_type = first(step_type),
      time = first(time),
      time_int = 1 + (time>1) + (time > 10) + (time > 100) + (time > 1000),
      steplab = first(steplab)
    ) %>%
    mutate(
      id = 1:n(),
      shape = ifelse(step_type=="load","dot","box"),
      color = case_when(
        #step_type == "load" ~ blues.col[time_int],
        cache ~ blues.col[time_int],
        step_type == "node" ~ greens.col[time_int],
        TRUE ~ reds.col[time_int]
      ),
      label = paste0(shorten.str(steplab,30),"\n",signif(time,2)," sec."),
      title = paste0(shorten.str(gsub("\n","<br>",steplab, fixed=TRUE),100),"<br>",signif(time,3)," sec.","<br>dstep = ",dstep)
    )

  edges = path.df %>%
    left_join(select(nodes, dstep, id), by="dstep") %>%
    group_by(node) %>%
    transmute(
      from = id,
      to = lead(id)
    ) %>%
    ungroup() %>%
    filter(!is.na(to)) %>%
    select(from, to) %>%
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
  # RColorBrewer::display.brewer.pal(5,"Blues")
  # RColorBrewer::brewer.pal(5,"Blues")
  # blues.col = c("#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C")



}


