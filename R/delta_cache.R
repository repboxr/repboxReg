# The goal of delta caching is to reduce the size of cached data sets
# We try to identify cache files that are identical to or simple
# modification of other cache files, e.g. dropped cols or rows
# or only a few rows different.

# Using delta caches REQUIRES that xtset info is stored for each
# cache file since with delta caches we need to manually
# add code to reset the xtset variables
# (in the original dta file the xtset info is stored)

example = function() {
  dat1 = tibble(x=1:10,y=x*2)
  dat2 = dat1 %>% mutate(z=x+3,b="BB")
  dat1 = dat1[-c(1,4),]
  i1 = cache_data_info(dat1,step=1)
  i2 = cache_data_info(dat2,step=2)

  comp = cache_info_compare(i1,i2)
  scomp = filter(comp, less_rows)

  dc = delta_cache_info(dat1, dat2, scomp)


  cc = projects.count.caches("~/repbox/projects_reg")

  project.dir = "~/repbox/projects_reg/aejapp_2_4_8"
  project.dir = "~/repbox/projects_reg/aejapp_3_2_2"
  res = make_delta_caches(project.dir)

  infos = mr_make_all_cache_infos(project.dir)
  comp_df = compare_all_cache_infos(infos)
  dci_df = make_delta_cache_graph(comp_df)
}


# Heuristically determine which caches
# shall be converted to a delta cache for another cache
#
# Some rules:
#
# If we store deltas from deltas only for simple column drops
#

make_delta_caches = function(project.dir) {
  restore.point("make_all_delta_cache")


  delta.cache.dir = file.path(project.dir, "metareg","dap","stata", "delta_cache")
  if (!dir.exists(delta.cache.dir)) dir.create(delta.cache.dir)

  cat(paste0(
"\n*****************************************************************",
"\nMake delta caches for ", project.dir,
"\n*****************************************************************"))

  cat("\n1. Make cache infos... ")
  infos = mr_make_all_cache_infos(project.dir)
  cat(" done for ", length(infos),"caches.\n")
  cat("\n2. Make delta cache infos... ")
  comp_df = compare_all_cache_infos(infos)
  dci_df = make_delta_cache_graph(comp_df)
  cat(NROW(dci_df),"delta caches\n")

  if (NROW(dci_df)==0) return(NULL)

  dci_df = dci_df %>%mutate(
    delta_mb = 0,
    need_dat = !is.true(identical | just_drop_cols)
  )

  dat_rows = which(dci_df$need_dat)
  cat("\n3. Store",length(dat_rows),"delta cache files\n")

  for (row in dat_rows) {
    dci = dci_df[row,]
    cache.file = mr_get_cache_file(project.dir, step = dci$step)
    org_dat = haven::read_dta(cache.file)
    delta_dat = delta_cache_make_data(org_dat, dci)
    delta.cache.file = file.path(delta.cache.dir, paste0("delta_", dci$step,".dta"))
    haven::write_dta(delta_dat, delta.cache.file)
    dci_df$delta_mb[row] = file.size(delta.cache.file) / 1e6
  }

  dci_df = remove.cols(dci_df,c("comp_rows","ok"))
  dci_df$delta_rate = dci$delta_mb / dci$org_mb
  cols = union(c("step","parent_step","org_mb","delta_mb","delta_rate"),names(dci_df))

  dci_df= dci_df[,cols]
  saveRDS(dci_df, file.path(delta.cache.dir,"dci.Rds"))
  cat(paste0("\n   ",NROW(dci_df), " delta caches reduced ", round(sum(dci_df$org_mb),1), " MB to ", round(sum(dci_df$delta_mb),1), " MB.\n"))

  invisible(dci)
}

make_delta_cache_graph = function(comp_df) {
  restore.point("make_delta_cache_graph")
  steps = unique(comp_df$step1)

  comp_df = comp_df %>%
    mutate(
      is_same = equal_rows & ncol_extra == 0 & ncol_missing == 0 & ncol_diff == 0 & ncol_same > 0,
      just_drop_cols = equal_rows &  ncol_extra == 0 & ncol_missing > 0 & ncol_diff == 0 & ncol_same > 0
    )

  same_dci = rm_col_dci = eq_rows_dci = NULL

  # 1. We store and remove same cache

  # We will then keep step2. Mainly step2 will be the cache to keep
  same.df = comp_df %>% filter(is_same, step1 > step2)
  if (NROW(same.df) > 0) {
    same_dci = transmute(same.df,
      ok=TRUE, step = step1, parent_step=step2, identical=TRUE, just_drop_cols=NA, rows_equal=NA, org_mb=file_mb1)
  }

  comp_df = filter(comp_df, !is_same, !step1 %in% same.df$step1, !step2 %in% same.df$step1)

  if (NROW(comp_df)==0) {
    return(same_dci)
  }

  # 2. Those caches where we just remove columns are obvious delta_caches
  rm.col.df = comp_df %>% filter(just_drop_cols)

  if (NROW(rm.col.df) > 0) {
    rm_col_dci = rm.col.df %>%
      transmute(step = step1, parent_step=step2, identical=FALSE, just_drop_cols=TRUE,rows_equal=NA, org_mb=file_mb1, missing_cols = missing_cols)
  }

  comp_df = filter(comp_df, !step1 %in% rm.col.df$step1, !step2 %in% rm.col.df$step1)

  if (NROW(comp_df)==0) {
    return(bind_rows(same_dci, rm_col_dci))
  }

  # 3. Create delta caches that have a substantial number of same columns
  #    (i.e. nrow must be same and likely same row ordering)
  #
  #    The number of same columns should exceed the number of extra and
  #    diff cols and also be above 2.

  big.eq.rows.df =
    filter(comp_df, equal_rows, ncol_same >= pmax(2,ncol_diff + ncol_extra)) %>%
    mutate(points = ncol_same - ncol_diff - ncol_extra - step2 / 1e6)

  df = big.eq.rows.df

  # We now have a graph that we want to convert into a DAG using
  # good heuristics

  # a) If step1 has multiple parents (step2) only keep the one with
  #    the most points
  df = df %>%
    group_by(step1) %>%
    filter(points == max(points)) %>%
    ungroup()

  # b) Now find circles and cut in each circle the egde with the lowest
  #    points.

  cut.rows = delta.cache.find.circle.cut.rows(df)
  if (length(cut.rows)>0) {
    df = df[-cut.rows,]
  }
  #plot.delta.cache.graph(df)


  eq.rows.df = df

  # Remove child nodes from further consideration.
  # This has two benefits:
  # 1. We need to check less in the costly next step
  # 2. delta_cache chains will be shorter
  if (NROW(eq.rows.df)>0) {
    comp_df = filter(comp_df, !step1 %in% eq.rows.df$step1,!step2 %in% eq.rows.df$step1)

    eq_rows_dci = delta_cache_make_info_rows_equal(eq.rows.df)
  }

  if (NROW(comp_df)==0) {
    return(bind_rows(same_dci, rm_col_dci, eq_rows_dci))
  }




  # 4. Now determine delta cache candidates where rows may be dropped
  #    or the data was arranged differently

  comp_df = comp_df %>%
    filter(
      # delta caches will not add rows
      equal_rows | less_rows,
      # There should be the chance of more same cols than extra cols
      # and also at least 2 same cols
      ncol_diff + ncol_same >= pmax(2,ncol_extra)
    )


  # To do: return if comp_df has no rows



  # Now we determine points for each node that might be a parent

  comp_df$is_open = TRUE
  big_comp_df = bind_rows(comp_df, same.df, rm.col.df, eq.rows.df) %>%
    mutate(
      diff_col_same_prob = 0.8,
      exp_pair_points = (ncol_same + diff_col_same_prob*ncol_diff - 0.8*ncol_extra + 1),
      cache_works = is_same | just_drop_cols,
      diff_col_same_share = ifelse(is_same | just_drop_cols, 1, 0),
      pair_points = ifelse(is_same | just_drop_cols, exp_pair_points, 0),
      possible = TRUE,
      step1_2 = paste0(step1,"_", step2)
  )

  comp_df = mutate(comp_df,step1_2 = paste0(step1,"_", step2))

  #plot.delta.cache.graph(big_comp_df)

  dci_df_li = vector("list", NROW(comp_df))
  counter = 0

  while(NROW(comp_df)>0) {
    counter = counter+1
    points_df = big_comp_df %>%
      group_by(step2) %>%
      summarize(
        # Sum over all potential delta childs
        # same_cols are best then diff_cols (who might be same)
        # while extra cols are not good.
        # Children with more rows get higher weight
        exp_points = sum(exp_pair_points*nrow1*possible)
      ) %>%
      filter(step2 %in% comp_df$step2) %>%
      arrange(desc(exp_points), step2)

    step2 = first(points_df$step2)
    #cat("\nCheck possible delta caches for parent step ", step2,".\n")
    dci_df = make.all.parent.step.delta.cache.infos(comp_df, step2)

    dci_df_li[[counter]] = filter(dci_df, ok)

    if (length(dci_df$comp_rows)==0) {
      restore.point("why are we here?")
      stop("We should not be here...")
    }

    # Directly infeasible rows
    inf_rows = dci_df$comp_rows[dci_df$ok==FALSE]
    big_inf_rows = match(comp_df$step1_2, big_comp_df$step1_2)
    big_comp_df$possible[big_inf_rows] = FALSE

    # Indirectly infeasible rows
    # To do: There can be indirectly infeasible rows
    #        if 3 is an infeasible child of 1
    #        and 2 is a feasible child of 1
    #        then likely 3 is also an infeasible child of 1
    #       (except infeasibility was a result of multi matches
    #        but that case may be rare)

    feas_childs = dci_df$step[dci_df$ok]
    infeas_childs = dci_df$step[!dci_df$ok]

    if (length(feas_childs)>0 & length(infeas_childs)>0) {
      gr = expand.grid(step1 = infeas_childs, step2=feas_childs)
      gr$step1_2 = paste0(gr$step1,"_", gr$step2)
      big_inf_rows = match(gr$step1_2, big_comp_df$step1_2)
      big_comp_df$possible[big_inf_rows] = FALSE
      inf_rows2 = match(gr$step1_2, comp_df$step1_2)
    } else {
      inf_rows2 = NULL
    }

    # remove rows from comp_df
    rm_rows = union(dci_df$comp_rows, inf_rows2)
    comp_df = comp_df[-rm_rows,]
  }

  dci_df = bind_rows(dci_df_li)


  return(bind_rows(same_dci, rm_col_dci, eq_rows_dci, dci_df))
}

# Internal function
make.all.parent.step.delta.cache.infos = function(comp_df, step2) {
  restore.point("make.all.parent.step.delta.cache.infos")

  .step2 = step2

  sel_rows = which(comp_df$step2 == .step2)
  scomp_df = comp_df[sel_rows,]

  file2 = first(scomp_df$file2)
  dat2 = haven::read_dta(file2)

  row = 1
  dci_li = lapply(seq_rows(scomp_df), function(row) {
    comp = scomp_df[row,]
    dat1 = haven::read_dta(comp$file1)
    dci = delta_cache_make_info(dat1, dat2, comp)
  })

  dci_df = bind_rows(dci_li)
  dci_df$comp_rows = sel_rows
  dci_df
}


# df must have columns step1, step2 and points
# points measures the attractivity of the edge from step1 to step2
#
# There is no direct function to find cycles in igraph
#    We proceed as follows:
#    1. Find all cliques: note that every node in df has at most 1 input
#    2. A clique has thus no cycle if and only if there is a node without an input
#
# For every cycle we want to cut the edge (=row of df) with the lowest points
delta.cache.find.circle.cut.rows = function(df) {
  restore.point("delta.cache.find.circle.cut.rows")

  if (NROW(df)<=1) return(NULL)

  # Transform step to integer ind starting from 1
  # because that is what igraph wants
  steps = unique(c(df[["step1"]],df[["step2"]]))
  df$step1_ind = match(df$step1, steps)
  df$step2_ind = match(df$step2, steps)

  library(igraph)
  g = graph_from_edgelist(cbind(df[["step1_ind"]],df[["step2_ind"]]), directed=FALSE)
  cliques = max_cliques(g)

  is_circle = sapply(cliques, function(inds) {
    # no circle if the clique has a node without input
    no_circle = any(!inds %in% df$step1_ind)
    !no_circle
  })
  circles = cliques[is_circle]
  if (length(circles)==0) return(NULL)

  cut_rows = sapply(circles, function(circle) {
    rows = which(df[["step1_ind"]] %in% circle | df[["step2_ind"]] %in% circle)
    cut_row = rows[which.min(df$points[rows])]
    cut_row
  })
  cut_rows
}

mr_make_all_cache_infos = function(project.dir) {
  cache_df = mr_get_cache_df(project.dir)
  infos = lapply(seq_rows(cache_df), function(i) {
    file = cache_df$file[[i]]
    dat = haven::read_dta(file)
    cache_data_info(step = cache_df$step[i], file=file, dat=dat)
  })
  infos
}

compare_all_cache_infos = function(infos) {
  n = length(infos)
  if (n<=1) return(NULL)
  n_comp = n*(n-1)/2
  res_li = vector("list", n_comp)
  counter = 0
  for (ind1 in 1:(n-1)) {
    for (ind2 in (ind1+1):n) {
      counter = counter+1
      res_li[[counter]] = cache_info_compare(infos[[ind1]], infos[[ind2]])
    }
  }
  bind_rows(res_li)
}



# sdat has weakly fewer rows than bdat
delta_cache_make_info = function(sdat, bdat, comp,match_cols_guesses = list(intersect(names(sdat),names(bdat))), parent_row_key = "Parent_rOw__x_NuM_o_", row_key = "rOw__x_NuM_o_", verbose=TRUE) {
  restore.point("delta_cache_make_info")

  cols = union(names(sdat), names(bdat))
  if (parent_row_key %in% cols) {
    parent_row_key = paste0(parent_row_key, random.string(1,12))
  }
  if (row_key %in% cols) {
    row_key = paste0(row_key, random.string(1,12))
  }

  for (match_cols in match_cols_guesses) {
    res = match_rows_after_filter_arrange(sdat, bdat, match_cols=match_cols, brow_col = parent_row_key)
    if (res$ok) break
  }
  if (!res$ok) {
    return(list(ok=FALSE,step = comp$step1, parent_step=comp$step2))
  }
  ma = res$brows

  scols = colnames(sdat)
  bcols = colnames(bdat)

  parent_rows = ma

  missing_cols = comp$missing_cols[[1]]
  extra_cols = comp$extra_cols[[1]]

  # Check which overlapping cols except match_cols
  # are the same
  overlap_cols = setdiff(intersect(scols,bcols),match_cols)

  if (length(overlap_cols)>0) {
    sbdat = bdat[parent_rows,]
    is_same = sapply(overlap_cols, function(col) {
      isTRUE(all.equal(sdat[[col]], sbdat[[col]]))
    })
    .same_cols = c(match_cols, overlap_cols[is_same])
    .diff_cols = overlap_cols[!is_same]
  } else {
    .same_cols = match_cols
    .diff_cols = character(0)
  }

  rows_equal = identical(parent_rows, seq_rows(bdat))

  transmute(comp,
    ok=TRUE, step = step1, parent_step=step2,
    identical=FALSE, just_drop_cols=FALSE, rows_equal = identical(parent_rows, seq_rows(bdat)),

    org_mb=file_mb1,

    nrow=nrow1, parent_nrow=nrow2,  parent_rows = list(parent_rows),

    ncol_missing = ncol_missing, ncol_extra = ncol_extra,
    ncol_same = length(.same_cols), ncol_diff=length(.diff_cols),

    ncol_org = ncol_extra + ncol_same + ncol_diff,
    ncol_delta = ncol_extra + ncol_diff + 1,

    same_cols=list(.same_cols), diff_cols=list(.diff_cols),
    missing_cols = missing_cols, extra_cols=extra_cols,

    same_cols=list(same_cols), diff_cols=list(diff_cols), missing_cols = list(missing_cols), extra_cols=list(extra_cols),
    parent_row_key=parent_row_key, row_key = row_key)

}


# Use this function if we assume that rows have not changed. This simply means the columns that are the same according to the md5 digest are assumed to stay the same while the others are assumed to differ.
delta_cache_make_info_rows_equal = function(comp_df, parent_row_key = "Parent_rOw__x_NuM_o_", row_key = "rOw__x_NuM_o_") {

  transmute(comp_df,
    ok=TRUE, step = step1, parent_step = step2,
    identical=FALSE, just_drop_cols=FALSE, rows_equal = TRUE,
    org_mb=file_mb1,
    nrow=nrow1, parent_nrow=nrow2,parent_rows = NA,

    ncol_missing = ncol_missing, ncol_extra = ncol_extra,
    ncol_same = ncol_same, ncol_diff=ncol_diff,

    ncol_org = ncol_extra + ncol_same + ncol_diff,
    ncol_delta = ncol_extra + ncol_diff + 1,

    same_cols=same_cols, diff_cols=diff_cols,
    missing_cols = missing_cols, extra_cols=extra_cols,

    parent_row_key=parent_row_key, row_key = row_key
  )

}


delta_cache_make_data = function(sdat, dci) {
  restore.point("make_delta_cache_data")

  if (isTRUE(dci$rows_equal)) {
    ddat = tibble(parent_row_key = seq_len(dci$nrow))
    names(ddat)= c(dci$parent_row_key)
  } else {
    ddat = tibble(row_key = seq_along(dci$parent_rows[[1]]), parent_row_key = dci$parent_rows[[1]])
    names(ddat)= c(dci$row_key, dci$parent_row_key)

  }

  cols = unlist(c(dci$extra_cols[[1]], dci$diff_cols[[1]]))
  if (length(cols)>0) {
    ddat = bind_cols(ddat, sdat[,cols,drop=FALSE])
  }
  ddat
}

# Compare from perspective of i1
# (you can call again for reverse perspective)
cache_info_compare = function(i1,i2) {
  restore.point("compare_cache_data_info")
  cols1 = names(i1$col_info)
  cols2 = names(i2$col_info)
  # Just compare colnames
  extra_cols = setdiff(cols1, cols2)
  missing_cols = setdiff(cols2, cols1)

  same_colnames = intersect(cols1, cols2)

  is_same = rep(FALSE, length(same_colnames))
  names(is_same) = same_colnames
  if (i1$nrow == i2$nrow) {
    for (col in same_colnames) {
      if (i1$col_info[[col]]$digest == i2$col_info[[col]]$digest) {
        is_same[col] = TRUE
      }
    }

  }
  comp1 = tibble(
    step1=i1$step,
    step2=i2$step,
    nrow1 = i1$nrow,
    nrow2 = i2$nrow,
    file_mb1 = i1$file_mb,
    file_mb2 = i2$file_mb,
    equal_rows = i1$nrow==i2$nrow,
    more_rows = i1$nrow>i2$nrow,
    less_rows = i1$nrow<i2$nrow,
    ncol_extra = length(extra_cols),
    ncol_missing = length(missing_cols),
    ncol_diff = sum(!is_same),
    ncol_same = sum(is_same),
    extra_cols = list(extra_cols),
    missing_cols = list(missing_cols),
    diff_cols = list(same_colnames[!is_same]),
    same_cols = list(same_colnames[is_same]),
    file1 = i1$file,
    file2 = i2$file

  )
  comp2 = tibble(
    step1=i2$step,
    step2=i1$step,
    nrow1 = i2$nrow,
    nrow2 = i1$nrow,
    file_mb1 = i2$file_mb,
    file_mb2 = i1$file_mb,
    equal_rows = i1$nrow==i2$nrow,
    more_rows = i1$nrow<i2$nrow,
    less_rows = i1$nrow>i2$nrow,
    ncol_extra = length(missing_cols),
    ncol_missing = length(extra_cols),
    ncol_diff = sum(!is_same),
    ncol_same = sum(is_same),
    extra_cols = list(missing_cols),
    missing_cols = list(extra_cols),
    diff_cols = list(same_colnames[!is_same]),
    same_cols = list(same_colnames[is_same]),
    file1 = i2$file,
    file2 = i1$file

  )

  bind_rows(comp1, comp2)

}


cache_data_info = function(step, file, dat=haven::read_dta(file)) {
  restore.point("cache_data_info")
  library(digest)
  col_info = lapply(seq_along(colnames(dat)), function(i) {
    col = names(dat)[i]
    v = dat[[i]]
    tibble(
      col = col,
      colpos = i,
      class = class(v)[1],
      digest = digest(as.vector(dat[[i]]),algo="md5")
    )
  })
  names(col_info) = names(dat)


  list(step=step, file=file, file_mb = file.size(file) / 1e6, nrow = NROW(dat), col_info=col_info)
}



delta_cache_stata_code = function(dc, big.dta.file="big.dta", delta.dta.file="delta.dta", load_data = TRUE) {
  restore.point("stata_code_delta_cache")

  use_code = drop_cols_code = drop_rows_code = NULL

  use_code = paste0('use "', big.dta.file,'"')

  if (length(dc$big_drop_cols) > 0) {
    drop_cols_code = paste0("drop ", paste0(dc$big_drop_cols, collapse=" "))
  }

  if (length(dc$big_drop_rows) > 0 & dc$just_drop) {
    # We need to merge to a data frame that contains dropped rows
    #gen big_row_num = _n
    #merge 1:1 n using drop
    #drop if _merge == 3

    drop_cols_code = paste0("drop ", paste0(dc$big_drop_cols, collapse=" "))
  }

  if (dc$just_drop) {
    delta_cache_stata_code_just_drop(dc, big.dta.file)
  }

}





# Assume sdat is the result of some
# filter, arrange and mutate operations on bdat
#
# But there is a set of match_cols columns
# shared between both that has not been mutated
# and allows a on-to-one match between sdat and bdat

# We return a vector of the big dat row numbers in
# sdat.
# If there is a problem like some sdat cols could not be matched to bdat we return list(ok=FALSE)
# otherwise list(ok=TRUE, brows = ...)
example = function() {
  dat1 = tibble(x=1:10,y=x*2)
  dat2 = dat1 %>% mutate(z=x+3,b="BB")
  dat1 = dat1[-c(1,4),]
  dat1 = dat1 %>% arrange(desc(x))

  match_rows_after_filter_arrange(dat1, dat2, c("x","y"))
}

# sdat has weakly fewer rows than bdat
match_rows_after_filter_arrange = function(sdat, bdat,match_cols = intersect(colnames(sdat,bdat)), brow_col = "b..Row._NmU.._", srow_col="s..Row._NmU.._", verbose=TRUE) {
  restore.point("match_rows_after_filter_arrange")

  if (!has.col(bdat,brow_col)) bdat[[brow_col]] = seq_rows(bdat)

  #if (!has.col(sdat,srow_col)) sdat[[srow_col]] = seq_rows(sdat)

  ma_df = left_join(sdat[,c(match_cols)], bdat[,c(match_cols, brow_col)], by=match_cols)
  ma = ma_df[[brow_col]]

  # Some row in sdat is not in bdat
  if (any(is.na(ma))) {
    return(list(ok=FALSE, brows=ma))
  }
  if (any(duplicated(ma))) {
    return(list(ok=FALSE, brows=ma))
  }
  list(ok=TRUE, brows=ma)
}



plot.delta.cache.graph = function(df, step=df$step1, parent_step=df$step2, is_open = is.true(df$is_open)) {
  restore.point("plot.delta.cache.graph")

  if (length(is_open)==0) is_open==rep(TRUE, length(step))

  steps = union(step, parent_step)
  nodes = tibble(id = steps, shape="box", label=steps)
  edges = tibble(
      from = parent_step,
      to = step,
      arrows = "to"
    )
  if (length(is_open)>0) {
    edges$color = case_when(
      is_open ~ "#5555ff",
      TRUE ~ "#333333"
    )

  }
  title = paste0("Delta-Cache Network")
  library(visNetwork)
  vis = visNetwork(nodes, edges,
                   main = list(text=title,style="font-family: Helvetia, sans-serif;font-weight:bold;font-size:20px; text-align: center;")
  )
  vis

}

example = function() {
  projects.dir = "~/repbox/projects_reg"
}

projects.count.caches = function(projects.dir) {
  project.dirs = list.dirs(projects.dir, full.names=TRUE, recursive = FALSE )
  project.dir = first(project.dirs)
  counts = lapply(project.dirs, function(project.dir) {
    cache.dir = paste0(file.path(project.dir,"metareg","dap","stata"),c("/cache","extra_cache"))
    cache.files = list.files(cache.dir, glob2rx("*.dta"),full.names = TRUE)
    mb=sum(file.size(cache.files), na.rm=TRUE) / 1e6
    tibble(project.dir = project.dir, num_caches = length(cache.files), total_mb = mb)
  }) %>% bind_rows()
  counts
}

