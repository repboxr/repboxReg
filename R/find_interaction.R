# Find which variables are interaction terms of each other

example.find_interactions = function() {
  n = 100
  dat = tibble(x1=runif(n),x2=runif(n),x3 = runif(n), x1_x2 = x1*x2, x1_x2_x3 = x1*x2*x3, x1_3 = x1*x3, const = 1, zero = 0, amba = sample(0:1, n, replace=TRUE), amba2 = amba)
  dat$amba2[1:20] = 0
  find_num_interactions(dat)
  find_num_interactions(dat, ignore.constants = FALSE)


  dat = tibble(i1 = sample(1:5,n, replace=TRUE), i2 = sample(1:5,n, replace=TRUE), i3 = sample(1:10,n, replace=TRUE), s1 = sample(letters,n, replace=TRUE), s1_i1 = paste0(s1,".",i1), s1_i2_i1 = paste0(s1,"_",i2,"_",i1))


  #dat = tibble(i1 = sample(1:5,n, replace=TRUE), i2 = sample(1:5,n, replace=TRUE), i3 = sample(1:10,n, replace=TRUE))
  dat$i3[3] = NA
  dat$i1[1] = NA
  dat$i1_i3 = paste(dat$i1,".",dat$i3)
  dat$i1_i3_NA = ifelse(is.na(dat$i3) | is.na(dat$i1),NA,paste(dat$i1,".",dat$i3))

  find_fe_interactions(dat,na.is.category = TRUE)
  find_fe_interactions(dat,na.is.category = FALSE)

}

#' Find tripplets (f1,f2,ia) where ia is the interaction of the fixed effects f1 and f2
#' We have an interaction essentially if an interaction label like paste0(f1,"_", f2),
#' can be uniquely mapped to ia.
#'
#' @param na.is.category if TRUE we assume that NA values were (wrongly) treated just like a specific category. For example, assume (f1=NA, f2="blue") then a correct specification would be ia=NA. But if you generated interaction effects by ia = paste0(f1,"_",f2) we would have ia = "NA_blue", i.e. paste just treats NA like a category. If we find interactions of fixed effects for na.is.category=TRUE that are no interactions for na.is.category=FALSE, it might indicate a programming error (not correctly dealing with NA).
find_fe_interactions = function(dat, cols = colnames(dat), ignore.constants=TRUE, max.cols = 500, na.is.category = FALSE) {
  restore.point("find_fe_interactions")

  # Only consider numeric and logical columns
  is_fe = sapply(dat[cols], function(v) {
    if (is.numeric(v)) return(all(as.integer(v)==v | is.na(v)))
    return(TRUE)
  })
  cols = cols[is_fe]

  # Unique values
  uvals=lapply(dat[cols], function(v)  {
    u =unique(v)
    if (na.is.category) return(u)
    u[!is.na(u)]
  })

  # Value counts
  lens = sapply(uvals, length)

  if (ignore.constants) {
    cols = cols[lens>1]
    uvals = uvals[cols]
    lens = lens[cols]
  }

  if (length(cols)>max.cols) {
    cat(paste0("More than max.col=", max.col, " columns (", length(col),"). Don't find fe interactions."))
    return(NULL)
  }

  # Transform values to integers
  idat=lapply(seq_along(cols), function(i) {
    match(dat[[i]],uvals[[i]])
  })
  names(idat) = cols

  icols = seq_along(cols)

  fgrid = expand.grid(f1=icols, f2=icols)
  fgrid = fgrid[fgrid[,1] < fgrid[,2],]

  fia = lapply(seq_len(NROW(fgrid)), function(r) {
    i1 = fgrid[r,1]
    i2 = fgrid[r,2]
    fia = paste0(idat[[i1]],"_",idat[[i2]])
    if (!na.is.category) {
      fia[is.na(idat[[i1]]) | is.na(idat[[i2]])] = NA_character_
    }
    fia
  })

  # Unique values
  ufia=lapply(fia, function(v)  {
    u =unique(v)
    if (na.is.category) return(u)
    u[!is.na(u)]
  })

  # Value counts
  lens_fia = sapply(ufia, length)

  # Only keep pairs that have a length equal to some variable
  keep_fia = lens_fia %in% lens

  fia  = fia[keep_fia]
  ufia = ufia[keep_fia]
  lens_fia = lens_fia[keep_fia]
  fgrid = fgrid[keep_fia,,drop=FALSE]

  # Similarly we can also reduce the set of possible ia cols
  ia_cols = cols
  keep_ia = lens %in% lens_fia
  ia_cols = cols[keep_ia]

  ia_col_nums=which(keep_ia)


  # Transform pair interactions to integers
  ifia =lapply(seq_along(fia), function(i) {
    match(fia[[i]],ufia[[i]])
  })

  # Now generate a key that corresponds to some random rows
  # Halton sequence
  shares = c(0.5, 0.25, 0.75, 0.125, 0.625, 0.375, 0.875, 0.0625, 0.5625, 0.3125, 0.8125, 0.1875)
  rows = ceiling(NROW(dat)*shares) %>% unique()

  keys_ia=sapply(ia_col_nums, function(i) {
    paste0(c(lens[i], idat[[i]][rows]), collapse=".")
  })

  keys_fia = sapply(seq_along(ifia), function(i) {
    paste0(c(lens_fia[i], ifia[[i]][rows]), collapse=".")
  })

  # Only keep fia pairs whose key matches an ia key
  keep_fia = keys_fia %in% keys_ia

  ifia = ifia[keep_fia]
  #fia  = fia[keep_fia]
  #ufia = ufia[keep_fia]
  #lens_fia = lens_fia[keep_fia]
  keys_fia = keys_fia[keep_fia]
  fgrid = fgrid[keep_fia,,drop=FALSE]

  # Similarly only keep ia whose key matches a fia key
  keep_ia = keys_ia %in% keys_fia
  keys_ia = keys_ia[keep_ia]
  ia_cols = ia_cols[keep_ia]
  ia_col_nums = ia_col_nums[keep_ia]


  # Now we loop through every row of grid and find tripplets
  tripplets.li = lapply(seq_len(NROW(fgrid)), function(fia_ind) {
    key_fia = keys_fia[[fia_ind]]

    cand_ia = ia_col_nums[keys_ia == key_fia]

    # We ignore ia that are identical to a factor
    f1.col = fgrid[fia_ind,1]
    f2.col = fgrid[fia_ind,2]
    cand_ia = setdiff(cand_ia, c(f1.col,f2.col))

    if (length(cand_ia)==0) return(NULL)

    fia_vals = ifia[[fia_ind]]
    if (!na.is.category) {
      matches = sapply(cand_ia, function(ia_col_num) {
        all(idat[[ia_col_num]]==fia_vals | (is.na(idat[[ia_col_num]]& is.na(fia_vals))))
      })
    } else {
      matches = sapply(cand_ia, function(ia_col_num) {
        all(idat[[ia_col_num]]==fia_vals)
      })
    }
    if (sum(matches)==0) return(NULL)

    cbind(f1 = f1.col, f2=f2.col, ia = cand_ia[matches])
  })

  itrips = do.call(rbind,tripplets.li)

  if (NROW(itrips)==0) {
    return(data.frame(f1=character(0), f2=character(0), ia = character(0)))
  }

  data.frame(f1 = cols[itrips[,1]],f2 = cols[itrips[,2]], ia = cols[itrips[,3]])
}

# F
find_num_interactions = function(dat, cols = colnames(dat), digits = 6, ignore.constants=TRUE, max.cols = 250) {
  restore.point("find_interactions")
  # Only consider numeric and logical columns
  is_num = sapply(dat[cols], function(v) !is.character(v) & !is.factor(v))
  cols = cols[is_num]

  dupl.cols  = cols[duplicated(as.list(dat[cols]))]
  cols = setdiff(cols, dupl.cols)

  if (ignore.constants) {
    n_dist = sapply(dat[cols], n_distinct, na.rm=TRUE)
    const_cols = cols[n_dist <= 1]
    cols = setdiff(cols, const_cols)
  }

  if (length(cols)<3) {
    return(data.frame(f1=character(0), f2=character(0), ia = character(0)))
  }

  if (length(cols)>max.cols) {
    cat(paste0("More than max.col=", max.col, " columns (", length(col),"). Don't find interactions."))
    return(NULL)
  }

  # Transform to matrix for better speed
  mat = as.matrix(dat[,cols])


  fcols = cols
  iacols = cols

  fcol_nums = match(fcols, colnames(mat))
  iacol_nums = match(iacols, colnames(mat))

  grid = expand.grid(f1 = fcol_nums, f2 = fcol_nums, ia = iacol_nums)
  grid = grid[grid[,1]<grid[,2],]
  grid = grid[grid[,1] != grid[,3] & grid[,2] != grid[,3],]


  # 1. Reduce the set of possible interaction terms
  #    by looking at single rows

  # Halton sequence
  shares = c(0.5, 0.25, 0.75, 0.125, 0.625, 0.375, 0.875, 0.0625, 0.5625, 0.3125, 0.8125, 0.1875, 0.6875, 0.4375, 0.9375, 0.03125, 0.53125, 0.28125, 0.78125, 0.15625, 0.65625, 0.40625, 0.90625, 0.09375, 0.59375, 0.34375, 0.84375, 0.21875, 0.71875, 0.46875, 0.96875, 0.015625, 0.515625, 0.265625, 0.765625, 0.140625, 0.640625, 0.390625, 0.890625, 0.078125, 0.578125, 0.328125, 0.828125, 0.203125, 0.703125, 0.453125, 0.953125, 0.046875, 0.546875, 0.296875, 0.796875, 0.171875, 0.671875, 0.421875, 0.921875, 0.109375, 0.609375, 0.359375, 0.859375, 0.234375, 0.734375, 0.484375, 0.984375, 0.0078125, 0.5078125, 0.2578125, 0.7578125, 0.1328125, 0.6328125, 0.3828125, 0.8828125, 0.0703125, 0.5703125, 0.3203125, 0.8203125, 0.1953125, 0.6953125, 0.4453125, 0.9453125, 0.0390625, 0.5390625, 0.2890625, 0.7890625, 0.1640625, 0.6640625, 0.4140625, 0.9140625, 0.1015625, 0.6015625, 0.3515625, 0.8515625, 0.2265625, 0.7265625, 0.4765625, 0.9765625, 0.0234375, 0.5234375, 0.2734375, 0.7734375)
  rows = ceiling(NROW(mat)*shares) %>% unique()

  rows = rows[seq_len(min(50,NROW(rows)))]

  smat = signif(mat, digits)

  i = 1
  cat("\nInitial no of tripplet candidates: ",NROW(grid),"\n")
  for (i in seq_along(rows)) {
    if (n < 10) break
    row = rows[i]

    fvals = mat[row, fcols]
    fmult = fvals[grid[,1]] * fvals[grid[,2]]
    fmult = signif(fmult, digits)

    iavals = smat[row, iacols]
    iav = iavals[grid[,3]]

    # Round to given number of significant digits
    grid = grid[is.true(iav==fmult) | (is.na(iav) & is.na(fmult)),]
    cat("\nAfter round ", i," no of tripplet candidates: ",NROW(grid),"\n")

    n = NROW(grid)
    if (n < 10) break
    if (n < 50 & i > 1) break
    if (n < 100 & i > 5) break

  }

  if (NROW(grid)==0) {
    return(data.frame(f1=character(0), f2=character(0), ia = character(0)))
  }


  # 2. Go through all candidate tripplets and check them completely
  grid.ok = rep(FALSE, NROW(grid))
  row = 1
  for (row in 1:NROW(grid)) {
    mult = mat[, grid[row,1]] * mat[, grid[row,2]]
    s_mult = signif(mult, digits)
    ia = smat[, grid[row,3]]
    grid.ok[row] = all(is.true(s_mult==ia) | (is.na(ia) & is.na(s_mult)))
  }

  grid = grid[grid.ok,,drop=FALSE]

  tripplets = data.frame(f1 = cols[grid[,1]], f2 = cols[grid[,2]], ia = cols[grid[,3]])
  tripplets
}


