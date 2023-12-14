example = function() {
  dat = readRDS("../sme.Rds")
  dat$did_dummy = dat$treat*dat$exp

  delta.cand = get.delta.cand(dat)

  colnames(dat)
  find.did.triplets(dat, delta.cand)
  find.did.triplets(dat, delta.cand, fe.cand = c("dma","date"))

  did.is.delta.group.period(dat$did_dummy, dat$treat, dat$exp)

}



find.did.triplets = function(df, delta.cand = get.delta.cand(df),dummy.cand = delta.cand, fe.cand = NULL, normalize.df = TRUE) {
  restore.point("find.did.triplets")

  if (length(delta.cand) < 1) return(NULL)

  if (normalize.df) {
    for (var in union(delta.cand, dummy.cand)) {
      df[[var]] = as.integer(df[[var]])
    }
  }

  vars = colnames(df)

  delta.inds = match(delta.cand, vars)
  dummy.inds = match(dummy.cand, vars)
  fe.inds = match(fe.cand, vars)

  mat=do.call(rbind,lapply(delta.inds, internal.find.did.triplets.for.delta, dummy.inds=dummy.inds, fe.inds = fe.inds, df=df))

  if (NROW(mat)==0) return(NULL)

  data.frame(delta = vars[mat[,1]], f1 = vars[mat[,2]], f2 = vars[mat[,3]])
}

internal.find.did.triplets.for.delta = function(delta.ind,dummy.inds, fe.inds = NULL, df) {
  restore.point("internal.find.did.triplets.for.delta")

  rows = !is.na(df[[delta.ind]])
  df = df[rows,]
  delta = df[[delta.ind]]

  # 1. Transform fe cols to dummy factors specific to delta
  for (fe.ind in fe.inds) {
    fe = df[[fe.ind]]
    fe.one = unique(fe[delta == 1])
    df[[fe.ind]] = (fe %in% fe.one)*1L
  }

  # 2. Specify factor.inds as union of dummy.inds and fe.inds

  factor.inds = setdiff(union(dummy.inds, fe.inds), delta.ind)
  if (length(factor.inds) < 2) return(NULL)

  # 3. First check neccessary condition:
  #    each DID factor must be 1 where delta is 1
  #    and the DID factor should be unequal delta

  delta.rows = delta == 1
  can.be.factor = sapply(factor.inds, function(factor.ind) {
    all(df[[factor.ind]][delta.rows] == 1L) &
      !identical(df[[factor.ind]], delta)
  })
  factor.inds = factor.inds[can.be.factor]
  if (length(factor.inds) < 2) return(NULL)

  # 4. Find triplets that also satisfy that
  #    factor1*factor2 == 0 where delta is 0

  delta.zero.rows = !delta.rows

  factor.grid = expand.triangular(factor.inds)

  ok = which(sapply(seq_len(NROW(factor.grid)), function(i) {
    f1 = df[[ factor.inds[factor.grid[i,1]] ]]
    f2 = df[[ factor.inds[factor.grid[i,2]] ]]
    all(f1[delta.zero.rows]*f2[delta.zero.rows] == 0L)
  }))
  if (length(ok)==0) return(NULL)

  factor.grid = factor.grid[ok,, drop=FALSE]
  na.omit(cbind(delta.ind, factor.grid))
}


expand.triangular = function(set) {
  if (length(set)<=1) return(NULL)
  ind.grid = expand.triangular.ind(length(set))
  cbind(set[ind.grid[,1]], set[ind.grid[,2]])
}

expand.triangular.ind = function(K) {
  if (K <= 1) return(NULL)
  grid = expand.grid(1:(K-1),2:K)
  grid[grid[,1] < grid[,2],]
}


get.dummy.vars = function(df) {
  colnames(df)[sapply(df, is.dummy.var)]
}

is.dummy.var = function(delta) {
  #if (is.logical(delta)) delta = suppressWarnings(as.integer(delta))
  if (!is.numeric(delta)) return(FALSE)
  delta = delta[(!is.na(delta))]
  all(delta==1 | delta==0)
}


get.delta.cand = function(df) {
  colnames(df)[sapply(df, could.be.delta)]
}

could.be.delta = function(delta) {
  if (!is.numeric(delta)) return(FALSE)
  delta = delta[(!is.na(delta))]
  is.dummy = all(delta==1 | delta==0)
  if (!is.dummy) return(FALSE)
  any(delta==1) & any(delta==0)
}


did.fe.to.dummy.factor = function(delta, fe) {
  fe.one = unique(fe[delta == 1])
  (fe %in% fe.one)*1L
}

add.fe.to.to.dummy.tripplets = function(data, trip.df) {
  restore.point("add.fe.to.dummy.tripplet.df")
  trip.df$..DUMMYROW = seq_along(NROW(trip.df))

  fe.df = lapply(seq_len(NROW(trip.df)), function(i) {
    res = find.fe.for.did.dummies(data, trip.df$delta[i],trip.df$d1[i], trip.df$d2[i])
    if (NROW(res)==0) return(NULL)
    data.frame(fe1=res[,1],fe2 = res[,2], ..DUMMYROW=i)
  }) %>% bind_rows()

  if (NROW(fe.df)==0) {
    trip.df$fe1 = NA
    trip.df$fe2 = NA
  } else {
    trip.df = full_join(trip.df,fe.df, by=c("..DUMMYROW"))
  }
  trip.df
}

# Assume we have a did tripplet (delta,d1,d2) of dummy variables
# We want to find fixed effects variables fe1 and fe2 in the data set
# that could correspond to d1 and d2 respectively.
# One fe variable will typically be a time identifier, the other one a
# unit identifier.
#
# If we want to make a further assessment of the data set like
# being balanced or having staggered adoption we need these fe variables.
# They may not always be available, however.

find.fe.for.did.dummies = function(data, delta, d1,d2, cols=NA) {
  restore.point("find.fe.for.did.dummies")
  if (identical(cols,NA)) {
    cols = fe.candidate.cols(data)
  }

  if (length(cols)==0) return(NULL)

  # Condition 1
  #
  # The fixed effect should only either have a 0 or a 1
  # for the dummy var, not both.

  can.be.dummy.cand = function(col) {
    length(intersect(dat0[[col]], dat1[[col]])) == 0
  }

  dat0 = data[data[[d1]]==0,]
  dat1 = data[data[[d1]]==1,]
  fe.cand1 = cols[sapply(cols, can.be.dummy.cand)]

  if (length(fe.cand1)==0) return(NULL)


  dat0 = data[data[[d2]]==0,]
  dat1 = data[data[[d2]]==1,]
  fe.cand2 = cols[sapply(cols, can.be.dummy.cand)]

  if (length(fe.cand2)==0) return(NULL)

  # Condition 2
  #
  # This means no pair of (fe1, fe2) valuas should be duplicated
  # in the data set
  fe.grid = expand.grid(fe1=fe.cand1, fe2 = fe.cand2,stringsAsFactors = FALSE)
  fe.grid = fe.grid[fe.grid[,1] != fe.grid[,2],]

  if (NROW(fe.grid)==0) return(NULL)
  fe.grid$ok = FALSE

  # For speed use data.table
  dt = as.data.table(data)
  i = 1
  for (i in 1:NROW(fe.grid)) {
    fe.grid$ok[i] = anyDuplicated(dt, by=c(fe.grid[i,1],fe.grid[i,2])) == 0
  }

  fe.grid[fe.grid$ok,1:2]
}

fe.candidate.cols = function(data) {
  is.cand = sapply(data, function(v) {
    if (is.character(v) | is.factor(v) | is.integer(v)) return(TRUE)
    if (is.numeric(v)) return(all(as.integer(v) == v,na.rm = TRUE))
    FALSE
  })
  names(data)[is.cand]
}

# The DID delta matrix has one row for each fe1 value and
# one column for each fe2 value
# It's values are equal to the interaction dummy of interest: delta
#
# The DID delta matrix can be used to quickly check for balancedness
# and staggered treatment adoption
did.delta.matrix = function(data, delta=trip$delta, fe1=trip$fe1, fe2=trip$fe2, trip=NULL) {
  restore.point("make.delta.matrix")

  fe1.val = unique(data[[fe1]])
  fe2.val = unique(data[[fe2]])

  nr = length(fe1.val)
  nc = length(fe2.val)

  mat = matrix(NA_integer_, nrow = nr, ncol=nc)

  fe1.ind = match(data[[fe1]], fe1.val)
  fe2.ind = match(data[[fe2]], fe2.val)

  rows = !is.na(fe1.ind) & !is.na(fe2.ind)
  mat[cbind(fe1.ind[rows], fe2.ind[rows])] = data[[delta]][rows]

  rownames(mat) = as.character(fe1.val)
  colnames(mat) = as.character(fe2.val)
  attr(mat,"rowvar") = fe1
  attr(mat,"colvar") = fe2
  mat
}

is.did.balanced = function(delta.mat) {
  !any(is.na(delta.mat))
}

plot.did.delta.mat = function(delta.mat, na.val=NA) {
  delta.mat[is.na(delta.mat)] = na.val

  rowvar = attr(delta.mat,"rowvar")
  colvar = attr(delta.mat,"colvar")

  xlab = ifelse(is.null(rowvar),"",rowvar)
  ylab = ifelse(is.null(colvar),"",colvar)


  image(x=1:NROW(delta.mat), y=1:NCOL(delta.mat), z=delta.mat, breaks=c(-0.1,0.9,1.1), col=c("#eeeeaa","#5555dd"),xlab=xlab,ylab=ylab)


  #filled.contour(x=1:NROW(delta.mat), y=1:NCOL(delta.mat), z=delta.mat, nlevels=2)
}

# Check if DID has staggered treatment adoption
is.did.staggered = function(delta.mat, balanced = is.did.balanced(delta.mat), show.warnings = TRUE) {
  restore.point("is.did.staggered")

  # A balanced delta.mat has no NA
  #
  # If not staggered we could form
  # a rectangle of 1 by rearranging the rows
  # and columns appropriately
  # This would mean that row and
  # colsums can only take two values
  # one of which is a zero
  if (balanced) {
    cs = unique(colSums(delta.mat))
    rs = unique(rowSums(delta.mat))

    if (length(cs) > 2 | length(rs) > 2) return(TRUE)

    if (min(cs)>0 | min(rs) > 0) {
      if (show.warnings) {
        cat("\nWe have a DID delta matrix that is not really staggered but has a row or col that completely consists of a 1. Please check out.\n")
      }
      return(NA)
    }

    return(FALSE)
  }

  # We have an unbalanced set-up
  # This means delta.mat has NA

  # We probably cannot rule the possibility
  # of staggered adoption with NA.
  # So returning FALSE is tricky.

  # But we can return TRUE if we know
  # that no rectangle of 1s could be formed
  # if we can flexibly replace each NA by either
  # a 0 or a 1.


  # We use the following heuristic:
  #
  # 1. First check if there is at leat one column
  #    and one row that could have a zero everywhere

  delta0.mat[is.na(delta.mat)] = 0
  cs.min = colSums(delta0.mat)

  # Is staggered: not any cs.min is zero
  if (!any(cs.min==0)) return(TRUE)

  rs.min = rowSums(delta0.mat)
  # Is staggered: not any rs.min is zero
  if (!any(cs.min==0)) return(TRUE)

  # Assume we have no staggering if we would have
  # no staggering given that all NA are zero.
  zero.staggered = is.did.staggered(delta0.mat,balanced=TRUE, show.warnings= FALSE)
  if (isTRUE(!zero.staggered)) {
    return(FALSE)
  }

  delta1.mat[is.na(delta.mat)] = 1

  # Now we check whether between
  # max(cs.min) and min(cs.max) there
  # is a positive integer that works for all
  # which cannot be zero
  cs.max = colSums(delta1.mat)

  no.zero = which(cs.min > 0)

  if (length(no.zero)>0) {
    start = max(cs.min[no.zero])
    end = min(cs.max[no.zero])
    if (end < start) return(TRUE)
  }

  # Do the same check for rs.min and rs.max
  rs.max = rowSums(delta1.mat)
  no.zero = which(rs.min > 0)

  if (length(no.zero)>0) {
    start = max(rs.min[no.zero])
    end = min(rs.max[no.zero])
    if (end < start) return(TRUE)
  }
  return(NA)
}
