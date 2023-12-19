example = function() {
  project = "aejapp_13_3_7"
  project_dir = file.path("~/repbox/projects_reg",project)
  rema = reg.match(project_dir)
  reg.df = rema$mreg.df
  reg = reg.df[20,]
  data = load.reg.dta(project_dir, reg)

  did.li = lapply(seq_len(NROW(reg.df)), function(i) {
    reg = reg.df[i,]
    data = load.reg.dta(project_dir, reg)
    did.df = stata.reg.did.info(data, reg)
  })

  reg.df$did = did.li
  did.df = stata.reg.did.info(data, reg)

  trip.df = stata.reg.find.did.triplets(reg, data)

  trip.df = add.fe.to.to.dummy.tripplets(data,trip.df)
  trip = trip.df[1,]
  delta.mat = did.delta.matrix(data, trip=trip)

  plot.did.delta.mat(delta.mat, NA)
  is.did.staggered(delta.mat)

  regs = df.rows.to.list(reg.df)
  did.df = lapply(regs, stata.reg.find.did.triplets, project_dir=project_dir) %>% bind_rows()

  reg$ct


  rstudioapi::filesPaneNavigate(paste0(project_dir,"/repbox"))
  rstudioapi::filesPaneNavigate("~/repbox/repboxReg/R")
}


stata.reg.did.info = function(data, reg) {
  data = load.reg.dta(project_dir, reg)
  trip.df = stata.reg.find.did.triplets(reg, data)
  if (NROW(trip.df)==0) return(NULL)
  trip.df = add.fe.to.to.dummy.tripplets(data,trip.df)

  trip.df = select(trip.df, - ..DUMMYROW)


  trip.df$num.fe1 = NA
  trip.df$num.fe2 = NA
  trip.df$is.balanced = NA
  trip.df$is.staggered = NA
  #trip.df$delta.mat = vector("list", NROW(trip.df))

  for (i in 1:NROW(trip.df)) {
    if (is.na(trip.df$fe1[i])) next
    delta.mat = did.delta.matrix(data, trip=trip)
    #plot.did.delta.mat(delta.mat, NA)
    trip.df$num.fe1[i] = NROW(delta.mat)
    trip.df$num.fe2[i] = NCOL(delta.mat)

    trip.df$is.balanced[i] = is.did.balanced(delta.mat)
    trip.df$is.staggered[i] = is.did.staggered(delta.mat)
  }

  trip.df

}

stata.reg.find.did.triplets = function(reg, df=load.reg.dta(project_dir, reg), project_dir) {
  restore.point("reg.find.triplets")
  delta.cand = get.delta.cand(df)
  ct = reg$ct[[1]]
  mct = reg$mct[[1]]
  xvar = ct$var
  pxvar = unique(mct$var)
  reg.delta.cand = intersect(delta.cand, pxvar)
  # We should extend this
  dummy.cand = get.dummy.vars(df)

  dummy.cand = intersect(dummy.cand, pxvar)

  res = find.did.triplets(df, reg.delta.cand,dummy.cand)

  if (is.null(res)) return(res)


  colnames(res)[2:3] = c("d1","d2")
  res$creg.num = rep(reg$creg.num, NROW(res))
  res
}
