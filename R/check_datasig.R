check.data.equal = function(dat, dta.file) {
  restore.point("check.data.equal")
  if (!file.exists(dta.file)) return(NA)
  stata.dat = rio::import(dta.file)

  all.equal(dat, stata.dat, check.attributes=FALSE, tolerance=1e-7, countEQ=FALSE)
}


check.datasig = function(dat, datasig) {
  restore.point("check.datasig")
  temp.dta = "repbox_temp.dta"
  temp.do = "repbox_temp.do"
  foreign::write.dta(dat, temp.dta)
  #rio::export(dat, temp.dta)
  do.code = '
use repbox_temp.dta
datasignature
file open repbox_file using "repbox_temp_datasig.txt", write replace
file write repbox_file "`r(datasignature)\'"
file write repbox_file _n
file close _all
'
  writeLines(do.code, temp.do)
  system(paste0("stata -b ", temp.do),wait = TRUE)
  newdatasig = readLines("repbox_temp_datasig.txt")
  list(same=datasig==newdatasig, newdatasig=newdatasig)
}
