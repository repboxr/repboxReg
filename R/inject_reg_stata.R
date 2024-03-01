
# Will be called from repboxStata
#
# Inject regression specific information
injection.reg = function(txt, lines=seq_along(txt),do, opts=rbs.opts()) {
  restore.point("injection.reg")

  repbox.dir = file.path(do$project_dir,"repbox/stata")
  res.dir = file.path(repbox.dir,"tsv")

  res.files = paste0(res.dir,"/",do$donum,"_",  lines,"_`repbox_local_cmd_count'",".dta")

paste0('
', end.injection(do$donum, lines, "RUNCMD",do),'
* REGRESSION INJECTION START
parmest, label saving("', res.files,'", replace)

',post.injection(txt,lines,do=do, report.xtset=TRUE),'
display "#~# INJECT REG_ERETURN ', do$donum,' ', lines,' `repbox_local_cmd_count\'"
ereturn list
display "#~# END INJECT REG_ERETURN ',do$donum,' ', lines,' `repbox_local_cmd_count\'"

* REGRESSION INJECTION END
')

}



# Still uses estout instead of parmest
injection.reg.old = function(txt, lines=seq_along(txt),do, opts=rbs.opts()) {
  restore.point("injection.reg")

  repbox.dir = file.path(do$project_dir,"repbox/stata")
  tsv.dir = file.path(repbox.dir,"tsv")

  tsv.files = paste0(tsv.dir,"/",do$donum,"_",  lines,"_`repbox_local_cmd_count'",".tsv")

  paste0('
', end.injection(do$donum, lines, "RUNCMD",do),'
* REGRESSION INJECTION START
capture noisily estout . using "',tsv.files,'", cells("b se t p ci_l ci_u") replace
',post.injection(txt,lines,do=do, report.xtset=TRUE),'
display "#~# INJECT REG_ERETURN ', do$donum,' ', lines,' `repbox_local_cmd_count\'"
ereturn list
display "#~# END INJECT REG_ERETURN ',do$donum,' ', lines,' `repbox_local_cmd_count\'"

* REGRESSION INJECTION END
')

}
