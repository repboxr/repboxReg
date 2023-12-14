
rstudio_job = function(name, expr, libs=NULL, wdir = getwd(), script.dir = tempdir(), importEnv=TRUE) {
  expr = substitute(expr)
  restore.point("as_job")
  code = deparse1(expr)

  #code = paste0('setwd("',wdir,'")\n', code)
  if (!is.null(libs)) {
    code = paste0(paste0("library(", libs,")", collapse="\n"), "\n", code)
  }
  cat("\nRun \n\n",code,"\n\n")

  script.file = file.path(script.dir, paste0(name,".R"))
  writeLines(code, script.file)
  rstudioapi::jobRunScript(path = script.file,workingDir = wdir, name = name,importEnv = importEnv)

}



fe = function(val) {
  as.factor(val)
}


df.rows.to.list = function(df) {
  lapply(seq_len(NROW(df)), function(i) df[i,])
}

