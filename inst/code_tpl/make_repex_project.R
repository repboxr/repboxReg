
make.repex.project = function() {
  sup.dir = file.path(project_dir, "org")
  if (!dir.exists(sup.dir)) dir.create(sup.dir, recursive = TRUE)

  code = readLines(file.path(repex.dir,"stata_code.do"))
  line = first(which(startsWith(code,"use")))
  cache.file = str.between(code[line], '"','"')

  code[line] = "use repex.dta"
  writeLines(code, file.path(sup.dir,"repex.do"))
  file.copy(cache.file, file.path(sup.dir,"repex.dta"),overwrite = TRUE)
}
make.repex.project()

library(repboxMain)
library(repboxStata)
library(repboxReg)

update.repbox.project(project_dir,stata_opts = repbox_stata_opts(all.do.timeout = 60*5,timeout = 60*5), run.lang = "stata",make.html = FALSE,make.matching = FALSE)
dap_and_cache_remove_from_project(project_dir)

run.project.with.reg(project_dir)

mr = mr_base_run_study(project_dir, stop.on.error = TRUE,create.repdb = TRUE)

rstudioapi::filesPaneNavigate(project_dir)
