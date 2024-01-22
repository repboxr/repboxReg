mr_filename_extractor_type__step = function(files, files_col=FALSE) {
  bases = basename(files)
  type = str.left.of(bases,"__")
  steps = str.between(bases,"__",".txt") %>% as.integer()
  if (files_col) {
    tibble(file=files,base=bases, type=type, step=steps)
  } else {
    tibble(type=type, step=steps)
  }
}

