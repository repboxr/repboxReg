# Compare the global variable dat with the Stata data set of a currently open
# Stata session in the terminal.
dat.vs.stata.dat = function() {
  dat = globalenv()$dat

  # Get current Stata data set
  id = terminalVisible()
  if (is.null(id)) {
    cat("\nNo terminal is open. First run\n\nAddins -> Start Stata\n\nto start Stata in a terminal.")
    return(invisible())
  }
  temp.file = tempfile(fileext = ".dta")
  cmd = paste0("save ", temp.file, "\n")
  library(rstudioapi)
  rstudioapi::terminalSend(id, cmd)
  time.out = 5
  start.time = Sys.time()
  while (TRUE) {
    stata_data = try(haven::read_dta(temp.file), silent = TRUE)
    if (!is(stata_data, "try-error")) {
      .GlobalEnv$stata_data = stata_data
      file.remove(temp.file)
      return(
        all_equal(dat, stata_data))
    }
    if (as.numeric(Sys.time() - start.time) > time.out)
      break
  }
  cat("\nCould not retrieve Stata data.\n")
  return(invisible())
}
