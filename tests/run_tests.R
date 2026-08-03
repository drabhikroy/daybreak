#!/usr/bin/env Rscript

test_file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(test_file)) {
  project <- normalizePath(file.path(dirname(sub("^--file=", "", test_file[1])), ".."))
  setwd(project)
}

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Tests need testthat. Run install.packages('testthat').")
}

status <- 0L
tryCatch(
  testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE),
  error = function(e) {
    status <<- 1L
    message(conditionMessage(e))
  }
)

if (status == 0L && requireNamespace("shiny", quietly = TRUE) && .Platform$OS.type == "unix") {
  message("\nStartup check")
  port <- httpuv::randomPort()
  process <- parallel::mcparallel(shiny::runApp(".", port = port, host = "127.0.0.1", launch.browser = FALSE), silent = TRUE)
  on.exit(try(parallel::mckill(process), silent = TRUE), add = TRUE)
  Sys.sleep(4)
  page <- tryCatch(readLines(sprintf("http://127.0.0.1:%d", port), warn = FALSE), error = function(e) character(0))
  if (!any(grepl("Daybreak", page, fixed = TRUE))) {
    status <- 1L
    message("The app did not serve its title during the startup check.")
  } else message("Daybreak served its start page.")
}

quit(status = status)
