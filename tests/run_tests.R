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
  # The app starts in its own Rscript process, the way the Finder launcher
  # starts it, rather than in a fork of this session. A forked R session that
  # opens an httpuv server is fragile, and it shares nothing with how the app
  # is actually launched. The installer step is skipped in the child because
  # the suite above has already run with every package it needs, and this
  # check is only asking whether the app serves its page.
  port <- httpuv::randomPort()
  pid_file <- tempfile(fileext = ".pid")
  log_file <- tempfile(fileext = ".log")
  launch <- sprintf(
    paste0("writeLines(as.character(Sys.getpid()), %s); ",
           "options(daybreak.dependencies.checked = TRUE); ",
           "shiny::runApp(%s, port = %d, host = \"127.0.0.1\", launch.browser = FALSE)"),
    deparse(pid_file), deparse(normalizePath(".")), port
  )
  system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(launch)),
          stdout = log_file, stderr = log_file, wait = FALSE)
  on.exit({
    if (file.exists(pid_file)) try(tools::pskill(as.integer(readLines(pid_file, n = 1L))), silent = TRUE)
  }, add = TRUE)

  page <- character(0)
  deadline <- Sys.time() + 90
  while (!any(grepl("Daybreak", page, fixed = TRUE)) && Sys.time() < deadline) {
    Sys.sleep(1)
    page <- tryCatch(suppressWarnings(readLines(sprintf("http://127.0.0.1:%d", port), warn = FALSE)),
                     error = function(e) character(0))
  }
  if (!any(grepl("Daybreak", page, fixed = TRUE))) {
    status <- 1L
    message("The app did not serve its title during the startup check.")
    if (file.exists(log_file)) message(paste(utils::tail(readLines(log_file, warn = FALSE), 15), collapse = "\n"))
  } else message("Daybreak served its start page.")
  if (file.exists(pid_file)) try(tools::pskill(as.integer(readLines(pid_file, n = 1L))), silent = TRUE)
}

quit(status = status)
