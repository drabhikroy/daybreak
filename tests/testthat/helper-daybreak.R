# Load the application the same way app.R does.
#
# This helper deliberately defines nothing of its own. A value supplied here
# would let a test pass against something the running app never provides, so
# every value has to come from the file that ships.

# testthat runs every test file from inside tests/testthat, so a path such as
# "R/version.R" only resolves from the project root. The root is found once by
# walking up to the folder that holds app.R, and every file reference in the
# tests goes through project_path() instead of relying on the working
# directory. That also lets the suite run from RStudio, the command line, or CI
# without anyone first changing directory.
find_daybreak_root <- function(start = getwd()) {
  here <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(here, "app.R")) && file.exists(file.path(here, "R", "version.R"))) return(here)
    parent <- dirname(here)
    if (identical(parent, here)) stop("Could not find the Daybreak project root above ", start, ".")
    here <- parent
  }
}

DAYBREAK_ROOT <- find_daybreak_root()

# app.R attaches shiny before sourcing the interface builders, which call div()
# and tags without a namespace. The helper does the same so the interface tests
# exercise those builders as the app does.
if (requireNamespace("shiny", quietly = TRUE)) suppressPackageStartupMessages(library(shiny))
project_path <- function(...) file.path(DAYBREAK_ROOT, ...)

# Sourced from the root so that anything an app file records about its own
# location at load time, such as the report template folder, matches what
# app.R would record.
local({
  old <- setwd(DAYBREAK_ROOT)
  on.exit(setwd(old), add = TRUE)
  source("R/version.R", local = globalenv())
  source("R/data_io.R", local = globalenv())
  source("R/catalog.R", local = globalenv())
  source("R/analysis_utils.R", local = globalenv())
  source("R/evidence.R", local = globalenv())
  source("R/imputation.R", local = globalenv())
  source("R/engines_core.R", local = globalenv())
  source("R/engines_advanced.R", local = globalenv())
  source("R/engines_ml.R", local = globalenv())
  source("R/engines_extra.R", local = globalenv())
  source("R/reporting.R", local = globalenv())
  source("R/explanations.R", local = globalenv())
  source("R/session_state.R", local = globalenv())
  source("R/local_models.R", local = globalenv())
  source("R/runtime.R", local = globalenv())
  source("R/plots.R", local = globalenv())
  source("R/exports.R", local = globalenv())
  source("R/ui_helpers.R", local = globalenv())
})

# A completed result used by several test files. Building it once here keeps
# the individual tests focused on the behavior they are checking.
demo_result <- function(method = "one_sample_t") {
  result_record(
    method, 40, 0,
    estimates = data.frame(Contrast = "mean minus 0", Estimate = 0.5, Lower = 0.2,
                           Upper = 0.8, Effect = 0.4, Effect_name = "Cohen's d"),
    tests = data.frame(Test = "One-sample t", Statistic = 3.2, df = 39, p = 0.003),
    checks = check_row("Normality", "No clear departure found", "Test detail", "ok"),
    conf_level = 0.95
  )
}
