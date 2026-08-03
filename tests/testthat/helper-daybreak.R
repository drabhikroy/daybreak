# Load the application the same way app.R does.
#
# This helper deliberately defines nothing of its own. An earlier version
# declared DAYBREAK_VERSION here, which meant the export and session tests
# passed against a constant that the running app never supplied. If a file
# needs a value, it has to come from the file that ships.

source("R/version.R")
source("R/data_io.R")
source("R/catalog.R")
source("R/analysis_utils.R")
source("R/imputation.R")
source("R/engines_core.R")
source("R/engines_advanced.R")
source("R/engines_ml.R")
source("R/reporting.R")
source("R/explanations.R")
source("R/session_state.R")
source("R/local_models.R")
source("R/plots.R")
source("R/exports.R")
source("R/ui_helpers.R")

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
