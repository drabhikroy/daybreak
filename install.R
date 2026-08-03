# Install Daybreak dependencies.

if (getRversion() < "4.0.0") {
  stop("Daybreak needs R 4.0 or later. Update R, then run this installer again.")
}

# Use a library owned by the person running Daybreak. This avoids a system
# password prompt when the app is opened from Finder or RStudio.
r_series <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = ".")
user_library <- file.path(tools::R_user_dir("daybreak", which = "data"), "library", r_series)
if (!dir.exists(user_library) && !dir.create(user_library, recursive = TRUE, showWarnings = FALSE)) {
  stop("Daybreak could not create a personal R package library at ", user_library, ".")
}
if (file.access(user_library, 2) != 0L) {
  stop("Daybreak cannot write to the personal R package library at ", user_library, ".")
}
.libPaths(unique(c(user_library, .libPaths())))

core <- c("shiny", "ggplot2", "dplyr", "tidyr", "readr", "scales")
core_minimum <- c(
  shiny = "1.6.0",
  ggplot2 = "3.4.0",
  dplyr = "1.0.0",
  tidyr = "1.2.0",
  readr = "2.0.0",
  scales = "1.2.0"
)
advanced <- c("lavaan", "lme4", "lmerTest", "nnet", "MASS", "psych", "survival", "nlme")
machine_learning <- c("rpart", "ranger", "class", "glmnet", "e1071", "dbscan")
missing_data <- c("mice", "missForest", "VIM")
imports <- c("readxl", "readODS", "haven", "jsonlite", "arrow")
exports <- c("rmarkdown", "knitr", "zip", "svglite", "ragg", "magick", "tikzDevice")
testing <- c("testthat")
local_models <- c("httr", "jsonlite", "processx")

packages_to_install <- function(packages, minimum = NULL) {
  packages[vapply(packages, function(package) {
    if (!requireNamespace(package, quietly = TRUE)) return(TRUE)
    required <- minimum[[package]]
    length(required) && utils::packageVersion(package) < package_version(required)
  }, logical(1))]
}

install_needed <- function(packages, minimum = NULL) {
  needed <- packages_to_install(packages, minimum)
  if (length(needed)) {
    message("Installing: ", paste(needed, collapse = ", "))
    install.packages(needed, dependencies = c("Depends", "Imports", "LinkingTo"))
  }
  invisible(needed)
}

repos <- getOption("repos")
cran_repo <- if (length(repos) && "CRAN" %in% names(repos)) repos[["CRAN"]] else NA_character_
if (is.null(repos) || !length(repos) || is.na(cran_repo) || identical(cran_repo, "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

install_needed(core, core_minimum)
all_optional <- unique(c(advanced, machine_learning, missing_data, imports, exports, local_models, testing))
install_needed(all_optional)

# Only the six core packages can stop the app. Several optional packages need
# system software that a given Mac may not have: magick needs ImageMagick,
# tikzDevice needs a TeX installation, and arrow ships a large compiled
# library. Treating any one of those as fatal meant a missing PSD export
# blocked the whole application from opening. Each method already names its own
# missing package before it tries to fit, so an absent optional package costs
# the reader one method rather than the app.
core_missing <- core[!vapply(core, requireNamespace, logical(1), quietly = TRUE)]
if (length(core_missing)) {
  stop(
    "Daybreak could not install these required packages: ", paste(core_missing, collapse = ", "),
    ". Check the messages above, then run source(\"install.R\") again."
  )
}

optional_missing <- all_optional[!vapply(all_optional, requireNamespace, logical(1), quietly = TRUE)]
options(daybreak.optional.missing = optional_missing)
options(daybreak.dependencies.checked = TRUE)

if (length(optional_missing)) {
  message(
    "Daybreak is ready. These optional packages are not installed, so the features that use them stay unavailable: ",
    paste(optional_missing, collapse = ", "), "."
  )
} else {
  message("Daybreak packages are ready. Run source(\"run_daybreak.R\") from this folder.")
}
