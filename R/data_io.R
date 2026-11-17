# Data import and built-in examples ---------------------------------------

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

SUPPORTED_EXTENSIONS <- c(
  "csv", "tsv", "txt", "xlsx", "xls", "ods", "sav", "zsav", "por",
  "dta", "sas7bdat", "xpt", "rds", "rdata", "rda", "json", "parquet", "feather"
)

IMPORT_PACKAGES <- list(
  xlsx = "readxl", xls = "readxl", ods = "readODS",
  sav = "haven", zsav = "haven", por = "haven", dta = "haven",
  sas7bdat = "haven", xpt = "haven", json = "jsonlite",
  parquet = "arrow", feather = "arrow"
)

extension_of <- function(path, original_name = path) {
  ext <- tolower(tools::file_ext(original_name))
  if (!nzchar(ext)) ext <- tolower(tools::file_ext(path))
  ext
}

spreadsheet_sheets <- function(path, original_name) {
  ext <- extension_of(path, original_name)
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) return(character(0))
    return(readxl::excel_sheets(path))
  }
  if (identical(ext, "ods")) {
    if (!requireNamespace("readODS", quietly = TRUE)) return(character(0))
    return(readODS::list_ods_sheets(path))
  }
  character(0)
}

clean_imported_data <- function(x) {
  if (!is.data.frame(x)) x <- as.data.frame(x, check.names = FALSE)
  if (!ncol(x)) stop("The file does not contain any columns.")
  if (!nrow(x)) stop("The file contains column names but no data rows.")
  cleaned_names <- trimws(names(x))
  blank <- is.na(cleaned_names) | !nzchar(cleaned_names)
  cleaned_names[blank] <- paste0("Column_", which(blank))
  names(x) <- make.unique(cleaned_names, sep = "_")
  if (requireNamespace("haven", quietly = TRUE)) {
    x[] <- lapply(x, function(col) {
      if (inherits(col, "haven_labelled")) haven::as_factor(col, levels = "default") else col
    })
  }
  # Text columns become factors only when the number of distinct values looks
  # like a category set rather than free text or an identifier. The ceiling
  # scales with file size so that a 40 row file does not treat 30 distinct
  # comments as 30 categories, and a 5,000 row file can still recognize a
  # 50 level site code. The column profile in the Data used tab reports the
  # resulting type so the reader can see what happened.
  x[] <- lapply(x, function(col) {
    if (is.character(col)) {
      col[trimws(col) == ""] <- NA_character_
      unique_count <- length(unique(stats::na.omit(col)))
      if (unique_count <= min(50, max(12, floor(length(col) * 0.2)))) factor(col) else col
    } else col
  })
  rownames(x) <- NULL
  x
}

# Reading a file ------------------------------------------------------------
#
# Format is decided from the extension rather than by inspecting content,
# because a wrong guess on a large file is slow and gives a confusing error.
# Each branch is one call into a package that specializes in that format.
read_uploaded_data <- function(path, original_name, sheet = NULL,
                               delimiter = "auto", decimal = ".") {
  ext <- extension_of(path, original_name)
  if (!ext %in% SUPPORTED_EXTENSIONS) {
    stop("This file type is not supported. Use CSV, TSV, Excel, ODS, SPSS, Stata, SAS, RDS, JSON, Parquet, or Feather.")
  }
  pkg <- IMPORT_PACKAGES[[ext]]
  if (!is.null(pkg) && !requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Reading .%s files needs the %s package. Run install.packages(\"%s\") and restart the app.", ext, pkg, pkg))
  }

  x <- switch(ext,
    csv = {
      locale <- readr::locale(decimal_mark = decimal)
      if (identical(delimiter, "auto")) {
        readr::read_delim(path, delim = NULL, locale = locale, show_col_types = FALSE,
                          progress = FALSE, name_repair = "unique")
      } else {
        readr::read_delim(path, delim = delimiter, locale = locale, show_col_types = FALSE,
                          progress = FALSE, name_repair = "unique")
      }
    },
    tsv = readr::read_tsv(path, show_col_types = FALSE, progress = FALSE, name_repair = "unique"),
    txt = readr::read_delim(path, delim = if (identical(delimiter, "auto")) NULL else delimiter,
                            locale = readr::locale(decimal_mark = decimal),
                            show_col_types = FALSE, progress = FALSE, name_repair = "unique"),
    xlsx = readxl::read_excel(path, sheet = sheet %||% 1, .name_repair = "unique"),
    xls = readxl::read_excel(path, sheet = sheet %||% 1, .name_repair = "unique"),
    ods = readODS::read_ods(path, sheet = sheet %||% 1, col_names = TRUE),
    # user_na is left at its default so that SPSS user-missing codes such as
    # 97, 98, and 99 arrive as NA. Read as values, "Refused" and "Not
    # applicable" would become ordinary categories and quietly shift means and
    # category counts.
    sav = haven::read_sav(path),
    zsav = haven::read_sav(path),
    por = haven::read_por(path),
    dta = haven::read_dta(path),
    sas7bdat = haven::read_sas(path),
    xpt = haven::read_xpt(path),
    rds = { require_safe_deserialization(".rds files"); readRDS(path) },
    rdata = read_rdata_frame(path),
    rda = read_rdata_frame(path),
    json = jsonlite::fromJSON(path, flatten = TRUE),
    parquet = arrow::read_parquet(path),
    feather = arrow::read_feather(path)
  )
  clean_imported_data(x)
}

# R releases before 4.4.0 can run code hidden in a crafted .rds or .RData file
# while it is being read (CVE-2024-27322). Data and session files are exactly
# the kind of thing people send each other, so on an affected R the app
# declines to open them and says how to proceed, instead of trusting the file.
require_safe_deserialization <- function(what, version = getRversion()) {
  if (version < "4.4.0") {
    stop(sprintf(
      "Opening %s needs R 4.4.0 or later. Earlier versions of R can run code hidden inside a crafted file of this kind. Update R, or save the data as a CSV file and open that instead.",
      what
    ))
  }
  invisible(TRUE)
}

read_rdata_frame <- function(path) {
  require_safe_deserialization(".RData files")
  box <- new.env(parent = emptyenv())
  object_names <- load(path, envir = box)
  frames <- object_names[vapply(object_names, function(nm) is.data.frame(box[[nm]]), logical(1))]
  if (length(frames) != 1) {
    stop("An RData file must contain exactly one data frame. Save the desired object as RDS when several objects are present.")
  }
  box[[frames[[1]]]]
}

SAMPLES <- list(
  iris = list(
    name = "Anderson iris flowers",
    note = "Measurements of 150 iris flowers from three species. Useful for description, ANOVA, correlation, regression, MANOVA, PCA, and clustering.",
    method = "one_way_anova"
  ),
  tooth = list(
    name = "Guinea pig tooth growth",
    note = "Tooth length by vitamin C dose and delivery method. Useful for t tests, ANOVA, and regression.",
    method = "one_way_anova"
  ),
  cars = list(
    name = "1974 automobile road tests",
    note = "Fuel economy and design features for 32 cars. Useful for correlation, linear regression, PCA, and clustering.",
    method = "linear_regression"
  ),
  air = list(
    name = "New York air quality",
    note = "Daily air measurements from May through September 1973. Includes genuine missing values for data-quality work.",
    method = "missingness"
  ),
  admissions = list(
    name = "UC Berkeley graduate admissions",
    note = "Admission outcomes by department and applicant sex from 1973. Useful for contingency tables and logistic regression.",
    method = "chi_square"
  ),
  orthodont = list(
    name = "Orthodontic growth measurements",
    note = "Repeated craniofacial measurements for children. Useful for hierarchical and growth models.",
    method = "growth_model",
    requires = "nlme"
  ),
  political = list(
    name = "Political democracy indicators",
    note = "Country-level indicators used in published SEM examples. Useful for CFA and SEM.",
    method = "sem",
    requires = "lavaan"
  ),
  lung = list(
    name = "Veterans' lung cancer survival",
    note = "Survival times and clinical measures from a randomized trial. Useful for Kaplan-Meier and Cox models.",
    method = "kaplan_meier",
    requires = "survival"
  ),
  nottem = list(
    name = "Nottingham temperatures",
    note = "Monthly air temperatures in Nottingham from 1920 through 1939. Useful for time-series analysis.",
    method = "time_series"
  ),
  diagnoses = list(
    name = "Multiple sclerosis diagnoses by two neurologists",
    note = "Two neurologists each rated the same 149 patients as certain, probable, possible, or doubtful. Useful for rater agreement.",
    method = "agreement"
  ),
  bcg = list(
    name = "BCG vaccine trials",
    note = "Thirteen trials of the BCG vaccine against tuberculosis, one row per trial with a log risk ratio and its standard error. Useful for meta-analysis.",
    method = "meta_analysis",
    requires = "metafor"
  )
)

default_sample_syntax <- function(sample_id, method_id) {
  if (!identical(sample_id, "political") || !method_id %in% c("cfa", "sem")) return(NULL)
  measurement <- paste(
    "ind60 =~ x1 + x2 + x3",
    "dem60 =~ y1 + y2 + y3 + y4",
    "dem65 =~ y5 + y6 + y7 + y8",
    sep = "\n"
  )
  if (method_id == "cfa") return(measurement)
  paste(
    measurement,
    "dem60 ~ ind60",
    "dem65 ~ ind60 + dem60",
    "y1 ~~ y5",
    "y2 ~~ y4 + y6",
    "y3 ~~ y7",
    "y4 ~~ y8",
    "y6 ~~ y8",
    sep = "\n"
  )
}

available_samples <- function() {
  keep <- vapply(SAMPLES, function(x) {
    is.null(x$requires) || requireNamespace(x$requires, quietly = TRUE)
  }, logical(1))
  SAMPLES[keep]
}

sample_choices <- function() {
  x <- available_samples()
  stats::setNames(names(x), vapply(x, `[[`, character(1), "name"))
}

# The Winnipeg patients from Westlund and Kurland (1953), in the form Landis
# and Koch (1977) used to introduce their reading of kappa. The published table
# is expanded to one row per patient, which is the shape a reader's own rating
# file will have.
sample_diagnoses <- function() {
  grades <- c("Certain", "Probable", "Possible", "Doubtful")
  counts <- matrix(c(38, 5, 0, 1,
                     33, 11, 3, 0,
                     10, 14, 5, 6,
                     3, 7, 3, 10), nrow = 4, byrow = TRUE)
  cells <- expand.grid(winnipeg = seq_along(grades), new_orleans = seq_along(grades))
  times <- counts[cbind(cells$new_orleans, cells$winnipeg)]
  rows <- cells[rep(seq_len(nrow(cells)), times), ]
  data.frame(
    patient = seq_len(nrow(rows)),
    new_orleans_neurologist = factor(grades[rows$new_orleans], levels = grades),
    winnipeg_neurologist = factor(grades[rows$winnipeg], levels = grades)
  )
}

# The thirteen trials collected by Colditz and colleagues (1994). The counts
# are the published ones, and the log risk ratio and its standard error are
# computed from them here so the arithmetic is on the page.
sample_bcg <- function() {
  d <- data.frame(
    study = c("Aronson 1948", "Ferguson and Simes 1949", "Rosenthal 1960", "Hart and Sutherland 1977",
              "Frimodt-Moller 1973", "Stein and Aronson 1953", "Vandiviere 1973", "TPT Madras 1980",
              "Coetzee and Berjak 1968", "Rosenthal 1961", "Comstock 1974", "Comstock and Webster 1969",
              "Comstock 1976"),
    year = c(1948, 1949, 1960, 1977, 1973, 1953, 1973, 1980, 1968, 1961, 1974, 1969, 1976),
    vaccinated_cases = c(4, 6, 3, 62, 33, 180, 8, 505, 29, 17, 186, 5, 27),
    vaccinated_healthy = c(119, 300, 228, 13536, 5036, 1361, 2537, 87886, 7470, 1699, 50448, 2493, 16886),
    control_cases = c(11, 29, 11, 248, 47, 372, 10, 499, 45, 65, 141, 3, 29),
    control_healthy = c(128, 274, 209, 12619, 5761, 1079, 619, 87892, 7232, 1600, 27197, 2338, 17825),
    latitude = c(44, 55, 42, 52, 13, 44, 19, 13, 27, 42, 18, 33, 33),
    stringsAsFactors = FALSE
  )
  vaccinated <- d$vaccinated_cases + d$vaccinated_healthy
  control <- d$control_cases + d$control_healthy
  d$log_risk_ratio <- log((d$vaccinated_cases / vaccinated) / (d$control_cases / control))
  d$standard_error <- sqrt(1 / d$vaccinated_cases - 1 / vaccinated + 1 / d$control_cases - 1 / control)
  d
}

# The built-in examples ------------------------------------------------------
#
# Each sample exists to demonstrate a particular method, and each one is
# reshaped here so that its columns carry names and types a reader can act on
# without a codebook.
load_sample <- function(id) {
  x <- switch(id,
    iris = datasets::iris,
    tooth = transform(datasets::ToothGrowth, dose = factor(dose), supp = factor(supp)),
    cars = transform(datasets::mtcars, cyl = factor(cyl), vs = factor(vs), am = factor(am),
                     gear = factor(gear), carb = factor(carb)),
    air = transform(datasets::airquality, Month = factor(Month)),
    admissions = {
      d <- as.data.frame(datasets::UCBAdmissions, responseName = "Count")
      d[rep(seq_len(nrow(d)), d$Count), c("Admit", "Gender", "Dept"), drop = FALSE]
    },
    orthodont = as.data.frame(nlme::Orthodont),
    political = lavaan::PoliticalDemocracy,
    lung = {
      # survival::veteran codes status as 0 for censored and 1 for dead. Any
      # other mapping turns the deaths into censored rows and leaves the column
      # with a single observed level, which keeps it out of the event status
      # selector.
      d <- survival::veteran
      d$status <- factor(d$status, levels = c(0, 1), labels = c("Censored", "Event"))
      d
    },
    nottem = data.frame(
      date = seq(as.Date("1920-01-01"), by = "month", length.out = length(datasets::nottem)),
      temperature_f = as.numeric(datasets::nottem)
    ),
    diagnoses = sample_diagnoses(),
    bcg = sample_bcg(),
    stop("Unknown sample.")
  )
  clean_imported_data(x)
}

column_profile <- function(data) {
  data.frame(
    Variable = names(data),
    Type = vapply(data, display_type, character(1)),
    Complete = vapply(data, function(x) sum(!is.na(x)), integer(1)),
    Missing = vapply(data, function(x) sum(is.na(x)), integer(1)),
    Unique = vapply(data, function(x) length(unique(stats::na.omit(x))), integer(1)),
    stringsAsFactors = FALSE
  )
}

display_type <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXt"))) return("Date/time")
  if (is.ordered(x)) return("Ordered category")
  if (is.factor(x)) return("Category")
  if (is.logical(x)) return("True/false")
  if (is.integer(x)) return("Integer")
  if (is.numeric(x)) return("Numeric")
  if (is.character(x)) return("Text")
  class(x)[[1]]
}

# A compact profile of the loaded columns, used for the role menus and the
# feasibility hints. Numeric columns with very few distinct values are listed
# as categorical as well as numeric, because a variable coded 1 through 5 is
# usually a rating and is sometimes a count, and only the analyst knows which.
data_signature <- function(data) {
  numeric <- names(data)[vapply(data, is.numeric, logical(1))]
  categorical <- names(data)[vapply(data, function(x) {
    is.factor(x) || is.character(x) || is.logical(x) ||
      length(unique(stats::na.omit(x))) <= 12
  }, logical(1))]
  binary <- names(data)[vapply(data, function(x) length(unique(stats::na.omit(x))) == 2, logical(1))]
  list(numeric = numeric, categorical = categorical, binary = binary, all = names(data))
}

suggest_methods <- function(data) {
  sig <- data_signature(data)
  out <- character(0)
  if (length(sig$numeric)) out <- c(out, "descriptives")
  if (length(sig$categorical)) out <- c(out, "frequencies")
  if (length(sig$numeric) >= 2) out <- c(out, "correlation", "linear_regression")
  if (length(sig$numeric) && length(sig$binary)) out <- c(out, "independent_t", "logistic_regression")
  if (length(sig$numeric) && length(sig$categorical)) out <- c(out, "one_way_anova")
  unique(out)[seq_len(min(5, length(unique(out))))]
}
