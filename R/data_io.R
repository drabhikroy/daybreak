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
    # 97, 98, and 99 arrive as NA. Reading them as values turned "Refused" and
    # "Not applicable" into ordinary categories, which quietly moved means and
    # inflated category counts.
    sav = haven::read_sav(path),
    zsav = haven::read_sav(path),
    por = haven::read_por(path),
    dta = haven::read_dta(path),
    sas7bdat = haven::read_sas(path),
    xpt = haven::read_xpt(path),
    rds = readRDS(path),
    rdata = read_rdata_frame(path),
    rda = read_rdata_frame(path),
    json = jsonlite::fromJSON(path, flatten = TRUE),
    parquet = arrow::read_parquet(path),
    feather = arrow::read_feather(path)
  )
  clean_imported_data(x)
}

read_rdata_frame <- function(path) {
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
      # survival::veteran codes status as 0 for censored and 1 for dead. An
      # earlier release mapped levels 1 and 2, which relabelled all 128 deaths
      # as censored and turned the 9 genuinely censored rows into NA. The
      # column then held one observed level, so it never appeared in the event
      # status selector and the example could not be run at all.
      d <- survival::veteran
      d$status <- factor(d$status, levels = c(0, 1), labels = c("Censored", "Event"))
      d
    },
    nottem = data.frame(
      date = seq(as.Date("1920-01-01"), by = "month", length.out = length(datasets::nottem)),
      temperature_f = as.numeric(datasets::nottem)
    ),
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
# usually a rating and is sometimes a count, and the reader knows which.
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
