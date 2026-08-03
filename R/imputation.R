# Missing-data preparation -------------------------------------------------

# Filling missing values ---------------------------------------------------
#
# Everything here writes to a working copy and leaves the loaded data alone, so
# a reader can always get back to what they started with. The interface says
# plainly that a filled value is an estimate, and the analysis carries a note
# saying the reported uncertainty does not include the uncertainty from
# filling. That note is the reason this is offered at all rather than left as a
# silent convenience.
most_common_value <- function(x) {
  observed <- x[!is.na(x)]
  if (!length(observed)) return(NA)
  counts <- sort(table(observed), decreasing = TRUE)
  names(counts)[1]
}

simple_impute_column <- function(x) {
  missing <- is.na(x)
  if (!any(missing)) return(x)
  if (all(missing)) stop("A column with no observed values cannot be imputed.")
  if (is.numeric(x)) {
    x[missing] <- stats::median(x, na.rm = TRUE)
  } else if (inherits(x, "Date")) {
    x[missing] <- as.Date(stats::median(as.numeric(x), na.rm = TRUE), origin = "1970-01-01")
  } else if (is.logical(x)) {
    replacement <- most_common_value(x)
    x[missing] <- identical(replacement, "TRUE")
  } else {
    replacement <- most_common_value(x)
    if (is.factor(x) && !replacement %in% levels(x)) levels(x) <- c(levels(x), replacement)
    x[missing] <- replacement
  }
  x
}

prepare_imputation_frame <- function(data) {
  out <- data
  for (name in names(out)) {
    if (is.character(out[[name]]) || is.logical(out[[name]])) out[[name]] <- factor(out[[name]])
  }
  out
}

# MICE returns the first completed dataset rather than a pooled analysis. A
# proper pooled result needs the model fitted once per imputation and the
# estimates combined, which is outside what this screen does, so the mids
# object is offered as a download and the limitation is stated rather than
# papered over.
impute_selected_data <- function(data, columns, method = "simple", seed = 2026L,
                                 imputations = 5L, iterations = 5L, neighbors = 5L) {
  columns <- unique(columns[columns %in% names(data)])
  if (!length(columns)) stop("Choose at least one column for missing-data preparation.")
  missing_columns <- columns[vapply(data[columns], function(x) anyNA(x), logical(1))]
  if (!length(missing_columns)) stop("The selected columns do not contain missing values.")

  out <- data
  model <- NULL
  seed <- as.integer(seed)
  if (length(seed) != 1L || is.na(seed) || seed < 0L) stop("The imputation seed must be a nonnegative whole number.")
  if (identical(method, "simple")) {
    out[missing_columns] <- lapply(out[missing_columns], simple_impute_column)
    label <- "Median and most common category"
  } else if (identical(method, "mice")) {
    if (!requireNamespace("mice", quietly = TRUE)) stop("MICE needs the mice package. Run install.packages(\"mice\") and restart Daybreak.")
    imputations <- as.integer(imputations); iterations <- as.integer(iterations)
    if (length(imputations) != 1L || is.na(imputations) || imputations < 2L) stop("MICE needs at least two completed datasets.")
    if (length(iterations) != 1L || is.na(iterations) || iterations < 1L) stop("MICE needs at least one iteration.")
    working <- prepare_imputation_frame(out[, columns, drop = FALSE])
    model <- with_local_seed(seed, mice::mice(working, m = imputations, maxit = iterations,
                                              seed = seed, printFlag = FALSE))
    out[, columns] <- mice::complete(model, 1L)
    label <- "Multiple imputation by chained equations"
  } else if (identical(method, "missforest")) {
    if (!requireNamespace("missForest", quietly = TRUE)) stop("Random-forest imputation needs the missForest package. Run install.packages(\"missForest\") and restart Daybreak.")
    working <- prepare_imputation_frame(out[, columns, drop = FALSE])
    model <- with_local_seed(seed, missForest::missForest(working, verbose = FALSE))
    out[, columns] <- model$ximp
    label <- "Random-forest imputation"
  } else if (identical(method, "knn")) {
    if (!requireNamespace("VIM", quietly = TRUE)) stop("Nearest-neighbor imputation needs the VIM package. Run install.packages(\"VIM\") and restart Daybreak.")
    neighbors <- as.integer(neighbors)
    if (length(neighbors) != 1L || is.na(neighbors) || neighbors < 1L) stop("Nearest-neighbor imputation needs at least one neighbor.")
    working <- prepare_imputation_frame(out[, columns, drop = FALSE])
    completed <- with_local_seed(seed, VIM::kNN(working, variable = missing_columns, k = neighbors, imp_var = FALSE))
    out[, columns] <- completed[, columns, drop = FALSE]
    label <- "Nearest-neighbor imputation"
  } else {
    stop("The selected missing-data method is not available.")
  }

  list(
    data = out,
    model = model,
    label = label,
    columns = missing_columns,
    filled = sum(is.na(data[missing_columns])) - sum(is.na(out[missing_columns]))
  )
}
