# Prediction engines -------------------------------------------------------
#
# These five methods answer a different question from the rest of the app.
# Everywhere else the question is what the data show; here it is how well a
# rule built on part of the data does on the part it has not seen. That
# difference drives three decisions that apply to all of them.
#
# Every score is a holdout score. Training accuracy is recorded in the fit
# table but never leads the reading, because a model that has memorized its
# training rows scores perfectly on them and tells the reader nothing.
#
# Every random step runs inside with_local_seed, and the exported script resets
# the seed at each of the same points. Reproducing the split but not the fold
# assignment gives numbers close enough to look right and different enough to
# be wrong, which is worse than not exporting a script at all.
#
# Nothing here selects a model for the reader. A method that quietly tried
# several settings and reported the best would be reporting a number that does
# not generalize, and the reader would have no way to see that had happened.

validate_split_options <- function(proportion = 0.80, seed = 2026L) {
  proportion <- as.numeric(proportion)
  seed <- as.integer(seed)
  if (length(proportion) != 1L || !is.finite(proportion) || proportion < 0.50 || proportion > 0.95) {
    stop("The training share must be between 0.50 and 0.95.")
  }
  if (length(seed) != 1L || is.na(seed) || seed < 0L) stop("The random seed must be a nonnegative whole number.")
  list(proportion = proportion, seed = seed)
}

ml_holdout_code <- function(outcome, predictors, mode, proportion, seed) {
  columns <- unique(c(outcome, predictors))
  quoted_columns <- paste(sprintf("\"%s\"", columns), collapse = ", ")
  quoted_predictors <- paste(sprintf("\"%s\"", predictors), collapse = ", ")
  common <- c(
    sprintf("columns <- c(%s)", quoted_columns),
    "model_data <- df[complete.cases(df[columns]), columns, drop = FALSE]",
    sprintf("predictors <- c(%s)", quoted_predictors),
    "for (name in predictors) {",
    "  if (is.character(model_data[[name]]) || is.logical(model_data[[name]])) model_data[[name]] <- factor(model_data[[name]])",
    "  if (inherits(model_data[[name]], c('Date', 'POSIXt'))) model_data[[name]] <- as.numeric(model_data[[name]])",
    "}",
    sprintf("set.seed(%d)", seed)
  )
  split <- if (identical(mode, "classification")) c(
    sprintf("model_data[[\"%s\"]] <- droplevels(factor(model_data[[\"%s\"]]))", outcome, outcome),
    sprintf("groups <- split(seq_len(nrow(model_data)), model_data[[\"%s\"]], drop = TRUE)", outcome),
    sprintf("training_rows <- sort(unlist(lapply(groups, function(rows) sample(rows, max(1L, min(length(rows) - 1L, floor(length(rows) * %s))))), use.names = FALSE))", format(proportion))
  ) else c(
    sprintf("training_count <- max(10L, min(nrow(model_data) - 5L, floor(nrow(model_data) * %s)))", format(proportion)),
    "training_rows <- sort(sample(seq_len(nrow(model_data)), training_count))"
  )
  c(common, split,
    "training_data <- model_data[training_rows, , drop = FALSE]",
    "holdout_data <- model_data[-training_rows, , drop = FALSE]")
}

ml_matrix_code <- function(predictors) {
  quoted_predictors <- paste(sprintf("\"%s\"", predictors), collapse = ", ")
  c(
    sprintf("predictors <- c(%s)", quoted_predictors),
    "model_matrix <- model.matrix(~ . - 1, data = model_data[predictors])",
    "spread <- apply(model_matrix[training_rows, , drop = FALSE], 2, sd)",
    "model_matrix <- model_matrix[, is.finite(spread) & spread > 0, drop = FALSE]"
  )
}

# The split is stratified for a classification outcome so that a rare category
# is present in both halves. An unstratified split on an uncommon outcome can
# leave the holdout with no positive cases at all, which makes every reported
# score meaningless rather than merely imprecise.
stratified_holdout <- function(outcome, proportion = 0.80, seed = 2026L) {
  settings <- validate_split_options(proportion, seed)
  groups <- split(seq_along(outcome), outcome, drop = TRUE)
  if (any(lengths(groups) < 5L)) {
    stop("Each outcome category needs at least five complete rows for a training and holdout split.")
  }
  train <- with_local_seed(settings$seed, unlist(lapply(groups, function(rows) {
    count <- max(1L, min(length(rows) - 1L, floor(length(rows) * settings$proportion)))
    sample(rows, count)
  }), use.names = FALSE))
  list(train = sort(train), test = setdiff(seq_along(outcome), train))
}

numeric_holdout <- function(n, proportion = 0.80, seed = 2026L) {
  settings <- validate_split_options(proportion, seed)
  need_n(n, 20, "complete rows for a training and holdout split")
  count <- max(10L, min(n - 5L, floor(n * settings$proportion)))
  train <- sort(with_local_seed(settings$seed, sample(seq_len(n), count)))
  list(train = train, test = setdiff(seq_len(n), train))
}

# Balanced accuracy sits beside plain accuracy because plain accuracy is
# misleading whenever the outcome is uneven. A model that predicts the majority
# category for every row scores well on one and poorly on the other, and the
# difference between the two numbers is the warning.
classification_scores <- function(observed, predicted) {
  observed <- droplevels(factor(observed))
  predicted <- factor(predicted, levels = levels(observed))
  if (length(observed) != length(predicted) || !length(observed) || anyNA(predicted)) {
    stop("The model did not return one valid category prediction for every holdout row.")
  }
  tab <- table(observed, predicted)
  # Recall per category, then averaged. This is what makes balanced accuracy
  # insensitive to how common each category is.
  recalls <- diag(tab) / rowSums(tab)
  data.frame(
    Metric = c("Accuracy", "Balanced accuracy", "Holdout rows"),
    Value = c(mean(observed == predicted), mean(recalls, na.rm = TRUE), length(observed)),
    stringsAsFactors = FALSE
  )
}

confusion_table <- function(observed, predicted) {
  tab <- as.data.frame.matrix(table(Observed = observed, Predicted = predicted))
  data.frame(Observed = rownames(tab), tab, row.names = NULL, check.names = FALSE)
}

regression_scores <- function(observed, predicted) {
  if (length(observed) != length(predicted) || !length(observed) || any(!is.finite(observed)) || any(!is.finite(predicted))) {
    stop("The model did not return one finite numeric prediction for every holdout row.")
  }
  error <- observed - predicted
  denominator <- sum((observed - mean(observed))^2)
  r_squared <- if (denominator > 0) 1 - sum(error^2) / denominator else NA_real_
  data.frame(
    Metric = c("RMSE", "MAE", "R-squared", "Holdout rows"),
    Value = c(sqrt(mean(error^2)), mean(abs(error)), r_squared, length(observed)),
    stringsAsFactors = FALSE
  )
}

# outcome is the full set of complete-case outcome values, not the holdout
# slice. Reporting balance from the holdout alone described a sample of roughly
# twenty percent of the rows under a heading the reader would read as a
# statement about the data.
prediction_checks <- function(train_n, test_n, outcome, classification = FALSE) {
  checks <- rbind(
    check_row("Held-out evaluation", "Used",
              sprintf("The model learned from %s rows and was checked on %s separate rows.", train_n, test_n), "ok"),
    check_row("Interpretation", "Prediction only",
              "Predictive performance does not establish that a predictor causes the outcome.", "note")
  )
  if (classification) {
    counts <- table(outcome)
    ratio <- min(counts) / max(counts)
    checks <- rbind(checks, check_row(
      "Outcome balance", if (ratio < 0.20) "Large imbalance" else "No large imbalance found",
      sprintf("Across the complete rows, the smallest outcome category has %s and the largest has %s.", min(counts), max(counts)),
      if (ratio < 0.20) "warn" else "ok"
    ))
  }
  checks
}

importance_table <- function(values) {
  if (is.null(values) || !length(values)) return(NULL)
  out <- data.frame(Variable = names(values), Importance = as.numeric(values), stringsAsFactors = FALSE)
  out[order(out$Importance, decreasing = TRUE), , drop = FALSE]
}

# Shared preparation for the prediction methods ---------------------------
#
# These five methods differ in how they fit and agree on everything else: the
# same predictor handling, the same holdout split, the same seed discipline,
# and the same exported script. Keeping the shared parts in one place is what
# makes the generated code reproduce the numbers on screen rather than
# something close to them.
prepare_ml_predictors <- function(data, predictors) {
  for (name in predictors) {
    if (is.character(data[[name]]) || is.logical(data[[name]])) data[[name]] <- factor(data[[name]])
    if (inherits(data[[name]], c("Date", "POSIXt"))) data[[name]] <- as.numeric(data[[name]])
  }
  data
}

check_holdout_factor_levels <- function(training, holdout, predictors) {
  unseen <- unlist(lapply(predictors, function(name) {
    if (!is.factor(training[[name]])) return(character(0))
    values <- setdiff(unique(as.character(holdout[[name]])), unique(as.character(training[[name]])))
    if (length(values)) paste0(name, ": ", paste(values, collapse = ", ")) else character(0)
  }), use.names = FALSE)
  if (length(unseen)) {
    stop(sprintf("The holdout rows contain predictor categories absent from the training rows: %s. Add rows or try another random seed.", paste(unseen, collapse = "; ")))
  }
  invisible(TRUE)
}

tree_prediction <- function(data, roles, options, mode = c("classification", "regression")) {
  mode <- match.arg(mode)
  columns <- unique(c(roles$outcome, roles$predictors))
  f <- complete_frame(data, columns)
  need_n(f$used, 20)
  f$data <- prepare_ml_predictors(f$data, roles$predictors)
  formula <- formula_from(roles$outcome, roles$predictors)
  seed <- as.integer(options$random_seed %||% 2026L)
  train_fraction <- as.numeric(options$train_fraction %||% 0.80)
  validate_split_options(train_fraction, seed)
  cp <- as.numeric(options$tree_cp %||% 0.01)
  minsplit <- as.integer(options$tree_minsplit %||% 20L)
  if (length(cp) != 1L || !is.finite(cp) || cp < 0 || cp > 1) stop("The tree complexity value must be between 0 and 1.")
  if (length(minsplit) != 1L || is.na(minsplit) || minsplit < 2L) stop("A tree split must require at least two training rows.")

  if (mode == "classification") {
    f$data[[roles$outcome]] <- droplevels(factor(f$data[[roles$outcome]]))
    need_levels(f$data[[roles$outcome]], 2, 12, "Categorical outcome")
    split <- stratified_holdout(f$data[[roles$outcome]], train_fraction, seed)
  } else {
    need_numeric(f$data, roles$outcome)
    need_variation(f$data, roles$outcome, "Regression tree")
    split <- numeric_holdout(f$used, train_fraction, seed)
  }
  train <- f$data[split$train, , drop = FALSE]
  test <- f$data[split$test, , drop = FALSE]
  check_holdout_factor_levels(train, test, roles$predictors)
  # rpart runs its own cross-validation while growing the tree, and that draws
  # from the random stream. Leaving the call unwrapped meant two things: the
  # cross-validation results depended on whatever state the session happened to
  # be in, and running an analysis quietly moved the reader's random stream.
  # Every other engine that touches randomness goes through with_local_seed, and
  # now this one does too.
  fit <- with_local_seed(seed, rpart::rpart(
    formula, data = train, method = if (mode == "classification") "class" else "anova",
    control = rpart::rpart.control(
      cp = cp,
      minsplit = minsplit
    )
  ))
  predicted <- stats::predict(fit, newdata = test, type = if (mode == "classification") "class" else "vector")
  observed <- test[[roles$outcome]]
  list(frame = f, fit = fit, observed = observed, predicted = predicted,
       train_n = nrow(train), test_n = nrow(test), importance = importance_table(fit$variable.importance),
       seed = seed, train_fraction = train_fraction, cp = cp, minsplit = minsplit)
}

analyse_classification_tree <- function(data, roles, options) {
  x <- tree_prediction(data, roles, options, "classification")
  result_record(
    "classification_tree", x$frame$used, x$frame$omitted,
    estimates = confusion_table(x$observed, x$predicted),
    fit = classification_scores(x$observed, x$predicted),
    checks = prediction_checks(x$train_n, x$test_n, x$frame$data[[roles$outcome]], TRUE), model = x$fit,
    plot_kind = "classification", plot_data = data.frame(Observed = x$observed, Predicted = x$predicted),
    extras = list(outcome = roles$outcome, predictors = roles$predictors, importance = x$importance),
    code = c("library(rpart)", ml_holdout_code(roles$outcome, roles$predictors, "classification", x$train_fraction, x$seed),
             sprintf("set.seed(%d)", x$seed),
             sprintf("fit <- rpart(%s, data = training_data, method = \"class\", control = rpart.control(cp = %s, minsplit = %d))", formula_string(formula_from(roles$outcome, roles$predictors)), format(x$cp), x$minsplit),
             "predict(fit, newdata = holdout_data, type = \"class\")"),
    warnings = "Performance comes from one held-out portion of the supplied data, not from repeated resampling."
  )
}

analyse_regression_tree <- function(data, roles, options) {
  x <- tree_prediction(data, roles, options, "regression")
  result_record(
    "regression_tree", x$frame$used, x$frame$omitted, estimates = x$importance,
    fit = regression_scores(x$observed, x$predicted),
    checks = prediction_checks(x$train_n, x$test_n, x$observed), model = x$fit,
    plot_kind = "ml_regression", plot_data = data.frame(Observed = x$observed, Predicted = x$predicted),
    extras = list(outcome = roles$outcome, predictors = roles$predictors),
    code = c("library(rpart)", ml_holdout_code(roles$outcome, roles$predictors, "regression", x$train_fraction, x$seed),
             sprintf("set.seed(%d)", x$seed),
             sprintf("fit <- rpart(%s, data = training_data, method = \"anova\", control = rpart.control(cp = %s, minsplit = %d))", formula_string(formula_from(roles$outcome, roles$predictors)), format(x$cp), x$minsplit),
             "predict(fit, newdata = holdout_data)"),
    warnings = "Performance comes from one held-out portion of the supplied data, not from repeated resampling."
  )
}

forest_prediction <- function(data, roles, options, mode = c("classification", "regression")) {
  mode <- match.arg(mode)
  columns <- unique(c(roles$outcome, roles$predictors))
  f <- complete_frame(data, columns)
  need_n(f$used, 30)
  f$data <- prepare_ml_predictors(f$data, roles$predictors)
  seed <- as.integer(options$random_seed %||% 2026L)
  train_fraction <- as.numeric(options$train_fraction %||% 0.80)
  validate_split_options(train_fraction, seed)
  trees <- as.integer(options$forest_trees %||% 500L)
  if (length(trees) != 1L || is.na(trees) || trees < 50L || trees > 10000L) stop("Choose between 50 and 10,000 trees.")
  if (mode == "classification") {
    f$data[[roles$outcome]] <- droplevels(factor(f$data[[roles$outcome]]))
    need_levels(f$data[[roles$outcome]], 2, 12, "Categorical outcome")
    split <- stratified_holdout(f$data[[roles$outcome]], train_fraction, seed)
  } else {
    need_numeric(f$data, roles$outcome)
    need_variation(f$data, roles$outcome, "Random-forest regression")
    split <- numeric_holdout(f$used, train_fraction, seed)
  }
  train <- f$data[split$train, , drop = FALSE]
  test <- f$data[split$test, , drop = FALSE]
  check_holdout_factor_levels(train, test, roles$predictors)
  formula <- formula_from(roles$outcome, roles$predictors)
  fit <- ranger::ranger(
    formula, data = train, num.trees = trees,
    importance = "permutation", seed = seed, num.threads = 1, verbose = FALSE
  )
  predicted <- predict(fit, data = test)$predictions
  list(frame = f, fit = fit, observed = test[[roles$outcome]], predicted = predicted,
       train_n = nrow(train), test_n = nrow(test), importance = importance_table(fit$variable.importance),
       seed = seed, train_fraction = train_fraction, trees = trees)
}

analyse_random_forest_classification <- function(data, roles, options) {
  x <- forest_prediction(data, roles, options, "classification")
  result_record(
    "random_forest_classification", x$frame$used, x$frame$omitted,
    estimates = x$importance, fit = classification_scores(x$observed, x$predicted),
    checks = prediction_checks(x$train_n, x$test_n, x$frame$data[[roles$outcome]], TRUE), model = x$fit,
    plot_kind = "classification", plot_data = data.frame(Observed = x$observed, Predicted = x$predicted),
    extras = list(outcome = roles$outcome, predictors = roles$predictors),
    code = c("library(ranger)", ml_holdout_code(roles$outcome, roles$predictors, "classification", x$train_fraction, x$seed),
             sprintf("fit <- ranger(%s, data = training_data, num.trees = %d, importance = \"permutation\", seed = %d, num.threads = 1)", formula_string(formula_from(roles$outcome, roles$predictors)), x$trees, x$seed), "predict(fit, data = holdout_data)$predictions"),
    warnings = "Performance comes from one held-out portion of the supplied data, not from repeated resampling."
  )
}

analyse_random_forest_regression <- function(data, roles, options) {
  x <- forest_prediction(data, roles, options, "regression")
  result_record(
    "random_forest_regression", x$frame$used, x$frame$omitted,
    estimates = x$importance, fit = regression_scores(x$observed, x$predicted),
    checks = prediction_checks(x$train_n, x$test_n, x$observed), model = x$fit,
    plot_kind = "ml_regression", plot_data = data.frame(Observed = x$observed, Predicted = x$predicted),
    extras = list(outcome = roles$outcome, predictors = roles$predictors),
    code = c("library(ranger)", ml_holdout_code(roles$outcome, roles$predictors, "regression", x$train_fraction, x$seed),
             sprintf("fit <- ranger(%s, data = training_data, num.trees = %d, importance = \"permutation\", seed = %d, num.threads = 1)", formula_string(formula_from(roles$outcome, roles$predictors)), x$trees, x$seed), "predict(fit, data = holdout_data)$predictions"),
    warnings = "Performance comes from one held-out portion of the supplied data, not from repeated resampling."
  )
}

# Nearest neighbors is the one method here where scaling is not a refinement
# but the whole result: distance is computed directly from the columns, so a
# predictor with a wide range would otherwise decide every neighbor by itself.
# The center and spread come from the training rows alone, because using the
# full data would leak holdout information into the model.
analyse_nearest_neighbors <- function(data, roles, options) {
  columns <- unique(c(roles$outcome, roles$predictors))
  f <- complete_frame(data, columns)
  need_n(f$used, 20, "complete rows for training and holdout checks")
  f$data <- prepare_ml_predictors(f$data, roles$predictors)
  f$data[[roles$outcome]] <- droplevels(factor(f$data[[roles$outcome]]))
  need_levels(f$data[[roles$outcome]], 2, 12, "Categorical outcome")
  split <- stratified_holdout(f$data[[roles$outcome]], as.numeric(options$train_fraction %||% 0.80),
                              as.integer(options$random_seed %||% 2026L))
  matrix <- stats::model.matrix(~ . - 1, data = f$data[, roles$predictors, drop = FALSE])
  spread <- apply(matrix[split$train, , drop = FALSE], 2, stats::sd)
  keep <- is.finite(spread) & spread > 0
  if (!any(keep)) stop("The selected predictors do not vary in the training rows.")
  matrix <- matrix[, keep, drop = FALSE]
  # Center and spread come from the training rows only. Computing them across
  # all rows would let the holdout shape the model that is about to be
  # scored against it, and the score would come out flattering.
  center <- colMeans(matrix[split$train, , drop = FALSE])
  spread <- apply(matrix[split$train, , drop = FALSE], 2, stats::sd)
  training <- sweep(sweep(matrix[split$train, , drop = FALSE], 2, center), 2, spread, "/")
  holdout <- sweep(sweep(matrix[split$test, , drop = FALSE], 2, center), 2, spread, "/")
  observed <- f$data[[roles$outcome]][split$test]
  neighbors <- as.integer(options$neighbors %||% 5L)
  if (length(neighbors) != 1L || is.na(neighbors) || neighbors < 1L) stop("Choose at least one neighbor.")
  if (neighbors > nrow(training)) stop(sprintf("Choose no more than %s neighbors for the current training rows.", nrow(training)))
  seed <- as.integer(options$random_seed %||% 2026L)
  predicted <- with_local_seed(seed, class::knn(training, holdout, cl = f$data[[roles$outcome]][split$train], k = neighbors))
  result_record(
    "nearest_neighbors", f$used, f$omitted,
    estimates = confusion_table(observed, predicted),
    fit = classification_scores(observed, predicted),
    checks = prediction_checks(length(split$train), length(split$test), f$data[[roles$outcome]], TRUE),
    plot_kind = "classification", plot_data = data.frame(Observed = observed, Predicted = predicted),
    extras = list(outcome = roles$outcome, predictors = roles$predictors, neighbors = neighbors),
    code = c("library(class)", ml_holdout_code(roles$outcome, roles$predictors, "classification", as.numeric(options$train_fraction %||% 0.80), seed),
             ml_matrix_code(roles$predictors),
             "center <- colMeans(model_matrix[training_rows, , drop = FALSE])",
             "spread <- apply(model_matrix[training_rows, , drop = FALSE], 2, sd)",
             "training_matrix <- sweep(sweep(model_matrix[training_rows, , drop = FALSE], 2, center), 2, spread, '/')",
             "holdout_matrix <- sweep(sweep(model_matrix[-training_rows, , drop = FALSE], 2, center), 2, spread, '/')",
             sprintf("training_outcome <- model_data[[\"%s\"]][training_rows]", roles$outcome),
             sprintf("set.seed(%d)", seed),
             sprintf("predicted <- knn(training_matrix, holdout_matrix, training_outcome, k = %d)", neighbors)),
    warnings = "Performance comes from one held-out portion of the supplied data, not from repeated resampling."
  )
}

# Regularized models ------------------------------------------------------

ml_matrix <- function(data, predictors) {
  prepared <- prepare_ml_predictors(data[, predictors, drop = FALSE], predictors)
  stats::model.matrix(~ . - 1, data = prepared)
}

training_fold_ids <- function(outcome, folds, seed, classification = FALSE) {
  if (length(folds) != 1L || is.na(folds) || folds < 3L) stop("Cross-validation needs at least three folds.")
  with_local_seed(seed, {
    ids <- integer(length(outcome))
    if (classification) {
      groups <- split(seq_along(outcome), outcome, drop = TRUE)
      for (rows in groups) ids[rows] <- sample(rep(seq_len(folds), length.out = length(rows)))
    } else {
      ids <- sample(rep(seq_len(folds), length.out = length(outcome)))
    }
    ids
  })
}

elastic_coefficients <- function(fit, lambda = "lambda.1se") {
  values <- stats::coef(fit, s = lambda)
  if (is.list(values)) {
    out <- do.call(rbind, lapply(names(values), function(category) {
      matrix <- as.matrix(values[[category]])
      data.frame(
        Category = category,
        Term = rownames(matrix),
        Estimate = as.numeric(matrix[, 1]),
        stringsAsFactors = FALSE
      )
    }))
  } else {
    matrix <- as.matrix(values)
    out <- data.frame(Term = rownames(matrix), Estimate = as.numeric(matrix[, 1]), stringsAsFactors = FALSE)
  }
  rownames(out) <- NULL
  nonzero <- out$Term != "(Intercept)" & abs(out$Estimate) > sqrt(.Machine$double.eps)
  shown <- out[nonzero, , drop = FALSE]
  if (!nrow(shown)) shown <- out[out$Term == "(Intercept)", , drop = FALSE]
  shown[order(abs(shown$Estimate), decreasing = TRUE), , drop = FALSE]
}

elastic_net_prediction <- function(data, roles, options, mode = c("classification", "regression")) {
  mode <- match.arg(mode)
  columns <- unique(c(roles$outcome, roles$predictors))
  f <- complete_frame(data, columns)
  need_n(f$used, 30, "complete rows for regularized training and holdout checks")
  matrix <- ml_matrix(f$data, roles$predictors)
  if (!ncol(matrix)) stop("The selected predictors did not produce usable model columns.")
  seed <- as.integer(options$random_seed %||% 2026L)
  train_fraction <- as.numeric(options$train_fraction %||% 0.80)
  alpha <- as.numeric(options$elastic_alpha %||% 0.50)
  validate_split_options(train_fraction, seed)
  if (length(alpha) != 1L || !is.finite(alpha) || alpha < 0 || alpha > 1) stop("The elastic-net penalty mix must be between 0 and 1.")

  if (mode == "classification") {
    outcome <- droplevels(factor(f$data[[roles$outcome]]))
    need_levels(outcome, 2, 12, "Categorical outcome")
    split <- stratified_holdout(outcome, train_fraction, seed)
    family <- if (nlevels(outcome) == 2L) "binomial" else "multinomial"
    max_folds <- min(table(outcome[split$train]))
  } else {
    need_numeric(f$data, roles$outcome)
    need_variation(f$data, roles$outcome, "Elastic-net regression")
    outcome <- f$data[[roles$outcome]]
    split <- numeric_holdout(f$used, train_fraction, seed)
    family <- "gaussian"
    max_folds <- length(split$train)
  }

  spread <- apply(matrix[split$train, , drop = FALSE], 2, stats::sd)
  keep <- is.finite(spread) & spread > 0
  if (!any(keep)) stop("The selected predictors do not vary in the training rows.")
  matrix <- matrix[, keep, drop = FALSE]
  requested_folds <- as.integer(options$cv_folds %||% 5L)
  if (length(requested_folds) != 1L || is.na(requested_folds) || requested_folds < 3L) stop("Cross-validation needs at least three folds.")
  folds <- min(requested_folds, as.integer(max_folds), length(split$train))
  if (folds < 3L) stop("The training rows do not provide at least three examples per outcome category for cross-validation.")
  fold_id <- training_fold_ids(outcome[split$train], folds, seed, mode == "classification")
  fit <- glmnet::cv.glmnet(
    x = matrix[split$train, , drop = FALSE], y = outcome[split$train],
    family = family, alpha = alpha, foldid = fold_id, standardize = TRUE
  )
  prediction_type <- if (mode == "classification") "class" else "response"
  predicted <- stats::predict(
    fit, newx = matrix[split$test, , drop = FALSE], s = "lambda.1se", type = prediction_type
  )
  predicted <- if (mode == "classification") factor(as.vector(predicted), levels = levels(outcome)) else as.numeric(predicted)

  list(
    frame = f, fit = fit, observed = outcome[split$test], predicted = predicted,
    train_n = length(split$train), test_n = length(split$test),
    coefficients = elastic_coefficients(fit), lambda = unname(fit$lambda.1se),
    alpha = alpha, folds = folds, family = family, seed = seed, train_fraction = train_fraction
  )
}

analyse_elastic_net_classification <- function(data, roles, options) {
  x <- elastic_net_prediction(data, roles, options, "classification")
  metrics <- rbind(
    classification_scores(x$observed, x$predicted),
    data.frame(Metric = c("Selected lambda", "Penalty mix"), Value = c(x$lambda, x$alpha))
  )
  result_record(
    "elastic_net_classification", x$frame$used, x$frame$omitted,
    estimates = x$coefficients, fit = metrics,
    checks = prediction_checks(x$train_n, x$test_n, x$frame$data[[roles$outcome]], TRUE), model = x$fit,
    plot_kind = "classification", plot_data = data.frame(Observed = x$observed, Predicted = x$predicted),
    extras = list(outcome = roles$outcome, predictors = roles$predictors, alpha = x$alpha, lambda = x$lambda),
    code = c(
      "library(glmnet)",
      ml_holdout_code(roles$outcome, roles$predictors, "classification", x$train_fraction, x$seed),
      ml_matrix_code(roles$predictors),
      sprintf("training_outcome <- model_data[[\"%s\"]][training_rows]", roles$outcome),
      sprintf("set.seed(%d)", x$seed),
      sprintf("fold_id <- integer(length(training_outcome)); groups <- split(seq_along(training_outcome), training_outcome, drop = TRUE); for (rows in groups) fold_id[rows] <- sample(rep(seq_len(%d), length.out = length(rows)))", x$folds),
      sprintf("fit <- cv.glmnet(model_matrix[training_rows, , drop = FALSE], training_outcome, family = \"%s\", alpha = %.2f, foldid = fold_id)", x$family, x$alpha),
      "predict(fit, newx = model_matrix[-training_rows, , drop = FALSE], s = \"lambda.1se\", type = \"class\")"
    ),
    warnings = "The penalty was selected by cross-validation inside the training rows. The displayed score still comes from one held-out portion of the supplied data."
  )
}

analyse_elastic_net_regression <- function(data, roles, options) {
  x <- elastic_net_prediction(data, roles, options, "regression")
  metrics <- rbind(
    regression_scores(x$observed, x$predicted),
    data.frame(Metric = c("Selected lambda", "Penalty mix"), Value = c(x$lambda, x$alpha))
  )
  result_record(
    "elastic_net_regression", x$frame$used, x$frame$omitted,
    estimates = x$coefficients, fit = metrics,
    checks = prediction_checks(x$train_n, x$test_n, x$observed), model = x$fit,
    plot_kind = "ml_regression", plot_data = data.frame(Observed = x$observed, Predicted = x$predicted),
    extras = list(outcome = roles$outcome, predictors = roles$predictors, alpha = x$alpha, lambda = x$lambda),
    code = c(
      "library(glmnet)",
      ml_holdout_code(roles$outcome, roles$predictors, "regression", x$train_fraction, x$seed),
      ml_matrix_code(roles$predictors),
      sprintf("training_outcome <- model_data[[\"%s\"]][training_rows]", roles$outcome),
      sprintf("set.seed(%d)", x$seed),
      sprintf("fold_id <- sample(rep(seq_len(%d), length.out = length(training_outcome)))", x$folds),
      sprintf("fit <- cv.glmnet(model_matrix[training_rows, , drop = FALSE], training_outcome, family = \"gaussian\", alpha = %.2f, foldid = fold_id)", x$alpha),
      "predict(fit, newx = model_matrix[-training_rows, , drop = FALSE], s = \"lambda.1se\")"
    ),
    warnings = "The penalty was selected by cross-validation inside the training rows. The displayed error still comes from one held-out portion of the supplied data."
  )
}

# Probability classification ---------------------------------------------

analyse_naive_bayes <- function(data, roles, options) {
  columns <- unique(c(roles$outcome, roles$predictors))
  f <- complete_frame(data, columns)
  need_n(f$used, 20, "complete rows for training and holdout checks")
  f$data <- prepare_ml_predictors(f$data, roles$predictors)
  f$data[[roles$outcome]] <- droplevels(factor(f$data[[roles$outcome]]))
  need_levels(f$data[[roles$outcome]], 2, 12, "Categorical outcome")
  train_fraction <- as.numeric(options$train_fraction %||% 0.80)
  seed <- as.integer(options$random_seed %||% 2026L)
  validate_split_options(train_fraction, seed)
  split <- stratified_holdout(
    f$data[[roles$outcome]], train_fraction, seed
  )
  training <- f$data[split$train, , drop = FALSE]
  holdout <- f$data[split$test, , drop = FALSE]
  check_holdout_factor_levels(training, holdout, roles$predictors)
  varying <- vapply(training[, roles$predictors, drop = FALSE], function(x) length(unique(x)) > 1L, logical(1))
  if (!any(varying)) stop("The selected predictors do not vary in the training rows.")
  predictors <- roles$predictors[varying]
  formula <- formula_from(roles$outcome, predictors)
  laplace <- as.numeric(options$naive_laplace %||% 1)
  if (length(laplace) != 1L || !is.finite(laplace) || laplace < 0) stop("The Laplace smoothing value must be zero or greater.")
  fit <- e1071::naiveBayes(formula, data = training, laplace = laplace)
  predicted <- stats::predict(fit, newdata = holdout, type = "class")
  observed <- holdout[[roles$outcome]]
  priors <- fit$apriori / sum(fit$apriori)
  estimates <- data.frame(Category = names(priors), Training_share = as.numeric(priors), stringsAsFactors = FALSE)

  result_record(
    "naive_bayes", f$used, f$omitted, estimates = estimates,
    tests = confusion_table(observed, predicted), fit = classification_scores(observed, predicted),
    checks = prediction_checks(nrow(training), nrow(holdout), f$data[[roles$outcome]], TRUE), model = fit,
    plot_kind = "classification", plot_data = data.frame(Observed = observed, Predicted = predicted),
    extras = list(outcome = roles$outcome, predictors = predictors, laplace = laplace),
    code = c(
      "library(e1071)",
      ml_holdout_code(roles$outcome, roles$predictors, "classification", train_fraction, seed),
      sprintf("fit <- naiveBayes(%s, data = training_data, laplace = %s)", formula_string(formula), format(laplace)),
      "predict(fit, newdata = holdout_data, type = \"class\")"
    ),
    warnings = "Naive Bayes treats predictors as conditionally independent within each outcome category. Performance comes from one held-out portion of the supplied data."
  )
}

# Density-based grouping --------------------------------------------------

density_cluster_frame <- function(data, variables) {
  f <- complete_frame(data, variables)
  need_numeric(f$data, variables)
  need_n(f$used, 5)
  spread <- vapply(f$data, stats::sd, numeric(1))
  keep <- is.finite(spread) & spread > 0
  if (sum(keep) < 2L) stop("Density-based clustering needs at least two numeric columns that vary.")
  f$data <- f$data[, keep, drop = FALSE]
  list(frame = f, matrix = scale(f$data), variables = names(f$data))
}

density_cluster_tables <- function(cluster, membership = NULL) {
  labels <- ifelse(cluster == 0L, "Noise", paste("Cluster", cluster))
  levels <- c(if (any(cluster == 0L)) "Noise", paste("Cluster", sort(unique(cluster[cluster > 0L]))))
  labels <- factor(labels, levels = levels)
  sizes <- as.data.frame(table(labels), stringsAsFactors = FALSE)
  names(sizes) <- c("Group", "Rows")
  if (!is.null(membership)) {
    means <- tapply(membership, labels, mean, na.rm = TRUE)
    sizes$Mean_membership <- as.numeric(means[as.character(sizes$Group)])
  }
  list(labels = labels, sizes = sizes)
}

density_checks <- function(cluster, method_label) {
  cluster_count <- length(unique(cluster[cluster > 0L]))
  noise_share <- mean(cluster == 0L)
  rbind(
    check_row(
      "Groups found", as.character(cluster_count),
      sprintf("%s found %s non-noise groups in the current standardized columns.", method_label, cluster_count),
      if (cluster_count >= 2L) "ok" else "warn"
    ),
    check_row(
      "Rows marked as noise", fmt_pct(noise_share),
      "Noise rows did not belong to a sufficiently dense group under the current settings.",
      if (noise_share > 0.40) "warn" else "note"
    ),
    check_row(
      "Scale", "Standardized",
      "Each selected column was centered and scaled before distances were calculated.", "note"
    )
  )
}

analyse_dbscan_clustering <- function(data, roles, options) {
  x <- density_cluster_frame(data, roles$variables)
  eps <- as.numeric(options$dbscan_eps %||% 0.7)
  min_points <- as.integer(options$density_min_points %||% 5L)
  if (!is.finite(eps) || eps <= 0) stop("The DBSCAN neighborhood radius must be greater than zero.")
  if (length(min_points) != 1L || is.na(min_points) || min_points < 2L || min_points > x$frame$used) stop("Minimum nearby points must be at least two and no larger than the number of complete rows.")
  fit <- dbscan::dbscan(x$matrix, eps = eps, minPts = min_points)
  tables <- density_cluster_tables(fit$cluster)

  result_record(
    "dbscan_clustering", x$frame$used, x$frame$omitted, estimates = tables$sizes,
    checks = density_checks(fit$cluster, "DBSCAN"), model = fit,
    plot_kind = "clusters", plot_data = cluster_projection(x$frame$data, tables$labels),
    extras = list(variables = x$variables, eps = eps, min_points = min_points, membership = fit$cluster),
    code = c(
      "library(dbscan)",
      sprintf("variables <- c(%s)", paste(sprintf("\"%s\"", x$variables), collapse = ", ")),
      "model_data <- df[complete.cases(df[variables]), variables, drop = FALSE]",
      sprintf("fit <- dbscan(scale(model_data), eps = %s, minPts = %d)", format(eps), min_points),
      "table(fit$cluster)"
    ),
    warnings = "DBSCAN groups depend on the distance radius and minimum-points setting. Compare nearby settings and judge whether the groups make sense for the subject matter."
  )
}

analyse_hdbscan_clustering <- function(data, roles, options) {
  x <- density_cluster_frame(data, roles$variables)
  min_points <- as.integer(options$density_min_points %||% 5L)
  if (length(min_points) != 1L || is.na(min_points) || min_points < 2L || min_points > x$frame$used) stop("Minimum cluster size must be at least two and no larger than the number of complete rows.")
  fit <- dbscan::hdbscan(x$matrix, minPts = min_points)
  membership <- fit$membership_prob %||% rep(NA_real_, length(fit$cluster))
  tables <- density_cluster_tables(fit$cluster, membership)

  result_record(
    "hdbscan_clustering", x$frame$used, x$frame$omitted, estimates = tables$sizes,
    checks = density_checks(fit$cluster, "HDBSCAN"), model = fit,
    plot_kind = "clusters", plot_data = cluster_projection(x$frame$data, tables$labels),
    extras = list(variables = x$variables, min_points = min_points, membership = membership, cluster = fit$cluster),
    code = c(
      "library(dbscan)",
      sprintf("variables <- c(%s)", paste(sprintf("\"%s\"", x$variables), collapse = ", ")),
      "model_data <- df[complete.cases(df[variables]), variables, drop = FALSE]",
      sprintf("fit <- hdbscan(scale(model_data), minPts = %d)", min_points),
      "table(fit$cluster)"
    ),
    warnings = "HDBSCAN can mark weakly attached rows as noise. Compare nearby minimum-cluster sizes and judge whether the groups make sense for the subject matter."
  )
}
