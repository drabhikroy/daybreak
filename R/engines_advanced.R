# Advanced regression, latent-variable, clustered, and time-to-event models -

analyse_negative_binomial <- function(data, roles, options) {
  columns <- c(roles$outcome, roles$predictors, roles$exposure_time %||% "")
  columns <- columns[nzchar(columns)]
  f <- complete_frame(data, columns)
  numeric_columns <- c(roles$outcome, roles$exposure_time %||% "")
  need_numeric(f$data, numeric_columns[nzchar(numeric_columns)])
  need_variation(f$data, roles$outcome, "Negative-binomial regression")
  need_n(f$used, 10)
  if (any(f$data[[roles$outcome]] < 0 | f$data[[roles$outcome]] %% 1 != 0)) stop("The count outcome must contain nonnegative whole numbers.")
  offset <- roles$exposure_time %||% ""
  if (nzchar(offset) && any(f$data[[offset]] <= 0)) stop("Exposure values must be greater than zero.")
  form <- formula_from(roles$outcome, roles$predictors, offset = if (nzchar(offset)) offset else NULL)
  need_model_matrix(form, f$data, 2L)
  model <- MASS::glm.nb(form, data = f$data)
  if (!isTRUE(model$converged) || any(!is.finite(stats::coef(model)))) stop("The negative-binomial model did not settle on finite estimates. Reduce the predictors or inspect sparse categories.")
  coefs <- tidy_coefs(model, exponentiate = TRUE, level = confidence_level(options))
  fit <- data.frame(Metric = c("AIC", "Theta"), Value = c(stats::AIC(model), model$theta))
  result_record("negative_binomial", f$used, f$omitted, coefs, fit = fit,
                checks = check_row("Count variation", "Negative binomial variance fitted",
                                   sprintf("The fitted theta parameter was %s.", fmt_num(model$theta, 2)), "note"),
                model = model, plot_kind = "count_model",
                plot_data = data.frame(Observed = f$data[[roles$outcome]], Fitted = stats::fitted(model)),
                extras = list(outcome = roles$outcome, predictors = roles$predictors, exposure = offset, formula = formula_string(form)),
                code = c("library(MASS)", sprintf("fit <- glm.nb(%s, data = df)", formula_string(form)),
                         sprintf("exp(cbind(rate_ratio = coef(fit), confint.default(fit, level = %s)))", confidence_level(options))))
}

# Models for outcomes that are not a single number ------------------------
#
# Each of these returns exponentiated coefficients, because a reader who is
# handed a log odds or a log hazard has to transform it before it means
# anything, and the transformed number is the one they were after.
analyse_multinomial_regression <- function(data, roles, options) {
  columns <- c(roles$outcome, roles$predictors)
  f <- complete_frame(data, columns); need_levels(f$data[[roles$outcome]], 3); need_n(f$used, 20)
  f$data[[roles$outcome]] <- as_group(f$data[[roles$outcome]])
  form <- formula_from(roles$outcome, roles$predictors)
  need_model_matrix(form, f$data, 2L)
  model <- nnet::multinom(form, data = f$data, trace = FALSE, Hess = TRUE)
  sm <- summary(model)
  beta <- as.matrix(sm$coefficients); se <- as.matrix(sm$standard.errors)
  if (any(!is.finite(beta)) || any(!is.finite(se)) || any(se <= 0)) {
    stop("The multinomial model produced unstable estimates. Add rows to sparse outcome categories or remove overlapping predictors.")
  }
  critical <- stats::qnorm(1 - (1 - confidence_level(options)) / 2)
  rows <- do.call(rbind, lapply(seq_len(nrow(beta)), function(i) {
    data.frame(Category = rownames(beta)[i], Term = colnames(beta), Estimate = exp(beta[i, ]),
               `Std. error` = se[i, ], Statistic = beta[i, ] / se[i, ],
               p = 2 * stats::pnorm(abs(beta[i, ] / se[i, ]), lower.tail = FALSE),
               Lower = exp(beta[i, ] - critical * se[i, ]), Upper = exp(beta[i, ] + critical * se[i, ]),
               check.names = FALSE, row.names = NULL)
  }))
  pred <- stats::predict(model, type = "class")
  accuracy <- mean(pred == f$data[[roles$outcome]])
  fit <- data.frame(Metric = c("AIC", "Training accuracy"), Value = c(stats::AIC(model), accuracy))
  result_record("multinomial_regression", f$used, f$omitted, rows, fit = fit,
                checks = check_row("Category sizes", if (min(table(f$data[[roles$outcome]])) >= 10) "At least 10 per category" else "A category has fewer than 10 rows",
                                   paste(names(table(f$data[[roles$outcome]])), as.integer(table(f$data[[roles$outcome]])), collapse = "; "),
                                   if (min(table(f$data[[roles$outcome]])) >= 10) "ok" else "warn"),
                model = model, plot_kind = "classification",
                plot_data = data.frame(Observed = f$data[[roles$outcome]], Predicted = pred),
                extras = list(outcome = roles$outcome, predictors = roles$predictors, reference = levels(f$data[[roles$outcome]])[1], formula = formula_string(form)),
                code = c("library(nnet)", sprintf("fit <- multinom(%s, data = df, trace = FALSE)", formula_string(form)), "summary(fit)"))
}

# Proportional odds is assumed and stated rather than tested. A formal test of
# the assumption on a small sample tends to reject for reasons unrelated to the
# question, so the check table names the assumption and leaves the judgment
# with the reader.
analyse_ordinal_regression <- function(data, roles, options) {
  columns <- c(roles$outcome, roles$predictors)
  f <- complete_frame(data, columns); need_levels(f$data[[roles$outcome]], 3); need_n(f$used, 20)
  outcome <- f$data[[roles$outcome]]
  observed_levels <- if (is.factor(outcome)) levels(droplevels(outcome)) else unique(as.character(outcome))
  requested_order <- as.character(options$ordinal_order %||% observed_levels)
  if (length(requested_order) != length(observed_levels) || anyDuplicated(requested_order) || !setequal(requested_order, observed_levels)) {
    stop("The outcome order must include every observed category exactly once.")
  }
  outcome <- ordered(outcome, levels = requested_order)
  f$data[[roles$outcome]] <- outcome
  form <- formula_from(roles$outcome, roles$predictors)
  need_model_matrix(form, f$data, 2L)
  model <- MASS::polr(form, data = f$data, Hess = TRUE, method = "logistic")
  tab <- coef(summary(model))
  beta_rows <- seq_along(stats::coef(model))
  if (any(!is.finite(tab[beta_rows, , drop = FALSE])) || any(tab[beta_rows, "Std. Error"] <= 0)) {
    stop("The ordinal model produced unstable estimates. Check the outcome order, sparse categories, and overlapping predictors.")
  }
  critical <- stats::qnorm(1 - (1 - confidence_level(options)) / 2)
  coefs <- data.frame(Term = rownames(tab)[beta_rows], Estimate = exp(tab[beta_rows, "Value"]),
                      `Std. error` = tab[beta_rows, "Std. Error"], Statistic = tab[beta_rows, "t value"],
                      p = 2 * stats::pnorm(abs(tab[beta_rows, "t value"]), lower.tail = FALSE),
                      Lower = exp(tab[beta_rows, "Value"] - critical * tab[beta_rows, "Std. Error"]),
                      Upper = exp(tab[beta_rows, "Value"] + critical * tab[beta_rows, "Std. Error"]),
                      check.names = FALSE, row.names = NULL)
  fit <- data.frame(Metric = c("AIC", "Residual deviance"), Value = c(stats::AIC(model), stats::deviance(model)))
  result_record("ordinal_regression", f$used, f$omitted, coefs, fit = fit,
                checks = rbind(
                  check_row("Outcome order", paste(levels(outcome), collapse = " < "), "The result depends on this low-to-high order. Confirm that it matches the meaning of the outcome.", "note"),
                  check_row("Proportional odds", "Assumed by this model", "The same predictor slope is used across each cumulative outcome split.", "warn")
                ),
                model = model, plot_kind = "classification",
                plot_data = data.frame(Observed = f$data[[roles$outcome]], Predicted = stats::predict(model, type = "class")),
                extras = list(outcome = roles$outcome, predictors = roles$predictors, order = levels(outcome), formula = formula_string(form)),
                code = c("library(MASS)", sprintf("df[[\"%s\"]] <- ordered(df[[\"%s\"]], levels = c(%s))", roles$outcome, roles$outcome,
                                                     paste(sprintf("\"%s\"", requested_order), collapse = ", ")),
                         sprintf("fit <- polr(%s, data = df, Hess = TRUE)", formula_string(form)), "summary(fit)"))
}

analyse_manova <- function(data, roles, options) {
  columns <- c(roles$outcomes, roles$predictors)
  if (length(roles$outcomes) < 2L) stop("MANOVA needs at least two numeric outcomes.")
  f <- complete_frame(data, columns); need_numeric(f$data, roles$outcomes)
  need_variation(f$data, roles$outcomes, "MANOVA")
  need_n(f$used, length(roles$outcomes) + 4L)
  no_variation <- roles$outcomes[vapply(f$data[roles$outcomes], function(x) stats::sd(x) <= sqrt(.Machine$double.eps), logical(1))]
  if (length(no_variation)) stop(sprintf("Remove outcomes with no variation: %s.", paste(no_variation, collapse = ", ")))
  lhs <- paste0("cbind(", paste(vapply(roles$outcomes, bt, character(1)), collapse = ", "), ")")
  rhs <- paste(vapply(roles$predictors, bt, character(1)), collapse = " + ")
  form <- stats::as.formula(paste(lhs, "~", rhs))
  need_model_matrix(form, f$data, length(roles$outcomes) + 1L)
  model <- stats::manova(form, data = f$data)
  mult <- as.data.frame(summary(model, test = "Pillai")$stats)
  mult$Term <- rownames(mult); rownames(mult) <- NULL
  names(mult) <- c("Df", "Pillai", "Approx_F", "Num_df", "Den_df", "p", "Term")
  univ <- summary.aov(model)
  univ_rows <- do.call(rbind, lapply(seq_along(univ), function(i) {
    tab <- as.data.frame(univ[[i]])
    tab$Term <- rownames(tab); tab$Outcome <- roles$outcomes[i]; rownames(tab) <- NULL
    tab
  }))
  checks <- check_row("Covariance structure", "Not fully tested", "Pillai's trace is less sensitive than some alternatives, but multivariate outliers and covariance differences still matter.", "warn")

  # Pillai's trace is the omnibus test, so it belongs in the tests table with
  # every other omnibus test rather than under estimates. Placing it under
  # estimates put the one number a reader was looking for beneath a heading
  # that told them it was something else.
  test_rows <- mult[mult$Term != "Residuals", , drop = FALSE]
  tests <- data.frame(
    Test = paste("Pillai's trace,", test_rows$Term),
    Statistic = test_rows$Approx_F, df1 = test_rows$Num_df, df2 = test_rows$Den_df,
    p = test_rows$p, stringsAsFactors = FALSE
  )
  result_record("manova", f$used, f$omitted, estimates = mult, tests = tests, checks = checks, model = model,
                plot_kind = "multivariate", plot_data = f$data,
                extras = list(outcomes = roles$outcomes, predictors = roles$predictors, univariate = univ_rows, formula = formula_string(form)),
                code = c(sprintf("fit <- manova(%s, data = df)", formula_string(form)), "summary(fit, test = 'Pillai')", "summary.aov(fit)"))
}

analyse_pca <- function(data, roles, options) {
  variables <- roles$variables
  if (length(variables) < 2L) stop("Principal components analysis needs at least two numeric columns.")
  f <- complete_frame(data, variables); need_numeric(f$data, variables); need_n(f$used, 3)
  zero_var <- variables[vapply(f$data, function(x) stats::sd(x) == 0, logical(1))]
  if (length(zero_var)) stop(sprintf("Remove columns with no variation: %s.", paste(zero_var, collapse = ", ")))
  model <- stats::prcomp(f$data, center = TRUE, scale. = TRUE)
  variance <- model$sdev^2 / sum(model$sdev^2)
  components <- seq_along(variance)
  variance_table <- data.frame(Component = paste0("PC", components), Explained = variance,
                               Cumulative = cumsum(variance))
  loadings <- as.data.frame(model$rotation)
  loadings$Variable <- rownames(loadings); rownames(loadings) <- NULL
  selected <- as.integer(options$components %||% min(5, length(variables)))
  if (length(selected) != 1L || is.na(selected) || selected < 1L || selected > length(variables)) {
    stop(sprintf("Choose between 1 and %d principal components.", length(variables)))
  }
  result_record("pca", f$used, f$omitted, estimates = variance_table,
                checks = check_row("Input scaling", "Variables standardized", "Each selected variable was centered and scaled before extraction.", "ok"),
                model = model, plot_kind = "pca", plot_data = data.frame(model$x[, seq_len(min(2, ncol(model$x))), drop = FALSE]),
                extras = list(variables = variables, loadings = loadings, selected = selected),
                code = c(sprintf("variables <- c(%s)", paste(sprintf("\"%s\"", variables), collapse = ", ")),
                         "model_data <- df[complete.cases(df[variables]), variables, drop = FALSE]",
                         "fit <- prcomp(model_data, center = TRUE, scale. = TRUE)", "summary(fit)", "fit$rotation"))
}

cluster_projection <- function(frame, cluster) {
  pca <- stats::prcomp(frame, center = TRUE, scale. = TRUE)
  scores <- pca$x[, seq_len(min(2, ncol(pca$x))), drop = FALSE]
  if (ncol(scores) == 1) scores <- cbind(scores, 0)
  data.frame(Component_1 = scores[, 1], Component_2 = scores[, 2], Cluster = factor(cluster))
}

# Grouping rows -----------------------------------------------------------
#
# Every clustering method here standardizes first. Without it the column with
# the widest numeric range decides the clusters, so a variable recorded in
# dollars quietly outranks one recorded in years.
analyse_kmeans_clustering <- function(data, roles, options) {
  variables <- roles$variables
  if (length(variables) < 2L) stop("K-means clustering needs at least two numeric columns.")
  f <- complete_frame(data, variables); need_numeric(f$data, variables)
  centers <- as.integer(options$clusters %||% 3)
  if (length(centers) != 1L || is.na(centers)) stop("The cluster count must be a whole number.")
  need_n(f$used, centers + 1)
  if (centers < 2 || centers >= f$used) stop("Choose at least 2 clusters and fewer clusters than complete rows.")
  scaled <- scale(f$data)
  if (any(!is.finite(scaled))) stop("Remove numeric columns with no variation before clustering.")
  if (nrow(unique(as.data.frame(scaled))) < centers) stop("The complete rows contain fewer distinct numeric profiles than the requested clusters.")
  seed <- as.integer(options$random_seed %||% 2026)
  model <- with_local_seed(seed, stats::kmeans(scaled, centers = centers, nstart = 30, iter.max = 100))
  sizes <- data.frame(Cluster = factor(seq_len(centers)), Rows = as.integer(model$size),
                      Within_SS = model$withinss)
  center_table <- as.data.frame(model$centers); center_table$Cluster <- factor(seq_len(centers))
  checks <- check_row("Cluster separation", sprintf("Between-cluster share: %s", fmt_pct(model$betweenss / model$totss)),
                      "This share describes separation in the current standardized variables; it is not a validation score.", "note")
  result_record("kmeans_clustering", f$used, f$omitted, estimates = sizes, checks = checks, model = model,
                plot_kind = "clusters", plot_data = cluster_projection(f$data, model$cluster),
                extras = list(variables = variables, clusters = centers, centers = center_table, seed = seed),
                code = c(sprintf("variables <- c(%s)", paste(sprintf("\"%s\"", variables), collapse = ", ")),
                         "model_data <- df[complete.cases(df[variables]), variables, drop = FALSE]",
                         sprintf("set.seed(%d)", seed),
                         sprintf("fit <- kmeans(scale(model_data), centers = %d, nstart = 30)", centers), "fit"))
}

analyse_hierarchical_clustering <- function(data, roles, options) {
  variables <- roles$variables
  if (length(variables) < 2L) stop("Hierarchical clustering needs at least two numeric columns.")
  f <- complete_frame(data, variables); need_numeric(f$data, variables)
  clusters <- as.integer(options$clusters %||% 3)
  if (length(clusters) != 1L || is.na(clusters) || clusters < 2L || clusters >= f$used) {
    stop("Choose at least 2 clusters and fewer clusters than complete rows.")
  }
  need_n(f$used, clusters + 1)
  scaled <- scale(f$data)
  if (any(!is.finite(scaled))) stop("Remove numeric columns with no variation before clustering.")
  model <- stats::hclust(stats::dist(scaled), method = "ward.D2")
  membership <- stats::cutree(model, k = clusters)
  sizes <- data.frame(Cluster = factor(seq_len(clusters)), Rows = as.integer(table(factor(membership, levels = seq_len(clusters)))))
  checks <- check_row("Cluster count", "Chosen before cutting the tree",
                      "Inspect nearby cluster counts and subject-matter meaning because the selected cut is not uniquely determined by the hierarchy.", "warn")
  result_record("hierarchical_clustering", f$used, f$omitted, estimates = sizes, checks = checks, model = model,
                plot_kind = "clusters", plot_data = cluster_projection(f$data, membership),
                extras = list(variables = variables, clusters = clusters, membership = membership),
                code = c(sprintf("variables <- c(%s)", paste(sprintf("\"%s\"", variables), collapse = ", ")),
                         "model_data <- df[complete.cases(df[variables]), variables, drop = FALSE]",
                         "fit <- hclust(dist(scale(model_data)), method = 'ward.D2')",
                         sprintf("cluster <- cutree(fit, k = %d)", clusters), "table(cluster)"))
}

analyse_factor_analysis <- function(data, roles, options) {
  variables <- roles$variables
  if (length(variables) < 3L) stop("Exploratory factor analysis needs at least three numeric columns.")
  f <- complete_frame(data, variables); need_numeric(f$data, variables); need_n(f$used, max(20, length(variables) + 2))
  no_variation <- variables[vapply(f$data, function(x) stats::sd(x) <= sqrt(.Machine$double.eps), logical(1))]
  if (length(no_variation)) stop(sprintf("Remove columns with no variation: %s.", paste(no_variation, collapse = ", ")))
  factors <- as.integer(options$factors %||% min(2, length(variables) - 1))
  if (length(factors) != 1L || is.na(factors) || factors < 1 || factors >= length(variables)) stop("The factor count must be at least 1 and smaller than the number of variables.")
  rotation <- options$rotation %||% "oblimin"
  if (!rotation %in% c("none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT", "bifactor", "promax", "oblimin", "simplimax", "bentlerQ", "geominQ", "cluster")) stop("Choose a rotation supported by the factor-analysis engine.")
  model <- psych::fa(f$data, nfactors = factors, rotate = rotation, fm = options$factor_method %||% "minres", scores = "regression")
  loadings <- as.data.frame(unclass(model$loadings))
  loadings$Variable <- rownames(loadings); rownames(loadings) <- NULL
  communalities <- data.frame(Variable = names(model$communality), Communality = unname(model$communality),
                              Uniqueness = unname(model$uniquenesses))
  fit <- data.frame(Metric = c("RMSR", "TLI", "RMSEA"),
                    Value = c(model$rms, model$TLI, model$RMSEA[1]))
  checks <- check_row("Factor solution", if (isTRUE(model$Heywood)) "Heywood case detected" else "No Heywood case flagged",
                      "A Heywood case includes a negative uniqueness or a loading outside an admissible range.", if (isTRUE(model$Heywood)) "warn" else "ok")
  result_record("factor_analysis", f$used, f$omitted, estimates = loadings, fit = fit, checks = checks,
                model = model, plot_kind = "loadings", plot_data = loadings,
                extras = list(variables = variables, factors = factors, rotation = rotation, communalities = communalities),
                code = c("library(psych)", sprintf("variables <- c(%s)", paste(sprintf("\"%s\"", variables), collapse = ", ")),
                         "model_data <- df[complete.cases(df[variables]), variables, drop = FALSE]",
                         sprintf("fit <- fa(model_data, nfactors = %d, rotate = '%s', fm = '%s')", factors, rotation, options$factor_method %||% "minres"), "print(fit$loadings)"))
}

analyse_reliability <- function(data, roles, options) {
  variables <- roles$variables
  if (length(variables) < 2L) stop("Reliability analysis needs at least two item columns.")
  f <- complete_frame(data, variables); need_numeric(f$data, variables); need_n(f$used, 3)
  no_variation <- variables[vapply(f$data, function(x) stats::sd(x) <= sqrt(.Machine$double.eps), logical(1))]
  if (length(no_variation)) stop(sprintf("Remove items with no variation: %s.", paste(no_variation, collapse = ", ")))
  if (!is.finite(stats::var(rowSums(f$data))) || stats::var(rowSums(f$data)) <= sqrt(.Machine$double.eps)) {
    stop("The total item score has no variation, so coefficient alpha cannot be calculated.")
  }
  overall <- alpha_manual(f$data)
  total <- rowSums(f$data)
  item_rows <- lapply(variables, function(v) {
    rest <- total - f$data[[v]]
    remaining <- setdiff(variables, v)
    data.frame(Item = v, Item_rest_correlation = stats::cor(f$data[[v]], rest),
               Alpha_if_removed = if (length(remaining) >= 2) alpha_manual(f$data[remaining]) else NA_real_)
  })
  estimates <- do.call(rbind, item_rows)
  fit <- data.frame(Metric = c("Coefficient alpha", "Items", "Complete cases"), Value = c(overall, length(variables), f$used))
  checks <- check_row("Scale meaning", "Not established by alpha", "Internal consistency does not show that the scale is one-dimensional or valid for its intended use.", "warn")
  result_record("reliability", f$used, f$omitted, estimates = estimates, fit = fit, checks = checks,
                plot_kind = "reliability", plot_data = estimates,
                extras = list(variables = variables, alpha = overall),
                code = c(sprintf("variables <- c(%s)", paste(sprintf("\"%s\"", variables), collapse = ", ")),
                         "items <- df[complete.cases(df[variables]), variables, drop = FALSE]",
                         "k <- ncol(items)", "alpha <- k/(k-1) * (1 - sum(sapply(items, var))/var(rowSums(items)))", "alpha"))
}

lavaan_result <- function(kind, data, syntax, options, fit_call = c("cfa", "sem")) {
  fit_call <- match.arg(fit_call)
  if (length(syntax) != 1L || is.na(syntax) || !nzchar(trimws(syntax))) {
    stop("Enter a nonblank lavaan model before running this analysis.")
  }
  estimator <- options$estimator %||% "MLR"
  missing <- options$missing_method %||% "fiml"
  ordered <- options$ordered %||% character(0)
  notes <- character(0)
  if (length(ordered) && estimator %in% c("ML", "MLR")) {
    stop("Ordered indicators need WLSMV or DWLS in this guided form. Change the estimator in Analysis settings.")
  }
  if (estimator %in% c("WLSMV", "DWLS") && identical(missing, "fiml")) {
    missing <- "listwise"
    notes <- "Full-information maximum likelihood is not available with WLSMV or DWLS, so this run used listwise deletion."
  }
  args <- list(model = syntax, data = data, estimator = estimator, missing = missing,
               std.lv = TRUE, fixed.x = FALSE)
  if (length(ordered)) args$ordered <- ordered
  model <- do.call(if (fit_call == "cfa") lavaan::cfa else lavaan::sem, args)
  if (!lavaan::lavInspect(model, "converged")) stop("The model did not converge. Review the syntax, variable distributions, and model identification.")
  pe <- lavaan::parameterEstimates(model, standardized = TRUE, ci = TRUE)
  keep <- pe$op %in% c("=~", "~", "~~", ":=")
  estimates <- pe[keep, c("lhs", "op", "rhs", "est", "se", "z", "pvalue", "ci.lower", "ci.upper", "std.all")]
  names(estimates) <- c("Left", "Path", "Right", "Estimate", "Std_error", "z", "p", "Lower", "Upper", "Standardized")
  fit_names <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic")
  measures <- lavaan::fitMeasures(model, fit_names)
  fit <- data.frame(Metric = names(measures), Value = unname(measures))
  fit_ok <- all(is.finite(measures[c("cfi", "tli", "rmsea", "srmr")])) &&
    measures[["cfi"]] >= 0.95 && measures[["tli"]] >= 0.95 &&
    measures[["rmsea"]] <= 0.06 && measures[["srmr"]] <= 0.08
  checks <- rbind(
    check_row("Convergence", "Converged", sprintf("The optimizer ended after %d iterations.", lavaan::lavInspect(model, "optim")$iterations), "ok"),
    check_row("Model fit", if (fit_ok) "Common fit guides were met" else "Some fit guides were not met or unavailable",
              sprintf("CFI = %s, TLI = %s, RMSEA = %s, SRMR = %s. Cutoffs are guides, not pass-fail rules.",
                      fmt_num(measures[["cfi"]], 3), fmt_num(measures[["tli"]], 3), fmt_num(measures[["rmsea"]], 3), fmt_num(measures[["srmr"]], 3)),
              if (fit_ok) "ok" else "warn")
  )
  n_used <- sum(as.integer(lavaan::lavInspect(model, "nobs")))
  syntax_code <- paste(deparse(syntax, width.cutoff = 500L), collapse = "")
  ordered_code <- if (length(ordered)) sprintf(", ordered = c(%s)", paste(sprintf("\"%s\"", ordered), collapse = ", ")) else ""
  result_record(kind, n_used, max(0, nrow(data) - n_used), estimates, fit = fit, checks = checks,
                model = model, plot_kind = "lavaan", plot_data = estimates,
                extras = list(syntax = syntax, estimator = estimator, modification_indices = head(lavaan::modindices(model, sort. = TRUE), 20)),
                code = c("library(lavaan)", sprintf("model_syntax <- %s", syntax_code),
                         sprintf("fit <- %s(model_syntax, data = df, estimator = '%s', missing = '%s', std.lv = TRUE, fixed.x = FALSE%s)", fit_call, estimator, missing, ordered_code),
                         "summary(fit, standardized = TRUE, fit.measures = TRUE)"),
                warnings = notes)
}

analyse_cfa <- function(data, roles, options) lavaan_result("cfa", data, roles$model_syntax, options, "cfa")
analyse_sem <- function(data, roles, options) lavaan_result("sem", data, roles$model_syntax, options, "sem")

# Latent variable models --------------------------------------------------
#
# The indirect effect gets a bootstrap interval rather than the Sobel test. The
# product of two coefficients is not normally distributed, and the normal-based
# test is known to be conservative in exactly the small samples where the
# question usually comes up.
analyse_mediation <- function(data, roles, options) {
  controls <- roles$controls %||% character(0)
  columns <- c(roles$outcome, roles$exposure, roles$mediator, controls)
  unsafe <- columns[make.names(columns) != columns]
  if (length(unsafe)) stop(sprintf("Mediation syntax needs R-safe column names. Rename these columns first: %s.", paste(unsafe, collapse = ", ")))
  f <- complete_frame(data, columns); need_numeric(f$data, columns); need_n(f$used, max(20L, length(columns) + 5L))
  covariates <- if (length(controls)) paste("+", paste(controls, collapse = " + ")) else ""
  syntax <- paste(
    sprintf("%s ~ a*%s %s", roles$mediator, roles$exposure, covariates),
    sprintf("%s ~ b*%s + direct*%s %s", roles$outcome, roles$mediator, roles$exposure, covariates),
    "indirect := a*b", "total := direct + indirect", sep = "\n"
  )
  bootstrap <- as.integer(options$bootstrap %||% 1000)
  if (length(bootstrap) != 1L || is.na(bootstrap) || bootstrap < 200L) stop("Use at least 200 bootstrap samples for the mediation interval.")
  model <- with_local_seed(2026L, lavaan::sem(syntax, data = f$data, se = "bootstrap", bootstrap = bootstrap, fixed.x = FALSE))
  if (!lavaan::lavInspect(model, "converged")) stop("The mediation model did not converge.")
  pe <- lavaan::parameterEstimates(model, standardized = TRUE, ci = TRUE, boot.ci.type = "perc")
  estimates <- pe[pe$label %in% c("a", "b", "direct") | pe$lhs %in% c("indirect", "total"),
                  c("lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "ci.lower", "ci.upper", "std.all")]
  names(estimates) <- c("Left", "Path", "Right", "Label", "Estimate", "Std_error", "z", "p", "Lower", "Upper", "Standardized")
  checks <- check_row("Causal wording", "Not licensed by the model alone", "Mediation needs defensible time order and control of exposure-mediator, mediator-outcome, and exposure-outcome confounding.", "warn")
  syntax_code <- paste(deparse(syntax, width.cutoff = 500L), collapse = "")
  result_record("mediation", f$used, f$omitted, estimates = estimates, checks = checks, model = model,
                plot_kind = "mediation", plot_data = estimates,
                extras = list(outcome = roles$outcome, exposure = roles$exposure, mediator = roles$mediator,
                              controls = controls, syntax = syntax, bootstrap = bootstrap, seed = 2026L),
                code = c("library(lavaan)", "set.seed(2026)", sprintf("model_syntax <- %s", syntax_code),
                         sprintf("fit <- sem(model_syntax, data = df, se = 'bootstrap', bootstrap = %d, fixed.x = FALSE)", bootstrap),
                         "parameterEstimates(fit, standardized = TRUE, ci = TRUE)"))
}

# Clustered and repeated data ---------------------------------------------
#
# A convergence warning from lme4 stops the analysis rather than passing an
# uncertain fit forward with a note attached. The reader of this app is not
# well placed to judge how far a gradient of the reported size can be trusted,
# and a result they cannot evaluate is worse than a message telling them the
# model needs simplifying.
analyse_hlm_linear <- function(data, roles, options) {
  is_growth <- identical(options$.method_id, "growth_model")
  predictors <- if (is_growth) unique(c(roles$time, roles$controls %||% character(0))) else roles$predictors
  random_slope <- if (is_growth && isTRUE(options$growth_random_slope %||% TRUE)) roles$time else roles$random_slope %||% ""
  if (!is_growth && nzchar(random_slope) && !random_slope %in% roles$predictors) {
    stop("The random-slope column must also be included among the fixed predictors.")
  }
  columns <- c(roles$outcome, predictors, roles$cluster, random_slope)
  columns <- unique(columns[nzchar(columns)])
  f <- complete_frame(data, columns)
  need_numeric(f$data, c(roles$outcome, if (nzchar(random_slope)) random_slope else character(0)))
  need_variation(f$data, roles$outcome, "Hierarchical linear analysis")
  need_levels(f$data[[roles$cluster]], 3, label = "The cluster column")
  need_n(f$used, length(predictors) + 6L)
  need_model_matrix(formula_from(roles$outcome, predictors), f$data, 2L)
  if (nzchar(random_slope)) {
    slope_varies <- tapply(f$data[[random_slope]], f$data[[roles$cluster]], function(x) length(unique(x)) >= 2L)
    if (sum(slope_varies) < 2L) stop("A random slope needs within-cluster variation in at least two clusters.")
  }
  fixed <- paste(vapply(predictors, bt, character(1)), collapse = " + ")
  random <- if (nzchar(random_slope)) sprintf("(1 + %s | %s)", bt(random_slope), bt(roles$cluster)) else sprintf("(1 | %s)", bt(roles$cluster))
  form <- stats::as.formula(paste(bt(roles$outcome), "~", fixed, "+", random))
  model <- lmerTest::lmer(form, data = f$data, REML = isTRUE(options$reml %||% TRUE))
  # lme4 reports a convergence problem as a warning and hands back a fitted
  # object anyway. Passing that object forward would put numbers on screen that
  # look exactly like converged ones.
  convergence_messages <- model@optinfo$conv$lme4$messages
  if (length(convergence_messages)) stop(paste("The hierarchical linear model did not converge:", paste(convergence_messages, collapse = " ")))
  sm <- summary(model)
  tab <- as.data.frame(sm$coefficients)
  tab$Term <- rownames(tab); rownames(tab) <- NULL
  names(tab)[1:5] <- c("Estimate", "Std_error", "df", "Statistic", "p")
  level <- confidence_level(options)
  critical <- ifelse(is.finite(tab$df), stats::qt(1 - (1 - level) / 2, tab$df), stats::qnorm(1 - (1 - level) / 2))
  tab$Lower <- tab$Estimate - critical * tab$Std_error; tab$Upper <- tab$Estimate + critical * tab$Std_error
  estimates <- tab[, c("Term", "Estimate", "Std_error", "df", "Statistic", "p", "Lower", "Upper")]
  vc <- as.data.frame(lme4::VarCorr(model))
  singular <- lme4::isSingular(model)
  cluster_sizes <- table(f$data[[roles$cluster]])
  fit <- data.frame(Metric = c("AIC", "BIC", "Log likelihood", "Clusters"),
                    Value = c(stats::AIC(model), stats::BIC(model), as.numeric(stats::logLik(model)), length(unique(f$data[[roles$cluster]]))))
  checks <- rbind(
    check_row("Singular fit", if (singular) "Detected" else "Not detected", if (singular) "The random-effects structure may be more complex than the data can support." else "The fitted random-effects covariance was not singular.", if (singular) "warn" else "ok"),
    check_row("Rows per cluster", if (min(cluster_sizes) >= 2L) "At least two in every cluster" else "A cluster has one row",
              paste(names(cluster_sizes), as.integer(cluster_sizes), collapse = "; "), if (min(cluster_sizes) >= 2L) "note" else "warn"),
    normality_check(stats::residuals(model), "Conditional residuals")
  )
  kind <- if (is_growth) "growth_model" else "hlm_linear"
  chart_data <- if (is_growth) cbind(f$data, .fitted = stats::fitted(model), .residual = stats::residuals(model)) else
    data.frame(Fitted = stats::fitted(model), Residual = stats::residuals(model))
  result_record(kind, f$used, f$omitted, estimates, fit = fit, checks = checks, model = model,
                plot_kind = if (is_growth) "growth" else "mixed", plot_data = chart_data,
                extras = list(outcome = roles$outcome, predictors = predictors, cluster = roles$cluster,
                              random_slope = random_slope, variance_components = vc, formula = formula_string(form)),
                code = c("library(lme4)", "library(lmerTest)",
                         sprintf("fit <- lmer(%s, data = df, REML = %s)", formula_string(form), isTRUE(options$reml %||% TRUE)), "summary(fit)"))
}

analyse_hlm_logistic <- function(data, roles, options) {
  columns <- c(roles$outcome, roles$predictors, roles$cluster, roles$random_slope %||% "")
  columns <- unique(columns[nzchar(columns)])
  f <- complete_frame(data, columns); binary <- binary01(f$data[[roles$outcome]])
  need_levels(f$data[[roles$cluster]], 3, label = "The cluster column")
  if (min(table(binary$value)) < 2L) stop("The binary outcome needs at least two complete rows for both values.")
  f$data$.daybreak_outcome <- binary$value
  random_slope <- roles$random_slope %||% ""
  if (nzchar(random_slope) && !random_slope %in% roles$predictors) {
    stop("The random-slope column must also be included among the fixed predictors.")
  }
  fixed <- paste(vapply(roles$predictors, bt, character(1)), collapse = " + ")
  need_model_matrix(formula_from(roles$outcome, roles$predictors), f$data, 2L)
  if (nzchar(random_slope)) {
    slope_varies <- tapply(f$data[[random_slope]], f$data[[roles$cluster]], function(x) length(unique(x)) >= 2L)
    if (sum(slope_varies) < 2L) stop("A random slope needs within-cluster variation in at least two clusters.")
  }
  random <- if (nzchar(random_slope)) sprintf("(1 + %s | %s)", bt(random_slope), bt(roles$cluster)) else sprintf("(1 | %s)", bt(roles$cluster))
  form <- stats::as.formula(paste(".daybreak_outcome ~", fixed, "+", random))
  model <- lme4::glmer(form, data = f$data, family = stats::binomial(), control = lme4::glmerControl(optimizer = "bobyqa"))
  convergence_messages <- model@optinfo$conv$lme4$messages
  if (length(convergence_messages)) stop(paste("The hierarchical logistic model did not converge:", paste(convergence_messages, collapse = " ")))
  tab <- as.data.frame(summary(model)$coefficients)
  tab$Term <- rownames(tab); rownames(tab) <- NULL
  names(tab)[1:4] <- c("Log_odds", "Std_error", "Statistic", "p")
  critical <- stats::qnorm(1 - (1 - confidence_level(options)) / 2)
  tab$Estimate <- exp(tab$Log_odds); tab$Lower <- exp(tab$Log_odds - critical * tab$Std_error); tab$Upper <- exp(tab$Log_odds + critical * tab$Std_error)
  estimates <- tab[, c("Term", "Estimate", "Std_error", "Statistic", "p", "Lower", "Upper")]
  singular <- lme4::isSingular(model)
  if (any(!is.finite(tab$Log_odds)) || any(!is.finite(tab$Std_error))) stop("The hierarchical logistic model produced unstable estimates. Check sparse outcomes within clusters or remove overlapping predictors.")
  cluster_sizes <- table(f$data[[roles$cluster]])
  fit <- data.frame(Metric = c("AIC", "BIC", "Log likelihood", "Clusters"),
                    Value = c(stats::AIC(model), stats::BIC(model), as.numeric(stats::logLik(model)), length(unique(f$data[[roles$cluster]]))))
  result_record("hlm_logistic", f$used, f$omitted, estimates, fit = fit,
                checks = rbind(
                  check_row("Singular fit", if (singular) "Detected" else "Not detected", if (singular) "The random-effects structure may be too complex for these data." else "The fitted random-effects covariance was not singular.", if (singular) "warn" else "ok"),
                  check_row("Rows per cluster", if (min(cluster_sizes) >= 2L) "At least two in every cluster" else "A cluster has one row",
                            paste(names(cluster_sizes), as.integer(cluster_sizes), collapse = "; "), if (min(cluster_sizes) >= 2L) "note" else "warn")
                ),
                model = model, plot_kind = "logistic",
                plot_data = data.frame(Observed = binary$value, Predicted = stats::predict(model, type = "response")),
                extras = list(outcome = roles$outcome, predictors = roles$predictors, cluster = roles$cluster,
                              random_slope = random_slope, event = binary$event, variance_components = as.data.frame(lme4::VarCorr(model)), formula = formula_string(form)),
                code = c("library(lme4)", sprintf("df$.daybreak_outcome <- as.integer(factor(df[[\"%s\"]])) - 1", roles$outcome),
                         sprintf("fit <- glmer(%s, data = df, family = binomial())", formula_string(form)), "summary(fit)"))
}

# Time to event -----------------------------------------------------------
#
# The log-log interval is used rather than the default. A plain interval on the
# survival scale can run below zero or above one near the ends of follow-up,
# where a survival curve spends much of its time.
# summary(survfit)$table arrives with the survival package's own column names:
# records, n.max, n.start, events, rmean, se(rmean), median, and a pair of
# confidence bounds. Those are correct and they are not readable. An app that
# promises statistics you can read should not put "se(rmean)" in front of
# someone. The values are unchanged; only the headings are rewritten, and the
# bounds name the level that produced them.
survival_summary_table <- function(model, level, grouped) {
  raw <- summary(model)$table
  frame <- if (is.matrix(raw)) {
    data.frame(Stratum = rownames(raw), raw, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    data.frame(Stratum = "All rows", t(raw), check.names = FALSE, stringsAsFactors = FALSE)
  }
  rownames(frame) <- NULL
  if (grouped) frame$Stratum <- sub("^[^=]+=", "", frame$Stratum)

  bound <- paste0(conf_percent(level), "%")
  wanted <- c(
    Stratum = "Group", records = "Rows", events = "Events",
    median = "Median survival time",
    rmean = "Restricted mean survival time", `se(rmean)` = "Standard error of the restricted mean"
  )
  lower_name <- grep("LCL$", names(frame), value = TRUE)
  upper_name <- grep("UCL$", names(frame), value = TRUE)
  keep <- c(names(wanted)[names(wanted) %in% names(frame)], lower_name, upper_name)
  out <- frame[, keep, drop = FALSE]
  labels <- unname(wanted[keep])
  labels[keep %in% lower_name] <- paste(bound, "lower bound for the median")
  labels[keep %in% upper_name] <- paste(bound, "upper bound for the median")
  names(out) <- labels
  out
}

analyse_kaplan_meier <- function(data, roles, options) {
  group <- roles$group %||% ""
  columns <- c(roles$time, roles$status, group); columns <- columns[nzchar(columns)]
  f <- complete_frame(data, columns); need_numeric(f$data, roles$time)
  if (any(f$data[[roles$time]] < 0)) stop("Time-to-event values cannot be negative.")
  status <- binary01(f$data[[roles$status]]); f$data$.event <- status$value
  if (sum(f$data$.event) < 1L) stop("At least one retained row must contain the event value.")
  if (nzchar(group)) {
    need_levels(f$data[[group]], 2, label = "The survival group column")
    events_by_group <- tapply(f$data$.event, f$data[[group]], sum)
  }
  rhs <- if (nzchar(group)) bt(group) else "1"
  form <- stats::as.formula(paste("survival::Surv(", bt(roles$time), ", .event) ~", rhs))
  model <- survival::survfit(form, data = f$data, conf.type = "log-log", conf.int = confidence_level(options))
  tests <- NULL
  if (nzchar(group)) {
    logrank <- survival::survdiff(form, data = f$data)
    df_test <- length(logrank$n) - 1
    tests <- data.frame(Test = "Log-rank", Statistic = unname(logrank$chisq), df = df_test,
                        p = stats::pchisq(logrank$chisq, df_test, lower.tail = FALSE))
  }
  sm <- summary(model)
  plot_data <- data.frame(Time = sm$time, Survival = sm$surv, Lower = sm$lower, Upper = sm$upper,
                          Stratum = if (is.null(sm$strata)) "All rows" else as.character(sm$strata))
  med_table <- survival_summary_table(model, confidence_level(options), nzchar(group))
  # Which value counts as the event is the single easiest thing to get backwards
  # in a survival analysis, and getting it backwards inverts the curve without
  # producing any error. The engine states its choice in the checks table rather
  # than leaving it in extras where a reader has to go looking.
  checks <- check_row(
    "Event value", status$event,
    sprintf("Rows where %s equals %s were counted as the event. The other value was treated as censored. If that is reversed, recode the column before running this again.",
            roles$status, status$event),
    "note"
  )
  checks <- rbind(checks, check_row("Censoring", "Treated as non-informative", "The model assumes that censoring is unrelated to later event risk after accounting for the selected grouping.", "warn"))
  if (nzchar(group)) {
    checks <- rbind(checks, check_row(
      "Events by group", if (all(events_by_group > 0L)) "At least one in every group" else "A group has no observed events",
      paste(names(events_by_group), as.integer(events_by_group), collapse = "; "),
      if (all(events_by_group > 0L)) "note" else "warn"
    ))
  }
  result_record("kaplan_meier", f$used, f$omitted, estimates = med_table, tests = tests,
                checks = checks,
                model = model, plot_kind = "survival", plot_data = plot_data,
                extras = list(time = roles$time, status = roles$status, group = group, event = status$event),
                code = c("library(survival)", sprintf("df$.event <- as.integer(factor(df[[\"%s\"]])) - 1", roles$status),
                         sprintf("fit <- survfit(%s, data = df)", formula_string(form)), "summary(fit)"))
}

# Proportional hazards is the assumption most often broken in practice and
# least often checked, so the Schoenfeld residual test runs automatically and
# reports into the check table rather than waiting to be asked for.
analyse_cox_regression <- function(data, roles, options) {
  columns <- c(roles$time, roles$status, roles$predictors)
  f <- complete_frame(data, columns); need_numeric(f$data, roles$time)
  if (any(f$data[[roles$time]] < 0)) stop("Time-to-event values cannot be negative.")
  status <- binary01(f$data[[roles$status]]); f$data$.event <- status$value
  rhs <- paste(vapply(roles$predictors, bt, character(1)), collapse = " + ")
  form <- stats::as.formula(paste("survival::Surv(", bt(roles$time), ", .event) ~", rhs))
  rank_info <- need_model_matrix(form, f$data, 1L)
  coefficient_count <- max(1L, rank_info$rank - 1L)
  if (sum(f$data$.event) <= coefficient_count) stop("The Cox model needs more observed events than fitted coefficient columns. Add events or remove predictors.")
  model <- survival::coxph(form, data = f$data, x = TRUE)
  sm <- summary(model)
  critical <- stats::qnorm(1 - (1 - confidence_level(options)) / 2)
  beta <- sm$coefficients[, "coef"]
  se <- sm$coefficients[, "se(coef)"]
  coefs <- data.frame(Term = rownames(sm$coefficients), Estimate = exp(beta),
                      Std_error = sm$coefficients[, "se(coef)"], Statistic = sm$coefficients[, "z"],
                      p = sm$coefficients[, "Pr(>|z|)"], Lower = exp(beta - critical * se), Upper = exp(beta + critical * se), row.names = NULL)
  # Schoenfeld residuals test whether a hazard ratio holds steady across
  # follow-up. A ratio that drifts is still reported, but the check says so,
  # because a single averaged ratio is then describing something that changed.
  zph <- survival::cox.zph(model)
  ztab <- as.data.frame(zph$table); ztab$Term <- rownames(ztab); rownames(ztab) <- NULL
  global_p <- ztab$p[ztab$Term == "GLOBAL"]
  checks <- check_row("Proportional hazards", if (length(global_p) && global_p >= 0.05) "No clear global departure found" else "A departure may be present",
                      sprintf("The global Schoenfeld-residual test had p %s.", fmt_p(if (length(global_p)) global_p else NA_real_)),
                      if (length(global_p) && global_p >= 0.05) "ok" else "warn")
  fit <- data.frame(Metric = c("Concordance", "Likelihood-ratio p", "AIC", "Events"),
                    Value = c(sm$concordance[1], sm$logtest[3], stats::AIC(model), model$nevent))
  result_record("cox_regression", f$used, f$omitted, coefs, fit = fit, checks = checks, model = model,
                plot_kind = "forest", plot_data = coefs,
                extras = list(time = roles$time, status = roles$status, predictors = roles$predictors,
                              event = status$event, proportional_hazards = ztab, formula = formula_string(form),
                              conf_level = confidence_level(options)),
                code = c("library(survival)", sprintf("df$.event <- as.integer(factor(df[[\"%s\"]])) - 1", roles$status),
                         sprintf("fit <- coxph(%s, data = df)", formula_string(form)), "summary(fit)", "cox.zph(fit)"))
}

# Continue a time column past its last observed point. The engine has already
# refused irregular spacing, so the median gap is the spacing, and Date columns
# keep their class rather than falling back to a day count.
future_time_points <- function(values, horizon) {
  if (horizon < 1L) return(values[0])
  if (inherits(values, "Date")) {
    # Monthly data is the common case and a median gap of 30.4 days walks the
    # day of the month backwards over a long projection. When the column looks
    # monthly, continue it with calendar months instead of a day count.
    calendar <- as.POSIXlt(values)
    month_index <- (calendar$year + 1900L) * 12L + calendar$mon
    monthly <- length(unique(calendar$mday)) == 1L &&
      length(unique(diff(month_index))) == 1L && diff(month_index)[1] >= 1L
    if (monthly) {
      step <- diff(month_index)[1]
      return(seq(max(values), by = paste(step, "month"), length.out = horizon + 1L)[-1L])
    }
    step <- stats::median(diff(as.numeric(values)))
    return(as.Date(max(as.numeric(values)) + step * seq_len(horizon), origin = "1970-01-01"))
  }
  if (inherits(values, "POSIXt")) {
    step <- stats::median(diff(as.numeric(values)))
    return(as.POSIXct(max(as.numeric(values)) + step * seq_len(horizon), origin = "1970-01-01", tz = attr(values, "tzone") %||% ""))
  }
  numeric_values <- as.numeric(values)
  step <- if (length(numeric_values) > 1L) stats::median(diff(numeric_values)) else 1
  max(numeric_values) + step * seq_len(horizon)
}

# The engine refuses irregular spacing instead of quietly treating rows as
# equally spaced. An ARIMA fitted to unevenly spaced observations produces
# numbers that look ordinary and mean nothing.
analyse_time_series <- function(data, roles, options) {
  f <- complete_frame(data, c(roles$outcome, roles$time)); need_numeric(f$data, roles$outcome); need_n(f$used, 12)
  need_variation(f$data, roles$outcome, "ARIMA")
  if (anyDuplicated(f$data[[roles$time]])) stop("The time column contains duplicate values. Keep one observation per time point or combine duplicates first.")
  ordered <- order(f$data[[roles$time]])
  frame <- f$data[ordered, , drop = FALSE]
  order_vec <- as.integer(c(options$ar_p %||% 1, options$ar_d %||% 0, options$ar_q %||% 0))
  frequency <- as.integer(options$frequency %||% 1)
  if (length(order_vec) != 3L || anyNA(order_vec) || any(order_vec < 0L)) stop("ARIMA p, d, and q must be nonnegative whole numbers.")
  if (length(frequency) != 1L || is.na(frequency) || frequency < 1L) stop("Time-series frequency must be a positive whole number.")
  if (sum(order_vec) >= f$used - 3L) stop("The selected ARIMA order is too large for this series. Lower p, d, or q, or add more time points.")
  time_values <- frame[[roles$time]]
  time_numeric <- if (inherits(time_values, "Date")) {
    calendar <- as.POSIXlt(time_values)
    month_index <- (calendar$year + 1900L) * 12L + calendar$mon
    if (length(unique(calendar$mday)) == 1L && length(unique(diff(month_index))) == 1L) month_index else as.numeric(time_values)
  } else if (inherits(time_values, "POSIXt")) {
    as.numeric(time_values)
  } else {
    time_values
  }
  if (is.numeric(time_numeric) && length(time_numeric) >= 3L) {
    gaps <- diff(time_numeric)
    tolerance <- sqrt(.Machine$double.eps) * max(1, max(abs(gaps)))
    if (any(gaps <= 0) || max(abs(gaps - stats::median(gaps))) > tolerance) {
      stop("ARIMA needs equally spaced, increasing time points. Fill or aggregate time gaps before fitting this model.")
    }
  }
  series <- stats::ts(frame[[roles$outcome]], frequency = frequency)
  model <- stats::arima(series, order = order_vec, include.mean = TRUE, method = "ML")
  if (!is.null(model$code) && model$code != 0L) stop("The ARIMA optimizer did not settle on a solution. Try a simpler p, d, and q order.")
  fitted <- as.numeric(series - stats::residuals(model))
  coefs <- data.frame(Term = names(model$coef), Estimate = unname(model$coef),
                      Std_error = sqrt(diag(model$var.coef)), row.names = NULL)
  coefs$Statistic <- coefs$Estimate / coefs$Std_error
  coefs$p <- 2 * stats::pnorm(abs(coefs$Statistic), lower.tail = FALSE)
  critical <- stats::qnorm(1 - (1 - confidence_level(options)) / 2)
  coefs$Lower <- coefs$Estimate - critical * coefs$Std_error; coefs$Upper <- coefs$Estimate + critical * coefs$Std_error
  fit_df <- sum(order_vec[c(1, 3)])
  lag <- min(length(series) - 1, max(fit_df + 3, min(10, floor(length(series) / 5))))
  if (lag > fit_df) {
    box <- stats::Box.test(stats::residuals(model), lag = lag, type = "Ljung-Box", fitdf = fit_df)
    checks <- check_row("Residual autocorrelation", if (box$p.value >= 0.05) "No clear pattern remained" else "Serial pattern remained",
                        sprintf("Ljung-Box at lag %d: p %s.", lag, fmt_p(box$p.value)), if (box$p.value >= 0.05) "ok" else "warn")
  } else {
    checks <- check_row("Residual autocorrelation", "Not checked",
                        "The selected AR and MA order leaves too few lags for a Ljung-Box check in this series.", "warn")
  }
  fit <- data.frame(Metric = c("AIC", "Log likelihood", "Residual variance"), Value = c(model$aic, as.numeric(model$loglik), model$sigma2))

  # The everyday explanation tells the reader to look for a widening forecast
  # band, so the model has to produce one. The horizon is capped at a quarter
  # of the observed length because an ARIMA forecast that runs much past that
  # is mostly the series mean with a very wide interval around it.
  horizon <- as.integer(options$forecast_horizon %||% 0L)
  if (length(horizon) != 1L || is.na(horizon) || horizon < 0L) stop("The forecast horizon must be zero or a positive whole number.")
  # Capped rather than refused, so a reader who asks for too much still gets a
  # projection instead of an error. Past roughly a quarter of the recorded
  # length an ARIMA forecast is mostly the series mean with a very wide band.
  horizon <- min(horizon, max(0L, floor(f$used / 4)))
  observed_frame <- data.frame(
    Time = time_values, Observed = as.numeric(series), Fitted = fitted,
    Lower = NA_real_, Upper = NA_real_, Segment = "Observed", stringsAsFactors = FALSE
  )
  forecast_frame <- NULL
  if (horizon > 0L) {
    ahead <- stats::predict(model, n.ahead = horizon)
    critical <- stats::qnorm(1 - (1 - confidence_level(options)) / 2)
    forecast_frame <- data.frame(
      Time = future_time_points(time_values, horizon),
      Observed = NA_real_, Fitted = as.numeric(ahead$pred),
      Lower = as.numeric(ahead$pred) - critical * as.numeric(ahead$se),
      Upper = as.numeric(ahead$pred) + critical * as.numeric(ahead$se),
      Segment = "Forecast", stringsAsFactors = FALSE
    )
  }
  plot_frame <- if (is.null(forecast_frame)) observed_frame else rbind(observed_frame, forecast_frame)

  result_record("time_series", f$used, f$omitted, coefs, fit = fit, checks = checks, model = model,
                plot_kind = "time_series", plot_data = plot_frame,
                extras = list(outcome = roles$outcome, time = roles$time, order = order_vec,
                              frequency = frequency, horizon = horizon,
                              forecast = forecast_frame),
                code = c(sprintf("complete <- complete.cases(df[c(\"%s\", \"%s\")])", roles$outcome, roles$time),
                         sprintf("ordered_rows <- which(complete)[order(df[[\"%s\"]][complete])]", roles$time),
                         sprintf("series <- ts(df[[\"%s\"]][ordered_rows], frequency = %d)", roles$outcome, frequency),
                         sprintf("fit <- arima(series, order = c(%s), method = 'ML')", paste(order_vec, collapse = ", ")), "fit"))
}
