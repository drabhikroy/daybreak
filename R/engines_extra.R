# Equivalence, agreement, and combining studies -----------------------------
#
# Three questions the comparison and model families do not answer. Are two
# groups close enough to treat as the same, do two raters agree beyond what
# chance would give, and what do several studies of one question say together.
# Each returns the same record as every other engine.

# Equivalence ---------------------------------------------------------------
#
# A test of difference that comes back unclear has not shown that two groups
# are the same, and reading it that way is one of the most common mistakes in
# applied work. Two one-sided tests (Schuirmann, 1987) put the burden the other
# way round: the groups count as equivalent only when the data rule out a
# difference as large as a margin chosen in advance. The Welch form is used so
# the result does not lean on equal variances (Lakens, 2017).
#
# The margin is a judgment about the subject matter and cannot be read from the
# data. When none is given the engine uses half a pooled standard deviation so
# the example runs, and says in the checks that it did.
analyze_equivalence <- function(data, roles, options) {
  f <- complete_frame(data, c(roles$outcome, roles$group))
  need_numeric(f$data, roles$outcome); need_levels(f$data[[roles$group]], 2, 2, "The group column")
  g <- as_group(f$data[[roles$group]])
  need_group_sizes(g, 2L)
  x <- f$data[[roles$outcome]][g == levels(g)[1]]
  y <- f$data[[roles$outcome]][g == levels(g)[2]]
  if (stats::sd(x) <= sqrt(.Machine$double.eps) && stats::sd(y) <= sqrt(.Machine$double.eps)) {
    stop("Neither group has measurable variation, so the comparison cannot be calculated.")
  }
  pooled_sd <- sqrt(((length(x) - 1) * stats::var(x) + (length(y) - 1) * stats::var(y)) / (length(x) + length(y) - 2))
  supplied <- suppressWarnings(as.numeric(options$equivalence_margin %||% NA_real_))
  given <- length(supplied) == 1L && is.finite(supplied)
  if (given && supplied <= 0) stop("The equivalence margin must be greater than zero.")
  margin <- if (given) supplied else 0.5 * pooled_sd

  level <- confidence_level(options)
  alpha <- 1 - level
  lower_test <- stats::t.test(x, y, mu = -margin, alternative = "greater")
  upper_test <- stats::t.test(x, y, mu = margin, alternative = "less")
  # The interval that matches a test at alpha is the one at 1 - 2 * alpha, so a
  # 95 percent setting gives a 90 percent interval here. The record carries
  # that level so the written readings name the interval actually shown.
  interval_level <- 1 - 2 * alpha
  interval <- stats::t.test(x, y, conf.level = interval_level)$conf.int
  difference <- stats::t.test(x, y)

  est <- data.frame(
    Contrast = paste(levels(g), collapse = " minus "), Estimate = mean(x) - mean(y),
    Lower = unname(interval[1]), Upper = unname(interval[2]),
    Margin_low = -margin, Margin_high = margin,
    Effect = hedges_g(x, y), Effect_name = "Hedges' g", stringsAsFactors = FALSE
  )
  tests <- data.frame(
    Test = c("Equivalence, the larger one-sided p", "Above the lower margin", "Below the upper margin", "Difference from zero, Welch t"),
    Statistic = unname(c(NA_real_, lower_test$statistic, upper_test$statistic, difference$statistic)),
    df = unname(c(lower_test$parameter, lower_test$parameter, upper_test$parameter, difference$parameter)),
    p = c(max(lower_test$p.value, upper_test$p.value), lower_test$p.value, upper_test$p.value, difference$p.value)
  )
  margin_check <- if (given) {
    check_row("Margin", "Set by you", sprintf("Differences smaller than %s in either direction count as equivalent.", fmt_num(margin, 3)), "ok")
  } else {
    check_row("Margin", "Default used",
              sprintf("No margin was entered, so Daybreak used half a pooled standard deviation, %s. Enter the smallest difference that would matter in your setting under Analysis settings.", fmt_num(margin, 3)),
              "warn")
  }
  checks <- rbind(margin_check, normality_check(x, levels(g)[1]), normality_check(y, levels(g)[2]))
  result_record("equivalence", f$used, f$omitted, est, tests, checks = checks,
                plot_kind = "equivalence", plot_data = f$data,
                extras = list(outcome = roles$outcome, group = roles$group, levels = levels(g),
                              margin = margin, margin_given = given, interval_level = interval_level),
                code = c(sprintf("fit_data <- df[complete.cases(df[c(%s, %s)]), ]", r_str(roles$outcome), r_str(roles$group)),
                         sprintf("group <- droplevels(factor(fit_data[[%s]]))", r_str(roles$group)),
                         sprintf("x <- fit_data[[%s]][group == levels(group)[1]]", r_str(roles$outcome)),
                         sprintf("y <- fit_data[[%s]][group == levels(group)[2]]", r_str(roles$outcome)),
                         sprintf("margin <- %s", format(margin, digits = 15)),
                         "t.test(x, y, mu = -margin, alternative = \"greater\")",
                         "t.test(x, y, mu = margin, alternative = \"less\")",
                         sprintf("t.test(x, y, conf.level = %s)$conf.int", format(interval_level, digits = 15))))
}

# Where the interval sits against the margin. Inside it on both ends is
# equivalence, wholly outside it is a difference too large to wave away, and
# anything else has not settled the question either way.
equivalence_state <- function(result) {
  e <- result$estimates[1, ]
  if (e$Lower > e$Margin_low && e$Upper < e$Margin_high) return("equivalent")
  if (e$Lower > e$Margin_high || e$Upper < e$Margin_low) return("apart")
  "unsettled"
}

# Agreement -----------------------------------------------------------------
#
# Two raters who each said "yes" to most cases will agree often by accident.
# Kappa (Cohen, 1960) reports agreement after removing that accidental share.
# The weighted form (Cohen, 1968) gives partial credit for near misses on an
# ordered scale. The standard error is the large-sample one of Fleiss, Cohen,
# and Everitt (1969), written once for a general weight matrix, of which the
# unweighted coefficient is the case with ones on the diagonal and zeros
# elsewhere.
kappa_weights <- function(k, scheme = c("none", "linear", "quadratic")) {
  scheme <- match.arg(scheme)
  distance <- abs(outer(seq_len(k), seq_len(k), "-"))
  switch(scheme,
    none = diag(k),
    linear = 1 - distance / (k - 1),
    quadratic = 1 - (distance / (k - 1))^2
  )
}

kappa_from_table <- function(tab, scheme = "none") {
  n <- sum(tab)
  p <- tab / n
  w <- kappa_weights(nrow(p), scheme)
  rows <- rowSums(p); cols <- colSums(p)
  observed <- sum(w * p)
  expected <- sum(w * outer(rows, cols))
  kappa <- (observed - expected) / (1 - expected)
  row_weight <- as.vector(w %*% cols)
  col_weight <- as.vector(rows %*% w)
  term <- (w * (1 - expected) - outer(row_weight, col_weight, "+") * (1 - observed))^2
  variance <- (sum(p * term) - (observed * expected - 2 * expected + observed)^2) / (n * (1 - expected)^4)
  list(kappa = kappa, se = sqrt(max(variance, 0)), observed = observed, expected = expected)
}

# The usual verbal bands of Landis and Koch (1977), offered as a reading aid
# beside the number and never in place of it.
kappa_band <- function(value) {
  if (!is.finite(value)) return("not available")
  if (value < 0) "less than chance"
  else if (value <= 0.20) "slight"
  else if (value <= 0.40) "fair"
  else if (value <= 0.60) "moderate"
  else if (value <= 0.80) "substantial"
  else "almost perfect"
}

analyze_agreement <- function(data, roles, options) {
  f <- complete_frame(data, c(roles$rater_1, roles$rater_2))
  first <- f$data[[roles$rater_1]]; second <- f$data[[roles$rater_2]]
  # Both raters are placed on one shared set of categories. A category only one
  # of them used still belongs in the table, as a row or column of zeros.
  categories <- if (is.factor(first) && is.factor(second) && identical(levels(first), levels(second))) {
    levels(first)
  } else {
    sort(unique(c(as.character(first), as.character(second))))
  }
  if (length(categories) < 2L) stop("Agreement needs at least two categories across the two rating columns.")
  if (length(categories) > 12L) stop("These columns hold more than 12 different values, which suggests measurements and not ratings. Choose two columns of category ratings.")
  need_n(f$used, 5, "rows rated by both raters")
  tab <- table(factor(as.character(first), levels = categories), factor(as.character(second), levels = categories))
  scheme <- options$kappa_weights %||% "none"
  if (!scheme %in% c("none", "linear", "quadratic")) stop("Choose unweighted, linear, or quadratic weights.")
  if (length(categories) == 2L) scheme <- "none"
  stat <- kappa_from_table(tab, scheme)
  if (!is.finite(stat$kappa)) stop("Both raters used a single category for every row, so agreement beyond chance cannot be calculated.")
  z <- stats::qnorm(1 - (1 - confidence_level(options)) / 2)
  plain <- kappa_from_table(tab, "none")
  est <- data.frame(
    Coefficient = switch(scheme, none = "Cohen's kappa", linear = "Weighted kappa, linear", quadratic = "Weighted kappa, quadratic"),
    Estimate = stat$kappa, Lower = max(-1, stat$kappa - z * stat$se), Upper = min(1, stat$kappa + z * stat$se),
    SE = stat$se, Observed_agreement = plain$observed, Chance_agreement = plain$expected,
    Reading = kappa_band(stat$kappa), stringsAsFactors = FALSE
  )
  spread <- min(rowSums(tab) + colSums(tab)) / (2 * sum(tab))
  checks <- rbind(
    check_row("Category use", if (spread >= 0.05) "Categories are all in use" else "A category is rarely used",
              sprintf("The least used category holds %s of all ratings. Kappa falls when one category dominates, even with high raw agreement.", fmt_pct(spread)),
              if (spread >= 0.05) "ok" else "warn"),
    check_row("Scale", if (scheme == "none") "Categories treated as unordered" else "Categories treated as ordered",
              if (scheme == "none") "Every disagreement counts the same." else sprintf("Near misses earn partial credit, in the order: %s.", paste(categories, collapse = ", ")),
              "note")
  )
  result_record("agreement", f$used, f$omitted, est, checks = checks,
                plot_kind = "agreement", plot_data = as.data.frame(tab, responseName = "Count"),
                extras = list(rater_1 = roles$rater_1, rater_2 = roles$rater_2, categories = categories,
                              weights = scheme, observed = as.data.frame.matrix(tab)),
                code = c(sprintf("ratings <- df[complete.cases(df[c(%s, %s)]), c(%s, %s)]", r_str(roles$rater_1), r_str(roles$rater_2), r_str(roles$rater_1), r_str(roles$rater_2)),
                         sprintf("categories <- c(%s)", paste(r_str(categories), collapse = ", ")),
                         "tab <- table(factor(as.character(ratings[[1]]), levels = categories), factor(as.character(ratings[[2]]), levels = categories))",
                         "p <- tab / sum(tab)",
                         "observed <- sum(diag(p)); expected <- sum(rowSums(p) * colSums(p))",
                         "c(observed = observed, chance = expected, kappa = (observed - expected) / (1 - expected))",
                         if (scheme != "none") sprintf("# The weighted coefficient shown in the app uses %s weights. psych::cohen.kappa(tab) reports weighted values.", scheme)))
}

# Combining studies ---------------------------------------------------------
#
# A random-effects model, because studies of one question almost never estimate
# exactly the same quantity. The between-study variance is estimated by
# restricted maximum likelihood, which Viechtbauer (2005) found to balance bias
# and efficiency better than the common alternatives, and the fit comes from
# the metafor package
# (Viechtbauer, 2010). The prediction interval is reported beside the pooled
# estimate because the pooled estimate describes the average study, and a
# reader usually wants to know what the next one might find (IntHout,
# Ioannidis, Rovers, and Goeman, 2016).
analyze_meta_analysis <- function(data, roles, options) {
  label_role <- roles$study %||% ""
  columns <- c(roles$effect, roles$se, if (nzchar(label_role)) label_role)
  f <- complete_frame(data, columns)
  need_numeric(f$data, c(roles$effect, roles$se))
  need_n(f$used, 3, "studies with an effect and a standard error")
  effect <- f$data[[roles$effect]]; se <- f$data[[roles$se]]
  if (any(se <= 0)) stop("Every standard error must be greater than zero. Check that the standard error column is not a variance, a sample size, or a p-value.")
  labels <- if (nzchar(label_role)) as.character(f$data[[label_role]]) else paste("Study", seq_along(effect))
  labels <- make.unique(labels, sep = " ")
  level <- confidence_level(options)
  model <- suppressWarnings(metafor::rma(yi = effect, sei = se, method = "REML", level = 100 * level))
  prediction <- stats::predict(model, level = 100 * level)
  est <- data.frame(
    Term = "Pooled effect", Estimate = as.numeric(model$beta), Lower = model$ci.lb, Upper = model$ci.ub,
    SE = model$se, Prediction_lower = prediction$pi.lb, Prediction_upper = prediction$pi.ub, stringsAsFactors = FALSE
  )
  tests <- data.frame(
    Test = c("Pooled effect differs from zero, z", "Heterogeneity, Q"),
    Statistic = c(model$zval, model$QE), df = c(NA_real_, model$k - 1), p = c(model$pval, model$QEp)
  )
  fit <- data.frame(
    Metric = c("Studies", "Tau squared", "Tau", "I squared, percent", "H squared"),
    Value = c(model$k, model$tau2, sqrt(model$tau2), model$I2, model$H2)
  )
  z <- stats::qnorm(1 - (1 - level) / 2)
  studies <- data.frame(Study = labels, Estimate = effect, Lower = effect - z * se, Upper = effect + z * se,
                        Weight = as.numeric(stats::weights(model)), stringsAsFactors = FALSE)
  checks <- rbind(
    check_row("Number of studies", if (model$k >= 10) "Enough to estimate the spread" else "Few studies",
              sprintf("%d studies entered. The between-study variance is poorly estimated from a handful, so read the prediction interval as rough.", model$k),
              if (model$k >= 10) "ok" else "warn"),
    check_row("Consistency", if (model$I2 < 50) "Studies broadly agree" else "Studies disagree",
              sprintf("About %s of the variation across studies reflects real differences between them and not sampling error.", fmt_pct(model$I2 / 100)),
              if (model$I2 < 50) "ok" else "warn"),
    check_row("Scale", "Effects pooled as entered",
              "Ratios must be entered on the log scale, with standard errors on the same scale. Daybreak does not transform the effect column.", "note")
  )
  result_record("meta_analysis", f$used, f$omitted, est, tests, fit = fit, checks = checks,
                plot_kind = "meta", plot_data = studies,
                extras = list(effect = roles$effect, se = roles$se, study = label_role, k = model$k,
                              i2 = model$I2, tau2 = model$tau2, studies = studies),
                code = c(sprintf("fit <- metafor::rma(yi = df[[%s]], sei = df[[%s]], method = \"REML\", level = %s)",
                                 r_str(roles$effect), r_str(roles$se), 100 * level),
                         "summary(fit)", sprintf("predict(fit, level = %s)", 100 * level)))
}
