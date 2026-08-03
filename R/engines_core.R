# Descriptive analyses, comparisons, and familiar models ------------------

# One entry point for every method. The catalog holds the contract, the engine
# holds the statistics, and this function is the only place that knows how to
# get from one to the other. Keeping the dispatch here means a new method needs
# a catalog entry and an analyse_ function and nothing else.
run_analysis <- function(method_id, data, roles, options = list()) {
  spec <- ANALYSES[[method_id]]
  if (is.null(spec)) stop("The selected analysis is not in the catalog.")
  absent <- missing_packages(spec)
  if (length(absent)) {
    cmd <- sprintf("install.packages(c(%s))", paste(sprintf("\"%s\"", absent), collapse = ", "))
    stop(sprintf("%s needs: %s. Run %s and restart the app.", spec$name, paste(absent, collapse = ", "), cmd))
  }
  # The same column filling two roles is almost always a slip rather than an
  # intention, and it produces results that look plausible. Model syntax is
  # exempt because a lavaan model names its variables inside a string.
  duplicate_check_roles <- setdiff(names(roles), "model_syntax")
  if (method_id %in% c("hlm_linear", "hlm_logistic")) {
    duplicate_check_roles <- setdiff(duplicate_check_roles, "random_slope")
  }
  column_roles <- roles[duplicate_check_roles]
  selected <- unlist(column_roles, use.names = FALSE)
  selected <- selected[nzchar(selected)]
  repeated <- unique(selected[duplicated(selected)])
  if (length(repeated)) {
    stop(sprintf("A column cannot fill more than one role in the same analysis. Choose a different role for: %s.", paste(repeated, collapse = ", ")))
  }
  engine_name <- paste0("analyse_", spec$engine)
  if (!exists(engine_name, mode = "function")) stop(sprintf("No engine is registered for %s.", spec$name))
  options$.method_id <- method_id
  result <- get(engine_name, mode = "function")(data, roles, options)

  # Every engine that builds an interval reads the level through
  # confidence_level(), which applies the same 0.95 default. Recording it once
  # here means no engine has to remember to pass it along, and the two writing
  # layers can always name the interval they are describing.
  result$conf_level <- tryCatch(confidence_level(options), error = function(e) 0.95)
  result
}

# Descriptives ------------------------------------------------------------
#
# Reports the median and the interquartile range beside the mean rather than
# the mean alone. Evaluation data is frequently skewed, and a reader shown only
# a mean has no way to notice that.
analyse_descriptives <- function(data, roles, options) {
  variables <- roles$variables
  # An optional role arrives as an empty string when the reader has not chosen a
  # column, and an empty string is not a column name. Leaving it in the vector
  # meant that summarising a file without grouping it, which is the first thing
  # most people do here, stopped with "undefined columns selected".
  group <- roles$group %||% ""
  group <- if (length(group) == 1L && nzchar(group)) group else ""
  columns <- c(variables, if (nzchar(group)) group else NULL)
  require_columns(data, columns)
  need_numeric(data, variables)
  summaries <- lapply(variables, function(v) {
    x <- data[[v]]
    by <- if (nzchar(group)) as_group(data[[group]]) else factor(rep("All rows", length(x)))
    pieces <- split(x, by, drop = TRUE)
    do.call(rbind, lapply(names(pieces), function(g) {
      z <- pieces[[g]]
      z <- z[is.finite(z)]
      if (!length(z)) {
        return(data.frame(
          Variable = v, Group = g, N = 0L, Missing = sum(is.na(pieces[[g]])),
          Mean = NA_real_, SD = NA_real_, Median = NA_real_, IQR = NA_real_,
          Minimum = NA_real_, Maximum = NA_real_, Q1 = NA_real_, Q3 = NA_real_,
          stringsAsFactors = FALSE
        ))
      }
      data.frame(
        Variable = v, Group = g, N = length(z), Missing = sum(is.na(pieces[[g]])),
        Mean = mean(z), SD = if (length(z) >= 2L) stats::sd(z) else NA_real_, Median = stats::median(z),
        IQR = stats::IQR(z), Minimum = min(z), Maximum = max(z),
        Q1 = unname(stats::quantile(z, 0.25)), Q3 = unname(stats::quantile(z, 0.75)),
        stringsAsFactors = FALSE
      )
    }))
  })
  estimates <- do.call(rbind, summaries)
  result_record(
    "descriptives", nrow(data), 0, estimates = estimates,
    checks = check_row("Data coverage", "Reported by variable", "Each row of the table states its complete and missing counts.", "note"),
    plot_kind = "distributions", plot_data = data[, unique(columns), drop = FALSE],
    extras = list(variables = variables, group = group),
    code = c(sprintf("variables <- c(%s)", paste(sprintf("\"%s\"", variables), collapse = ", ")),
             "summary(df[variables])")
  )
}

analyse_frequencies <- function(data, roles, options) {
  variables <- roles$variables
  split_var <- roles$split %||% ""
  require_columns(data, c(variables, split_var))
  pieces <- lapply(variables, function(v) {
    if (nzchar(split_var)) {
      tab <- as.data.frame(table(Variable = data[[v]], Split = data[[split_var]], useNA = "ifany"))
      tab$Percent <- ave(tab$Freq, tab$Split, FUN = function(x) x / sum(x))
    } else {
      tab <- as.data.frame(table(Variable = data[[v]], useNA = "ifany"))
      tab$Split <- "All rows"
      tab$Percent <- tab$Freq / sum(tab$Freq)
    }
    tab$Measure <- v
    names(tab)[names(tab) == "Freq"] <- "Count"
    tab[, c("Measure", "Variable", "Split", "Count", "Percent")]
  })
  estimates <- do.call(rbind, pieces)
  result_record(
    "frequencies", nrow(data), 0, estimates = estimates,
    checks = check_row("Denominator", "Includes displayed categories", "Percentages use the count within each split, including missing when shown.", "note"),
    plot_kind = "frequencies", plot_data = estimates,
    extras = list(variables = variables, split = split_var),
    code = c(sprintf("variables <- c(%s)", paste(sprintf("\"%s\"", variables), collapse = ", ")),
             "lapply(df[variables], table, useNA = 'ifany')")
  )
}

# Missingness is a first-class result here rather than a footnote on another
# method. Whether values are missing at random is a substantive finding, and a
# reader who can see the pattern before choosing a test makes a better choice.
analyse_missingness <- function(data, roles, options) {
  variables <- roles$variables
  require_columns(data, variables)
  need_n(nrow(data), 1)
  frame <- data[, variables, drop = FALSE]
  estimates <- data.frame(
    Variable = variables,
    Missing = vapply(frame, function(x) sum(is.na(x)), integer(1)),
    Percent = vapply(frame, function(x) mean(is.na(x)), numeric(1)),
    Complete = vapply(frame, function(x) sum(!is.na(x)), integer(1)),
    stringsAsFactors = FALSE
  )
  pattern <- apply(is.na(frame), 1, function(x) paste(ifelse(x, "Missing", "Present"), collapse = " | "))
  patterns <- sort(table(pattern), decreasing = TRUE)
  pattern_table <- data.frame(Pattern = names(patterns), Count = as.integer(patterns),
                              Percent = as.integer(patterns) / nrow(frame), row.names = NULL)
  result_record(
    "missingness", nrow(data), 0, estimates = estimates,
    checks = check_row("Missing-data cause", "Not identified", "The pattern can be described from the file, but its cause needs subject-matter knowledge.", "warn"),
    plot_kind = "missingness", plot_data = estimates,
    extras = list(patterns = head(pattern_table, 20), variables = variables),
    code = c(sprintf("variables <- c(%s)", paste(sprintf("\"%s\"", variables), collapse = ", ")),
             "colMeans(is.na(df[variables]))")
  )
}

# Comparing means ---------------------------------------------------------
analyse_one_sample_t <- function(data, roles, options) {
  mu <- as.numeric(options$mu %||% 0)
  if (length(mu) != 1L || !is.finite(mu)) stop("The reference mean must be one finite number.")
  f <- complete_frame(data, roles$outcome)
  x <- f$data[[roles$outcome]]
  need_numeric(f$data, roles$outcome); need_n(length(x), 2)
  level <- confidence_level(options)
  # A constant column gives a zero denominator and a t statistic of infinity.
  # Stopping here names the column problem instead of printing Inf.
  if (stats::sd(x) <= sqrt(.Machine$double.eps)) stop("The selected outcome has no measurable variation, so a t statistic cannot be calculated.")
  test <- stats::t.test(x, mu = mu, conf.level = level)
  sd_x <- stats::sd(x)
  est <- data.frame(
    Contrast = sprintf("Mean minus %s", fmt_num(mu)), Estimate = mean(x) - mu,
    Lower = unname(test$conf.int[1]) - mu, Upper = unname(test$conf.int[2]) - mu,
    Effect = (mean(x) - mu) / sd_x, Effect_name = "Cohen's d", stringsAsFactors = FALSE
  )
  tests <- data.frame(Test = "One-sample t", Statistic = unname(test$statistic),
                      df = unname(test$parameter), p = test$p.value)
  result_record("one_sample_t", f$used, f$omitted, est, tests,
                checks = normality_check(x), plot_kind = "one_numeric", plot_data = f$data,
                extras = list(outcome = roles$outcome, reference = mu),
                code = sprintf("t.test(df[[\"%s\"]], mu = %s, conf.level = %s)", roles$outcome, mu, level))
}

# Welch is the default rather than the pooled test. Equal variances are an
# assumption most datasets do not meet, Welch costs almost nothing when they
# do, and the pooled test is wrong when they do not. The form still offers the
# pooled version for a reader replicating published work that used it.
analyse_independent_t <- function(data, roles, options) {
  f <- complete_frame(data, c(roles$outcome, roles$group))
  need_numeric(f$data, roles$outcome); need_levels(f$data[[roles$group]], 2, 2, "The group column")
  g <- as_group(f$data[[roles$group]])
  need_group_sizes(g, 2L)
  x <- f$data[[roles$outcome]][g == levels(g)[1]]
  y <- f$data[[roles$outcome]][g == levels(g)[2]]
  equal_var <- isTRUE(options$equal_var)
  if (stats::sd(x) <= sqrt(.Machine$double.eps) && stats::sd(y) <= sqrt(.Machine$double.eps)) {
    stop("Neither group has measurable variation, so a t statistic cannot be calculated.")
  }
  test <- stats::t.test(x, y, var.equal = equal_var, conf.level = confidence_level(options))
  est <- data.frame(
    Contrast = paste(levels(g), collapse = " minus "), Estimate = mean(x) - mean(y),
    Lower = unname(test$conf.int[1]), Upper = unname(test$conf.int[2]),
    Effect = hedges_g(x, y), Effect_name = "Hedges' g", stringsAsFactors = FALSE
  )
  tests <- data.frame(Test = if (equal_var) "Pooled-variance t" else "Welch t",
                      Statistic = unname(test$statistic), df = unname(test$parameter), p = test$p.value)
  checks <- rbind(normality_check(x, levels(g)[1]), normality_check(y, levels(g)[2]))
  checks$Check <- paste(checks$Check, c(levels(g)[1], levels(g)[2]), sep = ": ")
  result_record("independent_t", f$used, f$omitted, est, tests, checks = checks,
                plot_kind = "group_numeric", plot_data = f$data,
                extras = list(outcome = roles$outcome, group = roles$group, levels = levels(g)),
                code = sprintf("t.test(%s ~ %s, data = df, var.equal = %s, conf.level = %s)", bt(roles$outcome), bt(roles$group), equal_var, confidence_level(options)))
}

analyse_paired_t <- function(data, roles, options) {
  f <- complete_frame(data, c(roles$before, roles$after))
  need_numeric(f$data, c(roles$before, roles$after)); need_n(f$used, 2, "complete pairs")
  before <- f$data[[roles$before]]; after <- f$data[[roles$after]]
  if (stats::sd(after - before) <= sqrt(.Machine$double.eps)) stop("The paired changes have no measurable variation, so a t statistic cannot be calculated.")
  test <- stats::t.test(after, before, paired = TRUE, conf.level = confidence_level(options))
  est <- data.frame(Contrast = paste(roles$after, "minus", roles$before),
                    Estimate = mean(after - before), Lower = unname(test$conf.int[1]), Upper = unname(test$conf.int[2]),
                    Effect = cohens_dz(before, after), Effect_name = "Cohen's dz")
  tests <- data.frame(Test = "Paired t", Statistic = unname(test$statistic),
                      df = unname(test$parameter), p = test$p.value)
  result_record("paired_t", f$used, f$omitted, est, tests,
                checks = normality_check(after - before, "Paired differences"),
                plot_kind = "paired", plot_data = f$data,
                extras = list(before = roles$before, after = roles$after),
                code = sprintf("t.test(df[[\"%s\"]], df[[\"%s\"]], paired = TRUE, conf.level = %s)", roles$after, roles$before, confidence_level(options)))
}

# The rank tests report a Hodges-Lehmann shift alongside the p-value. A rank
# test that returns only a p-value tells the reader that something differs
# without telling them by how much, which is the part they actually needed.
analyse_mann_whitney <- function(data, roles, options) {
  f <- complete_frame(data, c(roles$outcome, roles$group))
  need_numeric(f$data, roles$outcome); need_levels(f$data[[roles$group]], 2, 2, "The group column")
  g <- as_group(f$data[[roles$group]])
  need_group_sizes(g, 2L)
  test <- suppressWarnings(stats::wilcox.test(f$data[[roles$outcome]] ~ g, exact = FALSE))
  interval_test <- tryCatch(
    suppressWarnings(stats::wilcox.test(f$data[[roles$outcome]] ~ g, conf.int = TRUE,
                                        conf.level = confidence_level(options), exact = FALSE)),
    error = function(e) NULL
  )
  medians <- tapply(f$data[[roles$outcome]], g, stats::median)
  # The Hodges-Lehmann shift is the median of all between-group differences. It
  # is the effect size that belongs with a rank test, and reporting it stops the
  # result from being a p-value with nothing attached.
  shift <- hodges_lehmann_two_sample(f$data[[roles$outcome]][g == levels(g)[1]],
                                    f$data[[roles$outcome]][g == levels(g)[2]])
  if (!is.null(interval_test) && length(interval_test$estimate)) shift <- unname(interval_test$estimate)
  interval <- if (!is.null(interval_test)) interval_test$conf.int %||% c(NA_real_, NA_real_) else c(NA_real_, NA_real_)
  est <- data.frame(Contrast = paste(levels(g), collapse = " versus "),
                    Estimate = shift, Lower = unname(interval[1]), Upper = unname(interval[2]),
                    Effect = NA_real_, Effect_name = "Hodges-Lehmann location shift")
  tests <- data.frame(Test = "Mann-Whitney", Statistic = unname(test$statistic), df = NA_real_, p = test$p.value)
  result_record("mann_whitney", f$used, f$omitted, est, tests,
                checks = check_row("Scale", "Ranks compared", "The test concerns the distributions and ranks, not only the medians.", "note"),
                plot_kind = "group_numeric", plot_data = f$data,
                extras = list(outcome = roles$outcome, group = roles$group, levels = levels(g), medians = medians),
                code = sprintf("wilcox.test(%s ~ %s, data = df, exact = FALSE, conf.int = TRUE, conf.level = %s)", bt(roles$outcome), bt(roles$group), confidence_level(options)))
}

analyse_paired_wilcoxon <- function(data, roles, options) {
  f <- complete_frame(data, c(roles$before, roles$after))
  need_numeric(f$data, c(roles$before, roles$after)); need_n(f$used, 2, "complete pairs")
  test <- suppressWarnings(stats::wilcox.test(f$data[[roles$after]], f$data[[roles$before]], paired = TRUE, exact = FALSE))
  interval_test <- tryCatch(
    suppressWarnings(stats::wilcox.test(f$data[[roles$after]], f$data[[roles$before]], paired = TRUE,
                                        exact = FALSE, conf.int = TRUE, conf.level = confidence_level(options))),
    error = function(e) NULL
  )
  difference <- f$data[[roles$after]] - f$data[[roles$before]]
  interval <- if (!is.null(interval_test)) interval_test$conf.int %||% c(NA_real_, NA_real_) else c(NA_real_, NA_real_)
  shift <- hodges_lehmann_paired(difference)
  if (!is.null(interval_test) && length(interval_test$estimate)) shift <- unname(interval_test$estimate)
  est <- data.frame(Contrast = paste(roles$after, "minus", roles$before), Estimate = shift,
                    Lower = unname(interval[1]), Upper = unname(interval[2]), Effect = NA_real_,
                    Effect_name = "Hodges-Lehmann paired shift")
  tests <- data.frame(Test = "Wilcoxon signed-rank", Statistic = unname(test$statistic), df = NA_real_, p = test$p.value)
  result_record("paired_wilcoxon", f$used, f$omitted, est, tests,
                checks = check_row("Difference shape", "Symmetry is assumed", "The signed-rank test reads the paired-difference distribution as roughly symmetric.", "note"),
                plot_kind = "paired", plot_data = f$data, extras = list(before = roles$before, after = roles$after),
                code = sprintf("wilcox.test(df[[\"%s\"]], df[[\"%s\"]], paired = TRUE, exact = FALSE, conf.int = TRUE, conf.level = %s)", roles$after, roles$before, confidence_level(options)))
}

analyse_one_way_anova <- function(data, roles, options) {
  f <- complete_frame(data, c(roles$outcome, roles$group))
  need_numeric(f$data, roles$outcome); need_levels(f$data[[roles$group]], 2)
  need_variation(f$data, roles$outcome, "One-way ANOVA")
  f$data[[roles$group]] <- as_group(f$data[[roles$group]])
  need_group_sizes(f$data[[roles$group]], 2L)
  form <- formula_from(roles$outcome, roles$group)
  need_model_matrix(form, f$data, 2L)
  model <- stats::aov(form, data = f$data)
  tab <- as.data.frame(summary(model)[[1]])
  test <- data.frame(Test = "One-way ANOVA", Statistic = tab[["F value"]][1], df1 = tab[["Df"]][1],
                     df2 = tab[["Df"]][2], p = tab[["Pr(>F)"]][1])
  est <- data.frame(Effect = roles$group, Omega_squared = omega_squared(tab))
  tukey <- as.data.frame(stats::TukeyHSD(model)[[1]])
  tukey$Contrast <- rownames(tukey); rownames(tukey) <- NULL
  names(tukey)[1:4] <- c("Difference", "Lower", "Upper", "Adjusted_p")
  checks <- normality_check(stats::residuals(model), "Model residuals")
  result_record("one_way_anova", f$used, f$omitted, est, test, checks = checks, model = model,
                plot_kind = "group_numeric", plot_data = f$data,
                extras = list(outcome = roles$outcome, group = roles$group, pairwise = tukey),
                code = c(sprintf("fit <- aov(%s, data = df)", formula_string(form)), "summary(fit)", "TukeyHSD(fit)"))
}

analyse_welch_anova <- function(data, roles, options) {
  f <- complete_frame(data, c(roles$outcome, roles$group))
  need_numeric(f$data, roles$outcome); need_levels(f$data[[roles$group]], 2)
  need_variation(f$data, roles$outcome, "Welch ANOVA")
  need_group_sizes(f$data[[roles$group]], 2L)
  form <- formula_from(roles$outcome, roles$group)
  test <- stats::oneway.test(form, data = f$data, var.equal = FALSE)
  tests <- data.frame(Test = "Welch one-way ANOVA", Statistic = unname(test$statistic),
                      df1 = unname(test$parameter[1]), df2 = unname(test$parameter[2]), p = test$p.value)
  result_record("welch_anova", f$used, f$omitted, tests = tests,
                checks = check_row("Group variances", "May differ", "Welch's test permits unequal variances and unequal group sizes.", "ok"),
                plot_kind = "group_numeric", plot_data = f$data,
                extras = list(outcome = roles$outcome, group = roles$group),
                code = sprintf("oneway.test(%s, data = df, var.equal = FALSE)", formula_string(form)))
}

analyse_kruskal_wallis <- function(data, roles, options) {
  f <- complete_frame(data, c(roles$outcome, roles$group))
  need_numeric(f$data, roles$outcome); need_levels(f$data[[roles$group]], 2)
  need_variation(f$data, roles$outcome, "Kruskal-Wallis analysis")
  form <- formula_from(roles$outcome, roles$group)
  test <- stats::kruskal.test(form, data = f$data)
  tests <- data.frame(Test = "Kruskal-Wallis", Statistic = unname(test$statistic),
                      df = unname(test$parameter), p = test$p.value)
  result_record("kruskal_wallis", f$used, f$omitted, tests = tests,
                checks = check_row("Scale", "Ranks compared", "The test compares distributions through ranks.", "note"),
                plot_kind = "group_numeric", plot_data = f$data,
                extras = list(outcome = roles$outcome, group = roles$group),
                code = sprintf("kruskal.test(%s, data = df)", formula_string(form)))
}

# Repeated measures needs exactly one row per person per condition. Silently
# averaging duplicates would change the design without telling anyone, so a
# duplicated cell stops the analysis and names the problem instead.
analyse_repeated_anova <- function(data, roles, options) {
  f <- complete_frame(data, c(roles$outcome, roles$within, roles$id))
  need_numeric(f$data, roles$outcome); need_levels(f$data[[roles$within]], 2)
  need_variation(f$data, roles$outcome, "Repeated-measures ANOVA")
  need_repeated_cells(f$data, roles$id, roles$within)
  form_text <- sprintf("%s ~ %s + Error(%s/%s)", bt(roles$outcome), bt(roles$within), bt(roles$id), bt(roles$within))
  model <- stats::aov(stats::as.formula(form_text), data = f$data)
  sums <- summary(model)
  within_tab <- NULL
  for (piece in sums) {
    if (is.list(piece) && length(piece) && is.data.frame(piece[[1]]) && "F value" %in% names(piece[[1]])) within_tab <- piece[[1]]
  }
  if (is.null(within_tab)) stop("The repeated-measures table could not be formed. Check that each ID has observations in more than one condition.")
  tests <- data.frame(Test = "Repeated-measures ANOVA", Statistic = within_tab[["F value"]][1],
                      df1 = within_tab[["Df"]][1], df2 = within_tab[["Df"]][2], p = within_tab[["Pr(>F)"]][1])
  condition_count <- length(unique(f$data[[roles$within]]))
  sphericity_check <- if (condition_count <= 2L) {
    check_row("Sphericity", "Automatic with two conditions", "No sphericity correction is needed when there are only two repeated conditions.", "ok")
  } else {
    check_row("Sphericity", "Not estimated in this compact model", "For three or more conditions, confirm the result with a mixed model or a sphericity-corrected analysis.", "warn")
  }
  result_record("repeated_anova", f$used, f$omitted, tests = tests, model = model,
                checks = sphericity_check,
                plot_kind = "repeated", plot_data = f$data,
                extras = list(outcome = roles$outcome, within = roles$within, id = roles$id),
                code = c(sprintf("fit <- aov(%s, data = df)", form_text), "summary(fit)"))
}

# Tables and proportions --------------------------------------------------
analyse_chi_square <- function(data, roles, options) {
  f <- complete_frame(data, c(roles$row, roles$column))
  tab <- table(f$data[[roles$row]], f$data[[roles$column]])
  need_levels(f$data[[roles$row]], 2); need_levels(f$data[[roles$column]], 2)
  test <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  tests <- data.frame(Test = "Pearson chi-square", Statistic = unname(test$statistic),
                      df = unname(test$parameter), p = test$p.value)
  est <- data.frame(Effect = "Cramer's V", Estimate = cramers_v(tab, test$statistic))
  # The usual rule of thumb: the chi-square approximation is unreliable once
  # more than a fifth of expected counts fall below five. Reported rather than
  # enforced, because the reader may have a reason to proceed.
  low <- mean(test$expected < 5)
  checks <- check_row("Expected counts", if (low <= 0.2 && all(test$expected >= 1)) "Adequate" else "Some counts are small",
                      sprintf("%s of expected counts were below 5; the smallest was %s.", fmt_pct(low), fmt_num(min(test$expected), 2)),
                      if (low <= 0.2 && all(test$expected >= 1)) "ok" else "warn")
  result_record("chi_square", f$used, f$omitted, est, tests, checks = checks,
                plot_kind = "contingency", plot_data = as.data.frame(tab),
                extras = list(row = roles$row, column = roles$column, observed = as.data.frame.matrix(tab),
                              expected = as.data.frame.matrix(test$expected), residuals = as.data.frame.matrix(test$stdres)),
                code = sprintf("chisq.test(table(df[[\"%s\"]], df[[\"%s\"]]), correct = FALSE)", roles$row, roles$column))
}

# Fisher's exact test grows very quickly with table size, so a large table is
# refused rather than left to run for an unknown length of time. Monte Carlo
# simulation stays available for the cases where an exact answer is not needed.
analyse_fisher_exact <- function(data, roles, options) {
  f <- complete_frame(data, c(roles$row, roles$column))
  tab <- table(f$data[[roles$row]], f$data[[roles$column]])
  need_levels(f$data[[roles$row]], 2); need_levels(f$data[[roles$column]], 2)
  # Anything larger than 2 by 2 goes to Monte Carlo. The exact calculation on a
  # bigger table can take longer than a reader will wait, with no warning that
  # it is going to.
  simulated <- any(dim(tab) > 2)
  simulations <- as.integer(options$simulations %||% 10000L)
  if (is.na(simulations) || simulations < 2000L) stop("Use at least 2,000 Monte Carlo samples for a larger table.")
  test <- if (simulated) {
    with_local_seed(2026L, stats::fisher.test(tab, simulate.p.value = TRUE, B = simulations))
  } else {
    stats::fisher.test(tab, simulate.p.value = FALSE)
  }
  tests <- data.frame(Test = if (simulated) "Fisher exact, Monte Carlo" else "Fisher exact",
                      Statistic = if (!is.null(test$estimate)) unname(test$estimate) else NA_real_,
                      df = NA_real_, p = test$p.value)
  result_record("fisher_exact", f$used, f$omitted, tests = tests,
                checks = check_row("Small counts", "Exact method selected", if (simulated) "The p-value uses Monte Carlo simulation for a table larger than 2 by 2." else "The 2 by 2 p-value is exact.", "ok"),
                plot_kind = "contingency", plot_data = as.data.frame(tab),
                extras = list(row = roles$row, column = roles$column, observed = as.data.frame.matrix(tab)),
                code = c(if (simulated) "set.seed(2026)" else character(0),
                         sprintf("fisher.test(table(df[[\"%s\"]], df[[\"%s\"]]), simulate.p.value = %s%s)",
                                 roles$row, roles$column, simulated,
                                 if (simulated) sprintf(", B = %d", simulations) else "")))
}

analyse_mcnemar <- function(data, roles, options) {
  f <- complete_frame(data, c(roles$before, roles$after))
  need_levels(f$data[[roles$before]], 2, 2); need_levels(f$data[[roles$after]], 2, 2)
  # A paired binary test compares the same two response options measured twice.
  # Without this check the app happily crossed, say, smoking status against
  # employment status and reported a p-value for a table that means nothing.
  before_levels <- levels(as_group(f$data[[roles$before]]))
  after_levels <- levels(as_group(f$data[[roles$after]]))
  if (!setequal(before_levels, after_levels)) {
    stop(sprintf(
      "A paired binary test needs the same two response categories in both columns. The first column has %s and the second has %s.",
      paste(before_levels, collapse = " and "), paste(after_levels, collapse = " and ")
    ))
  }
  tab <- table(factor(f$data[[roles$before]], levels = before_levels),
               factor(f$data[[roles$after]], levels = before_levels))
  discordant <- tab[1, 2] + tab[2, 1]
  test <- suppressWarnings(stats::mcnemar.test(tab, correct = TRUE))
  tests <- data.frame(Test = "McNemar", Statistic = unname(test$statistic), df = unname(test$parameter), p = test$p.value)
  result_record("mcnemar", f$used, f$omitted, tests = tests,
                checks = rbind(
                  check_row("Pairing", "Rows treated as matched", "Each row must contain the two responses from the same case.", "note"),
                  check_row("Changed pairs", as.character(discordant), if (discordant > 0L) "McNemar's statistic uses only pairs whose two responses differ." else "No pair changed category, so the p-value is not informative.", if (discordant > 0L) "note" else "warn")
                ),
                plot_kind = "contingency", plot_data = as.data.frame(tab),
                extras = list(before = roles$before, after = roles$after, observed = as.data.frame.matrix(tab)),
                code = sprintf("mcnemar.test(table(df[[\"%s\"]], df[[\"%s\"]]))", roles$before, roles$after))
}

# Proportion intervals come from prop.test, which applies the Wilson score
# method. The Wald interval taught in most first courses behaves badly near
# zero and one, where it can produce a bound below zero or above one.
analyse_one_proportion <- function(data, roles, options) {
  f <- complete_frame(data, roles$outcome)
  binary <- binary01(f$data[[roles$outcome]])
  reference <- as.numeric(options$reference_proportion %||% 0.5)
  if (!is.finite(reference) || reference <= 0 || reference >= 1) stop("The reference proportion must be greater than 0 and smaller than 1.")
  events <- sum(binary$value)
  test <- suppressWarnings(stats::prop.test(events, f$used, p = reference,
                                             correct = isTRUE(options$continuity_correction),
                                             conf.level = confidence_level(options)))
  estimates <- data.frame(Category = binary$event, Proportion = events / f$used,
                          Difference = events / f$used - reference,
                          Lower = unname(test$conf.int[1]), Upper = unname(test$conf.int[2]),
                          Reference = reference)
  tests <- data.frame(Test = "One-sample proportion", Statistic = unname(test$statistic),
                      df = unname(test$parameter), p = test$p.value)
  checks <- check_row("Large-sample approximation", if (min(events, f$used - events) >= 5) "Counts are adequate" else "A count is below 5",
                      sprintf("The event and non-event counts were %d and %d.", events, f$used - events),
                      if (min(events, f$used - events) >= 5) "ok" else "warn")
  result_record("one_proportion", f$used, f$omitted, estimates, tests, checks = checks,
                plot_kind = "proportion", plot_data = estimates,
                extras = list(outcome = roles$outcome, event = binary$event, reference = reference),
                code = c(sprintf("complete <- !is.na(df[[\"%s\"]])", roles$outcome),
                         sprintf("outcome <- droplevels(factor(df[[\"%s\"]][complete]))", roles$outcome),
                         sprintf("prop.test(sum(outcome == levels(outcome)[2]), length(outcome), p = %s, correct = %s, conf.level = %s)",
                                 reference, isTRUE(options$continuity_correction), confidence_level(options))))
}

analyse_two_proportions <- function(data, roles, options) {
  f <- complete_frame(data, c(roles$outcome, roles$group))
  binary <- binary01(f$data[[roles$outcome]])
  group <- as_group(f$data[[roles$group]]); need_levels(group, 2, 2, "The group column")
  successes <- tapply(binary$value, group, sum)
  totals <- table(group)
  test <- suppressWarnings(stats::prop.test(successes, totals,
                                             correct = isTRUE(options$continuity_correction),
                                             conf.level = confidence_level(options)))
  proportions <- successes / totals
  estimates <- data.frame(Contrast = paste(levels(group), collapse = " minus "),
                          Estimate = unname(proportions[1] - proportions[2]),
                          Lower = unname(test$conf.int[1]), Upper = unname(test$conf.int[2]),
                          Event = binary$event)
  tests <- data.frame(Test = "Two-sample proportions", Statistic = unname(test$statistic),
                      df = unname(test$parameter), p = test$p.value)
  small <- min(successes, totals - successes) < 5
  checks <- check_row("Large-sample approximation", if (small) "A cell count is below 5" else "Counts are adequate",
                      paste(levels(group), sprintf("%d of %d events", successes, totals), collapse = "; "),
                      if (small) "warn" else "ok")
  plot_data <- data.frame(Group = factor(names(proportions), levels = names(proportions)), Proportion = unname(proportions))
  result_record("two_proportions", f$used, f$omitted, estimates, tests, checks = checks,
                plot_kind = "proportion", plot_data = plot_data,
                extras = list(outcome = roles$outcome, group = roles$group, event = binary$event, proportions = plot_data),
                code = c(sprintf("complete <- complete.cases(df[c(\"%s\", \"%s\")])", roles$outcome, roles$group),
                         sprintf("tab <- table(df[[\"%s\"]][complete], df[[\"%s\"]][complete])", roles$group, roles$outcome),
                         sprintf("prop.test(tab[, 2], rowSums(tab), correct = %s, conf.level = %s)",
                                 isTRUE(options$continuity_correction), confidence_level(options))))
}

# Type III sums of squares would need a contrast setting that most readers of
# this app would not know to check, so the engine reports the sequential Type I
# decomposition that aov produces and names it plainly in the output.
analyse_factorial_anova <- function(data, roles, options) {
  covariates <- roles$covariates %||% character(0)
  columns <- c(roles$outcome, roles$factors, covariates)
  f <- complete_frame(data, columns); need_numeric(f$data, c(roles$outcome, covariates))
  need_variation(f$data, roles$outcome, "Factorial ANOVA")
  for (factor_name in roles$factors) {
    f$data[[factor_name]] <- as_group(f$data[[factor_name]])
    need_levels(f$data[[factor_name]], 2, label = sprintf("Factor %s", factor_name))
  }
  main_terms <- c(roles$factors, covariates)
  # Only the first two factors get an interaction term. A full factorial across
  # four or five factors produces more terms than rows in most files this app
  # is used on, and none of them would be estimable.
  interaction <- if (isTRUE(options$factor_interaction) && length(roles$factors) >= 2) roles$factors[1:2] else NULL
  form <- formula_from(roles$outcome, main_terms, interaction = interaction)
  need_model_matrix(form, f$data, minimum_residual_df = 2L)
  model <- stats::lm(form, data = f$data)
  tab <- as.data.frame(stats::anova(model))
  tab$Term <- rownames(tab); rownames(tab) <- NULL
  names(tab)[1:5] <- c("df", "Sum_squares", "Mean_square", "Statistic", "p")
  residual_ss <- tab$Sum_squares[tab$Term == "Residuals"]
  tab$Partial_eta_squared <- ifelse(tab$Term == "Residuals", NA_real_, tab$Sum_squares / (tab$Sum_squares + residual_ss))
  checks <- normality_check(stats::residuals(model), "Model residuals")
  checks <- rbind(checks, check_row("Sums of squares", "Sequential", "For unbalanced data, each term is tested after the terms that appear before it in the model formula.", "note"))
  result_record("factorial_anova", f$used, f$omitted, estimates = tab, checks = checks, model = model,
                plot_kind = "group_numeric", plot_data = f$data,
                extras = list(outcome = roles$outcome, group = roles$factors[1], factors = roles$factors,
                              covariates = covariates, formula = formula_string(form)),
                code = c(sprintf("fit <- lm(%s, data = df)", formula_string(form)), "anova(fit)", "summary(fit)"))
}

analyse_friedman <- function(data, roles, options) {
  f <- complete_frame(data, c(roles$outcome, roles$within, roles$id))
  need_numeric(f$data, roles$outcome); need_levels(f$data[[roles$within]], 2)
  need_variation(f$data, roles$outcome, "Friedman analysis")
  need_repeated_cells(f$data, roles$id, roles$within)
  form <- stats::as.formula(sprintf("%s ~ %s | %s", bt(roles$outcome), bt(roles$within), bt(roles$id)))
  test <- stats::friedman.test(form, data = f$data)
  tests <- data.frame(Test = "Friedman", Statistic = unname(test$statistic),
                      df = unname(test$parameter), p = test$p.value)
  checks <- check_row("Complete blocks", "Required by the test", "Every retained case must have one observation in each repeated condition.", "note")
  result_record("friedman", f$used, f$omitted, tests = tests, checks = checks,
                plot_kind = "repeated", plot_data = f$data,
                extras = list(outcome = roles$outcome, within = roles$within, id = roles$id),
                code = sprintf("friedman.test(%s, data = df)", formula_string(form)))
}

# Association and prediction ----------------------------------------------
analyse_correlation <- function(data, roles, options) {
  variables <- roles$variables
  f <- complete_frame(data, variables); need_numeric(f$data, variables); need_n(f$used, 3)
  method <- options$cor_method %||% "pearson"
  if (!method %in% c("pearson", "spearman", "kendall")) stop("Choose Pearson, Spearman, or Kendall correlation.")
  constant <- variables[vapply(f$data, function(x) stats::sd(x) <= sqrt(.Machine$double.eps), logical(1))]
  if (length(constant)) stop(sprintf("Correlation needs variation in every selected column. Remove: %s.", paste(constant, collapse = ", ")))
  matrix <- stats::cor(f$data, method = method)
  pairs <- utils::combn(variables, 2, simplify = FALSE)
  rows <- lapply(pairs, function(pair) {
    test <- suppressWarnings(stats::cor.test(f$data[[pair[1]]], f$data[[pair[2]]], method = method,
                                             exact = FALSE, conf.level = confidence_level(options)))
    ci <- test$conf.int %||% c(NA_real_, NA_real_)
    data.frame(Variable_1 = pair[1], Variable_2 = pair[2], Correlation = unname(test$estimate),
               Lower = unname(ci[1]), Upper = unname(ci[2]), Statistic = unname(test$statistic), p = test$p.value)
  })
  estimates <- do.call(rbind, rows)
  result_record("correlation", f$used, f$omitted, estimates = estimates,
                checks = check_row("Interpretation", "Association only", "A correlation does not by itself establish direction or cause.", "warn"),
                plot_kind = "correlation", plot_data = matrix,
                extras = list(variables = variables, cor_method = method, matrix = matrix),
                code = sprintf("cor(df[c(%s)], use = 'complete.obs', method = '%s')", paste(sprintf("\"%s\"", variables), collapse = ", "), method))
}

# A partial correlation is the correlation of two sets of residuals, so it is
# computed that way here rather than through a matrix inversion. Doing it in
# the open means the degrees of freedom adjustment is visible and the exported
# script reproduces the same two regressions.
analyse_partial_correlation <- function(data, roles, options) {
  controls <- roles$controls
  columns <- c(roles$x, roles$y, controls)
  f <- complete_frame(data, columns); need_numeric(f$data, columns); need_n(f$used, length(controls) + 4)
  method <- options$cor_method %||% "pearson"
  if (!method %in% c("pearson", "spearman")) stop("Partial correlation offers Pearson or Spearman calculations.")
  constant <- columns[vapply(f$data, function(x) stats::sd(x) <= sqrt(.Machine$double.eps), logical(1))]
  if (length(constant)) stop(sprintf("Partial correlation needs variation in every selected column. Remove: %s.", paste(constant, collapse = ", ")))
  working <- f$data
  if (identical(method, "spearman")) {
    working[] <- lapply(working, rank, ties.method = "average")
  }
  rx <- stats::residuals(stats::lm(formula_from(roles$x, controls), data = working))
  ry <- stats::residuals(stats::lm(formula_from(roles$y, controls), data = working))
  estimate <- stats::cor(rx, ry)
  df <- f$used - length(controls) - 2L
  statistic <- if (abs(estimate) >= 1) sign(estimate) * Inf else estimate * sqrt(df / (1 - estimate^2))
  p <- 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE)
  level <- confidence_level(options)
  critical <- stats::qnorm(1 - (1 - level) / 2)
  bounded <- max(-1 + 1e-12, min(1 - 1e-12, estimate))
  z_error <- 1 / sqrt(f$used - length(controls) - 3L)
  ci <- tanh(atanh(bounded) + c(-1, 1) * critical * z_error)
  est <- data.frame(Contrast = paste(roles$x, "with", roles$y), Estimate = estimate,
                    Lower = ci[1], Upper = ci[2], Controls = paste(controls, collapse = ", "))
  tests <- data.frame(Test = paste("Partial", method, "correlation"), Statistic = statistic, df = df, p = p)
  method_note <- if (identical(method, "spearman")) "All selected columns were converted to ranks before the controls were removed." else "The calculation used the original numeric values."
  result_record("partial_correlation", f$used, f$omitted, est, tests,
                checks = rbind(
                  check_row("Calculation", paste("Partial", method), method_note, "note"),
                  check_row("Interpretation", "Adjusted association", "The result accounts for the selected controls, not unmeasured differences.", "warn")
                ),
                plot_kind = "partial", plot_data = data.frame(x = rx, y = ry),
                extras = list(x = roles$x, y = roles$y, controls = controls),
                code = c(sprintf("columns <- c(%s)", paste(sprintf("\"%s\"", columns), collapse = ", ")),
                         "working <- df[complete.cases(df[columns]), columns, drop = FALSE]",
                         if (identical(method, "spearman")) "working[] <- lapply(working, rank, ties.method = 'average')" else character(0),
                         sprintf("rx <- resid(lm(%s, data = working))", formula_string(formula_from(roles$x, controls))),
                         sprintf("ry <- resid(lm(%s, data = working))", formula_string(formula_from(roles$y, controls))),
                         sprintf("r <- cor(rx, ry) # df = %d", df)))
}

analyse_linear_regression <- function(data, roles, options) {
  columns <- c(roles$outcome, roles$predictors)
  f <- complete_frame(data, columns); need_numeric(f$data, roles$outcome)
  need_variation(f$data, roles$outcome, "Linear regression")
  need_n(f$used, length(roles$predictors) + 3)
  form <- formula_from(roles$outcome, roles$predictors)
  need_model_matrix(form, f$data, minimum_residual_df = 2L)
  model <- stats::lm(form, data = f$data)
  coefs <- tidy_coefs(model, level = confidence_level(options))
  sm <- summary(model)
  fit <- data.frame(Metric = c("R squared", "Adjusted R squared", "Residual SD", "AIC", "BIC"),
                    Value = c(sm$r.squared, sm$adj.r.squared, sm$sigma, stats::AIC(model), stats::BIC(model)))
  shapiro <- normality_check(stats::residuals(model), "Model residuals")
  # Cook's distance above one marks a row that is moving the fit by itself. It
  # is reported as something to look at rather than something to remove.
  max_cook <- max(stats::cooks.distance(model), na.rm = TRUE)
  checks <- rbind(shapiro, check_row("Unusual cases", if (max_cook < 1) "No Cook's distance above 1" else "A case may be highly consequential",
                                     sprintf("The largest Cook's distance was %s.", fmt_num(max_cook, 3)), if (max_cook < 1) "ok" else "warn"))
  result_record("linear_regression", f$used, f$omitted, coefs, fit = fit, checks = checks, model = model,
                plot_kind = "regression", plot_data = data.frame(Fitted = stats::fitted(model), Residual = stats::residuals(model)),
                extras = list(outcome = roles$outcome, predictors = roles$predictors, formula = formula_string(form)),
                code = c(sprintf("fit <- lm(%s, data = df)", formula_string(form)), "summary(fit)",
                         sprintf("confint(fit, level = %s)", confidence_level(options))))
}

# Both predictors are centered before the product is formed. Without centering
# the lower-order coefficients describe the outcome where the other predictor
# equals zero, which is often outside the observed range and occasionally
# impossible, and readers reliably misread them as main effects.
analyse_moderation <- function(data, roles, options) {
  controls <- roles$controls %||% character(0)
  columns <- c(roles$outcome, roles$focal, roles$moderator, controls)
  f <- complete_frame(data, columns); need_numeric(f$data, roles$outcome)
  need_variation(f$data, roles$outcome, "Moderation analysis")
  for (name in c(roles$focal, roles$moderator)) {
    if (is.numeric(f$data[[name]]) && stats::sd(f$data[[name]]) == 0) stop(sprintf("%s has no variation and cannot enter an interaction.", name))
    if (!is.numeric(f$data[[name]])) need_levels(f$data[[name]], 2)
  }

  # Numeric predictors in an interaction are mean-centered first. Without it the
  # lower-order coefficients describe the outcome where the other predictor is
  # zero, and zero horsepower or zero weight is not a car. Readers reliably read
  # those rows as main effects, so the model is fitted on values where that
  # reading is close to correct: each lower-order term is then the association
  # at the average of the other. The product term and its p-value are unchanged
  # by centering, so nothing about the moderation finding itself moves.
  centered <- character(0)
  centers <- stats::setNames(numeric(0), character(0))
  for (name in c(roles$focal, roles$moderator)) {
    if (is.numeric(f$data[[name]])) {
      centers[[name]] <- mean(f$data[[name]])
      f$data[[name]] <- f$data[[name]] - centers[[name]]
      centered <- c(centered, name)
    }
  }

  form <- formula_from(roles$outcome, controls, interaction = c(roles$focal, roles$moderator))
  need_model_matrix(form, f$data, minimum_residual_df = 2L)
  model <- stats::lm(form, data = f$data)
  coefs <- tidy_coefs(model, level = confidence_level(options))
  interaction_terms <- grep(":", coefs$Term)
  interaction <- if (length(interaction_terms)) coefs[interaction_terms, , drop = FALSE] else NULL
  sm <- summary(model)
  fit <- data.frame(Metric = c("R squared", "Adjusted R squared", "AIC"), Value = c(sm$r.squared, sm$adj.r.squared, stats::AIC(model)))
  centering_check <- if (length(centered)) {
    check_row("Centering", paste(centered, collapse = " and "),
              sprintf("Each was shifted so that its average became zero (%s). Each lower-order coefficient is therefore the association at the average of the other predictor.",
                      paste(sprintf("%s at %s", centered, fmt_num(centers[centered], 3)), collapse = "; ")),
              "note")
  } else {
    check_row("Centering", "Not applied", "Both interacting predictors are categorical, so there is nothing to center.", "note")
  }
  # The figure keeps the original values on its axes. Centering helps a reader
  # interpret a coefficient table and does nothing for a scatter plot, where a
  # shifted axis would only make the points harder to place.
  plot_frame <- f$data
  for (name in centered) plot_frame[[name]] <- plot_frame[[name]] + centers[[name]]

  result_record("moderation", f$used, f$omitted, coefs, fit = fit,
                checks = rbind(normality_check(stats::residuals(model), "Model residuals"),
                               centering_check,
                               check_row("Interaction", if (!is.null(interaction)) "Estimated" else "Not found", "Read the interaction before interpreting lower-order coefficients.", "note")),
                model = model, plot_kind = "moderation", plot_data = plot_frame,
                extras = list(outcome = roles$outcome, focal = roles$focal, moderator = roles$moderator,
                              controls = controls, interaction = interaction, formula = formula_string(form),
                              centered = centered, centers = centers),
                code = c(
                  vapply(centered, function(name) sprintf("df[[\"%s\"]] <- df[[\"%s\"]] - mean(df[[\"%s\"]], na.rm = TRUE)", name, name, name), character(1)),
                  sprintf("fit <- lm(%s, data = df)", formula_string(form)), "summary(fit)",
                  sprintf("confint(fit, level = %s)", confidence_level(options))))
}

# Coefficients are exponentiated before they are shown. A reader who is given
# a log odds has to do a transformation in their head before the number means
# anything, and the number they would arrive at is the one reported here.
analyse_logistic_regression <- function(data, roles, options) {
  columns <- c(roles$outcome, roles$predictors)
  f <- complete_frame(data, columns); need_n(f$used, length(roles$predictors) + 5)
  binary <- binary01(f$data[[roles$outcome]])
  f$data$.daybreak_outcome <- binary$value
  form <- formula_from(".daybreak_outcome", roles$predictors)
  need_model_matrix(form, f$data, minimum_residual_df = 2L)
  model <- stats::glm(form, data = f$data, family = stats::binomial())
  if (any(!is.finite(stats::coef(model)))) stop("The logistic model could not estimate every coefficient. Remove redundant predictors or categories with no events.")
  coefs <- tidy_coefs(model, exponentiate = TRUE, level = confidence_level(options))
  predicted <- stats::predict(model, type = "response")
  classified <- as.integer(predicted >= 0.5)
  accuracy <- mean(classified == binary$value)
  fit <- data.frame(Metric = c("AIC", "Null deviance", "Residual deviance", "Training accuracy at 0.50"),
                    Value = c(stats::AIC(model), model$null.deviance, model$deviance, accuracy))
  outcome_counts <- c(events = sum(binary$value), non_events = sum(binary$value == 0L))
  # Events per variable. Below about ten, logistic coefficients are biased away
  # from zero and their intervals are too narrow, which is the failure mode most
  # likely to produce a confident and wrong reading.
  count_per_coefficient <- min(outcome_counts) / length(stats::coef(model))
  extreme <- any(predicted < 1e-6 | predicted > 1 - 1e-6)
  checks <- rbind(
    check_row("Smaller outcome count per coefficient", if (count_per_coefficient >= 10) "At least 10" else "Below 10",
              sprintf("There were %d events, %d non-events, and %d fitted coefficients.", outcome_counts[1], outcome_counts[2], length(stats::coef(model))),
              if (count_per_coefficient >= 10) "ok" else "warn"),
    check_row("Near-perfect prediction", if (extreme) "Detected" else "Not detected",
              "Predicted probabilities extremely close to zero or one can signal separation or sparse categories.", if (extreme) "warn" else "ok")
  )
  result_record("logistic_regression", f$used, f$omitted, coefs, fit = fit, checks = checks, model = model,
                plot_kind = "logistic", plot_data = data.frame(Observed = binary$value, Predicted = predicted),
                extras = list(outcome = roles$outcome, predictors = roles$predictors, event = binary$event, formula = formula_string(form)),
                code = c(sprintf("df$.daybreak_outcome <- as.integer(factor(df[[\"%s\"]])) - 1", roles$outcome),
                         sprintf("fit <- glm(%s, data = df, family = binomial())", formula_string(form)),
                         sprintf("exp(cbind(odds_ratio = coef(fit), confint.default(fit, level = %s)))", confidence_level(options))))
}

analyse_poisson_regression <- function(data, roles, options) {
  columns <- c(roles$outcome, roles$predictors, roles$exposure_time %||% "")
  columns <- columns[nzchar(columns)]
  f <- complete_frame(data, columns); need_numeric(f$data, c(roles$outcome, if (nzchar(roles$exposure_time %||% "")) roles$exposure_time else character(0)))
  if (any(f$data[[roles$outcome]] < 0 | f$data[[roles$outcome]] %% 1 != 0)) stop("The count outcome must contain nonnegative whole numbers.")
  offset <- roles$exposure_time %||% ""
  if (nzchar(offset) && any(f$data[[offset]] <= 0)) stop("Exposure values must be greater than zero.")
  form <- formula_from(roles$outcome, roles$predictors, offset = if (nzchar(offset)) offset else NULL)
  need_model_matrix(form, f$data, minimum_residual_df = 2L)
  model <- stats::glm(form, data = f$data, family = stats::poisson())
  if (!isTRUE(model$converged) || any(!is.finite(stats::coef(model)))) stop("The Poisson model did not settle on finite estimates. Check sparse categories, all-zero counts, or overlapping predictors.")
  coefs <- tidy_coefs(model, exponentiate = TRUE, level = confidence_level(options))
  # Poisson assumes the variance equals the mean. When it does not, the
  # coefficients stay usable but the standard errors are too small, so the check
  # points at the negative binomial alternative rather than silently accepting.
  dispersion <- sum(stats::residuals(model, type = "pearson")^2) / stats::df.residual(model)
  fit <- data.frame(Metric = c("AIC", "Dispersion ratio"), Value = c(stats::AIC(model), dispersion))
  checks <- check_row("Dispersion", if (dispersion <= 1.5) "No large excess found" else "Overdispersion may matter",
                      sprintf("The Pearson dispersion ratio was %s.", fmt_num(dispersion, 2)), if (dispersion <= 1.5) "ok" else "warn")
  result_record("poisson_regression", f$used, f$omitted, coefs, fit = fit, checks = checks, model = model,
                plot_kind = "count_model", plot_data = data.frame(Observed = f$data[[roles$outcome]], Fitted = stats::fitted(model)),
                extras = list(outcome = roles$outcome, predictors = roles$predictors, exposure = offset, formula = formula_string(form)),
                code = c(sprintf("fit <- glm(%s, data = df, family = poisson())", formula_string(form)),
                         sprintf("exp(cbind(rate_ratio = coef(fit), confint.default(fit, level = %s)))", confidence_level(options))))
}
