# Deterministic statistical reporting -------------------------------------
#
# The reporting layer receives finished statistics. It cannot refit a model
# or replace a value, which keeps the prose tied to the displayed tables.

sentence_case <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

method_name <- function(id) ANALYSES[[id]]$name %||% id

interval_width_level <- function(result) {
  level <- suppressWarnings(as.numeric(result$conf_level %||% NA_real_))
  if (length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) 0.95 else level
}

# Methods whose coefficient table is already exponentiated. Their estimates sit
# on a ratio scale where 1 means no association, so size has to be judged by
# distance from 1 rather than distance from 0.
RATIO_METHODS <- c(
  "logistic_regression", "multinomial_regression", "ordinal_regression",
  "poisson_regression", "negative_binomial", "hlm_logistic", "cox_regression"
)

# Both writing layers headline one coefficient, and they have to headline the
# same one. The statistical description used to take the largest absolute
# estimate while the everyday explanation took the first listed term, so a
# logistic model could report an odds ratio of 27.3 in one paragraph and 0.9 in
# the next. Absolute size is also the wrong ruler on a ratio scale: an odds
# ratio of 0.1 is a stronger association than 1.5, but a smaller number. Taking
# the log first puts protective and elevated associations on equal footing.
primary_model_row <- function(result) {
  estimates <- result$estimates
  if (is.null(estimates) || !nrow(estimates) || is.null(estimates$Estimate)) return(NULL)
  terms <- as.character(estimates$Term %||% rep("", nrow(estimates)))
  keep <- which(!grepl("Intercept|threshold|\\|", terms, ignore.case = TRUE) &
                  is.finite(suppressWarnings(as.numeric(estimates$Estimate))))
  if (!length(keep)) keep <- seq_len(nrow(estimates))
  values <- suppressWarnings(as.numeric(estimates$Estimate[keep]))
  size <- if (result$method %in% RATIO_METHODS) abs(log(pmax(values, .Machine$double.eps))) else abs(values)
  size[!is.finite(size)] <- -Inf
  estimates[keep[which.max(size)], , drop = FALSE]
}

# The noun that belongs with an exponentiated coefficient for each model family.
ratio_noun <- function(method) {
  if (identical(method, "cox_regression")) return("hazard ratio")
  if (method %in% c("poisson_regression", "negative_binomial")) return("rate ratio")
  "odds ratio"
}

interval_sentence <- function(estimate, lower, upper, label = "estimate", level = 0.95) {
  if (any(is.na(c(estimate, lower, upper)))) {
    return(sprintf("The %s was %s.", label, fmt_num(estimate, 3)))
  }
  sprintf("The %s was %s, with a %s%% confidence interval from %s to %s.",
          label, fmt_num(estimate, 3), conf_percent(level), fmt_num(lower, 3), fmt_num(upper, 3))
}

# Where an interval is available it decides the wording, and the p-value is
# reported alongside rather than leading. An interval carries the size of the
# association as well as the evidence against the null, and a reader given only
# a p-value has to guess at the part that usually matters more.
evidence_sentence <- function(p, lower = NA_real_, upper = NA_real_, null = 0) {
  state <- p_state(p, lower = lower, upper = upper, null = null)
  if (state == "clear") {
    sprintf("The interval does not include %s, and the corresponding p-value was %s. Under the model and its assumptions, the data provide evidence of a difference or association in the estimated direction.", fmt_num(null, 0), fmt_p(p))
  } else if (state == "uncertain") {
    sprintf("The interval includes %s or the p-value was %s. These data do not separate the estimated direction clearly from sampling variation.", fmt_num(null, 0), fmt_p(p))
  } else {
    "The available output does not support a firm statement about sampling uncertainty."
  }
}

p_value_note <- function() {
  "A p-value describes how unusual the result would be under the null model and stated assumptions. It is not the probability that the null hypothesis is true, and it does not measure practical importance."
}

coverage_sentence <- function(result) {
  if (result$method == "descriptives") {
    return("Numeric summaries use the available values for each variable and group. The table reports its own complete and missing counts, so denominators may differ across rows.")
  }
  if (result$method == "frequencies") {
    return("Frequency rows state their observed counts and within-split percentages. Missing categories are displayed when present.")
  }
  if (result$method == "missingness") {
    return(sprintf("The profile inspected all %s rows across the selected columns.", format(result$n_used, big.mark = ",")))
  }
  if (result$n_omitted > 0) {
    sprintf("The analysis used %s complete rows and omitted %s rows because at least one required value was missing.",
            format(result$n_used, big.mark = ","), format(result$n_omitted, big.mark = ","))
  } else {
    sprintf("The analysis used %s complete rows; none were omitted for missing required values.",
            format(result$n_used, big.mark = ","))
  }
}

check_summary <- function(result) {
  if (is.null(result$checks) || !nrow(result$checks)) return("No automated diagnostic was available for this method.")
  warnings <- result$checks$Status == "warn"
  if (any(warnings)) {
    paste0("At least one diagnostic needs attention: ", paste(result$checks$Check[warnings], collapse = "; "),
           ". Read the Checks tab before reporting the estimate.")
  } else {
    "The automated checks did not flag a major problem. They cover only the conditions that can be read from the supplied data."
  }
}

primary_row <- function(result) {
  if (!is.null(result$estimates) && nrow(result$estimates)) result$estimates[1, , drop = FALSE] else NULL
}

primary_test <- function(result) {
  if (!is.null(result$tests) && nrow(result$tests)) result$tests[1, , drop = FALSE] else NULL
}

generic_inference <- function(result, estimate_label = "estimate") {
  e <- primary_row(result); t <- primary_test(result)
  estimate <- if (!is.null(e) && "Estimate" %in% names(e)) e$Estimate[1] else NA_real_
  lower <- if (!is.null(e) && "Lower" %in% names(e)) e$Lower[1] else NA_real_
  upper <- if (!is.null(e) && "Upper" %in% names(e)) e$Upper[1] else NA_real_
  p <- if (!is.null(t) && "p" %in% names(t)) t$p[1] else if (!is.null(e) && "p" %in% names(e)) e$p[1] else NA_real_
  c(interval_sentence(estimate, lower, upper, estimate_label, interval_width_level(result)),
    evidence_sentence(p, lower, upper))
}

# Descriptive methods get a shorter treatment: there is no inference to report,
# so the paragraph states what was summarized and how much was missing rather
# than reaching for language about evidence.
describe_narrative <- function(result) {
  if (result$method == "descriptives") {
    d <- result$estimates
    first <- d[1, ]
    lead <- sprintf("For %s%s, the mean was %s and the median was %s across %s complete values.",
                    first$Variable, if (first$Group != "All rows") paste0(" in ", first$Group) else "",
                    fmt_num(first$Mean), fmt_num(first$Median), format(first$N, big.mark = ","))
    detail <- sprintf("Values ranged from %s to %s. The standard deviation was %s and the interquartile range was %s.",
                      fmt_num(first$Minimum), fmt_num(first$Maximum), fmt_num(first$SD), fmt_num(first$IQR))
  } else if (result$method == "frequencies") {
    d <- result$estimates
    top <- d[which.max(d$Count), ]
    lead <- sprintf("The largest displayed category for %s was %s, with %s rows (%s within its split).",
                    top$Measure, as.character(top$Variable), format(top$Count, big.mark = ","), fmt_pct(top$Percent))
    detail <- "The full table reports every displayed count and denominator. Percentages should be read within the split shown in each row."
  } else {
    d <- result$estimates
    top <- d[which.max(d$Percent), ]
    lead <- sprintf("%s had the most missing data: %s values, or %s of rows.",
                    top$Variable, format(top$Missing, big.mark = ","), fmt_pct(top$Percent))
    detail <- "Missing-data patterns describe where values are absent. They do not reveal why values are missing or whether complete-case estimates are biased."
  }
  list(lead = lead, finding = detail, uncertainty = "These are sample descriptions. No population hypothesis was tested.")
}

comparison_narrative <- function(result) {
  if (result$method %in% c("one_sample_t", "independent_t", "paired_t")) {
    e <- result$estimates[1, ]; t <- result$tests[1, ]
    label <- tolower(as.character(e$Contrast))
    finding <- paste(interval_sentence(e$Estimate, e$Lower, e$Upper, label, interval_width_level(result)),
                     evidence_sentence(t$p, e$Lower, e$Upper))
    effect <- if (is.finite(e$Effect)) sprintf("The standardized difference was %s (%s).", fmt_num(e$Effect, 2), e$Effect_name) else ""
    return(list(lead = finding, finding = effect, uncertainty = p_value_note()))
  }
  if (result$method == "one_proportion") {
    e <- result$estimates[1, ]; t <- result$tests[1, ]
    lead <- sprintf("The observed %s proportion was %s, compared with a reference of %s. Its %s%% confidence interval ran from %s to %s.",
                    e$Category, fmt_pct(e$Proportion), fmt_pct(e$Reference), conf_percent(result$conf_level),
                    fmt_pct(e$Lower), fmt_pct(e$Upper))
    return(list(lead = lead, finding = evidence_sentence(t$p, e$Lower - e$Reference, e$Upper - e$Reference), uncertainty = p_value_note()))
  }
  if (result$method == "two_proportions") {
    e <- result$estimates[1, ]; t <- result$tests[1, ]
    lead <- paste(interval_sentence(e$Estimate, e$Lower, e$Upper, paste("proportion difference for", e$Event),
                                    interval_width_level(result)),
                  evidence_sentence(t$p, e$Lower, e$Upper))
    return(list(lead = lead, finding = "The group-specific proportions are shown in the figure and analysis metadata.", uncertainty = p_value_note()))
  }
  if (result$method %in% c("mann_whitney", "paired_wilcoxon")) {
    e <- result$estimates[1, ]; t <- result$tests[1, ]
    lead <- sprintf("The reported rank-based comparison had statistic %s and p %s.", fmt_num(t$Statistic, 2), fmt_p(t$p))
    finding <- sprintf("The displayed median contrast was %s. The hypothesis test concerns ranks and distributional ordering, so it should not be described as a test of means.", fmt_num(e$Estimate, 2))
    return(list(lead = lead, finding = finding, uncertainty = p_value_note()))
  }
  if (result$method == "factorial_anova") {
    tab <- result$estimates[result$estimates$Term != "Residuals", , drop = FALSE]
    row <- tab[which.min(tab$p), ]
    direction <- if (row$p < 0.05) "At least one fitted term was separated from sampling variation under the model." else "No fitted term was clearly separated from sampling variation under the model."
    return(list(lead = sprintf("%s The smallest term-level p-value was %s for %s.", direction, fmt_p(row$p), row$Term),
                finding = sprintf("The partial eta squared for that term was %s.", fmt_num(row$Partial_eta_squared, 3)),
                uncertainty = "Main effects are conditional when an interaction is present. Read the full term table before reporting any one coefficient."))
  }
  if (result$method %in% c("one_way_anova", "welch_anova", "kruskal_wallis", "repeated_anova", "friedman")) {
    t <- result$tests[1, ]
    direction <- if (t$p < 0.05) "The data provide evidence that at least one group or condition differs from another." else "The data do not clearly separate the groups or conditions at the omnibus level."
    extra <- if (result$method == "one_way_anova") sprintf("Omega squared was %s, an estimate of the share of outcome variation associated with group after bias correction.", fmt_num(result$estimates$Omega_squared[1], 3)) else ""
    return(list(lead = sprintf("%s The omnibus p-value was %s.", direction, fmt_p(t$p)), finding = extra,
                uncertainty = "An omnibus result does not identify which pairs differ. Read pairwise intervals when they are supplied, and account for repeated comparisons."))
  }
  if (result$method %in% c("chi_square", "fisher_exact", "mcnemar")) {
    t <- result$tests[1, ]
    lead <- if (t$p < 0.05) "The categorical pattern is difficult to reconcile with the null model of no association or no paired change." else "The categorical pattern is not clearly separated from the null model in this sample."
    effect <- if (result$method == "chi_square") sprintf("Cramer's V was %s. The p-value was %s.", fmt_num(result$estimates$Estimate[1], 3), fmt_p(t$p)) else sprintf("The p-value was %s.", fmt_p(t$p))
    return(list(lead = lead, finding = effect, uncertainty = p_value_note()))
  }
  list(lead = "The comparison finished.", finding = "Read the estimate and test tables together.", uncertainty = p_value_note())
}

relationship_narrative <- function(result) {
  if (result$method %in% c("correlation", "partial_correlation")) {
    e <- result$estimates[which.max(abs(result$estimates[[if (result$method == "correlation") "Correlation" else "Estimate"]])), ]
    value <- if (result$method == "correlation") e$Correlation else e$Estimate
    vars <- if (result$method == "correlation") paste(e$Variable_1, "and", e$Variable_2) else as.character(e$Contrast)
    lead <- sprintf("The strongest displayed association was between %s: r = %s.", vars, fmt_num(value, 3))
    finding <- if (value > 0) "Higher values on one variable tended to accompany higher values on the other." else "Higher values on one variable tended to accompany lower values on the other."
    return(list(lead = lead, finding = finding, uncertainty = "Association does not by itself establish direction, mechanism, or cause."))
  }
  if (result$method == "moderation") {
    e <- result$extras$interaction
    if (is.null(e) || !nrow(e)) return(list(lead = "No interaction coefficient was returned.", finding = "Review the model formula and coefficient table.", uncertainty = p_value_note()))
    row <- e[1, ]
    lead <- paste(interval_sentence(row$Estimate, row$Lower, row$Upper, "interaction coefficient",
                                    interval_width_level(result)),
                  evidence_sentence(row$p, row$Lower, row$Upper))
    return(list(lead = lead, finding = "An interaction describes how one fitted slope changes with another variable. Lower-order coefficients are conditional when the interaction is present.", uncertainty = p_value_note()))
  }
  if (result$method == "mediation") {
    e <- result$estimates[result$estimates$Left == "indirect", , drop = FALSE]
    if (!nrow(e)) e <- result$estimates[1, , drop = FALSE]
    lead <- paste(interval_sentence(e$Estimate[1], e$Lower[1], e$Upper[1], "indirect association",
                                    interval_width_level(result)),
                  evidence_sentence(e$p[1], e$Lower[1], e$Upper[1]))
    return(list(lead = lead, finding = "The direct, indirect, and total paths are listed together so the proposed decomposition can be read without treating one path in isolation.",
                uncertainty = "A mediation model does not by itself establish causal order. Study timing and control of shared causes must justify that reading."))
  }
  list(lead = "The relationship analysis finished.", finding = "Read the estimate table for direction and size.", uncertainty = p_value_note())
}

model_narrative <- function(result) {
  row <- primary_model_row(result)
  if (is.null(row)) {
    return(list(lead = "The model finished without a coefficient table.",
                finding = "Read the model-fit and check tables.",
                uncertainty = "Model conclusions depend on the stated form and assumptions."))
  }
  is_ratio <- result$method %in% RATIO_METHODS
  label <- if (is_ratio) ratio_noun(result$method) else "coefficient"
  lead <- paste(
    interval_sentence(row$Estimate[1], row$Lower[1], row$Upper[1],
                      paste(label, "for", row$Term[1]), interval_width_level(result)),
    evidence_sentence(row$p[1], row$Lower[1], row$Upper[1], null = if (is_ratio) 1 else 0)
  )
  finding <- if (is_ratio) {
    "Ratios above 1 indicate a higher modeled rate or odds; ratios below 1 indicate a lower modeled rate or odds, holding the other listed predictors constant."
  } else {
    "The coefficient gives the fitted outcome change for a one-unit predictor change, holding the other listed predictors constant."
  }
  list(lead = lead, finding = finding,
       uncertainty = "The model describes conditional associations. A causal reading needs a design that supports it, correct time order, and adequate control of shared causes.")
}

latent_narrative <- function(result) {
  if (result$method == "manova") {
    row <- result$estimates[result$estimates$Term != "Residuals", , drop = FALSE][1, ]
    lead <- sprintf("For %s, Pillai's trace was %s with p %s.", row$Term, fmt_num(row$Pillai, 3), fmt_p(row$p))
    finding <- if (row$p < 0.05) "The selected outcomes, considered together, differed across the fitted predictor levels or values." else "The selected outcomes, considered together, were not clearly separated across the fitted predictor levels or values."
    return(list(lead = lead, finding = finding,
                uncertainty = "The multivariate result should be read before outcome-specific tests. Follow-up tests require attention to repeated comparisons."))
  }
  if (result$method == "pca") {
    first <- result$estimates[1, ]
    retained <- min(result$extras$selected, nrow(result$estimates))
    return(list(lead = sprintf("The first principal component accounted for %s of standardized variation.", fmt_pct(first$Explained)),
                finding = sprintf("The first %d retained components together account for %s of variation.", retained, fmt_pct(result$estimates$Cumulative[retained])),
                uncertainty = "Components summarize variation in this sample. They are not automatically latent traits or causal variables."))
  }
  if (result$method %in% c("kmeans_clustering", "hierarchical_clustering")) {
    sizes <- result$estimates
    lead <- sprintf("The selected %d-cluster solution placed between %d and %d rows in each cluster.",
                    nrow(sizes), min(sizes$Rows), max(sizes$Rows))
    return(list(lead = lead,
                finding = "Cluster centers and the two-dimensional display describe the fitted grouping. The model used every selected standardized variable.",
                uncertainty = "Clusters are exploratory summaries. Their number and meaning require subject-matter judgment and should be checked in new data."))
  }
  if (result$method %in% c("dbscan_clustering", "hdbscan_clustering")) {
    sizes <- result$estimates
    cluster_rows <- sizes[as.character(sizes$Group) != "Noise", , drop = FALSE]
    noise <- sum(sizes$Rows[as.character(sizes$Group) == "Noise"])
    lead <- sprintf(
      "The density-based fit found %d groups and marked %d of %d rows as noise.",
      nrow(cluster_rows), noise, result$n_used
    )
    return(list(
      lead = lead,
      finding = "The two-dimensional display shows the fitted groups and noise rows. The grouping used every selected standardized variable.",
      uncertainty = "Density-based groups can change with settings, measurement scale, and new rows. Their subject-matter meaning requires outside judgment."
    ))
  }
  if (result$method == "factor_analysis") {
    return(list(lead = sprintf("The requested %d-factor solution was estimated with %s rotation.", result$extras$factors, result$extras$rotation),
                finding = "Loadings describe how closely each item relates to each fitted factor. Communalities show the share of item variation represented by the common factors.",
                uncertainty = check_summary(result)))
  }
  if (result$method == "reliability") {
    return(list(lead = sprintf("Coefficient alpha was %s across %d selected items.", fmt_num(result$extras$alpha, 3), length(result$extras$variables)),
                finding = "Item-rest correlations and alpha after removing each item are reported for diagnostic reading.",
                uncertainty = "Alpha is not evidence that a scale is one-dimensional or valid for its intended interpretation."))
  }
  cfi <- result$fit$Value[result$fit$Metric == "cfi"]
  rmsea <- result$fit$Value[result$fit$Metric == "rmsea"]
  lead <- sprintf("The model converged with CFI = %s and RMSEA = %s.", fmt_num(cfi, 3), fmt_num(rmsea, 3))
  list(lead = lead, finding = "Path estimates and their intervals are reported beside global fit indices. Both are needed to judge the proposed model.",
       uncertainty = "Fit indices are descriptive guides, not proof that the model is correct or uniquely supported by the data.")
}

time_narrative <- function(result) {
  if (result$method == "kaplan_meier") {
    lead <- if (!is.null(result$tests)) {
      sprintf("The log-rank comparison had p %s.", fmt_p(result$tests$p[1]))
    } else "The survival curve describes event-free probability over observed time."
    return(list(lead = lead, finding = "The step curve falls when observed events occur; censored cases contribute information through their last observed time.",
                uncertainty = "The estimates treat censoring as non-informative. Sparse later follow-up makes the tail of a survival curve less certain."))
  }
  if (result$method == "time_series") {
    return(list(lead = sprintf("An ARIMA(%s) model was fitted to %s ordered observations.", paste(result$extras$order, collapse = ","), result$n_used),
                finding = result$checks$Detail[1], uncertainty = "An adequate residual check does not show that forecasts will remain accurate after the observed period."))
  }
  model_narrative(result)
}

# Prediction results are described in terms of held-out performance only.
# Training accuracy is in the fit table for anyone who wants it, but leading
# with it would invite the reader to treat a memorized answer as a good one.
machine_learning_narrative <- function(result) {
  if (result$method %in% c("pca", "kmeans_clustering", "hierarchical_clustering", "dbscan_clustering", "hdbscan_clustering")) {
    return(latent_narrative(result))
  }
  metrics <- result$fit
  value <- function(name) metrics$Value[match(name, metrics$Metric)]
  if (result$method %in% c("classification_tree", "random_forest_classification", "nearest_neighbors", "elastic_net_classification", "naive_bayes")) {
    accuracy <- value("Accuracy")
    return(list(
      lead = sprintf("The model correctly classified %s of the held-out rows.", fmt_pct(accuracy)),
      finding = "The model learned from the training rows, then made category predictions for separate rows that were not used to fit it.",
      uncertainty = "This is one train-and-holdout split. Performance may move with a different split, new data, tuning choices, or changes in the population."
    ))
  }
  list(
    lead = sprintf("On the held-out rows, RMSE was %s and mean absolute error was %s outcome units.",
                   fmt_num(value("RMSE")), fmt_num(value("MAE"))),
    finding = "The observed-versus-predicted figure shows whether larger and smaller outcomes were tracked across rows that were not used to fit the model.",
    uncertainty = "This is one train-and-holdout split. Prediction error may change with a different split, new data, tuning choices, or changes in the population."
  )
}

# The statistical description ---------------------------------------------
#
# This layer writes for a reader who knows what an interval is. The everyday
# layer in explanations.R writes for one who does not. Both read the same
# result record, and where they describe the same quantity they have to select
# it the same way, which is why primary_model_row lives here and is called from
# both.
build_narrative <- function(result) {
  spec <- ANALYSES[[result$method]]
  family <- spec$family
  body <- if (identical(spec$kind, "machine_learning")) machine_learning_narrative(result) else if (family == "Describe") describe_narrative(result) else if (family == "Compare groups") comparison_narrative(result) else if (family == "Study relationships") relationship_narrative(result) else if (family %in% c("Build a model", "Study clustered or repeated data")) model_narrative(result) else if (family %in% c("Study several outcomes", "Study scales and latent variables")) latent_narrative(result) else time_narrative(result)
  list(
    title = method_name(result$method),
    headline = body$lead,
    cards = list(
      list(title = "What the numbers say", body = body$finding, tone = "finding"),
      list(title = "How much data entered", body = coverage_sentence(result), tone = "data"),
      list(title = "How cautiously to read it", body = body$uncertainty, tone = "caution"),
      list(title = "Checks and limits", body = check_summary(result), tone = if (!is.null(result$checks) && any(result$checks$Status == "warn")) "warning" else "check")
    ),
    warnings = result$warnings
  )
}

narrative_text <- function(narrative) {
  c(narrative$title, "", narrative$headline, "",
    unlist(lapply(narrative$cards, function(x) c(x$title, x$body, ""))),
    if (length(narrative$warnings)) c("Additional notes", paste0("- ", narrative$warnings)) else character(0))
}
