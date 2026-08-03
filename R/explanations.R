# Everyday result explanations -------------------------------------------
#
# This layer reads completed output only. It uses short sentences, familiar
# comparisons, and an explicit boundary so a first-time reader can separate
# the observed pattern from what the analysis cannot establish.

first_value <- function(data, name, default = NA) {
  if (is.null(data) || !nrow(data) || !name %in% names(data)) return(default)
  data[[name]][1]
}

# The estimate tables differ column by column between methods, so these small
# accessors keep the branches below readable and stop a missing or non-finite
# value from turning into an error in the middle of a sentence.
first_finite <- function(...) {
  values <- suppressWarnings(as.numeric(unlist(list(...))))
  values <- values[is.finite(values)]
  if (length(values)) values[1] else NA_real_
}

# Numbers are rounded for reading rather than for precision here. The exact
# values are one tab away in the Exact values panel, and a sentence carrying
# six significant figures is a sentence that does not get read.
# A zero-length argument is handled the same way a missing one is. Extras can
# be absent when a session written by an earlier release is reopened, and a
# sentence that says the value is unavailable is far better than an error that
# takes the whole explanation down with it.
plain_number <- function(x, digits = 2) {
  if (length(x) != 1L || !is.finite(x)) return("an unavailable value")
  format(round(x, digits), trim = TRUE, scientific = FALSE, big.mark = ",")
}

# Accepts either a proportion or an already-scaled percentage, because the
# result tables carry both depending on which method produced them.
plain_percent <- function(x, digits = 1) {
  if (length(x) != 1L || !is.finite(x)) return("an unavailable percentage")
  value <- if (abs(x) <= 1) x * 100 else x
  paste0(format(round(value, digits), trim = TRUE, scientific = FALSE), "%")
}

quoted_name <- function(x, fallback = "the selected column") {
  x <- as.character(x %||% "")
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (!length(x)) return(fallback)
  paste0("\u201c", x[[1]], "\u201d")
}

plain_list <- function(x, limit = 6L) {
  x <- unique(as.character(x %||% character(0)))
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (!length(x)) return("")
  if (length(x) > limit) x <- c(head(x, limit), sprintf("%s more", length(x) - limit))
  x <- vapply(x, quoted_name, character(1))
  if (length(x) == 1L) return(x)
  if (length(x) == 2L) return(paste(x, collapse = " and "))
  paste0(paste(head(x, -1L), collapse = ", "), ", and ", tail(x, 1L))
}

join_words <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return("")
  if (length(x) == 1L) return(x)
  if (length(x) == 2L) return(paste(x, collapse = " and "))
  paste0(paste(head(x, -1L), collapse = ", "), ", and ", tail(x, 1L))
}

observed_levels <- function(result, column) {
  data <- result$plot_data
  if (is.null(data) || !is.data.frame(data) || !column %in% names(data)) return(character(0))
  as.character(unique(stats::na.omit(data[[column]])))
}

selected_context <- function(result) {
  e <- result$extras %||% list()
  id <- result$method

  if (id %in% c("descriptives", "frequencies", "missingness", "correlation", "pca", "kmeans_clustering",
                "hierarchical_clustering", "dbscan_clustering", "hdbscan_clustering", "factor_analysis", "reliability")) {
    variables <- e$variables %||% character(0)
    return(sprintf("This answer uses these columns: %s.", plain_list(variables)))
  }
  if (id %in% c("paired_t", "paired_wilcoxon", "mcnemar")) {
    return(sprintf("Each row has both %s and %s, so the answer compares the two values within the same row.", quoted_name(e$before), quoted_name(e$after)))
  }
  if (id %in% c("chi_square", "fisher_exact")) {
    return(sprintf("This answer compares the category combinations formed by %s and %s.", quoted_name(e$row), quoted_name(e$column)))
  }
  if (id %in% c("one_way_anova", "welch_anova", "kruskal_wallis", "independent_t", "mann_whitney", "two_proportions")) {
    levels <- observed_levels(result, e$group)
    return(sprintf("This answer compares %s across the groups in %s%s.", quoted_name(e$outcome), quoted_name(e$group),
                   if (length(levels)) paste0(": ", plain_list(levels)) else ""))
  }
  if (id %in% c("repeated_anova", "friedman")) {
    levels <- observed_levels(result, e$within)
    return(sprintf("This answer follows %s across %s%s. Repeated rows are matched through %s.",
                   quoted_name(e$outcome), quoted_name(e$within),
                   if (length(levels)) paste0(": ", plain_list(levels)) else "", quoted_name(e$id)))
  }
  if (id == "factorial_anova") {
    return(sprintf("This answer compares %s using %s%s.", quoted_name(e$outcome), plain_list(e$factors),
                   if (length(e$covariates %||% character(0))) paste0(", with ", plain_list(e$covariates), " as numeric covariates") else ""))
  }
  if (id == "partial_correlation") {
    return(sprintf("This answer looks at how %s and %s move together after taking %s into account.",
                   quoted_name(e$x), quoted_name(e$y), plain_list(e$controls)))
  }
  if (id == "moderation") {
    return(sprintf("This answer asks whether the link between %s and %s is different at different values of %s.",
                   quoted_name(e$focal), quoted_name(e$outcome), quoted_name(e$moderator)))
  }
  if (id == "mediation") {
    return(sprintf("This answer checks a proposed route from %s through %s to %s.",
                   quoted_name(e$exposure), quoted_name(e$mediator), quoted_name(e$outcome)))
  }
  if (id == "manova") {
    return(sprintf("This answer considers %s together and compares them using %s.", plain_list(e$outcomes), plain_list(e$predictors)))
  }
  if (id %in% c("kaplan_meier", "cox_regression")) {
    return(sprintf("This answer uses %s for time and %s to show whether the event occurred%s.",
                   quoted_name(e$time), quoted_name(e$status),
                   if (length(e$group %||% "") && nzchar(e$group %||% "")) paste0(", split by ", quoted_name(e$group)) else ""))
  }
  if (id == "time_series") {
    return(sprintf("This answer follows %s in the order given by %s.", quoted_name(e$outcome), quoted_name(e$time)))
  }
  if (id %in% c("cfa", "sem")) {
    path_names <- unique(c(as.character(result$estimates$Left %||% character(0)),
                           as.character(result$estimates$Right %||% character(0))))
    path_names <- path_names[!is.na(path_names) & nzchar(path_names) & !grepl("^[0-9.]+$", path_names)]
    if (length(path_names)) {
      return(sprintf("This answer checks the proposed links among %s.", plain_list(path_names)))
    }
    return("This answer checks the measurement and path model entered for the active data.")
  }
  if (!is.null(e$outcome)) {
    predictors <- e$predictors %||% e$variables %||% character(0)
    return(sprintf("The value being explained is %s%s.", quoted_name(e$outcome),
                   if (length(predictors)) paste0(". The other columns in the model are ", plain_list(predictors)) else ""))
  }
  sprintf("Method: %s; rows used: %s.", ANALYSES[[id]]$name, format(result$n_used, big.mark = ","))
}

group_summary <- function(result, statistic = c("mean", "median")) {
  statistic <- match.arg(statistic)
  e <- result$extras %||% list()
  data <- result$plot_data
  if (is.null(data) || !is.data.frame(data) || !all(c(e$outcome, e$group) %in% names(data))) return(NULL)
  values <- suppressWarnings(as.numeric(data[[e$outcome]]))
  groups <- as.character(data[[e$group]])
  keep <- is.finite(values) & !is.na(groups)
  if (!any(keep)) return(NULL)
  fun <- if (statistic == "mean") mean else stats::median
  summaries <- tapply(values[keep], groups[keep], fun)
  counts <- table(groups[keep])
  summaries <- summaries[is.finite(summaries)]
  if (!length(summaries)) return(NULL)
  list(values = summaries, counts = counts[names(summaries)], high = names(which.max(summaries)),
       low = names(which.min(summaries)), statistic = statistic)
}

group_summary_sentence <- function(result, statistic = c("mean", "median")) {
  summary <- group_summary(result, match.arg(statistic))
  if (is.null(summary)) return("")
  pieces <- sprintf("%s for %s", vapply(summary$values, plain_number, character(1)),
                    vapply(names(summary$values), quoted_name, character(1)))
  label <- if (summary$statistic == "mean") "average" else "middle value"
  span <- if (length(summary$values) > 1L && !identical(summary$low, summary$high)) {
    sprintf(" %s had the lowest %s, and %s had the highest.",
            quoted_name(summary$low), label, quoted_name(summary$high))
  } else {
    ""
  }
  paste0(sprintf("The %s for %s was %s.", label, quoted_name(result$extras$outcome), join_words(pieces)), span)
}

anova_followup_sentence <- function(result) {
  pairwise <- result$extras$pairwise
  if (is.null(pairwise) || !nrow(pairwise) || !"Adjusted_p" %in% names(pairwise)) return("")
  clear <- as.character(pairwise$Contrast[is.finite(pairwise$Adjusted_p) & pairwise$Adjusted_p < 0.05])
  if (!length(clear)) return("Daybreak also checked every pair of groups. None of those pair checks was clear after accounting for the number of checks.")
  clear <- sub("-", " and ", clear, fixed = TRUE)
  sprintf("Daybreak also checked every pair of groups. After accounting for the number of checks, it found clear gaps between %s.", join_words(clear))
}

# Turns the evidence into one of four plain readings: clear, leaning, unclear,
# or no evidence. Four levels rather than a threshold, because a reader handed
# a single yes or no will treat it as a verdict.
plain_signal <- function(result, null = 0) {
  estimate <- first_finite(first_value(result$estimates, "Estimate"))
  lower <- first_finite(first_value(result$estimates, "Lower"))
  upper <- first_finite(first_value(result$estimates, "Upper"))
  p <- first_finite(first_value(result$tests, "p"), first_value(result$estimates, "p"))
  p_state(p, estimate, lower, upper, null)
}

plain_certainty_state <- function(signal, descriptive = FALSE) {
  if (isTRUE(descriptive)) {
    return("These numbers are exact for the rows in this file. A different set of rows could have a different average, shape, or category mix.")
  }
  if (identical(signal, "clear")) {
    return("Random samples do not come out exactly alike. Even so, a result this strong would be unusual if there were no pattern beyond this sample. That supports a difference or relationship, but it is not proof.")
  }
  if (identical(signal, "uncertain")) {
    return("The rows show a possible pattern, but ordinary differences from one random sample to another could produce something like it. This result does not give a clear yes-or-no answer.")
  }
  "The output does not contain enough information for a simple statement about random sampling. Read the statistical description and exact table before drawing a conclusion."
}

plain_certainty <- function(result, null = 0, descriptive = FALSE) {
  plain_certainty_state(plain_signal(result, null), descriptive)
}

# The certainty paragraph never says a result is proven or disproven. It
# describes what the interval covers and leaves the judgment where it belongs,
# because the reader knows what would count as a meaningful difference in their
# setting and the app does not.
plain_certainty_row <- function(row, null = 0) {
  signal <- p_state(
    first_finite(first_value(row, "p")),
    first_finite(first_value(row, "Estimate")),
    first_finite(first_value(row, "Lower")),
    first_finite(first_value(row, "Upper")),
    null
  )
  plain_certainty_state(signal)
}

plain_data_note <- function(result) {
  used <- result$n_used %||% NA_integer_
  omitted <- result$n_omitted %||% 0L
  if (!is.finite(used)) return("The number of rows used was not reported.")
  if (result$method == "descriptives") {
    return(sprintf("Daybreak inspected %s rows. Each numeric summary uses the available values for that column and reports its own missing count.", format(used, big.mark = ",")))
  }
  if (result$method == "frequencies") {
    return(sprintf("Daybreak counted %s rows. Missing categories are included when they appear in the displayed table.", format(used, big.mark = ",")))
  }
  if (result$method == "missingness") {
    return(sprintf("Daybreak checked the selected columns across all %s rows.", format(used, big.mark = ",")))
  }
  if (omitted > 0) {
    sprintf("Daybreak used %s rows. It left out %s rows because a value needed by this analysis was missing.",
            format(used, big.mark = ","), format(omitted, big.mark = ","))
  } else {
    sprintf("Daybreak used %s rows. No rows were left out for missing required values.",
            format(used, big.mark = ","))
  }
}

# A coefficient table mixes two kinds of row. A numeric predictor keeps its
# own name, so "rises by one unit" is the right description. A categorical
# predictor is split into dummy columns named for the level, such as
# GenderFemale, and telling a first-time reader that GenderFemale rises by one
# unit is meaningless. Any term that is not itself a selected column is treated
# as a category contrast against the level the model held back.
predictor_shift_words <- function(term, predictors) {
  term <- as.character(term)[1]
  if (length(predictors) && term %in% predictors) {
    return(list(
      kind = "numeric",
      clause = sprintf("when %s rises by one unit", quoted_name(term)),
      picture = sprintf("raising %s by one unit", quoted_name(term)),
      trigger = "that predictor rises by one unit"
    ))
  }
  base <- predictors[vapply(predictors, function(p) startsWith(term, p), logical(1))]
  level <- if (length(base)) trimws(sub(paste0("^", base[which.max(nchar(base))]), "", term)) else ""
  named <- if (nzchar(level)) quoted_name(level) else quoted_name(term)
  list(
    kind = "category",
    clause = sprintf("for rows in the %s category rather than the category the model held back for comparison", named),
    picture = sprintf("moving from the held-back category to %s", named),
    trigger = sprintf("a row is in the %s category instead of the held-back category", named)
  )
}

# A ratio can be described two ways, and which one is readable depends on its
# size. Near 1, a percentage is natural: an odds ratio of 1.2 is twenty percent
# higher. Far from 1 it stops being natural, and "about 2,628.9% higher" is a
# sentence nobody can picture. Past a doubling in either direction the multiple
# is the clearer form.
ratio_words <- function(value, noun, shift = NULL) {
  if (!is.finite(value) || value <= 0) return(sprintf("Daybreak could not summarize the model-based %s.", noun))
  trigger <- shift$trigger %||% "that predictor rises by one unit"
  size <- if (value >= 2) {
    sprintf("about %s times what it is otherwise", plain_number(value, 1))
  } else if (value <= 0.5) {
    sprintf("roughly %s of what it is otherwise", plain_fraction(value))
  } else if (value >= 1) {
    sprintf("about %s%% higher than it is otherwise", plain_number((value - 1) * 100, 1))
  } else {
    sprintf("about %s%% lower than it is otherwise", plain_number((1 - value) * 100, 1))
  }
  sprintf("Where %s, and the other listed columns stay the same, the model puts the %s at %s.",
          trigger, noun, size)
}

# "one half", "one third", and so on, for describing a ratio below 0.5 in the
# way a reader is most likely to already think about it.
plain_fraction <- function(value) {
  denominator <- round(1 / value)
  names <- c("one half", "one third", "one quarter", "one fifth", "one sixth",
             "one seventh", "one eighth", "one ninth", "one tenth")
  if (denominator >= 2 && denominator <= 10) names[denominator - 1] else sprintf("one %sth", denominator)
}

# One branch per method family. Each returns the same four pieces so the layout
# never changes between results, and a reader who has learned where the limits
# paragraph sits will find it in the same place next time.
accessible_core <- function(result) {
  id <- result$method
  estimates <- result$estimates
  tests <- result$tests
  boundary <- "This analysis describes a pattern in these data. By itself, it cannot show that one variable caused another."

  if (id %in% c("classification_tree", "random_forest_classification", "nearest_neighbors", "elastic_net_classification", "naive_bayes")) {
    accuracy <- first_finite(result$fit$Value[result$fit$Metric == "Accuracy"])
    outcome <- quoted_name(result$extras$outcome)
    return(list(
      bottom_line = sprintf("Daybreak tested the model on rows it had not studied. It placed about %s out of every 100 into the correct %s category.", plain_number(accuracy * 100, 0), outcome),
      picture = sprintf("The model used %s to guess %s. The figure places each guess beside the category that was actually recorded.", plain_list(result$extras$predictors), outcome),
      certainty = "The file was split once into rows for learning and rows for testing. A different split or a new dataset could produce a different score.",
      boundary = "A correct guess does not mean that the predictor columns caused the outcome. The score may also change for people or settings unlike those in this file."
    ))
  }

  if (id %in% c("regression_tree", "random_forest_regression", "elastic_net_regression")) {
    mae <- first_finite(result$fit$Value[result$fit$Metric == "MAE"])
    rmse <- first_finite(result$fit$Value[result$fit$Metric == "RMSE"])
    return(list(
      bottom_line = sprintf("On rows the model had not studied, its predictions for %s were off by %s units on average. A second error score that gives more weight to large misses was %s.", quoted_name(result$extras$outcome), plain_number(mae), plain_number(rmse)),
      picture = sprintf("The model used %s to guess %s. In the figure, a point closer to the diagonal line is a closer guess.", plain_list(result$extras$predictors), quoted_name(result$extras$outcome)),
      certainty = "The file was split once into rows for learning and rows for testing. A different split or a new dataset could produce different errors.",
      boundary = "Small prediction errors do not mean that the predictor columns caused the outcome. The errors may also change for people or settings unlike those in this file."
    ))
  }

  if (id == "descriptives") {
    row <- estimates[1, , drop = FALSE]
    return(list(
      bottom_line = sprintf("For %s, adding the values and dividing by the number of rows gives an average of %s. The value in the middle of the sorted list is %s.",
                            quoted_name(row$Variable), plain_number(row$Mean), plain_number(row$Median)),
      picture = sprintf("The values run from %s to %s. If they were placed from lowest to highest, the middle half would cover about %s units.",
                        plain_number(row$Minimum), plain_number(row$Maximum), plain_number(row$IQR)),
      certainty = plain_certainty(result, descriptive = TRUE),
      boundary = "This summary describes the supplied rows. It does not compare a population with a reference or explain why the values differ."
    ))
  }

  if (id == "frequencies") {
    row <- estimates[which.max(estimates$Count), , drop = FALSE]
    return(list(
      bottom_line = sprintf("The category that appears most often is %s. It appears in %s rows.", quoted_name(row$Variable), format(row$Count, big.mark = ",")),
      picture = sprintf("That category makes up about %s of the relevant rows. That is roughly %s out of every 100 rows.",
                        plain_percent(row$Percent), plain_number(row$Percent * 100, 0)),
      certainty = plain_certainty(result, descriptive = TRUE),
      boundary = "A common category is not necessarily a good, typical, or preferred outcome. The table only counts what appears in the data."
    ))
  }

  if (id == "missingness") {
    row <- estimates[which.max(estimates$Percent), , drop = FALSE]
    return(list(
      bottom_line = sprintf("%s has the most blank cells: %s values are missing.", quoted_name(row$Variable), format(row$Missing, big.mark = ",")),
      picture = sprintf("About %s of this column is blank. In a group of 100 rows, that would be about %s blank cells.",
                        plain_percent(row$Percent), plain_number(row$Percent * 100, 0)),
      certainty = "The count is exact for the supplied file. The reason those values are absent cannot be read from the blank cells alone.",
      boundary = "A missing-data profile cannot tell whether the missing rows would change the answer. That depends on why the values are absent."
    ))
  }

  if (id %in% c("one_sample_t", "independent_t", "paired_t")) {
    difference <- first_finite(first_value(estimates, "Estimate"))
    direction <- if (difference >= 0) "higher" else "lower"
    comparison <- if (id == "one_sample_t") {
      sprintf("The sample average of %s", quoted_name(result$extras$outcome))
    } else if (id == "paired_t") {
      sprintf("The later measurement, %s", quoted_name(result$extras$after))
    } else {
      sprintf("The average of %s for %s", quoted_name(result$extras$outcome), quoted_name(result$extras$levels[1]))
    }
    reference <- if (id == "one_sample_t") {
      sprintf("the reference value of %s", plain_number(result$extras$reference))
    } else if (id == "paired_t") {
      sprintf("the earlier measurement, %s", quoted_name(result$extras$before))
    } else {
      sprintf("the average for %s", quoted_name(result$extras$levels[2]))
    }
    return(list(
      bottom_line = sprintf("%s is about %s units %s than %s.", comparison, plain_number(abs(difference)), direction, reference),
      picture = "Picture the two averages as marks on a number line. The gap between the marks is the estimated difference. The interval in the exact table shows a range of differences that fit these data.",
      certainty = plain_certainty(result),
      boundary = "A difference in averages does not mean that every person or row in one group is above every person or row in the other."
    ))
  }

  if (id %in% c("one_proportion", "two_proportions")) {
    value <- if (id == "one_proportion") first_finite(first_value(estimates, "Proportion")) else first_finite(first_value(estimates, "Estimate"))
    signal <- if (id == "one_proportion") {
      reference <- first_finite(first_value(estimates, "Reference"))
      p_state(first_finite(first_value(tests, "p")), value - reference,
              first_finite(first_value(estimates, "Lower")) - reference,
              first_finite(first_value(estimates, "Upper")) - reference, 0)
    } else {
      plain_signal(result)
    }
    return(list(
      bottom_line = if (id == "one_proportion") sprintf("%s appears in about %s of the %s rows.", quoted_name(result$extras$event), plain_percent(value), quoted_name(result$extras$outcome)) else sprintf("For %s in %s, the two %s groups are about %s percentage points apart.", quoted_name(result$extras$event), quoted_name(result$extras$outcome), quoted_name(result$extras$group), plain_number(abs(value) * 100, 1)),
      picture = if (id == "one_proportion") sprintf("Out of 100 rows, about %s would show %s at the rate found in this file.", plain_number(value * 100, 0), quoted_name(result$extras$event)) else sprintf("Picture 100 rows from each %s group. Count how many show %s in each set. The difference between those two counts is the percentage-point gap.", quoted_name(result$extras$group), quoted_name(result$extras$event)),
      certainty = plain_certainty_state(signal),
      boundary = "The observed percentage does not predict what will happen to a particular person or row."
    ))
  }

  if (id %in% c("mann_whitney", "paired_wilcoxon", "kruskal_wallis", "friedman")) {
    is_grouped <- id %in% c("mann_whitney", "kruskal_wallis")
    observed <- if (is_grouped) group_summary_sentence(result, "median") else ""
    subject <- if (is_grouped) sprintf("%s across %s", quoted_name(result$extras$outcome), quoted_name(result$extras$group)) else sprintf("%s and %s", quoted_name(result$extras$before %||% result$extras$outcome), quoted_name(result$extras$after %||% result$extras$within))
    return(list(
      bottom_line = paste(if (plain_signal(result) == "clear") sprintf("When the values for %s are put in order from lowest to highest, some groups or conditions tend to sit higher in the list than others.", subject) else sprintf("When the values for %s are put in order from lowest to highest, the groups or conditions do not separate clearly in this sample.", subject), observed),
      picture = sprintf("Imagine sorting every %s value from lowest to highest and replacing it with its place in that list. The figure shows which group or condition tends to receive the higher places.", quoted_name(result$extras$outcome %||% result$extras$after)),
      certainty = plain_certainty(result),
      boundary = "A rank comparison is about ordering. It is not a direct comparison of group averages."
    ))
  }

  if (id %in% c("one_way_anova", "welch_anova", "repeated_anova", "factorial_anova")) {
    if (id %in% c("one_way_anova", "welch_anova")) {
      summary <- group_summary_sentence(result, "mean")
      followup <- if (id == "one_way_anova") anova_followup_sentence(result) else ""
      return(list(
        bottom_line = paste(
          summary,
          if (plain_signal(result) == "clear") {
            sprintf("The averages are far enough apart that a gap this large would be unusual if the %s groups beyond this sample really had the same average. The result says that at least two of the groups differ.", quoted_name(result$extras$group))
          } else {
            sprintf("The averages are not identical, but the gaps are small enough that ordinary random sampling could explain them. This result does not show a clear difference among the %s groups.", quoted_name(result$extras$group))
          },
          followup
        ),
        picture = sprintf("Each colored shape is one %s group. Higher dots mean a larger %s value. The pale dots are individual rows, and the box shows where the middle half of each group falls. Compare the box heights, then look at how much the dots overlap.", quoted_name(result$extras$group), quoted_name(result$extras$outcome)),
        certainty = if (plain_signal(result) == "clear") {
          sprintf("A different set of rows would give slightly different averages. Even after allowing for that normal sample-to-sample change, the gaps among the %s groups are clear here. The exact averages will still change with a new sample.", quoted_name(result$extras$group))
        } else {
          sprintf("A different set of rows would give slightly different averages. The gaps among the %s groups here are small enough to fit that normal sample-to-sample change. A new sample could change which group looks highest.", quoted_name(result$extras$group))
        },
        boundary = "The overall test says that at least two groups differ. It does not say that every group differs from every other group. The pair-by-pair rows answer that second question when they are available."
      ))
    }
    if (id == "factorial_anova") {
      rows <- estimates[estimates$Term != "Residuals" & is.finite(estimates$p), , drop = FALSE]
      row <- if (nrow(rows)) rows[which.min(rows$p), , drop = FALSE] else NULL
      term <- if (!is.null(row)) as.character(row$Term[[1]]) else "the fitted terms"
      clear <- !is.null(row) && row$p[[1]] < 0.05
      return(list(
        bottom_line = sprintf("For %s, the clearest model term was %s. %s", quoted_name(result$extras$outcome), quoted_name(term), if (clear) "Its pattern was stronger than random variation would usually produce if that term had no link with the outcome." else "Its pattern was still close enough to what random variation could produce, so this sample does not give a clear answer for that term."),
        picture = sprintf("The model considers %s at the same time. The term table gives a separate row for each factor, numeric column, and requested combination of factors.", plain_list(c(result$extras$factors, result$extras$covariates))),
        certainty = if (!is.null(row)) plain_certainty_row(row) else plain_certainty(result),
        boundary = "When an interaction is included, a main effect describes a conditional comparison. Read the full term table before describing any one factor."
      ))
    }
    return(list(
      bottom_line = if (plain_signal(result) == "clear") sprintf("The average %s value changes across the %s conditions recorded for each %s.", quoted_name(result$extras$outcome), quoted_name(result$extras$within), quoted_name(result$extras$id)) else sprintf("The average %s values differ across %s conditions, but this sample does not separate them clearly from random variation.", quoted_name(result$extras$outcome), quoted_name(result$extras$within)),
      picture = sprintf("Each line follows one %s across the %s conditions. The bright line shows the average %s at each condition.", quoted_name(result$extras$id), quoted_name(result$extras$within), quoted_name(result$extras$outcome)),
      certainty = plain_certainty(result),
      boundary = "An overall result does not name the specific groups that differ. Pair-by-pair results are needed for that question."
    ))
  }

  if (id %in% c("chi_square", "fisher_exact", "mcnemar")) {
    left <- result$extras$row %||% result$extras$before
    right <- result$extras$column %||% result$extras$after
    return(list(
      bottom_line = if (plain_signal(result) == "clear") sprintf("The mix of %s categories is different across the %s categories. In this file, knowing one gives some information about the other.", quoted_name(right), quoted_name(left)) else sprintf("The mix of %s categories looks similar enough across %s categories that random variation could explain the differences in this file.", quoted_name(right), quoted_name(left)),
      picture = sprintf("The table counts every combination of %s and %s. A cell that is much larger or smaller than expected contributes more to the result.", quoted_name(left), quoted_name(right)),
      certainty = plain_certainty(result),
      boundary = "A connection between categories does not explain its cause, and a small count can make the pattern unstable."
    ))
  }

  if (id %in% c("correlation", "partial_correlation")) {
    column <- if (id == "correlation") "Correlation" else "Estimate"
    values <- suppressWarnings(as.numeric(estimates[[column]]))
    row <- estimates[which.max(abs(values)), , drop = FALSE]
    value <- as.numeric(row[[column]])
    strength <- if (abs(value) < .3) "small" else if (abs(value) < .6) "medium-sized" else "strong"
    direction <- if (value >= 0) "higher values of one tend to appear with higher values of the other" else "higher values of one tend to appear with lower values of the other"
    pair <- if (id == "correlation") paste(row$Variable_1, "and", row$Variable_2) else as.character(row$Contrast)
    return(list(
      bottom_line = sprintf("The clearest relationship shown is between %s. Its correlation is %s. This is a %s relationship: %s.", pair, plain_number(value, 2), strength, direction),
      picture = sprintf("A correlation can run from -1 to 1. This one is %s. Values near 0 make a loose cloud of points. Values nearer -1 or 1 make a clearer downward or upward band.", plain_number(value, 2)),
      certainty = if (id == "correlation") plain_certainty_row(row) else plain_certainty(result),
      boundary = "Moving together does not show which variable came first, whether one caused the other, or whether a third variable accounts for both."
    ))
  }

  if (id == "moderation") {
    row <- result$extras$interaction
    value <- first_finite(first_value(row, "Estimate"))
    signal <- p_state(first_finite(first_value(row, "p")), value,
                      first_finite(first_value(row, "Lower")), first_finite(first_value(row, "Upper")), 0)
    return(list(
      bottom_line = if (signal == "clear") sprintf("The link between %s and %s is not the same at all values of %s.", quoted_name(result$extras$focal), quoted_name(result$extras$outcome), quoted_name(result$extras$moderator)) else sprintf("The link between %s and %s looks somewhat different across %s, but this sample does not separate that change clearly from random variation.", quoted_name(result$extras$focal), quoted_name(result$extras$outcome), quoted_name(result$extras$moderator)),
      picture = sprintf("The figure draws a separate line for different ranges or groups of %s. Lines with different slopes show that the %s-to-%s link changes. The model gives that change a value of %s.", quoted_name(result$extras$moderator), quoted_name(result$extras$focal), quoted_name(result$extras$outcome), plain_number(value)),
      certainty = plain_certainty_state(signal),
      boundary = "An interaction describes a changing relationship. It does not show why the relationship changes."
    ))
  }

  if (id == "mediation") {
    row <- estimates[estimates$Left == "indirect", , drop = FALSE]
    if (!nrow(row)) row <- estimates[1, , drop = FALSE]
    value <- first_finite(first_value(row, "Estimate"))
    return(list(
      bottom_line = sprintf("The model estimates that %s of the %s-to-%s relationship follows the proposed route through %s.", plain_number(value), quoted_name(result$extras$exposure), quoted_name(result$extras$outcome), quoted_name(result$extras$mediator)),
      picture = sprintf("Picture two routes from %s to %s. One goes straight there. The other first passes through %s. The indirect row describes the second route.", quoted_name(result$extras$exposure), quoted_name(result$extras$outcome), quoted_name(result$extras$mediator)),
      certainty = plain_certainty_row(row),
      boundary = "The arrows are a proposed order, not proof of cause. The study design and timing must support that order."
    ))
  }

  if (id %in% c("linear_regression", RATIO_METHODS, "hlm_linear", "growth_model")) {
    # primary_model_row lives in reporting.R and is shared with the statistical
    # description so that both paragraphs headline the same coefficient.
    row <- primary_model_row(result) %||% estimates[1, , drop = FALSE]
    value <- first_finite(first_value(row, "Estimate"))
    term <- as.character(first_value(row, "Term", "the first listed predictor"))
    shift <- predictor_shift_words(term, result$extras$predictors %||% character(0))
    if (id %in% RATIO_METHODS) {
      noun <- if (id == "cox_regression") "event rate at a given moment" else if (id %in% c("poisson_regression", "negative_binomial")) "count rate" else "odds"
      bottom <- paste(sprintf("For %s in the model of %s, the ratio is %s.", quoted_name(term), quoted_name(result$extras$outcome), plain_number(value, 2)),
                      ratio_words(value, noun, shift))
      null <- 1
    } else {
      direction <- if (value >= 0) "rises" else "falls"
      bottom <- sprintf("%s, the model estimates that %s %s by about %s units while the other listed columns stay the same.",
                        sentence_case(shift$clause), quoted_name(result$extras$outcome), direction, plain_number(abs(value)))
      null <- 0
    }
    return(list(
      bottom_line = bottom,
      picture = sprintf("Picture keeping the other listed columns at the same values, then %s. The model shows how much the outcome would be expected to move with it.", shift$picture),
      certainty = plain_certainty_row(row, null),
      boundary = if (id %in% c("hlm_linear", "hlm_logistic", "growth_model")) "The model accounts for rows nested within people or groups, but it still depends on the selected model form and the supplied columns." else boundary
    ))
  }

  if (id == "manova") {
    return(list(
      bottom_line = if (plain_signal(result) == "clear") sprintf("When %s are considered together, their combined pattern differs across at least one level or value of %s.", plain_list(result$extras$outcomes), plain_list(result$extras$predictors)) else sprintf("When %s are considered together, this sample does not show a clear combined difference across %s.", plain_list(result$extras$outcomes), plain_list(result$extras$predictors)),
      picture = sprintf("Rather than checking %s in separate tests, this method treats them as one combined set of outcomes and asks whether that set changes with %s.", plain_list(result$extras$outcomes), plain_list(result$extras$predictors)),
      certainty = plain_certainty(result),
      boundary = "The combined result does not say which single outcome carries the difference. Follow-up results must be read with care because several comparisons are made."
    ))
  }

  if (id == "pca") {
    first <- estimates[1, , drop = FALSE]
    kept <- min(result$extras$selected %||% 1L, nrow(estimates))
    return(list(
      bottom_line = sprintf("Daybreak replaced %s with a smaller set of summary scores. The first score keeps %s of the differences among rows. The first %s scores together keep %s.", plain_list(result$extras$variables), plain_percent(first$Explained), kept, plain_percent(estimates$Cumulative[kept])),
      picture = sprintf("Imagine a plot built from %s. Daybreak turns the viewing angle until the first new direction shows as much of the spread as possible.", plain_list(result$extras$variables)),
      certainty = "These percentages describe this dataset. A new dataset could produce somewhat different summary scores and percentages.",
      boundary = "A summary score is a mathematical shortcut. It is not automatically a real trait, cause, or outcome. Its meaning depends on the columns that make it up."
    ))
  }

  if (id %in% c("kmeans_clustering", "hierarchical_clustering")) {
    sizes <- estimates$Rows
    return(list(
      bottom_line = sprintf("Using %s, Daybreak placed the rows into %s groups. The smallest group has %s rows, and the largest has %s.", plain_list(result$extras$variables), length(sizes), min(sizes), max(sizes)),
      picture = sprintf("Rows with similar values across %s appear closer together. Rows with less similar values appear farther apart. The colors mark the groups created from those distances.", plain_list(result$extras$variables)),
      certainty = "This is one way to group these rows. A different group count, starting point, or new dataset can change which rows are placed together.",
      boundary = "A computer-created group is not automatically a real kind of person or object. Outside knowledge is needed to decide whether a group is meaningful."
    ))
  }

  if (id %in% c("dbscan_clustering", "hdbscan_clustering")) {
    cluster_rows <- estimates[as.character(estimates$Group) != "Noise", , drop = FALSE]
    noise <- sum(estimates$Rows[as.character(estimates$Group) == "Noise"])
    return(list(
      bottom_line = sprintf("Using %s, Daybreak found %s areas where many similar rows sit close together. It left %s rows outside those areas and labeled them noise.", plain_list(result$extras$variables), nrow(cluster_rows), noise),
      picture = sprintf("Rows with many nearby neighbors across %s join the same group. A noise row simply does not have enough close neighbors under the current settings.", plain_list(result$extras$variables)),
      certainty = "Different distance settings, columns, or new rows can change both the groups and the noise count.",
      boundary = "Noise does not mean that a row is wrong. A computer-created group is also not automatically a real kind of person or object."
    ))
  }

  if (id == "factor_analysis") {
    count <- result$extras$factors %||% NA_integer_
    return(list(
      bottom_line = sprintf("Daybreak summarized the selected items, %s, with %s shared patterns.", plain_list(result$extras$variables), count),
      picture = sprintf("Items in %s that tend to be high or low together are placed under the same shared pattern. A loading tells how closely each item follows that pattern.", plain_list(result$extras$variables)),
      certainty = plain_certainty(result, descriptive = TRUE),
      boundary = "A shared pattern is a proposed summary. It does not by itself prove that a distinct real-world trait exists."
    ))
  }

  if (id == "reliability") {
    alpha <- result$extras$alpha %||% NA_real_
    return(list(
      bottom_line = sprintf("For %s, the consistency score is %s.", plain_list(result$extras$variables), plain_number(alpha, 2)),
      picture = "The score is higher when rows with a high value on one item also tend to have high values on the other items. It is lower when the items do not move together as much.",
      certainty = "This score describes the item responses in this sample. It can change with a different population or a different set of items.",
      boundary = "Consistency does not prove that the items measure only one thing or that the score is valid for its intended use."
    ))
  }

  if (id %in% c("cfa", "sem")) {
    cfi <- first_finite(result$fit$Value[result$fit$Metric == "cfi"])
    rmsea <- first_finite(result$fit$Value[result$fit$Metric == "rmsea"])
    context <- selected_context(result)
    return(list(
      bottom_line = sprintf("%s Daybreak compared the pattern predicted by that model with the pattern in the data. Two overall comparison scores were CFI %s and RMSEA %s.", context, plain_number(cfi, 3), plain_number(rmsea, 3)),
      picture = "Picture the proposed links as a map. The fit scores ask how closely the map recreates the relationships seen in the data. The path rows show each proposed link separately.",
      certainty = "The overall fit scores and the individual paths need to be read together. Different maps can sometimes fit the same data similarly well.",
      boundary = "A well-fitting map is not proof that every arrow is correct, that the model is the only explanation, or that a path is causal."
    ))
  }

  if (id == "kaplan_meier") {
    return(list(
      bottom_line = sprintf("The step-shaped line shows the share of cases that have not yet had %s as %s passes%s.", quoted_name(result$extras$event), quoted_name(result$extras$time), if (length(result$extras$group) && nzchar(result$extras$group)) paste0(", with a separate line for each ", quoted_name(result$extras$group), " group") else ""),
      picture = sprintf("The line drops each time %s is recorded in %s. If a case leaves the data before that event, it still contributes information up to its last %s value.", quoted_name(result$extras$event), quoted_name(result$extras$status), quoted_name(result$extras$time)),
      certainty = plain_certainty(result),
      boundary = "The far end of the curve often rests on fewer cases. It is also assumed that leaving observation is not tied to an unrecorded event risk."
    ))
  }

  if (id == "time_series") {
    order <- paste(result$extras$order %||% c("?", "?", "?"), collapse = ",")
    horizon <- result$extras$horizon %||% 0L
    forecasting <- is.finite(horizon) && horizon > 0L
    return(list(
      bottom_line = sprintf(
        "Daybreak fitted a repeating pattern to %s ordered values of %s. The ARIMA settings were %s, and %s supplied the order.%s",
        format(result$n_used, big.mark = ","), quoted_name(result$extras$outcome), order, quoted_name(result$extras$time),
        if (forecasting) sprintf(" It then projected %s further points.", horizon) else ""
      ),
      picture = if (forecasting) {
        sprintf("The model looks for parts of earlier %s values that carry into later values, then extends that pattern past the last recorded point. The dashed line is the projection and the shaded band around it widens, because a point further ahead is less certain than the next one.", quoted_name(result$extras$outcome))
      } else {
        sprintf("The model looks for parts of earlier %s values that carry into later values. The solid pale line is what was recorded and the bright line is what the model expects at each of those same time points. Where the two run close together, the pattern fits.", quoted_name(result$extras$outcome))
      },
      certainty = if (forecasting) {
        "A projection is only useful while the process keeps behaving as it did during the recorded period. Points further ahead are much less certain than the next one."
      } else {
        "The fitted line describes the recorded period. Set a forecast horizon in analysis settings to project past the last recorded point."
      },
      boundary = "Matching the past closely cannot anticipate a new policy, shock, measurement change, or other break from the earlier pattern."
    ))
  }

  list(
    bottom_line = paste(selected_context(result), "Daybreak completed the calculation. The statistical description below explains the main result, and the tables give the exact values."),
    picture = "Use position, height, direction, and distance in the figure to compare the selected values. The exact table gives the numbers behind those marks.",
    certainty = plain_certainty(result),
    boundary = boundary
  )
}

# The everyday explanation -------------------------------------------------
#
# Five fixed questions, always in the same order: what was asked, what came
# back, how to picture it, how sure it is, and what it does not cover. The
# order is fixed so that a reader who works through one result knows where to
# look in the next, and the last question is never dropped, because the limits
# of a finding are the part most likely to be left out of a summary.
build_accessible_explanation <- function(result) {
  body <- accessible_core(result)
  list(
    title = "Your result in everyday words",
    question = ANALYSES[[result$method]]$question,
    context = selected_context(result),
    bottom_line = body$bottom_line,
    picture = body$picture,
    certainty = body$certainty,
    boundary = body$boundary,
    data_note = plain_data_note(result)
  )
}

accessible_explanation_text <- function(explanation) {
  paste(
    explanation$title,
    explanation$question,
    explanation$context,
    explanation$bottom_line,
    explanation$picture,
    explanation$certainty,
    explanation$boundary,
    explanation$data_note,
    sep = "\n\n"
  )
}
