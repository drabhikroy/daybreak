# A second evidence check ---------------------------------------------------
#
# An interval and a p-value answer one question: how surprising would these
# rows be if nothing were going on. They cannot say that nothing is going on,
# and a verdict built on them alone treats "not surprising" and "no effect" as
# the same thing. A Bayes factor answers a different question, which of two
# accounts predicts the rows better, and it can favor either one. Daybreak
# computes it beside the classical result for the tests where a default prior
# is well established, and the verdict weighs the two together, in the spirit
# of the American Statistical Association statement that no single index
# should stand in for a scientific conclusion (Wasserstein and Lazar, 2016).
#
# The calculations come from the BayesFactor package and use its documented
# defaults: the JZS prior of Rouder, Speckman, Sun, Morey, and Iverson (2009)
# for t tests, the fixed-effect prior of Rouder, Morey, Speckman, and Province
# (2012) for one-way designs, the stretched beta prior of Ly, Verhagen, and
# Wagenmakers (2016) for a correlation, the independent multinomial model of
# Gunel and Dickey (1974) for a two-way table, and a logistic prior on the log
# odds for one proportion. None of them draws random numbers, so the same rows
# always give the same value.

EVIDENCE_METHODS <- c(
  "one_sample_t", "independent_t", "paired_t", "one_way_anova",
  "chi_square", "one_proportion", "correlation"
)

bayes_factor_available <- function() requireNamespace("BayesFactor", quietly = TRUE)

# extractBF is the supported way to read the number out of the package object.
# A failed or non-finite value becomes NA, because an evidence measure that is
# sometimes wrong is worse than one that is sometimes absent.
bayes_factor_value <- function(expr) {
  value <- tryCatch(
    suppressMessages(suppressWarnings(BayesFactor::extractBF(expr)$bf[1])),
    error = function(e) NA_real_
  )
  if (length(value) != 1L || !is.finite(value) || value <= 0) NA_real_ else unname(value)
}

bf_two_groups <- function(x, y) bayes_factor_value(BayesFactor::ttestBF(x = x, y = y))
bf_one_sample <- function(x, mu = 0) bayes_factor_value(BayesFactor::ttestBF(x = x, mu = mu))
bf_paired <- function(before, after) bayes_factor_value(BayesFactor::ttestBF(x = after, y = before, paired = TRUE))
bf_correlation <- function(x, y) bayes_factor_value(BayesFactor::correlationBF(y = y, x = x))

bf_one_way <- function(outcome, group) {
  frame <- data.frame(outcome = outcome, group = factor(group))
  bayes_factor_value(BayesFactor::anovaBF(outcome ~ group, data = frame, progress = FALSE))
}

bf_table <- function(observed) {
  bayes_factor_value(BayesFactor::contingencyTableBF(
    as.matrix(observed), sampleType = "indepMulti", fixedMargin = "rows"
  ))
}

bf_proportion <- function(events, n, reference) {
  bayes_factor_value(BayesFactor::proportionBF(y = events, N = n, p = reference))
}

# The verbal bands are the ones proposed by Jeffreys and restated by Lee and
# Wagenmakers (2013). They are a reading aid and nothing more: the number is
# always shown beside the word, and the boundaries are not treated as cliffs
# anywhere else in the app.
evidence_strength <- function(bf) {
  if (!is.finite(bf)) return(NA_character_)
  ratio <- max(bf, 1 / bf)
  if (ratio < 3) "anecdotal"
  else if (ratio < 10) "moderate"
  else if (ratio < 30) "strong"
  else if (ratio < 100) "very strong"
  else "extreme"
}

# What the two accounts are called for each method, in words that fit a
# sentence beginning "evidence for".
evidence_claims <- function(method) {
  switch(
    method,
    one_way_anova = c(effect = "a difference among the groups", null = "no difference among the groups"),
    chi_square = c(effect = "an association", null = "no association"),
    correlation = c(effect = "a relationship", null = "no relationship"),
    one_proportion = c(effect = "a difference from the reference", null = "no difference from the reference"),
    c(effect = "a difference", null = "no difference")
  )
}

# The ratio as a reader would say it. Past a thousand the exact figure stops
# meaning anything to anyone, so the sentence says so and the table keeps the
# full value.
odds_words <- function(bf) {
  ratio <- max(bf, 1 / bf)
  if (ratio >= 1000) "more than 1,000 to 1"
  else if (ratio >= 10) sprintf("about %s to 1", formatC(round(ratio), format = "d", big.mark = ","))
  else sprintf("about %s to 1", formatC(ratio, format = "f", digits = 1))
}

evidence_row <- function(method, bf, prior) {
  if (!is.finite(bf)) return(NULL)
  claims <- evidence_claims(method)
  favors <- if (bf >= 1) claims[["effect"]] else claims[["null"]]
  data.frame(
    Measure = "Bayes factor (BF10)",
    Value = bf,
    Favors = favors,
    Strength = evidence_strength(bf),
    Prior = prior,
    stringsAsFactors = FALSE
  )
}

# Computed from the finished result instead of inside each engine. Every engine
# already keeps the complete rows it used and the roles it was given, so the
# second check reads exactly the data the first one did and cannot drift from
# it. Correlation is the exception, because its result holds one row for each
# pair of columns, and the engine adds the value to that table itself.
second_evidence <- function(result) {
  if (!result$method %in% EVIDENCE_METHODS || !bayes_factor_available()) return(NULL)
  data <- result$plot_data
  extras <- result$extras
  switch(
    result$method,
    one_sample_t = evidence_row(
      result$method, bf_one_sample(data[[extras$outcome]], extras$reference),
      "JZS, scale 0.707"),
    independent_t = {
      group <- as_group(data[[extras$group]])
      values <- data[[extras$outcome]]
      evidence_row(result$method,
                   bf_two_groups(values[group == levels(group)[1]], values[group == levels(group)[2]]),
                   "JZS, scale 0.707")
    },
    paired_t = evidence_row(
      result$method, bf_paired(data[[extras$before]], data[[extras$after]]),
      "JZS, scale 0.707"),
    one_way_anova = evidence_row(
      result$method, bf_one_way(data[[extras$outcome]], data[[extras$group]]),
      "Fixed effects, scale 0.5"),
    chi_square = evidence_row(
      result$method, bf_table(extras$observed),
      "Independent multinomial, concentration 1"),
    one_proportion = evidence_row(
      result$method,
      bf_proportion(round(result$estimates$Proportion[1] * result$n_used), result$n_used, extras$reference),
      "Logistic, scale 0.5"),
    correlation = {
      bf <- if ("BF10" %in% names(result$estimates)) result$estimates$BF10[1] else NA_real_
      evidence_row(result$method, bf, "Stretched beta, scale 0.333")
    },
    NULL
  )
}

# The lines that reproduce the value in the exported script. They sit behind a
# package check so that the script still runs for someone who has R and not
# BayesFactor, which is the whole point of handing over a script.
second_evidence_code <- function(result) {
  if (is.null(result$evidence)) return(character(0))
  extras <- result$extras
  call <- switch(
    result$method,
    one_sample_t = sprintf("BayesFactor::ttestBF(x = df[[%s]][!is.na(df[[%s]])], mu = %s)",
                           r_str(extras$outcome), r_str(extras$outcome), extras$reference),
    independent_t = c(
      sprintf("bf_data <- df[complete.cases(df[c(%s, %s)]), ]", r_str(extras$outcome), r_str(extras$group)),
      sprintf("bf_group <- droplevels(factor(bf_data[[%s]]))", r_str(extras$group)),
      sprintf("BayesFactor::ttestBF(x = bf_data[[%s]][bf_group == levels(bf_group)[1]], y = bf_data[[%s]][bf_group == levels(bf_group)[2]])",
              r_str(extras$outcome), r_str(extras$outcome))),
    paired_t = c(
      sprintf("bf_data <- df[complete.cases(df[c(%s, %s)]), ]", r_str(extras$before), r_str(extras$after)),
      sprintf("BayesFactor::ttestBF(x = bf_data[[%s]], y = bf_data[[%s]], paired = TRUE)",
              r_str(extras$after), r_str(extras$before))),
    one_way_anova = c(
      sprintf("bf_data <- data.frame(outcome = df[[%s]], group = factor(df[[%s]]))", r_str(extras$outcome), r_str(extras$group)),
      "bf_data <- bf_data[complete.cases(bf_data), ]",
      "BayesFactor::anovaBF(outcome ~ group, data = bf_data, progress = FALSE)"),
    chi_square = sprintf(
      "BayesFactor::contingencyTableBF(table(df[[%s]], df[[%s]]), sampleType = \"indepMulti\", fixedMargin = \"rows\")",
      r_str(extras$row), r_str(extras$column)),
    one_proportion = c(
      sprintf("bf_outcome <- droplevels(factor(df[[%s]][!is.na(df[[%s]])]))", r_str(extras$outcome), r_str(extras$outcome)),
      sprintf("BayesFactor::proportionBF(y = sum(bf_outcome == levels(bf_outcome)[2]), N = length(bf_outcome), p = %s)", extras$reference)),
    correlation = {
      pair <- c(result$estimates$Variable_1[1], result$estimates$Variable_2[1])
      c(sprintf("bf_data <- df[complete.cases(df[c(%s)]), ]", paste(r_str(extras$variables), collapse = ", ")),
        sprintf("BayesFactor::correlationBF(y = bf_data[[%s]], x = bf_data[[%s]])", r_str(pair[2]), r_str(pair[1])))
    },
    character(0)
  )
  if (!length(call)) return(character(0))
  c("", "# Second evidence check: a Bayes factor with the package default prior.",
    "if (requireNamespace(\"BayesFactor\", quietly = TRUE)) {",
    paste0("  ", call[-length(call)]),
    sprintf("  print(%s)", call[length(call)]),
    "}")
}

# Weighing the two checks -----------------------------------------------------
#
# Five readings. The interval leads, because it carries the size of the effect,
# and the Bayes factor can do two things to it. Where the interval excludes the
# null but the Bayes factor is below 3, the support is thinner than the
# interval alone suggests, and the reading says so. Where the interval includes
# the null and the Bayes factor favors the null by 3 or more, the rows are
# doing more than failing to show an effect, and the reading says that too.
# Without a Bayes factor the interval reading stands unchanged.
weighed_signal <- function(result, null = 0) {
  interval <- plain_signal(result, null)
  bf <- if (is.null(result$evidence)) NA_real_ else result$evidence$Value[1]
  state <- interval
  if (is.finite(bf)) {
    if (identical(interval, "clear") && bf < 3) state <- "tentative"
    if (identical(interval, "uncertain") && bf <= 1 / 3) state <- "null_favored"
    if (identical(interval, "unknown")) {
      state <- if (bf >= 3) "clear" else if (bf <= 1 / 3) "null_favored" else "uncertain"
    }
  }
  list(state = state, interval = interval, bf = bf)
}

# One sentence for each audience. The everyday version never uses the term,
# because the idea, which story predicts these rows better, needs no name.
evidence_sentence_plain <- function(result) {
  if (is.null(result$evidence)) return("")
  bf <- result$evidence$Value[1]
  claims <- evidence_claims(result$method)
  if (max(bf, 1 / bf) < 1.5) {
    return(sprintf("A second check asks which account predicts these rows better, %s or %s. It does not favor either one.",
                   claims[["effect"]], claims[["null"]]))
  }
  sprintf("A second check asks which account predicts these rows better, %s or %s. It favors %s by %s.",
          claims[["effect"]], claims[["null"]], result$evidence$Favors[1], odds_words(bf))
}

evidence_sentence_formal <- function(result) {
  if (is.null(result$evidence)) return("")
  row <- result$evidence[1, ]
  shown <- if (row$Value >= 1000) "greater than 1,000" else if (row$Value < 0.001) "less than 0.001" else fmt_num(row$Value, if (row$Value >= 10) 1 else 3)
  sprintf("The Bayes factor (BF10) was %s, %s evidence for %s under the default prior (%s).",
          shown, row$Strength, row$Favors, row$Prior)
}
