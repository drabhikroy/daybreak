testthat::test_that("MASS count and ordinal models return coefficient tables", {
  testthat::skip_if_not_installed("MASS")

  negative <- analyse_negative_binomial(
    MASS::quine,
    list(outcome = "Days", predictors = c("Eth", "Sex"), exposure_time = ""),
    list()
  )
  testthat::expect_gt(nrow(negative$estimates), 1)
  testthat::expect_true(all(negative$estimates$Estimate > 0))

  set.seed(101)
  ordinal_data <- data.frame(
    rating = ordered(sample(c("low", "middle", "high"), 180, replace = TRUE),
                     levels = c("low", "middle", "high")),
    score = stats::rnorm(180),
    group = factor(sample(c("A", "B"), 180, replace = TRUE))
  )
  ordinal <- analyse_ordinal_regression(
    ordinal_data,
    list(outcome = "rating", predictors = c("score", "group")),
    list(ordinal_order = c("low", "middle", "high"), conf_level = 0.90)
  )
  testthat::expect_gt(nrow(ordinal$estimates), 1)
  testthat::expect_identical(ordinal$extras$order, c("low", "middle", "high"))
  testthat::expect_true(any(grepl('levels = c("low", "middle", "high")', ordinal$code, fixed = TRUE)))
  testthat::expect_error(
    analyse_ordinal_regression(
      ordinal_data,
      list(outcome = "rating", predictors = c("score", "group")),
      list(ordinal_order = c("low", "high"))
    ),
    "every observed category exactly once",
    fixed = TRUE
  )
})

testthat::test_that("multinomial regression returns category-specific estimates", {
  testthat::skip_if_not_installed("nnet")

  result <- analyse_multinomial_regression(
    datasets::iris,
    list(outcome = "Species", predictors = c("Sepal.Length", "Sepal.Width")),
    list()
  )
  testthat::expect_gt(nrow(result$estimates), 2)
  testthat::expect_true(all(result$estimates$Estimate > 0))
})

testthat::test_that("factor analysis reports loadings and fit", {
  testthat::skip_if_not_installed("psych")

  items <- psych::bfi[stats::complete.cases(psych::bfi[, 1:6]), 1:6]
  result <- analyse_factor_analysis(
    items,
    list(variables = names(items)),
    list(factors = 2, rotation = "oblimin", factor_method = "minres")
  )
  testthat::expect_equal(nrow(result$estimates), ncol(items))
  testthat::expect_true(all(c("RMSR", "TLI", "RMSEA") %in% result$fit$Metric))
})

testthat::test_that("CFA, SEM, and mediation models converge on known examples", {
  testthat::skip_if_not_installed("lavaan")

  cfa_syntax <- paste(
    "visual =~ x1 + x2 + x3",
    "textual =~ x4 + x5 + x6",
    "speed =~ x7 + x8 + x9",
    sep = "\n"
  )
  cfa <- analyse_cfa(
    lavaan::HolzingerSwineford1939,
    list(model_syntax = cfa_syntax),
    list(estimator = "MLR", missing_method = "fiml", ordered = character(0))
  )
  testthat::expect_gt(nrow(cfa$estimates), 6)
  testthat::expect_true(any(cfa$fit$Metric == "cfi"))
  testthat::expect_silent(parse(text = paste(cfa$code, collapse = "\n")))

  sem_data <- lavaan::PoliticalDemocracy
  sem_syntax <- default_sample_syntax("political", "sem")
  sem <- analyse_sem(
    sem_data,
    list(model_syntax = sem_syntax),
    list(estimator = "MLR", missing_method = "fiml", ordered = character(0))
  )
  testthat::expect_gt(nrow(sem$estimates), 10)
  testthat::expect_silent(parse(text = paste(sem$code, collapse = "\n")))

  set.seed(102)
  mediation_data <- data.frame(x = stats::rnorm(140))
  mediation_data$m <- 0.6 * mediation_data$x + stats::rnorm(140)
  mediation_data$y <- 0.4 * mediation_data$m + 0.2 * mediation_data$x + stats::rnorm(140)
  mediation <- analyse_mediation(
    mediation_data,
    list(outcome = "y", exposure = "x", mediator = "m", controls = character(0)),
    list(bootstrap = 200)
  )
  testthat::expect_true(all(c("indirect", "total") %in% mediation$estimates$Left))
  testthat::expect_silent(parse(text = paste(mediation$code, collapse = "\n")))
})

testthat::test_that("linear, logistic, and growth mixed models return fitted results", {
  testthat::skip_if_not_installed("lme4")
  testthat::skip_if_not_installed("lmerTest")

  linear <- run_analysis(
    "hlm_linear",
    lme4::sleepstudy,
    list(outcome = "Reaction", predictors = "Days", cluster = "Subject", random_slope = "Days"),
    list(reml = FALSE)
  )
  testthat::expect_gt(nrow(linear$estimates), 1)
  testthat::expect_true(any(linear$fit$Metric == "Clusters"))

  growth <- run_analysis(
    "growth_model",
    lme4::sleepstudy,
    list(outcome = "Reaction", time = "Days", cluster = "Subject", controls = character(0)),
    list(reml = FALSE, growth_random_slope = TRUE)
  )
  testthat::expect_identical(growth$method, "growth_model")

  set.seed(103)
  cluster <- factor(rep(seq_len(24), each = 12))
  predictor <- stats::rnorm(length(cluster))
  random_intercept <- stats::rnorm(24, sd = 0.7)[cluster]
  probability <- stats::plogis(-0.3 + 0.5 * predictor + random_intercept)
  binary_data <- data.frame(event = factor(stats::rbinom(length(cluster), 1, probability)),
                            predictor = predictor, cluster = cluster)
  logistic <- run_analysis(
    "hlm_logistic",
    binary_data,
    list(outcome = "event", predictors = "predictor", cluster = "cluster", random_slope = ""),
    list()
  )
  testthat::expect_gt(nrow(logistic$estimates), 1)
})

testthat::test_that("survival analyses return curves and hazard ratios", {
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status <- factor(lung$status, levels = c(1, 2), labels = c("Censored", "Event"))
  lung$sex <- factor(lung$sex)

  kaplan_meier <- analyse_kaplan_meier(
    lung,
    list(time = "time", status = "status", group = "sex"),
    list()
  )
  testthat::expect_gt(nrow(kaplan_meier$plot_data), 10)
  testthat::expect_equal(nrow(kaplan_meier$tests), 1)

  cox <- analyse_cox_regression(
    lung,
    list(time = "time", status = "status", predictors = c("age", "sex")),
    list()
  )
  testthat::expect_gt(nrow(cox$estimates), 1)
  testthat::expect_true(all(cox$estimates$Estimate > 0))
})

testthat::test_that("survival analyses reject invalid time and event data", {
  testthat::skip_if_not_installed("survival")
  d <- data.frame(time = c(1, 2, -1, 4), status = factor(c("No", "Yes", "No", "Yes")), x = 1:4)
  testthat::expect_error(
    analyse_kaplan_meier(d, list(time = "time", status = "status", group = ""), list()),
    "cannot be negative",
    fixed = TRUE
  )
  d$time <- 1:4
  d$status <- factor(c("No", "No", "No", "Yes"))
  testthat::expect_error(
    analyse_cox_regression(d, list(time = "time", status = "status", predictors = "x"), list()),
    "more observed events",
    fixed = TRUE
  )
})

testthat::test_that("time-series analysis checks order and calendar spacing", {
  set.seed(104)
  regular <- data.frame(time = seq(as.Date("2020-01-01"), by = "month", length.out = 48), y = stats::rnorm(48))
  got <- analyse_time_series(
    regular, list(outcome = "y", time = "time"),
    list(ar_p = 1, ar_d = 0, ar_q = 0, frequency = 12)
  )
  testthat::expect_gt(nrow(got$estimates), 0)

  irregular <- data.frame(time = c(1:20, 22:40), y = stats::rnorm(39))
  testthat::expect_error(
    analyse_time_series(
      irregular, list(outcome = "y", time = "time"),
      list(ar_p = 1, ar_d = 0, ar_q = 0, frequency = 1)
    ),
    "equally spaced",
    fixed = TRUE
  )
})
