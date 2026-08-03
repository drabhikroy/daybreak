testthat::test_that("one-sample t output matches stats", {
  d <- data.frame(score = c(3, 4, 5, 6, 7))
  got <- analyse_one_sample_t(d, list(outcome = "score"), list(mu = 4, conf_level = 0.95))
  expected <- stats::t.test(d$score, mu = 4)
  testthat::expect_equal(got$estimates$Estimate, mean(d$score) - 4)
  testthat::expect_equal(got$estimates$Lower, unname(expected$conf.int[1]) - 4)
  testthat::expect_equal(got$estimates$Upper, unname(expected$conf.int[2]) - 4)
  testthat::expect_equal(got$tests$Statistic, unname(expected$statistic))
  testthat::expect_equal(got$tests$p, expected$p.value)
})

testthat::test_that("independent t output carries the requested contrast", {
  d <- data.frame(score = c(2, 3, 4, 7, 8, 9), group = factor(rep(c("A", "B"), each = 3)))
  got <- analyse_independent_t(d, list(outcome = "score", group = "group"), list(equal_var = FALSE, conf_level = 0.95))
  testthat::expect_equal(got$estimates$Estimate, mean(d$score[d$group == "A"]) - mean(d$score[d$group == "B"]))
  testthat::expect_lt(got$tests$p, 0.05)
})

testthat::test_that("linear regression matches lm coefficients", {
  d <- datasets::mtcars
  got <- analyse_linear_regression(d, list(outcome = "mpg", predictors = c("wt", "hp")), list())
  expected <- stats::lm(mpg ~ wt + hp, data = d)
  testthat::expect_equal(stats::coef(got$model), stats::coef(expected))
  testthat::expect_equal(got$n_used, nrow(d))
})

testthat::test_that("chi-square effect size is bounded", {
  d <- data.frame(a = factor(rep(c("yes", "no"), each = 20)),
                  b = factor(c(rep("up", 16), rep("down", 4), rep("up", 5), rep("down", 15))))
  got <- analyse_chi_square(d, list(row = "a", column = "b"), list())
  testthat::expect_gte(got$estimates$Estimate, 0)
  testthat::expect_lte(got$estimates$Estimate, 1)
  testthat::expect_lt(got$tests$p, 0.01)
})

testthat::test_that("PCA explained variation totals one", {
  got <- analyse_pca(datasets::iris, list(variables = names(datasets::iris)[1:4]), list(components = 3))
  testthat::expect_equal(sum(got$estimates$Explained), 1, tolerance = 1e-10)
  testthat::expect_equal(tail(got$estimates$Cumulative, 1), 1, tolerance = 1e-10)
})

testthat::test_that("manual alpha matches its defining formula", {
  items <- data.frame(a = 1:8, b = c(1, 2, 3, 5, 5, 6, 7, 8), c = c(2, 2, 4, 4, 6, 6, 8, 8))
  got <- analyse_reliability(items, list(variables = names(items)), list())
  k <- ncol(items)
  expected <- k / (k - 1) * (1 - sum(vapply(items, var, numeric(1))) / var(rowSums(items)))
  testthat::expect_equal(got$extras$alpha, expected)
})

testthat::test_that("partial correlation uses the control-adjusted degrees of freedom", {
  set.seed(44)
  d <- data.frame(x = stats::rnorm(80), y = stats::rnorm(80), z = stats::rnorm(80))
  got <- analyse_partial_correlation(
    d, list(x = "x", y = "y", controls = "z"),
    list(cor_method = "pearson", conf_level = 0.90)
  )
  rx <- stats::residuals(stats::lm(x ~ z, data = d))
  ry <- stats::residuals(stats::lm(y ~ z, data = d))
  expected_r <- stats::cor(rx, ry)
  expected_df <- nrow(d) - 3L
  expected_t <- expected_r * sqrt(expected_df / (1 - expected_r^2))
  expected_ci <- tanh(atanh(expected_r) + c(-1, 1) * stats::qnorm(0.95) / sqrt(nrow(d) - 4L))

  testthat::expect_equal(got$estimates$Estimate, expected_r)
  testthat::expect_equal(got$tests$df, expected_df)
  testthat::expect_equal(got$tests$Statistic, expected_t)
  testthat::expect_equal(c(got$estimates$Lower, got$estimates$Upper), expected_ci)
})

testthat::test_that("rank partial correlation ranks before removing controls", {
  d <- data.frame(x = c(1, 2, 2, 5, 8, 13), y = c(2, 1, 3, 5, 7, 12), z = c(1, 1, 2, 3, 5, 8))
  ranked <- as.data.frame(lapply(d, rank, ties.method = "average"))
  expected <- stats::cor(
    stats::residuals(stats::lm(x ~ z, data = ranked)),
    stats::residuals(stats::lm(y ~ z, data = ranked))
  )
  got <- analyse_partial_correlation(
    d, list(x = "x", y = "y", controls = "z"),
    list(cor_method = "spearman")
  )
  testthat::expect_equal(got$estimates$Estimate, expected)
  testthat::expect_true(any(grepl("lapply(working, rank", got$code, fixed = TRUE)))
})

testthat::test_that("repeated analyses reject missing or duplicate cells", {
  d <- expand.grid(id = factor(1:8), visit = factor(1:3))
  d$score <- seq_len(nrow(d))
  testthat::expect_error(
    analyse_repeated_anova(d[-1, ], list(outcome = "score", within = "visit", id = "id"), list()),
    "one row in every repeated condition",
    fixed = TRUE
  )
  testthat::expect_error(
    analyse_friedman(rbind(d, d[1, ]), list(outcome = "score", within = "visit", id = "id"), list()),
    "one row per case and condition",
    fixed = TRUE
  )
})

testthat::test_that("exported logistic and proportion code replays", {
  set.seed(45)
  d <- data.frame(outcome = factor(stats::rbinom(100, 1, 0.45)), x = stats::rnorm(100))
  logistic <- analyse_logistic_regression(d, list(outcome = "outcome", predictors = "x"), list())
  environment <- new.env(parent = globalenv())
  environment$df <- d
  testthat::expect_silent(eval(parse(text = paste(logistic$code, collapse = "\n")), envir = environment))

  p_data <- data.frame(answer = factor(c("no", "yes", "yes", NA, "no", "yes")))
  proportion <- analyse_one_proportion(
    p_data, list(outcome = "answer"),
    list(reference_proportion = 0.5, continuity_correction = FALSE)
  )
  environment$df <- p_data
  replayed <- suppressWarnings(eval(parse(text = paste(proportion$code, collapse = "\n")), envir = environment))
  testthat::expect_equal(replayed$p.value, proportion$tests$p)
})

testthat::test_that("randomized helpers leave the caller's random state alone", {
  set.seed(46)
  expected <- stats::runif(4)
  set.seed(46)
  invisible(stratified_holdout(factor(rep(c("A", "B"), each = 12)), 0.75, 77L))
  testthat::expect_equal(stats::runif(4), expected)

  set.seed(47)
  expected <- stats::runif(4)
  set.seed(47)
  invisible(analyse_kmeans_clustering(
    datasets::iris[1:4], list(variables = names(datasets::iris)[1:4]),
    list(clusters = 3, random_seed = 77L)
  ))
  testthat::expect_equal(stats::runif(4), expected)
})

testthat::test_that("optional roles can be left empty", {
  # An unset optional role arrives from the form as an empty string. Summarising
  # a file without grouping it is the first thing most people do here, and it
  # stopped with "undefined columns selected" until this was fixed.
  iris_data <- load_sample("iris")

  testthat::expect_silent(
    run_analysis("descriptives", iris_data,
                 list(variables = c("Sepal.Length", "Sepal.Width"), group = ""), list())
  )
  testthat::expect_silent(
    run_analysis("descriptives", iris_data,
                 list(variables = c("Sepal.Length", "Sepal.Width")), list())
  )
  testthat::expect_silent(
    run_analysis("frequencies", iris_data, list(variables = "Species", split = ""), list())
  )

  result <- run_analysis("descriptives", iris_data,
                         list(variables = "Sepal.Length", group = ""), list())
  testthat::expect_equal(nrow(result$estimates), 1L)
  testthat::expect_identical(as.character(result$estimates$Group[1]), "All rows")
})

testthat::test_that("the moderation model centers its numeric predictors", {
  # Centering leaves the product term untouched and makes each lower-order
  # coefficient the association at the average of the other predictor.
  cars_data <- load_sample("cars")
  result <- run_analysis("moderation", cars_data,
                         list(outcome = "mpg", focal = "wt", moderator = "hp",
                              controls = character(0)), list())

  centered_wt <- cars_data$wt - mean(cars_data$wt)
  centered_hp <- cars_data$hp - mean(cars_data$hp)
  reference <- stats::lm(cars_data$mpg ~ centered_wt * centered_hp)

  testthat::expect_equal(result$estimates$Estimate, unname(stats::coef(reference)))

  raw <- stats::lm(mpg ~ wt * hp, data = cars_data)
  testthat::expect_equal(unname(stats::coef(reference)[4]), unname(stats::coef(raw)[4]))

  testthat::expect_true("Centering" %in% result$checks$Check)
  testthat::expect_true(any(grepl("mean", result$code)))
})

testthat::test_that("fitting a tree leaves the caller's random stream alone", {
  # rpart draws from the random stream during its own cross-validation, so the
  # fit has to run inside with_local_seed like every other randomized step.
  iris_data <- load_sample("iris")
  set.seed(123)
  before <- .Random.seed
  invisible(run_analysis("classification_tree", iris_data,
                         list(outcome = "Species", predictors = c("Sepal.Length", "Petal.Length")),
                         list(random_seed = 7)))
  testthat::expect_identical(before, .Random.seed)
})
