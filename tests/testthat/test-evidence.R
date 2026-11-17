# The second evidence check ---------------------------------------------------
#
# The Bayes factors are compared with direct calls to the package that computes
# them, and the weighing of the two checks is tested on data built to land in
# each of its readings.

testthat::test_that("Bayes factors match direct BayesFactor calls", {
  testthat::skip_if_not_installed("BayesFactor")
  bf <- function(x) BayesFactor::extractBF(x)$bf[1]
  tooth <- load_sample("tooth")

  two <- run_analysis("independent_t", tooth, list(outcome = "len", group = "supp"), list())
  testthat::expect_equal(two$evidence$Value,
                         bf(BayesFactor::ttestBF(x = tooth$len[tooth$supp == "OJ"], y = tooth$len[tooth$supp == "VC"])))

  one <- run_analysis("one_sample_t", tooth, list(outcome = "len"), list(mu = 18))
  testthat::expect_equal(one$evidence$Value, bf(BayesFactor::ttestBF(x = tooth$len, mu = 18)))

  iris_data <- load_sample("iris")
  paired <- run_analysis("paired_t", iris_data, list(before = "Sepal.Width", after = "Petal.Length"), list())
  testthat::expect_equal(paired$evidence$Value,
                         bf(BayesFactor::ttestBF(x = iris_data$Petal.Length, y = iris_data$Sepal.Width, paired = TRUE)))

  anova <- run_analysis("one_way_anova", iris_data, list(outcome = "Sepal.Width", group = "Species"), list())
  frame <- data.frame(outcome = iris_data$Sepal.Width, group = iris_data$Species)
  testthat::expect_equal(anova$evidence$Value, bf(BayesFactor::anovaBF(outcome ~ group, data = frame, progress = FALSE)))

  cars <- load_sample("cars")
  table_result <- run_analysis("chi_square", cars, list(row = "am", column = "cyl"), list())
  testthat::expect_equal(table_result$evidence$Value,
                         bf(BayesFactor::contingencyTableBF(table(cars$am, cars$cyl), sampleType = "indepMulti", fixedMargin = "rows")))

  proportion <- run_analysis("one_proportion", cars, list(outcome = "am"), list(reference_proportion = 0.5))
  testthat::expect_equal(proportion$evidence$Value, bf(BayesFactor::proportionBF(y = sum(cars$am == "1"), N = nrow(cars), p = 0.5)))

  related <- run_analysis("correlation", cars, list(variables = c("mpg", "wt")), list())
  testthat::expect_equal(related$evidence$Value, bf(BayesFactor::correlationBF(y = cars$wt, x = cars$mpg)))
})

testthat::test_that("the same rows always give the same Bayes factor", {
  testthat::skip_if_not_installed("BayesFactor")
  iris_data <- load_sample("iris")
  roles <- list(outcome = "Sepal.Width", group = "Species")
  first <- run_analysis("one_way_anova", iris_data, roles, list())$evidence$Value
  second <- run_analysis("one_way_anova", iris_data, roles, list())$evidence$Value
  testthat::expect_identical(first, second)
})

testthat::test_that("rank correlations and untested methods carry no Bayes factor", {
  testthat::skip_if_not_installed("BayesFactor")
  cars <- load_sample("cars")
  spearman <- run_analysis("correlation", cars, list(variables = c("mpg", "wt")), list(cor_method = "spearman"))
  testthat::expect_null(spearman$evidence)
  testthat::expect_false("BF10" %in% names(spearman$estimates))
  testthat::expect_null(run_analysis("mann_whitney", load_sample("tooth"), list(outcome = "len", group = "supp"), list())$evidence)
  testthat::expect_null(run_analysis("descriptives", cars, list(variables = "mpg", group = ""), list())$evidence)
})

testthat::test_that("strength bands follow the published boundaries on both sides of one", {
  testthat::expect_identical(vapply(c(1, 2.9, 3, 9.9, 10, 29, 30, 99, 100, 5000), evidence_strength, character(1)),
                             c("anecdotal", "anecdotal", "moderate", "moderate", "strong", "strong",
                               "very strong", "very strong", "extreme", "extreme"))
  testthat::expect_identical(evidence_strength(1 / 4), "moderate")
  testthat::expect_identical(evidence_strength(1 / 40), "very strong")
  testthat::expect_true(is.na(evidence_strength(NA_real_)))
})

testthat::test_that("the verdict weighs the interval and the Bayes factor together", {
  record <- function(lower, upper, bf) {
    list(method = "independent_t",
         estimates = data.frame(Estimate = (lower + upper) / 2, Lower = lower, Upper = upper),
         tests = data.frame(p = 0.5),
         evidence = if (is.na(bf)) NULL else data.frame(Value = bf))
  }
  testthat::expect_identical(weighed_signal(record(0.2, 0.9, 25))$state, "clear")
  testthat::expect_identical(weighed_signal(record(0.2, 0.9, 1.4))$state, "tentative")
  testthat::expect_identical(weighed_signal(record(-0.4, 0.5, 0.2))$state, "null_favored")
  testthat::expect_identical(weighed_signal(record(-0.4, 0.5, 0.8))$state, "uncertain")
  testthat::expect_identical(weighed_signal(record(0.2, 0.9, NA))$state, "clear")
  testthat::expect_identical(weighed_signal(record(-0.4, 0.5, NA))$state, "uncertain")
})

testthat::test_that("a large sample with no difference is read as pointing to no effect", {
  testthat::skip_if_not_installed("BayesFactor")
  set.seed(1)
  data <- data.frame(score = stats::rnorm(400), arm = rep(c("a", "b"), 200))
  result <- run_analysis("independent_t", data, list(outcome = "score", group = "arm"), list())
  testthat::expect_lt(result$evidence$Value, 1 / 3)
  testthat::expect_identical(result_verdict(result)$label, "Points to no effect")
  testthat::expect_match(build_accessible_explanation(result)$certainty, "It favors no difference by about", fixed = TRUE)
  testthat::expect_match(build_narrative(result)$cards[[2]]$body, "evidence for no difference", fixed = TRUE)
})

testthat::test_that("a one-proportion verdict is judged against its reference value", {
  # The interval for a proportion never includes zero, so comparing it with
  # zero would call every proportion a clear signal.
  cars <- load_sample("cars")
  result <- run_analysis("one_proportion", cars, list(outcome = "am"), list(reference_proportion = 0.5))
  testthat::expect_identical(result_null(result), 0.5)
  testthat::expect_false(identical(result_verdict(result)$label, "Clear signal"))
  far <- run_analysis("one_proportion", cars, list(outcome = "am"), list(reference_proportion = 0.95))
  testthat::expect_identical(plain_signal(far, result_null(far)), "clear")
})

testthat::test_that("the Bayes factor travels into the script, the report, and the tables", {
  testthat::skip_if_not_installed("BayesFactor")
  tooth <- load_sample("tooth")
  result <- run_analysis("independent_t", tooth, list(outcome = "len", group = "supp"), list())
  script <- reproduction_script(result)
  testthat::expect_true(any(grepl("BayesFactor::ttestBF", script, fixed = TRUE)))
  testthat::expect_true(any(grepl("requireNamespace(\"BayesFactor\"", script, fixed = TRUE)))
  testthat::expect_silent(parse(text = script))
  markdown <- report_markdown(result, build_narrative(result), build_accessible_explanation(result))
  testthat::expect_true("## Second evidence check" %in% markdown)
  folder <- tempfile("tables-")
  write_result_tables(result, folder)
  testthat::expect_true(file.exists(file.path(folder, "second_evidence.csv")))
})
