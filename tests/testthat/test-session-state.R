testthat::test_that("a Daybreak session keeps data, choices, and a completed result", {
  result <- analyse_one_way_anova(
    iris,
    list(outcome = "Sepal.Length", group = "Species"),
    list(conf_level = 0.95)
  )
  bundle <- new_daybreak_session(
    data = iris,
    data_name = "Anderson iris flowers",
    data_source = "sample",
    sample_id = "iris",
    method_id = "one_way_anova",
    roles = list(outcome = "Sepal.Length", group = "Species"),
    options = list(conf_level = 0.95),
    result = result,
    narrative = build_narrative(result),
    accessible = build_accessible_explanation(result)
  )
  path <- tempfile(fileext = ".daybreak.rds")
  write_daybreak_session(bundle, path)
  restored <- read_daybreak_session(path)

  testthat::expect_equal(restored$data, iris)
  testthat::expect_identical(restored$method_id, "one_way_anova")
  testthat::expect_identical(restored$roles$group, "Species")
  testthat::expect_null(restored$result$model)
  testthat::expect_match(daybreak_session_filename(), "\\.daybreak\\.rds$")
})

testthat::test_that("session validation rejects unrelated RDS files", {
  path <- tempfile(fileext = ".daybreak.rds")
  saveRDS(list(message = "not a session"), path)

  testthat::expect_error(read_daybreak_session(path), "not a Daybreak session", fixed = TRUE)
})

testthat::test_that("session validation rejects a role that is absent from its data", {
  bundle <- new_daybreak_session(
    data = iris,
    data_name = "Anderson iris flowers",
    data_source = "sample",
    sample_id = "iris",
    method_id = "one_way_anova",
    roles = list(outcome = "missing_column", group = "Species"),
    options = list(),
    result = NULL,
    narrative = NULL,
    accessible = NULL
  )

  testthat::expect_error(validate_daybreak_session(bundle), "not present", fixed = TRUE)
})
