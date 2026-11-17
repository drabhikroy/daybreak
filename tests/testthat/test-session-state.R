testthat::test_that("a Daybreak session keeps data, choices, and a completed result", {
  # Reading a session file is refused on R releases affected by CVE-2024-27322.
  testthat::skip_if(getRversion() < "4.4.0", "session files need R 4.4.0 or later")
  result <- analyze_one_way_anova(
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
  # Reading a session file is refused on R releases affected by CVE-2024-27322.
  testthat::skip_if(getRversion() < "4.4.0", "session files need R 4.4.0 or later")
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

testthat::test_that("data and session files are refused on R releases that can run code while reading them", {
  # The guard takes the version as an argument so this runs the same way on
  # every R, including the patched releases where the real call would pass.
  testthat::expect_error(require_safe_deserialization("a session file", package_version("4.3.3")), "R 4.4.0 or later", fixed = TRUE)
  testthat::expect_error(require_safe_deserialization(".rds files", package_version("4.2.0")), "save the data as a CSV", fixed = TRUE)
  testthat::expect_true(require_safe_deserialization(".rds files", package_version("4.4.0")))
  testthat::expect_true(require_safe_deserialization(".RData files", package_version("4.6.1")))
})
