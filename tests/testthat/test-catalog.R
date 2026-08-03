testthat::test_that("every catalog entry has an engine", {
  for (id in names(ANALYSES)) {
    engine <- paste0("analyse_", ANALYSES[[id]]$engine)
    testthat::expect_true(exists(engine, mode = "function"), info = paste(id, "needs", engine))
  }
})

testthat::test_that("catalog ids and role ids are unique", {
  testthat::expect_equal(length(names(ANALYSES)), length(unique(names(ANALYSES))))
  for (id in names(ANALYSES)) {
    roles <- vapply(ANALYSES[[id]]$roles, `[[`, character(1), "id")
    testthat::expect_equal(length(roles), length(unique(roles)), info = id)
  }
})

testthat::test_that("the release contains the documented method count", {
  testthat::expect_equal(length(ANALYSES), 53)
  testthat::expect_equal(sum(vapply(ANALYSES, function(x) x$kind == "machine_learning", logical(1))), 13)
})

testthat::test_that("the installer lists every package named by the catalog", {
  installer <- paste(readLines("install.R", warn = FALSE), collapse = "\n")
  required <- unique(unlist(lapply(ANALYSES, `[[`, "requires"), use.names = FALSE))

  for (package in required) {
    testthat::expect_match(installer, paste0('"', package, '"'), fixed = TRUE, info = package)
  }
})

testthat::test_that("data-fit screening gives advice without disabling methods", {
  numeric_only <- data.frame(x = 1:20, y = 21:40)
  class_fit <- method_feasibility(ANALYSES$classification_tree, numeric_only)
  correlation_fit <- method_feasibility(ANALYSES$correlation, numeric_only)

  testthat::expect_false(class_fit$possible)
  testthat::expect_match(class_fit$reason, "a categorical outcome", fixed = TRUE)
  testthat::expect_true(correlation_fit$possible)
})

testthat::test_that("sample starting roles do not reuse columns", {
  sample_methods <- c(
    iris = "one_way_anova",
    tooth = "one_way_anova",
    cars = "linear_regression",
    air = "missingness",
    admissions = "chi_square",
    nottem = "time_series"
  )

  for (sample_id in names(sample_methods)) {
    method_id <- unname(sample_methods[[sample_id]])
    data <- load_sample(sample_id)
    roles <- default_role_values(ANALYSES[[method_id]], data, sample_id, method_id)
    selected <- unlist(roles, use.names = FALSE)
    selected <- selected[nzchar(selected)]

    testthat::expect_false(anyDuplicated(selected) > 0, info = sample_id)
    testthat::expect_true(all(selected %in% names(data)), info = sample_id)
  }
})
