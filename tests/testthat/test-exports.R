# Downloads --------------------------------------------------------------
#
# Every download path is exercised here against the real constant from
# R/version.R. A download fails at the moment of use, which is the one moment
# nobody is watching a test run, so these paths get called directly rather than
# trusted to work because the code around them does.

sample_result <- function() {
  data <- load_sample("iris")
  run_analysis("one_way_anova", data,
               list(outcome = "Sepal.Length", group = "Species"),
               list(conf_level = 0.95))
}

testthat::test_that("the version constant ships with the application", {
  testthat::expect_true(exists("DAYBREAK_VERSION"))
  testthat::expect_match(DAYBREAK_VERSION, "^[0-9]+[.][0-9]+[.][0-9]+$")
})

testthat::test_that("every text report format writes a file and names the version", {
  result <- sample_result()
  narrative <- build_narrative(result)
  accessible <- build_accessible_explanation(result)

  for (format in c("txt", "md", "tex")) {
    path <- tempfile(fileext = paste0(".", format))
    testthat::expect_silent(write_report(result, narrative, path, format, accessible))
    testthat::expect_true(file.exists(path))
    text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    testthat::expect_true(nzchar(text))
    testthat::expect_match(text, DAYBREAK_VERSION, fixed = TRUE)
  }
})

testthat::test_that("a session record can be built and read back", {
  # Reading a session file is refused on R releases affected by CVE-2024-27322.
  testthat::skip_if(getRversion() < "4.4.0", "session files need R 4.4.0 or later")
  data <- load_sample("iris")
  result <- sample_result()
  session <- new_daybreak_session(
    data, "iris", "sample", "iris", "one_way_anova",
    list(outcome = "Sepal.Length", group = "Species"), list(),
    result, build_narrative(result), build_accessible_explanation(result)
  )
  testthat::expect_identical(session$app_version, DAYBREAK_VERSION)

  path <- tempfile(fileext = ".rds")
  saveRDS(session, path)
  restored <- read_daybreak_session(path)
  testthat::expect_identical(restored$method_id, "one_way_anova")
  testthat::expect_equal(nrow(restored$data), nrow(data))
})

testthat::test_that("the reproduction script parses as R", {
  path <- tempfile(fileext = ".R")
  write_reproduction_script(sample_result(), path)
  testthat::expect_silent(parse(path))
})

testthat::test_that("result tables are written as one CSV per table", {
  folder <- tempfile("daybreak-tables-")
  write_result_tables(sample_result(), folder)
  written <- list.files(folder, pattern = "[.]csv$")
  testthat::expect_true(length(written) >= 2L)
  first <- readLines(file.path(folder, written[1]), warn = FALSE)
  testthat::expect_gt(length(first), 1L)
})

testthat::test_that("the report template can be located from any working directory", {
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  testthat::expect_true(file.exists(daybreak_report_template()))
})

testthat::test_that("Markdown output carries both readings", {
  result <- sample_result()
  text <- report_markdown(result, build_narrative(result),
                          build_accessible_explanation(result))
  joined <- paste(text, collapse = "\n")
  testthat::expect_match(joined, "Everyday explanation", fixed = TRUE)
  testthat::expect_match(joined, "Statistical description", fixed = TRUE)
})
