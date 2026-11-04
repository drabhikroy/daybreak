# Untrusted input ------------------------------------------------------------
#
# A data file can come from anyone, and three things built from it leave the
# app: the R script the reader is told to run, the HTML report they open in a
# browser, and the session file they may pass to a colleague. Each test below
# feeds a column name written to break out of its context and checks that it
# stays inert.

hostile_frame <- function(name, n = 40) {
  set.seed(11)
  d <- data.frame(stats::rnorm(n), factor(rep(c("p", "q"), length.out = n)))
  names(d) <- c(name, "grp")
  d
}

replay_script <- function(result, data) {
  folder <- tempfile("replay-")
  dir.create(folder)
  old <- setwd(folder)
  on.exit(setwd(old), add = TRUE)
  utils::write.csv(data, "your_data.csv", row.names = FALSE)
  write_reproduction_script(result, "script.R")
  suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), "script.R", stdout = TRUE, stderr = TRUE))
}

testthat::test_that("r_str and bt return every name unchanged after parsing", {
  names_to_try <- c("plain", "test score", 'x"); q("no")', "y` + 1; `z", "back\\slash", "tab\there")
  for (name in names_to_try) {
    testthat::expect_identical(eval(parse(text = r_str(name))), name)
    testthat::expect_identical(as.character(parse(text = bt(name))[[1]]), name)
  }
})

testthat::test_that("a crafted column name cannot run code from the exported script", {
  # This name leaves the surrounding call syntactically valid if it is pasted
  # between plain quotes, so an unescaped script would run the payload. The
  # marker is assembled at run time so it never appears in the name itself.
  crafted <- 'x"]], cat(paste0("PW", "NED"), file = stderr()), df[["x'
  data <- hostile_frame(crafted)
  for (method in c("one_sample_t", "independent_t", "correlation")) {
    roles <- switch(method,
      one_sample_t = list(outcome = crafted),
      independent_t = list(outcome = crafted, group = "grp"),
      correlation = list(variables = c(crafted, crafted))
    )
    if (method == "correlation") {
      data$second <- stats::rnorm(nrow(data))
      roles <- list(variables = c(crafted, "second"))
    }
    output <- replay_script(run_analysis(method, data, roles, list()), data)
    testthat::expect_false(any(grepl("^PWNED", output)), info = method)
  }
})

testthat::test_that("column names with spaces run in the app and in the exported script", {
  set.seed(12)
  data <- data.frame(stats::rnorm(60), stats::rnorm(60), factor(rep(c("a", "b", "c"), 20)))
  names(data) <- c("test score", "hours studied", "study group")
  for (case in list(
    list("one_way_anova", list(outcome = "test score", group = "study group")),
    list("linear_regression", list(outcome = "test score", predictors = "hours studied")),
    list("classification_tree", list(outcome = "study group", predictors = c("test score", "hours studied")))
  )) {
    result <- run_analysis(case[[1]], data, case[[2]], list())
    status <- attr(replay_script(result, data), "status")
    testthat::expect_true(is.null(status) || status == 0L, info = case[[1]])
  }
})

testthat::test_that("renaming columns never changes a prediction result", {
  # Tree, forest, and naive Bayes fit on internal names, so the numbers and the
  # importance ranking have to match exactly under any column names.
  iris_data <- load_sample("iris")
  renamed <- iris_data
  names(renamed) <- c("sepal length \"a\"", "sepal width `b`", "petal length \\c", "petal width d", "the species")
  for (method in c("classification_tree", "naive_bayes")) {
    first <- run_analysis(method, iris_data, list(outcome = "Species", predictors = names(iris_data)[1:4]), list())
    second <- run_analysis(method, renamed, list(outcome = "the species", predictors = names(renamed)[1:4]), list())
    testthat::expect_equal(first$fit$Value, second$fit$Value, info = method)
  }
  tree <- run_analysis("classification_tree", renamed, list(outcome = "the species", predictors = names(renamed)[1:4]), list())
  testthat::expect_true(all(tree$estimates$Variable %in% names(renamed)))
})

testthat::test_that("reports show a hostile column name as text, never as markup", {
  data <- hostile_frame("<img src=x onerror=alert(1)> & co")
  result <- run_analysis("independent_t", data, list(outcome = names(data)[1], group = "grp"), list())
  markdown <- paste(report_markdown(result, build_narrative(result), build_accessible_explanation(result)), collapse = "\n")
  testthat::expect_false(grepl("<img", markdown, fixed = TRUE))
  testthat::expect_match(markdown, "&lt;img src=x onerror=alert(1)&gt; &amp; co", fixed = TRUE)
  testthat::expect_identical(md_text("a < b & c > d"), "a &lt; b &amp; c &gt; d")
})

testthat::test_that("factor analysis never asks for more factors than the columns identify", {
  testthat::skip_if_not_installed("psych")
  testthat::expect_identical(vapply(3:8, max_identified_factors, integer(1)), c(1L, 1L, 2L, 3L, 3L, 4L))
  data <- load_sample("political")
  three <- run_analysis("factor_analysis", data, list(variables = c("y1", "y2", "y3")), list())
  testthat::expect_identical(ncol(three$estimates) - 1L, 1L)
  testthat::expect_error(
    run_analysis("factor_analysis", data, list(variables = c("y1", "y2", "y3")), list(factors = 2)),
    "can support at most 1 factor", fixed = TRUE
  )
})

testthat::test_that("every method gives the same numbers under awkward column names", {
  # Each method runs on the first sample it can use, then again with every
  # column renamed to include a quote, a backtick, a backslash, or a space.
  # Mediation is the one method that refuses such names, because lavaan model
  # syntax has no way to quote them, and it has to say so in plain words.
  awkward <- function(n) paste0(n, " ", c("a\"q", "b`t", "c\\s", "d e")[(seq_along(n) - 1) %% 4 + 1])
  numbers <- function(result) {
    tables <- list(result$estimates, result$tests, result$fit)
    values <- unlist(lapply(tables, function(t) if (is.data.frame(t)) unlist(t[vapply(t, is.numeric, logical(1))])))
    unname(round(values[is.finite(values)], 8))
  }
  samples <- names(available_samples())
  data_sets <- lapply(stats::setNames(samples, samples), load_sample)
  compared <- 0L
  for (id in names(ANALYSES)) {
    spec <- ANALYSES[[id]]
    spec$id <- id
    for (sample in samples) {
      data <- data_sets[[sample]]
      if (!isTRUE(method_feasibility(spec, data)$possible)) next
      roles <- default_role_values(spec, data, sample, id)
      for (role in spec$roles) {
        value <- roles[[role$id]]
        if (is.null(value) || identical(value, "")) {
          roles[[role$id]] <- if (grepl("many|predictors", role$type)) character(0) else ""
        }
      }
      original <- tryCatch(suppressWarnings(run_analysis(id, data, roles, list())), error = function(e) NULL)
      if (is.null(original)) next
      map <- stats::setNames(awkward(names(data)), names(data))
      renamed <- data
      names(renamed) <- map[names(data)]
      new_roles <- lapply(roles, function(v) {
        if (is.character(v) && length(v) && all(v %in% names(map))) unname(map[v]) else v
      })
      if (id == "mediation") {
        testthat::expect_error(run_analysis(id, renamed, new_roles, list()), "name", info = id)
      } else {
        again <- suppressWarnings(run_analysis(id, renamed, new_roles, list()))
        testthat::expect_identical(numbers(again), numbers(original), info = id)
      }
      compared <- compared + 1L
      break
    }
  }
  testthat::expect_gte(compared, 40L)
})
