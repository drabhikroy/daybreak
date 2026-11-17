# The interface, read as text ---------------------------------------------------
#
# These checks cover the joins between the server, the page, and the browser
# script. A mismatch at one of those joins does not raise an error in R, so
# without a test it is found only by someone using the app.

script_text <- function() paste(readLines(project_path("www/app.js"), warn = FALSE), collapse = "\n")
app_text <- function() paste(readLines(project_path("app.R"), warn = FALSE), collapse = "\n")

testthat::test_that("every message the server sends has a handler in the browser", {
  sent <- unique(regmatches(app_text(), gregexpr("sendCustomMessage\\(\"[a-z-]+\"", app_text()))[[1]])
  sent <- sub("^sendCustomMessage\\(\"", "", sub("\"$", "", sent))
  handled <- unique(regmatches(script_text(), gregexpr("addCustomMessageHandler\\(\"[a-z-]+\"", script_text()))[[1]])
  handled <- sub("^addCustomMessageHandler\\(\"", "", sub("\"$", "", handled))
  testthat::expect_true(length(sent) >= 5)
  testthat::expect_identical(setdiff(sent, handled), character(0))
})

testthat::test_that("every browser handler declares exactly one argument", {
  # Shiny throws when a handler declares none, and the throw stops every
  # registration that follows it, which silently disables the page.
  inline <- regmatches(script_text(), gregexpr("addCustomMessageHandler\\(\"[a-z-]+\", function \\(([^)]*)\\)", script_text()))[[1]]
  testthat::expect_true(length(inline) >= 6)
  arguments <- sub("^.*function \\(", "", sub("\\)$", "", inline))
  testthat::expect_true(all(nzchar(trimws(arguments))), info = paste(inline[!nzchar(trimws(arguments))], collapse = "; "))
  testthat::expect_false(any(grepl(",", arguments, fixed = TRUE)))
  named <- regmatches(script_text(), gregexpr("addCustomMessageHandler\\(\"[a-z-]+\", [A-Za-z]+\\)", script_text()))[[1]]
  for (entry in named) {
    name <- sub("\\)$", "", sub("^.*, ", "", entry))
    testthat::expect_match(script_text(), sprintf("function %s\\(message\\)", name), info = name)
  }
})

testthat::test_that("the saved theme is sent again once Shiny is connected", {
  testthat::expect_match(script_text(), "send(\"theme_mode\", document.body.classList.contains(\"light-mode\")", fixed = TRUE)
  testthat::expect_match(script_text(), "jQuery(panel).trigger(\"shown\")", fixed = TRUE)
})

testthat::test_that("a quick start runs on the server and through the one run path", {
  testthat::expect_match(app_text(), "state$pending_run <- method_id", fixed = TRUE)
  testthat::expect_match(app_text(), "isolate(execute_run())", fixed = TRUE)
  testthat::expect_identical(lengths(regmatches(app_text(), gregexpr("run_analysis(method_id, data, roles, opts)", app_text(), fixed = TRUE))), 1L)
  testthat::expect_false(grepl("invalidateLater", app_text(), fixed = TRUE))
})

testthat::test_that("the guide reaches every method and names only real ones", {
  listed <- unlist(lapply(METHOD_GUIDE, function(goal) lapply(goal$rows, `[[`, "methods")))
  testthat::expect_identical(setdiff(names(ANALYSES), listed), character(0))
  testthat::expect_identical(setdiff(listed, names(ANALYSES)), character(0))
  goals <- vapply(METHOD_GUIDE, `[[`, character(1), "id")
  testthat::expect_identical(anyDuplicated(goals), 0L)
  for (goal in METHOD_GUIDE) {
    for (row in goal$rows) testthat::expect_true(nzchar(row$when) && length(row$methods) >= 1L, info = goal$id)
  }
})

testthat::test_that("the directory offers the guide, both lists, and a labeled search box", {
  html <- as.character(methods_directory_ui())
  for (kind in c("guide", "statistics", "machine_learning")) {
    testthat::expect_match(html, sprintf("data-method-kind=\"%s\"", kind), fixed = TRUE)
    testthat::expect_match(html, sprintf("data-method-kind-panel=\"%s\"", kind), fixed = TRUE)
  }
  testthat::expect_match(html, "<label class=\"visually-hidden\" for=\"method-search\">", fixed = TRUE)
  testthat::expect_match(html, "id=\"method-search-status\"", fixed = TRUE)
  cards <- lengths(regmatches(html, gregexpr("class=\"method-name-choice\"", html, fixed = TRUE)))
  testthat::expect_identical(cards, length(ANALYSES))
  testthat::expect_identical(lengths(regmatches(html, gregexpr("data-search=\"", html, fixed = TRUE))), length(ANALYSES))
  testthat::expect_match(html, sprintf("Search all %d methods", length(ANALYSES)), fixed = TRUE)
})

testthat::test_that("every method and every walkthrough slide has a drawing of its own", {
  fallback <- as.character(method_visual_ui(list(id = "no-such-method")))
  for (id in setdiff(names(ANALYSES), "descriptives")) {
    testthat::expect_false(identical(as.character(method_visual_ui(list(id = id))), fallback), info = id)
  }
  slides <- tour_slides()
  testthat::expect_identical(length(slides), 6L)
  for (slide in slides) {
    testthat::expect_true(nzchar(slide$title) && nzchar(slide$text))
    testthat::expect_true(slide$art %in% names(ANALYSES), info = slide$title)
  }
  html <- as.character(tour_ui())
  testthat::expect_identical(lengths(regmatches(html, gregexpr("data-tour-slide=", html, fixed = TRUE))), 6L)
  testthat::expect_match(app_text(), "static_dialog_ui(\"tour-panel\"", fixed = TRUE)
  testthat::expect_match(script_text(), "daybreak-tour-seen", fixed = TRUE)
})

testthat::test_that("Help carries the version, the license, the copyright, and the newest release", {
  html <- as.character(help_ui())
  testthat::expect_match(html, DAYBREAK_VERSION, fixed = TRUE)
  testthat::expect_match(html, "PolyForm Noncommercial License 1.0.0", fixed = TRUE)
  testthat::expect_match(html, "Copyright 2026 Abhik Roy.", fixed = TRUE)
  testthat::expect_match(html, "https://github.com/drabhikroy/daybreak/releases/latest", fixed = TRUE)
  testthat::expect_match(html, "href=\"#tour-panel\"", fixed = TRUE)
  for (term in vapply(GLOSSARY, `[`, character(1), 1)) testthat::expect_match(html, term, fixed = TRUE)
  for (label in c("Clear signal", "Tentative signal", "Not clear", "Points to no effect")) {
    testthat::expect_match(html, label, fixed = TRUE)
  }
})

testthat::test_that("key numbers lead with the predictor and never with the intercept", {
  cars <- load_sample("cars")
  result <- run_analysis("linear_regression", cars, list(outcome = "mpg", predictors = c("wt", "hp")), list())
  tiles <- key_numbers(result)
  testthat::expect_identical(tiles[[1]]$note, "wt")
  testthat::expect_false(any(vapply(tiles, function(tile) identical(tile$note, "(Intercept)"), logical(1))))
  labels <- vapply(tiles, `[[`, character(1), "label")
  testthat::expect_true("95% interval" %in% labels)
  testthat::expect_identical(labels[length(labels)], "Rows used")
})

testthat::test_that("key numbers name the quantity and bound extreme values", {
  diagnoses <- load_sample("diagnoses")
  kappa <- run_analysis("agreement", diagnoses, list(rater_1 = "new_orleans_neurologist", rater_2 = "winnipeg_neurologist"), list())
  testthat::expect_identical(key_numbers(kappa)[[1]]$label, "Cohen's kappa")
  iris_data <- load_sample("iris")
  tree <- run_analysis("classification_tree", iris_data, list(outcome = "Species", predictors = names(iris_data)[1:4]), list())
  labels <- vapply(key_numbers(tree), `[[`, character(1), "label")
  testthat::expect_true("Accuracy" %in% labels)
  testthat::expect_false(any(grepl("Holdout rows", labels, fixed = TRUE)))
  testthat::expect_identical(result_verdict(tree)$label, "Holdout score")
  ranks <- run_analysis("kruskal_wallis", iris_data, list(outcome = "Sepal.Length", group = "Species"), list())
  testthat::expect_identical(key_numbers(ranks)[[1]]$label, "Test statistic")
  fake <- list(method = "logistic_regression", n_used = 10, n_omitted = 0, conf_level = 0.95,
               estimates = data.frame(Term = "x", Estimate = 2.3e18, Lower = 4e10, Upper = 1e26))
  testthat::expect_identical(key_numbers(fake)[[1]]$value, "more than 1,000,000")
})

testthat::test_that("the result page shows the script it would download", {
  tooth <- load_sample("tooth")
  result <- run_analysis("independent_t", tooth, list(outcome = "len", group = "supp"), list())
  page <- as.character(result_tabs_ui(result, build_narrative(result), build_accessible_explanation(result)))
  testthat::expect_match(page, "id=\"result-tab-code\"", fixed = TRUE)
  testthat::expect_match(page, "id=\"result-panel-code\"", fixed = TRUE)
  testthat::expect_match(page, "data-copy-target=\"result-code\"", fixed = TRUE)
  testthat::expect_match(page, "t.test(`len` ~ `supp`", fixed = TRUE)
  testthat::expect_match(page, "class=\"key-numbers\"", fixed = TRUE)
  path <- tempfile(fileext = ".R")
  write_reproduction_script(result, path)
  testthat::expect_identical(readLines(path), reproduction_script(result))
})

testthat::test_that("the model setup panel has three steps and the card states both sides", {
  testthat::expect_match(app_text(), "model_step_ui(\"1\", \"Choose how to run it\"", fixed = TRUE)
  testthat::expect_match(app_text(), "model_step_ui(\"2\", \"Get Ollama running\"", fixed = TRUE)
  testthat::expect_match(app_text(), "model_step_ui(\"3\", \"Pick a model\"", fixed = TRUE)
  testthat::expect_match(app_text(), "actionButton(\"machine_check\", \"Check this computer\"", fixed = TRUE)
  card <- as.character(model_card_ui("gemma3:4b", picked = "gemma3:4b", in_use = "gemma3:4b",
                                     installed = "gemma3:4b", fit = "comfortable", recommended = TRUE))
  for (piece in c("Good at: ", "The catch: ", "In use", "Fits comfortably", "Suggested for this computer",
                  "role=\"radio\"", "aria-checked=\"true\"", "data-model=\"gemma3:4b\"")) {
    testthat::expect_match(card, piece, fixed = TRUE)
  }
})
