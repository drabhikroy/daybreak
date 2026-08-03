hex_rgb <- function(x) grDevices::col2rgb(x)[, 1] / 255
linear_channel <- function(x) ifelse(x <= 0.04045, x / 12.92, ((x + 0.055) / 1.055)^2.4)
luminance <- function(x) sum(c(0.2126, 0.7152, 0.0722) * linear_channel(hex_rgb(x)))
contrast <- function(a, b) {
  values <- sort(c(luminance(a), luminance(b)), decreasing = TRUE)
  (values[1] + 0.05) / (values[2] + 0.05)
}

testthat::test_that("main text clears WCAG AA contrast", {
  pairs <- list(c("#f7f4ec", "#0d141b"), c("#aab7c3", "#0d141b"),
                c("#202a32", "#f7f3ea"), c("#5c6872", "#f7f3ea"),
                c("#1e1a0e", "#f6c453"), c("#ffffff", "#9c6500"))
  for (pair in pairs) testthat::expect_gte(contrast(pair[1], pair[2]), 4.5, info = paste(pair, collapse = " on "))
})

testthat::test_that("interface carries keyboard and reduced-motion rules", {
  css <- paste(readLines("www/app.css", warn = FALSE), collapse = "\n")
  js <- paste(readLines("www/app.js", warn = FALSE), collapse = "\n")
  testthat::expect_match(css, "prefers-reduced-motion", fixed = TRUE)
  testthat::expect_match(css, ":focus-visible", fixed = TRUE)
  testthat::expect_match(js, "ArrowRight", fixed = TRUE)
  testthat::expect_match(js, "Escape", fixed = TRUE)
})

testthat::test_that("decorative interval marks use explicit HTML tags", {
  helper_lines <- readLines("R/ui_helpers.R", warn = FALSE)
  interval_start <- grep('class = "mini-interval"', helper_lines, fixed = TRUE)
  interval_code <- paste(helper_lines[interval_start + 0:3], collapse = " ")

  testthat::expect_length(interval_start, 1L)
  testthat::expect_false(grepl("(?<!tags\\$)\\bi\\s*\\(", interval_code, perl = TRUE))
  testthat::expect_false(grepl("(?<!tags\\$)\\bb\\s*\\(", interval_code, perl = TRUE))
  testthat::expect_match(interval_code, "tags$i()", fixed = TRUE)
  testthat::expect_match(interval_code, "tags$b()", fixed = TRUE)
})

testthat::test_that("less common HTML elements use explicit tag constructors", {
  source_text <- paste(c(readLines("app.R", warn = FALSE),
                         readLines("R/ui_helpers.R", warn = FALSE)), collapse = "\n")

  testthat::expect_false(grepl("(?<!tags\\$)\\b(small|i|b)\\s*\\(", source_text, perl = TRUE))
})

testthat::test_that("the plot output remains compatible with earlier Shiny releases", {
  ui_helpers <- paste(readLines("R/ui_helpers.R", warn = FALSE), collapse = "\n")

  testthat::expect_false(grepl('plotOutput\\([^)]*alt\\s*=', ui_helpers, perl = TRUE))
  testthat::expect_match(ui_helpers, '`aria-label` = "Statistical result figure"', fixed = TRUE)
})

testthat::test_that("browser controls and help are present", {
  app <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
  helpers <- paste(readLines("R/ui_helpers.R", warn = FALSE), collapse = "\n")

  testthat::expect_match(app, 'includeScript("www/app.js")', fixed = TRUE)
  testthat::expect_match(app, 'href = "#methods-panel"', fixed = TRUE)
  testthat::expect_match(app, 'href = "#formats-panel"', fixed = TRUE)
  testthat::expect_match(app, 'href = "#help-panel"', fixed = TRUE)
  testthat::expect_match(app, 'href = "#models-panel"', fixed = TRUE)
  helpers <- paste(readLines("R/ui_helpers.R", warn = FALSE), collapse = "\n")

  testthat::expect_match(helpers, 'actionButton("test_local_model"', fixed = TRUE)
  testthat::expect_match(app, 'observeEvent(input$test_local_model', fixed = TRUE)
  testthat::expect_match(app, 'href = "#session-panel"', fixed = TRUE)
  testthat::expect_match(app, 'output$download_session <- downloadHandler', fixed = TRUE)
  testthat::expect_match(app, 'Save session (.daybreak.rds)', fixed = TRUE)
  testthat::expect_match(app, 'contentType = "application/x-r-rds"', fixed = TRUE)
  testthat::expect_match(app, 'observeEvent(input$open_session', fixed = TRUE)
  testthat::expect_match(app, 'read_daybreak_session', fixed = TRUE)
  testthat::expect_match(app, 'href = "#imputation-panel"', fixed = TRUE)
  testthat::expect_match(app, 'class = "brand-symbol"', fixed = TRUE)
  testthat::expect_match(app, 'href = "icons/daybreak-icon-transparent.svg"', fixed = TRUE)
  testthat::expect_match(app, 'class = "workflow-card"', fixed = TRUE)
  testthat::expect_match(app, 'uiOutput("data_preview_inline")', fixed = TRUE)
  testthat::expect_match(helpers, "methods_directory_ui", fixed = TRUE)
  testthat::expect_match(helpers, "static_dialog_ui", fixed = TRUE)
  testthat::expect_match(helpers, "landing_page_ui", fixed = TRUE)
  testthat::expect_match(helpers, "help_ui", fixed = TRUE)
  testthat::expect_match(helpers, "Machine learning", fixed = TRUE)
  testthat::expect_match(helpers, "imputation_ui", fixed = TRUE)
})

testthat::test_that("the opening route is usable before a file is loaded", {
  app <- paste(readLines("app.R", warn = FALSE), collapse = "\n")

  testthat::expect_false(grepl('getRversion\\(\\) < "4.3.0"', app))
  testthat::expect_false(grepl('uiOutput\\("source_controls"\\)', app))
  testthat::expect_false(grepl('radioButtons("data_source"', app, fixed = TRUE))
  testthat::expect_match(app, 'source = "upload"', fixed = TRUE)
  testthat::expect_match(app, 'choices = sample_choices()', fixed = TRUE)
  testthat::expect_match(app, 'actionButton("load_sample"', fixed = TRUE)
  testthat::expect_match(app, 'selectInput("method"', fixed = TRUE)
  testthat::expect_match(app, 'selectize = FALSE', fixed = TRUE)
  testthat::expect_match(app, 'div(id = "landing-page", landing_page_ui())', fixed = TRUE)
  testthat::expect_match(app, 'uiOutput("landing_visibility_style")', fixed = TRUE)
})

testthat::test_that("everyday and statistical readings are separate", {
  helpers <- paste(readLines("R/ui_helpers.R", warn = FALSE), collapse = "\n")
  app <- paste(readLines("app.R", warn = FALSE), collapse = "\n")

  testthat::expect_match(helpers, "Everyday explanation", fixed = TRUE)
  testthat::expect_match(helpers, "Statistical description", fixed = TRUE)
  testthat::expect_match(helpers, 'narrative_ui(narrative)', fixed = TRUE)
  testthat::expect_match(helpers, '"Exact values"', fixed = TRUE)
  testthat::expect_match(helpers, 'class = "result-reading-sequence"', fixed = TRUE)
  testthat::expect_false(grepl('class = "result-overview-grid"', helpers, fixed = TRUE))
  testthat::expect_match(helpers, 'uiOutput("local_restatement")', fixed = TRUE)
  testthat::expect_match(app, "build_accessible_explanation(result)", fixed = TRUE)
})

testthat::test_that("file controls and chart labels have breathing room", {
  css <- paste(readLines("www/app.css", warn = FALSE), collapse = "\n")
  plots <- paste(readLines("R/plots.R", warn = FALSE), collapse = "\n")

  testthat::expect_match(css, ".btn {\n  display: inline-flex", fixed = TRUE)
  testthat::expect_match(css, ".btn-file {\n  height: 44px", fixed = TRUE)
  testthat::expect_match(css, ".help-shell {\n  display: grid;\n  gap: 12px", fixed = TRUE)
  testthat::expect_match(plots, "axis.title.x = ggplot2::element_text", fixed = TRUE)
  testthat::expect_match(plots, "margin = ggplot2::margin(t = 16)", fixed = TRUE)
  testthat::expect_match(plots, "margin = ggplot2::margin(r = 16)", fixed = TRUE)
})

testthat::test_that("color vision choices use two fixed left-justified columns", {
  app <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
  css <- paste(readLines("www/app.css", warn = FALSE), collapse = "\n")

  testthat::expect_match(app, 'class = "palette-name"', fixed = TRUE)
  testthat::expect_match(css, "grid-template-columns: 72px minmax(0, 1fr)", fixed = TRUE)
  testthat::expect_match(css, "width: min(100%, 272px)", fixed = TRUE)
  testthat::expect_match(css, ".palette-name", fixed = TRUE)
})

testthat::test_that("footer states the version and license", {
  app <- paste(readLines("app.R", warn = FALSE), collapse = "\n")

  version_file <- paste(readLines("R/version.R", warn = FALSE), collapse = "\n")

  testthat::expect_match(version_file, 'DAYBREAK_VERSION <- "0.10.0"', fixed = TRUE)
  testthat::expect_match(app, "DAYBREAK_VERSION", fixed = TRUE)
  testthat::expect_match(app, "Copyright Abhik Roy", fixed = TRUE)
  testthat::expect_match(app, "PolyForm Noncommercial License 1.0.0", fixed = TRUE)
})

testthat::test_that("every method card includes a distinct statistical figure", {
  helpers <- paste(readLines("R/ui_helpers.R", warn = FALSE), collapse = "\n")

  testthat::expect_match(helpers, "method_visual_ui", fixed = TRUE)
  testthat::expect_match(helpers, 'class = paste("method-mini-visual"', fixed = TRUE)
  testthat::expect_match(helpers, 'method_visual_ui(x)', fixed = TRUE)
  testthat::expect_false(grepl("<text", helpers, fixed = TRUE))
  for (method in ANALYSES) {
    rendered <- as.character(method_visual_ui(method))
    testthat::expect_false(grepl("<text", rendered, fixed = TRUE), info = method$id)
    testthat::expect_match(rendered, "<svg", fixed = TRUE, info = method$id)
  }
})

testthat::test_that("analysis failures have plain guidance and a folded R message", {
  error <- plain_analysis_error(
    "This analysis needs at least 20 complete rows. The current choices provide 4.",
    "linear_regression", roles = list(outcome = "score", predictors = c("age", "group")),
    data_name = "survey.csv"
  )
  rendered <- as.character(analysis_error_ui(error))

  testthat::expect_match(error$reason, "Too few usable rows", fixed = TRUE)
  testthat::expect_match(error$specific, "provide 4", fixed = TRUE)
  testthat::expect_match(error$context, "survey.csv", fixed = TRUE)
  testthat::expect_match(error$context, "Numeric outcome: score", fixed = TRUE)
  testthat::expect_match(rendered, "Why it stopped", fixed = TRUE)
  testthat::expect_match(rendered, "What that means", fixed = TRUE)
  testthat::expect_match(rendered, "What to try", fixed = TRUE)
  testthat::expect_match(rendered, "Show the exact R message", fixed = TRUE)
})

testthat::test_that("unfamiliar analysis failures remain visible instead of becoming generic", {
  error <- plain_analysis_error("The custom model found three incompatible rows: 7, 12, and 19.", "sem")
  rendered <- as.character(analysis_error_ui(error))

  testthat::expect_match(rendered, "three incompatible rows: 7, 12, and 19", fixed = TRUE)
  testthat::expect_match(as.character(analysis_error_notification(error)), "three incompatible rows", fixed = TRUE)
})

testthat::test_that("the launcher checks the complete dependency set", {
  app <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
  installer <- paste(readLines("install.R", warn = FALSE), collapse = "\n")
  launcher <- paste(readLines("run_daybreak.R", warn = FALSE), collapse = "\n")

  testthat::expect_match(installer, '"glmnet"', fixed = TRUE)
  testthat::expect_match(installer, '"e1071"', fixed = TRUE)
  testthat::expect_match(installer, '"dbscan"', fixed = TRUE)
  testthat::expect_match(installer, '"lmerTest"', fixed = TRUE)
  testthat::expect_match(installer, "R_user_dir", fixed = TRUE)
  testthat::expect_match(installer, ".libPaths", fixed = TRUE)
  testthat::expect_match(installer, "core_missing", fixed = TRUE)
  testthat::expect_match(installer, "optional_missing", fixed = TRUE)
  testthat::expect_match(installer, "daybreak.dependencies.checked", fixed = TRUE)
  testthat::expect_match(app, 'source("install.R", chdir = TRUE)', fixed = TRUE)
  testthat::expect_match(launcher, 'file.path(app_dir, "install.R")', fixed = TRUE)
})

testthat::test_that("every package named by app code is covered by the installer", {
  files <- c("app.R", list.files("R", pattern = "\\.R$", full.names = TRUE),
             list.files("report", pattern = "\\.Rmd$", full.names = TRUE))
  source_text <- paste(unlist(lapply(files, readLines, warn = FALSE)), collapse = "\n")
  namespace_hits <- regmatches(source_text, gregexpr("[A-Za-z][A-Za-z0-9.]*::", source_text, perl = TRUE))[[1]]
  namespace_packages <- sub("::$", "", namespace_hits)
  library_hits <- regmatches(source_text, gregexpr("library\\([A-Za-z][A-Za-z0-9.]*\\)", source_text, perl = TRUE))[[1]]
  library_packages <- sub("^library\\(|\\)$", "", library_hits)
  used <- unique(c(namespace_packages, library_packages))
  base_packages <- c("datasets", "grDevices", "methods", "parallel", "stats", "tools", "utils")
  used <- setdiff(used, base_packages)
  installer <- paste(readLines("install.R", warn = FALSE), collapse = "\n")

  missing <- used[!vapply(used, function(package) {
    grepl(paste0('"', package, '"'), installer, fixed = TRUE)
  }, logical(1))]
  testthat::expect_empty(missing)
})

testthat::test_that("the connection warning has no speculative timer", {
  js <- paste(readLines("www/app.js", warn = FALSE), collapse = "\n")

  testthat::expect_false(grepl("__daybreakConnected", js, fixed = TRUE))
  testthat::expect_match(js, 'on("shiny:disconnected.daybreak"', fixed = TRUE)
})
