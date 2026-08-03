testthat::test_that("a clear interval receives restrained wording", {
  result <- result_record(
    "one_sample_t", 40, 0,
    estimates = data.frame(Contrast = "mean minus 0", Estimate = 0.5, Lower = 0.2, Upper = 0.8,
                           Effect = 0.4, Effect_name = "Cohen's d"),
    tests = data.frame(Test = "One-sample t", Statistic = 3.2, df = 39, p = 0.003),
    checks = check_row("Normality", "No clear departure found", "Test detail", "ok")
  )
  text <- paste(narrative_text(build_narrative(result)), collapse = " ")
  testthat::expect_match(text, "provide evidence", fixed = TRUE)
  testthat::expect_false(grepl("proves|causes|guarantees", text, ignore.case = TRUE))
})

testthat::test_that("an interval crossing zero is reported as uncertain", {
  result <- result_record(
    "one_sample_t", 20, 2,
    estimates = data.frame(Contrast = "mean minus 0", Estimate = 0.2, Lower = -0.3, Upper = 0.7,
                           Effect = 0.1, Effect_name = "Cohen's d"),
    tests = data.frame(Test = "One-sample t", Statistic = 0.8, df = 19, p = 0.43),
    checks = check_row("Normality", "No clear departure found", "Test detail", "ok")
  )
  text <- paste(narrative_text(build_narrative(result)), collapse = " ")
  testthat::expect_match(text, "do not separate", fixed = TRUE)
  testthat::expect_match(text, "omitted 2 rows", fixed = TRUE)
})

testthat::test_that("p-value note states what a p-value is not", {
  testthat::expect_match(p_value_note(), "not the probability", fixed = TRUE)
  testthat::expect_match(p_value_note(), "practical importance", fixed = TRUE)
})

testthat::test_that("ratio intervals are judged against one", {
  uncertain <- evidence_sentence(0.40, 0.70, 1.30, null = 1)
  clear <- evidence_sentence(0.01, 1.20, 1.80, null = 1)
  testthat::expect_match(uncertain, "includes", fixed = TRUE)
  testthat::expect_match(clear, "does not include 1", fixed = TRUE)
})

testthat::test_that("everyday explanation uses concrete novice sections", {
  result <- result_record(
    "one_sample_t", 40, 0,
    estimates = data.frame(Contrast = "mean minus 0", Estimate = 0.5, Lower = 0.2, Upper = 0.8,
                           Effect = 0.4, Effect_name = "Cohen's d"),
    tests = data.frame(Test = "One-sample t", Statistic = 3.2, df = 39, p = 0.003),
    checks = check_row("Normality", "No clear departure found", "Test detail", "ok")
  )
  explanation <- build_accessible_explanation(result)

  testthat::expect_match(explanation$title, "everyday words", fixed = TRUE)
  testthat::expect_match(explanation$picture, "number line", fixed = TRUE)
  testthat::expect_match(explanation$boundary, "every person", fixed = TRUE)
  testthat::expect_false(grepl("p-value|null hypothesis", accessible_explanation_text(explanation), ignore.case = TRUE))
})

testthat::test_that("the iris ANOVA answer names its data and observed groups", {
  result <- analyse_one_way_anova(
    iris,
    list(outcome = "Sepal.Length", group = "Species"),
    list(conf_level = 0.95)
  )
  explanation <- build_accessible_explanation(result)

  testthat::expect_match(explanation$bottom_line, "Sepal.Length", fixed = TRUE)
  testthat::expect_match(explanation$bottom_line, "Species", fixed = TRUE)
  for (group in c("setosa", "versicolor", "virginica")) {
    testthat::expect_match(explanation$bottom_line, group, fixed = TRUE)
  }
  for (average in c("5.01", "5.94", "6.59")) {
    testthat::expect_match(explanation$bottom_line, average, fixed = TRUE)
  }
  testthat::expect_match(explanation$bottom_line, "lowest average", fixed = TRUE)
  testthat::expect_match(explanation$bottom_line, "checked every pair", fixed = TRUE)
  testthat::expect_match(explanation$picture, "pale dots are individual rows", fixed = TRUE)
  testthat::expect_false(grepl("overall comparison|fitted term|omnibus", explanation$bottom_line, ignore.case = TRUE))
})

testthat::test_that("local rewrite guard preserves every number", {
  source <- "The estimate is 2.4 from 40 rows."
  testthat::expect_true(model_numbers_match(source, "The estimate is 2.4 and it came from 40 rows."))
  testthat::expect_false(model_numbers_match(source, "Across 40 rows, the estimate is 2.4."))
  testthat::expect_false(model_numbers_match(source, "Across 41 rows, the estimate is 2.4."))
  testthat::expect_false(model_numbers_match(source, "The estimate is 2.4."))
  testthat::expect_false(valid_local_model_name("qwen-cloud"))
})

testthat::test_that("LaTeX escaping preserves commands for original special characters", {
  testthat::expect_identical(escape_latex("95%"), "95\\%")
  testthat::expect_identical(escape_latex("a\\b"), "a\\textbackslash{}b")
  testthat::expect_identical(escape_latex("x_{1}"), "x\\_\\{1\\}")
})

testthat::test_that("Ollama model discovery reads documented response shapes", {
  payload <- list(models = data.frame(
    name = c("qwen3.5:4b", "team/private-model:latest", "example-cloud:latest"),
    stringsAsFactors = FALSE
  ))
  found <- ollama_models_result(payload, 200L)

  testthat::expect_true(found$ok)
  testthat::expect_identical(found$models, c("qwen3.5:4b", "team/private-model:latest"))
  testthat::expect_match(found$message, "2 local models", fixed = TRUE)

  empty <- ollama_models_result(list(models = list()), 200L)
  testthat::expect_true(empty$ok)
  testthat::expect_empty(empty$models)

  failed <- ollama_models_result(list(error = "service unavailable"), 503L)
  testthat::expect_false(failed$ok)
  testthat::expect_match(failed$message, "service unavailable", fixed = TRUE)
})

testthat::test_that("Ollama generation rejects incomplete and malformed replies", {
  completed <- ollama_generation_result(list(response = "READY", done = TRUE), 200L)
  testthat::expect_true(completed$ok)
  testthat::expect_identical(completed$text, "READY")

  unfinished <- ollama_generation_result(list(response = "part", done = FALSE), 200L)
  testthat::expect_false(unfinished$ok)
  testthat::expect_match(unfinished$message, "before it finished", fixed = TRUE)

  missing_model <- ollama_generation_result(list(error = "model not found"), 404L)
  testthat::expect_false(missing_model$ok)
  testthat::expect_match(missing_model$message, "model not found", fixed = TRUE)

  malformed <- ollama_generation_result(list(done = TRUE), 200L)
  testthat::expect_false(malformed$ok)
})

testthat::test_that("Ollama downloads use a literal pull command", {
  spec <- ollama_pull_spec("qwen3.5:4b", "/Applications/Ollama.app/Contents/Resources/ollama")
  testthat::expect_identical(spec$command, "/Applications/Ollama.app/Contents/Resources/ollama")
  testthat::expect_identical(spec$args, c("pull", "qwen3.5:4b"))
  testthat::expect_error(ollama_pull_spec("bad model", "/usr/local/bin/ollama"), "valid local model")
})

testthat::test_that("Ollama generation failures retain the actual cause", {
  body <- ollama_generate_body("Explain this result.", "qwen3.5:4b")
  testthat::expect_false(body$stream)
  testthat::expect_false(body$think)
  testthat::expect_identical(body$keep_alive, "10m")
  testthat::expect_identical(body$options$num_predict, 700)

  timeout <- ollama_generation_failure(
    simpleError("Timeout was reached after 300001 milliseconds"),
    "qwen3.5:4b", 300,
    list(ok = TRUE, models = "qwen3.5:4b")
  )
  testthat::expect_false(timeout$ok)
  testthat::expect_match(timeout$message, "did not finish within 5 minutes", fixed = TRUE)

  stopped_timeout <- ollama_generation_failure(
    simpleError("Timeout was reached after 300001 milliseconds"),
    "qwen3.5:4b", 300, list(ok = FALSE, models = character(0))
  )
  testthat::expect_match(stopped_timeout$message, "could not recheck the local service", fixed = TRUE)

  missing <- ollama_generation_failure(
    simpleError("request failed"), "qwen3.5:4b", 300,
    list(ok = TRUE, models = "llama3.2:3b")
  )
  testthat::expect_match(missing$message, "qwen3.5:4b is not installed", fixed = TRUE)

  refused <- ollama_generation_failure(
    simpleError("Failed to connect to 127.0.0.1 port 11434"),
    "qwen3.5:4b", 300, list(ok = FALSE, models = character(0))
  )
  testthat::expect_match(refused$message, "generation service could not be reached", fixed = TRUE)
})
