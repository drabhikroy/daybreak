# The local model runtime -----------------------------------------------------
#
# Nothing here reaches the network or starts a program. The pieces that decide
# what would be downloaded, whether it is intact, and where it would go are
# plain functions, and those are what is tested.

testthat::test_that("each platform is matched to the right release file", {
  mac <- host_platform(c(sysname = "Darwin"), "aarch64")
  testthat::expect_identical(mac$asset, "ollama-darwin.tgz")
  testthat::expect_identical(host_platform(c(sysname = "Linux"), "x86_64")$asset, "ollama-linux-amd64.tar.zst")
  testthat::expect_identical(host_platform(c(sysname = "Linux"), "aarch64")$asset, "ollama-linux-arm64.tar.zst")
  testthat::expect_true(is.na(host_platform(c(sysname = "Windows"), "x86_64")$asset))
  testthat::expect_true(is.na(host_platform(c(sysname = "Plan9"), "x86_64")$asset))
})

testthat::test_that("a private copy is refused where no archive exists", {
  windows <- host_platform(c(sysname = "Windows"), "x86_64")
  result <- managed_install(platform = windows, root = tempfile("never-"))
  testthat::expect_false(result$ok)
  testthat::expect_match(result$message, "installer", fixed = TRUE)
})

testthat::test_that("a published checksum is read only for the exact file name", {
  digest <- paste(rep("ab", 32), collapse = "")
  other <- paste(rep("cd", 32), collapse = "")
  listing <- paste(c(paste0(other, "  ./ollama-darwin.tgz.sig"), paste0(digest, "  ./ollama-darwin.tgz"),
                     paste0(other, "  ./Ollama-darwin.zip")), collapse = "\n")
  testthat::expect_identical(published_sha256("ollama-darwin.tgz", listing), digest)
  testthat::expect_true(is.na(published_sha256("ollama-linux-amd64.tar.zst", listing)))
  testthat::expect_true(is.na(published_sha256("ollama-darwin.tgz", "not-a-digest  ./ollama-darwin.tgz")))
  testthat::expect_true(is.na(published_sha256("ollama-darwin.tgz", "")))
})

testthat::test_that("file_sha256 returns the known digest of a known file", {
  path <- tempfile()
  writeBin(charToRaw("abc"), path)
  digest <- file_sha256(path)
  testthat::skip_if(is.na(digest), "no SHA-256 tool on this computer")
  testthat::expect_identical(tolower(digest), "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
})

testthat::test_that("model fit and the suggestion follow reported memory", {
  small <- list(ram_gb = 7.7, cores = 4, accelerated = FALSE, os = "macOS")
  testthat::expect_identical(unname(model_fit(small)[["llama3.2:3b"]]), "workable")
  testthat::expect_identical(unname(model_fit(small)[["llama3.1:8b"]]), "too large")
  testthat::expect_identical(recommended_model(small), names(LOCAL_MODEL_CATALOG)[1])
  middle <- list(ram_gb = 16, cores = 8, accelerated = TRUE, os = "macOS")
  testthat::expect_identical(unname(model_fit(middle)[["qwen3.5:4b"]]), "comfortable")
  testthat::expect_identical(unname(model_fit(middle)[["llama3.1:8b"]]), "workable")
  testthat::expect_identical(recommended_model(middle), "qwen3.5:4b")
  testthat::expect_identical(recommended_model(list(ram_gb = 64, cores = 12, accelerated = TRUE, os = "macOS")), "llama3.1:8b")
  unknown <- list(ram_gb = NA_real_, cores = NA_integer_, accelerated = FALSE, os = "this system")
  testthat::expect_true(all(model_fit(unknown) == "unknown"))
  testthat::expect_identical(recommended_model(unknown), names(LOCAL_MODEL_CATALOG)[1])
  testthat::expect_match(machine_sentence(unknown), "unknown", fixed = TRUE)
})

testthat::test_that("every catalog model states a strength, a catch, and a memory need", {
  for (id in names(LOCAL_MODEL_CATALOG)) {
    model <- LOCAL_MODEL_CATALOG[[id]]
    testthat::expect_true(valid_local_model_name(id), info = id)
    for (field in c("label", "size", "speed", "good", "tradeoff")) {
      testthat::expect_true(is.character(model[[field]]) && nzchar(model[[field]]), info = paste(id, field))
    }
    testthat::expect_true(is.numeric(model$min_ram) && model$min_ram >= 4, info = id)
  }
  needs <- vapply(LOCAL_MODEL_CATALOG, function(model) model$min_ram, numeric(1))
  testthat::expect_false(is.unsorted(needs))
})

testthat::test_that("the model in use follows the saved choice only while it is installed", {
  installed <- c("gemma3:4b", "mistral:latest")
  testthat::expect_identical(active_local_model("gemma3:4b", installed), "gemma3:4b")
  testthat::expect_identical(active_local_model("qwen3.5:4b", installed), "gemma3:4b")
  testthat::expect_identical(active_local_model("", "mistral:latest"), "mistral:latest")
  testthat::expect_identical(active_local_model("gemma3:4b", character(0)), "")
  testthat::expect_identical(other_installed_models(installed), "mistral:latest")
  testthat::expect_identical(other_installed_models(c("ok:1", "bad name; rm -rf", "x-cloud")), "ok:1")
})

testthat::test_that("names that could act as commands or options are not model names", {
  for (name in c("-rf", "--help", "a b", "a;b", "a|b", "$(id)", "`id`", "a\nb", "", "llama3.2:3b:extra")) {
    testthat::expect_false(valid_local_model_name(name), info = name)
  }
  testthat::expect_false(remove_model("--all")$ok)
  testthat::expect_error(ollama_pull_spec("x; id", "/usr/bin/true"), "valid local model")
  spec <- ollama_pull_spec("llama3.2:3b", "/usr/bin/true")
  testthat::expect_identical(spec$args, c("pull", "llama3.2:3b"))
})

testthat::test_that("preferences survive a round trip and reject what they should not hold", {
  path <- tempfile(fileext = ".dcf")
  testthat::expect_identical(read_model_preferences(path), list(route = "system", model = ""))
  testthat::expect_true(write_model_preferences(list(route = "managed", model = "gemma3:4b"), path))
  testthat::expect_identical(read_model_preferences(path), list(route = "managed", model = "gemma3:4b"))
  writeLines(c("route: sideways", "model: a;b"), path)
  testthat::expect_identical(read_model_preferences(path), list(route = "system", model = ""))
  writeLines("this is not the expected format", path)
  testthat::expect_identical(read_model_preferences(path)$route, "system")
})

testthat::test_that("removing the private copy touches only its own folder", {
  root <- tempfile("runtime-")
  testthat::expect_false(remove_runtime(root)$ok)
  dir.create(file.path(root, "bin"), recursive = TRUE)
  file.create(file.path(root, "bin", "ollama"))
  beside <- paste0(root, "-keep")
  dir.create(beside)
  testthat::expect_true(nzchar(managed_binary(root)))
  testthat::expect_true(remove_runtime(root)$ok)
  testthat::expect_false(dir.exists(root))
  testthat::expect_true(dir.exists(beside))
})

testthat::test_that("download progress is stripped of terminal control codes", {
  raw <- c("\033[?25lpulling manifest \033[K", "pulling 6a0746a1ec1a...  42% ▕██   ▏ 1.2 GB/2.0 GB", "   ", "success")
  cleaned <- pull_progress_lines(raw)
  testthat::expect_identical(cleaned[1], "pulling manifest")
  testthat::expect_identical(cleaned[length(cleaned)], "success")
  testthat::expect_false(any(grepl("\033", cleaned, fixed = TRUE)))
  testthat::expect_false(any(cleaned == ""))
})

testthat::test_that("the server address and the release address are fixed in source", {
  testthat::expect_identical(OLLAMA_BASE_URL, "http://127.0.0.1:11434")
  testthat::expect_match(OLLAMA_RELEASE_URL, "^https://github.com/ollama/ollama/releases/latest/download/$")
  source_text <- paste(readLines(project_path("R/runtime.R"), warn = FALSE), collapse = "\n")
  testthat::expect_match(source_text, "OLLAMA_HOST = \"127.0.0.1:11434\"", fixed = TRUE)
  testthat::expect_false(grepl("shQuote\\(name\\)|system\\(", source_text))
})

testthat::test_that("a model that returns its instructions is not shown as a rewrite", {
  tooth <- load_sample("tooth")
  result <- run_analysis("independent_t", tooth, list(outcome = "len", group = "supp"), list())
  explanation <- build_accessible_explanation(result)
  source_text <- local_model_source(explanation)
  # The source itself is a faithful answer: five headings, the same numbers.
  testthat::expect_true(check_local_rewrite(explanation, source_text)$ok)
  echoed <- check_local_rewrite(explanation, build_local_model_prompt(explanation))
  testthat::expect_false(echoed$ok)
  testthat::expect_match(echoed$message, "returned its instructions", fixed = TRUE)
  testthat::expect_false(check_local_rewrite(explanation, paste(source_text, source_text, sep = "\n"))$ok)
  padded <- paste(source_text, paste(rep("More words that say nothing new.", 400), collapse = " "))
  testthat::expect_false(check_local_rewrite(explanation, padded)$ok)
  changed <- sub("[0-9]", "9", sub("([0-9])\\.([0-9])", "\\1.\\2 1", source_text))
  testthat::expect_match(check_local_rewrite(explanation, changed)$message, "added, removed, or changed a number", fixed = TRUE)
})
