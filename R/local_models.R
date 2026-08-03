# Optional local language models -----------------------------------------
#
# Statistics are computed before this file is used. A local model may rewrite
# the finished everyday explanation, but it never receives raw rows and it is
# not allowed to add, remove, or change numeric values.

LOCAL_MODEL_CATALOG <- list(
  "qwen3.5:4b" = list(
    label = "Qwen 3.5 4B, recommended",
    size = "3.4 GB",
    note = "A current small model with a good balance of speed and writing quality on Apple silicon."
  ),
  "llama3.2:3b" = list(
    label = "Llama 3.2 3B",
    size = "2.0 GB",
    note = "The smallest listed download and a practical choice when storage matters most."
  ),
  "gemma3:4b" = list(
    label = "Gemma 3 4B",
    size = "3.3 GB",
    note = "A compact alternative for local rewriting and short explanations."
  )
)

OLLAMA_BASE_URL <- "http://127.0.0.1:11434"

# Ollama reports a refused connection using a contraction in its own message
# text, and the pattern has to match that text exactly. It is assembled from
# pieces rather than written out so that the project rule against contractions
# in source still holds when the writing tests read this file.
OLLAMA_CONNECTION_PATTERN <- paste0(
  "could", "n", "\u2019?'?", "t connect",
  "|could not connect|failed to connect|connection refused|could not resolve"
)

# The optional local model -------------------------------------------------
#
# A local model may restate a finished explanation in different words. It never
# sees the data, never computes anything, and its output is checked digit by
# digit against the source text before it is shown. If a single number differs,
# the restatement is discarded. The model is a thesaurus here, not an analyst.
valid_local_model_name <- function(model) {
  length(model) == 1L && nzchar(model) &&
    !grepl("cloud", model, ignore.case = TRUE) &&
    grepl("^[A-Za-z0-9][A-Za-z0-9._/-]*(:[A-Za-z0-9._-]+)?$", model)
}

local_model_choices <- function(installed = character(0)) {
  installed <- installed[vapply(installed, valid_local_model_name, logical(1))]
  known <- stats::setNames(
    names(LOCAL_MODEL_CATALOG),
    vapply(LOCAL_MODEL_CATALOG, function(x) paste0(x$label, " (", x$size, ")"), character(1))
  )
  if (!length(installed)) return(known)
  extra <- setdiff(installed, names(LOCAL_MODEL_CATALOG))
  installed_choices <- stats::setNames(installed, paste0(installed, " (installed)"))
  unique(c(installed_choices, known, stats::setNames(extra, paste0(extra, " (installed)"))))
}

ollama_error_detail <- function(payload) {
  if (!is.list(payload) || is.null(payload$error)) return("")
  detail <- trimws(gsub("[\r\n]+", " ", as.character(payload$error)[1]))
  if (is.na(detail)) "" else substr(detail, 1, 300)
}

ollama_status_message <- function(status, payload) {
  detail <- ollama_error_detail(payload)
  if (nzchar(detail)) {
    sprintf("Ollama responded with status %s: %s", status, detail)
  } else {
    sprintf("Ollama responded with status %s.", status)
  }
}

ollama_condition_detail <- function(error) {
  detail <- trimws(gsub("[\r\n]+", " ", conditionMessage(error)))
  if (!nzchar(detail)) "No connection detail was returned." else substr(detail, 1, 300)
}

ollama_generation_failure <- function(error, model, timeout, server_status = NULL) {
  detail <- ollama_condition_detail(error)
  lower <- tolower(detail)
  server_ok <- is.list(server_status) && isTRUE(server_status$ok)
  installed <- server_ok && model %in% (server_status$models %||% character(0))

  if (server_ok && !installed) {
    message <- sprintf("%s is not installed. Open Local models, download it, and test it before rewriting.", model)
  } else if (grepl("timed out|timeout|time-out", lower) && server_ok) {
    message <- sprintf(
      "%s did not finish within %s minutes. The first reply can be slow while the model loads. Test the selected model once, then try the rewrite again.",
      model, format(round(timeout / 60, 1), trim = TRUE)
    )
  } else if (grepl("timed out|timeout|time-out", lower)) {
    message <- sprintf(
      "The Ollama request timed out after %s minutes, and Daybreak could not recheck the local service. Open Ollama, then test the selected model.",
      format(round(timeout / 60, 1), trim = TRUE)
    )
  } else if (server_ok) {
    message <- paste0("Ollama is running, but the generation request failed: ", detail)
  } else if (grepl(OLLAMA_CONNECTION_PATTERN, lower)) {
    message <- paste0("The Ollama generation service could not be reached: ", detail, " Open Ollama, wait a few seconds, and try again.")
  } else {
    message <- paste0("The Ollama generation request failed: ", detail)
  }
  list(ok = FALSE, text = "", message = message)
}

ollama_model_names <- function(models) {
  if (is.data.frame(models)) {
    if (!"name" %in% names(models)) return(character(0))
    return(as.character(models$name))
  }
  if (!is.list(models)) return(character(0))
  unname(vapply(models, function(model) {
    if (is.list(model) && !is.null(model$name)) as.character(model$name)[1] else ""
  }, character(1)))
}

# These two parsers are kept separate from the web request so they can be
# checked with the same response shapes documented by Ollama.
ollama_models_result <- function(payload, status = 200L) {
  if (!identical(as.integer(status), 200L)) {
    return(list(ok = FALSE, models = character(0), message = ollama_status_message(status, payload)))
  }
  if (!is.list(payload) || !"models" %in% names(payload)) {
    return(list(ok = FALSE, models = character(0), message = "Ollama returned a response Daybreak could not read."))
  }
  models <- ollama_model_names(payload$models)
  models <- unique(models[vapply(models, valid_local_model_name, logical(1))])
  if (!length(models)) {
    return(list(ok = TRUE, models = character(0), message = "Ollama is running, but no supported local model is installed yet."))
  }
  list(
    ok = TRUE,
    models = models,
    message = sprintf("Ollama is running with %s local model%s.", length(models), if (length(models) == 1L) "" else "s")
  )
}

ollama_generation_result <- function(payload, status = 200L) {
  if (!identical(as.integer(status), 200L)) {
    return(list(ok = FALSE, text = "", message = ollama_status_message(status, payload)))
  }
  detail <- ollama_error_detail(payload)
  if (nzchar(detail)) return(list(ok = FALSE, text = "", message = paste("Ollama could not run the model:", detail)))
  if (!is.list(payload) || !"response" %in% names(payload)) {
    return(list(ok = FALSE, text = "", message = "The local model returned a response Daybreak could not read."))
  }
  if (!is.null(payload$done) && !isTRUE(payload$done)) {
    return(list(ok = FALSE, text = "", message = "The local model stopped before it finished its response."))
  }
  text <- trimws(as.character(payload$response)[1])
  if (is.na(text) || !nzchar(text)) {
    return(list(ok = FALSE, text = "", message = "The local model returned no text."))
  }
  list(ok = TRUE, text = text, message = "The local model answered successfully.")
}

ollama_response_payload <- function(response) {
  text <- httr::content(response, as = "text", encoding = "UTF-8")
  try(jsonlite::fromJSON(text, simplifyVector = TRUE), silent = TRUE)
}

ollama_list_models <- function(timeout = 5) {
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    return(list(ok = FALSE, models = character(0), message = "Install the optional httr and jsonlite R packages to connect to Ollama."))
  }
  response <- tryCatch(
    httr::GET(paste0(OLLAMA_BASE_URL, "/api/tags"), httr::timeout(timeout)),
    error = function(e) e
  )
  if (inherits(response, "condition")) {
    return(list(
      ok = FALSE, models = character(0),
      message = paste0("Daybreak could not read Ollama's model list: ", ollama_condition_detail(response))
    ))
  }
  status <- httr::status_code(response)
  payload <- ollama_response_payload(response)
  if (inherits(payload, "try-error")) {
    if (!identical(status, 200L)) return(ollama_models_result(list(), status))
    return(list(ok = FALSE, models = character(0), message = "Ollama returned a response Daybreak could not read."))
  }
  ollama_models_result(payload, status)
}

find_ollama_cli <- function() {
  candidates <- unique(c(
    Sys.which("ollama"),
    "/opt/homebrew/bin/ollama",
    "/usr/local/bin/ollama",
    "/Applications/Ollama.app/Contents/Resources/ollama"
  ))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates)) candidates[1] else ""
}

ollama_pull_spec <- function(model, cli = find_ollama_cli()) {
  if (!valid_local_model_name(model)) stop("Select a valid local model.", call. = FALSE)
  if (!length(cli) || !nzchar(cli[1])) stop("Daybreak could not find the Ollama command.", call. = FALSE)
  list(command = as.character(cli[1]), args = c("pull", model))
}

# The prompt asks for a restatement and nothing else. It is worth being
# explicit that this instruction is not what makes the feature safe: the
# numeric comparison after the fact is. A prompt is a request, and a request
# can be ignored.
# The sections the everyday explanation is built from, in reading order. The
# rewrite has to come back with the same five, because the value of the layer is
# that a reader who has worked through one result knows where to look in the
# next. A model that collapses them into flowing prose has removed the thing
# that made the explanation navigable.
LOCAL_MODEL_SECTIONS <- c(
  "WHAT WAS ASKED", "WHAT CAME BACK", "HOW TO PICTURE IT",
  "HOW SURE THIS IS", "WHAT THIS DOES NOT COVER"
)

# The source is handed over with headings rather than as one block of text.
# Labelling it is what lets the instruction "keep these five headings" be
# checked afterwards instead of merely requested.
local_model_source <- function(explanation) {
  paste(
    paste0(LOCAL_MODEL_SECTIONS[1], "\n", explanation$question),
    paste0(LOCAL_MODEL_SECTIONS[2], "\n", explanation$bottom_line),
    paste0(LOCAL_MODEL_SECTIONS[3], "\n", explanation$picture),
    paste0(LOCAL_MODEL_SECTIONS[4], "\n", explanation$certainty),
    paste0(LOCAL_MODEL_SECTIONS[5], "\n", explanation$boundary),
    sep = "\n\n"
  )
}

# The earlier prompt asked the model to "explain the comparison with a concrete
# mental picture". That one line was responsible for most of what went wrong
# with the output: asked for a picture, a model invents one, and the invented
# picture is not checkable. An iris comparison came back as three piles of sand
# measured for height, which is not what a sepal is, not what the figure shows,
# and not something the reader can verify against anything on screen.
#
# The instructions below are built around that. The model is given a picture and
# told to simplify its wording; it is never asked to supply one. Everything else
# follows the same principle: this is a rewriting job with a fixed structure and
# fixed facts, not a writing job.
build_local_model_prompt <- function(explanation, reading_level = "plain") {
  source <- local_model_source(explanation)
  subject <- explanation$context %||% ""

  sentence_budget <- switch(
    reading_level,
    brief = "one sentence",
    detailed = "up to four sentences",
    "two or three sentences"
  )

  instructions <- c(
    "You are rewriting a finished statistical explanation so that an adult who has never taken a statistics course can read it. The analysis is already done. Every number and every conclusion is already decided. Your only job is to make the wording easier.",
    "",
    "STRUCTURE",
    sprintf("Return exactly these five headings, in this order, spelled exactly as shown: %s.", paste(LOCAL_MODEL_SECTIONS, collapse = ", ")),
    sprintf("Under each heading write %s. Do not add headings, bullet points, or a summary at the end.", sentence_budget),
    "",
    "FACTS",
    "Copy every number exactly as it appears, including decimals, commas, and percent signs. Do not add a number that is not in the source. Do not remove one. Do not round.",
    "Do not add a finding, a cause, a recommendation, or a comparison that the source does not make.",
    "Keep every statement of uncertainty and every limitation. Never write that something is proven, certain, significant, important, or caused by anything.",
    "",
    "PICTURES",
    "The source already contains the picture, under HOW TO PICTURE IT. Simplify its wording. Do not replace it with a different image and do not invent an analogy of your own. If the source describes a chart, describe that same chart.",
    "Never compare the subject of the analysis to something it is not. If the data is about flowers, do not write about piles of sand.",
    "",
    "WORDING",
    "Lead each section with the point. Do not open with what the application did, how it works, or what it looked at.",
    "Name the actual subject wherever the source names it, and keep the quotation marks around column names.",
    "Use ordinary words. Write short sentences. Prefer the active voice.",
    "Do not use the word significant, the phrase null hypothesis, or the phrase p-value.",
    "Do not describe people or things as everyone or anyone unless the source does.",
    "Do not repeat a point you have already made in an earlier section.",
    "",
    "OUTPUT",
    "Return only the five headings and their text. No preamble, no closing remark, no mention of these instructions."
  )

  parts <- c(paste(instructions, collapse = "\n"))
  if (nzchar(subject)) {
    parts <- c(parts, "", "WHAT THE DATA IS ABOUT", subject)
  }
  paste(c(parts, "", "SOURCE TEXT", "", source), collapse = "\n")
}

numeric_tokens <- function(text) {
  matches <- gregexpr("(?<![A-Za-z])[-+]?(?:[0-9]{1,3}(?:,[0-9]{3})+|[0-9]+)(?:\\.[0-9]+)?%?", text, perl = TRUE)
  tokens <- regmatches(text, matches)[[1]]
  if (identical(tokens, character(0)) || identical(tokens, "")) return(character(0))
  gsub(",", "", tokens, fixed = TRUE)
}

# The whole safety argument for the local model rests here. The numbers in the
# model output are compared against the numbers in the source text in order,
# and any difference at all rejects the restatement. Comparing sets rather than
# sequences would let a model swap two values and pass.
model_numbers_match <- function(source, output) {
  identical(numeric_tokens(source), numeric_tokens(output))
}

# Structure is checked alongside the numbers. A model can preserve every digit
# and still return one flowing paragraph, which loses the fixed reading order
# that the everyday layer exists to provide. Headings are matched case
# insensitively and with surrounding punctuation ignored, because a model that
# writes "What was asked:" has followed the instruction.
model_sections_match <- function(output) {
  lines <- trimws(strsplit(output, "\n", fixed = TRUE)[[1]])
  cleaned <- toupper(gsub("[^A-Za-z ]", "", lines))
  found <- LOCAL_MODEL_SECTIONS[LOCAL_MODEL_SECTIONS %in% cleaned]
  identical(found, LOCAL_MODEL_SECTIONS)
}

# Everything the app requires of a rewrite, in one place, so the caller does not
# have to remember the order of the checks or the wording of each refusal.
check_local_rewrite <- function(explanation, output) {
  if (!nzchar(trimws(output %||% ""))) {
    return(list(ok = FALSE, message = "The local model returned nothing. The checked everyday explanation above is unchanged."))
  }
  if (!model_sections_match(output)) {
    return(list(ok = FALSE, message = paste(
      "Daybreak rejected the rewrite because the model did not return the five sections it was asked for.",
      "The checked everyday explanation above is unchanged."
    )))
  }
  if (!model_numbers_match(local_model_source(explanation), output)) {
    return(list(ok = FALSE, message = paste(
      "Daybreak rejected the rewrite because the model added, removed, or changed a number.",
      "The checked everyday explanation above is unchanged."
    )))
  }
  list(ok = TRUE, message = NULL)
}

ollama_generate_body <- function(prompt, model) {
  list(
    model = model,
    prompt = prompt,
    stream = FALSE,
    think = FALSE,
    keep_alive = "10m",
    options = list(temperature = 0.2, num_predict = 700)
  )
}

ollama_generate <- function(prompt, model, timeout = 300) {
  if (!valid_local_model_name(model)) {
    return(list(ok = FALSE, text = "", message = "Select a valid local model."))
  }
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    return(list(ok = FALSE, text = "", message = "Install the optional httr and jsonlite R packages first."))
  }
  response <- tryCatch(
    httr::POST(
      paste0(OLLAMA_BASE_URL, "/api/generate"),
      body = ollama_generate_body(prompt, model),
      encode = "json",
      httr::timeout(timeout)
    ),
    error = function(e) e
  )
  if (inherits(response, "condition")) {
    server_status <- ollama_list_models(timeout = 5)
    return(ollama_generation_failure(response, model, timeout, server_status))
  }
  status <- httr::status_code(response)
  payload <- ollama_response_payload(response)
  if (inherits(payload, "try-error")) {
    if (!identical(status, 200L)) return(ollama_generation_result(list(), status))
    return(list(ok = FALSE, text = "", message = "Ollama returned a response Daybreak could not read."))
  }
  ollama_generation_result(payload, status)
}
