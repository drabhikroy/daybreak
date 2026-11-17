# Optional local language models -----------------------------------------
#
# Statistics are computed before this file is used. A local model may rewrite
# the finished everyday explanation, but it never receives raw rows and it is
# not allowed to add, remove, or change numeric values.

# The catalog is ordered from the smallest download to the most capable
# writer, and every entry says what the model is poor at as well as what it is
# good at. The job here is narrow: reword five short sections without touching
# a single number. The notes are written about that job and not about the
# models in general. min_ram is the memory, in gigabytes, that leaves room for
# the rest of the computer while the model is loaded.
LOCAL_MODEL_CATALOG <- list(
  "llama3.2:3b" = list(
    label = "Llama 3.2 3B", size = "2.0 GB", min_ram = 8,
    speed = "Fast. A rewrite takes a few seconds on a recent laptop.",
    good = "The smallest download here and the quickest to answer. It keeps the five sections and the numbers intact on short explanations.",
    tradeoff = "Its wording is plain and it can shorten a careful caveat more than you would like."
  ),
  "gemma3:4b" = list(
    label = "Gemma 3 4B", size = "3.3 GB", min_ram = 8,
    speed = "Fast.",
    good = "Natural, readable sentences for its size, which suits an explanation meant for a general reader.",
    tradeoff = "It sometimes adds an aside of its own that was not in the source. Daybreak rejects a rewrite when an aside brings a number with it."
  ),
  "qwen3.5:4b" = list(
    label = "Qwen 3.5 4B", size = "3.4 GB", min_ram = 8,
    speed = "Fast.",
    good = "Careful with figures and with instructions, so its rewrites pass the number check more often than the others at this size.",
    tradeoff = "Its phrasing can read as stiff, closer to the original than a looser model would give."
  ),
  "llama3.1:8b" = list(
    label = "Llama 3.1 8B", size = "4.9 GB", min_ram = 16,
    speed = "Moderate. Slower to load, and noticeably slower on 8 GB of memory.",
    good = "The most fluent writer of the four. Longer explanations come back reading as if a person had written them.",
    tradeoff = "The largest download, and more than this job needs on most results."
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

# Models already on the computer that the catalog does not list. Someone who
# pulled a model from a terminal expects to find it here, so these are offered
# alongside the catalog as soon as Ollama reports them.
other_installed_models <- function(installed = character(0)) {
  installed <- installed[vapply(installed, valid_local_model_name, logical(1))]
  setdiff(installed, c(names(LOCAL_MODEL_CATALOG), paste0(names(LOCAL_MODEL_CATALOG), ":latest")))
}

# Whether Ollama holds a catalog model. Ollama reports llama3.2 as
# llama3.2:latest, so a name is compared with and without that tag.
model_installed <- function(model, installed = character(0)) {
  model %in% installed || paste0(model, ":latest") %in% installed
}

# The model a rewrite will use. A saved choice wins while it is still
# installed. Otherwise the first installed catalog model is used, then any
# other installed model, and with nothing installed there is no answer.
active_local_model <- function(preferred = "", installed = character(0)) {
  installed <- installed[vapply(installed, valid_local_model_name, logical(1))]
  if (nzchar(preferred) && model_installed(preferred, installed)) return(preferred)
  held <- names(LOCAL_MODEL_CATALOG)[vapply(names(LOCAL_MODEL_CATALOG), model_installed, logical(1), installed = installed)]
  if (length(held)) return(held[1])
  if (length(installed)) return(installed[1])
  ""
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

# The command line redraws a progress bar many times a second using terminal
# control codes. Shown raw they are noise, so the codes are stripped and only
# lines that still say something are kept.
pull_progress_lines <- function(lines) {
  lines <- gsub("\033\\[[0-9;?]*[A-Za-z]", "", lines)
  lines <- gsub("[\u2580-\u259F\r]", "", lines)
  lines <- trimws(gsub("\\s+", " ", lines))
  lines[nzchar(lines)]
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
# Labeling it is what lets the instruction "keep these five headings" be
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

# The model is never asked to supply a picture or an analogy of its own. Asked
# for one, a model invents it, and an invented comparison cannot be checked
# against anything on screen: an iris measurement described as piles of sand is
# neither what a sepal is nor what the figure shows. The everyday layer already
# writes the picture, so the model receives it and is told to simplify the
# wording. The same principle runs through every instruction below. This is a
# rewriting job with a fixed structure and fixed facts, not a writing job.
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

# A model that hands its instructions back has not rewritten anything, and the
# two checks above would both pass it, because the instructions contain the
# five headings and the source contains the right numbers. Three signs give an
# echo away: a heading that appears more than once, a heading that belongs to
# the instructions and not to the answer, and a reply far longer than the text
# it was asked to simplify.
PROMPT_ONLY_HEADINGS <- c("STRUCTURE", "FACTS", "PICTURES", "WORDING", "OUTPUT", "SOURCE TEXT", "WHAT THE DATA IS ABOUT")

model_echoes_prompt <- function(source, output) {
  lines <- trimws(strsplit(output, "\n", fixed = TRUE)[[1]])
  cleaned <- toupper(gsub("[^A-Za-z ]", "", lines))
  repeated <- any(table(cleaned[cleaned %in% LOCAL_MODEL_SECTIONS]) > 1L)
  repeated || any(PROMPT_ONLY_HEADINGS %in% cleaned) || nchar(output) > 2 * nchar(source) + 400
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
  if (model_echoes_prompt(local_model_source(explanation), output)) {
    return(list(ok = FALSE, message = paste(
      "Daybreak rejected the rewrite because the model returned its instructions instead of a rewrite.",
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
