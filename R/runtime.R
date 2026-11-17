# The local model runtime ---------------------------------------------------
#
# Everything here is about getting Ollama onto the reader's own computer and
# keeping it there between sessions. None of it is needed to use Daybreak, and
# none of it runs until the reader presses a button that says what it will do.
#
# Two routes are offered. One uses an Ollama the reader installed themselves,
# and Daybreak only checks for it. The other keeps a private copy in a folder
# that belongs to this app, which installs nothing into the system and is
# undone by deleting that one folder. The second route verifies what it
# downloaded against the checksums the vendor publishes before unpacking it.

OLLAMA_RELEASE_URL <- "https://github.com/ollama/ollama/releases/latest/download/"

# R gives every application a directory for data it should keep between runs.
# Using it means nothing is written beside the source files or loose in a home
# folder.
daybreak_dir <- function(which = c("data", "config")) {
  which <- match.arg(which)
  path <- tools::R_user_dir("daybreak", which)
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

# The platform, named the way the release files are named. An unrecognized
# platform is not an error. It means the private copy is not offered and the
# other route still is.
host_platform <- function(info = Sys.info(), arch = R.version$arch) {
  system <- as.character(info[["sysname"]])
  if (identical(system, "Darwin")) {
    return(list(id = "darwin", label = "macOS", asset = "ollama-darwin.tgz"))
  }
  if (identical(system, "Linux")) {
    asset <- if (grepl("aarch64|arm", arch)) "ollama-linux-arm64.tar.zst" else "ollama-linux-amd64.tar.zst"
    return(list(id = "linux", label = "Linux", asset = asset))
  }
  if (identical(system, "Windows")) {
    # Windows ships an installer and not an archive, so a private copy cannot
    # be offered there without running someone else's setup program.
    return(list(id = "windows", label = "Windows", asset = NA_character_))
  }
  list(id = "unknown", label = "this system", asset = NA_character_)
}

install_hint <- function(platform = host_platform()) {
  switch(platform$id,
    darwin = "Download Ollama from ollama.com/download, move it to Applications, and open it once. With Homebrew, brew install ollama does the same.",
    linux = "Follow the Linux instructions at ollama.com/download.",
    windows = "Download the installer from ollama.com/download and run it.",
    "See ollama.com/download for instructions.")
}

# The hardware check. It reads four things, the operating system, the amount
# of memory, the number of processor cores, and whether the processor is Apple
# silicon or an NVIDIA card is present, and it reads them only when the reader
# asks. The values stay in this session and decide nothing except which models
# are marked as a good fit.
machine_report <- function() {
  platform <- host_platform()
  cores <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_)
  bytes <- tryCatch({
    if (platform$id == "darwin") {
      as.numeric(system2("sysctl", c("-n", "hw.memsize"), stdout = TRUE))
    } else if (platform$id == "linux") {
      line <- grep("^MemTotal", readLines("/proc/meminfo", warn = FALSE), value = TRUE)[1]
      as.numeric(gsub("[^0-9]", "", line)) * 1024
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_, warning = function(w) NA_real_)
  accelerated <- tryCatch({
    if (platform$id == "darwin") {
      grepl("Apple", paste(system2("sysctl", c("-n", "machdep.cpu.brand_string"), stdout = TRUE), collapse = " "))
    } else if (platform$id == "linux") {
      nzchar(Sys.which("nvidia-smi"))
    } else {
      FALSE
    }
  }, error = function(e) FALSE, warning = function(w) FALSE)
  list(
    os = platform$label,
    ram_gb = if (length(bytes) == 1L && is.finite(bytes)) round(bytes / 1024^3, 1) else NA_real_,
    cores = cores,
    accelerated = isTRUE(accelerated)
  )
}

machine_sentence <- function(report) {
  sprintf("%s, %s of memory, %s processor cores%s.",
          report$os,
          if (is.finite(report$ram_gb)) paste(report$ram_gb, "GB") else "an unknown amount",
          if (is.finite(report$cores)) report$cores else "an unknown number of",
          if (isTRUE(report$accelerated)) ", with hardware that speeds up local models" else "")
}

# How each catalog model sits on the reported machine. The memory figure for a
# model is its working set with room left for everything else that is open,
# which is why it is larger than the download.
model_fit <- function(report) {
  ram <- report$ram_gb
  vapply(LOCAL_MODEL_CATALOG, function(model) {
    if (!is.finite(ram)) return("unknown")
    # A computer sold with 8 GB reports a little under 8, because some memory
    # is held back by the hardware, so the comparison allows for that.
    if (ram >= model$min_ram + 4) "comfortable"
    else if (ram >= model$min_ram - 0.75) "workable"
    else "too large"
  }, character(1))
}

# The suggestion is a model that fits comfortably, or the smallest one when
# none does. The catalog is ordered from smallest to most capable so that this
# rule can be read off the list.
recommended_model <- function(report) {
  fits <- model_fit(report)
  comfortable <- names(fits)[fits == "comfortable"]
  # Among models that fit comfortably the more capable small models are
  # preferred, but the 8B model is suggested only when memory is plentiful,
  # because this job rarely needs it.
  if (length(comfortable)) {
    small <- setdiff(comfortable, "llama3.1:8b")
    if (is.finite(report$ram_gb) && report$ram_gb >= 32 && "llama3.1:8b" %in% comfortable) return("llama3.1:8b")
    return(utils::tail(if (length(small)) small else comfortable, 1))
  }
  names(LOCAL_MODEL_CATALOG)[1]
}

managed_root <- function() file.path(daybreak_dir("data"), "ollama")

managed_binary <- function(root = managed_root()) {
  candidates <- c(file.path(root, "bin", "ollama"), file.path(root, "ollama"))
  found <- candidates[file.exists(candidates)]
  if (length(found)) found[1] else ""
}

# The private copy is preferred when it exists, because a reader who chose it
# expects Daybreak to use the thing it installed.
runtime_binary <- function() {
  managed <- managed_binary()
  if (nzchar(managed)) managed else find_ollama_cli()
}

# Everything the setup screen needs in one call, so the screen never has to
# work out what is true from three separate places.
runtime_state <- function() {
  platform <- host_platform()
  managed <- managed_binary()
  system <- find_ollama_cli()
  status <- ollama_list_models()
  list(
    platform = platform,
    managed_available = !is.na(platform$asset),
    managed_path = managed,
    system_path = system,
    installed = nzchar(managed) || nzchar(system),
    running = isTRUE(status$ok),
    models = status$models %||% character(0),
    message = status$message
  )
}

# SHA-256 of a file, from whichever tool this installation has. Base R gained
# one in 4.5, so older versions fall back to a package and then to the command
# every macOS and Linux system carries. With none of them the answer is NA and
# the caller refuses the download, because an unchecked archive is the one
# thing this function exists to prevent.
file_sha256 <- function(path) {
  if (exists("sha256sum", envir = asNamespace("tools"), inherits = FALSE)) {
    return(unname(get("sha256sum", envir = asNamespace("tools"))(path)))
  }
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(path, algo = "sha256", file = TRUE))
  }
  tool <- if (nzchar(Sys.which("shasum"))) list("shasum", c("-a", "256")) else if (nzchar(Sys.which("sha256sum"))) list("sha256sum", character(0)) else NULL
  if (is.null(tool)) return(NA_character_)
  out <- tryCatch(system2(tool[[1]], c(tool[[2]], shQuote(path)), stdout = TRUE, stderr = FALSE),
                  error = function(e) character(0), warning = function(w) character(0))
  if (!length(out)) return(NA_character_)
  tolower(sub("\\s.*$", "", out[1]))
}

# The published checksum for one release file, read from the list the vendor
# attaches to every release.
published_sha256 <- function(asset, listing) {
  lines <- strsplit(listing, "\n", fixed = TRUE)[[1]]
  parts <- strsplit(trimws(lines), "\\s+")
  for (part in parts) {
    if (length(part) == 2L && identical(sub("^\\./", "", part[2]), asset) && grepl("^[0-9a-f]{64}$", part[1])) {
      return(part[1])
    }
  }
  NA_character_
}

download_to <- function(url, destination, report = function(text) invisible(NULL), expected_mb = NA_real_) {
  connection <- file(destination, open = "wb")
  on.exit(try(close(connection), silent = TRUE), add = TRUE)
  seen <- 0
  said <- 0
  curl::curl_fetch_stream(url, function(chunk) {
    writeBin(chunk, connection)
    seen <<- seen + length(chunk)
    megabytes <- seen / 1024^2
    # Speaking on every chunk would spend more time on messages than on the
    # download, so progress is reported every twenty megabytes.
    if (megabytes - said >= 20) {
      said <<- megabytes
      report(if (is.finite(expected_mb)) sprintf("Downloaded %.0f MB of about %.0f MB.", megabytes, expected_mb)
             else sprintf("Downloaded %.0f MB.", megabytes))
    }
  }, handle = curl::new_handle(followlocation = TRUE, connecttimeout = 30L))
  invisible(seen)
}

# The private copy. The archive is fetched over HTTPS from the vendor's release
# page, checked against the vendor's published SHA-256, and only then unpacked
# into this app's own folder. A file that fails the check is deleted unopened.
# The function returns a list with ok and message and never signals, because
# the screen that calls it has to say something either way.
managed_install <- function(report = function(text) invisible(NULL), platform = host_platform(), root = managed_root()) {
  if (is.na(platform$asset)) {
    return(list(ok = FALSE, message = sprintf(
      "Daybreak cannot keep its own copy of Ollama on %s, because the vendor ships an installer there and not an archive. Install Ollama yourself and Daybreak will find it.",
      platform$label)))
  }
  if (!requireNamespace("curl", quietly = TRUE)) {
    return(list(ok = FALSE, message = "The curl R package is needed for the download. Install it, or install Ollama yourself and Daybreak will find it."))
  }
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  archive <- file.path(root, platform$asset)
  result <- tryCatch({
    report("Reading the published checksums.")
    listing <- rawToChar(curl::curl_fetch_memory(
      paste0(OLLAMA_RELEASE_URL, "sha256sum.txt"),
      handle = curl::new_handle(followlocation = TRUE, connecttimeout = 30L, timeout = 60L))$content)
    expected <- published_sha256(platform$asset, listing)
    if (is.na(expected)) stop("the release did not publish a checksum for this file")
    report("Starting the download.")
    download_to(paste0(OLLAMA_RELEASE_URL, platform$asset), archive, report)
    report("Checking the download against the published checksum.")
    actual <- file_sha256(archive)
    if (is.na(actual)) stop("this computer has no tool for computing a SHA-256 checksum")
    if (!identical(tolower(actual), expected)) stop("the downloaded file does not match the published checksum")
    report("Unpacking.")
    status <- suppressWarnings(utils::untar(archive, exdir = root))
    if (!identical(as.integer(status), 0L)) stop("the archive could not be unpacked on this computer")
    unlink(archive)
    binary <- managed_binary(root)
    if (!nzchar(binary)) stop("the archive did not contain the Ollama program")
    Sys.chmod(binary, "0755")
    list(ok = TRUE, message = sprintf("Ollama is installed in Daybreak's own folder at %s.", root))
  }, error = function(e) {
    # A download that stopped partway, or failed its check, is of no use and
    # can be hundreds of megabytes in a folder the reader never sees.
    unlink(archive)
    list(ok = FALSE, message = sprintf(
      "The private copy was not installed: %s. Nothing was left behind. You can try again, or install Ollama yourself and Daybreak will find it.",
      conditionMessage(e)))
  })
  result
}

# The server Daybreak started, if it started one, so that removing the private
# copy can stop the program before deleting the folder it lives in.
RUNTIME <- new.env(parent = emptyenv())

# Starts a server from whichever copy is present and returns once it answers.
# The address is set explicitly so the server listens on this computer only,
# whatever the reader's environment says. The private copy also keeps its
# models inside its own folder, which is what makes removing that folder a
# complete undo. A server that is already answering is left alone.
managed_start <- function(report = function(text) invisible(NULL), wait_seconds = 30L) {
  if (isTRUE(ollama_list_models()$ok)) return(list(ok = TRUE, message = "Ollama was already running."))
  binary <- runtime_binary()
  if (!nzchar(binary)) return(list(ok = FALSE, message = "Ollama is not installed yet, so there is nothing to start."))
  log <- file.path(daybreak_dir("data"), "ollama-serve.log")
  settings <- c(OLLAMA_HOST = "127.0.0.1:11434")
  if (identical(binary, managed_binary())) settings <- c(settings, OLLAMA_MODELS = file.path(managed_root(), "models"))
  tryCatch({
    report("Starting Ollama.")
    if (requireNamespace("processx", quietly = TRUE)) {
      RUNTIME$server <- processx::process$new(binary, "serve", stdout = log, stderr = log,
                                              env = c("current", settings), cleanup = FALSE)
    } else {
      system2(binary, "serve", wait = FALSE, stdout = log, stderr = log,
              env = paste0(names(settings), "=", settings))
    }
    for (second in seq_len(wait_seconds)) {
      Sys.sleep(1)
      if (isTRUE(ollama_list_models()$ok)) return(list(ok = TRUE, message = "Ollama is running."))
      report(sprintf("Waiting for Ollama to answer (%d seconds).", second))
    }
    list(ok = FALSE, message = sprintf(
      "Ollama was started but has not answered yet. It may still be setting itself up. Press Check again in a moment. Its log is at %s.", log))
  }, error = function(e) {
    list(ok = FALSE, message = paste("Ollama did not start:", conditionMessage(e)))
  })
}

# Removal goes through the local HTTP interface and not through a command, so
# no model name is ever handed to a shell.
remove_model <- function(model) {
  if (!valid_local_model_name(model)) return(list(ok = FALSE, message = "That is not a model name."))
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    return(list(ok = FALSE, message = "Install the optional httr and jsonlite R packages first."))
  }
  response <- tryCatch(
    httr::DELETE(paste0(OLLAMA_BASE_URL, "/api/delete"),
                 body = jsonlite::toJSON(list(model = model), auto_unbox = TRUE),
                 httr::content_type_json(), httr::timeout(30)),
    error = function(e) e)
  if (inherits(response, "condition")) {
    return(list(ok = FALSE, message = "Daybreak could not reach Ollama to remove the model. Start Ollama and try again."))
  }
  status <- httr::status_code(response)
  if (identical(status, 200L)) return(list(ok = TRUE, message = sprintf("%s has been removed from this computer.", model)))
  if (identical(status, 404L)) return(list(ok = FALSE, message = sprintf("%s is not installed, so there was nothing to remove.", model)))
  list(ok = FALSE, message = sprintf("Ollama answered with status %s and did not remove %s.", status, model))
}

# Only the private copy can be removed from here. Daybreak put that folder
# there and can take it away. An Ollama the reader installed belongs to them
# and leaves the way it arrived.
remove_runtime <- function(root = managed_root()) {
  if (!nzchar(managed_binary(root))) {
    return(list(ok = FALSE, message = "Daybreak has no private copy of Ollama to remove. An Ollama you installed yourself is removed the way you installed it."))
  }
  server <- RUNTIME$server
  if (!is.null(server) && isTRUE(tryCatch(server$is_alive(), error = function(e) FALSE))) {
    try(server$kill(), silent = TRUE)
  }
  RUNTIME$server <- NULL
  unlink(root, recursive = TRUE, force = TRUE)
  if (dir.exists(root)) {
    return(list(ok = FALSE, message = sprintf("The folder could not be deleted. It is at %s.", root)))
  }
  list(ok = TRUE, message = "Daybreak's private copy of Ollama and the models stored inside it have been removed.")
}

# The few choices worth keeping between sessions: which route the reader took
# and which model they chose to use. Stored as plain lines so the file can be
# read, and corrected, by a person.
read_model_preferences <- function(path = file.path(daybreak_dir("config"), "local-model.dcf")) {
  defaults <- list(route = "system", model = "")
  if (!file.exists(path)) return(defaults)
  saved <- tryCatch(as.list(read.dcf(path)[1, ]), error = function(e) list())
  route <- saved$route %||% defaults$route
  model <- saved$model %||% defaults$model
  list(route = if (route %in% c("system", "managed")) route else defaults$route,
       model = if (nzchar(model) && valid_local_model_name(model)) model else "")
}

write_model_preferences <- function(preferences, path = file.path(daybreak_dir("config"), "local-model.dcf")) {
  tryCatch({
    write.dcf(data.frame(route = preferences$route %||% "system", model = preferences$model %||% "",
                         stringsAsFactors = FALSE), path)
    TRUE
  }, error = function(e) FALSE)
}
