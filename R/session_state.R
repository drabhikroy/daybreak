# Portable Daybreak sessions --------------------------------------------
#
# A session file is an RDS bundle with a small schema marker. Models are
# omitted because the displayed result, tables, plot data, prose, and code
# are enough to reopen completed work and make a much smaller file.

DAYBREAK_SESSION_FORMAT <- "daybreak-session"
DAYBREAK_SESSION_SCHEMA <- 1L

session_safe_result <- function(result) {
  if (is.null(result)) return(NULL)
  result$model <- NULL
  result
}

# Session files ------------------------------------------------------------
#
# A session is stored as an RDS of a plain list and read back with readRDS, not
# load, so opening one cannot introduce a variable into the running session.
# The version is recorded so that a file written by a later release can be
# recognized rather than silently misread.
new_daybreak_session <- function(data, data_name, data_source, sample_id, method_id,
                                 roles, options, result, narrative, accessible,
                                 original_data = NULL, original_data_name = NULL,
                                 imputation_label = NULL, imputation_columns = character(0),
                                 imputation_filled = 0L, theme = "dark", palette = "standard",
                                 app_version = DAYBREAK_VERSION) {
  list(
    format = DAYBREAK_SESSION_FORMAT,
    schema = DAYBREAK_SESSION_SCHEMA,
    app_version = app_version,
    saved_at = Sys.time(),
    data = data,
    data_name = data_name,
    data_source = data_source,
    sample_id = sample_id,
    method_id = method_id,
    roles = roles %||% list(),
    options = options %||% list(),
    result = session_safe_result(result),
    narrative = narrative,
    accessible = accessible,
    original_data = original_data,
    original_data_name = original_data_name,
    imputation_label = imputation_label,
    imputation_columns = imputation_columns %||% character(0),
    imputation_filled = as.integer(imputation_filled %||% 0L),
    theme = if (identical(theme, "light")) "light" else "dark",
    palette = if (palette %in% c("standard", "protanopia", "deuteranopia", "tritanopia", "monochrome")) palette else "standard"
  )
}

validate_daybreak_session <- function(x) {
  if (!is.list(x) || !identical(x$format, DAYBREAK_SESSION_FORMAT)) {
    stop("This is not a Daybreak session file.")
  }
  if (!identical(as.integer(x$schema), DAYBREAK_SESSION_SCHEMA)) {
    stop("This Daybreak session uses a file version that this release cannot open.")
  }
  if (is.null(x$data) || !is.data.frame(x$data)) {
    stop("The session does not contain a readable data table.")
  }
  if (!nrow(x$data) || !ncol(x$data)) stop("The saved data table is empty.")
  if (!ncol(x$data) || any(!nzchar(names(x$data))) || anyDuplicated(names(x$data))) {
    stop("The saved data must have unique, nonblank column names.")
  }
  if (length(x$data_name) != 1L || is.na(x$data_name) || !nzchar(trimws(x$data_name))) {
    stop("The saved data name is not readable.")
  }
  if (length(x$theme) != 1L || is.na(x$theme) || !x$theme %in% c("dark", "light")) {
    stop("The saved display theme is not recognized.")
  }
  palettes <- c("standard", "protanopia", "deuteranopia", "tritanopia", "monochrome")
  if (length(x$palette) != 1L || is.na(x$palette) || !x$palette %in% palettes) {
    stop("The saved color-vision setting is not recognized.")
  }
  if (length(x$method_id) != 1L || is.na(x$method_id) || !x$method_id %in% names(ANALYSES)) {
    stop("The saved statistical method is not available in this release.")
  }
  if (!is.list(x$roles) || !is.list(x$options)) {
    stop("The saved column roles or settings are incomplete.")
  }
  role_specs <- ANALYSES[[x$method_id]]$roles
  role_ids <- vapply(role_specs, `[[`, character(1), "id")
  if (!all(role_ids %in% names(x$roles))) {
    stop("The saved column roles are incomplete for this method.")
  }
  for (role in role_specs) {
    value <- x$roles[[role$id]]
    if (identical(role$type, "lavaan_text")) {
      if (!is.character(value) || length(value) != 1L || is.na(value)) {
        stop("The saved model syntax is not readable.")
      }
      next
    }
    value <- as.character(value %||% character(0))
    chosen <- value[!is.na(value) & nzchar(value)]
    unknown <- setdiff(chosen, names(x$data))
    if (length(unknown)) {
      stop(sprintf("A saved column choice is not present in the saved data: %s.", paste(unknown, collapse = ", ")))
    }
  }
  if (!is.null(x$result) && (!is.list(x$result) || !identical(x$result$method, x$method_id))) {
    stop("The saved result does not match the saved method.")
  }
  if (!is.null(x$result) && !is.null(x$narrative) &&
      (!is.list(x$narrative) || !all(c("title", "headline", "cards") %in% names(x$narrative)))) {
    stop("The saved statistical description is incomplete.")
  }
  explanation_fields <- c("title", "question", "context", "bottom_line", "picture", "certainty", "boundary", "data_note")
  if (!is.null(x$result) && !is.null(x$accessible) &&
      (!is.list(x$accessible) || !all(explanation_fields %in% names(x$accessible)))) {
    stop("The saved everyday explanation is incomplete.")
  }
  x
}

write_daybreak_session <- function(x, path) {
  validate_daybreak_session(x)
  saveRDS(x, path, compress = "xz", version = 3)
  invisible(path)
}

# Every field is checked on the way in. A session file is something a reader
# may have been sent by someone else, so it is treated as input to validate
# rather than state to trust.
read_daybreak_session <- function(path) {
  info <- file.info(path)
  if (!nrow(info) || !is.finite(info$size) || info$size <= 0) stop("The selected session file is empty.")
  if (info$size > 200 * 1024^2) stop("The selected session file is larger than the 200 MB app limit.")
  x <- tryCatch(
    readRDS(path, refhook = function(...) stop("The session contains an unsupported external reference.")),
    error = function(e) stop(sprintf("Daybreak could not read this session file: %s", conditionMessage(e)))
  )
  validate_daybreak_session(x)
}

daybreak_session_filename <- function(time = Sys.time()) {
  paste0("daybreak-session-", format(time, "%Y%m%d-%H%M"), ".daybreak.rds")
}
