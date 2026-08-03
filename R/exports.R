# Reports, figures, tables, and reproducible code -------------------------
#
# Every download is assembled from one result record, so a report, a table, a
# figure, and a script taken from the same screen cannot disagree with each
# other or with what was displayed.
#
# The reproduction script is the one that carries the most weight. It is built
# from strings the engine recorded while it was working, not reconstructed
# afterwards from the output, and it is meant to run in a plain R session with
# no part of this app loaded. Where an engine cannot express a step faithfully in
# a few lines, the script says so rather than approximating it.

QUALITY_PRESETS <- list(
  screen = list(label = "Screen, 144 dpi", dpi = 144),
  standard = list(label = "Standard, 200 dpi", dpi = 200),
  print = list(label = "Print, 300 dpi", dpi = 300),
  ultra = list(label = "Ultra, 600 dpi", dpi = 600)
)

SIZE_PRESETS <- list(
  square = list(label = "Square, 8 by 8 in", width = 8, height = 8),
  standard = list(label = "Standard, 10 by 6.25 in", width = 10, height = 6.25),
  wide = list(label = "Wide, 12 by 6.75 in", width = 12, height = 6.75),
  portrait = list(label = "Portrait, 7.5 by 10 in", width = 7.5, height = 10)
)

# Figure export offers vector formats first. A chart headed for a report is
# nearly always better as SVG or PDF, and the raster sizes here are chosen for
# print rather than for the screen.
export_plot <- function(plot, path, format, quality = "print", size = "standard",
                        background = "#111820") {
  preset <- QUALITY_PRESETS[[quality]] %||% QUALITY_PRESETS$print
  dims <- SIZE_PRESETS[[size]] %||% SIZE_PRESETS$standard
  format <- tolower(format)
  if (format == "psd") {
    if (!requireNamespace("magick", quietly = TRUE)) stop("PSD export needs the magick package and ImageMagick with PSD write support.")
    png_path <- tempfile(fileext = ".png")
    on.exit(unlink(png_path), add = TRUE)
    ggplot2::ggsave(png_path, plot = plot, device = "png", width = dims$width, height = dims$height,
                    units = "in", dpi = preset$dpi, bg = background)
    image <- magick::image_read(png_path)
    tryCatch(magick::image_write(image, path = path, format = "psd"),
             error = function(e) stop("This ImageMagick installation cannot write PSD files. Choose TIFF or PNG, or install a build with PSD write support."))
    return(invisible(path))
  }
  if (format %in% c("tex", "tikz")) {
    if (!requireNamespace("tikzDevice", quietly = TRUE)) stop("TikZ export needs the tikzDevice package and a working TeX installation.")
    tikzDevice::tikz(file = path, width = dims$width, height = dims$height,
                     standAlone = FALSE, sanitize = TRUE, bg = background)
    tryCatch(print(plot), finally = grDevices::dev.off())
    return(invisible(path))
  }
  supported <- c("png", "jpg", "jpeg", "tiff", "svg", "pdf", "eps")
  if (!format %in% supported) stop("Choose PNG, JPEG, TIFF, SVG, PDF, EPS, PSD, or TikZ.")
  device <- switch(format, jpg = "jpeg", jpeg = "jpeg", format)
  args <- list(filename = path, plot = plot, device = device, width = dims$width,
               height = dims$height, units = "in", dpi = preset$dpi, bg = background)
  if (format == "tiff") args$compression <- "lzw"
  do.call(ggplot2::ggsave, args)
  invisible(path)
}

markdown_table <- function(x) {
  if (is.null(x) || !nrow(x)) return(character(0))
  x <- plain_table(x)
  headers <- names(x)
  rows <- apply(x, 1, function(row) paste0("| ", paste(gsub("\\|", "\\\\|", as.character(row)), collapse = " | "), " |"))
  c(paste0("| ", paste(headers, collapse = " | "), " |"),
    paste0("| ", paste(rep("---", length(headers)), collapse = " | "), " |"), rows)
}

# The Markdown report carries both readings and the tables, in the order they
# appear on screen. Matching the screen order matters: a reader who exports a
# result should recognize the document as the thing they were just looking at.
report_markdown <- function(result, narrative, accessible = NULL) {
  checks <- if (!is.null(result$checks)) result$checks[, setdiff(names(result$checks), "Status"), drop = FALSE] else NULL
  everyday <- if (is.null(accessible)) character(0) else c(
    "## Everyday explanation", "", accessible$question, "",
    accessible$context %||% "", "",
    "### The short answer", "", accessible$bottom_line, "",
    "### How to picture it", "", accessible$picture, "",
    "### How sure can we be?", "", accessible$certainty, "",
    "### What this does not tell us", "", accessible$boundary, "",
    accessible$data_note, ""
  )
  c(
    paste0("# ", narrative$title), "",
    everyday,
    "## Statistical description", "",
    narrative$headline, "",
    unlist(lapply(narrative$cards, function(card) c(paste0("## ", card$title), "", card$body, ""))),
    "## Estimates", "", markdown_table(result$estimates), "",
    "## Tests", "", markdown_table(result$tests), "",
    "## Model fit", "", markdown_table(result$fit), "",
    "## Checks", "", markdown_table(checks), "",
    "## Reproducibility note", "",
    "All statistics and sentences in this report were produced from deterministic R code. No language model generated or changed a number.", "",
    report_footer()
  )
}

# LaTeX escaping covers the ten characters that change meaning in a document
# body. Column names arrive from a reader's own file and regularly contain
# underscores and percent signs.
escape_latex <- function(x) {
  replacements <- c(
    "\\" = "\\textbackslash{}", "#" = "\\#", "$" = "\\$", "%" = "\\%",
    "&" = "\\&", "_" = "\\_", "{" = "\\{", "}" = "\\}",
    "~" = "\\textasciitilde{}", "^" = "\\textasciicircum{}"
  )
  vapply(as.character(x), function(value) {
    if (is.na(value)) return("NA")
    characters <- strsplit(value, "", fixed = TRUE)[[1]]
    paste0(vapply(characters, function(character) {
      replacement <- unname(replacements[character])
      if (length(replacement) && !is.na(replacement)) replacement else character
    }, character(1)), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

latex_table <- function(x) {
  if (is.null(x) || !nrow(x)) return("No table was produced.")
  x <- plain_table(x)
  cols <- paste(rep("l", ncol(x)), collapse = "")
  head <- paste(escape_latex(names(x)), collapse = " & ")
  rows <- apply(x, 1, function(row) paste(escape_latex(as.character(row)), collapse = " & "))
  c(sprintf("\\begin{longtable}{%s}", cols), "\\toprule", paste0(head, " \\\\"), "\\midrule",
    paste0(rows, " \\\\"), "\\bottomrule", "\\end{longtable}")
}

report_latex <- function(result, narrative, accessible = NULL) {
  cards <- unlist(lapply(narrative$cards, function(card) {
    c(sprintf("\\section*{%s}", escape_latex(card$title)), escape_latex(card$body), "")
  }))
  everyday <- if (is.null(accessible)) character(0) else c(
    "\\section*{Everyday explanation}", escape_latex(accessible$question),
    escape_latex(accessible$context %||% ""),
    "\\subsection*{The short answer}", escape_latex(accessible$bottom_line),
    "\\subsection*{How to picture it}", escape_latex(accessible$picture),
    "\\subsection*{How sure can we be?}", escape_latex(accessible$certainty),
    "\\subsection*{What this does not tell us}", escape_latex(accessible$boundary),
    escape_latex(accessible$data_note), ""
  )
  c(
    "\\documentclass[11pt]{article}",
    "\\usepackage[margin=1in]{geometry}", "\\usepackage{booktabs}", "\\usepackage{longtable}",
    "\\usepackage{xcolor}", "\\definecolor{sun}{HTML}{C58A08}",
    "\\title{\\textcolor{sun}{Daybreak} statistical report}",
    sprintf("\\author{%s}", escape_latex(narrative$title)), "\\date{}", "\\begin{document}", "\\maketitle",
    everyday, "\\section*{Statistical description}",
    escape_latex(narrative$headline), "", cards,
    "\\section*{Estimates}", latex_table(result$estimates),
    "\\section*{Tests}", latex_table(result$tests),
    "\\section*{Model fit}", latex_table(result$fit),
    "\\section*{Checks}", latex_table(if (!is.null(result$checks)) result$checks[, setdiff(names(result$checks), "Status"), drop = FALSE] else NULL),
    "\\section*{Reproducibility note}",
    "All statistics and sentences in this report were produced from deterministic R code. No language model generated or changed a number.",
    "", escape_latex(report_footer()),
    "\\end{document}"
  )
}

# The bundle writer changes the working directory while it zips, and a Shiny
# session can be started from a parent folder, so the template is located from
# a path recorded when this file is sourced rather than from getwd() at the
# moment of rendering.
DAYBREAK_SOURCE_DIR <- tryCatch(normalizePath(getwd(), mustWork = FALSE), error = function(e) ".")

daybreak_report_template <- function() {
  candidates <- c(
    file.path(DAYBREAK_SOURCE_DIR, "report", "analysis-report.Rmd"),
    file.path("report", "analysis-report.Rmd")
  )
  found <- candidates[file.exists(candidates)]
  if (!length(found)) stop("The report template report/analysis-report.Rmd could not be found.")
  normalizePath(found[1])
}

# Every exported document ends with the same line. A report that does not say
# which release produced it cannot be matched back to the code that produced
# it, which is the whole point of shipping a reproduction script beside it.
report_footer <- function() {
  sprintf("Generated by Daybreak %s on %s.", DAYBREAK_VERSION,
          format(Sys.time(), "%Y-%m-%d %H:%M %Z"))
}

write_report <- function(result, narrative, path, format = "html", accessible = NULL) {
  format <- tolower(format)
  if (format %in% c("txt", "text")) {
    text <- if (is.null(accessible)) narrative_text(narrative) else c(
      accessible_explanation_text(accessible), "", "Statistical description", "",
      narrative_text(narrative)
    )
    writeLines(c(text, "", report_footer()), path, useBytes = TRUE)
    return(invisible(path))
  }
  if (format %in% c("md", "markdown")) {
    writeLines(report_markdown(result, narrative, accessible), path, useBytes = TRUE)
    return(invisible(path))
  }
  if (format %in% c("tex", "latex")) {
    writeLines(report_latex(result, narrative, accessible), path, useBytes = TRUE)
    return(invisible(path))
  }
  if (!format %in% c("html", "docx", "pdf")) stop("Choose text, Markdown, HTML, Word, PDF, or LaTeX.")
  if (!requireNamespace("rmarkdown", quietly = TRUE)) stop("HTML, Word, and PDF reports need the rmarkdown package.")
  output <- switch(format, html = rmarkdown::html_document(toc = TRUE, theme = "flatly"),
                   docx = rmarkdown::word_document(toc = TRUE), pdf = rmarkdown::pdf_document(toc = TRUE))
  render_dir <- tempfile("daybreak-report-")
  dir.create(render_dir)
  on.exit(unlink(render_dir, recursive = TRUE), add = TRUE)
  template <- daybreak_report_template()
  produced <- rmarkdown::render(
    input = template,
    output_format = output,
    output_file = paste0("daybreak-report.", format), output_dir = render_dir, quiet = TRUE,
    params = list(result = result, narrative = narrative, accessible = accessible), envir = new.env(parent = globalenv())
  )
  if (!file.copy(produced, path, overwrite = TRUE)) stop("The rendered report could not be copied to the download path.")
  invisible(path)
}

# Downloads ----------------------------------------------------------------
#
# The reproduction script is the strongest claim this app makes, so it is
# assembled from the same strings the engine recorded while it worked rather
# than rebuilt afterwards from the result. Anything the script cannot express
# faithfully is left out rather than approximated.
write_reproduction_script <- function(result, path) {
  header <- c(
    "# Reproducible analysis exported from Daybreak.",
    sprintf("# Method: %s", method_name(result$method)),
    "# Replace the path below with the source data file used in the app.",
    "", "df <- read.csv(\"your_data.csv\", check.names = FALSE)", ""
  )
  writeLines(c(header, result$code, "", "# Session details", "sessionInfo()"), path, useBytes = TRUE)
  invisible(path)
}

write_result_tables <- function(result, directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  tables <- list(estimates = result$estimates, tests = result$tests, model_fit = result$fit, checks = result$checks)
  for (name in names(tables)) {
    if (!is.null(tables[[name]]) && nrow(tables[[name]])) {
      utils::write.csv(tables[[name]], file.path(directory, paste0(name, ".csv")), row.names = FALSE, na = "")
    }
  }
  invisible(directory)
}

write_analysis_bundle <- function(result, narrative, plot, path, mode = "dark", palette = "standard", accessible = NULL) {
  target <- file.path(normalizePath(dirname(path), mustWork = TRUE), basename(path))
  folder <- tempfile("daybreak-bundle-")
  dir.create(folder)
  # on.exit handlers run in the order they were added, so the directory change
  # has to be undone before the folder it points at is removed. Registering the
  # cleanup first left the process sitting in a deleted directory.
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  writeLines(report_markdown(result, narrative, accessible), file.path(folder, "report.md"), useBytes = TRUE)
  write_reproduction_script(result, file.path(folder, "reproduce.R"))
  write_result_tables(result, file.path(folder, "tables"))
  background <- plot_style(mode, palette)$background
  export_plot(plot, file.path(folder, "figure.png"), "png", "print", "standard", background)
  writeLines(c(sprintf("Daybreak %s analysis bundle", DAYBREAK_VERSION),
               sprintf("Method: %s", method_name(result$method)),
               "Contents: Markdown report, PNG figure, CSV tables, and an R script."),
             file.path(folder, "README.txt"))
  setwd(folder)
  relative <- list.files(".", recursive = TRUE, all.files = FALSE)
  if (requireNamespace("zip", quietly = TRUE)) zip::zipr(target, relative) else utils::zip(target, relative)
  invisible(target)
}
