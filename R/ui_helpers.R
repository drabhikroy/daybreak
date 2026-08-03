# Interface builders ------------------------------------------------------
#
# Everything the reader touches is built here from the catalog rather than
# written out by hand, so a new method appears in the selector, the directory,
# the feasibility hints, and the settings form without any of those places
# being edited.
#
# Two rules run through the whole file. The first is that nothing is hidden
# behind a control the reader has to discover: if a setting affects the result,
# it is on screen next to the button that produces the result. The second is
# that every message is written for the person who chose the columns, not for
# the person who wrote the engine.

# Interface pieces ---------------------------------------------------------
#
# Icons are inline SVG rather than an icon font or an image file. A font can
# fail to load and leave a box character in the middle of a sentence, and an
# image cannot inherit the current text color, which matters here because the
# same mark has to work across five palettes and two themes.
icon_svg <- function(name) {
  paths <- list(
    sun = '<circle cx="12" cy="12" r="4"></circle><path d="M12 2v2M12 20v2M4.93 4.93l1.41 1.41M17.66 17.66l1.41 1.41M2 12h2M20 12h2M4.93 19.07l1.41-1.41M17.66 6.34l1.41-1.41"></path>',
    data = '<ellipse cx="12" cy="5" rx="8" ry="3"></ellipse><path d="M4 5v6c0 1.66 3.58 3 8 3s8-1.34 8-3V5M4 11v6c0 1.66 3.58 3 8 3s8-1.34 8-3v-6"></path>',
    chart = '<path d="M4 19V9M10 19V5M16 19v-7M22 19H2"></path>',
    save = '<path d="M5 3h11l3 3v15H5zM8 3v6h8V3M8 21v-7h8v7"></path>',
    download = '<path d="M12 3v12M7 10l5 5 5-5M4 21h16"></path>',
    check = '<path d="M20 6 9 17l-5-5"></path>',
    palette = '<path d="M12 3a9 9 0 1 0 0 18h1.4a2.1 2.1 0 0 0 0-4.2H12a1.7 1.7 0 0 1 0-3.4h2.2A6.8 6.8 0 0 0 21 6.6 3.6 3.6 0 0 0 17.4 3H12Z"></path><circle cx="7.4" cy="9" r="1"></circle><circle cx="10" cy="6.4" r="1"></circle><circle cx="14" cy="6.2" r="1"></circle>',
    vision = '<path d="M2.5 12s3.5-6 9.5-6 9.5 6 9.5 6-3.5 6-9.5 6-9.5-6-9.5-6Z"></path><circle cx="9" cy="12" r="1.3"></circle><circle cx="12.8" cy="10.4" r="1.3"></circle><circle cx="14.2" cy="14" r="1.3"></circle>',
    moon = '<path d="M20.5 15.2A8.7 8.7 0 0 1 8.8 3.5 8.8 8.8 0 1 0 20.5 15.2Z"></path>',
    help = '<circle cx="12" cy="12" r="9"></circle><path d="M9.8 9a2.3 2.3 0 1 1 3.4 2c-.8.5-1.2 1-1.2 2M12 17h.01"></path>',
    table = '<rect x="3" y="4" width="18" height="16" rx="2"></rect><path d="M3 9h18M8 9v11M14 9v11"></path>',
    spark = '<path d="m12 3 1.8 4.8L19 9.5l-5.2 1.7L12 16l-1.8-4.8L5 9.5l5.2-1.7L12 3Z"></path>',
    alert = '<path d="M12 4 2.8 20h18.4L12 4Z"></path><path d="M12 10v4M12 17.4h.01"></path>',
    copy = '<rect x="9" y="9" width="11" height="11" rx="2"></rect><path d="M5 15V5a2 2 0 0 1 2-2h8"></path>',
    arrow = '<path d="M5 12h13M13 6l6 6-6 6"></path>'
  )
  shiny::HTML(sprintf('<svg class="icon" viewBox="0 0 24 24" aria-hidden="true" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round">%s</svg>', paths[[name]] %||% paths$spark))
}

# An empty panel that renders as nothing reads as a fault rather than as an
# absence. Every place that can legitimately have nothing to show says what is
# missing and what would fill it.
empty_state_ui <- function(title, detail, icon = "spark") {
  div(class = "empty-state", role = "status",
      icon_svg(icon),
      div(class = "empty-state-title", title),
      p(class = "empty-state-detail", detail))
}

field_shell <- function(input, help, required = TRUE) {
  div(class = "field-shell",
      div(class = "field-label-row", input,
          if (required) span(class = "required-mark", "Required") else span(class = "optional-mark", "Optional")),
      div(class = "field-help", help))
}

# Which columns to offer for a role. The lists here are advisory: a reader can
# still pick something the method will reject, and the engine explains why in
# words. Filtering harder would hide the column they meant to use when the
# type detection guessed wrong.
role_choices <- function(type, data) {
  sig <- data_signature(data)
  switch(type,
    numeric_one = sig$numeric,
    numeric_optional = c("None" = "", stats::setNames(sig$numeric, sig$numeric)),
    numeric_many = sig$numeric,
    numeric_many_optional = sig$numeric,
    categorical_one = unique(c(sig$categorical, setdiff(sig$binary, sig$categorical))),
    categorical_optional = c("None" = "", stats::setNames(sig$categorical, sig$categorical)),
    categorical_many = sig$categorical,
    binary_one = sig$binary,
    ordinal_one = sig$categorical,
    id_one = sig$all,
    predictors = sig$all,
    predictors_optional = sig$all,
    any_many = sig$all,
    numeric_or_factor_one = unique(c(sig$numeric, sig$categorical)),
    numeric_or_date_one = sig$all[vapply(data, function(x) is.numeric(x) || inherits(x, c("Date", "POSIXt")), logical(1))],
    sig$all
  )
}

# This is a first-pass screen of the active columns, not a verdict. A dimmed
# method stays available because column assignments and data preparation can
# change whether the model runs.
# A quick read of whether the loaded columns could satisfy a method. This runs
# for all fifty-three methods on every data change, so it works from the column
# profile rather than trying anything. It is deliberately optimistic: it marks
# a method doubtful only when the data plainly cannot support it, because a
# false discouragement is worse than an error message the reader can act on.
method_feasibility <- function(method_spec, data) {
  if (is.null(data) || !ncol(data)) {
    return(list(possible = TRUE, reason = "Load data to check the column types."))
  }

  sig <- data_signature(data)
  reasons <- character(0)
  required_roles <- method_spec$roles[vapply(method_spec$roles, function(x) isTRUE(x$required), logical(1))]
  minimum_distinct_columns <- 0L
  for (item in required_roles) {
    if (identical(item$type, "lavaan_text")) next
    choices <- unname(role_choices(item$type, data))
    choices <- unique(choices[nzchar(choices)])
    needed <- if (item$type == "numeric_many" && method_spec$id != "descriptives") 2L else 1L
    minimum_distinct_columns <- minimum_distinct_columns + needed
    if (length(choices) < needed) {
      # The role label is a heading such as "Categorical outcome", so it needs an
      # article to sit inside a sentence. The earlier wording read "It needs
      # categorical outcome and the file does not appear to contain that
      # choice", which is not a sentence a reader should be shown.
      label <- if (item$type == "numeric_many") "two numeric columns" else tolower(item$label)
      article <- if (item$type == "numeric_many") "" else if (grepl("^[aeiou]", label)) "an " else "a "
      reasons <- c(reasons, sprintf(
        "It needs %s%s, and no column in this file looks like one.", article, label
      ))
    }
  }
  if (ncol(data) < minimum_distinct_columns) {
    reasons <- c(reasons, sprintf("It needs at least %d different columns for the required roles.", minimum_distinct_columns))
  }

  classification_ids <- c(
    "classification_tree", "random_forest_classification", "nearest_neighbors",
    "elastic_net_classification", "naive_bayes"
  )
  regression_ids <- c("regression_tree", "random_forest_regression", "elastic_net_regression")
  numeric_set_ids <- c(
    "descriptives", "correlation", "pca", "kmeans_clustering", "hierarchical_clustering",
    "dbscan_clustering", "hdbscan_clustering", "factor_analysis", "reliability"
  )
  observed_counts <- vapply(data, function(x) length(unique(stats::na.omit(x))), integer(1))

  if (method_spec$id %in% classification_ids) {
    outcome_candidates <- names(data)[vapply(data, function(x) {
      counts <- table(stats::na.omit(x))
      length(counts) >= 2L && length(counts) <= 12L && min(counts) >= 5L
    }, logical(1))]
    if (!length(outcome_candidates)) {
      reasons <- c(reasons, "It needs a category outcome with at least five complete rows in every category.")
    }
    if (ncol(data) < 2L) reasons <- c(reasons, "It needs an outcome and at least one separate predictor column.")
  }
  if (method_spec$id %in% regression_ids && (length(sig$numeric) < 1L || ncol(data) < 2L)) {
    reasons <- c(reasons, "It needs a numeric outcome and at least one separate predictor column.")
  }
  if (method_spec$id %in% c("independent_t", "mann_whitney") && !any(observed_counts == 2L)) {
    reasons <- c(reasons, "It needs a group column with exactly two observed categories.")
  }
  if (method_spec$id %in% c("one_way_anova", "welch_anova", "kruskal_wallis") && !any(observed_counts >= 3L & observed_counts <= 12L)) {
    reasons <- c(reasons, "It needs a group column with at least three observed categories.")
  }
  if (method_spec$id == "mcnemar" && sum(observed_counts == 2L) < 2L) {
    reasons <- c(reasons, "It needs two different binary columns.")
  }
  if (method_spec$id %in% setdiff(numeric_set_ids, "descriptives") && length(sig$numeric) < 2L) {
    reasons <- c(reasons, "It needs at least two numeric columns.")
  }
  if (method_spec$id %in% c("cfa", "sem") && ncol(data) < 3L) {
    reasons <- c(reasons, "Latent-variable syntax usually needs at least three measured columns.")
  }

  reasons <- unique(reasons)
  list(
    possible = !length(reasons),
    reason = if (length(reasons)) reasons[[1]] else "The detected column types appear to fit this method."
  )
}

catalog_feasibility <- function(data) {
  lapply(ANALYSES, method_feasibility, data = data)
}

# Everything the sidebar needs to describe the current choice in one pass, so
# the method summary, the role inputs, and the feasibility note cannot fall out
# of step with each other.
analysis_selection_context <- function(method_id = NULL, roles = NULL, data_name = NULL) {
  if (!length(method_id) || !method_id %in% names(ANALYSES)) return(NULL)
  spec <- ANALYSES[[method_id]]
  role_labels <- stats::setNames(
    vapply(spec$roles, `[[`, character(1), "label"),
    vapply(spec$roles, `[[`, character(1), "id")
  )
  choices <- character(0)
  for (id in intersect(names(roles %||% list()), names(role_labels))) {
    role <- spec$roles[[match(id, names(role_labels))]]
    if (identical(role$type, "lavaan_text")) next
    value <- trimws(as.character(roles[[id]] %||% character(0)))
    value <- value[!is.na(value) & nzchar(value)]
    if (length(value)) {
      value <- ifelse(nchar(value) > 60L, paste0(substr(value, 1L, 57L), "..."), value)
      shown <- paste(utils::head(value, 4L), collapse = ", ")
      if (length(value) > 4L) shown <- paste0(shown, sprintf(", and %d more", length(value) - 4L))
      choices <- c(choices, sprintf("%s: %s", role_labels[[id]], shown))
    }
  }
  source <- if (length(data_name) && nzchar(trimws(data_name[[1]]))) {
    paste0(" in ", trimws(data_name[[1]]))
  } else {
    ""
  }
  if (!length(choices)) return(paste0(spec$name, source))
  paste0(spec$name, source, " using ", paste(choices, collapse = "; "))
}

# Errors as instructions ---------------------------------------------------
#
# An engine stops with a sentence written for the person who chose the columns,
# and this turns that sentence into a titled card with a suggested next step.
# The intent is that a reader never sees a raw R condition, because "contrasts can
# be applied only to factors with 2 or more levels" tells them nothing they can
# act on.
plain_analysis_error <- function(message, method_id = NULL, roles = NULL, data_name = NULL) {
  text <- trimws(as.character(message %||% ""))
  if (!nzchar(text)) text <- "R stopped without returning an error message."
  lower <- tolower(text)
  method_name_value <- if (length(method_id) && method_id %in% names(ANALYSES)) ANALYSES[[method_id]]$name else "This analysis"
  answer <- list(
    title = paste(method_name_value, "needs a change"),
    reason = text,
    next_step = "Review the assigned columns and analysis settings, then run it again.",
    specific = text,
    context = analysis_selection_context(method_id, roles, data_name),
    technical = text
  )

  if (grepl("choose .* before running|cannot fill more than one role|different role", lower)) {
    answer$reason <- "One or more column roles are empty or use the same column twice."
    answer$next_step <- "Open Assign columns, give every required role a column, and use a different column for each role."
  } else if (grepl("needs:|could not find package|there is no package|namespace", lower)) {
    answer$reason <- "A package used by this method is missing from the R setup."
    answer$next_step <- "Close Daybreak and run source(\"run_daybreak.R\") from the app folder. It will install missing packages before reopening the app."
  } else if (grepl("each outcome category needs at least five complete rows", lower, fixed = TRUE)) {
    answer$reason <- "At least one outcome category has too few complete rows for separate training and holdout portions."
    answer$next_step <- "Choose an outcome with at least five complete rows in every category, add more rows, or use fewer outcome categories."
  } else if (grepl("at least|fewer than|complete rows|five complete rows", lower) &&
             !grepl("categor|levels|binary|numeric column|density|cluster|time-to-event|observed event", lower)) {
    answer$reason <- "Too few usable rows or columns remain for this method. Missing values may also have reduced the usable rows."
    answer$next_step <- "Choose columns with more complete data, add more rows, or use the missing-data workspace before trying again."
  } else if (grepl("0 \\(non-na\\) cases|no complete element pairs|not enough.*observations|no complete rows", lower)) {
    answer$reason <- "No usable rows remain after Daybreak removes rows missing one or more selected values."
    answer$next_step <- "Choose columns with overlapping complete values, or prepare the missing values before running this method."
  } else if (grepl("contrasts can be applied only to factors with 2 or more levels|contrasts.*one level", lower)) {
    answer$reason <- "A selected category has only one value among the rows that remain, so the groups cannot be compared."
    answer$next_step <- "Choose a category column with at least two represented groups, or check whether missing values removed one group."
  } else if (grepl("na/nan/inf|infinite values|non-finite|nonfinite", lower)) {
    answer$reason <- "At least one selected numeric column contains an infinite or otherwise unusable value."
    answer$next_step <- "Replace infinite values with a valid number or a missing value, then run the analysis again."
  } else if (grepl("numeric column|choose numeric|not numeric|numeric outcome", lower)) {
    answer$reason <- "A role that expects numbers received a category, text, date, or another nonnumeric column."
    answer$next_step <- "Open Assign columns and place a numeric column in each numeric role."
  } else if (grepl("categor|exactly two|levels|outcome category|binary", lower)) {
    answer$reason <- "The selected category column does not have the number of observed categories required by this method."
    answer$next_step <- "Choose a category column with the stated number of categories and enough rows in each category."
  } else if (grepl("does not vary|no variation|measurable variation|zero variance|constant", lower)) {
    answer$reason <- "At least one selected column has no usable variation in the rows used by the model."
    answer$next_step <- "Remove constant columns or choose predictors whose values differ across rows."
  } else if (grepl("one row per case|one row in every repeated condition|case-condition", lower)) {
    answer$reason <- "The repeated data do not have exactly one row for every case and condition."
    answer$next_step <- "Check the case ID and condition columns, then remove duplicate case-condition rows or add the missing conditions."
  } else if (grepl("time-to-event values cannot be negative|at least one retained row must contain the event|more observed events", lower)) {
    answer$reason <- "The survival columns do not contain usable follow-up times and observed events."
    answer$next_step <- "Use nonnegative follow-up times and confirm which of the two status values means that the event occurred."
  } else if (grepl("equally spaced|duplicate values.*time|arima order|frequency must", lower)) {
    answer$reason <- "The time column or ARIMA settings do not describe one regularly spaced series."
    answer$next_step <- "Keep one row per time point, fill or combine gaps, and use smaller nonnegative p, d, and q settings."
  } else if (grepl("singular|converg|rank-deficient|computationally singular|unstable estimates|did not settle|redundant columns|overlapping predictor", lower)) {
    answer$reason <- "The model could not find a stable answer from these columns. Predictors may repeat the same information, or the model may be too large for the data."
    answer$next_step <- "Remove overlapping predictors, use fewer model terms, or add more complete rows."
  } else if (grepl("cluster|eps|minpts|density", lower)) {
    answer$reason <- "The clustering settings do not match the number or spacing of the selected rows."
    answer$next_step <- "Try more complete numeric columns, a different distance radius, or a smaller minimum-points setting."
  }
  answer
}

analysis_error_notification <- function(error) {
  div(
    class = "analysis-notification",
    strong(error$title),
    span(error$specific %||% error$reason)
  )
}

# An error gets the same visual weight as a result, not less. It is the screen
# the reader is looking at, and burying it in a small red line would leave them
# wondering whether anything happened at all.
analysis_error_ui <- function(error) {
  div(
    class = "analysis-error-card", role = "alert",
    div(class = "analysis-error-mark", icon_svg("help")),
    div(class = "analysis-error-copy",
        div(class = "eyebrow", "Analysis needs attention"),
        h2(error$title),
        if (length(error$context) && nzchar(error$context))
          p(class = "analysis-error-context", error$context),
        div(class = "analysis-error-specific", strong("Why it stopped"), p(error$specific %||% error$reason)),
        if (!identical(trimws(error$reason), trimws(error$specific %||% "")))
          div(class = "analysis-error-meaning", strong("What that means"), p(class = "analysis-error-reason", error$reason)),
        div(class = "analysis-error-next", strong("What to try"), p(error$next_step)),
        tags$details(class = "analysis-error-details",
                     tags$summary("Show the exact R message"),
                     tags$code(error$technical)))
  )
}

# Hand-written defaults for the built-in examples. These exist so that every
# sample opens on a result that demonstrates something, rather than on whatever
# the type-based guess happened to pick.
sample_role_defaults <- function(sample_id, method_id) {
  defaults <- list(
    iris = list(
      one_way_anova = list(outcome = "Sepal.Length", group = "Species")
    ),
    tooth = list(
      one_way_anova = list(outcome = "len", group = "dose")
    ),
    cars = list(
      linear_regression = list(outcome = "mpg", predictors = c("wt", "hp"))
    ),
    air = list(
      missingness = list(variables = c("Ozone", "Solar.R", "Wind", "Temp"))
    ),
    admissions = list(
      chi_square = list(row = "Admit", column = "Gender")
    ),
    orthodont = list(
      growth_model = list(outcome = "distance", time = "age", cluster = "Subject", controls = character(0))
    ),
    lung = list(
      kaplan_meier = list(time = "time", status = "status", group = "celltype")
    ),
    nottem = list(
      time_series = list(outcome = "temperature_f", time = "date")
    )
  )
  if (!length(sample_id) || !length(method_id) || !sample_id %in% names(defaults)) return(list())
  defaults[[sample_id]][[method_id]] %||% list()
}

preferred_name <- function(values, pattern) {
  hit <- grep(pattern, values, ignore.case = TRUE, perl = TRUE)
  if (length(hit)) values[[hit[[1]]]] else character(0)
}

# Preselecting roles is what lets a reader open a sample and press Run without
# first learning what an outcome is. The named defaults per sample come first,
# and a type-based guess fills anything left over.
default_role_values <- function(method_spec, data, sample_id = NULL, method_id = NULL) {
  explicit <- sample_role_defaults(sample_id, method_id)
  roles <- method_spec$roles
  reserved <- list()
  patterns <- c(
    id = "(^id$|_id$|id_|person|participant|student|subject|school|site|cluster|unit)",
    cluster = "(^id$|_id$|id_|person|participant|student|subject|school|site|cluster|unit)",
    time = "(^time$|date|day|week|month|year|period|wave|visit|occasion|age)",
    status = "status|event|censor|death|failure"
  )

  for (role in roles) {
    if (role$id %in% names(patterns)) {
      values <- unname(role_choices(role$type, data))
      reserved[[role$id]] <- preferred_name(values[nzchar(values)], patterns[[role$id]])
    }
  }

  out <- list()
  used <- character(0)
  for (role in roles) {
    if (!is.null(explicit[[role$id]])) {
      value <- explicit[[role$id]]
    } else if (identical(role$type, "lavaan_text")) {
      value <- ""
    } else if (!isTRUE(role$required)) {
      value <- if (grepl("many|predictors", role$type)) character(0) else ""
    } else {
      values <- unname(role_choices(role$type, data))
      values <- values[nzchar(values)]
      available <- setdiff(values, used)
      multiple <- grepl("many|predictors", role$type)

      if (length(reserved[[role$id]])) {
        value <- reserved[[role$id]]
      } else if (multiple) {
        future_reserved <- unique(unlist(reserved[setdiff(names(reserved), names(out))], use.names = FALSE))
        candidates <- setdiff(available, future_reserved)
        if (role$id == "predictors") {
          sig <- data_signature(data)
          candidates <- unique(c(intersect(sig$categorical, candidates), intersect(sig$numeric, candidates), candidates))
          value <- head(candidates, min(2L, length(candidates)))
        } else {
          value <- head(candidates, min(3L, length(candidates)))
        }
      } else {
        pattern <- switch(role$id,
                          outcome = "outcome|score|response|value|measure|distance|length|temperature|count",
                          group = "group|condition|treatment|class|species|category|type|sex|gender",
                          row = "group|condition|treatment|class|category|type|sex|gender|admit",
                          column = "group|condition|treatment|class|category|type|sex|gender|department|dept",
                          before = "before|pre|baseline|first|wave1|time1",
                          after = "after|post|follow|second|wave2|time2",
                          "")
        match <- if (nzchar(pattern)) preferred_name(available, pattern) else character(0)
        value <- if (length(match)) match else head(available, 1L)
      }
    }
    out[[role$id]] <- value
    used <- unique(c(used, value[nzchar(value)]))
  }
  out
}

# One input per role, built from the role type. Multi-select roles keep the
# order the reader chose, because for several methods the first predictor is
# the one the figure and the headline coefficient are drawn from.
role_input <- function(role, data, current = NULL) {
  sig <- data_signature(data)
  choices <- role_choices(role$type, data)
  multiple <- role$type %in% c("numeric_many", "numeric_many_optional", "categorical_many", "predictors", "predictors_optional", "any_many")
  if (role$type == "lavaan_text") {
    return(field_shell(
      div(tags$label(`for` = role$id, role$label),
          textAreaInput(role$id, label = NULL, value = current %||% "", rows = 8, width = "100%",
                        placeholder = "factor1 =~ item1 + item2 + item3")), role$help, role$required))
  }
  selected <- current
  if (is.null(selected) && length(choices)) {
    values <- unname(choices)
    if (multiple) {
      if (!role$required) {
        selected <- character(0)
      } else if (role$id == "predictors") {
        likely_outcomes <- unique(c(sig$numeric[1], sig$binary[1]))
        candidates <- unique(c(setdiff(sig$categorical, likely_outcomes), setdiff(sig$numeric, likely_outcomes), sig$all))
        selected <- head(candidates[candidates %in% values], min(2, length(candidates)))
      } else {
        selected <- head(values, min(3, length(values)))
      }
    } else {
      index <- switch(role$id,
                      after = 2, y = 2, column = 2, moderator = 2, exposure = 2,
                      mediator = 3, time = 2, 1)
      if (role$id %in% c("id", "cluster")) {
        match_id <- grep("(^id$|_id$|id_|person|student|school|site|cluster|unit)", values, ignore.case = TRUE)
        index <- if (length(match_id)) match_id[1] else length(values)
      }
      selected <- values[min(index, length(values))]
    }
  }
  field_shell(
    div(tags$label(`for` = role$id, role$label),
        selectizeInput(role$id, label = NULL, choices = choices, selected = selected,
                       multiple = multiple, width = "100%",
                       options = list(placeholder = if (multiple) "Choose columns" else "Choose a column",
                                      plugins = if (multiple) list("remove_button") else list()))),
    role$help, role$required
  )
}

analysis_options_ui <- function(method_id, data, ordinal_outcome = NULL) {
  conf <- sliderInput("conf_level", "Confidence level", min = 0.80, max = 0.99, value = 0.95, step = 0.01)
  specific <- switch(method_id,
    one_sample_t = numericInput("mu", "Reference mean", value = 0),
    independent_t = checkboxInput("equal_var", "Use a pooled equal-variance test", FALSE),
    correlation = selectInput("cor_method", "Correlation type", c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall")),
    partial_correlation = selectInput("cor_method", "Correlation type", c("Pearson" = "pearson", "Spearman" = "spearman")),
    fisher_exact = numericInput("simulations", "Monte Carlo samples for larger tables", 10000, min = 2000, step = 1000),
    one_proportion = tagList(numericInput("reference_proportion", "Reference proportion", 0.50, min = 0.01, max = 0.99, step = 0.01),
                             checkboxInput("continuity_correction", "Use continuity correction", FALSE)),
    two_proportions = checkboxInput("continuity_correction", "Use continuity correction", FALSE),
    ordinal_regression = {
      choices <- if (length(ordinal_outcome) && ordinal_outcome %in% names(data)) {
        column <- data[[ordinal_outcome]]
        if (is.factor(column)) levels(droplevels(column)) else unique(as.character(stats::na.omit(column)))
      } else character(0)
      selectizeInput(
        "ordinal_order", "Outcome order, lowest to highest", choices = choices,
        selected = choices, multiple = TRUE,
        options = list(plugins = list("remove_button", "drag_drop"))
      )
    },
    factorial_anova = checkboxInput("factor_interaction", "Include the interaction between the first two factors", TRUE),
    pca = numericInput("components", "Components to retain in tables", 3, min = 1, step = 1),
    kmeans_clustering = tagList(numericInput("clusters", "Number of clusters", 3, min = 2, step = 1),
                                numericInput("random_seed", "Random seed", 2026, min = 1, step = 1)),
    hierarchical_clustering = numericInput("clusters", "Number of clusters", 3, min = 2, step = 1),
    classification_tree = prediction_options_ui("tree"),
    regression_tree = prediction_options_ui("tree"),
    random_forest_classification = prediction_options_ui("forest"),
    random_forest_regression = prediction_options_ui("forest"),
    nearest_neighbors = prediction_options_ui("neighbors"),
    elastic_net_classification = prediction_options_ui("elastic_net"),
    elastic_net_regression = prediction_options_ui("elastic_net"),
    naive_bayes = prediction_options_ui("naive_bayes"),
    dbscan_clustering = tagList(
      numericInput("dbscan_eps", "Neighborhood radius", 0.7, min = 0.01, step = 0.05),
      numericInput("density_min_points", "Minimum nearby points", 5, min = 2, step = 1)
    ),
    hdbscan_clustering = numericInput("density_min_points", "Minimum cluster size", 5, min = 2, step = 1),
    factor_analysis = tagList(
      numericInput("factors", "Number of factors", 2, min = 1, step = 1),
      selectInput("rotation", "Rotation", c("Oblimin, factors may correlate" = "oblimin", "Promax, factors may correlate" = "promax", "Varimax, factors uncorrelated" = "varimax", "None" = "none")),
      selectInput("factor_method", "Extraction", c("Minimum residual" = "minres", "Maximum likelihood" = "ml", "Principal axis" = "pa"))
    ),
    cfa = lavaan_options_ui(data), sem = lavaan_options_ui(data),
    mediation = numericInput("bootstrap", "Bootstrap samples", 1000, min = 200, step = 200),
    hlm_linear = checkboxInput("reml", "Use restricted maximum likelihood", TRUE),
    growth_model = tagList(checkboxInput("reml", "Use restricted maximum likelihood", TRUE),
                           checkboxInput("growth_random_slope", "Let the time slope vary across units", TRUE)),
    time_series = tagList(
      div(class = "three-up", numericInput("ar_p", "AR order (p)", 1, min = 0, max = 10),
          numericInput("ar_d", "Difference (d)", 0, min = 0, max = 2),
          numericInput("ar_q", "MA order (q)", 0, min = 0, max = 10)),
      div(class = "two-up",
          numericInput("frequency", "Observations per cycle", 1, min = 1, step = 1),
          numericInput("forecast_horizon", "Points to project ahead", 0, min = 0, step = 1)),
      p(class = "micro-note", "Leave the projection at zero to fit the recorded period only. Daybreak caps a projection at one quarter of the recorded length.")
    ),
    NULL
  )
  confidence_methods <- c(
    "one_sample_t", "independent_t", "paired_t", "mann_whitney", "paired_wilcoxon",
    "one_proportion", "two_proportions",
    "correlation", "partial_correlation", "linear_regression", "moderation",
    "logistic_regression", "poisson_regression", "negative_binomial",
    "multinomial_regression", "ordinal_regression", "hlm_linear", "hlm_logistic",
    "growth_model", "kaplan_meier", "cox_regression", "time_series"
  )
  if (is.null(specific) && !method_id %in% confidence_methods) return(NULL)
  div(class = "advanced-box",
      tags$details(
        tags$summary("Analysis settings"),
        div(class = "advanced-body", specific,
            if (method_id %in% confidence_methods) conf)
      ))
}

# The prediction settings all affect reproducibility, so the seed sits beside
# them rather than in a separate advanced panel. A reader changing the split
# proportion should see, in the same glance, the thing that makes their result
# repeatable.
prediction_options_ui <- function(engine) {
  engine_controls <- switch(engine,
    tree = tagList(
      numericInput("tree_cp", "Tree complexity penalty", 0.01, min = 0, max = 1, step = 0.005),
      numericInput("tree_minsplit", "Minimum rows before a split", 20, min = 2, step = 1)
    ),
    forest = numericInput("forest_trees", "Number of trees", 500, min = 100, max = 5000, step = 100),
    neighbors = numericInput("neighbors", "Number of neighbors", 5, min = 1, max = 100, step = 1),
    elastic_net = tagList(
      sliderInput("elastic_alpha", "Penalty mix: ridge to lasso", min = 0, max = 1, value = 0.5, step = 0.05),
      numericInput("cv_folds", "Cross-validation folds", 5, min = 3, max = 10, step = 1)
    ),
    naive_bayes = numericInput("naive_laplace", "Laplace smoothing", 1, min = 0, max = 20, step = 1)
  )
  tagList(
    sliderInput("train_fraction", "Rows used for training", min = 0.60, max = 0.90, value = 0.80, step = 0.05),
    numericInput("random_seed", "Random seed", 2026, min = 1, step = 1),
    engine_controls
  )
}

# Latent variable settings are shown in full rather than reduced to a preset.
# The estimator and the missing-data treatment change what the numbers mean,
# and hiding them would leave the reader unable to report what they ran.
lavaan_options_ui <- function(data) {
  tagList(
    selectInput("estimator", "Estimator", c("MLR, adjusted standard errors" = "MLR", "Maximum likelihood" = "ML", "WLSMV for ordered indicators" = "WLSMV", "Diagonally weighted least squares" = "DWLS")),
    selectInput("missing_method", "Missing-data method", c("Full-information maximum likelihood" = "fiml", "Listwise deletion" = "listwise")),
    selectizeInput("ordered", "Ordered indicators, if any", choices = names(data), multiple = TRUE,
                   options = list(plugins = list("remove_button")))
  )
}

# Reading the form --------------------------------------------------------
#
# A missing required role stops here with the role's own label rather than its
# input id, so the message names the thing the reader can see on screen.
collect_roles <- function(input, method_spec) {
  out <- list()
  for (r in method_spec$roles) {
    value <- input[[r$id]]
    if (r$required && (is.null(value) || !length(value) || all(!nzchar(value)))) {
      stop(sprintf("Choose %s before running the analysis.", tolower(r$label)))
    }
    out[[r$id]] <- value %||% if (grepl("many|predictors", r$type)) character(0) else ""
  }
  out
}

collect_options <- function(input) {
  ids <- c("conf_level", "mu", "equal_var", "cor_method", "simulations", "reference_proportion", "continuity_correction",
           "factor_interaction", "components", "clusters", "random_seed", "factors",
           "rotation", "factor_method", "estimator", "missing_method", "ordered", "ordinal_order", "bootstrap", "reml",
           "growth_random_slope", "ar_p", "ar_d", "ar_q", "frequency", "forecast_horizon",
           "train_fraction", "tree_cp", "tree_minsplit", "forest_trees", "neighbors",
           "elastic_alpha", "cv_folds", "naive_laplace", "dbscan_eps", "density_min_points")
  stats::setNames(lapply(ids, function(id) input[[id]]), ids)
}

method_summary_ui <- function(method_id, compact = FALSE) {
  x <- ANALYSES[[method_id]]
  absent <- missing_packages(x)
  type <- if (identical(x$kind, "machine_learning")) "Machine learning" else "Statistics"
  if (isTRUE(compact)) {
    return(tags$details(
      class = "method-summary method-summary-compact",
      tags$summary(div(span(class = "method-kicker", type), strong(x$name), span(x$question))),
      div(class = "method-summary-compact-body", p(x$description),
          if (length(absent)) div(class = "package-note", icon_svg("spark"),
                                  span(sprintf("Install %s to run this method.", paste(absent, collapse = ", ")))))
    ))
  }
  div(class = "method-summary",
      div(class = "method-kicker", paste(type, x$family, sep = " · "), span(class = paste("level-chip", tolower(x$level)), x$level)),
      h2(x$name), p(class = "method-question", x$question), p(class = "method-description", x$description),
      if (length(absent)) div(class = "package-note", icon_svg("spark"),
                             span(sprintf("Install %s to run this method.", paste(absent, collapse = ", "))))
  )
}

# A compact fingerprint of everything that feeds an analysis. Comparing the
# fingerprint recorded at run time with the current state of the form is how
# the result panel knows to say that the numbers on screen no longer match the
# settings beside them. Options are sorted because collect_options returns them
# in whatever order the inputs happened to render.
analysis_signature <- function(method_id, roles, options) {
  roles <- roles[order(names(roles))]
  options <- options[setdiff(names(options), ".method_id")]
  options <- options[order(names(options))]
  flatten <- function(x) paste(vapply(x, function(v) paste(as.character(v), collapse = "|"), character(1)), collapse = ";")
  paste(method_id, flatten(roles), flatten(options), sep = "::")
}

# Shown above a result whose settings have moved on since it was produced.
stale_result_banner <- function() {
  div(
    class = "stale-banner", role = "status",
    icon_svg("alert"),
    div(
      strong("These numbers are from the previous settings."),
      p("Something below the method has changed since this ran. Run the analysis again to bring the reading up to date.")
    ),
    actionButton("run_analysis_stale", "Run again", class = "stale-banner-button btn")
  )
}

static_dialog_ui <- function(id, title, body) {
  div(
    id = id, class = "static-dialog-overlay", role = "dialog", `aria-modal` = "true",
    `aria-labelledby` = paste0(id, "-title"), `aria-hidden` = "true",
    tags$button(type = "button", class = "static-dialog-backdrop", `aria-label` = paste("Close", title)),
    div(
      class = "static-dialog-card",
      div(class = "static-dialog-header",
          h2(id = paste0(id, "-title"), title),
          tags$button(type = "button", class = "static-dialog-close",
                      `aria-label` = paste("Close", title), "\u00d7")),
      div(class = "static-dialog-body", body)
    )
  )
}

# Each method carries a small diagram. They share line weights and a single
# accent so the set reads as one family, and none of them carries a text label:
# the card copy beside the drawing does that, and a label inside a 120 pixel
# figure would be too small to read anyway.
method_visual_ui <- function(method) {
  id <- method$id
  # These small diagrams share the same line weights and colors. The card copy
  # carries the labels, so the drawings can stay quiet and legible.
  drawings <- list(
    descriptives = '<path class="viz-axis" d="M8 56h54"/><rect class="viz-muted" x="12" y="43" width="7" height="13"/><rect class="viz-secondary" x="21" y="33" width="7" height="23"/><rect class="viz-primary" x="30" y="19" width="7" height="37"/><rect class="viz-secondary" x="39" y="28" width="7" height="28"/><rect class="viz-muted" x="48" y="40" width="7" height="16"/><path class="viz-trend" d="M34 12v48"/><circle class="viz-primary" cx="34" cy="12" r="3"/>',
    frequencies = '<path class="viz-axis" d="M8 58h54"/><rect class="viz-secondary" x="12" y="20" width="10" height="38" rx="2"/><rect class="viz-primary" x="30" y="34" width="10" height="24" rx="2"/><rect class="viz-muted" x="48" y="43" width="10" height="15" rx="2"/>',
    missingness = '<g class="viz-grid"><rect x="9" y="10" width="11" height="11"/><rect x="22" y="10" width="11" height="11"/><rect x="35" y="10" width="11" height="11"/><rect x="48" y="10" width="11" height="11"/><rect x="9" y="23" width="11" height="11"/><rect class="viz-missing" x="22" y="23" width="11" height="11"/><rect x="35" y="23" width="11" height="11"/><rect x="48" y="23" width="11" height="11"/><rect x="9" y="36" width="11" height="11"/><rect x="22" y="36" width="11" height="11"/><rect class="viz-missing" x="35" y="36" width="11" height="11"/><rect x="48" y="36" width="11" height="11"/><rect x="9" y="49" width="11" height="11"/><rect class="viz-missing" x="22" y="49" width="11" height="11"/><rect x="35" y="49" width="11" height="11"/><rect class="viz-missing" x="48" y="49" width="11" height="11"/></g>',
    one_sample_t = '<path class="viz-muted-line" d="M7 53c9 0 10-35 28-35s19 35 28 35"/><path class="viz-axis" d="M7 54h56"/><path class="viz-secondary-line" d="M25 12v47"/><path class="viz-trend" d="M39 9v50"/>',
    independent_t = '<path class="viz-node" d="M11 20h20v32H11zM39 12h20v40H39z"/><path class="viz-muted-line" d="M11 35h20M39 27h20M21 15v5M21 52v6M49 7v5M49 52v6"/><circle class="viz-secondary" cx="21" cy="35" r="3"/><circle class="viz-primary" cx="49" cy="27" r="3"/>',
    paired_t = '<path class="viz-muted-line" d="M17 48 53 36M17 42 53 23M17 33 53 29M17 27 53 15M17 53 53 42"/><circle class="viz-secondary" cx="17" cy="48" r="3"/><circle class="viz-secondary" cx="17" cy="42" r="3"/><circle class="viz-secondary" cx="17" cy="33" r="3"/><circle class="viz-primary" cx="53" cy="36" r="3"/><circle class="viz-primary" cx="53" cy="23" r="3"/><circle class="viz-primary" cx="53" cy="29" r="3"/>',
    mann_whitney = '<path class="viz-axis" d="M8 27h54M8 50h54"/><circle class="viz-secondary" cx="15" cy="23" r="3"/><circle class="viz-secondary" cx="23" cy="30" r="3"/><circle class="viz-secondary" cx="31" cy="23" r="3"/><circle class="viz-secondary" cx="42" cy="30" r="3"/><circle class="viz-primary" cx="29" cy="46" r="3"/><circle class="viz-primary" cx="39" cy="53" r="3"/><circle class="viz-primary" cx="49" cy="46" r="3"/><circle class="viz-primary" cx="58" cy="53" r="3"/>',
    paired_wilcoxon = '<path class="viz-muted-line" d="M16 49 54 31M16 38 54 45M16 27 54 16M16 19 54 25M16 55 54 39"/><circle class="viz-secondary" cx="16" cy="49" r="3"/><circle class="viz-secondary" cx="16" cy="38" r="3"/><circle class="viz-secondary" cx="16" cy="27" r="3"/><circle class="viz-primary" cx="54" cy="31" r="3"/><circle class="viz-primary" cx="54" cy="45" r="3"/><circle class="viz-primary" cx="54" cy="16" r="3"/>',
    one_way_anova = '<path class="viz-node" d="M8 24h15v26H8zM28 15h15v35H28zM48 30h15v20H48z"/><path class="viz-muted-line" d="M8 36h15M28 27h15M48 39h15M15.5 18v6M35.5 9v6M55.5 24v6"/>',
    welch_anova = '<path class="viz-muted-line" d="M10 22h30M10 17v10M40 17v10M24 39h38M24 34v10M62 34v10M15 54h18M15 49v10M33 49v10"/><circle class="viz-secondary" cx="25" cy="22" r="4"/><circle class="viz-primary" cx="47" cy="39" r="4"/><circle class="viz-muted" cx="24" cy="54" r="4"/>',
    kruskal_wallis = '<path class="viz-axis" d="M9 18h52M9 35h52M9 52h52"/><circle class="viz-secondary" cx="17" cy="18" r="3"/><circle class="viz-secondary" cx="25" cy="18" r="3"/><circle class="viz-secondary" cx="34" cy="18" r="3"/><circle class="viz-primary" cx="27" cy="35" r="3"/><circle class="viz-primary" cx="38" cy="35" r="3"/><circle class="viz-primary" cx="47" cy="35" r="3"/><circle class="viz-muted" cx="35" cy="52" r="3"/><circle class="viz-muted" cx="48" cy="52" r="3"/><circle class="viz-muted" cx="56" cy="52" r="3"/>',
    repeated_anova = '<path class="viz-axis" d="M10 57h50"/><path class="viz-muted-line" d="M12 46 35 33 58 19M12 51 35 40 58 35M12 34 35 29 58 24M12 43 35 24 58 16"/><path class="viz-trend" d="M12 44 35 32 58 24"/><circle class="viz-secondary" cx="12" cy="44" r="3"/><circle class="viz-primary" cx="35" cy="32" r="3"/><circle class="viz-muted" cx="58" cy="24" r="3"/>',
    chi_square = '<rect class="viz-secondary" x="9" y="11" width="20" height="17" rx="2"/><rect class="viz-primary-soft" x="32" y="11" width="29" height="17" rx="2"/><rect class="viz-secondary-soft" x="9" y="31" width="34" height="26" rx="2"/><rect class="viz-primary" x="46" y="31" width="15" height="26" rx="2"/>',
    fisher_exact = '<rect class="viz-node" x="13" y="13" width="20" height="19" rx="2"/><rect class="viz-node" x="36" y="13" width="20" height="19" rx="2"/><rect class="viz-node" x="13" y="35" width="20" height="19" rx="2"/><rect class="viz-primary-soft" x="36" y="35" width="20" height="19" rx="2"/>',
    mcnemar = '<rect class="viz-node" x="11" y="12" width="19" height="19" rx="2"/><rect class="viz-secondary-soft" x="39" y="12" width="19" height="19" rx="2"/><rect class="viz-primary-soft" x="11" y="39" width="19" height="19" rx="2"/><rect class="viz-node" x="39" y="39" width="19" height="19" rx="2"/><path class="viz-trend" d="M31 22h7M35 18l4 4-4 4M38 49h-7M34 45l-4 4 4 4"/>',
    one_proportion = '<circle class="viz-group" cx="34" cy="34" r="23"/><path class="viz-primary-stroke" d="M34 11a23 23 0 1 1-21 32"/><circle class="viz-primary" cx="13" cy="43" r="3"/>',
    two_proportions = '<rect class="viz-group" x="9" y="18" width="52" height="13" rx="5"/><rect class="viz-secondary" x="9" y="18" width="36" height="13" rx="5"/><rect class="viz-group" x="9" y="40" width="52" height="13" rx="5"/><rect class="viz-primary" x="9" y="40" width="25" height="13" rx="5"/>',
    factorial_anova = '<path class="viz-axis" d="M9 57h52M10 57V10"/><path class="viz-secondary-line" d="M15 42 55 19"/><path class="viz-trend" d="M15 24 55 46"/><circle class="viz-secondary" cx="15" cy="42" r="3"/><circle class="viz-secondary" cx="55" cy="19" r="3"/><circle class="viz-primary" cx="15" cy="24" r="3"/><circle class="viz-primary" cx="55" cy="46" r="3"/>',
    friedman = '<path class="viz-axis" d="M12 12v48M35 12v48M58 12v48"/><path class="viz-muted-line" d="M12 48 35 25 58 18M12 22 35 43 58 37M12 36 35 18 58 47M12 54 35 50 58 29"/><circle class="viz-secondary" cx="12" cy="48" r="2.7"/><circle class="viz-primary" cx="35" cy="25" r="2.7"/><circle class="viz-muted" cx="58" cy="18" r="2.7"/>',
    correlation = '<path class="viz-axis" d="M8 58h54M9 58V10"/><circle class="viz-secondary" cx="15" cy="48" r="3"/><circle class="viz-secondary" cx="23" cy="42" r="3"/><circle class="viz-primary" cx="31" cy="35" r="3"/><circle class="viz-primary" cx="42" cy="27" r="3"/><circle class="viz-primary" cx="55" cy="17" r="3"/><path class="viz-trend" d="M12 52 58 14"/>',
    partial_correlation = '<path class="viz-axis" d="M7 58h55M8 58V10"/><circle class="viz-muted" cx="14" cy="48" r="2.7"/><circle class="viz-secondary" cx="23" cy="39" r="2.7"/><circle class="viz-muted" cx="31" cy="36" r="2.7"/><circle class="viz-primary" cx="40" cy="28" r="2.7"/><circle class="viz-muted" cx="49" cy="24" r="2.7"/><circle class="viz-primary" cx="57" cy="16" r="2.7"/><path class="viz-trend" d="M12 50 59 14"/><path class="viz-muted-line" d="M18 15h15M25.5 10v10M45 48h15M52.5 43v10"/>',
    linear_regression = '<path class="viz-band" d="M10 48 56 12 61 20 15 56z"/><path class="viz-trend" d="M11 52 59 16"/><circle class="viz-secondary" cx="16" cy="47" r="3"/><circle class="viz-secondary" cx="28" cy="38" r="3"/><circle class="viz-primary" cx="39" cy="31" r="3"/><circle class="viz-primary" cx="54" cy="18" r="3"/>',
    moderation = '<path class="viz-axis" d="M8 58h54M9 58V10"/><path class="viz-secondary-line" d="M13 48 58 18"/><path class="viz-trend" d="M13 36 58 29"/>',
    mediation = '<rect class="viz-node" x="4" y="27" width="16" height="15" rx="3"/><circle class="viz-secondary" cx="35" cy="34" r="10"/><rect class="viz-node" x="51" y="27" width="16" height="15" rx="3"/><path class="viz-muted-line" d="M20 34h5M45 34h6M12 27C18 8 52 8 59 27"/>',
    logistic_regression = '<path class="viz-axis" d="M8 58h54M9 58V10"/><path class="viz-trend" d="M11 52c13 0 15-2 21-18 6-15 11-18 27-18"/><circle class="viz-secondary" cx="17" cy="50" r="3"/><circle class="viz-secondary" cx="27" cy="47" r="3"/><circle class="viz-primary" cx="45" cy="20" r="3"/><circle class="viz-primary" cx="56" cy="16" r="3"/>',
    multinomial_regression = '<path class="viz-secondary-line" d="M9 48c15-26 25-27 52-5"/><path class="viz-trend" d="M9 36c18-9 30-5 52 15"/><path class="viz-muted-line" d="M9 55c18-3 29-21 52-39"/>',
    ordinal_regression = '<rect class="viz-secondary-soft" x="9" y="16" width="13" height="39" rx="2"/><rect class="viz-secondary" x="22" y="16" width="15" height="39"/><rect class="viz-primary-soft" x="37" y="16" width="11" height="39"/><rect class="viz-primary" x="48" y="16" width="13" height="39" rx="2"/><path class="viz-muted-line" d="M22 12v47M37 12v47M48 12v47"/>',
    poisson_regression = '<path class="viz-axis" d="M8 58h54"/><rect class="viz-secondary-soft" x="11" y="45" width="7" height="13"/><rect class="viz-secondary-soft" x="21" y="36" width="7" height="22"/><rect class="viz-secondary-soft" x="31" y="27" width="7" height="31"/><rect class="viz-secondary-soft" x="41" y="19" width="7" height="39"/><rect class="viz-secondary-soft" x="51" y="12" width="7" height="46"/><circle class="viz-primary" cx="14.5" cy="43" r="2.5"/><circle class="viz-primary" cx="24.5" cy="34" r="2.5"/><circle class="viz-primary" cx="34.5" cy="26" r="2.5"/><circle class="viz-primary" cx="44.5" cy="18" r="2.5"/><circle class="viz-primary" cx="54.5" cy="11" r="2.5"/>',
    negative_binomial = '<path class="viz-axis" d="M8 58h54"/><circle class="viz-secondary" cx="14" cy="49" r="3"/><circle class="viz-secondary" cx="22" cy="24" r="3"/><circle class="viz-secondary" cx="30" cy="45" r="3"/><circle class="viz-primary" cx="38" cy="15" r="3"/><circle class="viz-primary" cx="46" cy="40" r="3"/><circle class="viz-primary" cx="55" cy="10" r="3"/><path class="viz-trend" d="M11 47c17-5 26-13 48-32"/>',
    classification_tree = '<rect class="viz-node" x="28" y="7" width="14" height="10" rx="2"/><path class="viz-muted-line" d="M35 17v8M35 25 18 37M35 25l17 12M18 37v8M52 37v8"/><circle class="viz-secondary" cx="18" cy="37" r="3.5"/><circle class="viz-primary" cx="52" cy="37" r="3.5"/><rect class="viz-secondary-soft" x="8" y="47" width="20" height="11" rx="3"/><circle class="viz-secondary" cx="14" cy="52.5" r="2.5"/><circle class="viz-secondary" cx="22" cy="52.5" r="2.5"/><rect class="viz-primary-soft" x="42" y="47" width="20" height="11" rx="3"/><circle class="viz-primary" cx="48" cy="52.5" r="2.5"/><circle class="viz-primary" cx="56" cy="52.5" r="2.5"/>',
    regression_tree = '<rect class="viz-node" x="28" y="7" width="14" height="10" rx="2"/><path class="viz-muted-line" d="M35 17v8M35 25 18 37M35 25l17 12M18 37v8M52 37v8"/><circle class="viz-secondary" cx="18" cy="37" r="3.5"/><circle class="viz-primary" cx="52" cy="37" r="3.5"/><rect class="viz-node" x="8" y="47" width="20" height="11" rx="3"/><path class="viz-secondary-line" d="M12 54h12M18 49v8"/><rect class="viz-node" x="42" y="47" width="20" height="11" rx="3"/><path class="viz-trend" d="M46 54h12M55 49v8"/>',
    random_forest_classification = '<path class="viz-muted-line" d="M14 32V20m0 7-6-6m6 6 6-6M35 30V16m0 8-6-7m6 7 6-7M56 32V20m0 7-6-6m6 6 6-6M14 32 27 43M35 30v13M56 32 43 43"/><circle class="viz-secondary" cx="14" cy="16" r="3.5"/><circle class="viz-primary" cx="35" cy="12" r="3.5"/><circle class="viz-secondary" cx="56" cy="16" r="3.5"/><rect class="viz-node" x="24" y="44" width="22" height="14" rx="4"/><circle class="viz-secondary" cx="29" cy="51" r="2.6"/><circle class="viz-secondary" cx="35" cy="51" r="2.6"/><circle class="viz-primary" cx="41" cy="51" r="2.6"/>',
    random_forest_regression = '<path class="viz-muted-line" d="M14 31V19m0 7-6-6m6 6 6-6M35 29V15m0 8-6-7m6 7 6-7M56 31V19m0 7-6-6m6 6 6-6"/><circle class="viz-secondary" cx="14" cy="15" r="3.5"/><circle class="viz-primary" cx="35" cy="11" r="3.5"/><circle class="viz-primary-soft" cx="56" cy="15" r="3.5"/><path class="viz-muted-line" d="M14 33v12M35 31v14M56 33v12M9 54h52"/><circle class="viz-secondary" cx="20" cy="54" r="3"/><circle class="viz-primary" cx="38" cy="54" r="3"/><circle class="viz-muted" cx="51" cy="54" r="3"/><path class="viz-trend" d="M36 48v12"/>',
    nearest_neighbors = '<circle class="viz-group" cx="35" cy="34" r="17"/><circle class="viz-primary" cx="35" cy="34" r="4"/><circle class="viz-secondary" cx="25" cy="29" r="3"/><circle class="viz-secondary" cx="43" cy="25" r="3"/><circle class="viz-secondary" cx="46" cy="42" r="3"/><circle class="viz-muted" cx="15" cy="52" r="3"/><circle class="viz-muted" cx="58" cy="15" r="3"/><path class="viz-muted-line" d="M35 34 25 29M35 34 43 25M35 34 46 42"/>',
    elastic_net_classification = '<path class="viz-axis" d="M8 58h54M9 58V10"/><path class="viz-muted-line" d="M11 51c12 0 17-5 23-17 6-12 12-17 26-17"/><path class="viz-secondary-line" d="M11 53c15 0 19-5 24-19 4-12 11-19 25-19"/><path class="viz-trend" d="M11 49c10 0 17-4 24-14 8-11 14-15 25-15"/><circle class="viz-secondary" cx="18" cy="51" r="2.7"/><circle class="viz-primary" cx="52" cy="19" r="2.7"/>',
    naive_bayes = '<path class="viz-axis" d="M8 57h54"/><path class="viz-secondary-line" d="M9 55c7 0 8-31 22-31s14 31 21 31"/><path class="viz-trend" d="M24 55c6 0 8-38 21-38s12 38 18 38"/><circle class="viz-secondary" cx="23" cy="41" r="2.5"/><circle class="viz-secondary" cx="31" cy="29" r="2.5"/><circle class="viz-primary" cx="43" cy="25" r="2.5"/><circle class="viz-primary" cx="51" cy="39" r="2.5"/>',
    elastic_net_regression = '<path class="viz-axis" d="M8 58h54M9 58V10"/><path class="viz-muted-line" d="M12 17c15 6 28 17 46 33M12 51c15-8 29-17 46-34M12 32c15 2 28 4 46 5"/><path class="viz-secondary-line" d="M12 22c15 6 27 14 46 25"/><path class="viz-trend" d="M12 46c16-7 29-15 46-26"/><circle class="viz-primary" cx="35" cy="34" r="3.2"/>',
    dbscan_clustering = '<circle class="viz-neighborhood" cx="21" cy="25" r="15"/><circle class="viz-neighborhood" cx="50" cy="43" r="14"/><circle class="viz-secondary" cx="15" cy="22" r="3"/><circle class="viz-secondary" cx="22" cy="17" r="3"/><circle class="viz-secondary" cx="28" cy="28" r="3"/><circle class="viz-secondary" cx="18" cy="33" r="3"/><circle class="viz-primary" cx="44" cy="39" r="3"/><circle class="viz-primary" cx="53" cy="36" r="3"/><circle class="viz-primary" cx="57" cy="47" r="3"/><circle class="viz-primary" cx="47" cy="51" r="3"/><path class="viz-noise" d="m47 12 6 6m0-6-6 6M10 49l6 6m0-6-6 6"/>',
    hdbscan_clustering = '<ellipse class="viz-neighborhood" cx="35" cy="35" rx="28" ry="24"/><ellipse class="viz-neighborhood" cx="22" cy="31" rx="12" ry="14"/><ellipse class="viz-neighborhood" cx="50" cy="40" rx="10" ry="12"/><circle class="viz-secondary" cx="18" cy="27" r="3"/><circle class="viz-secondary" cx="25" cy="35" r="3"/><circle class="viz-primary" cx="47" cy="37" r="3"/><circle class="viz-primary" cx="53" cy="44" r="3"/>',
    manova = '<ellipse class="viz-secondary-soft" cx="24" cy="35" rx="17" ry="10" transform="rotate(-25 24 35)"/><ellipse class="viz-primary-soft" cx="48" cy="30" rx="17" ry="11" transform="rotate(25 48 30)"/><circle class="viz-secondary" cx="24" cy="35" r="3"/><circle class="viz-primary" cx="48" cy="30" r="3"/><path class="viz-axis" d="M7 58h56M8 58V9"/>',
    pca = '<path class="viz-axis" d="M7 58h56M35 62V7"/><circle class="viz-secondary-soft" cx="19" cy="42" r="3"/><circle class="viz-secondary" cx="25" cy="36" r="3"/><circle class="viz-primary-soft" cx="46" cy="25" r="3"/><circle class="viz-primary" cx="53" cy="20" r="3"/><path class="viz-trend" d="M35 35 59 13M35 35 12 20"/>',
    kmeans_clustering = '<circle class="viz-secondary-soft" cx="17" cy="22" r="4"/><circle class="viz-secondary" cx="24" cy="30" r="4"/><circle class="viz-secondary" cx="13" cy="34" r="4"/><circle class="viz-primary-soft" cx="47" cy="38" r="4"/><circle class="viz-primary" cx="55" cy="47" r="4"/><circle class="viz-primary" cx="42" cy="50" r="4"/><path class="viz-centroid" d="M18 27h8M22 23v8M45 44h8M49 40v8"/>',
    hierarchical_clustering = '<path class="viz-muted-line" d="M11 56V42h12v14M17 42V31h18M29 56V47h12v9M35 47V31M35 31V19h18M47 56V39h12v17M53 39V19"/>',
    factor_analysis = '<circle class="viz-primary" cx="35" cy="34" r="10"/><path class="viz-muted-line" d="M27 27 12 15M27 41 12 53M43 27 58 15M43 41 58 53"/><rect class="viz-node" x="6" y="9" width="12" height="12" rx="2"/><rect class="viz-node" x="6" y="47" width="12" height="12" rx="2"/><rect class="viz-node" x="52" y="9" width="12" height="12" rx="2"/><rect class="viz-node" x="52" y="47" width="12" height="12" rx="2"/>',
    reliability = '<rect class="viz-secondary-soft" x="8" y="17" width="10" height="38" rx="2"/><rect class="viz-secondary" x="21" y="22" width="10" height="33" rx="2"/><rect class="viz-primary-soft" x="34" y="19" width="10" height="36" rx="2"/><rect class="viz-primary" x="47" y="24" width="10" height="31" rx="2"/><path class="viz-muted-line" d="M13 12h39"/>',
    cfa = '<circle class="viz-secondary" cx="21" cy="34" r="9"/><circle class="viz-primary" cx="49" cy="34" r="9"/><path class="viz-muted-line" d="M15 27 8 14M15 41 8 54M55 27 62 14M55 41 62 54M30 34h10"/><rect class="viz-node" x="3" y="9" width="10" height="10" rx="2"/><rect class="viz-node" x="3" y="49" width="10" height="10" rx="2"/><rect class="viz-node" x="57" y="9" width="10" height="10" rx="2"/><rect class="viz-node" x="57" y="49" width="10" height="10" rx="2"/>',
    sem = '<circle class="viz-secondary" cx="18" cy="34" r="8"/><circle class="viz-primary" cx="50" cy="20" r="8"/><circle class="viz-primary-soft" cx="50" cy="49" r="8"/><path class="viz-muted-line" d="M26 31 41 23M26 38 41 46M50 28v12"/><path class="viz-trend" d="m38 20 4 3-5 1M38 44l4 3-5 1M47 38l3 4 3-4"/>',
    hlm_linear = '<rect class="viz-group" x="7" y="9" width="25" height="50" rx="6"/><rect class="viz-group" x="38" y="9" width="25" height="50" rx="6"/><path class="viz-secondary-line" d="M11 49 28 24"/><path class="viz-trend" d="M42 43 59 18"/><circle class="viz-secondary" cx="14" cy="43" r="2.5"/><circle class="viz-secondary" cx="24" cy="31" r="2.5"/><circle class="viz-primary" cx="45" cy="39" r="2.5"/><circle class="viz-primary" cx="55" cy="25" r="2.5"/>',
    hlm_logistic = '<rect class="viz-group" x="7" y="9" width="25" height="50" rx="6"/><rect class="viz-group" x="38" y="9" width="25" height="50" rx="6"/><path class="viz-secondary-line" d="M10 48c6 0 6-3 10-15 3-11 5-14 10-14"/><path class="viz-trend" d="M41 52c6 0 6-4 10-18 3-12 5-15 10-15"/>',
    growth_model = '<path class="viz-muted-line" d="M9 51 26 42 43 29 60 18M9 47 26 39 43 34 60 30M9 55 26 47 43 39 60 28M9 42 26 34 43 23 60 13"/><path class="viz-trend" d="M9 49 26 40 43 31 60 22"/>',
    kaplan_meier = '<path class="viz-axis" d="M8 58h54M9 58V10"/><path class="viz-secondary-line" d="M10 15h13v8h12v9h12v10h13"/><path class="viz-trend" d="M10 15h9v5h13v6h14v8h14"/>',
    cox_regression = '<path class="viz-axis" d="M35 9v51"/><path class="viz-muted-line" d="M12 19h31M12 15v8M43 15v8M27 34h32M27 30v8M59 30v8M18 49h25M18 45v8M43 45v8"/><circle class="viz-secondary" cx="27" cy="19" r="4"/><circle class="viz-primary" cx="44" cy="34" r="4"/><circle class="viz-muted" cx="31" cy="49" r="4"/>',
    time_series = '<path class="viz-axis" d="M7 58h56"/><path class="viz-secondary-line" d="M8 44 15 31 22 38 29 20 36 32 43 16 50 28"/><path class="viz-trend" d="M50 28 57 19 63 24"/><path class="viz-muted-line" d="M50 11v46"/>'
  )
  drawing <- drawings[[id]] %||% drawings$descriptives

  div(
    class = paste("method-mini-visual", paste0("visual-", gsub("_", "-", id))), `aria-hidden` = "true",
    HTML(paste0('<svg viewBox="0 0 70 68" focusable="false" shape-rendering="geometricPrecision">', drawing, "</svg>"))
  )
}

# The directory groups by the reader's question rather than by statistical
# family, matching the selector, so moving between the two does not require
# relearning where anything is.
method_family_sections_ui <- function(families, kind) {
  lapply(names(families), function(group_name) {
    group_ids <- names(ANALYSES)[vapply(ANALYSES, function(x) {
      identical(x$family, group_name) && identical(x$kind, kind)
    }, logical(1))]
    tags$section(
      class = "method-family-section",
      div(class = "method-family-heading", h3(group_name),
          span(sprintf("%d %s", length(group_ids), if (length(group_ids) == 1) "method" else "methods"))),
      div(class = "method-name-list", lapply(group_ids, function(id) {
        x <- ANALYSES[[id]]
        tags$a(
          href = "#method-control", class = "method-name-choice", role = "button",
          `data-modal-method` = id,
          method_visual_ui(x),
          span(class = "method-name-choice-top", strong(x$name), span(class = paste("level-chip", tolower(x$level)), x$level)),
          span(class = "method-name-question", x$question),
          tags$small(class = "method-data-fit", ""),
          if (length(missing_packages(x))) {
            tags$small(class = "method-package-line", paste("Needs", paste(missing_packages(x), collapse = ", ")))
          }
        )
      }))
    )
  })
}

# The full catalog as a browsable list. It is reachable before any data is
# loaded, because deciding what is possible is often the reason someone opens
# a tool like this in the first place.
methods_directory_ui <- function() {
  div(
    class = "method-picker-shell",
    div(class = "method-picker-intro",
        div(class = "eyebrow", icon_svg("chart"), "Methods by research question"),
        h2("What do you want to learn from the data?"),
        p("Statistics describes patterns and quantifies uncertainty. Machine learning focuses on prediction, grouping, and dimension reduction. Choose the area that matches your question.")),
    div(class = "method-kind-switch", role = "tablist", `aria-label` = "Method type",
        tags$button(type = "button", class = "method-kind-button active", role = "tab", `aria-selected` = "true", `data-method-kind` = "statistics", "Statistics"),
        tags$button(type = "button", class = "method-kind-button", role = "tab", `aria-selected` = "false", `data-method-kind` = "machine_learning", "Machine learning")),
    div(class = "method-kind-panel active", `data-method-kind-panel` = "statistics",
        div(class = "method-kind-note", strong("Statistical methods"), span("Describe samples, compare groups, estimate relationships, and quantify uncertainty. Dimmed cards may need different column types, but they remain selectable.")),
        div(class = "method-directory-groups", method_family_sections_ui(ANALYSIS_FAMILIES, "statistics"))),
    div(class = "method-kind-panel", `data-method-kind-panel` = "machine_learning", `aria-hidden` = "true",
        div(class = "method-kind-note", strong("Machine-learning methods"), span("Check predictions on held-out rows or explore structure without treating the result as causal evidence. Dimmed cards may need different column types, but they remain selectable.")),
        div(class = "method-directory-groups", method_family_sections_ui(MACHINE_LEARNING_FAMILIES, "machine_learning")))
  )
}

# Lists the formats and, where a format has a quirk worth knowing, says what it
# is. SPSS user-missing codes are the one readers most often get caught by.
data_formats_ui <- function() {
  formats <- data.frame(
    Format = c("Delimited text", "Excel", "LibreOffice", "Statistical software", "R", "JSON", "Columnar"),
    Extensions = c(".csv, .tsv, .txt", ".xlsx, .xls", ".ods", ".sav, .zsav, .por, .dta, .sas7bdat, .xpt",
                   ".rds, .rdata, .rda", ".json", ".parquet, .feather"),
    Package = c("readr", "readxl", "readODS", "haven", "Base R", "jsonlite", "arrow")
  )
  div(
    class = "formats-shell",
    p("The first row should contain column names. Each later row should represent one observation unless the selected method calls for repeated rows."),
    table_ui(formats),
    h3("Before analysis"),
    tags$ul(tags$li("Use a consistent missing-value marker such as a blank cell or NA."),
            tags$li("Store categories as labels or consistent numeric codes."),
            tags$li("Include a stable ID column for repeated or clustered data."),
            tags$li("Store dates in an unambiguous form such as 2026-08-01.")),
    p(tags$a(href = "#imputation-panel", "Open the missing-data workspace"),
      " after a file with blank cells has loaded.")
  )
}

# The panel names the cost of each method beside its name. Filling missing
# values is offered because deleting rows is also a choice with consequences,
# and a reader should be able to see both.
imputation_ui <- function() {
  div(
    class = "imputation-shell",
    div(class = "imputation-lead",
        div(class = "eyebrow", icon_svg("data"), "Prepare a working copy"),
        h2("Handle missing values without changing the source file"),
        p("Daybreak keeps the file you loaded and makes a separate working copy. Choose columns and a method only after considering why values are absent.")),
    uiOutput("imputation_controls"),
    div(class = "imputation-actions",
        actionButton("run_imputation", "Create working copy", class = "model-primary-button"),
        actionButton("restore_original_data", "Restore loaded data", class = "model-secondary-button")),
    uiOutput("imputation_status"),
    uiOutput("imputation_download"),
    tags$details(class = "imputation-cautions",
      tags$summary("How the four choices differ"),
      tags$ul(
        tags$li(strong("Median and most common category"), " fills each column separately. It is quick, but it understates uncertainty and can weaken relationships between columns."),
        tags$li(strong("Nearest neighbors"), " borrows values from rows with similar observed columns. Results depend on scaling and the chosen number of neighbors."),
        tags$li(strong("Random forest"), " predicts missing entries from the other selected columns. It can model nonlinear patterns but may take longer."),
        tags$li(strong("MICE"), " creates several completed datasets. Daybreak displays the first as a working copy and preserves the full object for download; inferential models should be fitted and pooled across all completed datasets.")))
  )
}

# The local model panel states its limits before it offers the feature: no
# data leaves the machine, the model does not compute anything, and its output
# is checked against the source before it is shown.
local_models_ui <- function() {
  div(
    class = "models-shell",
    div(
      class = "models-lead",
      div(class = "eyebrow", icon_svg("spark"), "Optional and local"),
      h2("Let a local model restate the finished result"),
      p("Daybreak already writes the everyday explanation without a language model. If you choose this option, Ollama rewrites that completed explanation on your Mac. Raw rows are never sent to the model, and no internet service receives the result.")
    ),
    div(
      class = "model-setup-grid",
      tags$section(
        class = "model-setup-card",
        span(class = "model-step-number", "1"),
        div(h3("Install Ollama"),
            p("Download the macOS app, move it to Applications, and open it once."),
            tags$a(href = "https://ollama.com/download", target = "_blank", rel = "noopener",
                   class = "model-link", "Open the Ollama download page"))
      ),
      tags$section(
        class = "model-setup-card",
        span(class = "model-step-number", "2"),
        div(h3("Choose a model"),
            p("Select an installed model or download one of the three choices below. The recommended download is 3.4 GB."))
      ),
      tags$section(
        class = "model-setup-card",
        span(class = "model-step-number", "3"),
        div(h3("Test, then rewrite"),
            p("Test the selected model here. After an analysis, open Exact values and optional rewrite below the figure."))
      )
    ),
    div(
      class = "model-control-card",
      uiOutput("ollama_status"),
      selectInput("local_model", "Local model", choices = local_model_choices(), selected = "qwen3.5:4b"),
      div(class = "model-control-actions",
          actionButton("refresh_local_models", "Check Ollama", class = "model-secondary-button"),
          actionButton("download_local_model", "Download selected model", class = "model-primary-button"),
          actionButton("test_local_model", "Test selected model", class = "model-secondary-button")),
      uiOutput("model_download_status"),
      uiOutput("model_test_status")
    ),
    div(class = "model-choice-grid", lapply(names(LOCAL_MODEL_CATALOG), function(id) {
      model <- LOCAL_MODEL_CATALOG[[id]]
      div(class = "model-choice-card",
          div(class = "model-choice-top", strong(model$label), span(model$size)),
          p(model$note), tags$code(id))
    })),
    div(class = "model-safety-note", icon_svg("check"),
        p("Daybreak rejects a rewrite if any number is added, removed, or changed. The original explanation always remains visible. Models with cloud in their name are not offered."))
  )
}

# A session carries the data, the roles, the settings, and both readings, so
# reopening it reproduces the screen rather than only the numbers.
session_file_ui <- function() {
  div(
    class = "session-file-shell",
    div(
      class = "session-file-intro",
      div(class = "eyebrow", icon_svg("save"), "Continue later"),
      h2("Save this analysis workspace"),
      p("A Daybreak session is a binary RDS file containing the active data, selected method, column roles, settings, completed result, explanations, and display colors. It is not an HTML report, and it stays on your computer.")
    ),
    div(
      class = "session-file-grid",
      tags$section(
        class = "session-file-card",
        span(class = "session-file-number", "1"),
        h3("Save the current session"),
        p("Use this after loading data, whether or not an analysis has been run. The downloaded file includes the active data."),
        uiOutput("session_save_control")
      ),
      tags$section(
        class = "session-file-card",
        span(class = "session-file-number", "2"),
        h3("Open a prior session"),
        p("Choose a .daybreak.rds file made by this app. Daybreak checks the file before replacing the current workspace."),
        fileInput("session_file", "Saved session", accept = c(".rds", ".daybreak.rds"),
                  buttonLabel = "Choose session", placeholder = "No session selected"),
        actionButton("open_session", "Open this session", class = "model-primary-button"),
        uiOutput("session_open_message")
      )
    ),
    div(class = "session-privacy-note", icon_svg("data"),
        div(strong("The data is inside the file"),
            p("Treat a .daybreak.rds file like the source dataset. Do not send it to someone who should not see those rows.")))
  )
}

# The landing page has to answer one question before anything else: what do I
# do first. Three sample datasets with their methods already chosen answer it
# better than an explanation would.
# The landing page has to answer one question before anything else: what do I
# do first. Prose does not answer it. Three examples with their methods already
# chosen do, because pressing one produces a finished result immediately, and a
# reader who has seen one result knows what the rest of the screen is for.
LANDING_QUICK_STARTS <- c("iris", "cars", "lung")

landing_quick_start_ui <- function() {
  available <- available_samples()
  ids <- LANDING_QUICK_STARTS[LANDING_QUICK_STARTS %in% names(available)]
  if (!length(ids)) return(NULL)
  div(
    class = "quick-start",
    div(class = "quick-start-label", "Start with an example"),
    div(class = "quick-start-row", lapply(ids, function(id) {
      sample <- available[[id]]
      method <- ANALYSES[[sample$method]]
      tags$button(
        type = "button", class = "quick-start-card", `data-sample` = id,
        span(class = "quick-start-name", sample$name),
        span(class = "quick-start-method", method$name %||% sample$method),
        span(class = "quick-start-go", "Run it", icon_svg("arrow"))
      )
    })),
    p(class = "quick-start-note", "Each one opens with its columns already assigned. Change anything afterwards.")
  )
}

landing_page_ui <- function() {
  div(
    class = "welcome",
    div(
      class = "welcome-copy",
      div(class = "eyebrow", icon_svg("sun"), "Private, local, reproducible"),
      h1("From raw columns to a report you can read."),
      p("Choose a method and assign its columns on the left. Daybreak computes the statistics, describes the result with calibrated wording, checks what it can, and prepares publication-ready files."),
      landing_quick_start_ui(),
      div(
        class = "welcome-points",
        div(class = "welcome-point", icon_svg("check"), span("Every estimate stays beside its interval, assumptions, and limits.")),
        div(class = "welcome-point", icon_svg("chart"), span("Figures use color, shape, and line style so color never carries meaning alone.")),
        div(class = "welcome-point", icon_svg("download"), span("Reports, tables, figures, LaTeX, and reproducible R code are ready to download."))
      )
    ),
    div(
      class = "welcome-visual", `aria-hidden` = "true",
      div(class = "orbit one"), div(class = "orbit two"), div(class = "orbit three"),
      div(class = "float-card a",
          div(class = "float-card-label", "Estimate"), div(class = "float-card-value", "0.42"),
          div(class = "mini-interval", tags$i(), tags$b(), tags$i())),
      div(class = "float-card b",
          div(class = "float-card-label", "95% interval"), div(class = "float-card-value", "0.18 to 0.66"))
    )
  )
}

# Suggestions come from the column profile alone. They are framed as places to
# start rather than recommendations, since the app cannot know the design that
# produced the file.
suggestion_cards_ui <- function(data) {
  ids <- suggest_methods(data)
  if (!length(ids)) return(NULL)
  div(class = "suggestions",
      div(class = "suggestions-label", "Good starting points for these columns"),
      div(class = "suggestion-row", lapply(ids, function(id) {
        x <- ANALYSES[[id]]
        tags$button(type = "button", class = "suggestion-card", `data-method` = id,
                    span(class = "suggestion-name", x$name), span(class = "suggestion-question", x$question))
      })))
}

# Tables render from whatever columns the engine produced rather than from a
# fixed schema, so an engine can add a column without the interface needing to
# know about it.
table_ui <- function(data, empty = "No table was produced.") {
  if (is.null(data) || !nrow(data)) return(div(class = "empty-table", empty))
  data <- plain_table(data)
  div(class = "table-scroll", tags$table(class = "result-table",
      tags$thead(tags$tr(lapply(names(data), tags$th))),
      tags$tbody(lapply(seq_len(nrow(data)), function(i) tags$tr(lapply(data[i, ], function(x) tags$td(as.character(x))))))))
}

# Checks are cards rather than a table because their status is the part that
# has to be readable at a glance, and status in a table column is something a
# reader has to go looking for.
check_cards_ui <- function(checks) {
  if (is.null(checks) || !nrow(checks)) {
    return(empty_state_ui(
      "No automated checks for this method",
      "Some methods have no assumption that can be tested from the file alone. The reading above states what the result does and does not cover.",
      "check"
    ))
  }
  div(class = "check-grid", lapply(seq_len(nrow(checks)), function(i) {
    row <- checks[i, ]
    div(class = paste("check-card", paste0("status-", row$Status)),
        div(class = "check-top", span(class = "check-dot"), span(class = "check-name", row$Check)),
        div(class = "check-finding", row$Finding), p(row$Detail))
  }))
}

# The statistical description, shown after the everyday reading. Both are
# always present: this is not a mode the reader switches between, because the
# point is that the two accounts describe the same numbers.
narrative_ui <- function(narrative) {
  tags$section(class = "reading statistical-description", `aria-labelledby` = "statistical-description-title",
      div(class = "reading-lead", div(class = "eyebrow", icon_svg("chart"), "Statistical description"),
          h2(id = "statistical-description-title", narrative$title), p(narrative$headline)),
      div(class = "reading-grid", lapply(narrative$cards, function(card) {
        div(class = paste("reading-card", paste0("tone-", card$tone)), h3(card$title), p(card$body))
      })),
      if (length(narrative$warnings)) div(class = "warning-list", h3("Additional notes"), tags$ul(lapply(narrative$warnings, tags$li)))
  )
}

# The everyday explanation comes first in the reading order, before the
# statistical description and before the figure. A reader who wants the
# technical account can move past it in one scroll; a reader who needs the
# plain reading should not have to find it underneath something harder.
accessible_explanation_ui <- function(explanation) {
  tags$section(
    class = "everyday-reading", `aria-labelledby` = "everyday-explanation-title",
    div(
      class = "everyday-lead",
      div(class = "everyday-title-row",
          div(class = "eyebrow", icon_svg("sun"), "Everyday explanation"),
          span(class = "result-context-pill", "Specific to this result")),
      h2(id = "everyday-explanation-title", explanation$title),
      p(class = "everyday-question", explanation$question),
      p(class = "everyday-context", explanation$context),
      div(class = "everyday-answer", h3("What the data says"), p(explanation$bottom_line))
    ),
    div(class = "everyday-guide",
      div(class = "everyday-guide-heading", strong("Read it one step at a time"), span("Figure, chance, and limits")),
      div(
        class = "everyday-grid",
        div(class = "everyday-card picture-card", span(class = "everyday-card-icon", icon_svg("chart")),
            h3("How to read the figure"), p(explanation$picture)),
        div(class = "everyday-card certainty-card", span(class = "everyday-card-icon", icon_svg("spark")),
            h3("Could this be random?"), p(explanation$certainty)),
        div(class = "everyday-card boundary-card", span(class = "everyday-card-icon", icon_svg("help")),
            h3("What this result cannot tell us"), p(explanation$boundary))
      ),
      div(class = "everyday-data-note", icon_svg("data"), span(explanation$data_note))
    )
  )
}

local_rewrite_ui <- function() {
  tags$details(class = "local-rewrite-panel",
    tags$summary(div(strong("Optional local rewrite"), span("Restate the checked answer on this Mac"))),
    div(class = "local-rewrite-callout",
        div(h3("Want another wording?"),
            p("An optional model running on this Mac can restate the completed explanation. Daybreak does not give it raw rows or ask it to calculate statistics.")),
        div(class = "local-rewrite-actions",
            actionButton("restate_local", "Rewrite with a local model", class = "model-primary-button"),
            tags$a(href = "#models-panel", class = "model-text-link", "Set up local models"))),
    uiOutput("local_restatement"))
}

result_table_section_ui <- function(title, data, open = FALSE) {
  tags$details(
    class = "result-table-section", open = if (isTRUE(open) && !is.null(data) && nrow(data)) "open" else NULL,
    tags$summary(div(strong(title), span(if (is.null(data) || !nrow(data)) "No table" else sprintf("%s rows", nrow(data))))),
    div(class = "result-table-section-body", table_ui(data))
  )
}

# Methods that describe a file rather than test a claim. They get no verdict
# chip, because there is nothing for a verdict to be about, and stamping one on
# a summary of averages would invent a claim the reader never made.
DESCRIPTIVE_METHODS <- c(
  "descriptives", "frequencies", "missingness", "pca", "factor_analysis",
  "kmeans_clustering", "hierarchical_clustering", "density_clustering",
  "hdbscan_clustering", "reliability"
)

# Four states, matching the four the everyday layer already uses in prose. The
# chip is a second reading of the same judgment, not a new one, so it is
# computed from the same function rather than from a threshold applied here.
result_verdict <- function(result) {
  if (result$method %in% DESCRIPTIVE_METHODS) {
    return(list(tone = "summary", label = "Summary", detail = "This describes the file rather than testing a claim."))
  }
  null <- if (result$method %in% RATIO_METHODS) 1 else 0
  switch(
    plain_signal(result, null),
    clear = list(tone = "clear", label = "Clear signal",
                 detail = "A pattern this strong would be unusual if there were nothing beyond this sample."),
    uncertain = list(tone = "uncertain", label = "Not clear",
                     detail = "These data are consistent with no difference as well as with a real one."),
    list(tone = "unknown", label = "No evidence measure",
         detail = "This result does not carry a test or an interval to judge it by.")
  )
}

# The reader came for one sentence. It used to sit in the third card down, in
# body text, at the same size as the surrounding explanation. Here it leads,
# once, at a size that says it is the answer. Everything below it is support.
answer_hero_ui <- function(result, accessible) {
  verdict <- result_verdict(result)
  div(
    class = paste("answer-hero", paste0("verdict-", verdict$tone)),
    div(class = "answer-hero-top",
        span(class = "verdict-chip",
             span(class = "verdict-dot", `aria-hidden` = "true"),
             verdict$label),
        span(class = "answer-hero-method", ANALYSES[[result$method]]$name)),
    p(class = "answer-hero-line", accessible$bottom_line),
    p(class = "answer-hero-detail", verdict$detail)
  )
}

result_tabs_ui <- function(result, narrative, accessible, stale = FALSE) {
  omitted <- result$n_omitted %||% 0L
  div(class = "result-stack", `aria-live` = "polite",
      if (isTRUE(stale)) stale_result_banner(),
      answer_hero_ui(result, accessible),
      div(class = "result-overview-bar",
          div(class = "result-overview-badges",
              span(sprintf("%s rows used", format(result$n_used, big.mark = ","))),
              if (omitted > 0) span(class = "has-omissions", sprintf("%s omitted", format(omitted, big.mark = ","))) else span("No required rows omitted"),
              if (is.finite(result$conf_level %||% NA)) span(sprintf("%s%% intervals", conf_percent(result$conf_level))))),
      div(class = "result-reading-sequence",
          accessible_explanation_ui(accessible),
          narrative_ui(narrative),
          tags$section(class = "result-figure-section", `aria-labelledby` = "result-figure-title",
              div(class = "result-column-heading", h2(id = "result-figure-title", "Pattern in the data"),
                  p("Use the figure to see the pattern described above. Use the tables below for exact values.")),
              div(class = "plot-card", role = "region", `aria-label` = "Statistical result figure",
                  plotOutput("main_plot", height = "540px")))),
      div(class = "result-detail-heading",
          div(h2("More detail and files"), p("Open only the section you need."))),
      div(class = "result-tabs", role = "tablist", `aria-label` = "Result sections",
          tags$button(id = "result-tab-details", class = "result-tab active", type = "button", role = "tab",
                      `aria-selected` = "true", `aria-controls` = "result-panel-details", tabindex = "0",
                      `data-tab` = "details", "Exact values"),
          tags$button(id = "result-tab-checks", class = "result-tab", type = "button", role = "tab",
                      `aria-selected` = "false", `aria-controls` = "result-panel-checks", tabindex = "-1",
                      `data-tab` = "checks", "Checks"),
          tags$button(id = "result-tab-data", class = "result-tab", type = "button", role = "tab",
                      `aria-selected` = "false", `aria-controls` = "result-panel-data", tabindex = "-1",
                      `data-tab` = "data", "Data used"),
          tags$button(id = "result-tab-export", class = "result-tab", type = "button", role = "tab",
                      `aria-selected` = "false", `aria-controls` = "result-panel-export", tabindex = "-1",
                      `data-tab` = "export", "Download")),
      div(id = "result-panel-details", class = "result-panel active", role = "tabpanel", `aria-labelledby` = "result-tab-details", `aria-hidden` = "false",
          div(class = "result-details-shell",
              div(class = "result-table-sections",
                  if (is.null(result$estimates) && is.null(result$tests) && is.null(result$fit)) {
                    empty_state_ui("This method reports no numeric tables",
                                   "The reading above and the checks tab carry everything this method produces.",
                                   "table")
                  },
                  result_table_section_ui("Estimates", result$estimates),
                  result_table_section_ui("Tests", result$tests),
                  result_table_section_ui("Model fit", result$fit)),
              local_rewrite_ui())),
      div(id = "result-panel-checks", class = "result-panel", role = "tabpanel", `aria-labelledby` = "result-tab-checks", `aria-hidden` = "true", check_cards_ui(result$checks)),
      div(id = "result-panel-data", class = "result-panel", role = "tabpanel", `aria-labelledby` = "result-tab-data", `aria-hidden` = "true", uiOutput("data_used_panel")),
      div(id = "result-panel-export", class = "result-panel", role = "tabpanel", `aria-labelledby` = "result-tab-export", `aria-hidden` = "true", export_panel_ui())
  )
}

# The preview is capped rather than paged. A reader checking that their file
# loaded correctly needs the first screenful and the column types; a reader who
# needs all of it has better tools than this panel.
data_preview_ui <- function(data, data_name, limit = 500L) {
  shown <- min(nrow(data), limit)
  div(
    class = "data-preview-shell",
    div(
      class = "data-preview-summary",
      div(h3(data_name), p(sprintf("Showing %s of %s rows and all %s columns.",
                                  format(shown, big.mark = ","),
                                  format(nrow(data), big.mark = ","),
                                  format(ncol(data), big.mark = ",")))),
      downloadButton("download_current_data", "Download full data as CSV", class = "download-btn compact-download")
    ),
    if (nrow(data) > limit) {
      div(class = "data-preview-note",
          sprintf("The on-screen preview stops at %s rows to keep the app responsive. The download contains every row.",
                  format(limit, big.mark = ",")))
    },
    table_ui(utils::head(data, limit))
  )
}

# Help text stays in the app rather than pointing at a website, because the
# whole application is built to run with no network at all.
help_ui <- function() {
  div(
    class = "help-shell",
    div(
      class = "help-lead",
      div(class = "eyebrow", icon_svg("sun"), "A short guide"),
      h2("Use Daybreak without memorizing statistical menus"),
      p("The app is organized around the question you want to answer. It keeps an everyday explanation, exact statistical wording, checks, data rows, and downloads together.")
    ),
    tags$section(
      class = "help-section", h3("Start here"),
      div(class = "help-step-grid",
          div(class = "help-step", span("1"), h4("Choose data"), p("Open a built-in example or upload your own file.")),
          div(class = "help-step", span("2"), h4("Choose a method"), p("Use the sidebar list or open Methods to browse by research question.")),
          div(class = "help-step", span("3"), h4("Assign columns"), p("Use the short descriptions under each field to place columns in the correct roles.")),
          div(class = "help-step", span("4"), h4("Run and read"), p("Read the everyday answer, the statistical description, and the figure in that order. Open more detail only when you need it.")))
    ),
    div(
      class = "help-two-column",
      tags$section(class = "help-section", h3("Choosing a method"),
                   p("The sidebar lists every method directly. The Methods directory separates statistics from machine learning, then groups each area by the kind of question it answers."),
                   p("After data loads, choices that do not appear to match the available column types become dimmer and show a short reason. They remain selectable because a different column assignment or a prepared working copy may still make them appropriate."),
                   tags$ul(
                     tags$li(strong("Describe"), ": summarize values, categories, or missing data."),
                     tags$li(strong("Compare"), ": examine differences across groups or repeated measurements."),
                     tags$li(strong("Relationships and statistical models"), ": study associations or adjusted estimates and quantify uncertainty."),
                     tags$li(strong("Advanced statistical groups"), ": work with several outcomes, latent variables, clustered data, survival, or time series."),
                     tags$li(strong("Machine learning"), ": predict a category or number, find groups, or reduce dimensions. Predictive results use held-out rows and do not support causal claims."))),
      tags$section(class = "help-section", h3("Reading a result"),
                   p("Daybreak presents each result in one reading order. The everyday explanation comes first. The statistical description follows immediately, then the figure and optional detail."),
                   tags$ul(
                     tags$li(strong("Everyday explanation"), ": what the data says, how to read the figure, whether random variation could explain the pattern, and what the result cannot answer."),
                     tags$li(strong("Statistical description"), ": the formal account with statistical terms, estimates, uncertainty, checks, and limits."),
                     tags$li(strong("Pattern in the data"), ": the figure that shows the result described above it."),
                     tags$li(strong("Exact values"), ": estimates, intervals, tests, model fit, and the optional local rewrite."),
                     tags$li(strong("Checks"), ": conditions the app can examine from the selected columns."),
                     tags$li(strong("Data used"), ": column profile and the first rows included in the analysis.")))
    ),
    div(
      class = "help-three-column",
      tags$section(class = "help-section", h3("Preparing data"),
                   p("Use a rectangular table: columns are variables, rows are observations, and each cell contains one value. Put short, distinct column names in the first row. Do not use merged cells, decorative title rows, subtotals, footnotes, or more than one table on a sheet."),
                   p("For a one-time survey, one row usually represents one person. For repeated observations, use long form: the same ID appears on several rows, with separate columns for time and the measured value."),
                   table_ui(data.frame(
                     participant_id = c("P01", "P01", "P02", "P02"),
                     visit = c(1, 2, 1, 2),
                     score = c(18, 21, 15, 17),
                     group = c("A", "A", "B", "B")
                   )),
                   p("Blank cells or NA may mark missing values. Keep dates consistent, such as 2026-08-01, and use the same spelling for each category. Open Data formats for accepted file types or the missing-data workspace after loading a file.")),
      tags$section(class = "help-section", h3("Local language models"),
                   p("The everyday explanation works without a language model. The optional Ollama connection can restate that completed text on your Mac."),
                   p("Open Local models in the header for installation steps. Daybreak does not send raw rows to a model, and it rejects a rewrite that changes any number.")),
      tags$section(class = "help-section", h3("If an analysis stops"),
                   p("Read the message at the top of the result area. It usually names a missing column choice, an unsuitable variable type, too few complete rows, or an R package that must be installed."),
                   p("Changing a method clears the prior result so an old table is never presented as the result of a new analysis."))
    ),
    tags$section(
      class = "help-section",
      h3("Save your place"),
      p("Open Session in the header and download a .daybreak.rds file. It keeps the active data, method, column roles, settings, completed result, explanations, and display colors."),
      p("To continue later, open Session, choose that file, and select Open this session. The data is stored inside the file, so keep it wherever you would safely keep the source dataset.")
    ),
    tags$section(
      class = "help-section help-boundary",
      h3("What the checks do not decide"),
      p("Automated checks cannot judge whether a research design supports causal claims, whether a measure is substantively valid, whether a sample represents a population, or whether a model answers the intended research question. Those decisions still belong to the analyst.")
    )
  )
}

# Every download is built from the same result record, so a report, a script,
# and a session file always describe the same numbers. The script is the one
# that matters most: it is the claim that the result can be reproduced without
# this app, and it has to run as written.
export_panel_ui <- function() {
  div(class = "export-layout",
      div(class = "export-card", div(class = "export-icon", icon_svg("chart")), h3("Figure"),
          p("Download the current figure with the selected theme and color-vision palette."),
          selectInput("plot_format", "File type", c("PNG" = "png", "JPEG" = "jpg", "TIFF" = "tiff", "SVG" = "svg", "PDF" = "pdf", "EPS" = "eps", "Photoshop, flattened" = "psd", "LaTeX TikZ" = "tex")),
          selectInput("plot_quality", "Quality", stats::setNames(names(QUALITY_PRESETS), vapply(QUALITY_PRESETS, `[[`, character(1), "label"))),
          selectInput("plot_size", "Canvas", stats::setNames(names(SIZE_PRESETS), vapply(SIZE_PRESETS, `[[`, character(1), "label"))),
          downloadButton("download_plot", "Download figure", class = "download-btn"),
          p(class = "micro-note", "PSD files are flattened. TikZ needs tikzDevice and a working TeX installation.")),
      div(class = "export-card", div(class = "export-icon", icon_svg("data")), h3("Narrative report"),
          p("Download the reading, estimates, model fit, and checks."),
          selectInput("report_format", "File type", c("HTML" = "html", "Word" = "docx", "PDF" = "pdf", "LaTeX source" = "tex", "Markdown" = "md", "Plain text" = "txt")),
          downloadButton("download_report", "Download report", class = "download-btn"),
          p(class = "micro-note", "Word and HTML need rmarkdown. PDF also needs a TeX installation.")),
      # Marked as the primary card because it is not an equal choice among four.
      # The script is the claim that this result can be rebuilt without the
      # application, and it is the download most worth a reader noticing.
      div(class = "export-card is-primary", div(class = "export-icon", icon_svg("download")), h3("Complete bundle"),
          p("One ZIP with the Markdown report, print PNG, CSV tables, and the R script that reproduces these numbers without Daybreak."),
          downloadButton("download_bundle", "Download bundle", class = "download-btn"),
          downloadButton("download_code", "R script only", class = "text-download"))
  )
}
