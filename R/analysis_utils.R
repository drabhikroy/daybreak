# Shared analysis helpers --------------------------------------------------

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "NA", formatC(x, digits = digits, format = "f", big.mark = ","))
}

fmt_p <- function(p) {
  if (is.na(p)) return("not available")
  if (p < 0.001) return("< .001")
  sub("^0", "", sprintf("%.3f", p))
}

fmt_pct <- function(x, digits = 1) paste0(fmt_num(100 * x, digits), "%")

bt <- function(x) paste0("`", gsub("`", "\\`", x, fixed = TRUE), "`")

formula_from <- function(outcome, predictors, interaction = NULL, offset = NULL) {
  rhs <- if (length(predictors)) vapply(predictors, bt, character(1)) else "1"
  if (!is.null(interaction)) {
    rhs <- c(rhs, paste(vapply(interaction, bt, character(1)), collapse = " * "))
  }
  if (!is.null(offset)) rhs <- c(rhs, sprintf("offset(log(%s))", bt(offset)))
  stats::as.formula(paste(bt(outcome), "~", paste(unique(rhs), collapse = " + ")))
}

formula_string <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")

# Complete-case handling is done once, here, so that every engine reports the
# same two numbers: how many rows were used and how many were set aside. Doing
# it per engine meant those counts drifted depending on which function dropped
# missing values first.
complete_frame <- function(data, columns) {
  columns <- unique(columns[nzchar(columns)])
  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns)) {
    stop(sprintf("These columns were not found: %s", paste(missing_columns, collapse = ", ")))
  }
  frame <- data[, columns, drop = FALSE]
  keep <- stats::complete.cases(frame)
  list(data = droplevels(frame[keep, , drop = FALSE]), used = sum(keep), omitted = sum(!keep))
}

require_columns <- function(data, columns) {
  columns <- unique(as.character(columns))
  columns <- columns[!is.na(columns) & nzchar(columns)]
  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns)) {
    stop(sprintf("These columns were not found: %s", paste(missing_columns, collapse = ", ")))
  }
  invisible(columns)
}

need_n <- function(n, minimum, label = "complete rows") {
  if (n < minimum) stop(sprintf("This analysis needs at least %d %s. The current choices provide %d.", minimum, label, n))
}

need_numeric <- function(data, columns) {
  bad <- columns[!vapply(data[columns], is.numeric, logical(1))]
  if (length(bad)) stop(sprintf("Choose numeric columns for: %s.", paste(bad, collapse = ", ")))
  nonfinite <- columns[vapply(data[columns], function(x) any(!is.na(x) & !is.finite(x)), logical(1))]
  if (length(nonfinite)) stop(sprintf("Replace infinite values in: %s.", paste(nonfinite, collapse = ", ")))
}

need_variation <- function(data, columns, label = "analysis") {
  bad <- columns[vapply(data[columns], function(x) {
    spread <- stats::sd(x)
    !is.finite(spread) || spread <= sqrt(.Machine$double.eps)
  }, logical(1))]
  if (length(bad)) stop(sprintf("%s needs measurable variation in: %s.", label, paste(bad, collapse = ", ")))
  invisible(columns)
}

need_levels <- function(x, minimum = 2, maximum = Inf, label = "group") {
  count <- length(unique(stats::na.omit(x)))
  if (count < minimum || count > maximum) {
    upper <- if (is.finite(maximum)) paste0(" and no more than ", maximum) else ""
    stop(sprintf("%s needs at least %d%s observed categories. The selected column has %d.", label, minimum, upper, count))
  }
}

need_group_sizes <- function(x, minimum = 2L, label = "Each group") {
  counts <- table(droplevels(factor(x)))
  if (!length(counts) || any(counts < minimum)) {
    detail <- if (length(counts)) paste(names(counts), as.integer(counts), collapse = "; ") else "no observed groups"
    stop(sprintf("%s needs at least %d complete rows. Current counts: %s.", label, minimum, detail))
  }
  invisible(counts)
}

need_repeated_cells <- function(data, id, condition) {
  need_levels(data[[id]], 2, label = "The case ID column")
  need_levels(data[[condition]], 2, label = "The repeated-condition column")
  counts <- table(droplevels(factor(data[[id]])), droplevels(factor(data[[condition]])))
  if (any(counts == 0L)) {
    stop("Every retained case must have one row in every repeated condition. At least one case-condition row is missing.")
  }
  if (any(counts > 1L)) {
    stop("Repeated analyses need one row per case and condition. At least one case-condition pair appears more than once.")
  }
  invisible(counts)
}

confidence_level <- function(options, default = 0.95) {
  value <- as.numeric(options$conf_level %||% default)
  if (length(value) != 1L || !is.finite(value) || value <= 0 || value >= 1) {
    stop("The confidence level must be greater than 0 and smaller than 1.")
  }
  value
}

# Builds the model matrix before fitting so that a rank deficiency or an empty
# design is caught here, where the message can name the problem, rather than
# inside a modelling function where it cannot.
need_model_matrix <- function(formula, data, minimum_residual_df = 1L) {
  matrix <- stats::model.matrix(formula, data = data)
  rank <- qr(matrix)$rank
  if (rank < ncol(matrix)) {
    stop("The selected predictors contain redundant columns or empty categories. Remove one overlapping predictor or category and run the model again.")
  }
  residual_df <- nrow(matrix) - rank
  if (residual_df < minimum_residual_df) {
    stop(sprintf("This model needs at least %d residual degree%s of freedom. Remove predictors or add complete rows.",
                 minimum_residual_df, if (minimum_residual_df == 1L) "" else "s"))
  }
  invisible(list(rank = rank, residual_df = residual_df, columns = ncol(matrix)))
}

with_local_seed <- function(seed, code) {
  seed <- as.integer(seed)
  if (length(seed) != 1L || is.na(seed) || seed < 0L) stop("The random seed must be a nonnegative whole number.")
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) previous_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  force(code)
}

as_group <- function(x) droplevels(factor(x, exclude = NULL))

binary01 <- function(x) {
  values <- unique(stats::na.omit(x))
  if (length(values) != 2) stop("The outcome must have exactly two observed values.")
  f <- droplevels(factor(x))
  list(value = as.integer(f) - 1L, levels = levels(f), event = levels(f)[[2]])
}

# Shapiro-Wilk is reported as a description rather than a gate. On a large
# sample it rejects for departures too small to matter, and on a small one it
# misses departures that do matter, so the check states what it found and
# leaves the decision with the reader.
normality_check <- function(x, label = "Outcome") {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(check_row("Normality", "Not checked", "Fewer than three complete values.", "note"))
  spread <- stats::sd(x)
  if (!is.finite(spread) || spread <= sqrt(.Machine$double.eps)) {
    return(check_row("Normality", "Not checked", sprintf("%s has no measurable variation.", label), "warn"))
  }
  if (length(x) > 5000) {
    skew <- mean((x - mean(x))^3) / spread^3
    status <- if (abs(skew) < 1) "No large skew found" else "Skew may matter"
    return(check_row("Distribution shape", status,
                     sprintf("Skewness was %s. With large samples, a shape summary is more useful than a Shapiro test.", fmt_num(skew, 2)),
                     if (abs(skew) < 1) "ok" else "warn"))
  }
  test <- stats::shapiro.test(x)
  check_row("Normality", if (test$p.value >= 0.05) "No clear departure found" else "Departure detected",
            sprintf("Shapiro-Wilk W = %s, p %s. Read this with the distribution plot and sample size.",
                    fmt_num(unname(test$statistic), 3), fmt_p(test$p.value)),
            if (test$p.value >= 0.05) "ok" else "warn")
}

check_row <- function(check, finding, detail, status = c("ok", "warn", "note")) {
  data.frame(Check = check, Finding = finding, Detail = detail,
             Status = match.arg(status), stringsAsFactors = FALSE)
}

# The exported R script ends with confint(fit), so the displayed interval has
# to be the one that call produces. For a linear model that means t quantiles
# and the residual degrees of freedom. Using confint.default everywhere gave
# normal-based bounds that were visibly narrower on small samples than the
# script the reader was told to run. Generalized models keep the Wald interval,
# which is what confint.default gives and what the export writes for them.
safe_confint <- function(model, level = 0.95) {
  linear_only <- inherits(model, "lm") && !inherits(model, "glm")
  tryCatch(
    as.data.frame(if (linear_only) stats::confint(model, level = level) else stats::confint.default(model, level = level)),
    error = function(e) NULL
  )
}

tidy_coefs <- function(model, exponentiate = FALSE, level = 0.95) {
  tab <- as.data.frame(summary(model)$coefficients)
  if (ncol(tab) < 4) {
    names(tab)[seq_len(min(3, ncol(tab)))] <- c("Estimate", "Std. error", "Statistic")[seq_len(min(3, ncol(tab)))]
    tab$p <- NA_real_
  } else {
    names(tab)[1:4] <- c("Estimate", "Std. error", "Statistic", "p")
  }
  ci <- safe_confint(model, level = level)
  if (!is.null(ci) && nrow(ci) == nrow(tab)) {
    tab$Lower <- ci[[1]]
    tab$Upper <- ci[[2]]
  } else {
    critical <- stats::qnorm(1 - (1 - level) / 2)
    tab$Lower <- tab$Estimate - critical * tab$`Std. error`
    tab$Upper <- tab$Estimate + critical * tab$`Std. error`
  }
  if (exponentiate) {
    tab$Estimate <- exp(tab$Estimate)
    tab$Lower <- exp(tab$Lower)
    tab$Upper <- exp(tab$Upper)
  }
  tab$Term <- rownames(tab)
  rownames(tab) <- NULL
  tab[, c("Term", "Estimate", "Std. error", "Statistic", "p", "Lower", "Upper"), drop = FALSE]
}

# Every engine returns this shape. conf_level travels with the record because
# the reporting and everyday layers have to name the interval they are reading.
# Before it was carried here, both layers printed "95% confidence interval"
# over bounds that had been computed at whatever level the form was set to.
result_record <- function(method, n_used, n_omitted = 0, estimates = NULL,
                          tests = NULL, fit = NULL, checks = NULL, model = NULL,
                          plot_kind = method, plot_data = NULL, extras = list(),
                          code = character(0), warnings = character(0),
                          conf_level = NA_real_) {
  list(
    method = method,
    n_used = n_used,
    n_omitted = n_omitted,
    estimates = estimates,
    tests = tests,
    fit = fit,
    checks = checks,
    model = model,
    plot_kind = plot_kind,
    plot_data = plot_data,
    extras = extras,
    code = code,
    warnings = warnings,
    conf_level = conf_level,
    created = Sys.time()
  )
}

# "95", "90", and so on. Used wherever prose or an axis label names the
# interval width. Trailing zeros are dropped so that 0.925 reads as 92.5.
conf_percent <- function(level) {
  level <- suppressWarnings(as.numeric(level))
  if (length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) level <- 0.95
  formatC(round(100 * level, 1), format = "fg", drop0trailing = TRUE)
}

interval_width <- function(result) conf_percent(result$conf_level %||% 0.95)

# Where an interval is available it decides the reading, and the p-value is
# only consulted when there is none. An interval that excludes the null and a
# p-value below the threshold are the same statement, but the interval also
# says how large the association might be, so it is the one that leads.
p_state <- function(p, estimate = NA_real_, lower = NA_real_, upper = NA_real_, null = 0) {
  if (!is.na(lower) && !is.na(upper)) {
    if (lower > null || upper < null) return("clear")
    return("uncertain")
  }
  if (is.na(p)) return("unknown")
  if (p < 0.05) "clear" else "uncertain"
}

hedges_g <- function(x, y) {
  nx <- length(x); ny <- length(y)
  pooled <- sqrt(((nx - 1) * stats::var(x) + (ny - 1) * stats::var(y)) / (nx + ny - 2))
  if (!is.finite(pooled) || pooled <= sqrt(.Machine$double.eps)) return(NA_real_)
  d <- (mean(x) - mean(y)) / pooled
  correction <- 1 - 3 / (4 * (nx + ny) - 9)
  d * correction
}

cohens_dz <- function(before, after) {
  change <- after - before
  spread <- stats::sd(change)
  if (!is.finite(spread) || spread <= sqrt(.Machine$double.eps)) return(NA_real_)
  mean(change) / spread
}

hodges_lehmann_two_sample <- function(x, y) {
  stats::median(as.vector(outer(x, y, "-")))
}

hodges_lehmann_paired <- function(difference) {
  pairs <- outer(difference, difference, "+") / 2
  stats::median(pairs[upper.tri(pairs, diag = TRUE)])
}

cramers_v <- function(tab, chi_square) {
  n <- sum(tab)
  dims <- dim(tab)
  sqrt(as.numeric(chi_square) / (n * min(dims - 1)))
}

# Valid for the two row one-way table only, where row 1 is the group term and
# row 2 is the residual. Do not reuse this for a factorial fit; that path
# reports partial eta squared per term instead.
omega_squared <- function(aov_table) {
  ss_between <- aov_table[["Sum Sq"]][1]
  df_between <- aov_table[["Df"]][1]
  ms_within <- aov_table[["Mean Sq"]][2]
  ss_total <- sum(aov_table[["Sum Sq"]], na.rm = TRUE)
  max(0, (ss_between - df_between * ms_within) / (ss_total + ms_within))
}

# Coefficient alpha compares the sum of item variances with the variance of the
# total score, so both have to come from the same rows. Taking item variances
# with na.rm while the total score silently dropped incomplete rows mixed two
# different denominators and could push alpha outside its usual range.
alpha_manual <- function(items) {
  items <- as.data.frame(items)
  k <- ncol(items)
  if (k < 2) stop("Reliability needs at least two items.")
  items <- items[stats::complete.cases(items), , drop = FALSE]
  if (nrow(items) < 2) stop("Reliability needs at least two complete rows.")
  item_var <- sum(vapply(items, stats::var, numeric(1)))
  total_var <- stats::var(rowSums(items))
  if (!is.finite(total_var) || total_var <= sqrt(.Machine$double.eps)) {
    stop("The total item score has no variation, so coefficient alpha cannot be calculated.")
  }
  k / (k - 1) * (1 - item_var / total_var)
}

plain_table <- function(x, digits = 3) {
  if (is.null(x)) return(NULL)
  out <- x
  for (name in names(out)) {
    col <- out[[name]]
    if (is.numeric(col) && grepl("(^p$|pvalue|p_value|adjusted_p)", tolower(name))) {
      out[[name]] <- vapply(col, fmt_p, character(1))
    } else if (is.numeric(col)) {
      out[[name]] <- round(col, digits)
    }
  }
  out
}
