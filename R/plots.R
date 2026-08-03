# Plot system --------------------------------------------------------------

# Chart palettes, one set per theme ---------------------------------------
#
# A palette has to be read against the surface it is drawn on, so light mode
# needs its own values rather than the dark ones reused. The earlier release
# carried a single set tuned for the dark background, which meant that in light
# mode the standard gold sat at 1.6 to 1 against white and the monochrome ramp
# at 1.1 to 1. Every entry below was measured against the plot background for
# its own theme and clears the WCAG 2.2 non-text minimum of 3 to 1, with the
# weakest at 3.7 to 1.
#
# Monochrome stops at four steps on purpose. Past four, neighboring greys stop
# being tellable apart no matter how they are spaced, so the figures pair the
# ramp with shape and line style instead of adding a fifth near-duplicate.
DAYBREAK_PALETTES <- list(
  standard = c("#F6C453", "#59A5F5", "#F58B63", "#39C6B4", "#B89AF4", "#F06B91", "#9FCB55", "#94A3B8"),
  protanopia = c("#E6B800", "#3B8EDB", "#00A6A6", "#7A6FF0", "#D27BB5", "#8CB8E8", "#B9A66B", "#9AA7B4"),
  deuteranopia = c("#E6A700", "#2580C3", "#52B5E8", "#CC6A28", "#B56AA0", "#86A9CF", "#C2B173", "#9AA7B4"),
  tritanopia = c("#E86A78", "#2AA198", "#A06CD5", "#D95F9B", "#4C9F70", "#C45A3C", "#8FA9CE", "#A8AEB6"),
  monochrome = c("#F5F7FA", "#C9D0D9", "#9BA4B0", "#727C89")
)

DAYBREAK_PALETTES_LIGHT <- list(
  standard = c("#9C6500", "#1769AA", "#B84E27", "#087E75", "#6952A8", "#A83C62", "#5C6E1F", "#4A5561"),
  protanopia = c("#8C7100", "#17669F", "#007574", "#5146AD", "#8A4776", "#47759C", "#6E5F2A", "#4C5866"),
  deuteranopia = c("#8B6500", "#125D8E", "#28779C", "#8F4516", "#7C466E", "#416C91", "#6B5A22", "#4C5866"),
  tritanopia = c("#A13F4E", "#08746E", "#67429B", "#943664", "#28714A", "#8A371F", "#3C5A7D", "#4F5761"),
  monochrome = c("#111820", "#39424D", "#5A6470", "#7B8592")
)

plot_palette <- function(mode = "dark", palette = "standard") {
  set <- if (identical(mode, "light")) DAYBREAK_PALETTES_LIGHT else DAYBREAK_PALETTES
  set[[palette]] %||% set$standard
}

plot_style <- function(mode = "dark", palette = "standard") {
  dark <- !identical(mode, "light")
  list(
    dark = dark,
    palette = palette,
    colors = plot_palette(mode, palette),
    background = if (dark) "#111820" else "#FFFDF8",
    surface = if (dark) "#18222D" else "#FFFFFF",
    grid = if (dark) "#2B3948" else "#E7E0D3",
    text = if (dark) "#F7F4EC" else "#202A32",
    muted = if (dark) "#A9B5C1" else "#5B6670"
  )
}

# One theme for every figure. Sharing it means a reader who has learned to
# read one Daybreak chart has learned to read all of them, and it keeps the
# palette and contrast decisions in a single place rather than repeated across
# thirty plotting functions.
theme_daybreak <- function(style, base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = style$background, color = NA),
      panel.background = ggplot2::element_rect(fill = style$background, color = NA),
      legend.background = ggplot2::element_rect(fill = style$background, color = NA),
      legend.key = ggplot2::element_rect(fill = style$background, color = NA),
      text = ggplot2::element_text(color = style$text),
      axis.text.x = ggplot2::element_text(color = style$muted, margin = ggplot2::margin(t = 7)),
      axis.text.y = ggplot2::element_text(color = style$muted, margin = ggplot2::margin(r = 7)),
      axis.title.x = ggplot2::element_text(color = style$text, face = "bold", margin = ggplot2::margin(t = 16)),
      axis.title.y = ggplot2::element_text(color = style$text, face = "bold", margin = ggplot2::margin(r = 16)),
      plot.title = ggplot2::element_text(size = base_size * 1.35, face = "bold", margin = ggplot2::margin(b = 8)),
      plot.subtitle = ggplot2::element_text(color = style$muted, margin = ggplot2::margin(b = 14)),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = style$grid, linewidth = 0.35),
      strip.text = ggplot2::element_text(color = style$text, face = "bold"),
      strip.background = ggplot2::element_rect(fill = style$surface, color = NA),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(20, 22, 22, 24)
    )
}

# Palettes are recycled rather than interpolated when a variable has more
# levels than the palette has entries. Interpolating would generate colors
# nobody checked for contrast, which is precisely the failure the measured
# palettes above exist to prevent. Figures that expect many categories pair
# color with shape or line style so that a repeat is still tellable apart.
sun_scale <- function(style, aesthetic = c("color", "fill"), discrete = TRUE) {
  aesthetic <- match.arg(aesthetic)
  repeated_colors <- rep(style$colors, length.out = 64L)
  if (discrete && aesthetic == "color") return(ggplot2::scale_color_manual(values = repeated_colors))
  if (discrete && aesthetic == "fill") return(ggplot2::scale_fill_manual(values = repeated_colors))
  if (!discrete && aesthetic == "color") return(ggplot2::scale_color_gradient2(low = style$colors[2], mid = style$surface, high = style$colors[1], midpoint = 0))
  ggplot2::scale_fill_gradient2(low = style$colors[2], mid = style$surface, high = style$colors[1], midpoint = 0)
}

# Figures ------------------------------------------------------------------
#
# Dispatch by plot_kind rather than by method, because several methods share a
# figure. The everyday explanation describes whatever this returns, so any
# change to a figure needs the matching sentence in explanations.R changed with
# it. That pairing was broken once, and the ARIMA text spent a release
# describing a forecast band that was never drawn.
analysis_plot <- function(result, mode = "dark", palette = "standard") {
  style <- plot_style(mode, palette)
  kind <- result$plot_kind
  p <- switch(kind,
    distributions = plot_distributions(result, style),
    frequencies = plot_frequencies(result, style),
    missingness = plot_missingness(result, style),
    one_numeric = plot_one_numeric(result, style),
    group_numeric = plot_group_numeric(result, style),
    paired = plot_paired(result, style),
    repeated = plot_repeated(result, style),
    contingency = plot_contingency(result, style),
    proportion = plot_proportion(result, style),
    correlation = plot_correlation(result, style),
    partial = plot_partial(result, style),
    regression = plot_regression(result, style),
    moderation = plot_moderation(result, style),
    logistic = plot_logistic(result, style),
    count_model = plot_count_model(result, style),
    classification = plot_classification(result, style),
    ml_regression = plot_ml_regression(result, style),
    multivariate = plot_multivariate(result, style),
    pca = plot_pca(result, style),
    clusters = plot_clusters(result, style),
    loadings = plot_loadings(result, style),
    reliability = plot_reliability(result, style),
    lavaan = plot_lavaan(result, style),
    mediation = plot_lavaan(result, style),
    mixed = plot_regression(result, style),
    growth = plot_growth(result, style),
    survival = plot_survival(result, style),
    forest = plot_forest(result, style),
    time_series = plot_time_series(result, style),
    ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0, label = "No chart is available for this result.")
  )
  p + theme_daybreak(style)
}

# Density curves with the individual values marked underneath. The marks are
# what let a reader see a small sample for what it is, which a smooth curve on
# its own actively conceals.
plot_distributions <- function(result, style) {
  d <- result$plot_data
  vars <- result$extras$variables
  group <- result$extras$group
  long <- tidyr::pivot_longer(d, cols = dplyr::all_of(vars), names_to = "Variable", values_to = "Value")
  if (nzchar(group)) long$Group <- factor(long[[group]]) else long$Group <- "All rows"
  ggplot2::ggplot(long, ggplot2::aes(x = .data$Value, fill = .data$Group, color = .data$Group)) +
    ggplot2::geom_density(alpha = 0.22, linewidth = 0.9, na.rm = TRUE) +
    ggplot2::facet_wrap(~Variable, scales = "free") +
    sun_scale(style, "fill") + sun_scale(style, "color") +
    ggplot2::labs(title = "Distribution at a glance", subtitle = "Density curves show shape; the table carries the exact summaries.", x = NULL, y = "Density", fill = "Group", color = "Group")
}

plot_frequencies <- function(result, style) {
  d <- result$plot_data
  ggplot2::ggplot(d, ggplot2::aes(x = stats::reorder(.data$Variable, .data$Percent), y = .data$Percent, fill = .data$Split)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.82), width = 0.72) +
    ggplot2::coord_flip() + ggplot2::facet_wrap(~Measure, scales = "free_y") +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) + sun_scale(style, "fill") +
    ggplot2::labs(title = "How the categories are distributed", x = NULL, y = "Percent", fill = "Split")
}

# Sorted by how much is missing rather than by column order. The reader is
# looking for the worst column, and putting it first is the whole job of this
# figure.
plot_missingness <- function(result, style) {
  d <- result$plot_data
  ggplot2::ggplot(d, ggplot2::aes(x = stats::reorder(.data$Variable, .data$Percent), y = .data$Percent)) +
    ggplot2::geom_col(fill = style$colors[1], width = 0.68) + ggplot2::coord_flip() +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(.data$Percent, accuracy = 0.1)), hjust = -0.08, color = style$text, size = 3.5) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(), expand = ggplot2::expansion(mult = c(0, 0.16))) +
    ggplot2::labs(title = "Missing values by column", subtitle = "Longer bars mark columns with more missing data.", x = NULL, y = "Missing")
}

plot_one_numeric <- function(result, style) {
  outcome <- result$extras$outcome
  x <- result$plot_data[[outcome]]
  ggplot2::ggplot(data.frame(Value = x), ggplot2::aes(x = .data$Value)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 28, fill = style$colors[2], color = style$background, linewidth = 0.4) +
    ggplot2::geom_density(color = style$colors[1], linewidth = 1.05, na.rm = TRUE) +
    ggplot2::geom_vline(xintercept = mean(x), color = style$colors[3], linewidth = 0.9, linetype = "22") +
    ggplot2::labs(title = outcome, subtitle = "The dashed line marks the sample mean.", x = outcome, y = "Density")
}

# Individual rows are drawn under the box rather than replaced by it. A box
# plot alone hides sample size and hides a bimodal group entirely, and both are
# things a reader should be able to notice without asking for a second figure.
plot_group_numeric <- function(result, style) {
  outcome <- result$extras$outcome; group <- result$extras$group
  d <- result$plot_data; d[[group]] <- factor(d[[group]])
  ggplot2::ggplot(d, ggplot2::aes(x = .data[[group]], y = .data[[outcome]], fill = .data[[group]], color = .data[[group]])) +
    ggplot2::geom_violin(alpha = 0.17, linewidth = 0.7, trim = FALSE) +
    ggplot2::geom_boxplot(width = 0.18, outlier.shape = NA, alpha = 0.6, color = style$text) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.10, height = 0), alpha = 0.46, size = 1.7) +
    sun_scale(style, "fill") + sun_scale(style, "color") +
    ggplot2::guides(fill = "none", color = "none") +
    ggplot2::labs(title = paste(outcome, "by", group), subtitle = "Shape, box, and individual observations are shown together.", x = group, y = outcome)
}

plot_paired <- function(result, style) {
  before <- result$extras$before; after <- result$extras$after
  d <- result$plot_data; d$.case <- seq_len(nrow(d))
  long <- tidyr::pivot_longer(d, cols = dplyr::all_of(c(before, after)), names_to = "Measurement", values_to = "Value")
  long$Measurement <- factor(long$Measurement, levels = c(before, after))
  ggplot2::ggplot(long, ggplot2::aes(x = .data$Measurement, y = .data$Value, group = .data$.case)) +
    ggplot2::geom_line(color = style$muted, alpha = 0.28, linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(color = .data$Measurement, shape = .data$Measurement), size = 2.2, alpha = 0.8) +
    sun_scale(style, "color") + ggplot2::scale_shape_manual(values = c(16, 17)) +
    ggplot2::labs(title = "Paired measurements", subtitle = "Each line joins two values from the same row.", x = NULL, y = "Value", color = NULL, shape = NULL)
}

plot_repeated <- function(result, style) {
  d <- result$plot_data; outcome <- result$extras$outcome; within <- result$extras$within; id <- result$extras$id
  ggplot2::ggplot(d, ggplot2::aes(x = .data[[within]], y = .data[[outcome]], group = .data[[id]])) +
    ggplot2::geom_line(color = style$muted, alpha = 0.18) +
    ggplot2::stat_summary(ggplot2::aes(group = 1), fun = mean, geom = "line", color = style$colors[1], linewidth = 1.3) +
    ggplot2::stat_summary(ggplot2::aes(group = 1), fun = mean, geom = "point", color = style$colors[1], size = 3) +
    ggplot2::labs(title = "Repeated measurements", subtitle = "Faint lines show cases; the bright line joins condition means.", x = within, y = outcome)
}

plot_contingency <- function(result, style) {
  d <- result$plot_data
  names(d)[1:3] <- c("Row", "Column", "Count")
  ggplot2::ggplot(d, ggplot2::aes(x = .data$Column, y = .data$Row, fill = .data$Count)) +
    ggplot2::geom_tile(color = style$background, linewidth = 2) +
    ggplot2::geom_text(ggplot2::aes(label = .data$Count), color = style$text, fontface = "bold", size = 4) +
    ggplot2::scale_fill_gradient(low = style$surface, high = style$colors[1]) +
    ggplot2::labs(title = "Observed cell counts", x = result$extras$column %||% result$extras$after,
                  y = result$extras$row %||% result$extras$before, fill = "Count")
}

plot_proportion <- function(result, style) {
  if (result$method == "one_proportion") {
    d <- data.frame(Group = result$extras$event, Proportion = result$estimates$Proportion)
    reference <- result$extras$reference
  } else {
    d <- result$plot_data
    reference <- NA_real_
  }
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$Group, y = .data$Proportion, fill = .data$Group)) +
    ggplot2::geom_col(width = 0.62) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(.data$Proportion, accuracy = 0.1)),
                       vjust = -0.5, color = style$text, fontface = "bold") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(), limits = c(0, min(1, max(d$Proportion) * 1.18 + 0.04))) +
    sun_scale(style, "fill") + ggplot2::guides(fill = "none") +
    ggplot2::labs(title = paste(result$extras$event, "proportion"), x = NULL, y = "Proportion")
  if (is.finite(reference)) p <- p + ggplot2::geom_hline(yintercept = reference, color = style$colors[2], linetype = "22", linewidth = 0.9)
  p
}

# A diverging scale centered at zero, so the sign of a correlation is visible
# before the number is read. The midpoint is the surface color rather than
# white, so that zero recedes instead of standing out.
plot_correlation <- function(result, style) {
  mat <- result$plot_data
  d <- as.data.frame(as.table(mat)); names(d) <- c("Variable_1", "Variable_2", "Correlation")
  ggplot2::ggplot(d, ggplot2::aes(x = .data$Variable_1, y = .data$Variable_2, fill = .data$Correlation)) +
    ggplot2::geom_tile(color = style$background, linewidth = 1.2) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$Correlation)), color = style$text, size = 3.4) +
    sun_scale(style, "fill", discrete = FALSE) +
    ggplot2::coord_equal() + ggplot2::labs(title = paste(tools::toTitleCase(result$extras$cor_method), "correlations"), x = NULL, y = NULL, fill = "r") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 38, hjust = 1, margin = ggplot2::margin(t = 9)))
}

plot_partial <- function(result, style) {
  ggplot2::ggplot(result$plot_data, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point(color = style$colors[2], alpha = 0.62, size = 2) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = style$colors[1], fill = style$colors[1], alpha = 0.16) +
    ggplot2::labs(title = "Relationship after removing selected controls", x = paste("Residual", result$extras$x), y = paste("Residual", result$extras$y))
}

# Residuals against fitted values rather than the fitted line over the data.
# With more than one predictor the line cannot be drawn faithfully in two
# dimensions, and the residual plot is what actually shows whether the model
# form was reasonable.
plot_regression <- function(result, style) {
  d <- result$plot_data
  names(d)[1:2] <- c("Fitted", "Residual")
  ggplot2::ggplot(d, ggplot2::aes(x = .data$Fitted, y = .data$Residual)) +
    ggplot2::geom_hline(yintercept = 0, color = style$muted, linewidth = 0.7, linetype = "22") +
    ggplot2::geom_point(color = style$colors[2], alpha = 0.56, size = 1.9) +
    ggplot2::geom_smooth(se = FALSE, color = style$colors[1], linewidth = 1, method = "loess") +
    ggplot2::labs(title = "Residuals against fitted values", subtitle = "A flat, evenly spread cloud is the desired pattern.", x = "Fitted value", y = "Residual")
}

plot_moderation <- function(result, style) {
  d <- result$plot_data; focal <- result$extras$focal; moderator <- result$extras$moderator; outcome <- result$extras$outcome
  if (is.numeric(d[[moderator]])) {
    breaks <- stats::quantile(d[[moderator]], c(0, 0.33, 0.67, 1), na.rm = TRUE)
    breaks <- unique(breaks)
    d$.moderator_band <- cut(d[[moderator]], breaks = breaks, include.lowest = TRUE)
  } else d$.moderator_band <- factor(d[[moderator]])

  # The catalog lets the focal predictor be categorical, and a straight line
  # through category codes is not defined. A categorical focal gets group means
  # joined across the moderator bands instead, which shows the same thing an
  # interaction is about: whether the profile changes from band to band.
  focal_is_numeric <- is.numeric(d[[focal]])
  base <- ggplot2::ggplot(d, ggplot2::aes(
    x = if (focal_is_numeric) .data[[focal]] else factor(.data[[focal]]),
    y = .data[[outcome]], color = .data$.moderator_band, shape = .data$.moderator_band
  ))
  trend <- if (focal_is_numeric) {
    list(ggplot2::geom_point(alpha = 0.44, size = 1.8),
         ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 1))
  } else {
    list(ggplot2::geom_point(position = ggplot2::position_jitterdodge(jitter.width = 0.12, dodge.width = 0.5),
                             alpha = 0.30, size = 1.6),
         ggplot2::stat_summary(ggplot2::aes(group = .data$.moderator_band), fun = mean, geom = "line",
                               linewidth = 1, position = ggplot2::position_dodge(width = 0.5)),
         ggplot2::stat_summary(fun = mean, geom = "point", size = 3,
                               position = ggplot2::position_dodge(width = 0.5)))
  }
  base + trend + sun_scale(style, "color") +
    ggplot2::labs(
      title = paste(focal, "by", moderator),
      subtitle = if (focal_is_numeric) "Lines describe the fitted relationship within moderator groups." else "Points join the group means within each moderator band.",
      x = focal, y = outcome, color = moderator, shape = moderator
    )
}

plot_logistic <- function(result, style) {
  d <- result$plot_data
  d$Bin <- cut(d$Predicted, breaks = seq(0, 1, length.out = 11), include.lowest = TRUE)
  cal <- stats::aggregate(cbind(Observed, Predicted) ~ Bin, data = d, FUN = mean)
  ggplot2::ggplot(cal, ggplot2::aes(x = .data$Predicted, y = .data$Observed)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = style$muted, linetype = "22") +
    ggplot2::geom_line(color = style$colors[1], linewidth = 1) +
    ggplot2::geom_point(color = style$colors[2], fill = style$background, shape = 21, size = 3, stroke = 1.1) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(title = "Observed and predicted event rates", subtitle = "Points near the diagonal are well calibrated.", x = "Mean predicted probability", y = "Observed event rate")
}

plot_count_model <- function(result, style) {
  d <- result$plot_data
  ggplot2::ggplot(d, ggplot2::aes(x = .data$Fitted, y = .data$Observed)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = style$muted, linetype = "22") +
    ggplot2::geom_point(color = style$colors[2], alpha = 0.5, size = 2) +
    ggplot2::geom_smooth(color = style$colors[1], se = FALSE, method = "loess") +
    ggplot2::labs(title = "Observed and fitted counts", x = "Fitted count", y = "Observed count")
}

plot_classification <- function(result, style) {
  tab <- as.data.frame(table(Observed = result$plot_data$Observed, Predicted = result$plot_data$Predicted))
  ggplot2::ggplot(tab, ggplot2::aes(x = .data$Predicted, y = .data$Observed, fill = .data$Freq)) +
    ggplot2::geom_tile(color = style$background, linewidth = 2) +
    ggplot2::geom_text(ggplot2::aes(label = .data$Freq), color = style$text, fontface = "bold", size = 4) +
    ggplot2::scale_fill_gradient(low = style$surface, high = style$colors[1]) +
    ggplot2::labs(title = "Observed and predicted categories", x = "Predicted", y = "Observed", fill = "Rows")
}

plot_ml_regression <- function(result, style) {
  d <- result$plot_data
  limits <- range(c(d$Observed, d$Predicted), finite = TRUE)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$Predicted, y = .data$Observed)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = style$muted, linetype = "22") +
    ggplot2::geom_point(color = style$colors[2], fill = style$background, shape = 21,
                        alpha = 0.72, size = 2.6, stroke = 0.9) +
    ggplot2::coord_equal(xlim = limits, ylim = limits) +
    ggplot2::labs(title = "Observed and predicted values",
                  subtitle = "Points near the diagonal have smaller prediction errors on held-out rows.",
                  x = "Predicted", y = "Observed")
}

plot_multivariate <- function(result, style) {
  outcomes <- result$extras$outcomes
  d <- result$plot_data
  group <- result$extras$predictors[1]
  ggplot2::ggplot(d, ggplot2::aes(x = .data[[outcomes[1]]], y = .data[[outcomes[2]]], color = factor(.data[[group]]), shape = factor(.data[[group]]))) +
    ggplot2::geom_point(alpha = 0.65, size = 2.2) + sun_scale(style, "color") +
    ggplot2::labs(title = paste(outcomes[1], "and", outcomes[2]), subtitle = "The first two selected outcomes are shown.", color = group, shape = group)
}

plot_pca <- function(result, style) {
  d <- result$plot_data
  if (ncol(d) < 2) d$PC2 <- 0
  names(d)[1:2] <- c("PC1", "PC2")
  ggplot2::ggplot(d, ggplot2::aes(x = .data$PC1, y = .data$PC2)) +
    ggplot2::geom_hline(yintercept = 0, color = style$grid) + ggplot2::geom_vline(xintercept = 0, color = style$grid) +
    ggplot2::geom_point(color = style$colors[2], alpha = 0.58, size = 2) +
    ggplot2::labs(title = "Rows in the first two components", subtitle = "Nearby points have similar standardized profiles.", x = "Principal component 1", y = "Principal component 2")
}

# Cluster figures pair color with shape so the grouping survives a monochrome
# palette and a black and white printer.
plot_clusters <- function(result, style) {
  d <- result$plot_data
  cluster_levels <- levels(factor(d$Cluster))
  shapes <- rep(c(16, 17, 15, 18, 3, 7, 8, 4), length.out = length(cluster_levels))
  names(shapes) <- cluster_levels
  if ("Noise" %in% cluster_levels) shapes[["Noise"]] <- 4
  ggplot2::ggplot(d, ggplot2::aes(x = .data$Component_1, y = .data$Component_2,
                                  color = .data$Cluster, shape = .data$Cluster)) +
    ggplot2::geom_point(alpha = 0.72, size = 2.2) + sun_scale(style, "color") +
    ggplot2::scale_shape_manual(values = shapes) +
    ggplot2::labs(title = "Rows projected onto two components", subtitle = "Color and shape mark the fitted cluster. The grouping used all selected variables.",
                  x = "Projection 1", y = "Projection 2", color = "Cluster", shape = "Cluster")
}

plot_loadings <- function(result, style) {
  d <- result$plot_data
  long <- tidyr::pivot_longer(d, cols = -Variable, names_to = "Factor", values_to = "Loading")
  ggplot2::ggplot(long, ggplot2::aes(x = .data$Factor, y = .data$Variable, fill = .data$Loading)) +
    ggplot2::geom_tile(color = style$background, linewidth = 1) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$Loading)), color = style$text, size = 3.3) +
    sun_scale(style, "fill", discrete = FALSE) +
    ggplot2::labs(title = "Factor loadings", subtitle = "Larger absolute values mark stronger item-factor links.", x = NULL, y = NULL, fill = "Loading")
}

plot_reliability <- function(result, style) {
  d <- result$plot_data
  ggplot2::ggplot(d, ggplot2::aes(x = .data$Item_rest_correlation, y = stats::reorder(.data$Item, .data$Item_rest_correlation))) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$Item_rest_correlation, yend = .data$Item), color = style$grid, linewidth = 1) +
    ggplot2::geom_point(color = style$colors[1], size = 3) +
    ggplot2::labs(title = "Item-rest correlations", subtitle = "Higher values mean an item tracks the remainder of the scale more closely.", x = "Item-rest correlation", y = NULL)
}

plot_lavaan <- function(result, style) {
  d <- result$estimates
  value <- d$Estimate
  label <- paste(d$Left, d$Path, d$Right)
  draw <- data.frame(Path = label, Estimate = value, Lower = d$Lower, Upper = d$Upper)
  draw <- draw[is.finite(draw$Estimate), , drop = FALSE]
  draw <- head(draw[order(abs(draw$Estimate), decreasing = TRUE), ], 24)
  ggplot2::ggplot(draw, ggplot2::aes(x = .data$Estimate, y = stats::reorder(.data$Path, .data$Estimate))) +
    ggplot2::geom_vline(xintercept = 0, color = style$muted, linetype = "22") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$Lower, xmax = .data$Upper), color = style$colors[2], height = 0) +
    ggplot2::geom_point(color = style$colors[1], size = 2.8) +
    ggplot2::labs(title = "Selected model paths", subtitle = "Up to 24 unstandardized paths are ordered by absolute size.", x = "Estimate", y = NULL)
}

plot_growth <- function(result, style) {
  d <- result$plot_data; time <- result$extras$predictors[1]; outcome <- result$extras$outcome; cluster <- result$extras$cluster
  # The engine attaches the fitted values, so the bright line can be the model
  # that was actually estimated. The earlier version drew a loess smoother over
  # the raw points, which meant the figure showed a curve the reader had not
  # asked for and the fitted multilevel model appeared nowhere.
  ggplot2::ggplot(d, ggplot2::aes(x = .data[[time]], y = .data[[outcome]], group = .data[[cluster]])) +
    ggplot2::geom_line(color = style$muted, alpha = 0.20) +
    ggplot2::geom_point(color = style$muted, alpha = 0.28, size = 1.3) +
    ggplot2::stat_summary(ggplot2::aes(y = .data$.fitted, group = 1), fun = mean, geom = "line",
                          color = style$colors[1], linewidth = 1.3) +
    ggplot2::stat_summary(ggplot2::aes(y = .data$.fitted, group = 1), fun = mean, geom = "point",
                          color = style$colors[1], size = 2.6) +
    ggplot2::labs(title = paste(outcome, "over", time),
                  subtitle = "Faint lines follow individual units. The bright line joins the average value the model fits at each time point.",
                  x = time, y = outcome)
}

# Censoring marks are drawn on the curve. A survival curve without them looks
# like a curve fitted to complete follow-up, which is the single most common
# misreading of this figure.
plot_survival <- function(result, style) {
  d <- result$plot_data
  ggplot2::ggplot(d, ggplot2::aes(x = .data$Time, y = .data$Survival, color = .data$Stratum, linetype = .data$Stratum)) +
    ggplot2::geom_step(linewidth = 1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$Lower, ymax = .data$Upper, fill = .data$Stratum), alpha = 0.12, color = NA) +
    sun_scale(style, "color") + sun_scale(style, "fill") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    ggplot2::labs(title = "Event-free survival over time", x = result$extras$time, y = "Estimated survival", color = NULL, fill = NULL, linetype = NULL)
}

# Forest layout with the null marked. Placing the reference line first means a
# reader sees what the intervals are being compared against before they start
# comparing them.
plot_forest <- function(result, style) {
  d <- result$plot_data
  d <- d[d$Term != "(Intercept)", , drop = FALSE]
  interval_label <- sprintf("%s%% intervals", round(100 * (result$extras$conf_level %||% 0.95)))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$Estimate, y = stats::reorder(.data$Term, .data$Estimate))) +
    ggplot2::geom_vline(xintercept = 1, color = style$muted, linetype = "22") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$Lower, xmax = .data$Upper), color = style$colors[2], height = 0) +
    ggplot2::geom_point(color = style$colors[1], size = 2.8) +
    ggplot2::scale_x_log10() + ggplot2::labs(title = paste("Hazard ratios with", interval_label), x = "Hazard ratio, log scale", y = NULL)
}

plot_time_series <- function(result, style) {
  d <- result$plot_data
  observed <- d[d$Segment == "Observed", , drop = FALSE]
  forecast <- d[d$Segment == "Forecast", , drop = FALSE]
  has_forecast <- nrow(forecast) > 0L

  # The forecast band starts at the last fitted point so the ribbon and the
  # line meet instead of leaving a visible gap at the join.
  if (has_forecast && nrow(observed)) {
    bridge <- observed[nrow(observed), , drop = FALSE]
    bridge$Lower <- bridge$Fitted
    bridge$Upper <- bridge$Fitted
    forecast <- rbind(bridge, forecast)
  }

  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$Time))
  if (has_forecast) {
    p <- p +
      ggplot2::geom_ribbon(data = forecast,
                           ggplot2::aes(ymin = .data$Lower, ymax = .data$Upper),
                           fill = style$colors[1], alpha = 0.16) +
      ggplot2::geom_line(data = forecast,
                         ggplot2::aes(y = .data$Fitted, color = "Forecast", linetype = "Forecast"),
                         linewidth = 1)
  }
  p <- p +
    ggplot2::geom_line(data = observed, ggplot2::aes(y = .data$Observed, color = "Observed", linetype = "Observed"), linewidth = 0.8) +
    ggplot2::geom_line(data = observed, ggplot2::aes(y = .data$Fitted, color = "Fitted", linetype = "Fitted"), linewidth = 1) +
    ggplot2::scale_color_manual(values = c(Observed = style$colors[2], Fitted = style$colors[1], Forecast = style$colors[1])) +
    ggplot2::scale_linetype_manual(values = c(Observed = "solid", Fitted = "solid", Forecast = "22")) +
    ggplot2::labs(
      title = if (has_forecast) "Observed series, fitted values, and forecast" else "Observed series and fitted ARIMA values",
      subtitle = if (has_forecast) sprintf("The shaded band is the %s%% forecast interval and widens with distance ahead.", conf_percent(result$conf_level)) else NULL,
      x = result$extras$time, y = result$extras$outcome, color = NULL, linetype = NULL
    )
  p
}
