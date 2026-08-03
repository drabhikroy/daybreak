# Color contrast ----------------------------------------------------------
#
# The palettes are the one part of the interface where a mistake is invisible
# to the person who made it. These tests compute WCAG 2.2 relative luminance
# directly rather than trusting a note in a comment, and they check every
# palette against every surface a mark can be drawn on in that theme.

relative_luminance <- function(hex) {
  hex <- gsub("^#", "", hex)
  channels <- strtoi(substring(hex, c(1, 3, 5), c(2, 4, 6)), 16L) / 255
  linear <- ifelse(channels <= 0.03928, channels / 12.92, ((channels + 0.055) / 1.055)^2.4)
  0.2126 * linear[1] + 0.7152 * linear[2] + 0.0722 * linear[3]
}

contrast_ratio <- function(a, b) {
  first <- relative_luminance(a)
  second <- relative_luminance(b)
  (max(first, second) + 0.05) / (min(first, second) + 0.05)
}

# Non-text contrast minimum from WCAG 2.2 success criterion 1.4.11.
NON_TEXT_MINIMUM <- 3

testthat::test_that("the contrast helper agrees with published reference values", {
  testthat::expect_equal(round(contrast_ratio("#FFFFFF", "#000000"), 2), 21)
  testthat::expect_equal(round(contrast_ratio("#777777", "#FFFFFF"), 2), 4.48)
})

testthat::test_that("dark mode chart colors clear the non-text minimum", {
  surfaces <- c("#111820", "#0D141B", "#17212B", "#1D2935")
  for (palette in names(DAYBREAK_PALETTES)) {
    for (color in DAYBREAK_PALETTES[[palette]]) {
      for (surface in surfaces) {
        testthat::expect_gte(contrast_ratio(color, surface), NON_TEXT_MINIMUM)
      }
    }
  }
})

testthat::test_that("light mode chart colors clear the non-text minimum", {
  surfaces <- c("#FFFDF8", "#F7F3EA", "#F2EDE3")
  for (palette in names(DAYBREAK_PALETTES_LIGHT)) {
    for (color in DAYBREAK_PALETTES_LIGHT[[palette]]) {
      for (surface in surfaces) {
        testthat::expect_gte(contrast_ratio(color, surface), NON_TEXT_MINIMUM)
      }
    }
  }
})

testthat::test_that("every palette is offered in both themes", {
  testthat::expect_setequal(names(DAYBREAK_PALETTES), names(DAYBREAK_PALETTES_LIGHT))
})

testthat::test_that("plot_palette returns the theme-appropriate set", {
  testthat::expect_identical(plot_palette("dark", "monochrome"), DAYBREAK_PALETTES$monochrome)
  testthat::expect_identical(plot_palette("light", "monochrome"), DAYBREAK_PALETTES_LIGHT$monochrome)
  # An unknown palette name falls back rather than returning NULL into a scale.
  testthat::expect_identical(plot_palette("dark", "nonsense"), DAYBREAK_PALETTES$standard)
})

testthat::test_that("interactive controls meet the 44 pixel target", {
  css <- readLines("www/app.css", warn = FALSE)
  interactive_rules <- c(
    ".summary-action.btn", ".palette-option", ".browse-method-button.btn",
    ".model-secondary-button.btn", ".result-tab", ".method-kind-button"
  )
  for (rule in interactive_rules) {
    opens <- grep(paste0("^", gsub("([.])", "\\\\.", rule), " \\{$"), css)
    testthat::expect_true(length(opens) >= 1L)
    for (start in opens) {
      closes <- which(grepl("^\\}", css)) 
      finish <- min(closes[closes > start])
      block <- css[start:finish]
      declared <- grep("min-height", block, value = TRUE)
      if (!length(declared)) next
      heights <- as.integer(sub(".*min-height: *([0-9]+)px.*", "\\1", declared))
      testthat::expect_true(all(heights >= 44))
    }
  }
})
