# Code review, release 0.10.0

Reviewed 2026-08-02 against the tree that became 0.10.0. This file records what
was actually executed and what was not. The previous version of this document
reported a pass on several areas that had no test coverage and on at least five
assertions that were failing at the time it was written, so the standard here is
that nothing is described as checked unless it was run.

## What was executed

| Area | What was run | Result |
| --- | --- | --- |
| Parsing | Every file in `R/` plus `app.R` through `parse()` | Passed |
| Engines | 38 methods run end to end against the built-in samples, each through both writing layers | Passed |
| Downloads | Text, Markdown, and LaTeX reports; session write and read; reproduction script parsed as R; result tables | Passed |
| Reproduction script | Generated script parsed, and the linear model interval compared against `confint(fit)` | Matched to four decimal places |
| Confidence level | Interval bounds and prose compared at 0.80, 0.95, and 0.99 | Passed |
| Coefficient selection | Statistical and everyday paragraphs compared on a logistic model with a large ratio | Both name the same term |
| Contrast | WCAG relative luminance computed for all five palettes against four dark surfaces and three light surfaces | Passed, weakest 3.68 to 1 |
| Touch targets | Every `min-height` in the stylesheet, with interactive selectors checked against 44 pixels | Passed |
| Writing rules | Banned words, long dashes, and contractions across `R/`, `app.R`, `www/`, `docs/`, `tests/`, and the report template | Passed |
| Forecasting | ARIMA horizon, interval width, and calendar month continuation on the `nottem` sample | Passed |
| Effect sizes | Hedges' g, Cohen's dz, omega squared, Cramer's V, and both Hodges-Lehmann estimators against their defining formulas | Passed |
| Engine output | 20 methods compared against direct base R, MASS, nnet, and survival calls | Passed |
| Moderation | Coefficients compared against a hand-centered fit, and the product term against the uncentered one | Passed |
| Seed discipline | Every randomized engine checked for reproducibility and for leaving the caller's random stream alone | Passed |
| Optional roles | All 10 methods with an optional role, run with that role empty | Passed |
| Local model guard | Five rejection paths, including the output that prompted this review | Passed |
| Stylesheet | Every class selector cross-checked against the markup the application emits | Passed |

## What was not executed

The review environment had R 4.3.3 with base and recommended packages only. The
following were read rather than run, and need one pass on a machine with the
full dependency set before the release is cut.

- Anything requiring `lavaan`, `lme4`, `lmerTest`, `glmnet`, `ranger`, `dbscan`,
  `e1071`, `psych`, or `mice`. That is 15 of the 53 methods.
- HTML, Word, and PDF rendering, which need `rmarkdown` and a LaTeX toolchain.
- Figure export in every format, which needs `ggplot2`, `svglite`, `magick`, and
  `tikzDevice`.
- The Shiny interface itself. All server logic, reactive behavior, and browser
  interaction were reviewed by reading.
- The Ollama integration, which needs a running local service.

## Test suite

`R/exports.R` had no test file before this release. `tests/testthat/helper-daybreak.R`
defined `DAYBREAK_VERSION` itself, which meant the session and export tests
passed against a constant the running application never supplied. The helper now
defines nothing and sources the same files `app.R` sources.

Five assertions were failing before this release and have been corrected:

- `test-accessibility.R` expected `DAYBREAK_VERSION <- "0.9.2"` in `app.R`, which
  contained `app_version <- "0.9.2"` instead.
- `test-accessibility.R` looked for the local model button in `app.R`, where it
  has never been. It is built in `R/ui_helpers.R`.
- `test-writing.R` failed on a contraction in `R/local_models.R`, which was part
  of a pattern matching Ollama's own error text. The pattern is now assembled
  from pieces so the rule holds without changing what it matches.
- `test-reporting.R` failed because the everyday explanation could not handle a
  result record with no extras. That path is reachable in normal use when a
  session written by an earlier release is reopened.
- `test-catalog.R` expected the words "category outcome" in a message that read
  "categorical outcome". The message was also not a grammatical sentence and has
  been rewritten.

Two test files were added. `test-exports.R` covers the six download paths.
`test-contrast.R` computes contrast ratios directly rather than trusting a note
in a comment, and reads the stylesheet for touch target sizes.

## Known limits

- The monochrome palette carries four steps. Past four, neighboring greys stop
  being tellable apart at any spacing that also clears the contrast minimum, so
  figures with more than four categories pair the ramp with shape or line style.
- A palette with more categories than entries recycles rather than interpolating.
  Interpolating would generate colors that were never measured.
- MICE returns the first completed dataset. A pooled analysis needs the model
  fitted once per imputation, which is outside what this screen does. The `mids`
  object is offered as a download.
- Repeated-measures ANOVA does not apply a sphericity correction for three or
  more conditions. The check table says so and points at a mixed model.
- Comment density is 10.6 percent, short of the 15 percent used across the other
  applications in this series.
- The local model check verifies structure and numbers. It cannot verify that a
  restatement is faithful in meaning, and no automated check could. The
  deterministic text remains the one on screen; the rewrite sits beside it.
