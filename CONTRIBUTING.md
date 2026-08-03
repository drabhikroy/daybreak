# Contributing

Daybreak is a personal project, but issues and pull requests are welcome. The
notes below describe the conventions the codebase follows, so that a change
arrives looking like the rest of it.

## Before opening a pull request

Run the suite:

```r
source("tests/run_tests.R")
```

It stops on the first failure. If a test fails for a reason unrelated to your
change, say so in the pull request rather than adjusting the assertion.

## Writing

Reader-facing text and source comments follow the same rules, and
`tests/testthat/test-writing.R` enforces them.

- No contractions.
- No em dashes or en dashes.
- A banned word list, including all inflections. Read the test for the current
  list.
- Comments explain why a choice was made, not what the line does.

If a pattern has to match text containing a contraction, as the Ollama error
matcher does, assemble the pattern from pieces so the rule still holds.

## Statistics

- An engine returns the shape defined by `result_record()`. Nothing else.
- A result that cannot be trusted stops with a sentence written for the person
  who chose the columns. It does not return a number with a warning attached.
- A method that reports an interval carries the level on the record so the two
  writing layers can name it.
- The exported R script has to reproduce what is on screen. If a step cannot be
  expressed faithfully in a few lines, the script says so rather than
  approximating it.

## The two readings

Every result is described twice, and both descriptions read the same record. If
they can describe the same quantity, they have to select it the same way. That
is why `primary_model_row()` lives in `R/reporting.R` and is called from
`R/explanations.R` rather than being reimplemented there.

A change to a figure needs the matching sentence in `R/explanations.R` changed
with it. That pairing was broken once, and the ARIMA text spent a release
describing a forecast band that was never drawn.

## Color and contrast

Palettes are measured, not chosen by eye. `tests/testthat/test-contrast.R`
computes WCAG relative luminance for every palette against every surface a mark
can be drawn on in that theme, and the non-text minimum of 3 to 1 is a floor,
not a target. If a palette needs another color, add it to the test first and
watch it fail.

Interactive controls are at least 44 pixels tall. The test reads the stylesheet.

## Adding a method

1. Add the entry to `ANALYSES` in `R/catalog.R`. Nothing in that file computes.
2. Write `analyse_<engine>()` in the matching engines file.
3. Add a branch to `R/reporting.R` and one to `R/explanations.R`.
4. Add a figure to `R/plots.R` if the method needs one that does not exist.
5. Add the packages it needs to `install.R` and `DESCRIPTION`.
6. Add a test that runs it end to end through both writing layers.

The interface builds itself from the catalog, so the selector, the directory,
the feasibility hints, and the settings form need no editing.

## License

Contributions are accepted under the PolyForm Noncommercial License 1.0.0.
