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
change, say so in the pull request and leave the assertion as it is.

A change to `www/app.js` also needs the browser check, which loads the script
into a simulated page and presses its controls. It needs Node and one package:

```sh
npm install --no-save jsdom
node tests/browser-smoke.js
```

The stand-in for Shiny in that file applies the rule the real one does: a
message handler has to declare exactly one argument. A handler that declares
none is refused with an exception, and every handler registered after it is
lost with it.

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
with it. Nothing checks that pairing automatically, and a sentence describing a
shaded band the figure does not draw is easy to miss in review.

## Color and contrast

Palettes are measured, not chosen by eye. `tests/testthat/test-contrast.R`
computes WCAG relative luminance for every palette against every surface a mark
can sit on in that theme, and the non-text minimum of 3 to 1 is a floor,
not a target. If a palette needs another color, add it to the test first and
watch it fail.

Interactive controls are at least 44 pixels tall. The test reads the stylesheet.

## Adding a method

1. Add the entry to `ANALYSES` in `R/catalog.R`. Nothing in that file computes.
2. Write `analyze_<engine>()` in the matching engines file.
3. Add a branch to `R/reporting.R` and one to `R/explanations.R`.
4. Add a figure to `R/plots.R` if the method needs one that does not exist.
5. Add the packages it needs to `install.R` and `DESCRIPTION`.
6. Add it to a situation in `METHOD_GUIDE` in `R/catalog.R`, and give it a
   drawing in `method_visual_ui()`.
7. Add a test that runs it end to end through both writing layers, and one
   that checks its numbers against a calculation that does not share its code.
8. Add it to `docs/METHODS.md` and `docs/METHOD_AUDIT.md`, with a peer-reviewed
   source for anything that is not a base R routine.

The interface builds itself from the catalog, so the selector, the directory,
the feasibility hints, and the settings form need no editing.

## Evidence

A verdict does not rest on one number. Where a default prior is well
established, a method carries a Bayes factor beside its interval, computed in
`R/evidence.R` from the finished record, and `weighed_signal()` reads the two
together. A new method that tests a claim should either join
`EVIDENCE_METHODS` with a cited prior or state in its checks why it has no
second measure.

## License

Contributions are accepted under the PolyForm Noncommercial License 1.0.0.
