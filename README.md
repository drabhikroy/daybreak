# Daybreak

Statistics you can read.

Daybreak is a local R Shiny workbench for descriptive statistics, group
comparisons, regression, multivariate analysis, measurement models,
hierarchical models, survival analysis, time series, missing-data preparation,
and common machine-learning tasks. It is written for
people who know the question they want to ask but may not remember every R
command or reporting convention. Experienced analysts can inspect the full
tables, diagnostics, model syntax, and exported R code.

The app never sends data to a remote service. Statistical values, the everyday
explanation, and the statistical description come from deterministic R code. An
optional Ollama connection can restate finished text with a model that runs on
the same Mac. Raw rows are not given to that model.

## What is included

- 53 paths, separated into statistical and machine-learning groups
- A direct method selector plus a categorized directory with a distinct
  illustration for every method
- CSV, TSV, Excel, LibreOffice ODS, SPSS, Stata, SAS, R, JSON, Parquet, and
  Feather import
- Nine real datasets with a visible selector, direct Load button, active-data
  status, and raw-row preview
- A missing-data workspace with median or category, nearest-neighbor,
  random-forest, and MICE choices, plus one-click restoration of the loaded file
- Trees, random forests, nearest neighbors, elastic net, and naive Bayes with
  held-out checks, alongside PCA and four clustering choices
- Advisory data-fit cues that dim methods whose column needs are not apparent
  while leaving every method selectable
- Neutral error cards that explain a failed run in everyday words and keep the
  exact R message in a folded section
- A novice reading with a short answer, mental picture, uncertainty statement,
  method-specific values and names, and a clear boundary
- One full-width reading order: everyday explanation, statistical description,
  figure, then optional tables, checks, data rows, and files
- A separate statistical description with confidence intervals, effect
  sizes, calibrated wording, and exact terms
- An explicit low-to-high category order for ordinal regression, complete-cell
  checks for repeated data, and regular-spacing checks for ARIMA, which can
  also project past the last recorded point with an interval that widens with
  distance ahead
- Optional local rewriting through Ollama, with live model testing, readable
  connection errors, a numeric-change check, and no remote model call
- Dark mode by default, plus light mode
- Separate settings for protanopia, deuteranopia, tritanopia, and monochrome,
  with a measured palette for each theme. Every color clears the WCAG 2.2
  non-text minimum of 3 to 1 against every surface it can be drawn on.
  Monochrome carries four steps, because past four a grey ramp stops being
  tellable apart, so those figures pair the ramp with shape or line style
- Figures that pair color with shape, line style, position, or labels
- PNG, JPEG, TIFF, SVG, PDF, EPS, flattened PSD, and LaTeX TikZ figure export
- Text, Markdown, HTML, Word, PDF, and LaTeX report export
- A ZIP bundle with report, figure, CSV tables, and reproducible R code
- Automated checks for calculations, catalog integrity, contrast, prose, and
  application startup
- An automatic raw-data preview and an in-app Help guide
- Portable `.daybreak.rds` session files that reopen the active data, choices,
  completed result, explanations, and display colors

The full method directory is in [docs/METHODS.md](docs/METHODS.md). The
release calculation checklist is in [docs/METHOD_AUDIT.md](docs/METHOD_AUDIT.md).

## Install and run

Daybreak supports R 4.0 or later. From the project folder, run:

```r
source("run_daybreak.R")
```

The app checks the complete package list before Shiny starts, installs missing
packages from CRAN into a personal R library, and verifies the result. This
same check runs when the app opens from RStudio, Finder, or `run_daybreak.R`.
Later starts repeat the quick check and do not reinstall packages that are
already present.

On macOS, `launch_daybreak.command` runs the same check from Finder. The first
open may require Control-click, Open because the file is not code-signed.

The core app uses `shiny`, `ggplot2`, `dplyr`, `tidyr`, `readr`, and `scales`.
Advanced, machine-learning, missing-data, import, export, local-model, and test
packages are checked in the same startup pass. If an installation remains
incomplete, startup stops and names each package that still needs attention.

## Use the app

1. Upload your own file or load a built-in example. Both choices are always visible.
2. Open the missing-data workspace when blank cells need a documented working copy.
3. Choose a method from the sidebar or browse the directory by the question it
   answers.
4. Assign columns to the labeled roles.
5. Open analysis settings only when the defaults need to change.
6. Run the analysis. Command and Return, or Control and Return, runs it from
   anywhere on the page.
7. Read the everyday answer, statistical description, and figure from top to
   bottom. Open Exact values or Checks when you need supporting detail. If you
   change a setting after running, a notice above the result says so and offers
   to run it again, so the numbers on screen always state whether they match
   the settings beside them.
8. Download the figure, report, code, complete bundle, or a `.daybreak.rds` session
   file for later work.

The interface shows the expected data structure beside each control. Methods,
data formats, Help, session files, color-vision settings, and the raw-data viewer remain
available without leaving the analysis screen.

## Optional local model

The Local models window gives macOS setup steps and three compact choices. The
recommended choice is `qwen3.5:4b`, a 3.4 GB download. `llama3.2:3b` is the
smallest listed download, and `gemma3:4b` is another compact option.

1. Install and open [Ollama](https://ollama.com/download).
2. Open Local models in Daybreak.
3. Select a model and use Download selected model. If the Ollama command is not
   visible to R, the window gives the matching Terminal command.
4. Use Test selected model. The first reply may take several minutes while the
   model is loaded into memory. Later replies should be quicker while the model
   remains loaded.
5. Run an analysis and select Rewrite with a local model.

This step is optional. The model receives only the finished everyday text. A
rewrite is discarded if it adds, removes, changes, or reorders a number. Names
containing `cloud` are filtered from the selector.

## Import notes

Each row should represent one observation unless the chosen method states that
long-form repeated rows are expected. The first row of a text or spreadsheet
file should contain column names.

| Source | Extensions | R package |
| --- | --- | --- |
| Delimited text | `.csv`, `.tsv`, `.txt` | `readr` |
| Excel | `.xlsx`, `.xls` | `readxl` |
| LibreOffice | `.ods` | `readODS` |
| SPSS | `.sav`, `.zsav`, `.por` | `haven` |
| Stata | `.dta` | `haven` |
| SAS | `.sas7bdat`, `.xpt` | `haven` |
| R | `.rds`, `.rdata`, `.rda` | Base R |
| JSON | `.json` | `jsonlite` |
| Columnar | `.parquet`, `.feather` | `arrow` |

SPSS user-missing codes, such as 97, 98, and 99 marked as missing in the source
file, arrive as missing values rather than as ordinary categories.

Text columns become categories when the number of distinct values looks like a
category set rather than free text. The ceiling scales with file size, so a
short file does not treat every distinct comment as its own category. The Data
used tab reports the type each column was given.

The upload limit is 200 MB per Shiny session. Large files may still require
more memory while a model is fitted.

## Export notes

Raster figure presets range from 144 through 600 dpi. Vector exports are SVG,
PDF, and EPS. PSD output is a flattened image because a statistical figure is
created as one composed graphic, not as editable Photoshop layers. TikZ output
needs `tikzDevice` and a working TeX installation.

HTML and Word reports need `rmarkdown`. PDF reports also need TeX. LaTeX source
can be downloaded without compiling it.

## Reporting policy

The reporting layer receives a completed result record. It cannot refit a
model or change a value. Its wording follows four rules:

1. Lead with the estimate and interval when both exist.
2. Describe evidence without treating a p-value as the probability that a
   hypothesis is true.
3. Distinguish statistical evidence from practical importance.
4. Reserve causal language for designs that support it. Most methods in this
   app describe association or comparison.

Automated checks cover only conditions that can be assessed from supplied
columns. Subject-matter validity, sampling design, measurement quality, data
provenance, and defensible causal assumptions still require an analyst.

## Project layout

```text
app.R                         interface and session flow
run_daybreak.R                package check and app launcher
launch_daybreak.command       Finder launcher for macOS
install.R                     dependency list and first-run installation
DESCRIPTION                   package metadata
CONTRIBUTING.md               conventions this codebase follows
RELEASE_TEMPLATE.md           release checklist
R/version.R                   release identity, read by every other file
R/catalog.R                   method directory and variable roles
R/data_io.R                   imports, examples, and column profiles
R/analysis_utils.R            shared calculations and result record
R/imputation.R                missing-data working copies
R/engines_core.R              descriptive tests and familiar models
R/engines_advanced.R          latent, clustered, survival, and time models
R/engines_ml.R                predictive models and held-out checks
R/reporting.R                 deterministic statistical description
R/explanations.R              deterministic everyday explanation
R/session_state.R             portable session file schema and validation
R/local_models.R              optional local rewrite and numeric check
R/plots.R                     chart system and color settings
R/exports.R                   reports, figures, code, and ZIP bundles
R/ui_helpers.R                reusable interface pieces
www/app.css                   visual tokens and responsive layout
www/app.js                    display controls, tabs, and keyboard behavior
www/icons/                    application icon variants
report/analysis-report.Rmd    HTML, Word, and PDF report template
tests/                        calculation, contrast, export, and startup checks
docs/                         method, architecture, and review notes
.github/workflows/            tests on push, release on a version tag
```

## Tests

```r
source("tests/run_tests.R")
```

The runner checks calculations and reporting first, then starts the Shiny app
on a temporary local port and confirms that it serves the Daybreak page.

Two of the files are worth naming. `test-exports.R` runs every download path,
because a missing constant once reached a release through the one file that had
no test. `test-contrast.R` computes WCAG relative luminance for every palette
against every surface it can be drawn on, and reads the stylesheet for touch
target sizes, so the accessibility claims in this file are measured rather than
asserted.

## Current boundary

Daybreak covers commonly taught descriptive and inferential families, several
predictive models, and a missing-data workspace, but no finite menu can
represent every statistical model. Complex survey weights, pooled inferential
models across MICE datasets, Bayesian models, causal estimators, spatial
models, meta-analysis, and specialist psychometric models remain outside this
release. The method directory states what this release actually computes.

## License

Copyright Abhik Roy. Released under the
[PolyForm Noncommercial License 1.0.0](LICENSE.md).
