# Daybreak

[![License](https://img.shields.io/badge/license-PolyForm%20Noncommercial%201.0.0-blue)](LICENSE.md)
[![Shiny](https://img.shields.io/badge/Shiny-R-276DC3?logo=r&logoColor=white)](#requirements)
[![Release](https://img.shields.io/github/v/release/drabhikroy/daybreak)](https://github.com/drabhikroy/daybreak/releases/latest)

Daybreak is a local R Shiny application for exploring data, running
statistical analyses, and reading the results. You bring a file and a
question, and it answers twice: once in everyday words, and once in the
language of a methods section. Both readings come from the same numbers,
and both say what the result cannot tell you.

It is written for people who know the question they want to ask but may
not remember every R command or reporting convention. Someone new to
statistics can find a method by describing their situation and read the
answer without a glossary. An experienced analyst can go straight to the
estimates, the diagnostics, the model syntax, and the R code.

Daybreak runs on your computer. Your data are not sent to a remote
service, and there is no account.

![The Daybreak result page in dark mode after a one-way ANOVA on the
iris data. A sidebar on the left shows the three workflow steps. The
main panel shows a Clear signal verdict, three key numbers for omega
squared, the Bayes factor, and rows used, and the start of the everyday
explanation.](docs/screenshots/screenshot-operator-1.png)

## What it does

Daybreak brings the parts of a statistical workflow into one place:

- Explore and prepare a dataset before analysis
- Find a method by describing the situation, by browsing, or by search
- Run the analysis through a guided form
- Read the result as a verdict, key numbers, an everyday explanation, a
  statistical description, and a figure
- Check which conditions held and which need a second look
- Export figures, reports, tables, and the R code
- Save the whole session and reopen it later

The goal is not to replace statistical judgment. Daybreak gives a
structured way to move from data to results while keeping calculations,
assumptions, and limits in view.

### Analyze data

- Import CSV, TSV, Excel, LibreOffice ODS, SPSS, Stata, SAS, R, JSON,
  Parquet, and Feather files
- Load one of eleven built-in datasets with a visible selector, a
  direct Load button, an active-data status, and a raw-row preview.
  Three of them open from the first screen with one press
- Prepare missing data in a workspace with median or category,
  nearest-neighbor, random-forest, and MICE choices, plus one-press
  restoration of the loaded file
- Choose among 56 methods, separated into statistical and
  machine-learning groups, each with its own illustration
- Use Help me choose, which lists situations in plain words and names
  the methods that fit each one, or search all 56 by name or purpose
- See advisory data-fit cues that dim methods whose column needs are
  not apparent while leaving every method selectable
- Run descriptions, group comparisons, equivalence tests, correlation
  and regression, models for binary, ordered, multi-category, and count
  outcomes, multivariate methods, measurement models, rater agreement,
  hierarchical models, survival models, time series, and meta-analysis
- Use trees, random forests, nearest neighbors, elastic net, and naive
  Bayes with held-out checks, alongside PCA and four clustering choices
- Set an explicit low-to-high category order for ordinal regression.
  Repeated data get complete-cell checks, and ARIMA gets a
  regular-spacing check and can project past the last recorded point
  with an interval that widens with distance ahead

The full method directory is in [docs/METHODS.md](docs/METHODS.md). The
release calculation checklist is in
[docs/METHOD_AUDIT.md](docs/METHOD_AUDIT.md).

### Understand results

Daybreak presents every result in one full-width reading order:

1. A verdict and the few numbers you would quote
2. The everyday explanation
3. The statistical description
4. The figure
5. Optional tables, checks, data rows, R code, and files

The everyday explanation gives a short answer, a way to read the
figure, a statement of uncertainty, values and names specific to the
method, and a clear boundary. The statistical description gives
confidence intervals, effect sizes, calibrated wording, and exact
terms.

Seven methods report a Bayes factor beside the classical result, and
the verdict weighs the two. That lets a result say that the data point
to no effect, which a test of difference alone cannot do.

If you change a setting after running, a notice above the result says
so and offers to run it again, so the numbers on screen always state
whether they match the settings beside them.

When an analysis fails, a neutral card explains the failure in everyday
words and keeps the exact R message in a folded section.

### Create reports and figures

Daybreak can export:

- Figures as PNG, JPEG, TIFF, SVG, PDF, EPS, flattened PSD, and LaTeX
  TikZ files
- Reports as text, Markdown, HTML, Word, PDF, and LaTeX files
- The R code behind a result, shown in the app to read and copy, or as
  a file
- A ZIP bundle with the report, the figure, CSV tables, and
  reproducible R code
- Portable `.daybreak.rds` session files that reopen the active data,
  the choices, the completed result, both explanations, and the display
  colors

Raster figure presets range from 144 through 600 dpi. Vector exports
are SVG, PDF, and EPS. PSD output is a flattened image because a
statistical figure is created as one composed graphic, not as editable
Photoshop layers. TikZ output needs `tikzDevice` and a working TeX
installation.

HTML and Word reports need `rmarkdown`. PDF reports also need TeX.
LaTeX source can be downloaded without compiling it.

## What it does not do

Daybreak covers commonly taught descriptive and inferential families,
several predictive models, and a missing-data workspace, but no finite
menu can represent every statistical model.

This release does not include:

- Complex survey weights
- Pooled inference across multiple imputed datasets
- Bayesian models. The Bayes factors reported beside seven tests are a
  second reading of the evidence, not fitted Bayesian models
- Causal estimators
- Spatial models
- Meta-regression, or corrections for publication bias
- Specialized psychometric models

The method directory states what this release computes.

## Requirements

R 4.2 or later. R 4.4 or later is recommended, and is needed to open a
saved session or an `.rds` or `.RData` file, because earlier releases
can run code hidden inside those files while reading them. Every other
format works on R 4.2.

The first launch checks the R packages Daybreak uses and installs any
that are missing into your personal R library.

## Install

From the project folder, run:

```r
source("run_daybreak.R")
```

The app checks the complete package list before Shiny starts, installs
missing packages from CRAN into a personal R library, and verifies the
result. The same check runs when the app opens from RStudio, Finder, or
`run_daybreak.R`. Later starts repeat the quick check and do not
reinstall packages that are already present.

On macOS, `launch_daybreak.command` runs the same check from Finder.
The first opening may require Control-click and selecting Open because
the file is not code-signed.

If an installation remains incomplete, startup stops and names each
core package that still needs attention. An optional package that could
not be installed costs the methods that need it and nothing else.

## Using Daybreak

A short walkthrough opens on the first visit and can be reopened from
Help.

1. Upload your own file or load a built-in example. Both choices are
   always visible.
2. Open the missing-data workspace when blank cells need a documented
   working copy.
3. Choose a method from the sidebar, or open Methods and use Help me
   choose, the full list, or the search box.
4. Assign columns to the labeled roles.
5. Open analysis settings only when the defaults need to change.
6. Run the analysis. Command and Return, or Control and Return, runs it
   from anywhere on the page.
7. Read the verdict, the everyday answer, the statistical description,
   and the figure from top to bottom. Open Exact values, Checks, or R
   code when you need supporting detail.
8. Download the figure, the report, the code, the complete bundle, or a
   `.daybreak.rds` session file for later work.

The interface shows the expected data structure beside each control.
Methods, data formats, Help, session files, color-vision settings, and
the raw-data viewer remain available without leaving the analysis
screen.

## Your data

Each row should represent one observation unless the chosen method
states that long-form repeated rows are expected. The first row of a
text or spreadsheet file should contain column names.

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

SPSS user-missing codes, such as 97, 98, and 99 marked as missing in
the source file, arrive as missing values and not as ordinary
categories.

Text columns become categories when the number of distinct values looks
like a category set and not free text. The ceiling scales with file
size, so a short file does not treat every distinct comment as its own
category. The Data used tab reports the type each column was given.

The upload limit is 200 MB per Shiny session. Large files may still
require more memory while a model is fitted.

Nothing you load leaves the computer. Statistical values, the everyday
explanation, and the statistical description come from deterministic R
code that runs in your own R session.

## Local model

The local model is optional. Every explanation is written by R code
with no language model involved, and nothing in the statistics, the
figures, or the reports depends on one.

If you would like a second wording, a small model running on the same
computer through [Ollama](https://ollama.com) can restate a finished
explanation. The model receives only the finished everyday text. Raw
data rows are never sent to it. A rewrite is discarded if it adds,
removes, changes, or reorders a number, if it loses any of the five
section headings, or if the model hands back its instructions in place
of a rewrite. Names containing `cloud` are filtered out, so a
rewrite cannot be routed to a remote model.

Setup is three steps in the Local models window, and you can stop after
any of them.

1. **Choose how to run it.** Use an Ollama you install yourself, or let
   Daybreak keep its own copy in a folder that belongs to the app.
   Nothing is installed into the system on the second route, and one
   press removes it.
2. **Get Ollama running.** Daybreak finds an existing installation and
   starts it, or downloads its own copy from the Ollama release page
   and checks the download against the published SHA-256 before
   unpacking it.
3. **Pick a model.** Four are described, each with what it is good at
   for this job and where it falls short. With your permission Daybreak
   reads the operating system, the amount of memory, the processor core
   count, and whether there is hardware that speeds models up, and
   marks which models fit. Models you already pulled from a terminal
   appear in the same list. You can download, test, switch between, and
   remove models from here.

The first reply from a model may take several minutes while it is
loaded into memory. Later replies are quicker while it stays loaded.

A private copy is offered on macOS and Linux. On Windows, Ollama comes
as an installer, so Daybreak uses the copy you install.

## Accessibility

- Dark mode by default, with an optional light mode
- Separate settings for protanopia, deuteranopia, tritanopia, and
  monochrome, with a measured palette for each theme
- Every color clears the WCAG 2.2 non-text minimum of 3 to 1 against
  every surface it can sit on, checked by an automated test
- Monochrome carries four steps, because past four a gray ramp stops
  being tellable apart, so those figures pair the ramp with shape or
  line style
- Figures that pair color with shape, line style, position, or labels
- Verdicts marked by a different symbol each, and not by color alone
- Interactive controls at least 44 pixels tall
- Keyboard operation throughout, a skip link to the results, visible
  focus, and reduced motion honored
- Layouts for narrow screens, where the header takes two rows and a
  figure scrolls inside its card instead of shrinking past legibility

## How it works

Statistical calculations, explanations, and descriptions are generated
directly from R code. The same data and settings produce the same
result each time.

Daybreak separates calculation from explanation. The reporting layer
receives a completed result record. It cannot refit a model or change a
value. Its wording follows four rules:

1. Lead with the estimate and interval when both exist.
2. Describe evidence without treating a p-value as the probability that
   a hypothesis is true.
3. Distinguish statistical evidence from practical importance.
4. Reserve causal language for designs that support it. Most methods in
   this app describe association or comparison.

No single number decides a verdict where a second one is available.
The interval says which sizes of effect fit the data. The Bayes factor
says which account predicts the data better, a real effect or none.
The verdict reads the two together, following the American Statistical
Association statement on p-values (Wasserstein and Lazar, 2016). The
priors, the thresholds, and their sources are in
[docs/METHODS.md](docs/METHODS.md).

Automated checks cover only conditions that can be assessed from the
supplied columns. Subject-matter validity, sampling design, measurement
quality, data provenance, and defensible causal assumptions still
require an analyst.

## Help

Help is inside the app and works with no network. It covers the
workflow, how to choose a method, how to read a result, what each
verdict means, a glossary of the terms the result page uses, data
preparation, the local model, keyboard shortcuts, and session files. It
also states the version, the license, the copyright, and where to find
the newest release.

## For developers

### Project layout

```text
app.R                         interface and session flow
run_daybreak.R                package check and app launcher
launch_daybreak.command       Finder launcher for macOS
install.R                     dependency list and first-run installation
DESCRIPTION                   package metadata
CONTRIBUTING.md               conventions this codebase follows
SECURITY.md                   what the app is exposed to and how to report
RELEASE_TEMPLATE.md           release checklist
R/version.R                   release identity, read by every other file
R/catalog.R                   method directory, roles, and the guide
R/data_io.R                   imports, examples, and column profiles
R/analysis_utils.R            shared calculations and result record
R/evidence.R                  Bayes factors and the weighing of evidence
R/imputation.R                missing-data working copies
R/engines_core.R              descriptive tests and familiar models
R/engines_advanced.R          latent, clustered, survival, and time models
R/engines_ml.R                predictive models and held-out checks
R/engines_extra.R             equivalence, agreement, and meta-analysis
R/reporting.R                 deterministic statistical description
R/explanations.R              deterministic everyday explanation
R/session_state.R             portable session file schema and validation
R/local_models.R              optional local rewrite and numeric check
R/runtime.R                   finding, installing, and removing Ollama
R/plots.R                     chart system and color settings
R/exports.R                   reports, figures, code, and ZIP bundles
R/ui_helpers.R                reusable interface pieces
www/app.css                   visual tokens and responsive layout
www/app.js                    display controls, tabs, and keyboard behavior
www/icons/                    application icon variants
report/analysis-report.Rmd    HTML, Word, and PDF report template
tests/                        calculation, contrast, export, and startup checks
docs/                         method, architecture, and release notes
.github/workflows/            tests on push, release on a version tag
```

### Dependencies

The core app uses `shiny`, `ggplot2`, `dplyr`, `tidyr`, `readr`, and
`scales`. Advanced, evidence, machine-learning, missing-data, import,
export, local-model, and test packages are checked in the same startup
pass. `install.R` holds the lists and `DESCRIPTION` mirrors them.

### Tests and standards gates

```r
source("tests/run_tests.R")
```

The runner checks calculations and reporting first, then starts the
Shiny app on a temporary local port and confirms that it serves the
Daybreak page.

Some of the files are worth naming. `test-exports.R` calls every
download path directly, since a download fails at the moment of use and
not at startup. `test-contrast.R` computes WCAG relative luminance for
every palette against every surface it can sit on, and reads the
stylesheet for touch target sizes, so the accessibility claims in this
file are measured and not asserted. `test-security.R` runs every method
again with column names built to break out of their quoting.
`test-writing.R` holds the prose rules. `test-interface.R` fails when
the server sends a message the page has no handler for.

The browser script has its own check, which needs Node:

```sh
npm install --no-save jsdom
node tests/browser-smoke.js
```

## Contributing

Issues and pull requests are welcome. [CONTRIBUTING.md](CONTRIBUTING.md)
describes the conventions the code follows, and
[CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) describes the conduct expected
in the project. Security reports go through
[SECURITY.md](SECURITY.md).

## Releases

Each release is listed at
<https://github.com/drabhikroy/daybreak/releases> with a downloadable
copy. [CHANGELOG.md](CHANGELOG.md) records what changed in each one.

## Citing

If you use Daybreak in published work, please cite the software.
[CITATION.cff](CITATION.cff) holds the details, and GitHub offers them
under Cite this repository.

## License

[PolyForm Noncommercial License 1.0.0](LICENSE.md). The full text is also at
<https://polyformproject.org/licenses/noncommercial/1.0.0>.

Personal use, personal study, hobby projects, teaching, academic research, and
use by charitable, educational, nonprofit, public research, public health, and
government organizations are permitted. Commercial use is not permitted without
a separate license.

Required notice: Copyright 2026 Abhik Roy.
