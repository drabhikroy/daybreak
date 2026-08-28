# Daybreak

[![License](https://img.shields.io/badge/license-PolyForm%20Noncommercial%201.0.0-blue)](LICENSE)
[![Shiny](https://img.shields.io/badge/Shiny-R-276DC3?logo=r&logoColor=white)](#requirements)
[![Release](https://img.shields.io/github/v/release/drabhikroy/rank-and-folder)](https://github.com/drabhikroy/rank-and-folder/releases)

Daybreak is a local R Shiny application for exploring data, running
common statistical analyses, and creating clear reports. It helps
researchers and analysts move from a research question to an
interpretable result without needing to remember every R command or
reporting convention.

Daybreak supports descriptive statistics, group comparisons, regression,
multivariate analysis, measurement models, hierarchical models, survival
analysis, time series, missing-data preparation, and common
machine-learning methods.

The app is designed for people who know the question they want to answer
but want support choosing analyses, checking results, interpreting
output, and creating reports. Experienced R users can review detailed
tables, diagnostics, analysis details, and generated R code.

![The Daybreak results page showing a matched comparison
analysis](docs/screenshots/screenshot-operator-1.png)

## What Daybreak does

Daybreak brings together several parts of the statistical workflow in
one application:

-   Explore and prepare datasets before analysis
-   Select methods based on research questions or statistical goals
-   Run analyses through a guided interface
-   Review results with plain-language explanations and statistical
    details
-   Export figures, reports, tables, and R code
-   Save complete analysis sessions for later review

The goal is not to replace statistical judgment. Instead, Daybreak
provides a structured way to move from data to results while keeping
calculations, assumptions, and limitations visible.

## Privacy and local processing

Daybreak runs on your computer. Your data are not sent to a remote
service.

Statistical calculations, explanations, and descriptions are generated
directly from R code. The same analysis settings produce the same
results each time.

An optional connection to Ollama can rewrite completed explanations
using a local model that runs on the same computer. Raw data rows are
never sent to that model.

## Features

### Analyze data

-   Import CSV, TSV, Excel, LibreOffice ODS, SPSS, Stata, SAS, R, JSON,
    Parquet, and Feather files
-   Load built-in example datasets with a visible selector and raw-data
    preview
-   Review missing data using several common approaches
-   Browse 53 analysis options organized by statistical purpose and
    machine-learning group
-   Select methods directly or browse methods by the question they
    address
-   Run descriptive analyses, comparisons, regression models,
    multivariate methods, measurement models, hierarchical models,
    survival models, time series methods, and predictive models
-   Use trees, random forests, nearest neighbors, elastic net, naive
    Bayes, PCA, and clustering methods with built-in checks

### Understand results

Daybreak presents results in a consistent order:

1.  Plain-language explanation
2.  Statistical description
3.  Figure
4.  Optional tables, checks, data details, and files

Results include confidence intervals, effect sizes, statistical details,
and method-specific information when available.

When an analysis fails, Daybreak provides a readable explanation along
with the original R message.

### Create reports and figures

Daybreak can export:

-   Figures as PNG, JPEG, TIFF, SVG, PDF, EPS, flattened PSD, and LaTeX
    TikZ files
-   Reports as text, Markdown, HTML, Word, PDF, and LaTeX files
-   Complete ZIP bundles containing reports, figures, tables, and
    generated R code
-   Portable `.daybreak.rds` session files containing analysis choices,
    results, explanations, and display settings

## Accessibility and customization

Daybreak includes features designed to make statistical results easier
to read.

-   Dark mode by default, with an optional light mode
-   Settings for protanopia, deuteranopia, tritanopia, and monochrome
    vision differences
-   Color choices checked against WCAG 2.2 contrast requirements
-   Figures that use color with shape, line style, position, or labels
-   Keyboard shortcuts throughout the application

## Install and run

Daybreak requires R 4.0 or later.

From the project folder, run:

``` r
source("run_daybreak.R")
```

The first launch checks required R packages and installs missing
packages into your personal R library.

On macOS, `launch_daybreak.command` starts Daybreak from Finder. The
first opening may require Control-click and selecting Open because the
file is not code-signed.

## Using Daybreak

1.  Upload your dataset or choose a built-in example.
2.  Review the data preview and prepare missing data if needed.
3.  Select an analysis.
4.  Assign columns to the requested roles.
5.  Adjust settings when needed.
6.  Run the analysis.
7.  Review the explanation, statistical details, figures, and supporting
    information.
8.  Export results or save a session file.

## Optional local model support

Daybreak can connect to Ollama for optional local rewriting of completed
explanations.

The local model receives only the finished explanation. Raw data rows
are not provided.

To use this feature:

1.  Install and open Ollama.
2.  Open Local models in Daybreak.
3.  Select and download a model.
4.  Test the model.
5.  Choose Rewrite with a local model after running an analysis.

## Reporting approach

Daybreak separates statistical calculations from explanation text.

The reporting system does not refit models or change calculated values.
Explanations follow these principles:

1.  Report estimates and intervals when available.
2.  Describe statistical evidence without treating a p-value as the
    probability that a hypothesis is true.
3.  Distinguish statistical evidence from practical importance.
4.  Use causal language only when supported by the study design.

Automated checks can evaluate only conditions available from the
supplied data. Research design, measurement quality, sampling, and
interpretation still require analyst judgment.

## Current boundaries

Daybreak covers many commonly used statistical approaches, including
descriptive analyses, comparisons, regression models, predictive
methods, and missing-data preparation.

The current release does not include:

-   Complex survey weights
-   Pooled inference across multiple imputed datasets
-   Bayesian models
-   Causal estimators
-   Spatial models
-   Meta-analysis
-   Specialized psychometric models

The method directory describes what each release supports.

## Tests

Run the test suite with:

``` r
source("tests/run_tests.R")
```

The test suite checks calculations, reporting, exports,
accessibility-related features, and application startup.

## License

Copyright Abhik Roy.

Released under the [PolyForm Noncommercial License 1.0.0](LICENSE.md).
