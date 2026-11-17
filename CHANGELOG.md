# Changes

## 1.0.0 - 2026-11-16

- Added a second evidence check. One-sample, independent, and paired t tests,
  one-way ANOVA, the chi-square test, the one-proportion test, and Pearson
  correlation now report a Bayes factor beside the interval, and the verdict
  weighs the two together.
- Added three methods: an equivalence test with a margin you set, rater
  agreement with Cohen's kappa and its weighted forms, and random-effects
  meta-analysis with a forest plot and a prediction interval.
- Added two example datasets, the multiple sclerosis diagnoses of Westlund and
  Kurland and the thirteen BCG vaccine trials.
- Added a Help me choose guide to the Methods directory, a search box across
  all 56 methods, and compact method cards.
- Added a first-visit walkthrough that can be reopened from Help.
- Added a verdict and key numbers at the top of every result, and an R code
  tab that shows the script and copies it.
- Rebuilt Local models as three steps: choose how to run Ollama, get it
  running, and pick a model. Daybreak can keep its own copy of Ollama in a
  folder it can remove, checks that download against the published SHA-256,
  describes what each model is good at and where it falls short, and can add,
  remove, and switch between installed models.
- Rejected a local rewrite when the model returns its instructions in place
  of a rewrite, which the heading and number checks alone would have passed.
- Added a glossary, a guide to the verdicts, keyboard shortcuts, and version,
  license, and release details to Help.
- Fixed the messages from the server to the page, none of which were being
  received, so a quick-start example now runs and the sidebar follows the
  workflow.
- Fixed repeated-measures ANOVA, which treated a numeric condition as a
  straight-line trend and returned no test. It now reports the
  Greenhouse-Geisser correction and partial eta squared.
- Fixed the two-proportions figure, which failed with an error.
- Fixed the one-proportion verdict, which was judged against zero instead of
  the reference value.
- Fixed figures that kept their dark colors when the page opened in light mode.
- Fixed a type scale that rendered smaller than intended, and laid out the
  header and figures for narrow screens.
- Restored `SECURITY.md`, `CODE_OF_CONDUCT.md`, `.Rbuildignore`, and the
  standard README structure, which the 0.10.1 commit removed by mistake.
- Listed every optional package in `DESCRIPTION` so continuous integration
  installs what the suite uses.

## 0.10.1 - 2026-11-04

- Quoted every column name written into an exported R script, so a name that
  contains quotes or backticks stays a name and cannot run as code.
- Escaped column names and other data-derived text in the Markdown, HTML,
  Word, and PDF reports, so a name written as markup shows as plain text.
- Refused session files and `.rds` and `.RData` data files on R releases before
  4.4.0, which can run code hidden inside such files while reading them.
- Fixed confirmatory factor analysis, structural equation models, and
  mediation, which stopped with an internal error instead of fitting.
- Fixed one-way ANOVA, classification and regression trees, random forests,
  and naive Bayes for column names that contain spaces or symbols.
- Limited exploratory factor analysis to the number of factors the selected
  columns can identify, and left the factor count blank by default so Daybreak
  chooses two, or one when the columns cannot support two.
- Named the column in level-count messages instead of a generic phrase.
- Started the quick-start example once the page has finished loading instead
  of after a fixed delay.
- Made the test suite runnable from any working directory, and started the
  startup check in its own R process.
- Added tests for script quoting, report escaping, renamed columns across
  every method, the factor-count limit, and several calculation details.
- Made the release workflow draft the release page for review instead of
  publishing it.
- Raised the installer's minimum R version to 4.2 to match `DESCRIPTION`, and
  stated the R 4.4 requirement for saved sessions in the README.

## 0.10.0 - 2026-08-02

- Moved the version number into `R/version.R` so the session file, the
  Markdown, HTML, Word, and PDF reports, and the analysis bundle all read it
  from one place.
- Corrected the event coding in the `lung` example so the 128 deaths count as
  events and the example runs.
- Named the selected confidence level in every interval sentence instead of a
  fixed 95 percent.
- Made the statistical description and the everyday explanation headline the
  same coefficient, judged on the log scale for odds, rate, and hazard ratios.
- Described large ratios as multiples and categorical coefficients as category
  contrasts in the everyday explanation.
- Centered numeric predictors in moderation so each lower-order coefficient
  reads at the average of the other predictor, and wrote the centering into the
  exported script.
- Matched linear model intervals to `confint(fit)` in the exported script.
- Added ARIMA forecasting with an interval at the selected level, a horizon
  capped at a quarter of the series, and calendar months for monthly data.
- Allowed optional roles to stay empty, so descriptives run without a grouping
  column.
- Required matching response categories for McNemar's test.
- Read SPSS user-missing codes as missing values.
- Computed coefficient alpha from one set of complete rows.
- Gave the survival summary table readable column names and stated the event
  value in the checks.
- Moved Pillai's trace into the tests table for MANOVA.
- Ran tree fitting inside the local seed so it leaves the session random
  stream alone, and set the same seed in the exported script.
- Drew the fitted model in the growth figure and handled a categorical focal
  predictor in the moderation figure.
- Added separate chart palettes for the light theme and rebuilt monochrome as
  four steps, all measured above 3 to 1 against every surface.
- Raised interactive controls to 44 pixels, darkened light mode secondary text,
  added a focus trap to dialogs, and put the data-fit reason in the option
  label for screen readers.
- Added an answer panel with a verdict at the top of each result, quick-start
  examples on the opening screen, a notice when settings change after a run, a
  progress indicator, and Command or Control with Return to run.
- Rewrote the local model instructions to keep the five explanation sections
  and the existing picture, and rejected rewrites that drop a section or change
  a number.
- Limited the installer's hard stop to the six core packages.
- Added export and contrast tests, and CI and release workflows.

## 0.9.2 - 2026-08-02

- Moved the complete dependency check into `app.R` so RStudio's Run App button,
  the Finder launcher, and `run_daybreak.R` all install missing packages before
  Shiny loads.
- Added a writable personal R library for first-run installation and retained
  the final verification of every core and optional package, including
  `lmerTest`.
- Changed Ollama generation to request a short non-thinking reply, wait up to
  five minutes for the first model load, and keep the model loaded for ten
  minutes afterward.
- Replaced the generic Ollama connection message with separate details for a
  timeout, missing selected model, stopped local service, and other request
  failures.
- Restyled local model IDs with a dark-mode-safe background, border, and gold
  text.
- Added regression checks for the Ollama request body, connection messages,
  app-level installer call, personal package library, `lmerTest`, and local
  model ID colors.

## 0.9.1 - 2026-08-02

- Replaced browser-dependent dialog closing with an explicit open and closed
  state shared by the X, backdrop, Escape key, and server messages.
- Added browser checks for the static-dialog X and backdrop, color-vision X,
  URL cleanup, and accessibility visibility.
- Replaced the generic failed-analysis notice with the exact requirement that
  stopped the calculation.
- Added the active dataset and selected column roles to the full error card,
  kept the plain explanation and next action visible, and moved that card into
  view after a failure.
- Added plain mappings for empty complete-row sets, one-level categories, and
  nonfinite numeric values while keeping unfamiliar R messages visible.

## 0.9.0 - 2026-08-02

- Corrected partial-correlation degrees of freedom, p-values, intervals, and
  rank handling when controls are present.
- Changed Mann-Whitney and paired Wilcoxon effect summaries to
  Hodges-Lehmann shifts, with safe handling when an interval cannot be formed.
- Added checks for repeated-data cells, model-matrix rank, outcome variation,
  sparse binary outcomes, cluster counts, random slopes, survival times,
  observed events, ARIMA order, and regular time spacing.
- Applied the selected confidence level to regression, proportion, rank,
  mixed, survival, and time-series intervals that expose that setting.
- Added an editable low-to-high category order for ordinal logistic
  regression and carried that order into exported R code.
- Kept randomized splitting, fold assignment, K-nearest-neighbor tie breaking,
  clustering starts, Fisher simulation, imputation, and bootstrap mediation
  from changing the caller's random-number state.
- Rebuilt machine-learning exports so they contain the complete-row filter,
  saved split, predictor preparation, chosen settings, and fitted call instead
  of undefined training placeholders.
- Corrected logistic exported code, custom-order ordinal code, ordered lavaan
  arguments, mediation settings, LaTeX special-character handling, and the
  local-model number-order check.
- Added complete method-path tests, invalid-input cases, exported-code replay,
  random-state checks, malformed-session checks, and another full browser,
  stylesheet, drawing, and documentation pass.

## 0.8.0 - 2026-08-02

- Rebuilt the color-vision menu as a centered two-column choice list. Every
  swatch row and every palette name now begins on a shared left edge.
- Added a live Test selected model action that asks the chosen Ollama model for
  a short reply before it is used on an analysis.
- Split Ollama response parsing from web requests and added checks for model
  discovery, empty installations, server errors, missing models, incomplete
  replies, malformed replies, and pull-command arguments.
- Changed Ollama failures to include the readable error returned by the local
  service instead of showing only a status number.
- Added dialog focus return, Escape-key focus return, scroll locking, contained
  dialog scrolling, and a short reduced-motion-aware opening transition.
- Added restrained hover and selection transitions to header, workflow, color,
  and result controls without changing the established page structure.
- Repeated the full source, catalog, calculation, explanation, session, export,
  browser, stylesheet, dependency, drawing, and documentation review while
  retaining the 0.7.0 archive as the rollback release.

## 0.7.0 - 2026-08-02

- Replaced the two-column result overview with one full-width reading order:
  everyday explanation, statistical description, figure, then optional detail.
- Rewrote the everyday ANOVA result around the selected outcome, group names,
  observed averages, lowest and highest groups, overall result, and pair checks.
- Returned the statistical description to a visible panel directly below the
  everyday explanation.
- Changed session downloads to checked `.daybreak.rds` files and retained the
  complete write, reopen, and malformed-file tests.
- Centered file and download button text, made Help card gaps consistent, and
  increased plot spacing between tick labels and axis titles.
- Redrew tree, forest, and partial-correlation method diagrams and removed the
  decorative card grid and hover movement.
- Replaced the cumulative code-review log with a shorter review of the current
  release and added brief section comments to the Shiny entry point.

## 0.6.0 - 2026-08-02

- Replaced broad everyday statements with result-aware wording that names the
  selected columns, observed categories, group summaries, fitted terms, and
  relevant follow-up comparisons.
- Added an Iris ANOVA regression check that requires Sepal.Length, Species, all
  three flower groups, their observed averages, and adjusted follow-up wording.
- Placed the result-specific answer beside the figure and moved formal prose,
  exact tables, checks, data rows, and files into one secondary section row.
- Reworked all method miniatures around one measured field and removed every
  embedded word or number. Rank, repeated-measure, and density drawings were
  redrawn with shared geometry and spacing.
- Added downloadable `.daybreak` session files with checked reopening of data,
  method, roles, settings, completed results, explanations, and display colors.
- Added research notes for graphical perception, overview-first interaction,
  visual aesthetics, chart memory, WCAG 2.2, and Shiny state handling.

## 0.5.0 - 2026-08-01

- Added a launcher that installs and verifies the complete R package set before
  opening the app.
- Added elastic-net classification and regression, naive Bayes classification,
  DBSCAN, and HDBSCAN with held-out or exploratory reporting as appropriate.
- Added advisory data-fit screening. Methods whose required column types are
  not apparent are dimmed but remain selectable and runnable.
- Replaced raw red analysis failures with neutral everyday guidance, a next
  step, and a folded exact R message.
- Removed embedded words and values from every method miniature, added a fixed
  inner margin, and reviewed all 53 drawings as one contact sheet.
- Reduced result-page density with a smaller everyday lead, a folded data note,
  and separate estimate-table sections that open only when requested.
- Restored the original circular Daybreak symbol in the header and rebuilt the
  reusable SVG variants from that exact mark.

## 0.4.0 - 2026-08-01

- Replaced the browser-only landing-card switch with a server-rendered rule tied
  to the active dataset.
- Reworked the sidebar as three expanding workflow cards with larger step
  numbers and one visible task at a time.
- Made each built-in example select the same starting method named in its note.
- Separated statistics from machine learning in the method directory and
  sidebar selector.
- Added classification and regression trees, classification and regression
  forests, and nearest-neighbor classification with held-out checks.
- Added a missing-data workspace with four choices, source restoration, and a
  full MICE-object download.
- Reduced the initial result density by collapsing the reading guide, formal
  interpretation, local rewrite, raw rows, and method details after a result.
- Added dark, transparent, and monochrome Daybreak SVG icons and used the dark
  variant in the application header and browser tab.
- Expanded Help with a concrete tidy-data example and guidance for repeated
  observations.
- Reworked method cards so their drawings remain inside a fixed visual region
  without touching labels.

## 0.3.0 - 2026-08-01

- Added a separate everyday explanation with a short answer, mental picture,
  uncertainty statement, clear boundary, and row-coverage note.
- Kept the formal statistical interpretation in a separate collapsible panel.
- Added optional local rewriting through Ollama. The model receives finished
  prose only, and its output is rejected if any number changes.
- Added guided macOS setup, local status checks, three model choices, and an
  in-app model download route with a Terminal fallback.
- Removed the upload and example radio buttons. Loading either source now
  updates one shared data state.
- Enlarged the sidebar step numbers.
- Rebuilt the method directory as larger square cards with high-contrast
  method-specific visuals above direct labels.
- Added the application version, R and Shiny links, copyright notice, and
  PolyForm Noncommercial License 1.0.0 to the sidebar footer.

## 0.2.4 - 2026-08-01

- Removed the delayed browser timer that could report a disconnected R session
  while Shiny was still connected. The warning now appears only after a
  confirmed disconnect event.
- Changed the example-data path so a loaded dataset immediately replaces the
  landing presentation with its summary and an open raw-data preview.
- Added an in-sidebar status that names the active dataset and reports its row
  and column counts.
- Stopped method changes from restoring the landing presentation over an
  already loaded dataset.
- Replaced the eight repeated family graphics with 43 distinct statistical
  illustrations, one for every method in the directory.

## 0.2.3 - 2026-08-02

- Moved the original landing presentation into the initial page structure so
  it appears before any server output is received.
- Kept the built-in example selector visible at all times and added a direct
  Load button.
- Added a connected-session indicator in the sidebar and a browser warning for
  an interrupted R session.
- Added compact statistical graphics to all 43 method cards. The graphics
  represent distributions, comparisons, relationships, counts, dimensions,
  latent structure, clustered data, survival, or ordered time.

## 0.2.2 - 2026-08-02

- Restored the original landing headline, copy, key points, orbit graphic, and
  estimate cards on the opening screen.
- Replaced the Help, Methods, and Data Formats server modals with page dialogs
  that open through ordinary links and CSS targets.
- Replaced the searchable sidebar method field with a plain HTML select that
  remains visible and usable without Selectize.
- Connected each categorized method card to the same sidebar selector and
  returned the reader to the column-assignment controls after a choice.
- Added an expanded raw-data preview that appears automatically after a
  built-in or uploaded dataset loads.
- Added a visible browser warning when the R session disconnects.
- Replaced bare `small()` calls with `tags$small()` and extended the markup
  regression check.

## 0.2.1 - 2026-08-02

- Restored the full opening page before a file or example has been loaded.
- Made personal file upload the opening data choice.
- Removed the R 4.3 startup stop that could leave the static shell visible
  while all server-backed controls remained blank or inert.
- Placed upload and sample controls directly in the page so the example picker
  does not depend on a preliminary server-rendered panel.
- Restored a visible method selector in the sidebar and retained a wide,
  categorized method window as an additional route.
- Replaced the method window's many server observers with one searchable choice
  and one confirmation action.
- Removed the full-screen analysis layer, which could cover navigation after a
  failed or interrupted run.
- Added opening-route checks for the default data source, sample selector,
  landing page, and supported R version.

## 0.2.0 - 2026-08-02

- Replaced the sidebar method dropdown with a wide, searchable method picker
  organized by statistical question.
- Added a raw-data viewer and full CSV download for built-in and uploaded data.
- Added an in-app Help guide with short instructions, result-reading notes,
  data guidance, and troubleshooting.
- Rebuilt the theme, appearance, palette, result-tab, and suggestion-card
  browser controls around one event system.
- Limited the full-screen progress layer to analysis runs so ordinary server
  updates do not cover the controls.
- Added a separate palette icon and theme-specific sun and moon icons.
- Expanded large dialogs and added responsive layouts for method cards, Help,
  and data previews.
- Removed a `plotOutput()` argument that failed on earlier Shiny releases.
- Replaced independent column defaults with role-aware choices that avoid
  duplicate outcome, time, ID, event, and control assignments.
- Allowed a mixed-model random slope to repeat a fixed predictor while checking
  that the slope also appears in the fixed part of the model.
- Corrected TikZ device cleanup and exported SEM syntax.
- Added package-aware tests for latent-variable, mixed, survival, factor,
  multinomial, ordinal, and negative-binomial models.

## 0.1.1 - 2026-08-02

- Corrected the decorative interval markup that caused the initial results
  screen to stop with a missing `i()` function error.
- Added a regression test for the interval markup.

## 0.1.0 - 2026-08-01

- Added 43 guided statistical analysis paths.
- Added deterministic plain-language reports and method-specific checks.
- Added nine built-in real datasets.
- Added common text, spreadsheet, statistical, R, JSON, and columnar imports.
- Added dark and light modes with four color-vision settings plus standard
  color.
- Added raster, vector, PSD, TikZ, report, table, code, and bundle downloads.
- Added calculation, catalog, accessibility, writing, and startup tests.
