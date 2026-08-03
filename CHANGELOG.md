# Changes

## 0.10.0 - 2026-08-02

### Fixed

- Defined `DAYBREAK_VERSION` in a new `R/version.R` and removed the differently
  named local variable in `app.R`. Four files referred to a constant that was
  never defined, so the Markdown, HTML, Word, and PDF reports, the session file,
  and the analysis bundle all stopped at the point of use.
- Corrected the coding of the `lung` sample. `survival::veteran` records status
  as 0 for censored and 1 for dead, and the previous mapping of levels 1 and 2
  relabelled all 128 deaths as censored and turned the 9 censored rows into
  missing values. The column then held a single observed level and never
  appeared in the event status selector, so the survival example could not run.
- Named the confidence level that was actually used. Interval bounds honored
  the setting but every sentence described the result as a 95 percent interval.
- Gave the statistical description and the everyday explanation one shared
  coefficient selector. The two paragraphs could headline different predictors
  from the same model, and on a ratio scale the previous rule could never
  select a strong protective association.
- Rebuilt the chart palettes for each theme. A single dark-tuned set was used
  in both modes, so in light mode the standard gold measured 1.6 to 1 against
  the plot background and the monochrome ramp measured 1.1 to 1.
- Replaced the monochrome accents whose contrast measured 2.96 and 1.94 to 1
  against the page. The palette offered for readers who cannot rely on hue was
  the only one that failed.
- Matched the displayed interval to the exported script. Linear models now use
  t quantiles, which is what `confint(fit)` produces in the script the reader is
  told to run.
- Stopped a paired binary test from accepting two columns with different
  response categories.
- Corrected the moderation figure when the focal predictor is categorical, and
  drew the fitted model rather than a loess smoother in the growth figure.
- Read SPSS user-missing codes as missing values instead of ordinary categories.
- Computed coefficient alpha from one consistent set of complete rows.
- Restored the working directory before removing it in the bundle writer, and
  resolved the report template from a recorded path rather than from whatever
  the working directory happened to be.
- Raised six interactive controls to the 44 pixel minimum, and corrected the
  light mode secondary text color, which measured 3.39 to 1.
- Added a focus trap to the modal dialogs, moved the data-fit reason into the
  option label so a screen reader announces it, and completed the tab and panel
  relationships.
- Guarded the browser setup so it runs once per page rather than on every
  reconnect.
- Stopped an absent optional package from blocking startup. Only the six core
  packages are now required to open the app.
- Reported prediction outcome balance from the full analysis set rather than
  from the holdout rows alone.
- Made the plain-language helpers tolerate absent extras, so a session written
  by an earlier release reopens instead of failing.
- Rewrote the instructions sent to an optional local model. The earlier prompt
  asked the model to explain the comparison with a concrete mental picture,
  which is an invitation to invent one: an iris comparison came back described
  as three piles of sand measured for height. The model is now given the picture
  the application already produced and told to simplify its wording, with the
  five section headings, a sentence budget, and a list of the wordings that went
  wrong. The rewrite is also checked for structure now, not only for numbers, so
  a reply that collapses the sections is discarded.
- Corrected the moderation model, which did not center its numeric predictors
  despite a check row that assumed it did. Each lower-order coefficient
  described the outcome where the other predictor was zero, which for horsepower
  or weight is not a car. The product term and its p-value are unchanged.
- Wrapped the tree fit in the seed helper. rpart draws from the random stream
  during its own cross-validation, so results depended on ambient session state
  and running an analysis quietly moved the reader's random stream.
- Allowed an optional role to be left empty. Summarising a file without grouping
  it, which is the first thing most people do here, stopped with "undefined
  columns selected".
- Replaced the survival summary column names. The table shipped the survival
  package's own headings, including n.max, n.start, and se(rmean), in an
  application whose premise is statistics you can read. Values are unchanged and
  the interval bounds now name the level that produced them.
- Moved Pillai's trace into the tests table. MANOVA was the only method that put
  its omnibus test under estimates, which placed the one number the reader
  wanted beneath a heading saying it was something else.
- Stated the event value in the survival checks table. Which value counts as the
  event is the easiest thing to get backwards in a survival analysis, and
  getting it backwards inverts the curve without producing any error.
- Corrected four stylesheet rules that targeted class names the markup never
  emitted.

### Added

- ARIMA forecasting. The everyday explanation described a forecast band that
  was never drawn, so the engine now produces one, with an interval at the
  selected confidence level and a horizon capped at a quarter of the recorded
  length. Monthly series continue on calendar months.
- A notice above a result whose settings have moved on since it was produced,
  with a control to run the analysis again.
- Progress reporting while an analysis runs.
- Command or Control with Return to run the analysis from anywhere on the page.
- `tests/testthat/test-exports.R`, covering the six download paths. `R/exports.R`
  had no test file, which is how the missing constant reached a release.
- `tests/testthat/test-contrast.R`, which computes WCAG relative luminance for
  every palette against every surface it can be drawn on, and checks the touch
  target minimum in the stylesheet.
- An answer panel at the top of every result, carrying the finding once at
  display size with a verdict of clear, not clear, no evidence measure, or
  summary. The verdict comes from the same function the prose uses, so it is a
  second reading of one judgment rather than a new one.
- Quick-start cards on the opening screen. Each loads an example with its
  columns already assigned and runs it, so a first-time reader sees a finished
  result before learning what any control does.
- A working indicator while Shiny recalculates, and empty states for the panels
  that can legitimately have nothing to show.
- A type and spacing scale, tabular figures wherever numbers are compared down a
  column, and two shadow levels rather than one.

### Changed

- Removed the definition of `DAYBREAK_VERSION` from the test helper. The helper
  had supplied a constant the running application never defined, so the export
  and session tests passed against a value that did not exist at run time.
- Corrected five test assertions that were checking for text the application no
  longer contained.
- Stated the version in every exported document rather than in Markdown alone.
- Raised comment density from under 1 percent to about 10 percent, describing
  the reasoning behind choices rather than restating the code.
- Reworded large ratios in the everyday explanation. An odds ratio of 27.3 read
  as "about 2,628.9% higher" and now reads as a multiple.
- Described categorical coefficients as category contrasts rather than as
  one-unit increases.
- Rewrote the data-fit message, which was not a grammatical sentence.

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
