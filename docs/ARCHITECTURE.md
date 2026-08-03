# Architecture

Daybreak is a single-process Shiny application with eleven boundaries.

1. `catalog.R` states what methods exist and what column roles they require.
2. `data_io.R` reads files and converts labelled columns into explicit factors.
3. `imputation.R` prepares a recoverable working copy and leaves the loaded
   data object available for restoration.
4. The core, advanced, and machine-learning engine files compute results and
   return one result-record shape.
5. `reporting.R` reads completed values and assembles the statistical
   description. It does not fit models.
6. `explanations.R` reads the same completed values and writes the everyday
   explanation for a novice reader.
7. `session_state.R` writes and checks portable workspace files. Fitted model
   objects are omitted, while data, choices, result tables, plot data, prose,
   and display colors are retained.
8. `local_models.R` reads Ollama discovery and generation responses, can pass
   only the finished everyday text to a model on the same computer, and rejects
   output when its numeric check finds a changed value.
9. `plots.R` reads the result record and creates one ggplot object.
10. `exports.R` writes reports, figures, tables, scripts, and bundles.
11. `ui_helpers.R`, the stylesheet, and the browser script present the same
    state through compact cards, dialogs, and downloadable panels.

The interface does not contain statistical formulas. The engines do not
contain HTML. This keeps statistical review separate from interface review.

## Result record

Every engine returns these fields:

| Field | Contents |
| --- | --- |
| `method` | Catalog ID |
| `n_used`, `n_omitted` | Analysis coverage |
| `estimates` | Coefficients, contrasts, effect sizes, or loadings |
| `tests` | Omnibus or focused hypothesis tests |
| `fit` | Fit indices and model summaries |
| `checks` | Named findings with `ok`, `warn`, or `note` state |
| `model` | Fitted R object when one exists |
| `plot_kind`, `plot_data` | Chart route and prepared values |
| `extras` | Method-specific tables and metadata |
| `code` | Reproduction commands |
| `warnings` | Additional notes for report readers |

## Adding a method

1. Add one catalog entry with roles and packages.
2. Add `analyse_<id>()` to the appropriate engine file.
3. Return `result_record()` with at least coverage and one output table.
4. Add or reuse a plot route.
5. Add reporting and everyday branches only when the shared branch is not
   adequate.
6. Add a calculation test with a known answer and prose tests for both readings.
7. Add the method to `docs/METHODS.md`.

## Failure behavior

Imports and analyses use explicit errors written for the person operating the
app. The complete package list is checked before the interface starts, even
when `app.R` is opened directly. A method also checks its packages immediately
before fitting as a second line of defense. Invalid role choices remain in the
session so the user can correct one field without starting over.

## Privacy

The application contains no analytics script, remote model API, remote
database, or upload endpoint other than the Shiny session itself. The optional
Ollama API is bound to `127.0.0.1` and receives finished prose, not raw rows.
Deployed copies inherit the privacy and retention properties of their hosting
service, so deployment documentation should state those properties separately.

## Session files

The `.daybreak.rds` format is a compressed RDS list with a format marker and an
integer schema. Opening a file requires the marker, supported schema, a data
frame with unique column names, a catalog method, list-shaped roles and
settings, and a result whose method matches the saved choice. Daybreak never
uses `load()` for these files. An unrelated or incomplete RDS object is refused
before the active workspace changes.
