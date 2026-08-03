# Research notes and primary references

The method catalog was checked against current R documentation and the official
sites of the specialist packages. These notes record the sources that shaped
the current release.

## Statistical scope

- The [R `stats` package index](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html)
  covers t tests, analysis of variance, rank tests, contingency-table tests,
  linear and generalized linear models, MANOVA, PCA, factor analysis,
  clustering, and ARIMA used in the core engines.
- The [lavaan overview](https://lavaan.ugent.be/) documents path analysis,
  CFA, SEM, growth curves, and multilevel SEM. Daybreak uses lavaan for CFA,
  SEM, and bootstrap mediation.
- The [lavaan tutorial](https://lavaan.ugent.be/tutorial/) informed the model
  syntax field, estimator choice, ordered indicators, and fit tables.
- The [lme4 site](https://lme4.github.io/lme4/) documents linear and generalized
  linear mixed models with nested or crossed random effects. The guided forms
  in this release expose a random intercept and one optional random slope.
- The [R survival package manual](https://cran.r-project.org/package=survival)
  is the source for Kaplan-Meier, log-rank, Cox, and Schoenfeld-residual routes.

## Files and reports

- The [haven documentation](https://haven.tidyverse.org/) lists SPSS, Stata,
  and SAS formats read through ReadStat.
- The [R Markdown documentation](https://pkgs.rstudio.com/rmarkdown/reference/rmarkdown-package.html)
  documents HTML, Word, PDF, and other report conversions.
- The [ggplot2 `ggsave()` reference](https://ggplot2.tidyverse.org/reference/ggsave.html)
  documents raster and vector devices, dimensions, and dpi.
- The [tikzDevice manual](https://cran.r-project.org/web/packages/tikzDevice/tikzDevice.pdf)
  documents native TikZ graphics output and its TeX requirements.

## Machine learning and missing data

- The [tidymodels case study](https://www.tidymodels.org/start/case-study/)
  demonstrates separate training, validation, and test data and stratified
  splitting when outcome categories are uneven. Daybreak uses a stratified
  training and holdout split for category prediction.
- The [tidymodels performance article](https://www.tidymodels.org/learn/models/bootstrap-metrics/)
  treats out-of-sample data as the basis for empirical model assessment and
  explains why one validation set produces only one performance estimate.
  Daybreak therefore labels its holdout scores as one starting check.
- The official [`rpart` engine reference](https://parsnip.tidymodels.org/reference/details_decision_tree_rpart.html)
  documents CART classification and regression trees and their complexity,
  depth, and minimum-row controls.
- The official [`ranger` engine reference](https://parsnip.tidymodels.org/reference/details_rand_forest_ranger.html)
  documents random forests for classification and regression, including tree
  count and predictor-sampling defaults.
- The original [elastic-net paper](https://hastie.su.domains/Papers/B67.2%20%282005%29%20301-320%20Zou%20%26%20Hastie.pdf)
  describes the combined ridge and lasso penalty. The official
  [`glmnet` vignette](https://glmnet.stanford.edu/articles/glmnet.html)
  documents cross-validated penalty selection used by both added elastic-net
  paths.
- The official [naive Bayes model reference](https://parsnip.tidymodels.org/reference/naive_Bayes.html)
  and [`e1071` manual](https://cran.r-project.org/web/packages/e1071/e1071.pdf)
  document category prediction, conditional distributions, and Laplace
  smoothing.
- The original [DBSCAN paper](https://cdn.aaai.org/KDD/1996/KDD96-037.pdf)
  defines density-connected groups and noise. The official
  [`dbscan` package documentation](https://cran.r-project.org/web/packages/dbscan/readme/)
  documents DBSCAN and HDBSCAN in R.
- The [MICE documentation](https://amices.org/mice/) describes fully
  conditional specification, mixed variable types, several completed
  datasets, and diagnostic plots. Daybreak stores the full `mids` object and
  warns that a guided result from its first completed dataset is not pooled.
- The CRAN manuals for [missForest](https://cran.r-project.org/package=missForest)
  and [VIM](https://cran.r-project.org/package=VIM) document the random-forest
  and nearest-neighbor choices in the missing-data workspace.

## Accessibility

The interface was designed against the
[WCAG 2.2 quick reference](https://www.w3.org/WAI/WCAG22/quickref/), with
particular attention to text alternatives, color-independent meaning,
contrast, reflow, keyboard access, visible focus, labels, status messages, and
minimum target size.

## Everyday explanations

- The [ASA statement on p-values](https://www.tandfonline.com/doi/full/10.1080/00031305.2016.1154108)
  supports keeping probability statements restrained and separating evidence
  from practical importance.
- The [guide to common statistical misinterpretations](https://pmc.ncbi.nlm.nih.gov/articles/PMC4877414/)
  supports explicit statements about what an interval, test, or model cannot
  establish.
- A [randomized study of plain-language summaries](https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2021.771399/full)
  found better content knowledge when technical terms were replaced, rather
  than merely defined in a glossary. It also found value in a direct statement
  about evidence quality.
- A [systematic review of plain-language summaries](https://pmc.ncbi.nlm.nih.gov/articles/PMC9170105/)
  supports short sentences, familiar wording, and clear organization for
  non-specialist readers.

## Method directory visuals

- [Cleveland and McGill](https://www.tandfonline.com/doi/abs/10.1080/01621459.1984.10478080)
  provide the experimental basis for preferring position and length over less
  accurately judged visual encodings.
- A [review of effective data visualization](https://pmc.ncbi.nlm.nih.gov/articles/PMC7733875/)
  supports direct labels, high contrast, minimal visual competition, and
  matching the display to the reader's task.
- Research on [small multiples](https://www.cs.au.dk/~elm/pdf/multilinevis.pdf)
  supports repeated panels with a stable visual grammar for comparison.
- Research on [screen-reader access to data visualizations](https://vis.csail.mit.edu/pubs/rich-screen-reader-vis-experiences/)
  supports keeping meaningful text outside decorative graphics and preserving
  a clear reading order.

The method cards therefore use larger square panels, a high-contrast geometric
visual above the name and question, and a stable inner margin. Descriptive
words and values remain outside the miniature so they cannot collide with its
marks or compete with the method name.

## Optional local models

- The [Ollama API documentation](https://docs.ollama.com/api/introduction)
  states that the local API is served at `http://localhost:11434/api`.
- The current [generate endpoint](https://docs.ollama.com/api/generate)
  documents nonstreaming replies, the `think` switch, response settings, and
  model keep-alive time. Daybreak requests a short non-thinking reply for its
  plain-language restatement.
- The [macOS documentation](https://docs.ollama.com/macos) gives the supported
  installation path, while the [GPU documentation](https://docs.ollama.com/gpu)
  covers Apple Metal support.
- The listed model pages document the current downloads for
  [Qwen 3.5 4B](https://ollama.com/library/qwen3.5%3A4b),
  [Llama 3.2 3B](https://ollama.com/library/llama3.2%3A3b), and
  [Gemma 3 4B](https://ollama.com/library/gemma3%3A4b).

## Result hierarchy, charts, and interaction

- Shneiderman's peer-reviewed [visual information-seeking paper](https://www.cs.umd.edu/~ben/papers/Shneiderman1996eyes.pdf)
  proposes an overview first, followed by filtering and detail on request. The
  result screen now places the everyday answer, statistical description, and
  figure in one full-width order. Tables, checks, data rows, and files open
  below one section at a time.
- Cleveland and McGill's [graphical perception experiments](https://euclid.psych.yorku.ca/www/psy6135/papers/ClevelandMcGill1984.pdf)
  found that position on a common scale and length support more accurate
  quantitative judgments than area, angle, or color saturation. Daybreak keeps
  axes, intervals, dots, and lengths as its main quantitative marks.
- Bateman and colleagues' CHI study of [visual embellishment and chart memory](https://sites.stat.columbia.edu/gelman/communication/Bateman2010.pdf)
  found that recognizable visual form can aid preference and recall under the
  study conditions without lowering comprehension. The method miniatures use
  recognizable statistical forms, but no decorative copy, invented values,
  textures, or pictorial scenes.
- Tractinsky, Katz, and Ikar reported a strong relationship between perceived
  beauty and perceived usability in [their interface experiment](https://academic.oup.com/iwc/article-abstract/13/2/127/898608).
  Daybreak treats visual finish and task clarity as connected: one spacing
  rhythm, repeated panel geometry, a restrained palette, and a stable reading
  order are used throughout.
- [WCAG 2.2](https://www.w3.org/TR/WCAG22/) adds criteria for minimum target
  size, unobscured focus, consistent help, and other access needs. Interactive
  controls retain visible focus, 24-pixel-or-larger targets, keyboard tab
  movement, reduced-motion rules, reflow, and text outside decorative SVGs.
- Posit's official [Shiny state article](https://shiny.posit.co/r/articles/share/bookmarking-state/)
  describes URL and server bookmarking. Daybreak instead downloads a local
  session file because the app commonly runs on a personal computer and the
  active dataset must travel with the work. The file has a checked schema and
  an explicit notice that it contains the data.

The method drawings use the same 70 by 68 box, line weights, corner shapes, and
small color set. Density icons use circles and ellipses rather than loose blobs.
No SVG contains words or numeric labels, so chart marks cannot collide with
copy.

## Product decisions

- Question-based method groups reduce the need to know package names.
- Dark mode is the default because that was part of the product brief; light
  mode is a full token set rather than a filter.
- Protanopia and deuteranopia have separate controls even though both settings
  use blue-yellow-safe foundations.
- PSD is labeled as flattened. The statistical figure has no meaningful native
  Photoshop layer structure.
- Both reader-facing explanations are rule-based. A language model is not
  needed to turn a completed result into restrained wording.
- A local language model is optional and receives finished prose only. Its
  output is not shown when any number changes.
- Complex surveys, pooled MICE inference, meta-analysis, Bayesian models, and
  spatial models are documented as outside this release instead of appearing
  as partial menu items.
