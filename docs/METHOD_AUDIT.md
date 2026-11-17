# Calculation audit

Release: 1.0.0  
Review date: 2026-11-16

This table records the calculation path, the main data checks, and where its
regression test runs. “Base R” means the test needs nothing beyond base R and
the recommended packages, so it runs on any installation. “Package test” means
the test is included in `tests/testthat` and runs once `source("run_daybreak.R")`
has installed the named package.

| Method | Calculation | Main checks | Release test |
| --- | --- | --- | --- |
| Numeric summaries | Base summaries by variable and optional group | Numeric type, missing and all-missing groups | Base R |
| Counts and percentages | `table` with within-split denominators | Required columns, missing category retained | Base R |
| Missing data profile | Column and row-pattern counts | Nonempty data, required columns | Base R |
| One-sample t | `t.test` | Complete rows, variation, reference and confidence range | Base R against `t.test` |
| Independent t | Welch or pooled `t.test` | Two groups, two rows per group, variation | Base R |
| Paired t | Paired `t.test` | Complete pairs, change variation | Base R |
| Mann-Whitney | `wilcox.test` | Two groups, Hodges-Lehmann shift, interval fallback | Base R |
| Paired Wilcoxon | Paired `wilcox.test` | Complete pairs, Hodges-Lehmann paired shift | Base R |
| One-way ANOVA | `aov`, omega squared, `TukeyHSD` | Group sizes, model rank, residual degrees of freedom, any column name | Base R |
| Welch ANOVA | `oneway.test` | Group sizes and numeric outcome | Base R |
| Kruskal-Wallis | `kruskal.test` | Observed groups and numeric outcome | Base R |
| Repeated-measures ANOVA | `aov` with case and condition error strata, condition and case as categories, Greenhouse-Geisser epsilon | One row per case-condition cell, complete cases, any column name | Base R against `lm`, and against `car` for epsilon |
| Equivalence test | Two one-sided Welch `t.test` calls and the matching interval | Two groups, group sizes, positive margin, default margin flagged | Base R against `t.test` |
| Chi-square association | `chisq.test`, Cramer's V | Two categorical columns, expected counts | Base R |
| Fisher exact | `fisher.test` | Two categories, Monte Carlo size, isolated seed | Base R |
| McNemar | `mcnemar.test` | Two binary paired columns, discordant-pair count | Base R |
| One proportion | `prop.test` | Two outcome values, reference and confidence range | Base R with exported-code replay |
| Two proportions | `prop.test` | Binary outcome, two groups, cell counts | Base R |
| Factorial ANOVA/ANCOVA | `lm` and sequential `anova` | Factor levels, numeric covariates, rank, residual degrees | Base R |
| Friedman | `friedman.test` | One row per case-condition cell, complete blocks | Base R invalid-cell tests |
| Correlation matrix | `cor.test` by pair | Numeric variation, method and confidence range | Base R |
| Partial correlation | Residual correlation with adjusted t test and interval | Numeric controls, rank-first Spearman, adjusted degrees | Base R against manual calculation |
| Linear regression | `lm` | Complete rows, rank, residual degrees, Cook distance | Base R against `lm` |
| Moderation | `lm` with interaction | Focal and moderator variation, rank | Base R |
| Mediation | `lavaan::sem` with percentile bootstrap | Numeric columns, safe names, sample size, convergence, isolated seed | Package test |
| Binary logistic regression | Binomial `glm` | Two outcomes, rank, finite coefficients, smaller outcome count | Base R with exported-code replay |
| Multinomial logistic regression | `nnet::multinom` | Three outcomes, rank, category size, finite estimates | Package test |
| Ordinal logistic regression | `MASS::polr` | Explicit complete category order, rank, finite estimates | Package test |
| Poisson regression | Poisson `glm` | Whole nonnegative counts, positive exposure, rank, convergence | Base R |
| Negative binomial regression | `MASS::glm.nb` | Whole nonnegative counts, positive exposure, rank, convergence | Package test |
| MANOVA | `manova` with Pillai trace | Two varying outcomes, model rank, residual degrees | Base R |
| Principal components | Standardized `prcomp` | Two numeric columns, variation, component count | Base R; explained share sums to one |
| K-means | Standardized `kmeans` with 30 starts | Two columns, distinct profiles, cluster count, isolated seed | Base R with random-state test |
| Hierarchical clustering | Ward `hclust` and `cutree` | Two columns, variation, cluster count | Base R |
| Exploratory factor analysis | `psych::fa` | Three varying items, row count, factor count the items can identify, rotation | Package test |
| Scale reliability | Coefficient alpha and item-rest correlations | Two varying items, total-score variation | Base R against defining formula |
| Rater agreement | Cohen's kappa with Fleiss-Cohen-Everitt standard error, general weight matrix | Shared category set, at most 12 categories, rare-category check | Package test against `psych` and `vcd` |
| Confirmatory factor analysis | `lavaan::cfa` | Nonblank syntax, estimator and ordered-data pairing, convergence | Package test |
| Structural equation model | `lavaan::sem` | Nonblank syntax, estimator and ordered-data pairing, convergence | Package test |
| Hierarchical linear model | `lmerTest::lmer` | Three clusters, fixed rank, random-slope variation, convergence | Package test |
| Hierarchical logistic model | `lme4::glmer` | Three clusters, two outcomes, rank, slope variation, convergence | Package test |
| Multilevel growth model | Linear mixed model with time within unit | Numeric time, cluster count, slope variation, convergence | Package test |
| Kaplan-Meier | `survfit` and optional `survdiff` | Nonnegative time, event coding, total events, group event note | Package test |
| Cox proportional hazards | `coxph` and `cox.zph` | Nonnegative time, model rank, events beyond coefficient count | Package test |
| ARIMA | `arima` and Ljung-Box residual check | Unique regular time, p-d-q and frequency range, convergence | Base R, including calendar months |
| Random-effects meta-analysis | `metafor::rma` with REML and `predict` | Three studies, positive standard errors, study count and heterogeneity checks | Package test against the published BCG values |
| Classification tree | `rpart` | Stratified holdout, category size, settings, unseen factor levels | Package test |
| Regression tree | `rpart` | Numeric holdout, settings, unseen factor levels, finite predictions | Package test |
| Random forest classification | `ranger` | Stratified holdout, tree count, unseen factor levels | Package test |
| Random forest regression | `ranger` | Numeric holdout, tree count, finite predictions | Package test |
| K-nearest neighbors | Standardized `class::knn` | Stratified holdout, varying predictors, neighbor count, isolated ties | Package test |
| Elastic-net classification | `glmnet::cv.glmnet` | Stratified holdout, penalty mix, three or more folds | Package test |
| Elastic-net regression | `glmnet::cv.glmnet` | Numeric holdout, penalty mix, three or more folds | Package test |
| Naive Bayes | `e1071::naiveBayes` | Stratified holdout, varying predictors, nonnegative smoothing | Package test |
| DBSCAN | `dbscan::dbscan` | Two varying numeric columns, radius, nearby-point count | Package test |
| HDBSCAN | `dbscan::hdbscan` | Two varying numeric columns, minimum cluster size | Package test |

The suite also parses generated R code for every result it creates,
reopens a saved session, builds both writing layers, and checks all 56 method
illustrations. Package-gated tests do not silently skip on a normal Daybreak
installation because the launcher installs the full package list first.

Every method also runs again with its columns renamed to names that contain
spaces, quotes, backticks, and backslashes, and the numbers must match the
original run. Mediation refuses such names in plain words, because lavaan model
syntax cannot quote them. A further test exports scripts for a column name
written to break out of its quotes and checks that the script runs without
running the name.

The Bayes factor reported by seven methods is compared in the suite with a
direct call to `BayesFactor` for each design, and the weighing of the interval
and the Bayes factor is tested on data built to land in each verdict.

Two further checks cover the interface. One reads the browser script and the
server and fails when a message is sent that no handler receives, or when a
handler would be refused by Shiny. The other, `tests/browser-smoke.js`, loads
the script into a simulated page and exercises the controls.
