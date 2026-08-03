# Calculation audit

Release: 0.9.2  
Review date: 2026-08-02

This table records the calculation path, the main data checks, and where its
regression test runs. “Portable” means the test ran in WebAssembly R during the
release review. “Mac package test” means the test is included in
`tests/testthat` and runs after `source("run_daybreak.R")` installs the named
package.

| Method | Calculation | Main checks | Release test |
| --- | --- | --- | --- |
| Numeric summaries | Base summaries by variable and optional group | Numeric type, missing and all-missing groups | Portable |
| Counts and percentages | `table` with within-split denominators | Required columns, missing category retained | Portable |
| Missing data profile | Column and row-pattern counts | Nonempty data, required columns | Portable |
| One-sample t | `t.test` | Complete rows, variation, reference and confidence range | Portable against `t.test` |
| Independent t | Welch or pooled `t.test` | Two groups, two rows per group, variation | Portable |
| Paired t | Paired `t.test` | Complete pairs, change variation | Portable |
| Mann-Whitney | `wilcox.test` | Two groups, Hodges-Lehmann shift, interval fallback | Portable |
| Paired Wilcoxon | Paired `wilcox.test` | Complete pairs, Hodges-Lehmann paired shift | Portable |
| One-way ANOVA | `aov`, omega squared, `TukeyHSD` | Group sizes, model rank, residual degrees of freedom | Portable |
| Welch ANOVA | `oneway.test` | Group sizes and numeric outcome | Portable |
| Kruskal-Wallis | `kruskal.test` | Observed groups and numeric outcome | Portable |
| Repeated-measures ANOVA | `aov` with case and condition error strata | One row per case-condition cell, complete blocks | Portable invalid-cell tests |
| Chi-square association | `chisq.test`, Cramer's V | Two categorical columns, expected counts | Portable |
| Fisher exact | `fisher.test` | Two categories, Monte Carlo size, isolated seed | Portable |
| McNemar | `mcnemar.test` | Two binary paired columns, discordant-pair count | Portable |
| One proportion | `prop.test` | Two outcome values, reference and confidence range | Portable with exported-code replay |
| Two proportions | `prop.test` | Binary outcome, two groups, cell counts | Portable |
| Factorial ANOVA/ANCOVA | `lm` and sequential `anova` | Factor levels, numeric covariates, rank, residual degrees | Portable |
| Friedman | `friedman.test` | One row per case-condition cell, complete blocks | Portable invalid-cell tests |
| Correlation matrix | `cor.test` by pair | Numeric variation, method and confidence range | Portable |
| Partial correlation | Residual correlation with adjusted t test and interval | Numeric controls, rank-first Spearman, adjusted degrees | Portable against manual calculation |
| Linear regression | `lm` | Complete rows, rank, residual degrees, Cook distance | Portable against `lm` |
| Moderation | `lm` with interaction | Focal and moderator variation, rank | Portable |
| Mediation | `lavaan::sem` with percentile bootstrap | Numeric columns, safe names, sample size, convergence, isolated seed | Mac package test |
| Binary logistic regression | Binomial `glm` | Two outcomes, rank, finite coefficients, smaller outcome count | Portable with exported-code replay |
| Multinomial logistic regression | `nnet::multinom` | Three outcomes, rank, category size, finite estimates | Mac package test |
| Ordinal logistic regression | `MASS::polr` | Explicit complete category order, rank, finite estimates | Mac package test |
| Poisson regression | Poisson `glm` | Whole nonnegative counts, positive exposure, rank, convergence | Portable |
| Negative binomial regression | `MASS::glm.nb` | Whole nonnegative counts, positive exposure, rank, convergence | Mac package test |
| MANOVA | `manova` with Pillai trace | Two varying outcomes, model rank, residual degrees | Portable |
| Principal components | Standardized `prcomp` | Two numeric columns, variation, component count | Portable; explained share sums to one |
| K-means | Standardized `kmeans` with 30 starts | Two columns, distinct profiles, cluster count, isolated seed | Portable with random-state test |
| Hierarchical clustering | Ward `hclust` and `cutree` | Two columns, variation, cluster count | Portable |
| Exploratory factor analysis | `psych::fa` | Three varying items, row and factor count, rotation | Mac package test |
| Scale reliability | Coefficient alpha and item-rest correlations | Two varying items, total-score variation | Portable against defining formula |
| Confirmatory factor analysis | `lavaan::cfa` | Nonblank syntax, estimator and ordered-data pairing, convergence | Mac package test |
| Structural equation model | `lavaan::sem` | Nonblank syntax, estimator and ordered-data pairing, convergence | Mac package test |
| Hierarchical linear model | `lmerTest::lmer` | Three clusters, fixed rank, random-slope variation, convergence | Mac package test |
| Hierarchical logistic model | `lme4::glmer` | Three clusters, two outcomes, rank, slope variation, convergence | Mac package test |
| Multilevel growth model | Linear mixed model with time within unit | Numeric time, cluster count, slope variation, convergence | Mac package test |
| Kaplan-Meier | `survfit` and optional `survdiff` | Nonnegative time, event coding, total events, group event note | Mac package test |
| Cox proportional hazards | `coxph` and `cox.zph` | Nonnegative time, model rank, events beyond coefficient count | Mac package test |
| ARIMA | `arima` and Ljung-Box residual check | Unique regular time, p-d-q and frequency range, convergence | Portable, including calendar months |
| Classification tree | `rpart` | Stratified holdout, category size, settings, unseen factor levels | Mac package test |
| Regression tree | `rpart` | Numeric holdout, settings, unseen factor levels, finite predictions | Mac package test |
| Random forest classification | `ranger` | Stratified holdout, tree count, unseen factor levels | Mac package test |
| Random forest regression | `ranger` | Numeric holdout, tree count, finite predictions | Mac package test |
| K-nearest neighbors | Standardized `class::knn` | Stratified holdout, varying predictors, neighbor count, isolated ties | Mac package test |
| Elastic-net classification | `glmnet::cv.glmnet` | Stratified holdout, penalty mix, three or more folds | Mac package test |
| Elastic-net regression | `glmnet::cv.glmnet` | Numeric holdout, penalty mix, three or more folds | Mac package test |
| Naive Bayes | `e1071::naiveBayes` | Stratified holdout, varying predictors, nonnegative smoothing | Mac package test |
| DBSCAN | `dbscan::dbscan` | Two varying numeric columns, radius, nearby-point count | Mac package test |
| HDBSCAN | `dbscan::hdbscan` | Two varying numeric columns, minimum cluster size | Mac package test |

The portable suite also parses generated R code for every result it creates,
reopens a saved session, builds both writing layers, and checks all 53 method
illustrations. Package-gated tests do not silently skip on a normal Daybreak
installation because the launcher installs the full package list first.
