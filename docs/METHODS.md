# Method directory

Daybreak 0.9.2 contains 53 paths. Statistical and machine-learning choices are
shown separately, then grouped by the question a user is asking rather than by
the R package that performs the computation.

## Describe

| Method | Use | Main output |
| --- | --- | --- |
| Numeric summaries | Describe numeric columns, with an optional group | N, missing, mean, SD, median, IQR, quartiles, range |
| Counts and percentages | Describe categorical columns, with an optional split | Counts, within-split percentages |
| Missing data profile | Locate missing cells and common patterns | Missing counts, percentages, patterns |

## Compare groups

| Method | Required structure | Main output |
| --- | --- | --- |
| One-sample t test | One numeric outcome and a reference mean | Mean contrast, interval, Cohen's d |
| Independent-samples t test | Numeric outcome and two independent groups | Welch or pooled contrast, interval, Hedges' g |
| Paired-samples t test | Two numeric columns from the same rows | Mean paired change, interval, Cohen's dz |
| Mann-Whitney test | Numeric or ordinal outcome and two independent groups | Rank statistic and Hodges-Lehmann location shift |
| Wilcoxon signed-rank test | Two paired numeric or ordinal columns | Signed-rank statistic and Hodges-Lehmann paired shift |
| One-way ANOVA | Numeric outcome and categorical group | F test, omega squared, Tukey comparisons |
| Welch one-way ANOVA | Numeric outcome and categorical group | Welch F test for unequal variances |
| Kruskal-Wallis test | Numeric or ordinal outcome and categorical group | Rank-based omnibus test |
| Repeated-measures ANOVA | Long data with outcome, condition, and case ID | Within-case omnibus test |
| Chi-square test | Two categorical columns | Chi-square, Cramer's V, expected counts, residuals |
| Fisher's exact test | Two categorical columns with small counts | Exact or Monte Carlo p-value |
| McNemar test | Two paired binary columns | Paired categorical change test |
| One-sample proportion test | One binary column and a reference proportion | Proportion, interval, and large-sample test |
| Two-sample proportion test | Binary outcome and two independent groups | Proportion difference and interval |
| Factorial ANOVA and ANCOVA | Numeric outcome, factors, optional numeric covariates | Term tests and partial eta squared |
| Friedman test | Long data with outcome, condition, and case ID | Rank-based repeated-measures test |

## Study relationships

| Method | Required structure | Main output |
| --- | --- | --- |
| Correlation matrix | Two or more numeric columns | Pearson, Spearman, or Kendall coefficients |
| Partial correlation | Two numeric columns and numeric controls | Residual correlation and interval |
| Moderation model | Numeric outcome, focal predictor, moderator | Interaction coefficient and fitted lines |
| Mediation model | Exposure, mediator, outcome, optional controls | Direct, indirect, and total paths with bootstrap intervals |

Mediation uses `lavaan`.

## Build a model

| Method | Outcome | Main output | Package |
| --- | --- | --- | --- |
| Linear regression | Numeric | Coefficients, intervals, R squared, residual checks | Base R |
| Binary logistic regression | Two categories | Odds ratios, intervals, fit, calibration | Base R |
| Multinomial logistic regression | Three or more unordered categories | Category-specific odds ratios | `nnet` |
| Ordinal logistic regression | Three or more ordered categories | Proportional-odds ratios | `MASS` |
| Poisson regression | Nonnegative count | Rate ratios and dispersion check | Base R |
| Negative binomial regression | Overdispersed nonnegative count | Rate ratios and theta | `MASS` |

Poisson and negative binomial models accept an optional positive exposure
column as a log offset.

Ordinal logistic regression displays every observed category in a low-to-high
list. The user can reorder that list before fitting; the reported proportional
odds depend on this order.

## Study several outcomes

| Method | Use | Main output |
| --- | --- | --- |
| MANOVA | Compare several numeric outcomes together | Pillai's trace and outcome-specific ANOVAs |

## Study scales and latent variables

| Method | Use | Main output | Package |
| --- | --- | --- | --- |
| Exploratory factor analysis | Explore common factors among numeric items | Loadings, communalities, RMSR, TLI, RMSEA | `psych` |
| Scale reliability | Study internal consistency among numeric items | Alpha, item-rest correlations, alpha if removed | Base R |
| Confirmatory factor analysis | Test a proposed measurement model | Standardized loadings and global fit | `lavaan` |
| Structural equation model | Test measurement and structural paths | Standardized paths and global fit | `lavaan` |

The CFA and SEM forms accept native `lavaan` syntax. Advanced users retain
direct control of latent-variable definitions, regressions, covariances,
estimators, ordered indicators, and missing-data handling.

## Study clustered or repeated data

| Method | Structure | Main output | Package |
| --- | --- | --- | --- |
| Hierarchical linear model | Numeric outcome within clusters | Fixed effects, random variance, singular-fit check | `lme4`, `lmerTest` |
| Hierarchical logistic model | Binary outcome within clusters | Conditional odds ratios and random variance | `lme4` |
| Multilevel growth model | Repeated numeric outcome over time within units | Time slope, optional random time slope, random variance | `lme4`, `lmerTest` |

Each mixed model includes a random intercept. A random slope can be added from
the form and must also appear among the fixed predictors. Crossed cluster terms
and three-level syntax are outside the guided form in this release.

## Study events over time

| Method | Structure | Main output | Package |
| --- | --- | --- | --- |
| Kaplan-Meier survival analysis | Time, event indicator, optional group | Survival curve, medians, log-rank test | `survival` |
| Cox proportional hazards model | Time, event indicator, predictors | Hazard ratios, concordance, Schoenfeld check | `survival` |
| Time-series trend and ARIMA | Ordered time and numeric outcome | ARIMA coefficients, fitted series, Ljung-Box check | Base R |

## Machine learning

Predictive paths divide complete rows into training and holdout portions. The
holdout rows are not used to fit the model. Their outcomes are used only to
report prediction performance. One split is a starting check, not a promise of
future performance.

### Predict a category

| Method | Main output | Package |
| --- | --- | --- |
| Classification tree | Holdout accuracy, balanced accuracy, confusion display | `rpart` |
| Random forest classification | Holdout accuracy, balanced accuracy, variable importance | `ranger` |
| K-nearest neighbors | Holdout accuracy, balanced accuracy, confusion display | `class` |
| Elastic-net classification | Cross-validated penalty, coefficients, holdout accuracy, confusion display | `glmnet` |
| Naive Bayes classification | Training shares, holdout accuracy, confusion display | `e1071` |

### Predict a number

| Method | Main output | Package |
| --- | --- | --- |
| Regression tree | Holdout RMSE, MAE, R squared, variable importance | `rpart` |
| Random forest regression | Holdout RMSE, MAE, R squared, variable importance | `ranger` |
| Elastic-net regression | Cross-validated penalty, coefficients, holdout RMSE, MAE, R squared | `glmnet` |

### Find groups

| Method | Main output | Package |
| --- | --- | --- |
| K-means clustering | Cluster sizes, centers, two-component display | Base R |
| Hierarchical clustering | Cluster sizes and two-component display | Base R |
| DBSCAN clustering | Density-based groups, noise rows, two-component display | `dbscan` |
| HDBSCAN clustering | Density-based groups, membership strength, noise rows, two-component display | `dbscan` |

### Reduce dimensions

| Method | Main output |
| --- | --- |
| Principal components analysis | Explained variance, scores, loadings |

## Method-selection cautions

- A nonparametric test changes the target of inference; it is not merely an
  ANOVA or t test without normality.
- A model check that does not flag a problem is not proof that every assumption
  is satisfied.
- Automated variable types are suggestions. Analysts should confirm category
  order, event coding, exposure units, case IDs, and time order.
- Repeated-measures ANOVA and Friedman analysis require one row per case and
  condition. ARIMA requires one observation at each equally spaced time point.
- Listwise deletion is used by most guided analyses. CFA and SEM can use
  full-information maximum likelihood when the selected estimator permits it.
- The missing-data workspace can create a working copy by four methods. MICE
  stores every completed dataset in a downloadable R object, but guided
  inferential analyses use the first completed dataset and do not pool results.
- Statistical models do not establish causation by themselves.
