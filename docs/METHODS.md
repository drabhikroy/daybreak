# Method directory

Daybreak 1.0.0 contains 56 methods. Statistical and machine-learning choices
are shown separately, then grouped by the question a user is asking and not by
the R package that performs the computation. In the app, Help me choose reaches
the same methods from a plain description of the situation.

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
| Repeated-measures ANOVA | Long data with outcome, condition, and case ID | Within-case test, Greenhouse-Geisser correction, partial eta squared |
| Equivalence test | Numeric outcome, two independent groups, and a margin | Two one-sided Welch tests and the interval set against the margin |
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
| Rater agreement | Two columns of category ratings for the same cases | Cohen's kappa, optional linear or quadratic weights, interval, observed and chance agreement | Base R |
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

## Combine studies

| Method | Structure | Main output | Package |
| --- | --- | --- | --- |
| Random-effects meta-analysis | One effect and its standard error per study, optional study label | Pooled effect, interval, prediction interval, tau squared, I squared, Q test, forest plot | `metafor` |

Effects are pooled on the scale they are entered on. Ratios belong on the log
scale, with standard errors on the same scale.

## A second evidence check

Seven methods report a Bayes factor beside the classical result: the
one-sample, independent, and paired t tests, one-way ANOVA, the chi-square
test, the one-proportion test, and Pearson correlation. Each uses the default
prior of the `BayesFactor` package for that design. The verdict at the top of a
result weighs the interval and the Bayes factor together, as follows.

| Interval | Bayes factor | Verdict |
| --- | --- | --- |
| Excludes no effect | 3 or more, or not available | Clear signal |
| Excludes no effect | Below 3 | Tentative signal |
| Includes no effect | Between 1/3 and 3, or not available | Not clear |
| Includes no effect | 1/3 or below | Points to no effect |

The verbal bands for strength follow Jeffreys (1961) as restated by Lee and
Wagenmakers (2013): anecdotal below 3, moderate from 3, strong from 10, very
strong from 30, and extreme from 100.

## Sources

- Cohen, J. (1960). A coefficient of agreement for nominal scales. *Educational and Psychological Measurement, 20*(1), 37-46.
- Cohen, J. (1968). Weighted kappa: Nominal scale agreement with provision for scaled disagreement or partial credit. *Psychological Bulletin, 70*(4), 213-220.
- Colditz, G. A., Brewer, T. F., Berkey, C. S., Wilson, M. E., Burdick, E., Fineberg, H. V., & Mosteller, F. (1994). Efficacy of BCG vaccine in the prevention of tuberculosis. *JAMA, 271*(9), 698-702.
- Fleiss, J. L., Cohen, J., & Everitt, B. S. (1969). Large sample standard errors of kappa and weighted kappa. *Psychological Bulletin, 72*(5), 323-327.
- Greenhouse, S. W., & Geisser, S. (1959). On methods in the analysis of profile data. *Psychometrika, 24*(2), 95-112.
- Gunel, E., & Dickey, J. (1974). Bayes factors for independence in contingency tables. *Biometrika, 61*(3), 545-557.
- IntHout, J., Ioannidis, J. P. A., Rovers, M. M., & Goeman, J. J. (2016). Plea for routinely presenting prediction intervals in meta-analysis. *BMJ Open, 6*(7), e010247.
- Lakens, D. (2017). Equivalence tests: A practical primer for t tests, correlations, and meta-analyses. *Social Psychological and Personality Science, 8*(4), 355-362.
- Landis, J. R., & Koch, G. G. (1977). The measurement of observer agreement for categorical data. *Biometrics, 33*(1), 159-174.
- Ly, A., Verhagen, J., & Wagenmakers, E.-J. (2016). Harold Jeffreys's default Bayes factor hypothesis tests: Explanation, extension, and application in psychology. *Journal of Mathematical Psychology, 72*, 19-32.
- Rouder, J. N., Morey, R. D., Speckman, P. L., & Province, J. M. (2012). Default Bayes factors for ANOVA designs. *Journal of Mathematical Psychology, 56*(5), 356-374.
- Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. *Psychonomic Bulletin & Review, 16*(2), 225-237.
- Schuirmann, D. J. (1987). A comparison of the two one-sided tests procedure and the power approach for assessing the equivalence of average bioavailability. *Journal of Pharmacokinetics and Biopharmaceutics, 15*(6), 657-680.
- Viechtbauer, W. (2005). Bias and efficiency of meta-analytic variance estimators in the random-effects model. *Journal of Educational and Behavioral Statistics, 30*(3), 261-293.
- Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. *Journal of Statistical Software, 36*(3), 1-48.
- Wasserstein, R. L., & Lazar, N. A. (2016). The ASA statement on p-values: Context, process, and purpose. *The American Statistician, 70*(2), 129-133.

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
- An equivalence result holds only for the margin it was tested against. The
  margin should be chosen on subject-matter grounds before the result is seen.
- Kappa measures agreement between raters and not the accuracy of either one,
  and it falls when one category dominates.
- A pooled effect does not correct for publication bias or for flaws the
  studies share.
- A Bayes factor depends on its prior. The values here use package defaults
  and are a second reading of the data, not a replacement for the estimate and
  its interval.
