# Analysis catalog ---------------------------------------------------------
#
# This file is the single source for the method picker, variable forms,
# package checks, and method directory. Adding a method starts here.

# The method catalog -------------------------------------------------------
#
# Every method is described here as data: what it is called, which roles it
# needs, which packages it requires, and which engine runs it. Nothing in this
# file computes anything. Keeping the description separate from the statistics
# is what lets the interface build the form, the feasibility hints, and the
# method directory without knowing anything about how a method works.
role <- function(id, label, type, help, required = TRUE) {
  list(id = id, label = label, type = type, help = help, required = required)
}

method <- function(id, name, family, question, description, roles,
                   engine = id, requires = character(0), level = "Core",
                   kind = "statistics") {
  list(
    id = id,
    name = name,
    family = family,
    question = question,
    description = description,
    roles = roles,
    engine = engine,
    requires = requires,
    level = level,
    kind = kind
  )
}

r_num_one <- function(id = "outcome", label = "Numeric outcome")
  role(id, label, "numeric_one", "Choose one numeric column.")
r_num_many <- function(id = "variables", label = "Numeric variables")
  role(id, label, "numeric_many", "Choose two or more numeric columns.")
r_cat_one <- function(id = "group", label = "Group")
  role(id, label, "categorical_one", "Choose a categorical column.")
r_cat_optional <- function(id = "group", label = "Group")
  role(id, label, "categorical_optional", "Optional. Split the result by this column.", FALSE)
r_predictors <- function(id = "predictors", label = "Predictors")
  role(id, label, "predictors", "Choose one or more predictor columns.")

ANALYSIS_FAMILIES <- c(
  "Describe" = "Describe",
  "Compare groups" = "Compare groups",
  "Study relationships" = "Study relationships",
  "Build a model" = "Build a model",
  "Study several outcomes" = "Study several outcomes",
  "Study scales and latent variables" = "Study scales and latent variables",
  "Study clustered or repeated data" = "Study clustered or repeated data",
  "Study events over time" = "Study events over time"
)

# The two selector groupings. Everything above the machine learning families is
# offered as a question about the data at hand; everything within them is
# offered as a question about a rule that might generalize.
MACHINE_LEARNING_FAMILIES <- c(
  "Predict a category" = "Predict a category",
  "Predict a number" = "Predict a number",
  "Find groups" = "Find groups",
  "Reduce dimensions" = "Reduce dimensions"
)

# The methods themselves. Each entry states the question it answers in the
# reader's words, not in statistical vocabulary, because the selector is
# grouped by question and that phrasing is what a reader scans. The description
# is what appears in the method directory, and the roles drive the form.
#
# Order matters here: methods appear in the selector in the order they are
# listed within their family, so the simpler methods come first.
ANALYSES <- list(
  descriptives = method(
    "descriptives", "Numeric summaries", "Describe",
    "What does a numeric variable look like?",
    "Reports sample size, missing values, center, spread, range, and quartiles.",
    list(r_num_many(), r_cat_optional())
  ),
  frequencies = method(
    "frequencies", "Counts and percentages", "Describe",
    "How common is each category?",
    "Reports counts and percentages for one or more categorical variables.",
    list(role("variables", "Categorical variables", "categorical_many",
              "Choose one or more categorical columns."), r_cat_optional("split", "Split by"))
  ),
  missingness = method(
    "missingness", "Missing data profile", "Describe",
    "Where is data missing?",
    "Shows missing counts, percentages, and the most common missing-data patterns.",
    list(role("variables", "Variables", "any_many", "Choose the columns to inspect."))
  ),
  # Comparing groups. The parametric and rank-based versions sit next to each
  # other on purpose, so a reader who has been told their data are skewed can
  # find the alternative without leaving the family.
  one_sample_t = method(
    "one_sample_t", "One-sample t test", "Compare groups",
    "Does one mean differ from a reference value?",
    "Compares one sample mean with a value you provide and reports a confidence interval and standardized difference.",
    list(r_num_one()), level = "Core"
  ),
  independent_t = method(
    "independent_t", "Independent-samples t test", "Compare groups",
    "Do two independent groups have different means?",
    "Uses Welch's test by default, with a mean difference, confidence interval, and Hedges' g.",
    list(r_num_one(), r_cat_one()), level = "Core"
  ),
  paired_t = method(
    "paired_t", "Paired-samples t test", "Compare groups",
    "Did paired measurements change on average?",
    "Compares two measurements from the same cases and reports the mean paired change.",
    list(role("before", "First measurement", "numeric_one", "Choose the earlier or first measure."),
         role("after", "Second measurement", "numeric_one", "Choose the later or second measure."))
  ),
  mann_whitney = method(
    "mann_whitney", "Mann-Whitney test", "Compare groups",
    "Do two independent groups tend to differ in rank?",
    "A rank-based comparison for two independent groups. It does not require normally distributed scores.",
    list(r_num_one(), r_cat_one())
  ),
  paired_wilcoxon = method(
    "paired_wilcoxon", "Wilcoxon signed-rank test", "Compare groups",
    "Do paired measurements tend to change?",
    "A rank-based comparison for two paired measurements.",
    list(role("before", "First measurement", "numeric_one", "Choose the earlier or first measure."),
         role("after", "Second measurement", "numeric_one", "Choose the later or second measure."))
  ),
  one_way_anova = method(
    "one_way_anova", "One-way ANOVA", "Compare groups",
    "Do three or more groups have different means?",
    "Compares group means, reports omega squared, and provides multiplicity-adjusted pairwise comparisons.",
    list(r_num_one(), r_cat_one())
  ),
  welch_anova = method(
    "welch_anova", "Welch one-way ANOVA", "Compare groups",
    "Do group means differ when variances may be unequal?",
    "A mean comparison that does not require equal group variances.",
    list(r_num_one(), r_cat_one())
  ),
  kruskal_wallis = method(
    "kruskal_wallis", "Kruskal-Wallis test", "Compare groups",
    "Do three or more groups tend to differ in rank?",
    "A rank-based comparison across several independent groups.",
    list(r_num_one(), r_cat_one())
  ),
  repeated_anova = method(
    "repeated_anova", "Repeated-measures ANOVA", "Compare groups",
    "Does a numeric outcome change across repeated conditions?",
    "Compares repeated measurements stored in long form, with one row per person and condition.",
    list(r_num_one(), role("within", "Repeated condition", "categorical_one", "Choose the within-person condition."),
         role("id", "Case ID", "id_one", "Choose the column that identifies each person or unit.")),
    level = "Advanced"
  ),
  # Categorical outcomes. Fisher and McNemar are here rather than under their
  # own heading, because a reader asking about a table does not yet know which
  # of the three they need.
  chi_square = method(
    "chi_square", "Chi-square test of association", "Compare groups",
    "Are two categorical variables associated?",
    "Compares observed and expected cell counts and reports Cramer's V.",
    list(r_cat_one("row", "Row variable"), r_cat_one("column", "Column variable"))
  ),
  fisher_exact = method(
    "fisher_exact", "Fisher's exact test", "Compare groups",
    "Are two categorical variables associated when counts are small?",
    "Computes an exact test for a contingency table. A simulation is used for tables larger than 2 by 2.",
    list(r_cat_one("row", "Row variable"), r_cat_one("column", "Column variable"))
  ),
  mcnemar = method(
    "mcnemar", "McNemar test", "Compare groups",
    "Did a paired binary response change?",
    "Compares two binary measurements from the same cases.",
    list(r_cat_one("before", "First binary measurement"), r_cat_one("after", "Second binary measurement"))
  ),
  one_proportion = method(
    "one_proportion", "One-sample proportion test", "Compare groups",
    "Does one observed proportion differ from a reference proportion?",
    "Compares the proportion in the second displayed category with a reference value and reports a confidence interval.",
    list(role("outcome", "Binary outcome", "binary_one", "Choose a column with exactly two observed values."))
  ),
  two_proportions = method(
    "two_proportions", "Two-sample proportion test", "Compare groups",
    "Do two independent groups have different event proportions?",
    "Compares the second displayed outcome category across two groups and reports a confidence interval for the difference.",
    list(role("outcome", "Binary outcome", "binary_one", "Choose a column with exactly two observed values."), r_cat_one())
  ),
  factorial_anova = method(
    "factorial_anova", "Factorial ANOVA and ANCOVA", "Compare groups",
    "How do several factors and covariates relate to a numeric outcome?",
    "Fits main effects, an optional two-factor interaction, and optional numeric covariates.",
    list(r_num_one(), role("factors", "Categorical factors", "categorical_many", "Choose one or more categorical predictors."),
         role("covariates", "Numeric covariates", "numeric_many_optional", "Optional numeric control variables.", FALSE))
  ),
  friedman = method(
    "friedman", "Friedman test", "Compare groups",
    "Do repeated conditions tend to differ in rank?",
    "A rank-based repeated-measures comparison for long-form data.",
    list(r_num_one(), role("within", "Repeated condition", "categorical_one", "Choose the within-case condition."),
         role("id", "Case ID", "id_one", "Choose the column that identifies each case."))
  ),
  correlation = method(
    "correlation", "Correlation matrix", "Study relationships",
    "How strongly do numeric variables move together?",
    "Reports Pearson, Spearman, or Kendall correlations with confidence intervals where available.",
    list(r_num_many())
  ),
  partial_correlation = method(
    "partial_correlation", "Partial correlation", "Study relationships",
    "How are two variables related after accounting for others?",
    "Correlates residuals after removing the selected control variables.",
    list(r_num_one("x", "First variable"), r_num_one("y", "Second variable"),
         role("controls", "Control variables", "numeric_many_optional", "Choose one or more numeric controls."))
  ),
  # Model families. Each of these returns coefficients, and the reporting layer
  # exponentiates the ones whose coefficients are logs before showing them.
  linear_regression = method(
    "linear_regression", "Linear regression", "Build a model",
    "How is a numeric outcome related to several predictors?",
    "Reports coefficients, confidence intervals, model fit, residual checks, and standardized coefficients.",
    list(r_num_one(), r_predictors())
  ),
  moderation = method(
    "moderation", "Moderation model", "Study relationships",
    "Does one relationship differ across values of another variable?",
    "Fits an interaction between a focal predictor and a moderator, with optional control variables.",
    list(r_num_one(), role("focal", "Focal predictor", "numeric_or_factor_one", "Choose the predictor of primary interest."),
         role("moderator", "Moderator", "numeric_or_factor_one", "Choose the proposed moderator."),
         role("controls", "Control variables", "predictors_optional", "Optional control variables.", FALSE)),
    level = "Advanced"
  ),
  mediation = method(
    "mediation", "Mediation model", "Study relationships",
    "Is an association carried through an intermediate variable?",
    "Estimates direct, indirect, and total paths with bootstrap confidence intervals.",
    list(r_num_one(), role("exposure", "Exposure", "numeric_one", "Choose the proposed starting variable."),
         role("mediator", "Mediator", "numeric_one", "Choose the proposed intermediate variable."),
         role("controls", "Numeric control variables", "numeric_many_optional", "Optional numeric control variables.", FALSE)),
    requires = "lavaan", level = "Advanced"
  ),
  logistic_regression = method(
    "logistic_regression", "Binary logistic regression", "Build a model",
    "How do predictors relate to a binary outcome?",
    "Reports odds ratios, confidence intervals, model fit, and classification summaries.",
    list(role("outcome", "Binary outcome", "binary_one", "Choose a column with exactly two observed values."), r_predictors())
  ),
  multinomial_regression = method(
    "multinomial_regression", "Multinomial logistic regression", "Build a model",
    "How do predictors relate to an unordered outcome with several categories?",
    "Models category membership relative to a reference category.",
    list(r_cat_one("outcome", "Categorical outcome"), r_predictors()),
    requires = "nnet", level = "Advanced"
  ),
  ordinal_regression = method(
    "ordinal_regression", "Ordinal logistic regression", "Build a model",
    "How do predictors relate to an ordered outcome?",
    "Fits a proportional-odds model to an ordered categorical outcome.",
    list(role("outcome", "Ordered outcome", "ordinal_one", "Choose an ordered or orderable categorical column."), r_predictors()),
    requires = "MASS", level = "Advanced"
  ),
  poisson_regression = method(
    "poisson_regression", "Poisson regression", "Build a model",
    "How do predictors relate to a count outcome?",
    "Models event counts and reports rate ratios. An exposure column can be used as an offset.",
    list(r_num_one("outcome", "Count outcome"), r_predictors(),
         role("exposure_time", "Exposure amount", "numeric_optional", "Optional positive exposure time or population.", FALSE))
  ),
  negative_binomial = method(
    "negative_binomial", "Negative binomial regression", "Build a model",
    "How do predictors relate to an overdispersed count outcome?",
    "Models counts when variation is larger than a Poisson model expects.",
    list(r_num_one("outcome", "Count outcome"), r_predictors(),
         role("exposure_time", "Exposure amount", "numeric_optional", "Optional positive exposure time or population.", FALSE)),
    requires = "MASS", level = "Advanced"
  ),
  # Prediction. These are grouped by what the reader is predicting rather than
  # by algorithm, because someone who wants to predict a category does not
  # start by choosing between a tree and a penalized regression.
  classification_tree = method(
    "classification_tree", "Classification tree", "Predict a category",
    "Can a series of decision rules predict a category?",
    "Fits a decision tree on a training set and checks classification accuracy on held-out rows.",
    list(role("outcome", "Categorical outcome", "categorical_one", "Choose the category to predict."),
         r_predictors()), requires = "rpart", kind = "machine_learning"
  ),
  regression_tree = method(
    "regression_tree", "Regression tree", "Predict a number",
    "Can a series of decision rules predict a numeric value?",
    "Fits a decision tree on a training set and checks prediction error on held-out rows.",
    list(r_num_one(), r_predictors()), requires = "rpart", kind = "machine_learning"
  ),
  random_forest_classification = method(
    "random_forest_classification", "Random forest classification", "Predict a category",
    "Can a collection of trees predict a category?",
    "Fits many decision trees and checks classification accuracy on held-out rows.",
    list(role("outcome", "Categorical outcome", "categorical_one", "Choose the category to predict."),
         r_predictors()), requires = "ranger", level = "Advanced", kind = "machine_learning"
  ),
  random_forest_regression = method(
    "random_forest_regression", "Random forest regression", "Predict a number",
    "Can a collection of trees predict a numeric value?",
    "Fits many decision trees and checks prediction error on held-out rows.",
    list(r_num_one(), r_predictors()), requires = "ranger", level = "Advanced", kind = "machine_learning"
  ),
  nearest_neighbors = method(
    "nearest_neighbors", "K-nearest neighbors", "Predict a category",
    "Do nearby rows tend to belong to the same category?",
    "Predicts a category from the most similar standardized training rows and checks accuracy on held-out rows.",
    list(role("outcome", "Categorical outcome", "categorical_one", "Choose the category to predict."),
         r_predictors()), requires = "class", kind = "machine_learning"
  ),
  elastic_net_classification = method(
    "elastic_net_classification", "Elastic-net classification", "Predict a category",
    "Can a regularized model predict a category?",
    "Balances ridge and lasso penalties, selects a penalty by cross-validation, and checks classification on held-out rows.",
    list(role("outcome", "Categorical outcome", "categorical_one", "Choose the category to predict."),
         r_predictors()), requires = "glmnet", level = "Advanced", kind = "machine_learning"
  ),
  naive_bayes = method(
    "naive_bayes", "Naive Bayes classification", "Predict a category",
    "Can conditional probabilities predict a category?",
    "Fits a probability classifier and checks its category predictions on held-out rows.",
    list(role("outcome", "Categorical outcome", "categorical_one", "Choose the category to predict."),
         r_predictors()), requires = "e1071", kind = "machine_learning"
  ),
  elastic_net_regression = method(
    "elastic_net_regression", "Elastic-net regression", "Predict a number",
    "Can a regularized model predict a numeric value?",
    "Balances ridge and lasso penalties, selects a penalty by cross-validation, and checks error on held-out rows.",
    list(r_num_one(), r_predictors()), requires = "glmnet", level = "Advanced", kind = "machine_learning"
  ),
  dbscan_clustering = method(
    "dbscan_clustering", "DBSCAN clustering", "Find groups",
    "Which rows form dense groups, and which look like noise?",
    "Finds dense groups among standardized numeric columns without requiring a fixed number of clusters.",
    list(r_num_many()), requires = "dbscan", level = "Advanced", kind = "machine_learning"
  ),
  hdbscan_clustering = method(
    "hdbscan_clustering", "HDBSCAN clustering", "Find groups",
    "Which dense groups remain stable across several density levels?",
    "Finds density-based groups, marks noise, and reports membership strength without a distance-radius setting.",
    list(r_num_many()), requires = "dbscan", level = "Advanced", kind = "machine_learning"
  ),
  manova = method(
    "manova", "Multivariate analysis of variance", "Study several outcomes",
    "Do groups differ across several numeric outcomes considered together?",
    "Runs MANOVA with Pillai's trace, followed by outcome-specific summaries.",
    list(role("outcomes", "Numeric outcomes", "numeric_many", "Choose two or more outcomes."), r_predictors()),
    level = "Advanced"
  ),
  pca = method(
    "pca", "Principal components analysis", "Reduce dimensions",
    "Can many numeric variables be summarized by fewer components?",
    "Uses standardized variables and reports loadings, explained variance, and a biplot.",
    list(r_num_many()), level = "Core", kind = "machine_learning"
  ),
  kmeans_clustering = method(
    "kmeans_clustering", "K-means clustering", "Find groups",
    "Which rows have similar numeric profiles?",
    "Groups standardized rows around cluster centers and reports cluster sizes and centers.",
    list(r_num_many()), level = "Advanced", kind = "machine_learning"
  ),
  hierarchical_clustering = method(
    "hierarchical_clustering", "Hierarchical clustering", "Find groups",
    "How do similar rows join into a nested grouping?",
    "Builds a Ward hierarchy from standardized variables and cuts it into a selected number of groups.",
    list(r_num_many()), level = "Advanced", kind = "machine_learning"
  ),
  factor_analysis = method(
    "factor_analysis", "Exploratory factor analysis", "Study scales and latent variables",
    "What latent dimensions may underlie a set of items?",
    "Fits a requested common-factor solution and reports loadings, communalities, and fit summaries.",
    list(r_num_many()), requires = "psych", level = "Advanced"
  ),
  reliability = method(
    "reliability", "Scale reliability", "Study scales and latent variables",
    "How consistently do a set of items measure together?",
    "Reports coefficient alpha, item-rest correlations, and alpha if each item is removed.",
    list(r_num_many())
  ),
  cfa = method(
    "cfa", "Confirmatory factor analysis", "Study scales and latent variables",
    "Does a proposed measurement model fit the observed data?",
    "Fits lavaan measurement syntax and reports loadings, fit indices, residuals, and warnings.",
    list(role("model_syntax", "Measurement model", "lavaan_text", "Use lavaan syntax, such as factor1 =~ item1 + item2 + item3.")),
    requires = "lavaan", level = "Advanced"
  ),
  sem = method(
    "sem", "Structural equation model", "Study scales and latent variables",
    "Does a proposed system of measurement and structural paths fit?",
    "Fits lavaan model syntax and reports path estimates, fit indices, residuals, and warnings.",
    list(role("model_syntax", "SEM model", "lavaan_text", "Enter lavaan measurement, regression, and covariance paths.")),
    requires = "lavaan", level = "Advanced"
  ),
  hlm_linear = method(
    "hlm_linear", "Hierarchical linear model", "Study clustered or repeated data",
    "How do predictors relate to a numeric outcome within clustered data?",
    "Fits a mixed model with a random intercept and an optional random slope.",
    list(r_num_one(), r_predictors(), role("cluster", "Cluster ID", "id_one", "Choose the school, site, person, or other cluster."),
         role("random_slope", "Random slope", "numeric_optional", "Optional numeric predictor whose slope may vary by cluster.", FALSE)),
    requires = c("lme4", "lmerTest"), level = "Advanced"
  ),
  hlm_logistic = method(
    "hlm_logistic", "Hierarchical logistic model", "Study clustered or repeated data",
    "How do predictors relate to a binary outcome within clustered data?",
    "Fits a mixed logistic model with a random intercept and an optional random slope.",
    list(role("outcome", "Binary outcome", "binary_one", "Choose a column with exactly two observed values."),
         r_predictors(), role("cluster", "Cluster ID", "id_one", "Choose the cluster column."),
         role("random_slope", "Random slope", "numeric_optional", "Optional numeric predictor whose slope may vary by cluster.", FALSE)),
    requires = "lme4", level = "Advanced"
  ),
  growth_model = method(
    "growth_model", "Multilevel growth model", "Study clustered or repeated data",
    "How does a numeric outcome change over time within people or units?",
    "Fits time within unit, with random intercepts and optional random time slopes.",
    list(r_num_one(), r_num_one("time", "Time"), role("cluster", "Person or unit ID", "id_one", "Choose the repeated-measure unit."),
         role("controls", "Other predictors", "predictors_optional", "Optional time-varying or unit-level predictors.", FALSE)),
    engine = "hlm_linear", requires = c("lme4", "lmerTest"), level = "Advanced"
  ),
  # Time to event. Both methods need a status column, and the engine states
  # which value it is treating as the event, since that is the single easiest
  # thing to get backwards in survival analysis.
  kaplan_meier = method(
    "kaplan_meier", "Kaplan-Meier survival analysis", "Study events over time",
    "How does time to an event differ across groups?",
    "Estimates event-free survival over time and compares curves when a group is selected.",
    list(r_num_one("time", "Time to event or censoring"), role("status", "Event status", "binary_one", "Choose the event indicator."),
         r_cat_optional()), requires = "survival", level = "Advanced"
  ),
  cox_regression = method(
    "cox_regression", "Cox proportional hazards model", "Study events over time",
    "How do predictors relate to the rate of an event over time?",
    "Reports hazard ratios, confidence intervals, concordance, and a proportional-hazards check.",
    list(r_num_one("time", "Time to event or censoring"), role("status", "Event status", "binary_one", "Choose the event indicator."),
         r_predictors()), requires = "survival", level = "Advanced"
  ),
  time_series = method(
    "time_series", "Time-series trend and ARIMA", "Study events over time",
    "What trend and serial pattern appear in measurements over time?",
    "Fits a selected ARIMA order, reports coefficients and residual checks, and plots fitted values.",
    list(r_num_one(), role("time", "Time order", "numeric_or_date_one", "Choose the date, period, or ordered time column.")),
    level = "Advanced"
  )
)

# Grouped by the question a reader is trying to answer rather than by
# statistical family. Someone who wants to know whether two groups differ does
# not begin by deciding between a parametric and a nonparametric test.
analysis_choices <- function() {
  grouped_choices <- function(families, kind) lapply(names(families), function(family) {
    items <- Filter(function(x) identical(x$family, family) && identical(x$kind, kind), ANALYSES)
    stats::setNames(vapply(items, `[[`, character(1), "id"),
                    vapply(items, `[[`, character(1), "name"))
  })
  statistics <- grouped_choices(ANALYSIS_FAMILIES, "statistics")
  machine_learning <- grouped_choices(MACHINE_LEARNING_FAMILIES, "machine_learning")
  names(statistics) <- paste("Statistics", names(ANALYSIS_FAMILIES), sep = " · ")
  names(machine_learning) <- paste("Machine learning", names(MACHINE_LEARNING_FAMILIES), sep = " · ")
  c(statistics, machine_learning)
}

method_available <- function(x) {
  all(vapply(x$requires, requireNamespace, logical(1), quietly = TRUE))
}

# Optional packages are checked at the moment a method is run, not at startup.
# A reader with no interest in structural equation models should not have to
# install lavaan to open the app.
missing_packages <- function(x) {
  x$requires[!vapply(x$requires, requireNamespace, logical(1), quietly = TRUE)]
}

catalog_table <- function() {
  do.call(rbind, lapply(ANALYSES, function(x) {
    data.frame(
      Type = if (identical(x$kind, "machine_learning")) "Machine learning" else "Statistics",
      Family = x$family,
      Method = x$name,
      Question = x$question,
      Level = x$level,
      Packages = if (length(x$requires)) paste(x$requires, collapse = ", ") else "Base R",
      stringsAsFactors = FALSE
    )
  }))
}
