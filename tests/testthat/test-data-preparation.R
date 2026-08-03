testthat::test_that("simple imputation fills selected numeric and categorical cells", {
  d <- data.frame(
    score = c(1, 2, NA, 4),
    group = factor(c("A", "A", NA, "B"))
  )
  got <- impute_selected_data(d, c("score", "group"), "simple")
  testthat::expect_false(anyNA(got$data))
  testthat::expect_equal(got$data$score[3], 2)
  testthat::expect_equal(as.character(got$data$group[3]), "A")
  testthat::expect_equal(got$filled, 2)
})

testthat::test_that("the source frame is not changed during preparation", {
  d <- data.frame(score = c(1, NA, 3))
  got <- impute_selected_data(d, "score", "simple")
  testthat::expect_true(is.na(d$score[2]))
  testthat::expect_false(is.na(got$data$score[2]))
})

testthat::test_that("classification trees report held-out accuracy", {
  testthat::skip_if_not_installed("rpart")
  d <- datasets::iris
  got <- analyse_classification_tree(
    d,
    list(outcome = "Species", predictors = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
    list(random_seed = 11, train_fraction = 0.8, tree_cp = 0.01, tree_minsplit = 10)
  )
  accuracy <- got$fit$Value[got$fit$Metric == "Accuracy"]
  testthat::expect_gte(accuracy, 0)
  testthat::expect_lte(accuracy, 1)
  testthat::expect_equal(nrow(got$plot_data), got$fit$Value[got$fit$Metric == "Holdout rows"])
})

testthat::test_that("tree regression and nearest neighbors return one value per holdout row", {
  testthat::skip_if_not_installed("rpart")
  testthat::skip_if_not_installed("class")
  regression <- analyse_regression_tree(
    datasets::iris,
    list(outcome = "Sepal.Length", predictors = c("Sepal.Width", "Petal.Length", "Petal.Width")),
    list(random_seed = 12, train_fraction = 0.8, tree_cp = 0.01, tree_minsplit = 10)
  )
  neighbors <- analyse_nearest_neighbors(
    datasets::iris,
    list(outcome = "Species", predictors = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
    list(random_seed = 12, train_fraction = 0.8, neighbors = 5)
  )
  testthat::expect_equal(nrow(regression$plot_data), regression$fit$Value[regression$fit$Metric == "Holdout rows"])
  testthat::expect_equal(nrow(neighbors$plot_data), neighbors$fit$Value[neighbors$fit$Metric == "Holdout rows"])
})

testthat::test_that("random forests return bounded or finite holdout scores", {
  testthat::skip_if_not_installed("ranger")
  classification <- analyse_random_forest_classification(
    datasets::iris,
    list(outcome = "Species", predictors = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
    list(random_seed = 13, train_fraction = 0.8, forest_trees = 100)
  )
  regression <- analyse_random_forest_regression(
    datasets::iris,
    list(outcome = "Sepal.Length", predictors = c("Sepal.Width", "Petal.Length", "Petal.Width")),
    list(random_seed = 13, train_fraction = 0.8, forest_trees = 100)
  )
  accuracy <- classification$fit$Value[classification$fit$Metric == "Accuracy"]
  rmse <- regression$fit$Value[regression$fit$Metric == "RMSE"]
  testthat::expect_true(is.finite(accuracy) && accuracy >= 0 && accuracy <= 1)
  testthat::expect_true(is.finite(rmse) && rmse >= 0)
})

testthat::test_that("elastic-net models use training cross-validation and held-out rows", {
  testthat::skip_if_not_installed("glmnet")
  d <- datasets::iris
  classification <- analyse_elastic_net_classification(
    d,
    list(outcome = "Species", predictors = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
    list(random_seed = 11, train_fraction = 0.8, elastic_alpha = 0.5, cv_folds = 5)
  )
  regression <- analyse_elastic_net_regression(
    d,
    list(outcome = "Sepal.Length", predictors = c("Sepal.Width", "Petal.Length", "Petal.Width")),
    list(random_seed = 11, train_fraction = 0.8, elastic_alpha = 0.5, cv_folds = 5)
  )

  testthat::expect_true(all(c("Accuracy", "Selected lambda") %in% classification$fit$Metric))
  testthat::expect_true(all(c("RMSE", "Selected lambda") %in% regression$fit$Metric))
  testthat::expect_gt(nrow(classification$plot_data), 0)
  testthat::expect_gt(nrow(regression$plot_data), 0)
})

testthat::test_that("naive Bayes reports held-out categories", {
  testthat::skip_if_not_installed("e1071")
  got <- analyse_naive_bayes(
    datasets::iris,
    list(outcome = "Species", predictors = c("Sepal.Length", "Petal.Length")),
    list(random_seed = 11, train_fraction = 0.8, naive_laplace = 1)
  )

  testthat::expect_true("Accuracy" %in% got$fit$Metric)
  testthat::expect_equal(sum(got$estimates$Training_share), 1, tolerance = 1e-8)
})

testthat::test_that("density models preserve noise as an explicit result", {
  testthat::skip_if_not_installed("dbscan")
  roles <- list(variables = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
  db <- analyse_dbscan_clustering(datasets::iris, roles, list(dbscan_eps = 0.7, density_min_points = 5))
  hdb <- analyse_hdbscan_clustering(datasets::iris, roles, list(density_min_points = 5))

  testthat::expect_equal(sum(db$estimates$Rows), nrow(datasets::iris))
  testthat::expect_equal(sum(hdb$estimates$Rows), nrow(datasets::iris))
  testthat::expect_equal(nrow(db$plot_data), nrow(datasets::iris))
  testthat::expect_equal(nrow(hdb$plot_data), nrow(datasets::iris))
})

testthat::test_that("prediction settings reject invalid values before fitting", {
  testthat::expect_error(numeric_holdout(30, 0.2, 2), "between 0.50 and 0.95", fixed = TRUE)
  testthat::expect_error(
    analyse_nearest_neighbors(
      datasets::iris,
      list(outcome = "Species", predictors = c("Sepal.Length", "Sepal.Width")),
      list(train_fraction = 0.8, random_seed = 2, neighbors = 0)
    ),
    "at least one neighbor",
    fixed = TRUE
  )
  testthat::expect_error(
    analyse_dbscan_clustering(
      datasets::iris,
      list(variables = c("Sepal.Length", "Sepal.Width")),
      list(dbscan_eps = -1, density_min_points = 5)
    ),
    "greater than zero",
    fixed = TRUE
  )
})
