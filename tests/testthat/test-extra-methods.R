# Equivalence, agreement, and meta-analysis -----------------------------------
#
# Each engine is checked against a calculation that does not share its code:
# base R one-sided tests for equivalence, two other packages for kappa, and
# the published figures for the vaccine trials.

testthat::test_that("equivalence matches two one-sided Welch tests in base R", {
  tooth <- load_sample("tooth")
  result <- run_analysis("equivalence", tooth, list(outcome = "len", group = "supp"), list(equivalence_margin = 8))
  x <- tooth$len[tooth$supp == "OJ"]; y <- tooth$len[tooth$supp == "VC"]
  lower <- stats::t.test(x, y, mu = -8, alternative = "greater")
  upper <- stats::t.test(x, y, mu = 8, alternative = "less")
  testthat::expect_equal(result$tests$p[1], max(lower$p.value, upper$p.value))
  testthat::expect_equal(result$tests$p[2:3], c(lower$p.value, upper$p.value))
  # A test at 0.05 goes with a 90 percent interval, and the record says so.
  testthat::expect_equal(result$conf_level, 0.90)
  interval <- stats::t.test(x, y, conf.level = 0.90)$conf.int
  testthat::expect_equal(c(result$estimates$Lower, result$estimates$Upper), as.numeric(interval))
  testthat::expect_identical(equivalence_state(result), "equivalent")
  testthat::expect_identical(result_verdict(result)$label, "Close enough")
  testthat::expect_match(build_narrative(result)$headline, "90% confidence interval", fixed = TRUE)
})

testthat::test_that("equivalence reads the interval against the margin in all three ways", {
  tooth <- load_sample("tooth")
  roles <- list(outcome = "len", group = "supp")
  testthat::expect_identical(equivalence_state(run_analysis("equivalence", tooth, roles, list(equivalence_margin = 8))), "equivalent")
  testthat::expect_identical(equivalence_state(run_analysis("equivalence", tooth, roles, list(equivalence_margin = 3))), "unsettled")
  testthat::expect_identical(equivalence_state(run_analysis("equivalence", tooth, roles, list(equivalence_margin = 0.2))), "apart")
})

testthat::test_that("equivalence says when it had to choose the margin itself", {
  tooth <- load_sample("tooth")
  result <- run_analysis("equivalence", tooth, list(outcome = "len", group = "supp"), list())
  testthat::expect_false(result$extras$margin_given)
  testthat::expect_identical(result$checks$Status[1], "warn")
  testthat::expect_match(result$checks$Detail[1], "half a pooled standard deviation", fixed = TRUE)
  testthat::expect_match(build_accessible_explanation(result)$boundary, "No margin was entered", fixed = TRUE)
  testthat::expect_error(run_analysis("equivalence", tooth, list(outcome = "len", group = "supp"), list(equivalence_margin = -1)),
                         "greater than zero", fixed = TRUE)
})

testthat::test_that("kappa and its standard error match psych and vcd", {
  testthat::skip_if_not_installed("psych")
  diagnoses <- load_sample("diagnoses")
  roles <- list(rater_1 = "new_orleans_neurologist", rater_2 = "winnipeg_neurologist")
  tab <- table(diagnoses$new_orleans_neurologist, diagnoses$winnipeg_neurologist)
  testthat::expect_equal(sum(tab), 149)
  reference <- psych::cohen.kappa(tab)

  plain <- run_analysis("agreement", diagnoses, roles, list())
  testthat::expect_equal(plain$estimates$Estimate, reference$kappa, tolerance = 1e-10)
  testthat::expect_equal(plain$estimates$SE, sqrt(reference$var.kappa), tolerance = 1e-8)
  testthat::expect_identical(plain$estimates$Reading, "fair")

  quadratic <- run_analysis("agreement", diagnoses, roles, list(kappa_weights = "quadratic"))
  testthat::expect_equal(quadratic$estimates$Estimate, reference$weighted.kappa, tolerance = 1e-10)
  testthat::expect_equal(quadratic$estimates$SE, sqrt(reference$var.weighted), tolerance = 1e-8)

  testthat::skip_if_not_installed("vcd")
  linear <- run_analysis("agreement", diagnoses, roles, list(kappa_weights = "linear"))
  published <- vcd::Kappa(tab)
  testthat::expect_equal(linear$estimates$Estimate, unname(published$Weighted["value"]), tolerance = 1e-10)
  testthat::expect_equal(linear$estimates$SE, unname(published$Weighted["ASE"]), tolerance = 1e-8)
})

testthat::test_that("kappa handles perfect agreement, a category one rater never used, and bad input", {
  same <- data.frame(a = rep(c("x", "y", "z"), 10), b = rep(c("x", "y", "z"), 10))
  testthat::expect_equal(run_analysis("agreement", same, list(rater_1 = "a", rater_2 = "b"), list())$estimates$Estimate, 1)
  uneven <- data.frame(a = rep(c("x", "y", "z"), 10), b = rep(c("x", "y", "y"), 10))
  result <- run_analysis("agreement", uneven, list(rater_1 = "a", rater_2 = "b"), list())
  testthat::expect_identical(result$extras$categories, c("x", "y", "z"))
  testthat::expect_identical(dim(as.matrix(result$extras$observed)), c(3L, 3L))
  constant <- data.frame(a = rep("x", 12), b = rep("x", 12))
  testthat::expect_error(run_analysis("agreement", constant, list(rater_1 = "a", rater_2 = "b"), list()), "at least two categories")
  measured <- data.frame(a = seq_len(40), b = rev(seq_len(40)))
  testthat::expect_error(run_analysis("agreement", measured, list(rater_1 = "a", rater_2 = "b"), list()), "more than 12")
})

testthat::test_that("the vaccine trials reproduce the published random-effects result", {
  testthat::skip_if_not_installed("metafor")
  bcg <- load_sample("bcg")
  testthat::expect_identical(nrow(bcg), 13L)
  result <- run_analysis("meta_analysis", bcg, list(effect = "log_risk_ratio", se = "standard_error", study = "study"), list())
  # Values reported for these trials by Viechtbauer (2010), to four places.
  testthat::expect_equal(round(result$estimates$Estimate, 4), -0.7145)
  testthat::expect_equal(round(result$estimates$SE, 4), 0.1798)
  testthat::expect_equal(round(result$fit$Value[result$fit$Metric == "Tau squared"], 4), 0.3132)
  testthat::expect_equal(round(result$fit$Value[result$fit$Metric == "I squared, percent"], 2), 92.22)
  testthat::expect_equal(round(result$tests$Statistic[2], 3), 152.233)
  testthat::expect_lt(result$estimates$Prediction_lower, result$estimates$Lower)
  testthat::expect_gt(result$estimates$Prediction_upper, result$estimates$Upper)
  testthat::expect_equal(sum(result$plot_data$Weight), 100, tolerance = 1e-6)
})

testthat::test_that("meta-analysis works without labels and refuses unusable standard errors", {
  testthat::skip_if_not_installed("metafor")
  bcg <- load_sample("bcg")
  unlabeled <- run_analysis("meta_analysis", bcg, list(effect = "log_risk_ratio", se = "standard_error", study = ""), list())
  testthat::expect_identical(unlabeled$plot_data$Study[1], "Study 1")
  bcg$standard_error[2] <- 0
  testthat::expect_error(run_analysis("meta_analysis", bcg, list(effect = "log_risk_ratio", se = "standard_error", study = ""), list()),
                         "greater than zero")
  testthat::expect_error(run_analysis("meta_analysis", load_sample("bcg")[1:2, ], list(effect = "log_risk_ratio", se = "standard_error", study = ""), list()),
                         "studies")
})

testthat::test_that("the three methods write both readings, a figure, and a runnable script", {
  testthat::skip_if_not_installed("metafor")
  cases <- list(
    list("equivalence", load_sample("tooth"), list(outcome = "len", group = "supp")),
    list("agreement", load_sample("diagnoses"), list(rater_1 = "new_orleans_neurologist", rater_2 = "winnipeg_neurologist")),
    list("meta_analysis", load_sample("bcg"), list(effect = "log_risk_ratio", se = "standard_error", study = "study"))
  )
  for (case in cases) {
    result <- run_analysis(case[[1]], case[[2]], case[[3]], list())
    everyday <- build_accessible_explanation(result)
    for (part in c("bottom_line", "picture", "certainty", "boundary")) {
      testthat::expect_true(nzchar(everyday[[part]]), info = paste(case[[1]], part))
    }
    testthat::expect_true(nzchar(build_narrative(result)$headline), info = case[[1]])
    testthat::expect_s3_class(analysis_plot(result), "ggplot")
    testthat::expect_silent(ggplot2::ggplot_build(analysis_plot(result, "light", "deuteranopia")))
    testthat::expect_silent(parse(text = reproduction_script(result)))
  }
})

# Repairs to two existing methods --------------------------------------------

testthat::test_that("repeated-measures ANOVA treats a numeric condition as conditions", {
  testthat::skip_if_not_installed("nlme")
  growth <- load_sample("orthodont")
  result <- run_analysis("repeated_anova", growth, list(outcome = "distance", within = "age", id = "Subject"), list())
  # Four ages are four conditions and three degrees of freedom. Read as a
  # number, age would be one straight line and one degree of freedom.
  growth$age <- factor(growth$age)
  reference <- stats::anova(stats::lm(distance ~ age + Subject, data = growth))
  assumed <- result$tests[result$tests$Test == "Sphericity assumed", ]
  testthat::expect_equal(assumed$df1, 3)
  testthat::expect_equal(assumed$Statistic, reference[["F value"]][1])
  testthat::expect_equal(assumed$p, reference[["Pr(>F)"]][1])
  testthat::expect_true(all(is.finite(result$tests$p)))
  testthat::expect_identical(result$tests$Test[1], "Greenhouse-Geisser corrected")
  testthat::expect_true(nzchar(build_narrative(result)$headline))
})

testthat::test_that("Greenhouse-Geisser epsilon matches car and stays within its bounds", {
  testthat::skip_if_not_installed("nlme")
  growth <- load_sample("orthodont")
  result <- run_analysis("repeated_anova", growth, list(outcome = "distance", within = "age", id = "Subject"), list())
  epsilon <- result$tests$Epsilon[1]
  testthat::expect_gte(epsilon, 1 / 3)
  testthat::expect_lte(epsilon, 1)
  testthat::expect_identical(greenhouse_geisser(diag(2)), 1)
  # Equal variances and equal covariances satisfy sphericity exactly.
  testthat::expect_equal(greenhouse_geisser(matrix(0.5, 4, 4) + diag(0.5, 4)), 1)
  testthat::skip_if_not_installed("car")
  wide <- stats::reshape(growth[c("Subject", "age", "distance")], idvar = "Subject", timevar = "age", direction = "wide")
  fit <- stats::lm(as.matrix(wide[-1]) ~ 1)
  design <- data.frame(age = factor(c(8, 10, 12, 14)))
  published <- summary(car::Anova(fit, idata = design, idesign = ~age, type = 3), multivariate = FALSE)$pval.adjustments
  testthat::expect_equal(epsilon, published[1, "GG eps"], tolerance = 1e-8)
  testthat::expect_equal(result$tests$p[1], published[1, "Pr(>F[GG])"], tolerance = 1e-8)
})

testthat::test_that("the two-proportions figure has one bar for each group", {
  admissions <- load_sample("admissions")
  result <- run_analysis("two_proportions", admissions, list(outcome = "Admit", group = "Gender"), list())
  testthat::expect_identical(names(result$plot_data), c("Group", "Proportion"))
  testthat::expect_identical(nrow(result$plot_data), 2L)
  testthat::expect_true(is.numeric(result$plot_data$Proportion))
  testthat::expect_silent(ggplot2::ggplot_build(analysis_plot(result)))
})
