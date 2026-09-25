# tests/testthat/test-statistics-runners.R
#
# Numeric statistics runners, batch 2 findings 13, 14, 22, 23, 24 and 25, each
# pinned against a reference calculation written here.
#
# 13. ANCOVA exported a sequential table with group before the covariates, so
#     the group test was unadjusted while the text said adjusted. The slope
#     check also formed group * cov1 + cov2, dropping the second interaction.
# 14. Partial correlation took its p value from an ordinary Pearson test on
#     n - 2 degrees of freedom while printing n - 2 - k. Spearman ranked the
#     residuals, where the variables must be ranked before residualising.
# 22. sample_size_plan() returned 64 or 50 per group for t tests and ANOVA,
#     whatever alpha and power were requested, and printed both anyway.
# 23. The Firth likelihood-ratio statistic was half the reference value.
# 24. Two-way ANOVA gave the residual row a partial eta squared of 0.5.
# 25. render_results() rounded every numeric column to 2 decimals, so a p of
#     .004 was shown as 0 and a p near .05 lost the precision needed to judge it.

ancova_data <- function(n = 90) {
  set.seed(3)
  group <- rep(c("a", "b", "c"), length.out = n)
  cov1 <- stats::rnorm(n) + ifelse(group == "c", 1.2, 0)   # correlated with group
  cov2 <- stats::rnorm(n)
  y <- 2 + 0.8 * cov1 + 0.5 * cov2 + ifelse(group == "b", 0.4, 0) + stats::rnorm(n)
  keep <- -c(3, 7, 50, 61)                                 # unbalanced
  data.frame(group = group, cov1 = cov1, cov2 = cov2, y = y)[keep, ]
}

test_that("13: the ANCOVA group test is adjusted for the covariates", {
  d <- ancova_data()
  res <- sframe_run_ancova(d, list(group = "group", outcome = "y",
                                   covariates = c("cov1", "cov2")))
  df <- transform(d, group = factor(group))
  ref <- stats::anova(stats::lm(y ~ cov1 + cov2, data = df),
                      stats::lm(y ~ cov1 + cov2 + group, data = df))
  row <- res$table[res$table$effect == "group", ]

  expect_equal(row$F, ref$F[2], tolerance = 1e-8)
  expect_equal(row$p, ref$`Pr(>F)`[2], tolerance = 1e-8)
})

test_that("13: the slope check includes an interaction with every covariate", {
  d <- ancova_data()
  res <- sframe_run_ancova(d, list(group = "group", outcome = "y",
                                   covariates = c("cov1", "cov2")))
  summary_text <- paste(res$slope_model_summary, collapse = "\n")
  expect_match(summary_text, "group:cov1", fixed = TRUE)
  expect_match(summary_text, "group:cov2", fixed = TRUE)
})

partial_data <- function(n = 60) {
  set.seed(5)
  z1 <- stats::rnorm(n); z2 <- stats::rnorm(n)
  x <- z1 + stats::rnorm(n); y <- 0.5 * x + z1 - z2 + stats::rnorm(n)
  data.frame(x = x, y = y^3, z1 = z1, z2 = exp(z2))   # skewed, for Spearman
}

partial_reference <- function(d, controls, rank_first) {
  if (rank_first) d <- as.data.frame(lapply(d, rank))
  rx <- stats::residuals(stats::lm(d$x ~ as.matrix(d[controls])))
  ry <- stats::residuals(stats::lm(d$y ~ as.matrix(d[controls])))
  r <- stats::cor(rx, ry)
  df <- nrow(d) - 2 - length(controls)
  t <- r * sqrt(df / (1 - r^2))
  list(r = r, df = df, p = 2 * stats::pt(-abs(t), df))
}

test_that("14: partial Pearson inference uses n - 2 - k degrees of freedom", {
  d <- partial_data()
  res <- sframe_run_partial_correlation(
    d, list(x = "x", y = "y", controls = c("z1", "z2")))
  ref <- partial_reference(d, c("z1", "z2"), rank_first = FALSE)

  expect_equal(res$r, ref$r, tolerance = 1e-10)
  expect_equal(res$p, ref$p, tolerance = 1e-10)
  expect_equal(res$df, ref$df)
})

test_that("14: partial Spearman ranks the variables before residualising", {
  d <- partial_data()
  res <- sframe_run_partial_correlation(
    d, list(x = "x", y = "y", controls = c("z1", "z2")),
    options = list(method = "spearman"))
  ref <- partial_reference(d, c("z1", "z2"), rank_first = TRUE)

  expect_equal(res$r, ref$r, tolerance = 1e-10)
  expect_equal(res$p, ref$p, tolerance = 1e-10)
})

test_that("14: an unsupported method is reported", {
  res <- sframe_run_partial_correlation(
    partial_data(), list(x = "x", y = "y", controls = "z1"),
    options = list(method = "kendall"))
  expect_false(is.null(res$error))
})

test_that("22: t-test planning follows alpha, power and effect size", {
  plan <- sample_size_plan("t_test", d = 0.5, alpha = 0.05, power = 0.80)
  per_group <- ceiling(stats::power.t.test(delta = 0.5, sd = 1, sig.level = 0.05,
                                           power = 0.80)$n)
  expect_identical(as.integer(plan$estimated_n), as.integer(2 * per_group))

  # alpha alone moves the estimate, and to the exact reference value
  strict_alpha <- sample_size_plan("t_test", d = 0.5, alpha = 0.01, power = 0.80)
  ref_alpha <- 2 * ceiling(stats::power.t.test(delta = 0.5, sd = 1, sig.level = 0.01,
                                               power = 0.80)$n)
  expect_identical(as.integer(strict_alpha$estimated_n), as.integer(ref_alpha))
  expect_gt(strict_alpha$estimated_n, plan$estimated_n)
})

test_that("22: ANOVA planning follows alpha, power and Cohen's f", {
  plan <- sample_size_plan("anova", f = 0.25, groups = 3L, alpha = 0.05, power = 0.80)
  per_group <- ceiling(stats::power.anova.test(
    groups = 3, between.var = 0.25^2 * 3 / 2, within.var = 1,
    sig.level = 0.05, power = 0.80)$n)
  expect_identical(as.integer(plan$estimated_n), as.integer(3 * per_group))
})

test_that("22: an assumed effect size is stated", {
  plan <- sample_size_plan("t_test")
  expect_match(paste(plan$warnings, collapse = " "), "0.5")
})

test_that("23: the Firth likelihood-ratio statistic is twice the log-likelihood gain", {
  skip_if_not_installed("logistf")
  set.seed(9)
  d <- data.frame(x = stats::rnorm(40))
  d$y <- as.integer(d$x + stats::rnorm(40) > 0)
  res <- sframe_run_firth_logistic(d, list(outcome = "y", predictors = "x"))
  fit <- logistf::logistf(y ~ x, data = d)
  expect_equal(res$likelihood_ratio,
               unname(-2 * (fit$loglik["null"] - fit$loglik["full"])),
               tolerance = 1e-10)
})

test_that("24: the residual row of a two-way ANOVA carries no effect size", {
  set.seed(4)
  d <- data.frame(a = rep(c("x", "y"), 20), b = rep(c("p", "q"), each = 20),
                  y = stats::rnorm(40))
  res <- sframe_run_anova_two(d, list(factor1 = "a", factor2 = "b", outcome = "y"))
  resid <- res$table[res$table$effect == "Residuals", ]
  expect_true(is.na(resid$partial_eta_sq))
  expect_true(all(!is.na(res$table$partial_eta_sq[res$table$effect != "Residuals"])))
})

test_that("25: rendered p values keep their precision", {
  instr <- sf_instrument(title = "Render", components = list(
    sf_item("g", "Group", "text")))
  instr$analysis_plan <- list(list(id = "b1", research_question = "Q",
                                   variables = "g", test = "frequency",
                                   alpha = 0.05, citations = character(0),
                                   interpretation = "", result = NULL))
  testthat::local_mocked_bindings(sframe_run_frequency = function(data, vars, weights = NULL) {
    list(test = "frequency", apa = "x",
         table = data.frame(effect = c("A", "B", "C"), F = c(9.1, 3.2, 20),
                            p = c(0.004, 0.0437, 0.00001)))
  })
  res <- run_analysis_plan(data.frame(g = c("a", "b")), instr)
  out <- render_results(res, instr, output_file = tempfile(fileext = ".html"))
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")

  expect_match(html, "<td>0.004</td>", fixed = TRUE)
  expect_match(html, "<td>0.044</td>", fixed = TRUE)
  expect_match(html, "<td>&lt;.001</td>", fixed = TRUE)
  expect_false(grepl("<td>0</td>", html, fixed = TRUE))
})

test_that("render_results() puts each value in its own table cell", {
  # htmltools_escape() pastes a vector into one string, so every results table
  # was written with a whole row in a single cell, headers included.
  instr <- sf_instrument(title = "Cells", components = list(
    sf_item("g", "Group", "text")))
  instr$analysis_plan <- list(list(id = "b1", research_question = "Q",
                                   variables = "g", test = "frequency",
                                   alpha = 0.05, citations = character(0),
                                   interpretation = "", result = NULL))
  testthat::local_mocked_bindings(sframe_run_frequency = function(data, vars, weights = NULL) {
    list(test = "frequency", apa = "x",
         table = data.frame(level = c("a", "b"), n = c(9, 12)))
  })
  res <- run_analysis_plan(data.frame(g = c("a", "b")), instr)
  out <- render_results(res, instr, output_file = tempfile(fileext = ".html"))
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")

  expect_match(html, "<th>level</th><th>n</th>", fixed = TRUE)
  expect_match(html, "<tr><td>a</td><td>9</td></tr>", fixed = TRUE)
  expect_match(html, "<tr><td>b</td><td>12</td></tr>", fixed = TRUE)
})
