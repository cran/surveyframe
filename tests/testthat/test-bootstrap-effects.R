# tests/testthat/test-bootstrap-effects.R
#
# Rank effect sizes and bootstrap intervals, batch 2 findings 15 and 16.
#
# 15. The Mann-Whitney and signed-rank runners took z from the continuity-
#     corrected p value, while their bootstrap resamples used an uncorrected
#     z, and the signed-rank point estimate divided by all pairs where the
#     resamples dropped zero differences. The interval described a different
#     statistic from the r it sat beside. z, rebuilt from p/2, was also always
#     negative, so its sign said nothing about direction.
# 16. Percentile intervals dropped failed resamples without saying how many,
#     and a resample distribution collapsed to one value returned a zero-width
#     interval. A Kruskal-Wallis resample with every value tied returned 0.

mw_data <- function() {
  set.seed(21)
  data.frame(group = rep(c("a", "b"), each = 25),
             y = c(round(stats::rnorm(25, 5, 2)), round(stats::rnorm(25, 6.5, 2))))
}

test_that("15: the Mann-Whitney r is the estimate its interval is built around", {
  res <- sframe_run_mann_whitney(mw_data(), c("group", "y"))
  expect_equal(res$r, unname(res$r_ci[["estimate"]]), tolerance = 1e-12)
  # z, p and r describe the same normal approximation
  expect_equal(res$p, 2 * stats::pnorm(-abs(res$z)), tolerance = 1e-10)
  expect_equal(res$r, abs(res$z) / sqrt(res$n1 + res$n2), tolerance = 1e-12)
})

test_that("15: the Mann-Whitney z carries the direction of the difference", {
  d <- mw_data()
  swapped <- d
  swapped$group <- ifelse(d$group == "a", "b", "a")
  swapped <- swapped[order(swapped$group), ]
  z1 <- sframe_run_mann_whitney(d, c("group", "y"))$z
  z2 <- sframe_run_mann_whitney(swapped, c("group", "y"))$z
  expect_equal(z1, -z2, tolerance = 1e-10)
  expect_true(z1 != 0)
})

test_that("15: the signed-rank r uses the same pairs as its test and interval", {
  set.seed(8)
  x <- round(stats::rnorm(30, 5, 2))
  y <- x + sample(c(-2, -1, 0, 0, 0, 1, 2, 3), 30, replace = TRUE)
  res <- sframe_run_wilcoxon_pair(data.frame(x = x, y = y), c("x", "y"))
  nonzero <- sum(x != y)
  differences <- x - y
  nonzero_differences <- differences[differences != 0]
  expected_V <- sum(rank(abs(nonzero_differences))[nonzero_differences > 0])

  expect_equal(res$r, unname(res$r_ci[["estimate"]]), tolerance = 1e-12)
  expect_equal(res$V, expected_V, tolerance = 1e-12)
  expect_equal(res$p, 2 * stats::pnorm(-abs(res$z)), tolerance = 1e-10)
  expect_equal(res$r, abs(res$z) / sqrt(nonzero), tolerance = 1e-12)
})

test_that("16: a collapsed bootstrap distribution gives no interval, with a reason", {
  tab <- table(c("a", "a", "a", "b", "b", "b"), c("x", "x", "x", "y", "y", "y"))
  ci <- cramers_v_ci(tab, R = 500, seed = 1)
  expect_true(is.na(ci[["lower"]]) && is.na(ci[["upper"]]))
  expect_false(is.null(attr(ci, "reason")))
  expect_identical(attr(ci, "resamples"), 500L)
})

test_that("16: a usable interval reports how many resamples it used", {
  set.seed(2)
  ci <- cohens_d_ci(stats::rnorm(40), stats::rnorm(40, .5), R = 400, seed = 3)
  expect_true(all(is.finite(ci[c("lower", "upper")])))
  expect_identical(attr(ci, "resamples"), 400L)
  expect_identical(attr(ci, "valid_resamples"), 400L)
  expect_null(attr(ci, "reason"))
})

test_that("16: an all-tied Kruskal-Wallis sample has no effect size", {
  expect_true(is.na(sframe_kw_eta_sq(rep(3, 12), rep(c("a", "b", "c"), 4))))
})

test_that("16: mediation reports how many bootstrap resamples it used", {
  set.seed(6)
  x <- stats::rnorm(60); m <- 0.5 * x + stats::rnorm(60); y <- 0.4 * m + stats::rnorm(60)
  res <- sframe_run_mediation(data.frame(x = x, m = m, y = y),
                              list(predictor = "x", mediator = "m", outcome = "y"),
                              options = list(bootstrap = 200))
  expect_identical(attr(res$indirect_ci, "resamples"), 200L)
  expect_false(is.null(attr(res$indirect_ci, "valid_resamples")))
})
