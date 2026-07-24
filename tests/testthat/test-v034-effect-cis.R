# tests/testthat/test-v034-effect-cis.R
# Effect-size confidence intervals: the four exported bootstrap helpers and
# the additive CI keys on the analysis-plan runners.

test_that("bootstrap_ci is reproducible with a seed and guards small n", {
  x <- mtcars$mpg
  a <- bootstrap_ci(x, seed = 42)
  b <- bootstrap_ci(x, seed = 42)
  expect_identical(a, b)
  expect_identical(unname(a["estimate"]), stats::median(x))
  expect_true(a["lower"] <= a["estimate"] && a["estimate"] <= a["upper"])

  narrow <- bootstrap_ci(x, conf.level = 0.5, seed = 42)
  wide <- bootstrap_ci(x, conf.level = 0.99, seed = 42)
  expect_true(narrow["upper"] - narrow["lower"] < wide["upper"] - wide["lower"])

  tiny <- bootstrap_ci(c(1, 2))
  expect_true(is.na(tiny["lower"]) && is.na(tiny["upper"]))
})

test_that("a seeded bootstrap call does not disturb the caller's RNG stream", {
  set.seed(123)
  before <- runif(5)

  set.seed(123)
  bootstrap_ci(mtcars$mpg, seed = 999)
  cohens_d_ci(mtcars$mpg[mtcars$am == 1], mtcars$mpg[mtcars$am == 0], seed = 999)
  after <- runif(5)

  expect_identical(before, after)
})

test_that("cohens_d_ci brackets the point estimate and guards small groups", {
  set.seed(1)
  x <- rnorm(40, 3.5); y <- rnorm(40, 3.0)
  ci <- cohens_d_ci(x, y, seed = 42)
  expect_identical(unname(ci["estimate"]),
                   surveyframe:::sframe_cohens_d(x, y))
  expect_true(ci["lower"] <= ci["estimate"] && ci["estimate"] <= ci["upper"])
  tiny <- cohens_d_ci(c(1, 2), y)
  expect_true(is.na(tiny["lower"]))
})

test_that("cramers_v_ci and eta_sq_ci return sane intervals", {
  set.seed(2)
  tab <- table(sample(1:2, 120, TRUE), sample(1:3, 120, TRUE))
  v <- cramers_v_ci(tab, seed = 42)
  expect_true(v["estimate"] >= 0 && v["upper"] <= 1)
  expect_true(v["lower"] <= v["estimate"] && v["estimate"] <= v["upper"])

  e <- eta_sq_ci(mtcars$mpg, mtcars$cyl, seed = 42)
  expect_true(e["estimate"] > 0 && e["upper"] <= 1)
  expect_true(e["lower"] <= e["estimate"] && e["estimate"] <= e["upper"])
  one_group <- eta_sq_ci(mtcars$mpg, rep("a", nrow(mtcars)))
  expect_true(is.na(one_group["lower"]))
})

test_that("runners attach CI keys and the apa string carries the interval", {
  set.seed(3)
  dat <- data.frame(
    g = rep(c("a", "b"), c(40, 45)),
    o = c(rnorm(40, 3.4), rnorm(45, 3.0)),
    x1 = rnorm(85)
  )
  dat$x2 <- dat$x1 * 0.6 + rnorm(85, 0, 0.6)
  ci_shape <- function(ci) {
    expect_named(ci, c("estimate", "lower", "upper"))
    expect_false(anyNA(ci))
  }

  tt <- surveyframe:::sframe_run_t_test(dat, c("g", "o"))
  ci_shape(tt$d_ci)
  expect_match(tt$apa, "d = -?\\d+\\.\\d+ \\[")
  expect_match(tt$prompt, "\\[")

  mw <- surveyframe:::sframe_run_mann_whitney(dat, c("g", "o"))
  ci_shape(mw$r_ci)
  expect_match(mw$apa, "r = \\d+\\.\\d+ \\[")

  av <- surveyframe:::sframe_run_anova_one(dat, c("g", "o"))
  ci_shape(av$eta_ci)
  expect_match(av$apa, "η² = \\d+\\.\\d+ \\[")

  kw <- surveyframe:::sframe_run_kruskal(dat, c("g", "o"))
  ci_shape(kw$eta_ci)
  expect_match(kw$apa, "\\[")

  pe <- surveyframe:::sframe_run_correlation(dat, c("x1", "x2"))
  ci_shape(pe$ci)
  expect_match(pe$apa, "r\\(\\d+\\) = -?\\d+\\.\\d+ \\[")
  # The Pearson interval is the analytic Fisher-z one
  analytic <- surveyframe:::sframe_fisher_z_ci(pe$r, pe$n)
  expect_equal(pe$ci, analytic)

  sp <- surveyframe:::sframe_run_correlation(dat, c("x1", "x2"), "spearman")
  ci_shape(sp$ci)

  pr <- surveyframe:::sframe_run_t_test_pair(dat, c("x1", "x2"))
  ci_shape(pr$d_ci)
  expect_match(pr$apa, "d_z = -?\\d+\\.\\d+ \\[")

  wx <- surveyframe:::sframe_run_wilcoxon_pair(dat, c("x1", "x2"))
  ci_shape(wx$r_ci)

  ct <- surveyframe:::sframe_run_crosstab(
    data.frame(a = rep(1:2, 40), b = rep(1:3, length.out = 80)), c("a", "b"))
  ci_shape(ct$v_ci)
  expect_match(ct$apa, "\\[")
})

test_that("degenerate data keeps the 0.3.3 apa string, with no bracket", {
  # Pearson with n = 3 has no Fisher-z interval
  tiny <- data.frame(x1 = c(1, 2, 3), x2 = c(2, 1, 3))
  pe <- surveyframe:::sframe_run_correlation(tiny, c("x1", "x2"))
  expect_true(anyNA(pe$ci))
  expect_no_match(pe$apa, "\\[")
})
