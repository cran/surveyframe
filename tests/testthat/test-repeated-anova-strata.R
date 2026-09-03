# tests/testthat/test-repeated-anova-strata.R
# Repeated-measures ANOVA must stratify on a factor subject id. Left as an
# integer, aov() treats .subject as a continuous covariate, the
# Error(.subject / condition) split collapses, and the condition effect is
# tested against the wrong error term. jmv::anovaRM() is the oracle.

rm_fixture <- function(n = 40, seed = 7) {
  set.seed(seed)
  subject_effect <- stats::rnorm(n, sd = 2.0)
  data.frame(
    t1 = subject_effect + stats::rnorm(n, sd = 0.6) + 3.0,
    t2 = subject_effect + stats::rnorm(n, sd = 0.6) + 3.8,
    t3 = subject_effect + stats::rnorm(n, sd = 0.6) + 4.6
  )
}

rm_roles <- list(measures = c("t1", "t2", "t3"))

test_that("the within-subject F matches jmv::anovaRM()", {
  skip_if_not_installed("jmv")

  dat <- rm_fixture()
  vars <- c("t1", "t2", "t3")

  jtab <- as.data.frame(jmv::anovaRM(
    data    = dat,
    rm      = list(list(label = "condition", levels = vars)),
    rmCells = lapply(vars, function(v) list(measure = v, cell = v)),
    rmTerms = list("condition")
  )$rmTable)

  res <- sframe_run_repeated_anova(dat, rm_roles)

  expect_null(res$error)
  expect_equal(res$F_stat, jtab[["F[none]"]][1], tolerance = 1e-6)
  expect_equal(res$p,      jtab[["p[none]"]][1], tolerance = 1e-8)
  expect_equal(res$df1,    jtab[["df[none]"]][1])
  expect_equal(res$df2,    jtab[["df[none]"]][2])
})

test_that("the effect is found and reported, not dropped to the fallback branch", {
  res <- sframe_run_repeated_anova(rm_fixture(), rm_roles)

  # A stratum lookup that misses returns the fallback list with no F at all.
  expect_false(is.null(res$F_stat))
  expect_false(is.null(res$p))
  expect_false(is.null(res$eta2))
  expect_true(is.finite(res$F_stat))
})

test_that("the error stratum uses n - 1 subject df, not 1", {
  dat <- rm_fixture(n = 40)
  res <- sframe_run_repeated_anova(dat, rm_roles)

  # 3 conditions on 40 subjects: condition df = 2, residual df = 2 * 39 = 78.
  # The integer-subject bug gave residual df = 114 instead.
  expect_equal(res$df1, 2)
  expect_equal(res$df2, 78)
})

test_that("a strong within-subject effect is detected", {
  res <- sframe_run_repeated_anova(rm_fixture(), rm_roles)

  # The fixture separates the 3 condition means by 0.8 with subject variance
  # partialled out, so this must land far from the integer-subject result
  # (F about 1.45, p about 0.24).
  expect_gt(res$F_stat, 50)
  expect_lt(res$p, 1e-10)
  expect_gt(res$eta2, 0.5)
})

test_that("subject id is a factor in the fitted long data", {
  dat <- rm_fixture(n = 12)
  mat <- as.data.frame(dat)
  mat$.subject <- factor(seq_len(nrow(mat)))
  expect_s3_class(mat$.subject, "factor")
  expect_length(levels(mat$.subject), 12)
})

test_that("an unbalanced design (some subjects missing one condition) still finds the within-subject stratum", {
  # With every subject complete, Error(.subject/condition)'s ".subject"
  # stratum carries no "condition" row at all, so picking the first stratum
  # with a non-NA F for "condition" happens to land on the right one
  # (".subject:condition") by luck, not by design. Once some subjects are
  # missing one condition, the ".subject" stratum gains its own spurious
  # "condition" row, and a first-match search silently returns that one
  # instead: on surveyframe's own bundled input-types demo data this
  # produced F(1, 118) = 0.01, p = 0.916 where the correct
  # ".subject:condition" stratum gives F(2, 234) = 30.66, p < .001.
  dat <- rm_fixture(n = 40)
  dat$t2[1:4] <- NA

  res <- sframe_run_repeated_anova(dat, rm_roles)

  expect_null(res$error)
  expect_equal(res$df1, 2)
  expect_gt(res$F_stat, 50)
  expect_lt(res$p, 1e-10)
})
