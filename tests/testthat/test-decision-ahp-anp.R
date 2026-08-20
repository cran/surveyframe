# tests/testthat/test-decision-ahp-anp.R
# AHP and ANP: the eigenvector weight computation, the Saaty consistency
# ratio's two branches and its n > 10 error path, ANP's power-iteration
# convergence guard, and an end-to-end pairwise_comparison collection
# fixture for each, following the pattern in test-decision-topsis.R.

crits3 <- c("price", "quality", "speed")

# A perfectly consistent 3 x 3 judgement matrix: price is twice as important
# as quality, four times as important as speed, and quality is twice as
# important as speed (2 x 2 = 4, so the ratios are transitive and lambda_max
# = n exactly). Cross-checked below against RMCDA::find.weight(), which
# returns the same weights via its column-normalise/row-mean approximation
# rather than the eigenvector, and independently against a fresh
# `eigen()` call.
consistent_matrix <- matrix(
  c(1,   2,   4,
    0.5, 1,   2,
    0.25, 0.5, 1),
  nrow = 3, byrow = TRUE,
  dimnames = list(crits3, crits3)
)

# A textbook example of an intransitive (highly inconsistent) matrix: A is
# judged much more important than B, B much more important than C, but C
# is also judged more important than A. Values verified by hand via
# `eigen()`: lambda_max = 4.838038, CI = (4.838038 - 3) / 2 = 0.919019,
# RI(3) = 0.58, CR = 1.584515, comfortably at or above the 0.10 threshold.
inconsistent_matrix <- matrix(
  c(1,   5,   1/3,
    0.2, 1,   3,
    3,   1/3, 1),
  nrow = 3, byrow = TRUE,
  dimnames = list(crits3, crits3)
)

# ---------------------------------------------------------------------------
# sframe_ahp_cr() and sframe_ahp_compute()
# ---------------------------------------------------------------------------

test_that("a perfectly consistent matrix has CR 0 and lambda_max == n", {
  fit <- sframe_ahp_compute(consistent_matrix)
  expect_null(fit$error)
  # Hand-computed: price:quality:speed ratios are exactly 4:2:1.
  expect_equal(unname(fit$weights), c(4, 2, 1) / 7, tolerance = 1e-6)
  expect_equal(fit$lambda_max, 3, tolerance = 1e-6)
  expect_equal(fit$cr, 0, tolerance = 1e-6)
  expect_false(fit$consistency_warning)
})

test_that("the consistent fixture matches RMCDA's find.weight() as an oracle", {
  skip_if_not_installed("RMCDA")
  # find.weight() is internal to RMCDA (not exported), used here only as a
  # numeric cross-check on the weights, not adopted as a calling convention.
  res <- RMCDA:::find.weight(consistent_matrix)
  expect_equal(unname(sframe_ahp_compute(consistent_matrix)$weights),
               unname(res[[2]]), tolerance = 1e-6)
  expect_equal(res[[1]], 0, tolerance = 1e-6)
})

test_that("an intransitive matrix crosses the 0.10 consistency threshold", {
  fit <- sframe_ahp_compute(inconsistent_matrix)
  expect_null(fit$error)
  expect_equal(fit$lambda_max, 4.838038, tolerance = 1e-5)
  expect_equal(fit$cr, 1.584515, tolerance = 1e-4)
  expect_true(fit$cr >= 0.10)
  expect_true(fit$consistency_warning)
})

test_that("a matrix beyond n = 10 has no defined consistency ratio", {
  m11 <- matrix(1, nrow = 11, ncol = 11,
                dimnames = list(paste0("c", 1:11), paste0("c", 1:11)))
  cr <- sframe_ahp_cr(m11)
  expect_match(cr$error, "only defined for up to 10 criteria")

  fit <- sframe_ahp_compute(m11)
  expect_match(fit$error, "only defined for up to 10 criteria")
})

test_that("sframe_ahp_compute() reports non-matrix and shape faults", {
  expect_match(sframe_ahp_compute(as.data.frame(consistent_matrix))$error,
               "numeric matrix")
  expect_match(sframe_ahp_compute(matrix(1, 2, 3))$error, "square")
  expect_match(sframe_ahp_compute(matrix(1, 1, 1))$error, "at least 2")
  bad <- consistent_matrix
  bad[1, 2] <- NA
  expect_match(sframe_ahp_compute(bad)$error, "missing values")
})

# ---------------------------------------------------------------------------
# AHP runner: resolution order and output shape
#
# `sframe_run_one_block()`'s dispatch switch (R/analysis_plan.R) does not
# yet carry an "ahp"/"anp" case: wiring every new decision method into that
# switch, `sframe_plot_for_result()`'s switch, and the two JS UI registries
# is an explicit later pass by the lead across all 4 parallel agents, kept
# out of this task to avoid a 4-way merge conflict on those exact files. So
# these tests call `sframe_run_ahp()`/`sframe_run_anp()` directly with the
# same (data, roles, options, instrument) shape `sframe_run_one_block()`
# will eventually pass them, rather than going through
# `run_analysis_plan()`, which would currently return "Test 'ahp' is
# unavailable."
# ---------------------------------------------------------------------------

ahp_instrument <- function() {
  sf_instrument(
    title = "AHP criterion weighting",
    version = "1.0.0",
    components = list(
      sf_item("crit_pairs", "Compare the importance of each pair of criteria",
              type = "pairwise_comparison", comparison_items = crits3,
              comparison_scale = "saaty", required = TRUE)
    )
  )
}

# Judgements proportional to (4, 2, 1), same ratios as consistent_matrix, so
# every respondent agrees exactly and the aggregate is that exact matrix.
ahp_responses <- function(n = 3) {
  data.frame(
    crit_pairs__price__vs__quality = rep(2, n),
    crit_pairs__price__vs__speed   = rep(4, n),
    crit_pairs__quality__vs__speed = rep(2, n)
  )
}

test_that("a researcher-supplied matrix takes precedence over a collected item", {
  study <- ahp_instrument()
  res <- sframe_run_ahp(ahp_responses(),
                        roles = list(pairwise = "crit_pairs"),
                        options = list(matrix = consistent_matrix),
                        instrument = study)
  expect_null(res$error)
  expect_equal(res$test, "ahp")
  expect_equal(res$matrix_source, "supplied")
  expect_equal(unname(res$weights), c(4, 2, 1) / 7, tolerance = 1e-6)
})

test_that("a collected pairwise item runs end to end through the runner", {
  study <- ahp_instrument()
  res <- sframe_run_ahp(ahp_responses(),
                        roles = list(pairwise = "crit_pairs"),
                        options = NULL, instrument = study)

  expect_null(res$error)
  expect_equal(res$matrix_source, "collected")
  expect_equal(unname(res$weights), c(4, 2, 1) / 7, tolerance = 1e-6)
  expect_true(is.data.frame(res$table))
  expect_equal(names(res$table), c("Criterion", "Weight", "Rank"))
  expect_equal(res$table$Criterion[1], "price")
  expect_equal(res$table$Rank, 1:3)
  expect_equal(res$score_label, "Priority weight")
  expect_match(res$apa, "AHP derived priority weights")
  expect_true(nzchar(res$prompt))
  expect_false(res$consistency_warning)
  expect_true(any(grepl("Weights come from 3 respondent", res$notes)))
})

test_that("an inconsistent collected matrix sets the consistency warning note", {
  study <- ahp_instrument()
  responses <- data.frame(
    crit_pairs__price__vs__quality = 5,
    crit_pairs__price__vs__speed   = -3,  # signed-integer encoding of 1/3
    crit_pairs__quality__vs__speed = 3
  )
  res <- sframe_run_ahp(responses, roles = list(pairwise = "crit_pairs"),
                        options = NULL, instrument = study)
  expect_null(res$error)
  expect_true(res$consistency_warning)
  expect_true(res$cr >= 0.10)
  expect_true(any(grepl("inconsistent", res$notes)))
})

test_that("AHP without a matrix or a pairwise role is a typed error", {
  study <- ahp_instrument()
  res <- sframe_run_ahp(ahp_responses(), roles = list(), options = NULL,
                        instrument = study)
  expect_match(res$error, "needs a pairwise judgement matrix")
  expect_match(res$error, "pairwise")
})

test_that("the shared ranking chart draws AHP's weight-only result", {
  skip_if_not_installed("ggplot2")
  study <- ahp_instrument()
  res <- sframe_run_ahp(ahp_responses(),
                        roles = list(pairwise = "crit_pairs"),
                        options = NULL, instrument = study)
  p <- sframe_plot_decision_ranking(res)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$y, "Priority weight")
  expect_equal(p$labels$title, "AHP ranking")
})

# ---------------------------------------------------------------------------
# sframe_anp_compute(): limiting priorities and the convergence guard
# ---------------------------------------------------------------------------

anp_matrix <- matrix(
  c(0, 4, 1,
    2, 0, 3,
    1, 2, 0),
  nrow = 3, byrow = TRUE,
  dimnames = list(crits3, crits3)
)

test_that("the limiting priority vector matches a hand-verified power iteration", {
  # Verified interactively: column-normalising anp_matrix and iterating the
  # priority vector from a uniform start converges after 43 steps (tol
  # 1e-8) to (0.3375, 0.4125, 0.2500), summing to 1.
  fit <- sframe_anp_compute(anp_matrix)
  expect_null(fit$error)
  expect_true(fit$converged)
  expect_equal(unname(fit$weights), c(0.3375, 0.4125, 0.2500), tolerance = 1e-3)
  expect_equal(sum(fit$weights), 1, tolerance = 1e-8)
  expect_true(fit$iterations > 1)
})

test_that("the convergence guard reports an error rather than a partial result", {
  fit <- sframe_anp_compute(anp_matrix, max_iter = 2)
  expect_null(fit$weights)
  expect_match(fit$error, "did not converge within 2 iterations")
})

test_that("sframe_anp_compute() reports shape and value faults", {
  expect_match(sframe_anp_compute(matrix(1, 2, 3))$error, "square")
  expect_match(sframe_anp_compute(matrix(1, 1, 1))$error, "at least 2")
  bad <- anp_matrix; bad[1, 2] <- NA
  expect_match(sframe_anp_compute(bad)$error, "missing values")
  bad2 <- anp_matrix; bad2[1, 2] <- -1
  expect_match(sframe_anp_compute(bad2)$error, "negative")
})

test_that("a zero-sum column is spread evenly rather than producing NaN", {
  m <- matrix(c(0, 0, 1, 2, 0, 3, 1, 2, 0), nrow = 3, byrow = TRUE)
  fit <- sframe_anp_compute(m)
  expect_null(fit$error)
  expect_false(any(is.na(fit$weights)))
})

# ---------------------------------------------------------------------------
# ANP runner: resolution order, output shape, and options$max_iter
# ---------------------------------------------------------------------------

anp_instrument <- function() {
  sf_instrument(
    title = "ANP network weighting",
    version = "1.0.0",
    components = list(
      sf_item("crit_pairs", "Compare the influence of each pair of criteria",
              type = "pairwise_comparison", comparison_items = crits3,
              comparison_scale = "saaty", required = TRUE)
    )
  )
}

test_that("a supplied supermatrix runs end to end and reports iterations", {
  study <- anp_instrument()
  res <- sframe_run_anp(ahp_responses(),
                        roles = list(pairwise = "crit_pairs"),
                        options = list(matrix = anp_matrix),
                        instrument = study)
  expect_null(res$error)
  expect_equal(res$test, "anp")
  expect_equal(res$matrix_source, "supplied")
  expect_true(is.data.frame(res$table))
  expect_equal(names(res$table), c("Node", "Weight", "Rank"))
  expect_equal(res$score_label, "Limiting priority weight")
  expect_true(res$converged)
  expect_true(res$iterations > 1)
  expect_match(res$apa, "ANP resolved the limiting priorities")
})

test_that("a collected pairwise item resolves ANP's supermatrix end to end", {
  study <- anp_instrument()
  res <- sframe_run_anp(ahp_responses(),
                        roles = list(pairwise = "crit_pairs"),
                        options = NULL, instrument = study)
  expect_null(res$error)
  expect_equal(res$matrix_source, "collected")
  # The aggregated matrix is consistent_matrix (perfect agreement across
  # respondents), so its limiting priorities equal its AHP eigenvector
  # exactly: a reciprocal, consistent matrix is already a valid stochastic
  # generator once column-normalised.
  expect_equal(unname(res$weights), c(4, 2, 1) / 7, tolerance = 1e-4)
})

test_that("options$max_iter reaches the runner and surfaces the guard as an error", {
  study <- anp_instrument()
  res <- sframe_run_anp(ahp_responses(),
                        roles = list(pairwise = "crit_pairs"),
                        options = list(matrix = anp_matrix, max_iter = 2),
                        instrument = study)
  expect_match(res$error, "did not converge within 2 iterations")
})

test_that("ANP without a matrix or a pairwise role is a typed error", {
  study <- anp_instrument()
  res <- sframe_run_anp(ahp_responses(), roles = list(), options = NULL,
                        instrument = study)
  expect_match(res$error, "needs a pairwise judgement matrix")
})
