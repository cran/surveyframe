# tests/testthat/test-decision-ranking.R
# The four MCDM ranking methods added in R/decision_ranking.R: VIKOR, MOORA,
# SMART, and WASPAS. Each computation helper is checked against a
# hand-computed (or RMCDA-cross-checked) fixture, and each runner is
# exercised once end to end with a researcher-supplied matrix and weights.
# Input-resolution order, the shared validator, and the collected-weights
# path are already covered by test-decision-topsis.R and are not repeated
# here. The runners are called directly (sframe_run_vikor() etc.) rather
# than through run_analysis_plan(), because sframe_decision_methods and
# sframe_run_one_block()'s dispatch switch do not yet list these methods;
# that registration is a separate consolidation pass.

# ---------------------------------------------------------------------------
# VIKOR
# ---------------------------------------------------------------------------

test_that("VIKOR's S, R, Q and ranks match the hand-computed values", {
  # Two alternatives, two benefit criteria, equal weights -- the same
  # fixture as the TOPSIS closeness-coefficient test. By hand: f_best =
  # (2, 4), f_worst = (1, 3). Utility for A1 is (0.5, 0.5) so S1 = 1,
  # R1 = 0.5; utility for A2 is (0, 0) so S2 = 0, R2 = 0. Q1 = 1, Q2 = 0.
  x <- matrix(c(1, 2, 3, 4), nrow = 2,
              dimnames = list(c("A1", "A2"), c("c1", "c2")))
  fit <- sframe_vikor_compute(x, c(0.5, 0.5), c("benefit", "benefit"))
  expect_equal(unname(fit$Q), c(1, 0))
  expect_equal(unname(fit$S), c(1, 0))
  expect_equal(unname(fit$R), c(0.5, 0))
  expect_equal(unname(fit$scores), c(0, 1))
  expect_equal(unname(fit$ranks), c(2L, 1L))
  # n = 2, so DQ = 1 and the gap between the two Q values is exactly 1:
  # acceptable advantage holds. A2 is also the unique minimum of both S
  # and R, so acceptable stability holds too.
  expect_true(fit$acceptable_advantage)
  expect_true(fit$acceptable_stability)
})

test_that("VIKOR matches RMCDA::apply.VIKOR on an independent fixture", {
  skip_if_not_installed("RMCDA")
  # All-benefit so no sign flip is needed (apply.VIKOR has no cb argument;
  # RMCDA's own doc example pre-negates cost columns before calling it).
  x <- matrix(c(10, 20, 15, 5, 3, 8), nrow = 3,
              dimnames = list(c("A1", "A2", "A3"), c("c1", "c2")))
  w <- c(0.6, 0.4)
  fit <- sframe_vikor_compute(x, w, c("benefit", "benefit"))
  oracle <- RMCDA::apply.VIKOR(x, w)
  expect_equal(unname(fit$Q), unname(oracle[[2]]), tolerance = 1e-8)
  expect_equal(unname(fit$S), unname(oracle[[3]]), tolerance = 1e-8)
  expect_equal(unname(fit$R), unname(oracle[[4]]), tolerance = 1e-8)
})

test_that("VIKOR's acceptance conditions can both fail", {
  # A 4-alternative, 3-criterion, unequally weighted fixture found by random
  # search and confirmed independently in R (not via sframe_vikor_compute)
  # against the Opricovic & Tzeng formulas: f_best/f_worst from the column
  # max/min, utility = w * (f_best - x) / spread, S = rowSums, R = rowMax,
  # Q = 0.5 * S-term + 0.5 * R-term. A1 is the Q winner but neither the
  # unique S winner (A3) nor the unique R winner (A2), so acceptable
  # stability fails; the Q gap between A1 (0.107356) and A3 (0.128233) is
  # 0.0209, below DQ = 1 / (4 - 1) = 0.3333, so acceptable advantage fails
  # too.
  x <- matrix(c(26, 25, 16, 10,
                6, 10, 29, 26,
                18, 13, 19, 28), nrow = 4,
              dimnames = list(c("A1", "A2", "A3", "A4"),
                              c("c1", "c2", "c3")))
  w <- c(0.5, 0.3, 0.2)
  fit <- sframe_vikor_compute(x, w, rep("benefit", 3))
  expect_equal(unname(fit$Q),
               c(0.107356, 0.218400, 0.128233, 1.000000), tolerance = 1e-5)
  expect_equal(unname(fit$ranks), c(1L, 3L, 2L, 4L))
  expect_false(fit$acceptable_advantage)
  expect_false(fit$acceptable_stability)
})

test_that("VIKOR's acceptance advantage alone can fail", {
  # A single benefit criterion, three alternatives: with one criterion
  # S = R = utility, so Q reduces to the normalised utility itself and the
  # Q winner is automatically also the S and R winner (stability holds by
  # construction). f_best = 21, f_worst = 10, spread = 11: utility(10) = 1,
  # utility(20) = 1/11, utility(21) = 0, so Q = (1, 1/11, 0) in that
  # alternative order.
  x <- matrix(c(10, 20, 21), nrow = 3,
              dimnames = list(c("Low", "Mid", "High"), "c1"))
  fit <- sframe_vikor_compute(x, 1, "benefit")
  expect_equal(unname(fit$Q), c(1, 1 / 11, 0), tolerance = 1e-8)
  # DQ = 1 / (3 - 1) = 0.5; the gap between High (0) and Mid (1/11) is
  # 1/11 = 0.0909, well under 0.5.
  expect_false(fit$acceptable_advantage)
  expect_true(fit$acceptable_stability)
})

test_that("sframe_run_vikor runs end to end on a supplied matrix", {
  options <- list(
    matrix = list(c(10, 5), c(20, 3), c(15, 8)),
    alternatives = c("A1", "A2", "A3"),
    criteria = c("c1", "c2"),
    criteria_types = c("benefit", "cost"),
    weights = c(0.6, 0.4)
  )
  res <- sframe_run_vikor(data.frame(), roles = list(), options = options,
                          instrument = NULL)
  expect_null(res$error)
  expect_equal(res$test, "vikor")
  expect_equal(res$matrix_source, "supplied")
  expect_equal(res$weights_source, "supplied")
  expect_true(is.data.frame(res$table))
  expect_equal(names(res$table), c("Alternative", "Score", "Rank"))
  expect_equal(res$table$Rank, 1:3)
  expect_equal(res$table$Alternative[1], "A2")   # cheapest and near-top c1
  expect_match(res$apa, "VIKOR ranked 3 alternatives")
  expect_true(nzchar(res$prompt))
  expect_true(is.logical(res$acceptable_advantage))
  expect_true(is.logical(res$acceptable_stability))
})

# ---------------------------------------------------------------------------
# MOORA
# ---------------------------------------------------------------------------

test_that("MOORA's ratio-system scores match the hand-computed values", {
  # One benefit, one cost criterion, equal weights. Vector norms: col1
  # sqrt(1^2 + 2^2) = sqrt(5), col2 sqrt(3^2 + 4^2) = 5. Weighted:
  # A1 = (0.2236068, 0.3), A2 = (0.4472136, 0.4). Score = benefit - cost:
  # A1 = 0.2236068 - 0.3 = -0.0763932, A2 = 0.4472136 - 0.4 = 0.0472136.
  x <- matrix(c(1, 2, 3, 4), nrow = 2,
              dimnames = list(c("A1", "A2"), c("c1", "c2")))
  fit <- sframe_moora_compute(x, c(0.5, 0.5), c("benefit", "cost"))
  expect_equal(unname(fit$scores), c(-0.0763932, 0.0472136), tolerance = 1e-6)
  expect_equal(unname(fit$ranks), c(2L, 1L))
})

test_that("MOORA matches RMCDA::apply.MOORA on its own worked example", {
  skip_if_not_installed("RMCDA")
  # RMCDA's apply.MOORA() @examples fixture: 7 robot alternatives, 5
  # criteria, criteria 1, 3, 4, 5 beneficial and criterion 2 (repeatability)
  # a cost.
  x <- matrix(c(60, 6.35, 6.8, 10, 2.5, 4.5, 3,
                0.4, 0.15, 0.1, 0.2, 0.1, 0.08, 0.1,
                2540, 1016, 1727.2, 1000, 560, 1016, 177,
                500, 3000, 1500, 2000, 500, 350, 1000,
                990, 1041, 1676, 965, 915, 508, 920), nrow = 7)
  rownames(x) <- paste0("A", 1:7)
  w <- c(0.1574, 0.1825, 0.2385, 0.2172, 0.2043)
  ben_idx <- c(1, 3, 4, 5)
  criteria_types <- rep("cost", ncol(x))
  criteria_types[ben_idx] <- "benefit"
  fit <- sframe_moora_compute(x, w, criteria_types)
  oracle <- RMCDA::apply.MOORA(x, w, ben_idx)
  expect_equal(unname(fit$scores), unname(oracle), tolerance = 1e-6)
})

test_that("sframe_run_moora runs end to end on a supplied matrix", {
  options <- list(
    matrix = list(c(1, 3), c(2, 4)),
    alternatives = c("A1", "A2"),
    criteria = c("c1", "c2"),
    criteria_types = c("benefit", "cost"),
    weights = c(0.5, 0.5)
  )
  res <- sframe_run_moora(data.frame(), roles = list(), options = options,
                          instrument = NULL)
  expect_null(res$error)
  expect_equal(res$test, "moora")
  expect_equal(res$matrix_source, "supplied")
  expect_equal(res$weights_source, "supplied")
  expect_equal(names(res$table), c("Alternative", "Score", "Rank"))
  expect_equal(res$table$Rank, 1:2)
  expect_equal(res$table$Alternative[1], "A2")
  expect_match(res$apa, "MOORA \\(ratio system\\)")
  expect_true(!is.null(res$reference_ranks))
})

# ---------------------------------------------------------------------------
# SMART
# ---------------------------------------------------------------------------

test_that("SMART's weighted utility values match the hand-computed values", {
  # One benefit, one cost criterion, weights (0.6, 0.4). Min-max
  # normalisation: col1 (benefit, range 10-20) gives (0, 1, 0.5); col2
  # (cost, range 2-8) gives (0.5, 0, 1). Utility = 0.6 * col1n + 0.4 *
  # col2n: A1 = 0 + 0.2 = 0.2, A2 = 0.6 + 0 = 0.6, A3 = 0.3 + 0.4 = 0.7.
  x <- matrix(c(10, 20, 15, 5, 8, 2), nrow = 3,
              dimnames = list(c("A1", "A2", "A3"), c("c1", "c2")))
  fit <- sframe_smart_compute(x, c(0.6, 0.4), c("benefit", "cost"))
  expect_equal(unname(fit$scores), c(0.2, 0.6, 0.7), tolerance = 1e-8)
  expect_equal(unname(fit$ranks), c(3L, 2L, 1L))
})

test_that("SMART is invariant to the units a criterion is measured in", {
  x <- matrix(c(10, 20, 15, 5, 8, 2), nrow = 3,
              dimnames = list(c("A1", "A2", "A3"), c("c1", "c2")))
  rescaled <- x
  rescaled[, "c1"] <- rescaled[, "c1"] * 1000
  base <- sframe_smart_compute(x, c(0.6, 0.4), c("benefit", "cost"))
  same <- sframe_smart_compute(rescaled, c(0.6, 0.4), c("benefit", "cost"))
  expect_equal(base$scores, same$scores)
})

test_that("sframe_run_smart runs end to end on a supplied matrix", {
  options <- list(
    matrix = list(c(10, 5), c(20, 8), c(15, 2)),
    alternatives = c("A1", "A2", "A3"),
    criteria = c("c1", "c2"),
    criteria_types = c("benefit", "cost"),
    weights = c(0.6, 0.4)
  )
  res <- sframe_run_smart(data.frame(), roles = list(), options = options,
                          instrument = NULL)
  expect_null(res$error)
  expect_equal(res$test, "smart")
  expect_equal(res$matrix_source, "supplied")
  expect_equal(res$weights_source, "supplied")
  expect_equal(res$table$Rank, 1:3)
  expect_equal(res$table$Alternative[1], "A3")
  expect_match(res$apa, "SMART ranked 3 alternatives")
})

# ---------------------------------------------------------------------------
# WASPAS
# ---------------------------------------------------------------------------

test_that("WASPAS's WSM/WPM blend matches the hand-computed values", {
  # Same matrix and weights as the SMART fixture, but WASPAS normalises
  # each column by ratio to its own best (benefit: x / max(x); cost:
  # min(x) / x), not min-max to [0, 1]. Normalised: col1 (0.5, 1, 0.75),
  # col2 (0.4, 0.25, 1). WSM = 0.6 * col1n + 0.4 * col2n:
  # A1 = 0.3 + 0.16 = 0.46, A2 = 0.6 + 0.1 = 0.70, A3 = 0.45 + 0.4 = 0.85.
  # WPM = col1n^0.6 * col2n^0.4, computed in R to full precision.
  x <- matrix(c(10, 20, 15, 5, 8, 2), nrow = 3,
              dimnames = list(c("A1", "A2", "A3"), c("c1", "c2")))
  fit <- sframe_waspas_compute(x, c(0.6, 0.4), c("benefit", "cost"),
                               lambda = 0.5)
  expect_equal(unname(fit$normalised[, "c1"]), c(0.5, 1, 0.75))
  expect_equal(unname(fit$normalised[, "c2"]), c(0.4, 0.25, 1))
  expect_equal(unname(fit$wsm_scores), c(0.46, 0.70, 0.85), tolerance = 1e-8)
  expect_equal(unname(fit$scores),
               unname(0.5 * fit$wsm_scores + 0.5 * fit$wpm_scores))
  expect_equal(unname(fit$ranks), c(3L, 2L, 1L))
})

test_that("WASPAS matches RMCDA::apply.WASPAS on its own worked example", {
  skip_if_not_installed("RMCDA")
  x <- matrix(c(0.04, 0.11, 0.05, 0.02, 0.08, 0.05, 0.03, 0.1, 0.03,
                1.137, 0.854, 1.07, 0.524, 0.596, 0.722, 0.521, 0.418, 0.62,
                960, 1920, 3200, 1280, 2400, 1920, 1600, 1440, 2560), nrow = 9)
  rownames(x) <- paste0("A", 1:9)
  bv <- 3
  w <- c(0.1047, 0.2583, 0.6369)
  criteria_types <- rep("cost", ncol(x))
  criteria_types[bv] <- "benefit"
  fit <- sframe_waspas_compute(x, w, criteria_types, lambda = 0.5)
  oracle <- suppressWarnings(RMCDA::apply.WASPAS(x, w, bv, 0.5))
  expect_equal(unname(fit$scores), unname(as.numeric(oracle)), tolerance = 1e-6)
})

test_that("sframe_run_waspas runs end to end on a supplied matrix", {
  options <- list(
    matrix = list(c(10, 5), c(20, 8), c(15, 2)),
    alternatives = c("A1", "A2", "A3"),
    criteria = c("c1", "c2"),
    criteria_types = c("benefit", "cost"),
    weights = c(0.6, 0.4)
  )
  res <- sframe_run_waspas(data.frame(), roles = list(), options = options,
                           instrument = NULL)
  expect_null(res$error)
  expect_equal(res$test, "waspas")
  expect_equal(res$matrix_source, "supplied")
  expect_equal(res$weights_source, "supplied")
  expect_equal(res$table$Rank, 1:3)
  expect_equal(res$table$Alternative[1], "A3")
  expect_match(res$apa, "WASPAS ranked 3 alternatives")
  expect_equal(res$lambda, 0.5)

  # A researcher-supplied lambda overrides the default.
  options$lambda <- 0.2
  res2 <- sframe_run_waspas(data.frame(), roles = list(), options = options,
                            instrument = NULL)
  expect_equal(res2$lambda, 0.2)
})
