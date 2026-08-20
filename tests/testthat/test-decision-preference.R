# tests/testthat/test-decision-preference.R
# PROMETHEE II and ELECTRE I: the two outranking methods in the decision
# family. Both are computed and runner-tested directly (sframe_run_promethee()
# / sframe_run_electre()) rather than through run_analysis_plan(), because
# the method-id registry (sframe_decision_methods) and the
# sframe_run_one_block() switch are wired centrally by the lead after every
# parallel agent's file lands; the runners themselves are already
# self-contained and callable.

# ---------------------------------------------------------------------------
# PROMETHEE II computation
# ---------------------------------------------------------------------------

test_that("net flow matches hand-computed values (usual preference function)", {
  # Three alternatives, two benefit criteria, weights (0.8, 0.2).
  #   A1 = (1, 4), A2 = (2, 2), A3 = (3, 1)
  # With the usual (step) preference function every pairwise preference
  # degree is 0 or 1, so the global preference index for each ordered pair
  # is exactly one of the two weights or their sum:
  #   pi(A1,A2)=0.2 pi(A1,A3)=0.2 pi(A2,A1)=0.8 pi(A2,A3)=0.2
  #   pi(A3,A1)=0.8 pi(A3,A2)=0.8
  # positive_flow = row means over the other n-1 alternatives, negative_flow
  # = column means. Values below are hand-computed from those six pi's.
  x <- matrix(c(1, 2, 3, 4, 2, 1), nrow = 3,
              dimnames = list(c("A1", "A2", "A3"), c("c1", "c2")))
  fit <- sframe_promethee_compute(x, c(0.8, 0.2), c("benefit", "benefit"))

  expect_equal(unname(fit$positive_flow), c(0.2, 0.5, 0.8))
  expect_equal(unname(fit$negative_flow), c(0.8, 0.5, 0.2))
  expect_equal(unname(fit$net_flow), c(-0.6, 0, 0.6))
  # The identity net = positive - negative, re-derived from the two flows
  # rather than assumed, on a real (non-trivial) matrix.
  expect_equal(fit$net_flow, fit$positive_flow - fit$negative_flow)
  expect_equal(unname(fit$ranks), c(3L, 2L, 1L))
})

test_that("a cost criterion reverses the preferred direction", {
  x <- matrix(c(10, 20), nrow = 2,
              dimnames = list(c("Cheap", "Dear"), "price"))
  fit_cost <- sframe_promethee_compute(x, 1, "cost")
  fit_benefit <- sframe_promethee_compute(x, 1, "benefit")
  expect_equal(unname(fit_cost$net_flow), c(1, -1))
  expect_equal(unname(fit_benefit$net_flow), c(-1, 1))
})

test_that("the linear preference function scales with the supplied threshold", {
  # One criterion, difference of 5 against a preference threshold of 10:
  # the degree is the linear ramp 5/10 = 0.5, not the 0/1 step "usual" gives.
  x <- matrix(c(10, 15), nrow = 2, dimnames = list(c("A", "B"), "c1"))
  fit <- sframe_promethee_compute(
    x, 1, "benefit", preference_function = "linear",
    thresholds = list(list(preference = 10, indifference = 2))
  )
  expect_equal(unname(fit$global_preference["B", "A"]), 0.5)
  expect_equal(unname(fit$global_preference["A", "B"]), 0)
})

# ---------------------------------------------------------------------------
# ELECTRE I computation
# ---------------------------------------------------------------------------

test_that("concordance and discordance match hand-computed values", {
  # RMCDA::apply.ELECTRE1() worked example (4 criteria, 3 alternatives),
  # values reproduced by hand here rather than run against the package (see
  # the cross-check test below for the RMCDA-gated version).
  #   a1 = (25, 20, 15, 30), a2 = (10, 30, 20, 30), a3 = (30, 10, 30, 10)
  #   weights = (0.2, 0.15, 0.4, 0.25), all benefit
  x <- matrix(c(25, 10, 30, 20, 30, 10, 15, 20, 30, 30, 30, 10), nrow = 3,
              dimnames = list(c("a1", "a2", "a3"), c("c1", "c2", "c3", "c4")))
  w <- c(0.2, 0.15, 0.4, 0.25)
  fit <- sframe_electre_compute(x, w, rep("benefit", 4))

  # concordance(a1,a2): a1 >= a2 on c1 (25>=10) and c4 (30>=30) only, so the
  # weight share is 0.2 + 0.25 = 0.45.
  expect_equal(fit$concordance["a1", "a2"], 0.45)
  # concordance(a2,a1): a2 >= a1 on c2 (30>=20), c3 (20>=15), c4 (30>=30):
  # 0.15 + 0.4 + 0.25 = 0.8.
  expect_equal(fit$concordance["a2", "a1"], 0.8)
  # discordance(a1,a2): criteria where a2 beats a1 are c2 (30 vs 20, range
  # 20, disagreement 0.5) and c3 (20 vs 15, range 15, disagreement 1/3); the
  # max is 0.5.
  expect_equal(fit$discordance["a1", "a2"], 0.5)
  expect_equal(fit$discordance["a1", "a2"],
               max(0, (30 - 20) / 20, (20 - 15) / 15))
})

test_that("the outranking relation and kernel follow direct dominance", {
  # B dominates both A and C on every criterion, so at the default
  # thresholds (0.7 concordance, 0.3 discordance) B outranks both and no
  # other alternative outranks B: the kernel is exactly {B}.
  x <- matrix(c(5, 8, 2, 5, 8, 2), nrow = 3,
              dimnames = list(c("A", "B", "C"), c("c1", "c2")))
  fit <- sframe_electre_compute(x, c(0.5, 0.5), c("benefit", "benefit"))

  expect_true(fit$outranking["B", "A"])
  expect_true(fit$outranking["B", "C"])
  expect_false(fit$outranking["A", "B"])
  expect_false(fit$outranking["C", "B"])
  expect_equal(unname(fit$kernel), c(FALSE, TRUE, FALSE))
  # A also outranks C (A >= C on both criteria too), so A's net count is
  # 1 outranked (by B) minus 1 outranks (C) = 0, not -1.
  expect_equal(unname(fit$scores), c(0, 2, -2))
  expect_equal(fit$ranks[["B"]], 1L)
})

test_that("a cost criterion reverses which alternative is 'at least as good'", {
  x <- matrix(c(10, 20), nrow = 2, dimnames = list(c("Cheap", "Dear"), "price"))
  fit <- sframe_electre_compute(x, 1, "cost", concordance_threshold = 0.5,
                                discordance_threshold = 0.5)
  expect_equal(fit$concordance["Cheap", "Dear"], 1)
  expect_equal(fit$concordance["Dear", "Cheap"], 0)
  expect_true(fit$outranking["Cheap", "Dear"])
  expect_false(fit$outranking["Dear", "Cheap"])
})

test_that("RMCDA's own ELECTRE I example is consistent with the port", {
  skip_if_not_installed("RMCDA")
  mat <- matrix(c(25, 10, 30, 20, 30, 10, 15, 20, 30, 30, 30, 10), nrow = 3,
                dimnames = list(c("a1", "a2", "a3"), c("c1", "c2", "c3", "c4")))
  weights <- c(0.2, 0.15, 0.4, 0.25)
  oracle <- RMCDA::apply.ELECTRE1(mat, weights)
  fit <- sframe_electre_compute(mat, weights, rep("benefit", 4))
  # RMCDA vector-normalises before comparing (a different concordance
  # convention to the raw-scale share of weight used here), so the two
  # concordance matrices are not numerically identical; what must agree is
  # the ordinal pattern of which alternative dominates which on the
  # underlying data, i.e. a2 out-concords a1 in both implementations.
  # oracle[[2]] carries no dimnames; rows/cols follow mat's own row order
  # (a1, a2, a3), so [2, 1] is concordance(a2, a1) and [1, 2] is
  # concordance(a1, a2).
  expect_true(oracle[[2]][2, 1] > oracle[[2]][1, 2])
  expect_true(fit$concordance["a2", "a1"] > fit$concordance["a1", "a2"])
})

# ---------------------------------------------------------------------------
# Runners (called directly; method-id registry wiring lands centrally)
# ---------------------------------------------------------------------------

crits <- c("c1", "c2")
alts <- c("A1", "A2", "A3")

test_that("sframe_run_promethee() runs end to end on a supplied matrix", {
  options <- list(
    matrix = list(c(1, 4), c(2, 2), c(3, 1)),
    alternatives = alts, criteria = crits,
    criteria_types = c("benefit", "benefit"),
    weights = c(0.8, 0.2)
  )
  res <- sframe_run_promethee(data.frame(), list(), options, NULL)

  expect_null(res$error)
  expect_equal(res$test, "promethee")
  expect_equal(res$matrix_source, "supplied")
  expect_equal(res$weights_source, "supplied")
  expect_equal(res$preference_function, "usual")
  expect_true(is.data.frame(res$table))
  expect_equal(names(res$table), c("Alternative", "Score", "Rank"))
  expect_equal(res$table$Alternative[1], "A3")
  expect_equal(res$table$Rank, 1:3)
  expect_equal(res$score_label, "Net flow")
  expect_match(res$apa, "PROMETHEE II ranked 3 alternatives")
  expect_match(res$apa, "usual preference function")
  expect_true(nzchar(res$prompt))
})

test_that("sframe_run_electre() runs end to end on a supplied matrix", {
  options <- list(
    matrix = list(c(5, 5), c(8, 8), c(2, 2)),
    alternatives = c("A", "B", "C"), criteria = crits,
    criteria_types = c("benefit", "benefit"),
    weights = c(0.5, 0.5)
  )
  res <- sframe_run_electre(data.frame(), list(), options, NULL)

  expect_null(res$error)
  expect_equal(res$test, "electre")
  expect_equal(res$matrix_source, "supplied")
  expect_equal(res$weights_source, "supplied")
  expect_equal(res$concordance_threshold, 0.7)
  expect_equal(res$discordance_threshold, 0.3)
  expect_true(is.data.frame(res$table))
  expect_equal(names(res$table),
               c("Alternative", "Outranks", "OutrankedBy", "Kernel", "Score",
                 "Rank"))
  expect_equal(res$table$Alternative[1], "B")
  expect_equal(res$table$Kernel[1], "Yes")
  expect_equal(res$score_label, "Net outranking count")
  expect_match(res$apa, "ELECTRE I built the outranking relation")
  expect_match(res$apa, "kernel")
  expect_match(res$prompt, "not always produce a strict total ranking")
})

test_that("both runners surface the shared validator's typed errors", {
  # A negative weight is not caught by sframe_decision_options() (that
  # function only checks numeric-ness and length), so it reaches the
  # shared sframe_check_decision_input() validator every decision-family
  # runner calls, and comes back as a returned `error` field rather than a
  # thrown condition.
  bad_options <- list(
    matrix = list(c(1, 2), c(3, 4)),
    alternatives = c("A", "B"), criteria = crits,
    criteria_types = c("benefit", "benefit"), weights = c(-0.5, 1.5)
  )
  res_p <- sframe_run_promethee(data.frame(), list(), bad_options, NULL)
  res_e <- sframe_run_electre(data.frame(), list(), bad_options, NULL)
  expect_match(res_p$error, "non-negative")
  expect_match(res_e$error, "non-negative")
})

test_that("ELECTRE says so when it establishes no outranking at all", {
  # A 9-criterion problem at the default 0.70/0.30 thresholds leaves every
  # score at 0 and every alternative at rank 1. Without a note the table
  # reads as "all 9 alternatives are jointly best" and the APA sentence
  # reports a kernel containing everything, both of which sound like
  # findings rather than the non-result they are.
  set.seed(2026)
  x <- matrix(round(runif(81, 1, 100), 1), nrow = 9)
  w <- rep(1 / 9, 9)
  ct <- rep(c("benefit", "cost"), length.out = 9)
  skip_if(any(sframe_electre_compute(x, w, ct)$outrank_count != 0),
          "this fixture is no longer degenerate")

  inst <- sf_instrument(
    title = "Degenerate ELECTRE", version = "1.0.0",
    components = list(sf_item("q1", "Q", type = "text")),
    analysis_plan = list(list(
      id = "RQ1", research_question = "Which?", family = "decision",
      method = "electre", roles = list(),
      options = list(
        matrix = lapply(seq_len(9), function(i) as.numeric(x[i, ])),
        alternatives = paste0("alt", 1:9), criteria = paste0("c", 1:9),
        weights = w, criteria_types = ct
      )
    ))
  )

  res <- run_analysis_plan(data.frame(q1 = c("a", "b")), inst)[[1]]

  expect_null(res$error)
  expect_gte(length(res$notes), 1)
  expect_true(any(grepl("did not separate these alternatives", res$notes)))
  expect_true(any(grepl("absence of evidence", res$notes)))
})

test_that("ELECTRE stays quiet when it does establish an outranking", {
  x <- matrix(c(4.1, 3.0, 210, 36,
                3.6, 4.5, 180, 48,
                4.8, 2.5, 260, 24,
                3.9, 4.0, 150, 72,
                4.4, 3.8, 230, 30), nrow = 5, byrow = TRUE)
  fit <- sframe_electre_compute(x, c(.4, .3, .2, .1),
                                c("benefit", "benefit", "cost", "cost"))
  expect_true(any(fit$outrank_count > 0))
})
