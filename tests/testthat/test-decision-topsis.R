# tests/testthat/test-decision-topsis.R
# The TOPSIS computation, its runner's input-resolution order, and the shared
# decision ranking chart. TOPSIS is the reference implementation the other
# nine MCDM methods are built against, so the resolution order and the
# provenance fields are tested here rather than per method.

crits <- c("service", "location", "price", "delivery")
vendors <- c("Alpha", "Basilica", "Coral", "Dhoni", "Equator")

# ---------------------------------------------------------------------------
# Computation
# ---------------------------------------------------------------------------

test_that("closeness coefficients match the hand-computed values", {
  # Two alternatives, two benefit criteria, equal weights. Alternative 2 is
  # better on both, so it sits on the ideal point (score 1) and alternative 1
  # on the anti-ideal (score 0).
  x <- matrix(c(1, 2, 3, 4), nrow = 2,
              dimnames = list(c("A1", "A2"), c("c1", "c2")))
  fit <- sframe_topsis_compute(x, c(0.5, 0.5), c("benefit", "benefit"))
  expect_equal(unname(fit$scores), c(0, 1))
  expect_equal(unname(fit$ranks), c(2L, 1L))
  # Vector normalisation: column 1 divided by sqrt(1 + 4), column 2 by 5.
  expect_equal(unname(fit$normalised[, 1]), c(1, 2) / sqrt(5))
  expect_equal(unname(fit$normalised[, 2]), c(0.6, 0.8))
  expect_equal(unname(fit$d_minus[1]), 0)
  expect_equal(unname(fit$d_plus[2]), 0)
})

test_that("a cost criterion is inverted once, not twice", {
  # The regression test for the defect in the source app, where the
  # normalisation took the complement of a cost column *and* the ideal point
  # swapped for it, so the two inversions cancelled and the cheaper option
  # lost. Here the only criterion is a cost, so the lower figure must win.
  x <- matrix(c(10, 20), nrow = 2,
              dimnames = list(c("Cheap", "Dear"), "price"))
  fit <- sframe_topsis_compute(x, 1, "cost")
  expect_equal(unname(fit$scores), c(1, 0))
  expect_equal(fit$ranks[["Cheap"]], 1L)

  # The same figures read as a benefit reverse the ranking exactly.
  benefit <- sframe_topsis_compute(x, 1, "benefit")
  expect_equal(unname(benefit$scores), c(0, 1))
})

test_that("scores are invariant to the units a criterion is measured in", {
  x <- matrix(c(4.1, 3.6, 4.8, 210, 180, 260), nrow = 3,
              dimnames = list(c("a", "b", "c"), c("rating", "price")))
  rescaled <- x
  rescaled[, "price"] <- rescaled[, "price"] * 1000
  base <- sframe_topsis_compute(x, c(0.6, 0.4), c("benefit", "cost"))
  same <- sframe_topsis_compute(rescaled, c(0.6, 0.4), c("benefit", "cost"))
  expect_equal(base$scores, same$scores)
})

# ---------------------------------------------------------------------------
# Shared input validation
# ---------------------------------------------------------------------------

test_that("the shared validator reports each fault by name", {
  x <- matrix(1:4, 2, 2)
  expect_match(sframe_check_decision_input(as.data.frame(x), c(0.5, 0.5),
                                           c("benefit", "benefit"))$error,
               "numeric matrix")
  expect_match(sframe_check_decision_input(matrix(1:2, 1, 2), c(0.5, 0.5),
                                           c("benefit", "benefit"))$error,
               "at least 2 alternatives")
  expect_match(sframe_check_decision_input(matrix(c(1, NA, 3, 4), 2, 2),
                                           c(0.5, 0.5),
                                           c("benefit", "benefit"))$error,
               "missing values")
  expect_match(sframe_check_decision_input(x, c(0.5, 0.3, 0.2),
                                           c("benefit", "benefit"))$error,
               "3 weight\\(s\\) for 2 criteria")
  expect_match(sframe_check_decision_input(x, c(-1, 2),
                                           c("benefit", "benefit"))$error,
               "non-negative")
  expect_match(sframe_check_decision_input(x, c(0.5, 0.5),
                                           c("benefit", "maximise"))$error,
               "Found: maximise")
  expect_match(sframe_check_decision_input(x, c(0.5, 0.5), "benefit")$error,
               "1 criterion type\\(s\\) for 2 criteria")
})

test_that("weights that do not sum to 1 are renormalised with a note", {
  x <- matrix(1:4, 2, 2)
  checked <- sframe_check_decision_input(x, c(2, 2), c("benefit", "benefit"))
  expect_null(checked$error)
  expect_equal(checked$weights, c(0.5, 0.5))
  expect_match(checked$note, "renormalised")
})

# ---------------------------------------------------------------------------
# Fixtures for the runner
# ---------------------------------------------------------------------------

# The section 1g design target, trimmed to what RQ2 needs: one collected
# pairwise item supplying the weights, and a researcher-supplied 5 x 4
# performance matrix of audited figures.
hotel_instrument <- function(roles = list(weights_item = "crit_pairs"),
                             options = NULL) {
  options <- options %||% list(
    matrix = list(c(4.1, 3.0, 210, 36),
                  c(3.6, 4.5, 180, 48),
                  c(4.8, 2.5, 260, 24),
                  c(3.9, 4.0, 150, 72),
                  c(4.4, 3.8, 230, 30)),
    alternatives   = vendors,
    criteria       = crits,
    criteria_types = c("benefit", "benefit", "cost", "cost")
  )
  sf_instrument(
    title = "Hotel supplier selection",
    version = "1.0.0",
    components = list(
      sf_item("crit_pairs", "Compare the importance of each pair of criteria",
              type = "pairwise_comparison", comparison_items = crits,
              comparison_scale = "saaty", required = TRUE)
    ),
    analysis_plan = list(
      list(id = "RQ2",
           research_question = "Which supplier ranks best on the figures?",
           family = "decision", method = "topsis",
           roles = roles, options = options)
    )
  )
}

# Judgements proportional to (8, 4, 2, 1), so every ratio is a whole number on
# the 1-9 scale and the derived weights are known exactly.
hotel_responses <- function(n = 3) {
  data.frame(
    crit_pairs__service__vs__location  = rep(2, n),
    crit_pairs__service__vs__price     = rep(4, n),
    crit_pairs__service__vs__delivery  = rep(8, n),
    crit_pairs__location__vs__price    = rep(2, n),
    crit_pairs__location__vs__delivery = rep(4, n),
    crit_pairs__price__vs__delivery    = rep(2, n)
  )
}

# ---------------------------------------------------------------------------
# The runner and its resolution order
# ---------------------------------------------------------------------------

test_that("a supplied matrix and collected weights run end to end", {
  study <- hotel_instrument()
  results <- run_analysis_plan(hotel_responses(), study, scored = FALSE)
  res <- results[["RQ2"]]

  expect_null(res$error)
  expect_equal(res$test, "topsis")
  expect_equal(res$matrix_source, "supplied")
  expect_equal(res$weights_source, "collected")
  expect_equal(unname(res$weights), c(8, 4, 2, 1) / 15, tolerance = 1e-8)
  expect_equal(res$alternatives, vendors)

  expect_true(is.data.frame(res$table))
  expect_equal(names(res$table), c("Alternative", "Score", "Rank"))
  expect_equal(nrow(res$table), 5)
  expect_equal(res$table$Rank, 1:5)             # the table is sorted by rank
  expect_true(all(res$table$Alternative %in% vendors))
  expect_true(all(res$table$Score >= 0 & res$table$Score <= 1))
  expect_match(res$apa, "ranked first")
  expect_true(nzchar(res$prompt))

  # The consistency distribution travels with a collected pairwise weight.
  expect_lt(res$consistency$max, 1e-8)
  expect_true(any(grepl("Weights come from 3 respondent", res$notes)))

  # Citations resolve through the same path as every other method.
  expect_true(any(grepl("Hwang", unlist(res$citations))))
})

test_that("supplied weights take precedence over a collected item", {
  study <- hotel_instrument(
    roles = list(weights_item = "crit_pairs"),
    options = list(
      matrix = list(c(4.1, 3.0, 210, 36),
                    c(3.6, 4.5, 180, 48),
                    c(4.8, 2.5, 260, 24),
                    c(3.9, 4.0, 150, 72),
                    c(4.4, 3.8, 230, 30)),
      alternatives   = vendors,
      criteria       = crits,
      criteria_types = c("benefit", "benefit", "cost", "cost"),
      weights        = c(0.25, 0.25, 0.25, 0.25)
    )
  )
  res <- run_analysis_plan(hotel_responses(), study, scored = FALSE)[["RQ2"]]
  expect_null(res$error)
  expect_equal(res$weights_source, "supplied")
  expect_equal(unname(res$weights), rep(0.25, 4))
})

test_that("a fully collected performance matrix resolves through path C", {
  study <- sf_instrument(
    title = "Rated suppliers",
    components = list(
      sf_choices("q5", values = 1:5,
                 labels = c("Very poor", "Poor", "Fair", "Good", "Excellent")),
      sf_item("crit_points", "Divide 100 points", type = "criteria_weight",
              comparison_items = c("service", "price")),
      sf_item("rate_service", "Rate service", type = "matrix",
              matrix_items = c("Alpha", "Basilica"), choice_set = "q5"),
      sf_item("rate_price", "Rate value for money", type = "matrix",
              matrix_items = c("Alpha", "Basilica"), choice_set = "q5")
    ),
    analysis_plan = list(
      list(id = "RQ3", research_question = "Which supplier is rated best?",
           family = "decision", method = "topsis",
           roles = list(performance_items = c("rate_service", "rate_price"),
                        weights_item = "crit_points"),
           options = list(criteria_types = c("benefit", "benefit")))
    )
  )
  responses <- data.frame(
    crit_points__service   = c(60, 60),
    crit_points__price     = c(40, 40),
    rate_service__Alpha    = c(5, 5),
    rate_service__Basilica = c(2, 2),
    rate_price__Alpha      = c(4, 4),
    rate_price__Basilica   = c(3, 3)
  )
  res <- run_analysis_plan(responses, study, scored = FALSE)[["RQ3"]]
  expect_null(res$error)
  expect_equal(res$matrix_source, "collected")
  expect_equal(res$weights_source, "collected")
  expect_equal(unname(res$weights), c(0.6, 0.4))
  expect_equal(res$table$Alternative[1], "Alpha")
  expect_true(any(grepl("performance matrix is the respondent mean",
                        res$notes)))
})

test_that("a missing input is a typed error naming what to supply", {
  no_weights <- hotel_instrument(roles = list())
  res <- run_analysis_plan(hotel_responses(), no_weights,
                           scored = FALSE)[["RQ2"]]
  expect_match(res$error, "needs criterion weights")
  expect_match(res$error, "weights_item")

  no_matrix <- sf_instrument(
    title = "No matrix",
    components = list(
      sf_item("crit_points", "Divide 100 points", type = "criteria_weight",
              comparison_items = c("service", "price"))
    ),
    analysis_plan = list(
      list(id = "RQ1", research_question = "Which?", family = "decision",
           method = "topsis", roles = list(weights_item = "crit_points"))
    )
  )
  res2 <- run_analysis_plan(
    data.frame(crit_points__service = 60, crit_points__price = 40),
    no_matrix, scored = FALSE
  )[["RQ1"]]
  expect_match(res2$error, "needs a performance matrix")
  expect_match(res2$error, "performance_items")
})

test_that("a weight vector of the wrong length is reported, not recycled", {
  study <- sf_instrument(
    title = "Mismatch",
    components = list(
      sf_item("crit_points", "Divide 100 points", type = "criteria_weight",
              comparison_items = c("service", "price"))
    ),
    analysis_plan = list(
      list(id = "RQ1", research_question = "Which?", family = "decision",
           method = "topsis", roles = list(weights_item = "crit_points"),
           options = list(matrix = list(c(1, 2, 3), c(4, 5, 6)),
                          alternatives = c("a", "b"),
                          criteria = c("x", "y", "z")))
    )
  )
  res <- run_analysis_plan(
    data.frame(crit_points__service = 60, crit_points__price = 40),
    study, scored = FALSE
  )[["RQ1"]]
  expect_match(res$error, "weights 2 criterion/criteria but the performance")
})

test_that("the frequency weighting role is never folded into decision options", {
  # `roles$weights` names a weighting *variable* elsewhere in the engine. A
  # decision block must ignore it rather than treat the item id as a number.
  study <- hotel_instrument(
    roles = list(weights_item = "crit_pairs", weights = "crit_pairs")
  )
  res <- run_analysis_plan(hotel_responses(), study, scored = FALSE)[["RQ2"]]
  expect_null(res$error)
  expect_equal(res$weights_source, "collected")
})

# ---------------------------------------------------------------------------
# The shared ranking chart
# ---------------------------------------------------------------------------

test_that("the decision ranking chart is generic over the method", {
  skip_if_not_installed("ggplot2")
  study <- hotel_instrument()
  res <- run_analysis_plan(hotel_responses(), study, scored = FALSE,
                           plots = TRUE)[["RQ2"]]
  expect_s3_class(res$plot, "ggplot")
  expect_equal(res$plot$labels$y, "Closeness coefficient")
  expect_equal(res$plot$labels$title, "TOPSIS ranking")

  # A result carrying a differently named unit and score keeps them, so the
  # same helper serves AHP weights and every other ranking method.
  weights_result <- list(
    test = "ahp", score_label = "Priority weight",
    table = data.frame(Criterion = crits, Weight = c(0.5, 0.3, 0.15, 0.05))
  )
  p <- sframe_plot_decision_ranking(weights_result)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "Criterion")
  expect_equal(p$labels$y, "Priority weight")
  expect_equal(p$labels$title, "AHP ranking")

  expect_null(sframe_plot_decision_ranking(list(test = "topsis")))
})

test_that("a result carrying an error draws no chart", {
  skip_if_not_installed("ggplot2")
  expect_null(sframe_plot_for_result(
    list(test = "topsis", error = "no weights"), data.frame()
  ))
})
