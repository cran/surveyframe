# tests/testthat/test-decision-alignment.R
#
# Criterion identity in the decision layer.
#
# D1. Weights were matched to the performance matrix by count alone, then
#     applied by position. A matrix ordered (price, quality) with a weight item
#     ordered (quality, price) at 90 and 10 applied 90 to price. Supplied named
#     weights and criterion types lost their names the same way.
# D2. Aggregation stacked judgement matrices by position and stamped the first
#     matrix's labels on the result. The same A-over-B judgement of 9, from one
#     matrix ordered A,B and one ordered B,A, averaged to indifference.
# D3. A data-frame options$matrix was treated as a list of rows, so AHP and ANP
#     read it transposed, reversing every preference with perfect consistency.
# Batch 3, finding 4. Factor judgements were read as level positions.
# Batch 3, finding 5. A supplied AHP matrix got a consistency verdict without
#     being positive, finite, unit-diagonal or reciprocal.
# Batch 3, finding 9. Unavailable consistency ratios became Inf and NaN
#     summaries, and filtering reported them as too inconsistent.

weights_instrument <- function(order = c("quality", "price")) {
  sf_instrument(title = "Weights", components = list(
    sf_item("w", "Divide 100 points", "criteria_weight",
            comparison_items = order)))
}

supplied_matrix <- function() {
  # A trade-off: A1 is better on quality, A2 on price, so weights decide.
  list(matrix = list(c(10, 5), c(20, 3)), alternatives = c("A1", "A2"),
       criteria = c("price", "quality"))
}

test_that("D1: collected weights are aligned to the matrix by criterion name", {
  data <- data.frame(w__quality = c(90, 90), w__price = c(10, 10))
  res <- sframe_resolve_decision_inputs(data, list(weights_item = "w"),
                                        supplied_matrix(), weights_instrument(),
                                        method = "TOPSIS")
  expect_null(res$error)
  expect_equal(res$weights[["price"]], 0.1)
  expect_equal(res$weights[["quality"]], 0.9)
  expect_identical(names(res$weights), c("price", "quality"))
})

test_that("D1: a ranking does not change when the weight item is reordered", {
  opts <- supplied_matrix()
  a <- sframe_run_topsis(data.frame(w__quality = 90, w__price = 10),
                         list(weights_item = "w"), opts,
                         weights_instrument(c("quality", "price")))
  b <- sframe_run_topsis(data.frame(w__price = 10, w__quality = 90),
                         list(weights_item = "w"), opts,
                         weights_instrument(c("price", "quality")))
  expect_null(a$error)
  expect_equal(a$table, b$table)
  # and price, weighted 10, does not decide it: A1 wins on quality
  expect_identical(a$table$Alternative[a$table$Rank == 1], "A1")
})

test_that("D1: collected weights for different criteria are refused", {
  ins <- weights_instrument(c("quality", "speed"))
  data <- data.frame(w__quality = 50, w__speed = 50)
  res <- sframe_resolve_decision_inputs(data, list(weights_item = "w"),
                                        supplied_matrix(), ins, method = "TOPSIS")
  expect_false(is.null(res$error))
  expect_match(res$error, "speed")
  expect_match(res$error, "price")
})

test_that("D1: supplied named weights and types are aligned by name", {
  opts <- c(supplied_matrix(),
            list(weights = list(quality = 0.9, price = 0.1),
                 criteria_types = c(quality = "benefit", price = "cost")))
  res <- sframe_resolve_decision_inputs(data.frame(), list(), opts,
                                        weights_instrument(), method = "TOPSIS")
  expect_equal(unname(res$weights), c(0.1, 0.9))
  expect_identical(unname(res$criteria_types), c("cost", "benefit"))
})

test_that("D1: supplied named weights for other criteria are refused", {
  opts <- c(supplied_matrix(), list(weights = c(quality = 0.5, speed = 0.5)))
  expect_error(
    sframe_resolve_decision_inputs(data.frame(), list(), opts,
                                   weights_instrument(), method = "TOPSIS"),
    "speed", class = "sframe_error")
})

reciprocal <- function(labels, ab) {
  m <- matrix(1, 2, 2, dimnames = list(labels, labels))
  m[1, 2] <- ab; m[2, 1] <- 1 / ab
  m
}

test_that("D2: the same judgement in a different order aggregates to itself", {
  ab <- reciprocal(c("A", "B"), 9)     # A over B = 9
  ba <- reciprocal(c("B", "A"), 1 / 9) # B over A = 1/9, the same judgement
  agg <- sframe_aggregate_judgements(list(ab, ba))$matrix
  expect_equal(agg["A", "B"], 9)
  expect_equal(agg["B", "A"], 1 / 9)
})

test_that("D2: matrices naming different criteria are refused", {
  expect_error(
    sframe_aggregate_judgements(list(reciprocal(c("A", "B"), 3),
                                     reciprocal(c("A", "C"), 3))),
    class = "sframe_error")
})

test_that("D2: non-square and non-finite matrices are refused", {
  expect_error(sframe_aggregate_judgements(list(matrix(1, 2, 3))),
               class = "sframe_error")
  bad <- reciprocal(c("A", "B"), 3); bad[1, 2] <- Inf
  expect_error(sframe_aggregate_judgements(list(bad)), class = "sframe_error")
  # Arithmetic aggregation, used for DEMATEL, has no reciprocity check, so the
  # finiteness check is the only thing standing between it and an Inf mean.
  influence <- matrix(c(0, 2, Inf, 0), 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  expect_error(sframe_aggregate_judgements(list(influence), method = "arithmetic"),
               class = "sframe_error")
})

test_that("D3: a data-frame matrix is read as written, the same as a matrix", {
  m <- matrix(c(1, 1 / 9, 9, 1), 2, 2)   # row 1: (1, 9), A over B = 9
  as_df <- as.data.frame(m)
  as_rows <- list(c(1, 9), c(1 / 9, 1))
  resolve <- function(x) {
    sframe_resolve_pairwise_matrix(data.frame(), list(),
                                   list(matrix = x, criteria = c("A", "B")),
                                   NULL, method = "AHP")$matrix
  }
  expect_equal(resolve(as_df), resolve(m))
  expect_equal(resolve(as_rows), resolve(m))
  expect_equal(resolve(as_df)["A", "B"], 9)
})

pairwise_instrument <- function() {
  sf_instrument(title = "Pairwise", components = list(
    sf_item("pw", "Compare", "pairwise_comparison",
            comparison_items = c("price", "quality"))))
}

test_that("4: factor judgements are read through their labels", {
  numeric_data <- data.frame(pw__price__vs__quality = c(-9, 9))
  factor_data <- data.frame(pw__price__vs__quality = factor(c("-9", "9")))
  a <- sframe_assemble_pairwise(numeric_data, pairwise_instrument(), "pw")
  b <- sframe_assemble_pairwise(factor_data, pairwise_instrument(), "pw")
  expect_equal(b$matrices, a$matrices)
  expect_identical(b$n_respondents, 2L)
})

test_that("4: factor point allocations are read through their labels", {
  ins <- weights_instrument(c("price", "quality"))
  # Asymmetric, so level positions and labels give different weights.
  numeric_data <- data.frame(w__price = c(30, 60), w__quality = c(70, 40))
  factor_data <- data.frame(w__price = factor(c("30", "60")),
                            w__quality = factor(c("70", "40")))
  expect_equal(sframe_collected_weights(factor_data, ins, "w")$weights,
               sframe_collected_weights(numeric_data, ins, "w")$weights)
})

test_that("5: a supplied AHP matrix must be a positive reciprocal matrix", {
  not_reciprocal <- matrix(c(1, 9, 9, 1), 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  bad_diagonal <- reciprocal(c("A", "B"), 3); bad_diagonal[1, 1] <- 2
  zero <- reciprocal(c("A", "B"), 3); zero[1, 2] <- 0
  for (m in list(not_reciprocal, bad_diagonal, zero)) {
    res <- sframe_ahp_compute(m)
    expect_false(is.null(res$error))
  }
  expect_null(sframe_ahp_compute(reciprocal(c("A", "B"), 3))$error)
})

test_that("9: unavailable consistency ratios are reported as unavailable", {
  eleven <- matrix(1, 11, 11, dimnames = list(letters[1:11], letters[1:11]))
  s <- sframe_consistency_summary(list(eleven, eleven))
  expect_true(is.na(s$min) && is.na(s$max) && is.na(s$share_above))
  expect_identical(s$n_unavailable, 2L)

  err <- tryCatch(sframe_aggregate_judgements(list(eleven, eleven), cr_filter = TRUE),
                  sframe_error = function(e) conditionMessage(e))
  expect_match(err, "unavailable")
  expect_false(grepl("at or above", err, fixed = TRUE))
})

test_that("D1: rated items pair with differently named weights in declared order, and say so", {
  ins <- sf_instrument(title = "Rated", components = list(
    sf_choices("r5", 1:5, as.character(1:5)),
    sf_item("rate_price", "Price", "matrix", choice_set = "r5",
            matrix_items = c("A1", "A2")),
    sf_item("rate_quality", "Quality", "matrix", choice_set = "r5",
            matrix_items = c("A1", "A2")),
    sf_item("w", "Divide 100 points", "criteria_weight",
            comparison_items = c("price", "quality"))))
  data <- data.frame(rate_price__A1 = 2, rate_price__A2 = 4,
                     rate_quality__A1 = 5, rate_quality__A2 = 3,
                     w__price = 25, w__quality = 75)
  res <- sframe_resolve_decision_inputs(
    data, list(performance_items = c("rate_price", "rate_quality"),
               weights_item = "w"), list(), ins, method = "TOPSIS")
  expect_null(res$error)
  expect_equal(unname(res$weights), c(0.25, 0.75))
  expect_match(paste(res$notes, collapse = " "), "rate_price with price", fixed = TRUE)
  expect_match(paste(res$notes, collapse = " "), "rate_quality with quality", fixed = TRUE)
})
