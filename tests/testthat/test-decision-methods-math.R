# tests/testthat/test-decision-methods-math.R
#
# Decision method mathematics, batch 3 findings 6, 7, 8, 10, 11 and 15.
#
# 6.  ANP declared convergence when the uniform starting vector stopped moving,
#     which a periodic network like rows (0, 1), (1, 0) and a reducible one
#     like the identity both satisfy, though their matrix powers never settle.
# 7.  DEMATEL claimed its normalisation guaranteed an inverse. Rows (0, 1) and
#     (1, 0) normalise to themselves and I - N is singular.
# 8.  DEMATEL named the first criterion as the strongest cause when every net
#     relation was zero, and called below-threshold relations noise.
# 10. WASPAS accepted negative values, which reverse its ratio normalisation.
# 11. VIKOR's v, WASPAS's lambda, PROMETHEE thresholds and ELECTRE cutoffs were
#     used with no check of their bounds.
# 15. ELECTRE's kernel was the set with no incoming outranking edge, which
#     misses Roy's condition that every excluded alternative is outranked by
#     an included one.

labelled <- function(m, prefix = "N") {
  dimnames(m) <- list(paste0(prefix, seq_len(nrow(m))), paste0(prefix, seq_len(ncol(m))))
  m
}

test_that("6: a periodic network gets its stationary priorities and says how", {
  fit <- sframe_anp_compute(labelled(matrix(c(0, 1, 1, 0), 2, 2)))
  expect_null(fit$error)
  expect_equal(unname(fit$weights), c(0.5, 0.5))
  expect_identical(fit$limit_method, "cesaro")
})

test_that("6: a reducible network is refused, since its limit depends on the start", {
  fit <- sframe_anp_compute(labelled(diag(3)))
  expect_false(is.null(fit$error))
  expect_match(fit$error, "reducible")
})

test_that("6: a primitive network keeps its limit supermatrix result", {
  m <- labelled(matrix(c(0.2, 0.5, 0.3,  0.6, 0.1, 0.3,  0.3, 0.3, 0.4), 3, 3))
  fit <- sframe_anp_compute(m)
  expect_null(fit$error)
  expect_identical(fit$limit_method, "power")
  limit <- Reduce(`%*%`, rep(list(sweep(m, 2, colSums(m), "/")), 200))
  expect_equal(unname(fit$weights), unname(limit[, 1] / sum(limit[, 1])), tolerance = 1e-8)
})

test_that("7: DEMATEL explains an undefined total relation", {
  res <- sframe_run_dematel(data.frame(), list(),
                            list(matrix = matrix(c(0, 1, 1, 0), 2, 2),
                                 criteria = c("A", "B")), NULL)
  expect_false(is.null(res$error))
  expect_match(res$error, "spectral radius")
})

test_that("8: DEMATEL names no strongest cause when no relation is net causal", {
  # Symmetric, so every net relation is 0, with unequal row sums so the
  # total relation exists.
  sym <- matrix(c(0, 1, 2, 1, 0, 1, 2, 1, 0), 3, 3)
  res <- sframe_run_dematel(data.frame(), list(),
                            list(matrix = sym, criteria = c("A", "B", "C")), NULL)
  expect_null(res$error)
  expect_match(res$apa, "No criterion had a net causal role", fixed = TRUE)
  expect_false(grepl("strongest net causal role", res$apa, fixed = TRUE))
  expect_false(grepl("noise", res$prompt, fixed = TRUE))
})

ranking_opts <- function(matrix_rows, extra = list()) {
  c(list(matrix = matrix_rows, alternatives = paste0("A", seq_along(matrix_rows)),
         criteria = paste0("C", seq_along(matrix_rows[[1]])),
         weights = rep(1 / length(matrix_rows[[1]]), length(matrix_rows[[1]]))),
    extra)
}

test_that("10: WASPAS refuses values outside its positive domain", {
  res <- sframe_run_waspas(data.frame(), list(),
                           ranking_opts(list(-2, -1)), NULL)
  expect_false(is.null(res$error))
  expect_match(res$error, "positive")
})

test_that("10: every method refuses infinite performance values", {
  res <- sframe_run_topsis(data.frame(), list(),
                           ranking_opts(list(c(1, Inf), c(2, 3))), NULL)
  expect_false(is.null(res$error))
})

test_that("11: blend parameters outside 0 to 1 are refused", {
  rows <- list(c(3, 5), c(4, 2), c(5, 1))
  expect_match(sframe_run_vikor(data.frame(), list(),
                                ranking_opts(rows, list(v = 1.5)), NULL)$error, "v")
  expect_match(sframe_run_waspas(data.frame(), list(),
                                 ranking_opts(rows, list(lambda = -0.2)), NULL)$error, "lambda")
})

test_that("11: PROMETHEE thresholds must be ordered and non-negative", {
  rows <- list(c(3, 5), c(4, 2), c(5, 1))
  bad <- list(list(preference = 1, indifference = -2),
              list(preference = 1, indifference = 0.5))
  res <- sframe_run_promethee(data.frame(), list(),
                              ranking_opts(rows, list(preference_function = "level",
                                                      thresholds = bad)), NULL)
  expect_false(is.null(res$error))
  inverted <- list(list(preference = 0.5, indifference = 1),
                   list(preference = 1, indifference = 0.5))
  res2 <- sframe_run_promethee(data.frame(), list(),
                               ranking_opts(rows, list(preference_function = "level",
                                                       thresholds = inverted)), NULL)
  expect_false(is.null(res2$error))
})

test_that("11: ELECTRE cutoffs outside 0 to 1 are refused", {
  rows <- list(c(3, 5), c(4, 2), c(5, 1))
  res <- sframe_run_electre(data.frame(), list(),
                            ranking_opts(rows, list(concordance_threshold = 1.4)), NULL)
  expect_false(is.null(res$error))
})

test_that("15: the ELECTRE kernel is Roy's kernel on an acyclic graph", {
  opts <- list(matrix = list(c(3, 2, 0), c(2, 1, 1), c(1, 0, 2)),
               alternatives = c("A", "B", "C"), criteria = c("x", "y", "z"),
               weights = c(0.4, 0.4, 0.2),
               concordance_threshold = 0.7, discordance_threshold = 0.5)
  res <- sframe_run_electre(data.frame(), list(), opts, NULL)
  expect_null(res$error)
  expect_true(res$outranking["A", "B"] && res$outranking["B", "C"])
  expect_false(res$outranking["A", "C"])
  expect_setequal(names(res$kernel)[res$kernel], c("A", "C"))
})

test_that("15: a cyclic outranking graph reports no kernel", {
  cyc <- matrix(FALSE, 3, 3, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  cyc["A", "B"] <- TRUE; cyc["B", "C"] <- TRUE; cyc["C", "A"] <- TRUE
  k <- sframe_electre_kernel(cyc)
  expect_false(k$defined)
})
