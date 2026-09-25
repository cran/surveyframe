# tests/testthat/test-decision-sensitivity-conjoint.R
#
# Batch 3, findings 12, 13 and 14.
#
# 12. Sensitivity forwarded a `thresholds` field ELECTRE does not accept, and
#     dropped the concordance and discordance cutoffs it does, so the baseline
#     was re-ranked at the defaults. A failed sensitivity run left no trace.
# 13. With weights (1, 0), every perturbation renormalised back to (1, 0), so
#     nothing was tested and the result still said stable.
# 14. Conjoint balance counted only the levels present, so a design never
#     showing one attribute's second level scored as perfectly balanced.

electre_opts <- function() {
  list(matrix = list(c(3, 2, 0), c(2, 1, 1), c(1, 0, 2)),
       alternatives = c("A", "B", "C"), criteria = c("x", "y", "z"),
       weights = c(0.4, 0.4, 0.2),
       concordance_threshold = 0.7, discordance_threshold = 0.5,
       sensitivity = TRUE)
}

electre_instrument <- function() {
  sf_instrument(title = "E", components = list(sf_item("q", "Q", "text")))
}

run_block <- function(options, method = "electre") {
  sframe_run_one_block(list(id = "b", research_question = "Q", method = method,
                            options = options),
                       data.frame(), electre_instrument())
}

test_that("12: ELECTRE sensitivity uses the cutoffs the published result used", {
  res <- run_block(electre_opts())
  expect_null(res$error)
  expect_false(is.null(res$sensitivity))
  expect_identical(unname(res$sensitivity$base_ranks), as.integer(res$ranks))
})

test_that("12: a requested sensitivity run that fails says so", {
  opts <- electre_opts()
  opts$sensitivity_delta <- 2          # outside (0, 1)
  res <- run_block(opts)
  expect_null(res[["sensitivity"]])   # [[ ]], since $ would match sensitivity_error
  expect_match(res$sensitivity_error, "delta")
})

test_that("13: perturbations that change no weight are not called stable", {
  x <- matrix(c(4, 1, 2, 3, 5, 2), nrow = 3)
  sa <- sensitivity_analysis(x, c(1, 0), c("benefit", "benefit"), method = "topsis")
  expect_identical(sa$n_effective, 0L)
  expect_false(sa$stable)
  expect_identical(sa$n_perturbations, 4L)
})

test_that("13: an ordinary design reports effective and failed perturbation counts", {
  x <- matrix(c(4, 1, 2, 3, 5, 2), nrow = 3)
  sa <- sensitivity_analysis(x, c(0.6, 0.4), c("benefit", "benefit"), method = "topsis")
  expect_identical(sa$n_effective, 4L)
  expect_identical(sa$n_failed, 0L)
})

test_that("14: a design that never shows a declared level is not balanced", {
  attrs <- list(A = c("a1", "a2"), B = c("b1", "b2"))
  profiles <- data.frame(A = c("a1", "a1"), B = c("b1", "b2"),
                         stringsAsFactors = FALSE)
  expect_gt(sframe_conjoint_imbalance(profiles, attrs), 0)

  d <- sf_conjoint_design("d", attrs, profiles = profiles, n_alternatives = 2L,
                          n_tasks = 1L, seed = 1)
  expect_identical(d$balance$level_counts$A$a2, 0L)
  expect_identical(d$balance$absent_levels, list(A = "a2"))
})
