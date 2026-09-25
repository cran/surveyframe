# tests/testthat/test-cochran-q.R
#
# C1. Cochran's Q recoded each value by testing membership of a list of
# positive codes, so NA became 0 before complete-case filtering and a missing
# response was counted as an observed failure. Any unrecognised code, such as
# 2 or "maybe", also became 0. The review's case: rows (1,0,0), (0,1,0) and
# (1,NA,0) give Q = 1 on N = 2 complete rows, and the package reported Q = 2 on
# N = 3. There was no Cochran test at all.

# Textbook Cochran's Q on a complete 0/1 matrix, written independently.
reference_q <- function(m) {
  k <- ncol(m); cs <- colSums(m); rs <- rowSums(m)
  (k - 1) * (k * sum(cs^2) - sum(cs)^2) / (k * sum(rs) - sum(rs^2))
}

test_that("C1: a missing response removes the row, never counts as 0", {
  d <- data.frame(a = c(1, 0, 1), b = c(0, 1, NA), c = c(0, 0, 0))
  res <- sframe_run_cochran_q(d, list(measures = c("a", "b", "c")))

  expect_null(res$error)
  expect_identical(res$n, 2L)
  expect_equal(res$Q, 1)
  expect_equal(res$p, stats::pchisq(1, df = 2, lower.tail = FALSE))
})

test_that("C1: Q matches the reference on a larger fixture with missing cells", {
  set.seed(11)
  m <- matrix(rbinom(120, 1, c(.3, .5, .7)), ncol = 3, byrow = TRUE)
  m[c(4, 17, 30)] <- NA
  d <- as.data.frame(m)
  complete <- m[stats::complete.cases(m), ]
  res <- sframe_run_cochran_q(d, list(measures = names(d)))

  expect_identical(res$n, nrow(complete))
  expect_equal(res$Q, reference_q(complete))
})

test_that("C1: yes/no and TRUE/FALSE codes are read as 1 and 0", {
  d <- data.frame(a = c("yes", "no", "Yes", "no"), b = c(TRUE, TRUE, FALSE, FALSE),
                  c = c("1", "0", "0", "0"), stringsAsFactors = FALSE)
  res <- sframe_run_cochran_q(d, list(measures = c("a", "b", "c")))
  m <- cbind(c(1, 0, 1, 0), c(1, 1, 0, 0), c(1, 0, 0, 0))
  expect_equal(res$Q, reference_q(m))
})

test_that("C1: a value that is not a binary code is reported, not read as 0", {
  d <- data.frame(a = c(1, 0, 2), b = c(0, 1, 1), c = c(0, 0, 1))
  res <- sframe_run_cochran_q(d, list(measures = c("a", "b", "c")))
  expect_false(is.null(res$error))
  expect_match(res$error, "2")
})

test_that("C1: rows with no variation give an explained result, not NaN", {
  d <- data.frame(a = c(1, 0, 1), b = c(1, 0, 1), c = c(1, 0, 1))
  res <- sframe_run_cochran_q(d, list(measures = c("a", "b", "c")))
  expect_false(is.null(res$error))
  expect_false(isTRUE(is.nan(res$Q)))
})
