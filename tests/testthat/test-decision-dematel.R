# tests/testthat/test-decision-dematel.R
# DEMATEL's total-relation computation, its own directed-matrix resolution
# (a `pairwise` role with `comparison_scale = "influence"`, not the
# performance-matrix/weights shape TOPSIS uses), and the influence-map plot.

crits <- c("service", "location", "price", "delivery")

# ---------------------------------------------------------------------------
# Computation: hand-verified 2x2 (worked by hand, reproduced with base R
# below in a comment for anyone re-deriving it)
# ---------------------------------------------------------------------------

test_that("the total-relation matrix matches a hand-derived 2x2 case", {
  # X = [[0,2],[1,0]]. max_sum = max(rowSums)=max(2,1)=2, max(colSums)=
  # max(1,2)=2, so max_sum = 2. N = [[0,1],[0.5,0]].
  # I - N = [[1,-1],[-0.5,1]], det = 1 - 0.5 = 0.5,
  # inv(I-N) = (1/0.5) * [[1,1],[0.5,1]] = [[2,2],[1,2]].
  # T = N %*% inv(I-N) = [[0,1],[0.5,0]] %*% [[2,2],[1,2]]
  #   = [[0*2+1*1, 0*2+1*2], [0.5*2+0*1, 0.5*2+0*2]] = [[1,2],[1,1]].
  x <- matrix(c(0, 2, 1, 0), nrow = 2, byrow = TRUE,
              dimnames = list(c("A", "B"), c("A", "B")))
  fit <- sframe_dematel_compute(x)
  expect_equal(unname(fit$total_relation), matrix(c(1, 2, 1, 1), nrow = 2,
                                                   byrow = TRUE))
  expect_equal(unname(fit$D), c(3, 2))    # row sums of T: A=1+2=3, B=1+1=2
  expect_equal(unname(fit$R), c(2, 3))    # col sums of T: A=1+1=2, B=2+1=3
  expect_equal(unname(fit$prominence), c(5, 5))
  expect_equal(unname(fit$relation), c(1, -1))
  expect_equal(fit$threshold, mean(c(1, 2, 1, 1)))
  expect_equal(unname(fit$role), c("cause", "effect"))
})

test_that("the total-relation matrix matches a hand-derived 4x4 case", {
  # A textbook-style influence matrix (Price/Storage/Camera/Processor), the
  # same shape as RMCDA's apply.DEMATEL() example but re-derived here with
  # the standard normalisation this port uses: max_sum is the greater of the
  # largest row sum and the largest column sum (RMCDA normalises by the
  # largest row sum alone, so its numeric output differs and is not a valid
  # oracle for this exact case; the algebra itself, N(I-N)^-1, is identical
  # and independently checked by the 2x2 case above).
  x <- matrix(c(0, 1, 1, 1,
                3, 0, 2, 2,
                3, 2, 0, 1,
                4, 1, 2, 0), nrow = 4, byrow = TRUE,
              dimnames = list(c("Price", "Storage", "Camera", "Processor"),
                              c("Price", "Storage", "Camera", "Processor")))
  fit <- sframe_dematel_compute(x)

  expect_equal(max(rowSums(x)), 7)
  expect_equal(max(colSums(x)), 10)
  # max_sum = 10, so N = x / 10.
  expect_equal(fit$normalised, x / 10)

  expect_equal(unname(round(fit$D, 4)), c(0.7039, 1.4325, 1.2349, 1.3718))
  expect_equal(unname(round(fit$R, 4)), c(1.9311, 0.8899, 1.0462, 0.8757))
  expect_equal(unname(round(fit$prominence, 4)),
               c(2.6351, 2.3224, 2.2811, 2.2475))
  expect_equal(unname(round(fit$relation, 4)),
               c(-1.2272, 0.5426, 0.1886, 0.4961))
  expect_equal(round(fit$threshold, 6), 0.296441)
  expect_equal(unname(fit$role), c("effect", "cause", "cause", "cause"))
})

# ---------------------------------------------------------------------------
# Shared input validation
# ---------------------------------------------------------------------------

test_that("the validator reports each fault by name", {
  expect_match(sframe_check_dematel_input(as.data.frame(matrix(1:4, 2, 2)))$error,
               "numeric matrix")
  expect_match(sframe_check_dematel_input(matrix(1:6, 2, 3))$error, "square")
  expect_match(sframe_check_dematel_input(matrix(0, 1, 1))$error,
               "at least 2 criteria")
  x_na <- matrix(c(0, NA, 1, 0), 2, 2)
  expect_match(sframe_check_dematel_input(x_na)$error, "missing values")
  x_neg <- matrix(c(0, -1, 1, 0), 2, 2)
  expect_match(sframe_check_dematel_input(x_neg)$error, "non-negative")
})

test_that("a non-zero diagonal is zeroed with a note", {
  x <- matrix(c(2, 1, 1, 3), 2, 2)
  checked <- sframe_check_dematel_input(x)
  expect_null(checked$error)
  expect_equal(diag(checked$matrix), c(0, 0))
  expect_match(checked$note, "diagonal")
})

# ---------------------------------------------------------------------------
# Fixtures for the runner
# ---------------------------------------------------------------------------

dematel_instrument <- function(roles = list(pairwise = "crit_influence"),
                               options = NULL) {
  sf_instrument(
    title = "Hotel supplier selection",
    version = "1.0.0",
    components = list(
      sf_item("crit_influence",
              "How strongly does each factor influence the others?",
              type = "pairwise_comparison", comparison_items = crits,
              comparison_scale = "influence")
    ),
    analysis_plan = list(
      list(id = "RQ4", research_question = "Which criteria drive the others?",
           family = "decision", method = "dematel",
           roles = roles, options = options %||% list())
    )
  )
}

# One respondent's directed judgements over 12 ordered pairs (n=4 items,
# influence scale, 0-diagonal implied). Values chosen arbitrarily within the
# declared 0-4 range.
dematel_responses <- function(n = 2) {
  data.frame(
    crit_influence__service__to__location  = rep(2, n),
    crit_influence__service__to__price     = rep(3, n),
    crit_influence__service__to__delivery  = rep(1, n),
    crit_influence__location__to__service  = rep(0, n),
    crit_influence__location__to__price    = rep(1, n),
    crit_influence__location__to__delivery = rep(0, n),
    crit_influence__price__to__service     = rep(1, n),
    crit_influence__price__to__location    = rep(1, n),
    crit_influence__price__to__delivery    = rep(2, n),
    crit_influence__delivery__to__service  = rep(0, n),
    crit_influence__delivery__to__location = rep(1, n),
    crit_influence__delivery__to__price    = rep(1, n)
  )
}

# ---------------------------------------------------------------------------
# The runner and its resolution order
# ---------------------------------------------------------------------------

test_that("a collected influence item runs end to end", {
  study <- dematel_instrument()
  res <- sframe_run_dematel(dematel_responses(), list(pairwise = "crit_influence"),
                            list(), study)

  expect_null(res$error)
  expect_equal(res$test, "dematel")
  expect_equal(res$matrix_source, "collected")
  expect_equal(sort(res$criteria), sort(crits))

  expect_true(is.data.frame(res$table))
  expect_equal(names(res$table), c("Criterion", "D", "R", "Prominence",
                                   "Relation", "Role"))
  expect_equal(nrow(res$table), 4)
  expect_true(all(res$table$Role %in% c("cause", "effect")))
  # Sorted by prominence, descending.
  expect_equal(res$table$Prominence, sort(res$table$Prominence,
                                          decreasing = TRUE))
  expect_match(res$apa, "DEMATEL classified 4 criteria")
  expect_true(nzchar(res$prompt))
  expect_true(any(grepl("arithmetic mean of 2 respondent", res$notes)))
})

test_that("a researcher-supplied matrix takes precedence over a collected item", {
  study <- dematel_instrument()
  m <- list(c(0, 1, 1, 1), c(3, 0, 2, 2), c(3, 2, 0, 1), c(4, 1, 2, 0))
  res <- sframe_run_dematel(
    dematel_responses(), list(pairwise = "crit_influence"),
    list(matrix = m, criteria = crits), study
  )
  expect_null(res$error)
  expect_equal(res$matrix_source, "supplied")
  expect_equal(res$criteria, crits)
})

test_that("a missing input is a typed error naming what to supply", {
  study <- dematel_instrument()
  res <- sframe_run_dematel(dematel_responses(), list(), list(), study)
  expect_match(res$error, "needs a directed influence matrix")
  expect_match(res$error, "pairwise")
})

test_that("a saaty-scale item is rejected with a scale-specific message", {
  study <- sf_instrument(
    title = "Wrong scale",
    components = list(
      sf_item("crit_pairs", "Compare the criteria",
              type = "pairwise_comparison", comparison_items = crits,
              comparison_scale = "saaty")
    ),
    analysis_plan = list(
      list(id = "RQ4", research_question = "Which drive the others?",
           family = "decision", method = "dematel",
           roles = list(pairwise = "crit_pairs"))
    )
  )
  res <- sframe_run_dematel(data.frame(), list(pairwise = "crit_pairs"),
                            list(), study)
  expect_match(res$error, "influence")
})

test_that("mismatched criteria labels against a supplied matrix are reported", {
  study <- dematel_instrument()
  m <- list(c(0, 1), c(1, 0))
  res <- sframe_run_dematel(data.frame(), list(), list(matrix = m,
                                                       criteria = crits),
                            study)
  expect_match(res$error, "square")
})

# ---------------------------------------------------------------------------
# The influence-map plot
# ---------------------------------------------------------------------------

test_that("the influence plot draws a point per criterion with cause/effect colour", {
  skip_if_not_installed("ggplot2")
  study <- dematel_instrument()
  res <- sframe_run_dematel(dematel_responses(), list(pairwise = "crit_influence"),
                            list(), study)
  p <- sframe_plot_dematel_influence(res)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "Prominence (D + R)")
  expect_equal(p$labels$y, "Relation (D - R)")
  expect_equal(p$labels$title, "DEMATEL influence map")
  expect_equal(nrow(p$data), 4)
})

test_that("the influence plot returns NULL when the result carries no DEMATEL fields", {
  skip_if_not_installed("ggplot2")
  expect_null(sframe_plot_dematel_influence(list(test = "dematel")))
  expect_null(sframe_plot_dematel_influence(
    list(test = "dematel", error = "no matrix")
  ))
})
