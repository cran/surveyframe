# tests/testthat/test-decision-sensitivity.R
# sensitivity_analysis() perturbs one criterion weight at a time, renormalises,
# reruns the same ranking method, and reports how far the ranking moved. The
# point is to let a researcher say whether a ranking is robust to its weights
# rather than assert it, so the checks here are about the arithmetic being
# right and the reported stability matching what actually happened.

sens_matrix <- function() {
  matrix(
    c(4.1, 3.0, 210,
      3.6, 4.5, 180,
      4.8, 2.5, 260,
      3.9, 4.0, 150),
    nrow = 4, byrow = TRUE,
    dimnames = list(c("Alpha", "Basilica", "Coral", "Dhoni"),
                    c("service", "location", "price"))
  )
}

sens_args <- function(...) {
  utils::modifyList(
    list(
      x              = sens_matrix(),
      weights        = c(0.4, 0.3, 0.3),
      criteria_types = c("benefit", "benefit", "cost"),
      method         = "topsis"
    ),
    list(...)
  )
}

test_that("the table has one row per criterion and direction", {
  sa <- do.call(sensitivity_analysis, sens_args())

  expect_s3_class(sa, "sframe_sensitivity")
  expect_equal(nrow(sa$table), 3 * 2)
  expect_setequal(sa$table$criterion, c("service", "location", "price"))
  expect_setequal(unique(sa$table$direction), c("up", "down"))
  expect_true(all(c("criterion", "direction", "weight", "rho",
                    "rank_changed", "top_changed") %in% names(sa$table)))
})

test_that("perturbed weights are renormalised to sum to 1", {
  # Pinned to the exact renormalised share, not merely "moved in the right
  # direction". An earlier version of this test asserted only up > 0.4 and
  # down < 0.4, which holds whether or not renormalisation happens
  # (0.4 * 1.1 = 0.44 > 0.4 either way), so deleting the renormalisation
  # left every test passing. A mutation check caught that.
  #
  # delta 0.10 on weights c(0.4, 0.3, 0.3):
  #   up   raw 0.44, vector sums to 1.04, share = 0.44 / 1.04 = 0.4231
  #   down raw 0.36, vector sums to 0.96, share = 0.36 / 0.96 = 0.3750
  # Without renormalisation these would read 0.44 and 0.36.
  sa <- do.call(sensitivity_analysis, sens_args(delta = 0.10))

  up   <- sa$table[sa$table$criterion == "service" &
                     sa$table$direction == "up", "weight"]
  down <- sa$table[sa$table$criterion == "service" &
                     sa$table$direction == "down", "weight"]

  expect_equal(up,   round(0.44 / 1.04, 4), tolerance = 1e-6)
  expect_equal(down, round(0.36 / 0.96, 4), tolerance = 1e-6)
  expect_false(isTRUE(all.equal(up, 0.44)))
  expect_false(isTRUE(all.equal(down, 0.36)))

  # every reported share must be a share of a vector that sums to 1
  expect_true(all(sa$table$weight > 0))
  expect_true(all(sa$table$weight < 1))
})

test_that("stable is TRUE only when no perturbation moved the ranking", {
  sa <- do.call(sensitivity_analysis, sens_args())
  expect_identical(sa$stable, !any(sa$table$rank_changed))
  expect_equal(sa$n_changed, sum(sa$table$rank_changed))
})

test_that("rho is 1 exactly where the ranking did not change", {
  sa <- do.call(sensitivity_analysis, sens_args())
  unchanged <- !sa$table$rank_changed
  skip_if(!any(unchanged), "no unchanged perturbation in this fixture")
  expect_true(all(sa$table$rho[unchanged] == 1))
})

test_that("a dominated alternative keeps its rank under any single nudge", {
  # Coral is best on service and worst on price, Dhoni is cheapest. A tiny
  # delta must not reorder a matrix with this much separation.
  sa <- do.call(sensitivity_analysis, sens_args(delta = 0.001))
  expect_true(sa$stable)
  expect_equal(sa$n_top_changed, 0)
})

test_that("a large perturbation can move the ranking and it is reported", {
  # Not asserting that it does move, which depends on the matrix. Asserting
  # that whatever happened is reported consistently across the 3 fields.
  sa <- do.call(sensitivity_analysis, sens_args(delta = 0.9))
  for (i in seq_len(nrow(sa$table))) {
    if (isTRUE(sa$table$top_changed[i])) {
      expect_true(sa$table$rank_changed[i],
                  info = "a changed leader implies a changed ranking")
    }
  }
})

test_that("all 7 ranking methods are supported", {
  for (m in c("topsis", "vikor", "moora", "smart", "waspas", "promethee",
              "electre")) {
    sa <- do.call(sensitivity_analysis, sens_args(method = m))
    expect_s3_class(sa, "sframe_sensitivity")
    expect_equal(nrow(sa$table), 6, info = m)
    expect_identical(sa$method, m)
  }
})

test_that("methods that do not consume a weight vector are refused", {
  # AHP, ANP, and DEMATEL produce weights or influence structure rather than
  # ranking alternatives from a weight vector, so there is nothing to perturb.
  for (m in c("ahp", "anp", "dematel")) {
    expect_error(do.call(sensitivity_analysis, sens_args(method = m)),
                 class = "sframe_validation_error")
  }
})

test_that("method-specific tuning reaches the perturbed run", {
  # If v were dropped, the sensitivity run would rank under different
  # settings from the result it claims to be testing.
  a <- do.call(sensitivity_analysis, sens_args(method = "vikor", v = 0.0))
  b <- do.call(sensitivity_analysis, sens_args(method = "vikor", v = 1.0))
  expect_false(identical(a$base_ranks, b$base_ranks))
})

test_that("bad input is rejected with a typed error", {
  expect_error(do.call(sensitivity_analysis, sens_args(weights = c(0.5, 0.5))),
               class = "sframe_validation_error")
  expect_error(
    do.call(sensitivity_analysis, sens_args(criteria_types = c("benefit"))),
    class = "sframe_validation_error"
  )
  expect_error(do.call(sensitivity_analysis, sens_args(delta = 0)),
               class = "sframe_validation_error")
  expect_error(do.call(sensitivity_analysis, sens_args(delta = 1)),
               class = "sframe_validation_error")
  expect_error(
    do.call(sensitivity_analysis, sens_args(weights = c(0, 0, 0))),
    class = "sframe_validation_error"
  )
})

test_that("a single criterion is refused rather than reported as stable", {
  # With one criterion the renormalised weight is always 1, so every
  # perturbation is a no-op and "stable" would be vacuously true.
  expect_error(
    sensitivity_analysis(matrix(c(1, 2, 3), ncol = 1), weights = 1,
                         criteria_types = "benefit"),
    class = "sframe_validation_error"
  )
})

test_that("labels come from the matrix dimnames when not supplied", {
  sa <- do.call(sensitivity_analysis, sens_args())
  expect_setequal(names(sa$base_ranks),
                  c("Alpha", "Basilica", "Coral", "Dhoni"))
  expect_setequal(sa$table$criterion, c("service", "location", "price"))
})

test_that("print reports the stability verdict", {
  sa <- do.call(sensitivity_analysis, sens_args(delta = 0.001))
  out <- paste(utils::capture.output(print(sa)), collapse = "\n")
  expect_match(out, "TOPSIS")
  expect_match(out, "Stable")
})

test_that("plot returns a ggplot when ggplot2 is available", {
  skip_if_not_installed("ggplot2")
  sa <- do.call(sensitivity_analysis, sens_args())
  expect_s3_class(plot(sa), "ggplot")
})

test_that("a plan block attaches sensitivity only when it asks for it", {
  vendors <- c("Alpha", "Basilica", "Coral", "Dhoni")
  crits   <- c("service", "location", "price")

  make_plan <- function(opts) {
    list(list(
      id = "RQ1", research_question = "Which supplier ranks best?",
      family = "decision", method = "topsis", roles = list(),
      options = opts
    ))
  }
  base_opts <- list(
    matrix = lapply(seq_len(4), function(i) sens_matrix()[i, ]),
    alternatives = vendors, criteria = crits,
    weights = c(0.4, 0.3, 0.3),
    criteria_types = c("benefit", "benefit", "cost")
  )

  inst_off <- sf_instrument(title = "Sensitivity off", version = "1.0.0",
                            components = list(sf_item("q1", "Anything",
                                                      type = "text")),
                            analysis_plan = make_plan(base_opts))
  inst_on  <- sf_instrument(title = "Sensitivity on", version = "1.0.0",
                            components = list(sf_item("q1", "Anything",
                                                      type = "text")),
                            analysis_plan = make_plan(
                              c(base_opts, list(sensitivity = TRUE))))

  dat <- data.frame(q1 = c("a", "b", "c"), stringsAsFactors = FALSE)

  off <- run_analysis_plan(dat, inst_off)
  on  <- run_analysis_plan(dat, inst_on)

  expect_null(off[[1]]$sensitivity)
  expect_s3_class(on[[1]]$sensitivity, "sframe_sensitivity")
  expect_equal(nrow(on[[1]]$sensitivity$table), 6)
})

test_that("a base ranking with no discrimination is flagged, not called stable", {
  # ELECTRE I on a 9 by 9 problem at the default thresholds establishes no
  # outranking at all: every score is 0 and every alternative ranks 1. A
  # ranking that never separated anything cannot be moved by perturbing a
  # weight, so every check passes and `stable` is TRUE. That is the strongest
  # robustness signal this function has, produced by the weakest possible
  # result, so `degenerate` marks it and print() leads with the warning.
  set.seed(2026)
  x <- matrix(round(runif(81, 1, 100), 1), nrow = 9)
  w <- rep(1 / 9, 9)
  ct <- rep(c("benefit", "cost"), length.out = 9)

  base <- sframe_electre_compute(x, w, ct)
  skip_if(length(unique(as.integer(base$ranks))) > 1,
          "this fixture is no longer degenerate")

  sa <- sensitivity_analysis(x, w, ct, method = "electre")

  expect_true(sa$degenerate)
  expect_true(sa$stable)          # factually nothing changed
  expect_equal(sa$n_changed, 0)

  out <- paste(utils::capture.output(print(sa)), collapse = "\n")
  expect_match(out, "No result to test")
  expect_no_match(out, "^Stable\\.")
})

test_that("a discriminating ranking is not flagged as degenerate", {
  sa <- do.call(sensitivity_analysis, sens_args())
  expect_false(sa$degenerate)
  expect_gt(length(unique(sa$base_ranks)), 1)
})
