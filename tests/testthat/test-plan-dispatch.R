# tests/testthat/test-plan-dispatch.R
#
# How run_analysis_plan() dispatches blocks, batch 2 findings 6, 10, 11, 12.
#
# 10. A plan where every block failed returned an ordinary results object, so
#     a caller treating a normal return as success reported success. Duplicate
#     block ids were accepted, and a scoring failure fell back to unscored data
#     with a warning the results did not record.
# 11. The t test ignored options$var_equal, and one-way ANOVA and the t test
#     judged significance at .05 whatever alpha the block declared. Options a
#     method never reads were kept in the result as if they applied.
# 12. Reliability, item and EFA blocks analysed every scale whatever the block
#     selected, and the quality block ran with no respondent id, so its
#     duplicate check never ran and reported "0 flagged".
# 6.  Statistics runners converted factors with as.numeric(), which returns
#     level positions, and regression turned a nominal predictor into a
#     numeric trend.

block <- function(id, method, roles = list(), options = list(), alpha = NULL) {
  list(id = id, research_question = paste("Question", id), method = method,
       roles = roles, options = options, alpha = alpha)
}

with_plan <- function(instrument, ...) {
  instrument$analysis_plan <- list(...)
  instrument
}

two_group <- function() {
  sf_instrument(title = "Two group", components = list(
    sf_item("grp", "Group", "text"),
    sf_item("y", "Outcome", "numeric")
  ))
}

two_group_data <- function() {
  set.seed(4)
  data.frame(grp = rep(c("x", "y"), each = 20),
             y = c(stats::rnorm(20, 5), stats::rnorm(20, 5.7)))
}

# ── 10 ──

test_that("10: results report how many blocks failed", {
  ins <- with_plan(two_group(),
    block("ok", "t_test_ind", list(group = "grp", outcome = "y")),
    block("typo", "t_tset_ind", list(group = "grp", outcome = "y")))
  res <- run_analysis_plan(two_group_data(), ins)
  status <- attr(res, "status")

  expect_identical(status$blocks, 2L)
  expect_identical(status$failed, 1L)
  expect_identical(status$failed_blocks, "typo")
  expect_match(paste(utils::capture.output(print(res)), collapse = "\n"),
               "1 of 2 blocks failed")
})

test_that("10: strict = TRUE turns a failed block into an error", {
  ins <- with_plan(two_group(),
    block("typo", "t_tset_ind", list(group = "grp", outcome = "y")))
  expect_error(run_analysis_plan(two_group_data(), ins, strict = TRUE),
               "typo", class = "sframe_error")
})

test_that("10: duplicate block ids are refused", {
  ins <- with_plan(two_group(),
    block("same", "t_test_ind", list(group = "grp", outcome = "y")),
    block("same", "t_test_ind", list(group = "grp", outcome = "y")))
  expect_error(run_analysis_plan(two_group_data(), ins), "same",
               class = "sframe_error")
})

test_that("10: a scoring failure is recorded in the results", {
  ins <- sf_instrument(title = "Clash", components = list(
    sf_choices("ag5", 1:5, as.character(1:5)),
    sf_item("q1", "One", "likert", choice_set = "ag5", scale_id = "q1"),
    sf_item("q2", "Two", "likert", choice_set = "ag5", scale_id = "q1"),
    sf_scale("q1", "Clashing", items = c("q1", "q2"))))
  ins <- with_plan(ins, block("d", "descriptives", list(variables = "q2")))
  res <- suppressWarnings(run_analysis_plan(data.frame(q1 = 1:3, q2 = 3:1), ins))
  expect_match(attr(res, "status")$scoring, "failed")
})

# ── 11 ──

test_that("11: the t test honours var_equal", {
  d <- two_group_data()
  ins <- with_plan(two_group(),
    block("t", "t_test_ind", list(group = "grp", outcome = "y"),
          options = list(var_equal = TRUE)))
  res <- run_analysis_plan(d, ins)[["t"]]
  ref <- stats::t.test(d$y[d$grp == "x"], d$y[d$grp == "y"], var.equal = TRUE)
  expect_equal(res$df, unname(ref$parameter))
  expect_equal(res$p, ref$p.value)
})

test_that("11: a declared alpha of .01 judges p = .025 non-significant", {
  ins <- with_plan(two_group(),
    block("t", "t_test_ind", list(group = "grp", outcome = "y"), alpha = 0.01),
    block("a", "anova_one", list(group = "grp", outcome = "y"), alpha = 0.01))
  res <- run_analysis_plan(two_group_data(), ins)

  expect_gt(res[["t"]]$p, 0.01)
  expect_lt(res[["t"]]$p, 0.05)
  expect_match(res[["t"]]$prompt, "did not reveal", fixed = TRUE)
  expect_match(res[["a"]]$prompt, "did not", fixed = TRUE)
})

test_that("11: options a method never reads are reported as ignored", {
  ins <- with_plan(two_group(),
    block("t", "t_test_ind", list(group = "grp", outcome = "y"),
          options = list(var_equal = TRUE, tails = 1)))
  res <- run_analysis_plan(two_group_data(), ins)[["t"]]
  expect_identical(res$options_ignored, "tails")
})

# ── 12 ──

two_scales <- function() {
  sf_instrument(title = "Scoped", components = list(
    sf_choices("ag5", 1:5, as.character(1:5)),
    sf_item("a1", "A1", "likert", choice_set = "ag5", scale_id = "a"),
    sf_item("a2", "A2", "likert", choice_set = "ag5", scale_id = "a"),
    sf_item("b1", "B1", "likert", choice_set = "ag5", scale_id = "b"),
    sf_item("b2", "B2", "likert", choice_set = "ag5", scale_id = "b"),
    sf_scale("a", "A", items = c("a1", "a2")),
    sf_scale("b", "B", items = c("b1", "b2"))))
}

scale_data <- function() {
  set.seed(1)
  base <- sample(1:5, 30, replace = TRUE)
  data.frame(respondent_id = c("r1", "r1", paste0("r", 3:30)),
             a1 = base, a2 = pmin(5, base + sample(0:1, 30, TRUE)),
             b1 = sample(1:5, 30, TRUE), b2 = sample(1:5, 30, TRUE))
}

test_that("12: an item diagnostics block analyses only the scale it selects", {
  ins <- with_plan(two_scales(), block("i", "item_diagnostics", list(scales = "b")))
  res <- run_analysis_plan(scale_data(), ins, scored = FALSE)[["i"]]
  expect_null(res$error)
  expect_setequal(unique(res$table$Scale), "B")
})

test_that("12: the quality block checks duplicates when respondent ids exist", {
  ins <- with_plan(two_scales(), block("q", "quality"))
  res <- run_analysis_plan(scale_data(), ins, scored = FALSE)[["q"]]
  dup <- res$table[res$table$Check == "duplicates", ]
  expect_match(dup$Result, "1 flagged")
})

test_that("12: an unperformed duplicate check says so", {
  ins <- with_plan(two_scales(), block("q", "quality"))
  d <- scale_data()
  d$respondent_id <- NULL
  res <- run_analysis_plan(d, ins, scored = FALSE)[["q"]]
  dup <- res$table[res$table$Check == "duplicates", ]
  expect_match(dup$Result, "not checked")
})

# ── 6 ──

test_that("6: a factor outcome is analysed on its labels", {
  d <- two_group_data()
  labelled <- transform(d, y = factor(round(y * 10)))
  ins <- with_plan(two_group(), block("t", "t_test_ind", list(group = "grp", outcome = "y")))
  res <- run_analysis_plan(labelled, ins)[["t"]]
  expect_equal(res$mean1, mean(round(d$y[d$grp == "x"] * 10)))
})

test_that("6: a text predictor in linear regression stays categorical", {
  set.seed(2)
  d <- data.frame(region = rep(c("north", "south", "west"), 20),
                  y = stats::rnorm(60))
  ins <- sf_instrument(title = "Reg", components = list(
    sf_item("region", "Region", "text"), sf_item("y", "Y", "numeric")))
  ins <- with_plan(ins, block("r", "regression_linear",
                              list(predictors = "region", outcome = "y")))
  res <- run_analysis_plan(d, ins)[["r"]]
  expect_null(res$error)
  expect_true(any(grepl("regionsouth", rownames(res$coefficients), fixed = TRUE)))
})
