# Batch 4 #28 and #29: both runners retain information their own prompt asks a
# reader to report, and neither the table nor the report carried it.
#
# #28. The mediation table gives Direct, Indirect and Total with no variable
#      names and no a or b paths, so a reader cannot tell which model produced
#      them, nor whether a and b are both positive or both negative. Those two
#      cases give the same positive product.
# #29. The moderation runner computes slopes at the moderator's mean and one SD
#      either side, retains them in conditional_effects, and asks the researcher
#      to report them. The table showed only the coefficient matrix, so the
#      numeric moderator values appeared nowhere.

med_data <- function(n = 120) {
  set.seed(11)
  x <- stats::rnorm(n)
  m <- 0.6 * x + stats::rnorm(n, sd = 0.7)
  y <- 0.5 * m + 0.2 * x + stats::rnorm(n, sd = 0.7)
  data.frame(x = x, m = m, y = y)
}

med_result <- function() {
  surveyframe:::sframe_run_mediation(
    med_data(), list(predictor = "x", mediator = "m", outcome = "y"),
    list(nboot = 200L))
}

mod_result <- function() {
  set.seed(12)
  n <- 150
  d <- data.frame(x = stats::rnorm(n), w = stats::rnorm(n))
  d$y <- 0.4 * d$x + 0.3 * d$w + 0.35 * d$x * d$w + stats::rnorm(n, sd = 0.6)
  surveyframe:::sframe_run_moderation(
    d, list(predictor = "x", moderator = "w", outcome = "y"))
}

test_that("28: the mediation table names its paths and its variables", {
  res <- med_result()
  tbl <- surveyframe:::sframe_result_table(res)
  expect_true(is.data.frame(tbl))

  # a and b are there, so a reader can see their signs
  expect_true(any(grepl("^a ", tbl$Effect)))
  expect_true(any(grepl("^b ", tbl$Effect)))
  # and the variables they run between are named
  expect_true(any(grepl("x", tbl$Effect, fixed = TRUE)))
  expect_true(any(grepl("m", tbl$Effect, fixed = TRUE)))
  expect_true(any(grepl("y", tbl$Effect, fixed = TRUE)))

  # the three effects it always had are still there
  expect_true(any(grepl("Direct", tbl$Effect, fixed = TRUE)))
  expect_true(any(grepl("Indirect", tbl$Effect, fixed = TRUE)))
  expect_true(any(grepl("Total", tbl$Effect, fixed = TRUE)))
  # an interval on the indirect effect alone, where one is available
  expect_true(any(nzchar(tbl[["95% CI"]])))
})

test_that("28: two models with the same product read differently", {
  res <- med_result()
  tbl <- surveyframe:::sframe_result_table(res)
  a_row <- as.numeric(tbl$Estimate[grepl("^a ", tbl$Effect)])
  b_row <- as.numeric(tbl$Estimate[grepl("^b ", tbl$Effect)])
  # both paths are reported with their own sign, so a positive product built
  # from 2 negatives cannot be mistaken for one built from 2 positives. The
  # table rounds for display, so this is the rounded value.
  expect_lt(abs(a_row - res$a_path), 0.01)
  expect_lt(abs(b_row - res$b_path), 0.01)
  expect_equal(sign(a_row), sign(res$a_path))
  expect_equal(sign(b_row), sign(res$b_path))
})

test_that("29: the moderation result carries its conditional slopes as a table", {
  res <- mod_result()
  expect_true(is.data.frame(res$conditional_effects))
  # the supplementary table the report renders, with the moderator's own values
  sup <- surveyframe:::sframe_result_supplement(res)
  expect_true(is.data.frame(sup$table))
  expect_match(sup$caption, "onditional", fixed = FALSE)
  expect_true(any(grepl("w", names(sup$table))) ||
              any(grepl("w", sup$table[[1]], fixed = TRUE)))
  expect_equal(nrow(sup$table), nrow(res$conditional_effects))
})

test_that("29: the report renders the conditional slopes", {
  src <- sframe_source_text("R", "reporting.R")
  qmd <- sframe_installed_text("inst", "templates",
                                   "report.qmd")
  expect_match(src, "sframe_result_supplement", fixed = TRUE)
  expect_match(qmd, "sframe_result_supplement", fixed = TRUE)
})
