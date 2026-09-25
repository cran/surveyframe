# tests/testthat/test-report-scoring-parity.R
#
# B3. The report path computed its own scale scores: an unconditional row
# mean with na.rm = TRUE, labelled "scale score". It ignored the declared
# method, reverse coding, weights and min_valid, so a declared sum of 2 and 4
# was published as 3, and a respondent below the completeness threshold still
# entered the figure. R/reporting.R and inst/templates/report.qmd both did
# this. Both now plot score_scales() output.

skip_if_not_installed("ggplot2")

captured_scores <- function(instrument, data) {
  seen <- list()
  testthat::local_mocked_bindings(
    sframe_plot_scale_chart = function(scores, label, palette = "web") {
      seen[[label]] <<- scores
      ggplot2::ggplot()
    },
    .render_report_ggplot_png = function(gg, alt = "") ""
  )
  .render_report_distributions(instrument, data)
  seen
}

agree <- function() sf_choices("ag5", 1:5, c("SD", "D", "N", "A", "SA"))

test_that("B3: a declared sum is plotted as the sum", {
  ins <- sf_instrument(title = "Sum", components = list(
    agree(),
    sf_item("s1", "One", "numeric", scale_id = "tot"),
    sf_item("s2", "Two", "numeric", scale_id = "tot"),
    sf_scale("tot", "Total", items = c("s1", "s2"), method = "sum")
  ))
  d <- data.frame(s1 = c(2, 1), s2 = c(4, 1))
  got <- captured_scores(ins, d)[["Total"]]
  expect_equal(unname(got), c(6, 2))
})

test_that("B3: plotted scores are score_scales() scores, reversal included", {
  ins <- sf_instrument(title = "Reversed", components = list(
    agree(),
    sf_item("r1", "One", "likert", choice_set = "ag5", scale_id = "sat"),
    sf_item("r2", "Two", "likert", choice_set = "ag5", scale_id = "sat",
            reverse = TRUE),
    sf_scale("sat", "Satisfaction", items = c("r1", "r2"))
  ))
  d <- data.frame(r1 = c(5, 2, 4), r2 = c(1, 4, 2))
  got <- captured_scores(ins, d)[["Satisfaction"]]
  expect_equal(unname(got), score_scales(d, ins)$sat)
})

test_that("B3: a respondent below min_valid is left out of the figure", {
  ins <- sf_instrument(title = "Threshold", components = list(
    agree(),
    sf_item("t1", "One", "likert", choice_set = "ag5", scale_id = "t"),
    sf_item("t2", "Two", "likert", choice_set = "ag5", scale_id = "t"),
    sf_item("t3", "Three", "likert", choice_set = "ag5", scale_id = "t"),
    sf_scale("t", "Three items", items = c("t1", "t2", "t3"), min_valid = 3L)
  ))
  d <- data.frame(t1 = c(3, 5), t2 = c(3, NA), t3 = c(3, NA))
  got <- captured_scores(ins, d)[["Three items"]]
  expect_equal(unname(got), 3)
})

test_that("B3: the Quarto report template scores through score_scales()", {
  path <- system.file("templates", "report.qmd", package = "surveyframe")
  skip_if(!nzchar(path), "report template not found")
  qmd <- paste(readLines(path, warn = FALSE), collapse = "\n")
  block <- regmatches(qmd, regexpr(
    "# One histogram per scale score[\\s\\S]*?\n```", qmd, perl = TRUE))
  expect_length(block, 1)
  expect_match(block, "score_scales(", fixed = TRUE)
  expect_false(grepl("rowMeans", block, fixed = TRUE))
})
