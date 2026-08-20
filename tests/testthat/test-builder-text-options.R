# tests/testthat/test-builder-text-options.R
#
# The builder/studio UI gap this closes: term_context's required keyword
# (options$term) had no UI field in either survey_builder.html or app.R, so
# a user could create a term_context research question that always failed
# at run time. Both UIs now write options$term the same way they already
# write options$alpha for other methods (see sframe_restore_analysis_block()
# in R/read_write_sframe.R, the shared R-side surface both UIs round-trip
# an analysis-plan block's `options` list through). This file is the R-side
# regression test for that round trip; the JS UI logic itself was verified
# headlessly with chromote (see the task's final report), not here, since
# there is no R-side surface for DOM/JS behaviour.

fresh_text_instrument <- function(options) {
  sf_instrument(
    title = "Text options round trip", version = "1.0.0",
    components = list(
      sf_item("fb", "Feedback", type = "textarea")
    ),
    analysis_plan = list(list(
      id = "RQ1", research_question = "What keyword shows up in feedback?",
      family = "text", method = "term_context", test = "term_context",
      roles = list(item = "fb"),
      variables = "fb",
      options = options
    ))
  )
}

test_that("term_context's options$term round-trips through write_sframe/read_sframe", {
  inst <- fresh_text_instrument(list(term = "delivery"))
  p <- tempfile(fileext = ".sframe")
  write_sframe(inst, p)

  back <- read_sframe(p)
  block <- back$analysis_plan[[1]]
  expect_identical(block$method, "term_context")
  expect_identical(block$options$term, "delivery")

  # A second write/read cycle must not lose or mutate it (the same fixed-
  # point property test-serialisation-fixed-point.R checks for the plan
  # block as a whole, exercised here specifically for a text-family option).
  p2 <- tempfile(fileext = ".sframe")
  write_sframe(back, p2)
  back2 <- read_sframe(p2)
  expect_identical(back2$analysis_plan[[1]]$options$term, "delivery")
})

test_that("sframe_run_term_context resolves the keyword from options$term end to end", {
  inst <- fresh_text_instrument(list(term = "great"))
  p <- tempfile(fileext = ".sframe")
  write_sframe(inst, p)
  back <- read_sframe(p)

  data <- data.frame(
    fb = c(
      "the service was great and fast", "great value for money",
      "nothing great here at all", "would not recommend this",
      "great experience overall, will return", "okay but not great",
      "the great outdoors theme was nice", "simply great",
      "not a great fit for us", "great, thanks for asking"
    ),
    stringsAsFactors = FALSE
  )

  block <- back$analysis_plan[[1]]
  result <- sframe_run_term_context(data, block$roles, block$options, back)
  expect_null(result$error)
  expect_identical(result$term, "great")
  expect_true(nrow(result$table) > 0)
})

test_that("a blank options$term still fails with the documented setup error", {
  inst <- fresh_text_instrument(list())
  p <- tempfile(fileext = ".sframe")
  write_sframe(inst, p)
  back <- read_sframe(p)

  data <- data.frame(fb = rep("some feedback text here", 10), stringsAsFactors = FALSE)
  block <- back$analysis_plan[[1]]
  result <- sframe_run_term_context(data, block$roles, block$options, back)
  expect_identical(
    result$error,
    "Term context needs a keyword: set roles$term or options$term."
  )
})
