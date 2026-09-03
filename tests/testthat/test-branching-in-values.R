# tests/testthat/test-branching-in-values.R
# Guards multi-value `%in%` branch rules. The value arrives either as a vector,
# which sf_branch() documents, or as a comma-separated string, and all 3
# evaluators have to accept both. When they did not, a gated question stayed
# hidden whatever the respondent answered, with no error anywhere.

branch_instrument <- function(value) {
  sf_instrument(
    title = "Branching", version = "1.0.0",
    components = list(
      sf_choices("abc", c("a", "b", "c", "d"), c("A", "B", "C", "D")),
      sf_item("q1", "Pick one", type = "single_choice", choice_set = "abc"),
      sf_item("q2", "Gated", type = "text"),
      sf_branch(item_id = "q2", depends_on = "q1", operator = "%in%",
                value = value, action = "show")
    )
  )
}

test_that("sframe_branch_in_values() accepts both shapes a file can carry", {
  # the documented shape, a vector, which serialises to a JSON array
  expect_identical(surveyframe:::sframe_branch_in_values(c("a", "b", "c")),
                   c("a", "b", "c"))
  # the shape a hand-written file or an older builder carries
  expect_identical(surveyframe:::sframe_branch_in_values("a,b,c"),
                   c("a", "b", "c"))
  expect_identical(surveyframe:::sframe_branch_in_values("a, b , c"),
                   c("a", "b", "c"))
  # numerics survive the trip
  expect_identical(surveyframe:::sframe_branch_in_values(c(1, 2, 3)),
                   c("1", "2", "3"))
  # nothing to match is empty rather than an error
  expect_identical(surveyframe:::sframe_branch_in_values(NULL), character(0))
  expect_identical(surveyframe:::sframe_branch_in_values(character(0)),
                   character(0))
})

test_that("both R evaluators match every value of a multi-value %in% rule", {
  # This is the regression. Before the fix sframe_module_eval_op() returned
  # TRUE only for "a", the vector's first element, and .evaluate_branch()
  # returned FALSE for every value of the comma-separated form.
  for (value in list(c("a", "b", "c"), "a,b,c")) {
    for (actual in c("a", "b", "c")) {
      expect_true(
        surveyframe:::sframe_module_eval_op("%in%", actual, as.character(value)),
        info = paste("survey_module:", actual)
      )
      expect_true(
        surveyframe:::.evaluate_branch(
          list(operator = "%in%", value = value, action = "show"), actual
        ),
        info = paste("render_survey:", actual)
      )
    }
    # a value outside the set still does not match
    expect_false(
      surveyframe:::sframe_module_eval_op("%in%", "d", as.character(value))
    )
    expect_false(
      surveyframe:::.evaluate_branch(
        list(operator = "%in%", value = value, action = "show"), "d"
      )
    )
  }
})

test_that("a multi-value %in% rule survives write_sframe() as an array", {
  instr <- branch_instrument(c("a", "b", "c"))
  p <- tempfile(fileext = ".sframe")
  write_sframe(instr, p, overwrite = TRUE)
  written <- jsonlite::fromJSON(p, simplifyVector = FALSE)
  # the array shape is the contract sf_branch() documents, so it must reach
  # the file intact rather than being flattened on the way
  expect_length(written$branching[[1]]$value, 3L)
  back <- read_sframe(p)
  expect_identical(as.character(back$branching[[1]]$value), c("a", "b", "c"))
})

test_that("validate_sframe() flags a %in% rule with no values to match", {
  empty <- branch_instrument(character(0))
  v <- validate_sframe(empty, strict = FALSE)
  expect_false(v$valid)
  expect_true(any(grepl("never be satisfied", unlist(v$problems))))
  # and the check appears in the roster either way, so a pass is
  # distinguishable from a check that never ran
  expect_true("branching_values" %in% summary(v)$check)

  ok <- branch_instrument(c("a", "b"))
  expect_true(validate_sframe(ok, strict = FALSE)$valid)
})

test_that("the exported survey's evaluate() consumes an array value", {
  # The defect lived in the R-to-JS serialisation boundary, so a test on the R
  # side alone would not have caught it. This reads the shipped template and
  # asserts the branch that broke is gone and the array branch is present.
  tpl <- readLines(
    system.file("static_survey", "template.html", package = "surveyframe"),
    warn = FALSE
  )
  builder <- readLines(
    system.file("builder", "survey_builder.html", package = "surveyframe"),
    warn = FALSE
  )
  for (src in list(tpl, builder)) {
    # the old unguarded split is what threw on an array
    expect_false(any(grepl("value.split(',')", src, fixed = TRUE)))
    expect_true(any(grepl("Array.isArray(value)", src, fixed = TRUE)))
  }
})
