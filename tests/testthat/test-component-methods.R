# tests/testthat/test-component-methods.R
# S3 print/format/summary methods for the sframe component classes.

test_that("each component class exposes print, format, and summary methods", {
  for (cls in c("sf_choices", "sf_item", "sf_scale",
                "sf_branch", "sf_check", "sf_model")) {
    available <- as.character(methods(class = cls))
    expect_true(any(grepl("^print\\.",   available)),
                info = paste("print method for", cls))
    expect_true(any(grepl("^format\\.",  available)),
                info = paste("format method for", cls))
    expect_true(any(grepl("^summary\\.", available)),
                info = paste("summary method for", cls))
  }
})

test_that("sf_choices methods return expected shapes", {
  cs <- sf_choices("agree5", 1:5,
                   c("Strongly disagree", "Disagree", "Neutral",
                     "Agree", "Strongly agree"))
  expect_output(print(cs), "sf_choices")
  expect_identical(print(cs), cs)
  expect_type(format(cs), "character")
  expect_match(format(cs), "5 option")
  expect_identical(summary(cs), cs)
})

test_that("sf_item methods return expected shapes", {
  it <- sf_item("q1", "How satisfied are you?", type = "likert",
                choice_set = "agree5")
  expect_output(print(it), "sf_item")
  expect_match(format(it), "likert")
  expect_identical(print(it), it)
})

test_that("sf_scale methods return expected shapes", {
  sc <- sf_scale("sat", "Satisfaction", items = c("q1", "q2", "q3"))
  expect_output(print(sc), "sf_scale")
  expect_match(format(sc), "3 item")
  expect_identical(summary(sc), sc)
})

test_that("sf_branch methods use the real field names", {
  br <- sf_branch("q2", depends_on = "q1", operator = "==",
                  value = "yes", action = "show")
  expect_output(print(br), "sf_branch")
  expect_match(format(br), "q2")
  expect_match(format(br), "q1")
  expect_identical(print(br), br)
})

test_that("sf_check methods return expected shapes", {
  ck <- sf_check("attn1", item_id = "q5", type = "attention",
                 pass_values = 3)
  expect_output(print(ck), "sf_check")
  expect_match(format(ck), "q5")
  expect_identical(summary(ck), ck)
})

test_that("sf_model methods report construct count", {
  mdl <- sf_model("m1", label = "Measurement model", type = "cfa")
  expect_output(print(mdl), "sf_model")
  expect_match(format(mdl), "construct")
  expect_identical(summary(mdl), mdl)
})
