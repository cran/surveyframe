test_that("input-types demo loads and validates", {
  demo <- sframe_input_types_demo_data()

  expect_s3_class(demo$instrument, "sframe")
  expect_true(is.data.frame(demo$responses))
  expect_gt(nrow(demo$responses), 50)

  validation <- validate_sframe(demo$instrument, strict = FALSE)

  expect_true(validation$valid)
  expect_length(validation$problems, 0)
})

test_that("input-types demo covers all declared item types", {
  demo <- sframe_input_types_demo_data()

  observed_types <- sort(unique(vapply(
    demo$instrument$items,
    function(x) x$type,
    character(1)
  )))

  expected_types <- sort(c(
    "likert",
    "single_choice",
    "multiple_choice",
    "numeric",
    "text",
    "textarea",
    "date",
    "matrix",
    "slider",
    "ranking",
    "rating",
    "section_break",
    "text_block"
  ))

  expect_equal(observed_types, expected_types)
})

test_that("input-types demo has responses for response-producing items", {
  demo <- sframe_input_types_demo_data()

  display_only <- c("section_break", "text_block")

  response_item_ids <- vapply(
    Filter(
      function(x) !x$type %in% display_only,
      demo$instrument$items
    ),
    function(x) x$id,
    character(1)
  )

  expect_true(all(response_item_ids %in% names(demo$responses)))
})

test_that("display-only items are not required response columns", {
  instr <- sf_instrument(
    title = "Display-only read test",
    components = list(
      sf_item("intro", "Intro", type = "section_break"),
      sf_item("note", "Note", type = "text_block"),
      sf_item("age", "Age", type = "numeric")
    )
  )

  responses <- read_responses(
    data.frame(age = c(20, 30)),
    instr,
    strict = TRUE
  )

  expect_equal(names(responses), "age")
})
