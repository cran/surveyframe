# tests/testthat/test-response-append-schema.R
#
# A9. render_survey(save_responses = "csv") appended each response to the
# output file without reading its header. The row's column order is rebuilt
# from the instrument at submission, so reusing a file after reordering,
# adding or removing questions wrote values under another question's heading.
# The write succeeded and the participant saw the thank-you page.
#
# A row is now aligned to the file's own header by name when the 2 hold the
# same columns, and refused when they do not. render_survey() also checks the
# file when it starts, so the researcher learns before any participant does.

skip_if_not_installed("shiny")

row_of <- function(...) {
  data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
}

test_that("A9: a row with the same columns in another order lands under its own headings", {
  path <- tempfile(fileext = ".csv")
  sframe_append_response_csv(path, row_of(started_at = "t1", q1 = "a", q2 = "b"))
  sframe_append_response_csv(path, row_of(started_at = "t2", q2 = "B", q1 = "A"))

  saved <- utils::read.csv(path, colClasses = "character", check.names = FALSE)
  expect_identical(names(saved), c("started_at", "q1", "q2"))
  expect_identical(saved$q1, c("a", "A"))
  expect_identical(saved$q2, c("b", "B"))
})

test_that("A9: a row with different columns is refused and the file is left alone", {
  path <- tempfile(fileext = ".csv")
  sframe_append_response_csv(path, row_of(started_at = "t1", q1 = "a", q2 = "b"))
  before <- readLines(path)

  expect_error(
    sframe_append_response_csv(path, row_of(started_at = "t2", q1 = "A", q3 = "C")),
    class = "sframe_error"
  )
  expect_identical(readLines(path), before)
})

test_that("A9: the refusal names the columns that differ", {
  path <- tempfile(fileext = ".csv")
  sframe_append_response_csv(path, row_of(started_at = "t1", q1 = "a", q2 = "b"))

  err <- tryCatch(
    sframe_append_response_csv(path, row_of(started_at = "t2", q1 = "A", q3 = "C")),
    sframe_error = function(e) conditionMessage(e))
  expect_match(err, "q2")
  expect_match(err, "q3")
})

test_that("A9: an empty existing file is written with a header", {
  path <- tempfile(fileext = ".csv")
  file.create(path)
  sframe_append_response_csv(path, row_of(started_at = "t1", q1 = "a"))

  saved <- utils::read.csv(path, colClasses = "character", check.names = FALSE)
  expect_identical(names(saved), c("started_at", "q1"))
  expect_identical(saved$q1, "a")
})

two_items <- function(ids) {
  sf_instrument(title = "Append", components = lapply(ids, function(id)
    sf_item(id, paste("Question", id), "text")))
}

test_that("A9: render_survey() refuses to start on a file written for other questions", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("started_at,submitted_at,q1,q2", "t,t,a,b"), path)

  expect_error(
    render_survey(two_items(c("q1", "q3")), save_responses = "csv",
                  output_path = path),
    class = "sframe_error"
  )
})

test_that("A9: render_survey() starts on a file written for the same questions", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("started_at,submitted_at,q2,q1", "t,t,b,a"), path)

  expect_s3_class(
    render_survey(two_items(c("q1", "q2")), save_responses = "csv",
                  output_path = path),
    "shiny.appobj"
  )
})
