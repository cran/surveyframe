# tests/testthat/test-read-responses-boundary.R
#
# read_responses(), batch 5 findings 5, 6 and 7.
#
# 5. read.csv() inferred types, so a respondent id of 001 became 1 and a text
#    answer of NA became missing. A data frame took a different route, so the
#    same responses could come back different by source.
# 6. A multi-column item counted as present when any column started with its
#    id, so a matrix missing a row, or offering only an unknown suffix, raised
#    no warning.
# 7. Two columns with the same name were silently reduced to one.

instrument <- function() {
  sf_instrument(title = "Reader", components = list(
    sf_choices("ag5", 1:5, as.character(1:5)),
    sf_item("q1", "Rating", "likert", choice_set = "ag5"),
    sf_item("note", "Comment", "text"),
    sf_item("mx", "Grid", "matrix", choice_set = "ag5", matrix_items = c("a", "b"))
  ))
}

csv_file <- function(lines) {
  path <- tempfile(fileext = ".csv")
  writeLines(lines, path)
  path
}

test_that("5: identifiers and literal text survive a CSV read", {
  path <- csv_file(c("respondent_id,q1,note,mx__a,mx__b",
                     "001,4,NA,3,2",
                     "002,5,fine,1,5"))
  res <- read_responses(path, instrument(), respondent_id = "respondent_id")
  expect_identical(res$respondent_id, c("001", "002"))
  expect_identical(res$note, c("NA", "fine"))
  expect_identical(res$q1, c(4, 5))
  expect_identical(res$mx__b, c(2, 5))
})

test_that("5: CSV, typed data frame and all-character data frame read the same", {
  path <- csv_file(c("respondent_id,q1,note,mx__a,mx__b",
                     "001,4,ok,3,2",
                     "002,,fine,1,5"))
  from_csv <- read_responses(path, instrument(), respondent_id = "respondent_id")
  typed <- data.frame(respondent_id = c("001", "002"), q1 = c(4, NA),
                      note = c("ok", "fine"), mx__a = c(3, 1), mx__b = c(2, 5),
                      stringsAsFactors = FALSE)
  as_text <- data.frame(respondent_id = c("001", "002"), q1 = c("4", ""),
                        note = c("ok", "fine"), mx__a = c("3", "1"), mx__b = c("2", "5"),
                        stringsAsFactors = FALSE)
  from_typed <- read_responses(typed, instrument(), respondent_id = "respondent_id")
  from_text <- read_responses(as_text, instrument(), respondent_id = "respondent_id")
  expect_identical(from_typed, from_csv)
  expect_identical(from_text, from_csv)
})

test_that("6: a matrix with a missing row column is reported", {
  path <- csv_file(c("q1,note,mx__a", "4,ok,3"))
  expect_warning(read_responses(path, instrument()), "mx__b")
})

test_that("6: an unknown suffix does not count as the matrix being present", {
  path <- csv_file(c("q1,note,mx__zzz", "4,ok,3"))
  # the matrix is reported as an absent item, and the stray column as undeclared
  expect_warning(
    expect_warning(read_responses(path, instrument(), strict = FALSE),
                   "absent from the response data: mx"),
    "mx__zzz")
})

test_that("7: duplicate response headers are refused", {
  path <- csv_file(c("q1,q1,note,mx__a,mx__b", "4,5,ok,3,2"))
  expect_error(read_responses(path, instrument()), "q1", class = "sframe_error")
  dup <- data.frame(q1 = 4, q1 = 5, check.names = FALSE)
  expect_error(read_responses(dup, instrument()), "q1", class = "sframe_error")
})
