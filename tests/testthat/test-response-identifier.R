# The per-response identifier, decided by the owner on 2026-09-19 after being
# deferred three times (A9, batch 5 #12, batch 4 #22).
#
# The static survey has written a generated respondent_id since the first
# release. The Shiny path wrote none, so the same instrument produced different
# column sets depending on how it was deployed, and neither the duplicate check
# nor an idempotent retry had a key to work from.
#
# A response file written before this version has no such column. Every question
# column still matches, so that file stays appendable and the response goes in
# without an id, rather than the append being refused mid-study.

id_instrument <- function() {
  sf_instrument("Identified", components = list(
    sf_item("q1", "How was it?", type = "text")))
}

test_that("a Shiny response carries an identifier, first", {
  row <- sframe_response_row(id_instrument(), list(q1 = "fine"), list(),
                             Sys.time())
  expect_equal(names(row)[1], "respondent_id")
  expect_match(row$respondent_id, "^R[0-9A-Z]{8}$")
})

test_that("the identifier matches the shape the static survey writes", {
  # the static template builds "R" plus 8 upper-case base36 characters
  for (i in 1:20) {
    expect_match(sframe_new_response_id(), "^R[0-9A-Z]{8}$")
  }
  expect_gt(length(unique(replicate(200, sframe_new_response_id()))), 190)
})

test_that("a retry writes the same identifier, never a second row", {
  path <- tempfile(fileext = ".csv")
  row <- sframe_response_row(id_instrument(), list(q1 = "fine"), list(),
                             Sys.time())
  state <- sframe_new_submission_state()

  first <- sframe_persist_response(row, path, function(...) stop("boom"), state)
  expect_true(first$saved)
  second <- sframe_persist_response(row, path, function(...) NULL, first$state)
  expect_true(second$notified)

  written <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(nrow(written), 1)
  expect_equal(written$respondent_id, row$respondent_id)
})

test_that("two responses get different identifiers", {
  path <- tempfile(fileext = ".csv")
  for (answer in c("first", "second")) {
    row <- sframe_response_row(id_instrument(), list(q1 = answer), list(),
                               Sys.time())
    sframe_append_response_csv(path, row)
  }
  written <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(nrow(written), 2)
  expect_length(unique(written$respondent_id), 2)
})

test_that("a file written before the identifier existed still accepts a response", {
  path <- tempfile(fileext = ".csv")
  # a 0.4.1-shaped file: the same questions, and no respondent_id
  old <- data.frame(started_at = "2026-01-01T00:00:00Z",
                    submitted_at = "2026-01-01T00:01:00Z",
                    q1 = "collected earlier", stringsAsFactors = FALSE)
  utils::write.csv(old, path, row.names = FALSE)

  row <- sframe_response_row(id_instrument(), list(q1 = "collected today"),
                             list(), Sys.time())
  expect_message(sframe_append_response_csv(path, row),
                 class = "sframe_response_id_absent")

  written <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(nrow(written), 2)
  # the older file keeps its own columns, and the earlier row is untouched
  expect_equal(names(written), c("started_at", "submitted_at", "q1"))
  expect_equal(written$q1, c("collected earlier", "collected today"))
})

test_that("a genuinely different question set is still refused", {
  path <- tempfile(fileext = ".csv")
  other <- data.frame(respondent_id = "R00000001",
                      started_at = "t", submitted_at = "t",
                      something_else = "x", stringsAsFactors = FALSE)
  utils::write.csv(other, path, row.names = FALSE)
  row <- sframe_response_row(id_instrument(), list(q1 = "fine"), list(),
                             Sys.time())
  expect_error(sframe_append_response_csv(path, row), class = "sframe_error")
})
