# tests/testthat/test-sheets-sharing-guidance.R
#
# G9. The export_google_sheet() help told researchers the collection sheet
# "must be shared so that anyone with the link can edit". That instructs a
# researcher to make participant data world readable and world writable, and
# the collector never needed it: the generated Apps Script is bound to the
# sheet and writes through SpreadsheetApp.getActiveSpreadsheet(), under the
# authority of whoever deploys it.
#
# This guards the documentation source, because the harm is in what a reader
# is told to do before any code runs.

roxygen_source <- function() {
  path <- sframe_source_path("R", "google_sheets.R")
  skip_if(is.na(path), "no source tree here")
  skip_if_not(file.exists(path), "package source not available")
  lines <- readLines(path, warn = FALSE)
  lines[grepl("^\\s*#'", lines)]
}

test_that("the Sheets help never advises link-edit sharing of the sheet", {
  doc <- paste(roxygen_source(), collapse = " ")

  # The exact instruction that shipped, and the near variants of it.
  expect_false(grepl("anyone with the link can edit", doc, ignore.case = TRUE))
  expect_false(grepl("shared so that anyone", doc, ignore.case = TRUE))
  expect_false(grepl("must be shared", doc, ignore.case = TRUE))
})

test_that("the Sheets help states the sheet stays private", {
  doc <- paste(roxygen_source(), collapse = " ")

  expect_true(grepl("Keep the sheet private", doc, fixed = TRUE))
  # The reader must be able to tell the web app's access setting, which is
  # legitimately "Anyone", apart from the sheet's own sharing.
  expect_true(grepl("getActiveSpreadsheet", doc, fixed = TRUE))
})
