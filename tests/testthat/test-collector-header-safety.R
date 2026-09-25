# tests/testthat/test-collector-header-safety.R
#
# Batch 5, finding 12. The Google Sheets collector mapped each submission onto
# the sheet's header after trimming it, with no check for a duplicate or blank
# heading, and with nothing stopping 2 submissions interleaving while the header
# was being created or extended. A duplicate heading put the same answer in 2
# columns, and a blank one shifted the meaning of what followed.
#
# An ambiguous header now sends the raw submission to an "Unmapped submissions"
# sheet, so the answer is neither mis-filed nor lost, and header work and the
# append run inside a script lock.

seed_header <- function(ctx, header) {
  ctx$assign("__seed", header)
  ctx$eval("var __s = __ss.insertSheet('Responses'); __s.rows.push(__seed.slice());")
}

responses_rows <- function(ctx) ctx$get("__ss.sheets['Responses'].rows.length")

test_that("12: a duplicate heading sends the submission to the unmapped sheet", {
  ctx <- apps_script_context(c("respondent_id", "q1"))
  seed_header(ctx, c("respondent_id", "q1", "q1"))
  apps_script_post(ctx, c(respondent_id = "r1", q1 = "yes"))

  expect_identical(responses_rows(ctx), 1L)
  unmapped <- ctx$get("__ss.sheets['Unmapped submissions'].rows")
  expect_true(any(grepl("\"q1\":\"yes\"", unlist(unmapped), fixed = TRUE)))
  expect_true(any(grepl("q1", unlist(unmapped), fixed = TRUE)))
})

test_that("12: a blank heading between columns is treated as ambiguous", {
  ctx <- apps_script_context(c("respondent_id", "q1", "q2"))
  seed_header(ctx, c("respondent_id", "", "q2"))
  apps_script_post(ctx, c(respondent_id = "r1", q1 = "a", q2 = "b"))

  expect_identical(responses_rows(ctx), 1L)
  expect_false(is.null(ctx$get("__ss.sheets['Unmapped submissions']")))
})

test_that("12: header work and the append run inside a script lock", {
  ctx <- apps_script_context(c("respondent_id", "q1"))
  apps_script_post(ctx, c(respondent_id = "r1", q1 = "a"))
  apps_script_post(ctx, c(respondent_id = "r2", q1 = "b"))
  expect_identical(ctx$get("__lock.waited"), 2L)
  expect_identical(ctx$get("__lock.released"), 2L)
})

test_that("12: an ordinary header still maps every answer", {
  ctx <- apps_script_context(c("respondent_id", "q1"))
  apps_script_post(ctx, c(respondent_id = "r1", q1 = "a"))
  expect_identical(apps_script_cell(ctx, 1, "q1"), "a")
})
