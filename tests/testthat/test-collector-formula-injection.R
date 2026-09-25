# tests/testthat/test-collector-formula-injection.R
#
# A8. The collector wrote each row with appendRow(), which applies Sheets'
# user-entered semantics. A respondent whose answer began with "=" had it
# parsed as a formula before the researcher ever saw it: "=1+1" was stored as
# 2, and "=IMPORTXML(...)" was evaluated inside the researcher's own sheet.
# Numeric-looking codes lost their leading zeros the same way.
#
# The answer is altered between submission and storage, so nothing downstream
# can recover it. The mock in helper-apps-script.R models the coercion, which
# is what makes this visible at all.

test_that("an answer beginning with = is stored as the text the participant typed", {
  cols <- c("respondent_id", "submitted_at", "q_comment")
  ctx <- apps_script_context(cols)
  apps_script_post(ctx, c(respondent_id = "r1", submitted_at = "t1",
                          q_comment = "=1+1"))

  expect_identical(apps_script_cell(ctx, 1, "q_comment"), "=1+1")
})

test_that("a formula-shaped answer is never evaluated in the researcher's sheet", {
  cols <- c("respondent_id", "submitted_at", "q_comment")
  ctx <- apps_script_context(cols)
  hostile <- "=IMPORTXML(\"http://example.invalid/x\",\"//a\")"
  apps_script_post(ctx, c(respondent_id = "r1", submitted_at = "t1",
                          q_comment = hostile))

  stored <- apps_script_cell(ctx, 1, "q_comment")
  expect_identical(stored, hostile)
  expect_false(grepl("#FORMULA", stored, fixed = TRUE))
})

test_that("a leading-zero code keeps its zeros", {
  cols <- c("respondent_id", "submitted_at", "q_site")
  ctx <- apps_script_context(cols)
  apps_script_post(ctx, c(respondent_id = "r1", submitted_at = "t1",
                          q_site = "007"))

  expect_identical(apps_script_cell(ctx, 1, "q_site"), "007")
})

test_that("ordinary answers and header mapping are unaffected", {
  cols <- c("respondent_id", "submitted_at", "q_freq", "q_prior")
  ctx <- apps_script_context(cols)
  apps_script_post(ctx, c(respondent_id = "r1", submitted_at = "t1",
                          q_freq = "Daily", q_prior = "Yes"))
  apps_script_post(ctx, c(respondent_id = "r2", submitted_at = "t2",
                          q_freq = "Weekly", q_prior = "No"))

  expect_identical(ctx$get("__ss.sheets['Responses'].rows[0]"), cols)
  expect_identical(apps_script_cell(ctx, 1, "q_freq"), "Daily")
  expect_identical(apps_script_cell(ctx, 2, "q_prior"), "No")
  expect_identical(apps_script_cell(ctx, 2, "respondent_id"), "r2")
})

test_that("the builder's inlined collector carries the same text-write fix", {
  # The builder ships its own copy of the collector script. It is maintained
  # by hand, so a fix applied to inst/static_survey/collector_template.gs
  # alone leaves every GUI-built survey still writing user-entered values.
  # That drift is what this asserts against.
  p <- system.file("builder", "survey_builder.html", package = "surveyframe")
  skip_if(!nzchar(p) || !file.exists(p), "builder not found")
  builder <- paste(readLines(p, warn = FALSE), collapse = "\n")

  expect_true(grepl("appendResponseRow_", builder, fixed = TRUE))
  expect_true(grepl("setNumberFormat(\"@\")", builder, fixed = TRUE))
  expect_false(grepl("sheet.appendRow(row)", builder, fixed = TRUE))
  # and the inlined copy fails closed too, which is the whole point of gate 1
  expect_true(grepl("this response was NOT saved", builder, fixed = TRUE))
})

test_that("neither copy of the collector writes user-entered values", {
  paths <- c(
    system.file("static_survey", "collector_template.gs", package = "surveyframe"),
    system.file("builder", "survey_builder.html", package = "surveyframe")
  )
  skip_if(!all(nzchar(paths) & file.exists(paths)), "shipped assets not found")

  for (p in paths) {
    src <- paste(readLines(p, warn = FALSE), collapse = "\n")
    expect_false(grepl("sheet.appendRow(", src, fixed = TRUE),
                 info = paste("user-entered write still present in", basename(p)))
  }
})

# A8, reopened by review: the original fix set the plain-text number format and
# then called setValues(). Google documents setValues() as applying
# user-entered semantics and promises nothing about the number format
# suppressing that, and the mock used to encode the assumption it was meant to
# test by returning the string unchanged whenever the format was "@". The mock
# now parses regardless of format, so an answer can only survive literally
# through the Sheets API's documented RAW option.

test_that("A8: the row is written through the documented RAW option", {
  skip_if_not_installed("V8")
  ctx <- apps_script_context(c("respondent_id", "q1"))
  apps_script_post(ctx, c(respondent_id = "r1", q1 = "=1+1"))

  writes <- ctx$get("__rawWrites")
  expect_gt(nrow(writes), 0)
  expect_true(all(writes$option == "RAW"))
})

test_that("A8: a formula-like answer is stored as the participant typed it", {
  skip_if_not_installed("V8")
  ctx <- apps_script_context(c("respondent_id", "q1", "q2", "q3"))
  apps_script_post(ctx, c(respondent_id = "r1", q1 = "=1+1",
                          q2 = '=IMPORTXML("http://x","//a")', q3 = "007"))
  row <- ctx$get("__ss.sheets['Responses'].rows[1]")
  expect_equal(row[[2]], "=1+1")
  expect_equal(row[[3]], '=IMPORTXML("http://x","//a")')
  expect_equal(row[[4]], "007")
})

test_that("A8: the response says which write path stored the row", {
  skip_if_not_installed("V8")
  ctx <- apps_script_context(c("respondent_id", "q1"))
  apps_script_post(ctx, c(respondent_id = "r1", q1 = "ok"))
  parsed <- apps_script_reply(ctx)
  expect_equal(parsed$status, "ok")
  expect_equal(parsed$stored, "raw")
  expect_null(parsed$warning)
})

# Reopened a second time by the pre-publication review. The fallback used to
# write through setValues() and reply {status: "ok"}, with the degradation named
# only in a `warning` field. The respondent's browser posts no-cors and cannot
# read any of it, so the participant saw "your response has been recorded" over
# an answer the collector had just mangled. A write that cannot be performed
# literally is now refused, which leaves the page holding the only copy and
# able to say so.

test_that("A8: without the advanced service the response row is refused", {
  skip_if_not_installed("V8")
  ctx <- apps_script_context(c("respondent_id", "q1"))
  # A researcher who skipped the Services step has no Sheets global at all,
  # which is what the source guards on with typeof. Disabling the mock's
  # update() instead makes it throw, and the collector then fails closed for
  # the wrong reason, so this must be the undefined case.
  ctx$eval("Sheets = undefined;")
  apps_script_post(ctx, c(respondent_id = "r1", q1 = "=1+1"))
  parsed <- apps_script_reply(ctx)

  expect_equal(parsed$status, "error")
  # the message has to name the step that fixes it, because this is what the
  # researcher reads in the collector's own execution log
  expect_match(parsed$message, "Sheets", fixed = TRUE)

  # and nothing was stored: the header is row 0, so a refused write leaves no
  # row 1 at all. A partially written row would be worse than none.
  expect_equal(ctx$get("__ss.sheets['Responses'].rows.length"), 1L)
})

test_that("A8: no shipped path stores a response through user-entered semantics", {
  skip_if_not_installed("V8")
  p <- sframe_installed_path("static_survey", "collector_template.gs")
  skip_if(!nzchar(p), "collector template not found")
  src <- paste(readLines(p, warn = FALSE), collapse = "\n")

  # setValues() applies user-entered semantics. It may appear for a diagnostic
  # sheet, never for a response row, so the response path is checked by name.
  # Asserted present first: sub() returns its input unchanged when the pattern
  # does not match, so a renamed function would leave this test passing on the
  # file's opening comment.
  expect_match(src, "function appendResponseRow_", fixed = TRUE)
  body <- sub(".*function appendResponseRow_", "", src)
  body <- sub("\nfunction .*", "", body)
  expect_false(grepl("setValues(", body, fixed = TRUE),
               info = "the response write path must not use setValues()")
})
