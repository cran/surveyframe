# tests/testthat/test-amendments.R
# amend_sframe() is the disclosed-revision path around read_sframe()'s hash
# check: it does not weaken that check (an undisclosed direct file edit still
# hard-aborts, tested here too), it adds a structured place for a legitimate,
# disclosed change to be recorded inside the file itself. The two-tier split
# (pipeline vs design) is what keeps the package's design-time analysis-plan
# binding meaningful -- a design-tier change is disclosed but made more
# effortful, not silently as easy as a data correction.

amend_instr <- function(label = "q1") {
  item <- sf_item("q1", "How satisfied are you?", type = "text")
  sf_instrument("Demo", components = list(item))
}

amend_instr_revised <- function() {
  item <- sf_item("q1", "How satisfied are you overall?", type = "text")
  sf_instrument("Demo", components = list(item))
}

test_that("a disclosed pipeline amendment succeeds without a deviation report", {
  instr <- amend_instr()
  amended <- amend_sframe(
    instr, instr,
    reason_code = "data_correction",
    reason_text = "Corrected a mis-keyed respondent ID."
  )
  log <- amendment_log(amended)
  expect_equal(nrow(log), 1)
  expect_equal(log$tier, "pipeline")
  expect_equal(log$reason_code, "data_correction")
  expect_equal(log$signoff, "none")
  expect_true(is.na(log$deviation_report) || !nzchar(log$deviation_report))
})

test_that("bot_removal also defaults to pipeline tier", {
  instr <- amend_instr()
  amended <- amend_sframe(
    instr, instr,
    reason_code = "bot_removal",
    reason_text = "Removed 4 responses matching the bot-timing signature."
  )
  expect_equal(amendment_log(amended)$tier, "pipeline")
})

test_that("a design-tier amendment without deviation_report errors", {
  instr <- amend_instr()
  revised <- amend_instr_revised()
  expect_error(
    amend_sframe(
      instr, revised,
      reason_code = "model_respecification",
      reason_text = "Dropped a misfitting indicator."
    ),
    "deviation_report"
  )
})

test_that("a design-tier amendment with deviation_report succeeds and logs signoff: none by default", {
  instr <- amend_instr()
  revised <- amend_instr_revised()
  amended <- amend_sframe(
    instr, revised,
    reason_code = "model_respecification",
    reason_text = "Dropped a misfitting indicator.",
    deviation_report = "Item cross-loaded on the pilot data; dropped before the main run."
  )
  log <- amendment_log(amended)
  expect_equal(log$tier, "design")
  expect_equal(log$signoff, "none")
  expect_true(nzchar(log$deviation_report))
})

test_that("a design-tier amendment records a supplied second_signoff", {
  instr <- amend_instr()
  revised <- amend_instr_revised()
  amended <- amend_sframe(
    instr, revised,
    reason_code = "instrument_revision",
    reason_text = "Clarified item wording after a pilot round.",
    deviation_report = "Wording only; no change to the construct measured.",
    second_signoff = "Ethics Board Ref 2026-114"
  )
  expect_equal(amendment_log(amended)$signoff, "Ethics Board Ref 2026-114")
})

test_that("reason_code must be one of the controlled vocabulary", {
  instr <- amend_instr()
  expect_error(
    amend_sframe(instr, instr, reason_code = "because", reason_text = "x"),
    class = "rlang_error"
  )
})

test_that("reason_text must be non-empty", {
  instr <- amend_instr()
  expect_error(
    amend_sframe(instr, instr, reason_code = "data_correction", reason_text = ""),
    "reason_text"
  )
})

test_that("changed_fields reflects the actual diff between previous and instrument", {
  instr <- amend_instr()
  revised <- amend_instr_revised()
  amended <- amend_sframe(
    instr, revised,
    reason_code = "instrument_revision",
    reason_text = "Wording clarified.",
    deviation_report = "Wording only."
  )
  expect_match(amendment_log(amended)$changed_fields, "items")
})

test_that("amendment_log() returns a zero-row data frame for an unamended instrument", {
  instr <- amend_instr()
  log <- amendment_log(instr)
  expect_equal(nrow(log), 0)
  expect_true(all(c("timestamp", "reason_code", "tier", "signoff",
                    "previous_hash", "new_hash") %in% names(log)))
})

test_that("multiple amendments accumulate as an ordered log, not an overwrite", {
  instr <- amend_instr()
  a1 <- amend_sframe(instr, instr, reason_code = "data_correction", reason_text = "fix 1")
  a2 <- amend_sframe(a1, a1, reason_code = "bot_removal", reason_text = "fix 2")
  log <- amendment_log(a2)
  expect_equal(nrow(log), 2)
  expect_equal(log$reason_text, c("fix 1", "fix 2"))
})

test_that("a second amendment still accumulates when the 'after' object was not derived from the amended 'previous'", {
  # The existing accumulation test above always passes the same object as
  # both previous and instrument for the follow-up call (amend_sframe(a1,
  # a1, ...)), so instrument$amendments already carries the prior entry in
  # by construction. That shape never exercises the actual risk: a caller
  # (SurveyStudio's Amendments screen, or any external tool such as the
  # SurveyBuilder JS app) supplying a genuinely independent "after" object
  # that was never derived from the amended "previous" and so does not
  # carry its amendment history forward on its own. Found live: a second
  # amendment applied through SurveyStudio silently replaced the first
  # log entry instead of appending to it, because amend_sframe() based the
  # new log on instrument$amendments (the "after" object's own, absent,
  # history) rather than previous$amendments (the actual prior history).
  instr <- amend_instr()
  a1 <- amend_sframe(instr, instr, reason_code = "data_correction", reason_text = "fix 1")
  expect_equal(nrow(amendment_log(a1)), 1)

  fresh_after <- amend_instr_revised()  # built from scratch, carries no amendments
  a2 <- amend_sframe(a1, fresh_after, reason_code = "bot_removal", reason_text = "fix 2")
  log <- amendment_log(a2)
  expect_equal(nrow(log), 2)
  expect_equal(log$reason_text, c("fix 1", "fix 2"))
})

test_that("amendments survive a write_sframe()/read_sframe() round trip", {
  instr <- amend_instr()
  revised <- amend_instr_revised()
  amended <- amend_sframe(
    instr, revised,
    reason_code = "instrument_revision",
    reason_text = "Wording clarified.",
    deviation_report = "Wording only."
  )
  path <- tempfile(fileext = ".sframe")
  write_sframe(amended, path, overwrite = TRUE)
  back <- read_sframe(path)
  expect_equal(nrow(amendment_log(back)), 1)
  expect_equal(amendment_log(back)$reason_code, "instrument_revision")
})

test_that("an instrument with no amendments hashes identically to before this feature existed", {
  instr <- amend_instr()
  path <- tempfile(fileext = ".sframe")
  write_sframe(instr, path, overwrite = TRUE)
  back <- read_sframe(path)
  expect_equal(length(back$amendments %||% list()), 0)
  expect_false(grepl('"amendments"', paste(readLines(path), collapse = "")))
})

test_that("an undisclosed direct file edit still hard-aborts (amend_sframe does not weaken the hash check)", {
  instr <- amend_instr()
  path <- tempfile(fileext = ".sframe")
  write_sframe(instr, path, overwrite = TRUE)
  txt <- readLines(path)
  txt <- gsub("How satisfied are you\\?", "TAMPERED", txt, fixed = FALSE)
  writeLines(txt, path)
  expect_error(read_sframe(path), "Integrity check failed")
})
