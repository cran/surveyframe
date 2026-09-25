# Batch 7 #18: the builder autosaves a dirty session every 30 seconds and on
# unload, and its recovery refused to offer anything older than 120 minutes
# even though the data was still in storage. Returning the next day showed an
# empty builder with no visible way to reach the saved work. Storage failures
# were swallowed too, so a researcher in a private window believed their work
# was being saved when nothing was.

builder_source <- function() {
  sframe_installed_text("inst", "builder", "survey_builder.html")
}

autosave_block <- function() {
  src <- builder_source()
  regmatches(src, regexpr("// -- Autosave(?s).*?// -- Toast", src,
                          perl = TRUE))[[1]]
}

test_that("18: recovery is offered whatever its age", {
  block <- autosave_block()
  expect_true(nzchar(block))
  # the 120-minute cutoff that discarded recoverable work is gone
  expect_false(grepl("age>120", block, fixed = TRUE))
  expect_match(block, "function recoveryAge(", fixed = TRUE)
})

test_that("18: the age is reported in units a researcher reads", {
  block <- autosave_block()
  age <- regmatches(block, regexpr("function recoveryAge\\((?s).*?\\n\\}",
                                   block, perl = TRUE))[[1]]
  expect_match(age, "min", fixed = TRUE)
  expect_match(age, "hour", fixed = TRUE)
  expect_match(age, "day", fixed = TRUE)
})

test_that("18: a storage failure is disclosed, not swallowed", {
  block <- autosave_block()
  # the save path reports a failure instead of an empty catch
  expect_match(block, "autosaveUnavailable", fixed = TRUE)
  expect_false(grepl("catch(e){}}\nfunction chkAutoSave", block, fixed = TRUE))
})

test_that("18: the banner keeps its dismiss, which is the explicit discard", {
  src <- builder_source()
  expect_match(src, 'id="recBan"', fixed = TRUE)
  expect_match(src, "function dismissRec(", fixed = TRUE)
  # dismissing is what clears storage, so nothing else throws the work away
  dismiss <- regmatches(src, regexpr("function dismissRec\\((?s).*?\\n\\}", src,
                                     perl = TRUE))[[1]]
  expect_match(dismiss, "removeItem", fixed = TRUE)
})

test_that("18: the builder's script still parses", {
  skip_if_not_installed("V8")
  src <- builder_source()
  blocks <- regmatches(src, gregexpr("(?s)<script>.*?</script>", src,
                                     perl = TRUE))[[1]]
  body <- sub("(?s)^<script>", "", blocks[[length(blocks)]], perl = TRUE)
  body <- sub("(?s)</script>$", "", body, perl = TRUE)
  ctx <- V8::v8()
  ctx$assign("__src", body)
  expect_true(ctx$get("(function(){ try { new Function(__src); return true; }
                        catch (e) { return false; } })()"))
})

# Batch 7 #19: the builder's Preview renders from its own markup and does not
# run the deployed survey's answer, validation and branching lifecycle, while
# launch_builder()'s help called it a full live render. A researcher could not
# establish from it that a required or conditional survey is answerable. The
# review's second option is taken: say what the preview is, and name the route
# that does exercise the respondent's path.

test_that("19: the preview says what it does not exercise", {
  src <- builder_source()
  note <- regmatches(src, regexpr('<div class="pv-note"(?s).*?</div>', src,
                                  perl = TRUE))[[1]]
  expect_true(nzchar(note))
  expect_match(note, "Layout preview", fixed = TRUE)
  expect_match(note, "branching", fixed = TRUE)
  # and it names the route that does test answering
  expect_match(note, "Export survey", fixed = TRUE)
})

test_that("19: the help no longer calls it a full live render", {
  help <- sframe_source_text("R", "builder.R")
  expect_false(grepl("full live render", help, fixed = TRUE))
  expect_match(help, "layout preview", fixed = TRUE)
  expect_match(help, "export_static_survey()", fixed = TRUE)
})
