# Batch 7's findings on what the static survey tells a participant about
# their own progress, run under V8 against the stub DOM (helper-v8-dom.R).
#
# #12. Progress counted the parent response key for every type but matrix, and
#      pairwise and criteria-weight answers live in child keys, so a completed
#      comparison and allocation both read as unanswered. Separately, returning
#      to a completed allocation showed "Total: 0 / 100".
# #13. setRating() updated the answer, the stars and the progress, and left
#      branching alone, so a question a rating reveals stayed hidden until
#      some other control refreshed it. Validation then demanded an answer to
#      a question the participant could not see.

mixed_instrument <- function() {
  sf_instrument("Progress", components = list(
    sf_choices("ag3", values = 1:3, labels = c("Low", "Mid", "High")),
    sf_item("pick", "Pick one", type = "single_choice", choice_set = "ag3"),
    sf_item("rate", "Rate us", type = "rating", rating_max = 5),
    sf_item("pw", "Compare the criteria", type = "pairwise_comparison",
            comparison_items = c("cost", "speed"), comparison_scale = "saaty"),
    sf_item("cw", "Divide 100 points", type = "criteria_weight",
            comparison_items = c("cost", "speed"))
  ))
}

# Sets a response key and refreshes progress, as an answer handler would.
set_response <- function(ctx, key, value) {
  ctx$eval(sprintf("responses[%s] = %s;", jsonlite::toJSON(key, auto_unbox = TRUE),
                   jsonlite::toJSON(as.character(value), auto_unbox = TRUE)))
}

progress_label <- function(ctx) {
  ctx$eval("updateProgress();")
  ctx$get("__els['progTitle'] ? __els['progTitle'].textContent : ''")
}

test_that("12: a completed comparison and allocation count as answered", {
  skip_if_not_installed("V8")
  ctx <- static_survey_context(mixed_instrument())
  ctx$eval("screen='survey';")

  expect_equal(progress_label(ctx), "0 of 4 answered")

  set_response(ctx, "pick", "2")
  expect_equal(progress_label(ctx), "1 of 4 answered")

  set_response(ctx, "rate", "4")
  expect_equal(progress_label(ctx), "2 of 4 answered")

  # every pair judged: the answer lives under the child keys
  set_response(ctx, "pw__cost__vs__speed", "3")
  expect_equal(progress_label(ctx), "3 of 4 answered")

  # a part-filled allocation is still incomplete
  set_response(ctx, "cw__cost", "60")
  expect_equal(progress_label(ctx), "3 of 4 answered")

  set_response(ctx, "cw__speed", "40")
  expect_equal(progress_label(ctx), "4 of 4 answered")
})

test_that("12: returning to a filled allocation shows its real total", {
  skip_if_not_installed("V8")
  ctx <- static_survey_context(mixed_instrument())
  set_response(ctx, "cw__cost", "60")
  set_response(ctx, "cw__speed", "40")

  html <- static_render_item(ctx, "cw")
  # the rendered total reflects the restored answers, where it used to say 0
  expect_match(html, "Total: 100 / 100", fixed = TRUE)
})

test_that("13: a rating reveals the question that depends on it", {
  skip_if_not_installed("V8")
  instr <- sf_instrument("Branching", components = list(
    sf_item("rate", "Rate us", type = "rating", rating_max = 5),
    sf_item("why", "What went wrong?", type = "text"),
    sf_branch("why", depends_on = "rate", operator = "<", value = 3)
  ))
  ctx <- static_survey_context(instr)
  # render, then settle the DOM the way a first paint does
  ctx$eval("screen='survey'; renderItem(SF.items[0]); renderItem(SF.items[1]); updateBranching();")
  expect_true(ctx$get("document.getElementById('item_why').classList.contains('hidden')"))

  # the participant rates 2, which is what the branch turns on
  ctx$eval("setRating('rate', 2, 5);")
  expect_true(ctx$get("isVisible('why')"))
  # the element is no longer marked hidden, so the question is on screen
  expect_false(ctx$get("document.getElementById('item_why').classList.contains('hidden')"))
})

test_that("13: a rating that hides a question hides it at once", {
  skip_if_not_installed("V8")
  instr <- sf_instrument("Branching", components = list(
    sf_item("rate", "Rate us", type = "rating", rating_max = 5),
    sf_item("why", "What went wrong?", type = "text"),
    sf_branch("why", depends_on = "rate", operator = "<", value = 3)
  ))
  ctx <- static_survey_context(instr)
  ctx$eval("screen='survey'; renderItem(SF.items[0]); renderItem(SF.items[1]); updateBranching();")
  ctx$eval("setRating('rate', 1, 5);")
  expect_false(ctx$get("document.getElementById('item_why').classList.contains('hidden')"))

  ctx$eval("setRating('rate', 5, 5);")
  expect_false(ctx$get("isVisible('why')"))
  expect_true(ctx$get("document.getElementById('item_why').classList.contains('hidden')"))
})
