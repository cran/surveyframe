# Batch 7 #8: the static survey told a participant their response had been
# recorded before it knew whether it had been delivered.
#
# The POST is no-cors, so the browser never sees the collector's answer and
# acceptance can never be confirmed. A network failure is a different matter:
# fetch rejects, and the template discarded that rejection with an empty
# .catch() and showed the thank-you screen regardless. A configured redirect
# then carried the participant away 2.5 seconds later, with the response in
# nobody's hands.

delivery_instrument <- function() {
  sf_instrument("Delivery", components = list(
    sf_item("q1", "How was it?", type = "text")
  ))
}

submit_with <- function(fails, endpoint = "https://example.com/collect",
                        thankyou = NULL) {
  instr <- delivery_instrument()
  if (!is.null(thankyou)) {
    instr$render <- list(thankyou = thankyou)
  }
  ctx <- static_survey_context(instr, endpoint_url = endpoint)
  ctx$eval(sprintf("__fetchFails = %s;", if (fails) "true" else "false"))
  ctx$eval("responses['q1'] = 'fine'; doSubmit();")
  ctx
}

app_html <- function(ctx) ctx$get("document.getElementById('app').innerHTML")

test_that("8: a failed delivery says so, and keeps the response reachable", {
  skip_if_not_installed("V8")
  ctx <- submit_with(fails = TRUE)

  expect_equal(ctx$get("deliveryState"), "failed")
  html <- app_html(ctx)
  # the claim that it was recorded is gone
  expect_false(grepl("has been recorded", html, fixed = TRUE))
  # and the participant is told, and given both ways out
  expect_match(html, "could not be sent", fixed = TRUE)
  expect_match(html, "Download my response", fixed = TRUE)
  expect_match(html, "Try sending again", fixed = TRUE)
})

test_that("8: a failed delivery cancels the redirect", {
  skip_if_not_installed("V8")
  ctx <- submit_with(fails = TRUE,
                     thankyou = list(redirect_url = "https://example.com/next"))
  ctx$eval("__runTimeouts();")
  expect_equal(ctx$get("window.location.href"), "")
})

test_that("8: retrying sends the same response again", {
  skip_if_not_installed("V8")
  ctx <- submit_with(fails = TRUE)
  expect_equal(ctx$get("__posts.length"), 1)

  ctx$eval("__fetchFails = false; retrySubmission();")
  expect_equal(ctx$get("__posts.length"), 2)
  expect_equal(ctx$get("deliveryState"), "sent")
  # the second attempt carries the same answers
  expect_equal(ctx$get("JSON.parse(__posts[0].body).q1"),
               ctx$get("JSON.parse(__posts[1].body).q1"))
})

test_that("8: a delivery that left the browser reports what it can", {
  skip_if_not_installed("V8")
  ctx <- submit_with(fails = FALSE)
  # "sent" is as much as a no-cors POST can establish: the request left
  # without error, and the collector's answer is unreadable
  expect_equal(ctx$get("deliveryState"), "sent")
  html <- app_html(ctx)
  expect_false(grepl("could not be sent", html, fixed = TRUE))
  expect_false(grepl("Try sending again", html, fixed = TRUE))
})

# Reopened by the pre-publication review. deliveryState 'sent' means the request
# left the browser, and nothing more: a no-cors POST is opaque, so a collector
# that replied "unmapped", or "error", or refused the write for want of the
# Sheets API, is indistinguishable here from one that stored the row. The screen
# claimed "your response has been recorded" on that state, hid the download,
# offered a reload that discards the only copy, and auto-redirected away from it.

test_that("8: an unconfirmed send does not claim the response was recorded", {
  skip_if_not_installed("V8")
  html <- app_html(submit_with(fails = FALSE))

  expect_false(grepl("has been recorded", html, fixed = TRUE))
  # and it says what is actually known, which is that receipt is unconfirmed
  expect_match(html, "cannot confirm", fixed = TRUE)
})

test_that("8: an unconfirmed send keeps the response downloadable", {
  skip_if_not_installed("V8")
  # the designer left the download off, and it is offered anyway, because this
  # page is holding the only copy anyone can prove exists
  html <- app_html(submit_with(fails = FALSE))
  expect_match(html, "Download my response", fixed = TRUE)
})

test_that("8: an unconfirmed send is never auto-redirected past", {
  skip_if_not_installed("V8")
  ctx <- submit_with(fails = FALSE,
                     thankyou = list(redirect_url = "https://example.com/next"))
  ctx$eval("__runTimeouts();")
  expect_equal(ctx$get("window.location.href"), "")
  # the participant can still choose to go, so the study is not a dead end
  expect_match(app_html(ctx), "https://example.com/next", fixed = TRUE)
})

test_that("8: restarting over an unconfirmed send asks before discarding it", {
  skip_if_not_installed("V8")
  ctx <- submit_with(fails = FALSE)
  ctx$eval("__confirmAnswer = false; __reloads = 0; restartSurvey();")

  expect_equal(ctx$get("__confirms.length"), 1)
  expect_match(ctx$get("__confirms[0]"), "download", ignore.case = TRUE)
  # declining leaves the page, and the response, exactly where they were
  expect_equal(ctx$get("__reloads"), 0)

  ctx$eval("__confirmAnswer = true; restartSurvey();")
  expect_equal(ctx$get("__reloads"), 1)
})

test_that("8: with no collector the screen still offers the download", {
  skip_if_not_installed("V8")
  ctx <- submit_with(fails = FALSE, endpoint = "")
  expect_equal(ctx$get("deliveryState"), "local")
  expect_equal(ctx$get("__posts.length"), 0)
  expect_match(app_html(ctx), "Download my response", fixed = TRUE)
})

test_that("8: a local survey with no collector still redirects", {
  skip_if_not_installed("V8")
  # With no endpoint there is nothing to be unconfirmed about, and the download
  # is the documented way the response travels, so the offer stays on screen and
  # the redirect waits for the participant to take it.
  ctx <- submit_with(fails = FALSE, endpoint = "",
                     thankyou = list(redirect_url = "https://example.com/next"))
  ctx$eval("__runTimeouts();")
  expect_equal(ctx$get("deliveryState"), "local")
  expect_equal(ctx$get("window.location.href"), "")
  expect_match(app_html(ctx), "https://example.com/next", fixed = TRUE)
})

# Reopened again: the first pass at gate 2 changed only the DEFAULT message, on
# the reasoning that a designer's own wording was their claim to make. That was
# wrong. The package is the thing that knows receipt is unconfirmed, and a custom
# message asserting storage produced a screen that said "your response has been
# recorded" and "cannot confirm" at the same time.

test_that("8: a custom thank-you message cannot assert storage either", {
  skip_if_not_installed("V8")
  ctx <- submit_with(fails = FALSE, thankyou = list(
    message = "Your response has been recorded. Thank you."))
  html <- app_html(ctx)

  expect_false(grepl("has been recorded", html, fixed = TRUE))
  expect_match(html, "cannot confirm", fixed = TRUE)
  expect_match(html, "Download my response", fixed = TRUE)
})

test_that("8: a custom message still shows where delivery is not in doubt", {
  skip_if_not_installed("V8")
  # No collector, so nothing was sent and nothing is being claimed about it. The
  # researcher's own wording is what the participant should read.
  ctx <- submit_with(fails = FALSE, endpoint = "", thankyou = list(
    message = "Thank you. Please email the file to the research team."))
  expect_match(app_html(ctx), "email the file", fixed = TRUE)
})

test_that("8: an unconfirmed send does not offer a destructive restart", {
  skip_if_not_installed("V8")
  # The review's gate: no "Submit another response" on a state that cannot
  # establish the answers are stored. A researcher on a shared device reloads.
  html <- app_html(submit_with(fails = FALSE))
  expect_false(grepl("Submit another response", html, fixed = TRUE))
})
