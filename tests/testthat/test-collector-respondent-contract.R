# tests/testthat/test-collector-respondent-contract.R
# The contract between the two halves of static collection, which the two
# existing suites each verified one side of and neither checked across.
#
# The collector reports its outcome in a JSON reply. The respondent's page posts
# no-cors, so it never reads that reply, whatever it says. Every claim the page
# makes must therefore be true under the WEAKEST outcome the collector can
# return, not the best one. Before the pre-publication review it asserted the
# best: "your response has been recorded", with the download hidden and a reload
# button that dropped the only copy.
#
# test-collector-formula-injection.R proved the collector warns. The delivery
# tests proved the page reaches its 'sent' state. Both passed while the
# participant was being told something false, because nothing tested the join.

# Every outcome doPost() can return for a well-formed submission, with whether
# the response row is on the Responses sheet afterwards.
collector_outcomes <- function() {
  list(
    list(name = "stored literally", setup = "", status = "ok", stored = TRUE),
    list(name = "header unusable, filed as unmapped",
         # a duplicate heading leaves no single column for an answer. The sheet
         # is created here because doPost() otherwise makes it on first use.
         setup = "__ss.insertSheet('Responses').rows.push(['q1','q1']);",
         status = "unmapped", stored = FALSE),
    list(name = "Sheets API absent, write refused",
         setup = "Sheets = undefined;",
         status = "error", stored = FALSE)
  )
}

test_that("the collector's own reply distinguishes stored from not stored", {
  skip_if_not_installed("V8")
  for (case in collector_outcomes()) {
    ctx <- apps_script_context(c("q1"))
    if (nzchar(case$setup)) ctx$eval(case$setup)
    apps_script_post(ctx, c(q1 = "=1+1"))
    reply <- apps_script_reply(ctx)

    expect_equal(reply$status, case$status, info = case$name)

    rows <- ctx$get("__ss.sheets['Responses'].rows.length")
    # the header occupies row 0, so a stored response makes the length 2
    expect_equal(rows > 1, case$stored, info = case$name)
  }
})

test_that("two of the three outcomes store nothing, and none is readable", {
  skip_if_not_installed("V8")
  # The point of the coupling: a majority of reachable outcomes leave the
  # participant's answers nowhere, and the browser cannot tell which it got.
  not_stored <- vapply(collector_outcomes(), function(x) !x$stored, logical(1))
  expect_gt(sum(not_stored), 0)

  src <- sframe_installed_text("static_survey", "template.html")
  # no-cors is what makes the reply unreadable, so it is asserted rather than
  # assumed: a future change to cors would let the page do better than this.
  expect_match(src, "mode:'no-cors'", fixed = TRUE)
  expect_false(grepl("response.json()", src, fixed = TRUE))
})

test_that("the page claims nothing that is false under the weakest outcome", {
  skip_if_not_installed("V8")
  instr <- sf_instrument("Coupling", components = list(
    sf_item("q1", "How was it?", type = "text")
  ))
  ctx <- static_survey_context(instr, endpoint_url = "https://example.com/collect")
  ctx$eval("__fetchFails = false; responses['q1'] = 'fine'; doSubmit();")
  expect_equal(ctx$get("deliveryState"), "sent")

  html <- ctx$get("document.getElementById('app').innerHTML")

  # Under "unmapped" and under "write refused" nothing was stored, so a claim of
  # recording is false in both. The page must not make it.
  expect_false(grepl("has been recorded", html, fixed = TRUE))
  expect_false(grepl("saved", html, fixed = TRUE))
  # It says what is true of all three: the answers left the browser.
  expect_match(html, "have been sent", fixed = TRUE)
  expect_match(html, "cannot confirm", fixed = TRUE)
  # And the copy that survives a non-storing outcome stays reachable.
  expect_match(html, "Download my response", fixed = TRUE)
})

test_that("the participant is never walked away from an unstored response", {
  skip_if_not_installed("V8")
  instr <- sf_instrument("Coupling", components = list(
    sf_item("q1", "How was it?", type = "text")
  ))
  instr$render <- list(thankyou = list(redirect_url = "https://example.com/next"))
  ctx <- static_survey_context(instr, endpoint_url = "https://example.com/collect")
  ctx$eval("__fetchFails = false; responses['q1'] = 'fine'; doSubmit();")

  # no timer carries them off the screen holding the only copy
  ctx$eval("__runTimeouts();")
  expect_equal(ctx$get("window.location.href"), "")

  # and clearing the page for the next participant asks first
  ctx$eval("__confirmAnswer = false; __reloads = 0; restartSurvey();")
  expect_equal(ctx$get("__reloads"), 0)
})
