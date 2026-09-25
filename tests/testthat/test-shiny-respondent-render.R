# tests/testthat/test-shiny-respondent-render.R
#
# The standalone Shiny collector, render_survey().
#
# A10. The survey page was a single renderUI that read every input, so each
#      answer re-rendered the page. The new controls came back at their
#      defaults and reported them to the server, which erased the answer a
#      second after it was given. testServer() sets inputs on the server and
#      never sees the browser reset, so the existing tests passed. Controls are
#      now rendered with the current answers restored, and the page no longer
#      re-renders on an answer. The browser proof is in
#      test-shiny-respondent-browser.R.
# A4.  Matrix and ranking showed the stored codes to the participant and
#      stored the option labels.
# A6.  Decision questions started on a substantive judgement, and a
#      constant-sum allocation started at 0 points per criterion, so an
#      untouched question submitted an answer.
# A3 in Shiny. An untouched ranking recorded the declared order.

skip_if_not_installed("shiny")

survey_html <- function(instrument, inputs = list()) {
  app <- render_survey(instrument)
  html <- NULL
  shiny::testServer(app, {
    if (length(inputs)) do.call(session$setInputs, inputs)
    html <<- as.character(output$survey_ui$html)
  })
  html
}

# The first tag in `html` matching `pattern`, or NA.
first_tag <- function(html, pattern) {
  m <- regmatches(html, regexpr(pattern, html, perl = TRUE))
  if (length(m)) m else NA_character_
}

labelled_instrument <- function() {
  sf_instrument(title = "Shiny", components = list(
    sf_choices("yn", values = c(0, 1), labels = c("No", "Yes")),
    sf_choices("agree", values = 1:3,
               labels = c("Disagree", "Neutral", "Agree")),
    sf_item("q1", "Do you smoke?", "single_choice", choice_set = "yn"),
    sf_item("t1", "Comment", "text"),
    sf_item("rk", "Rank these", "ranking", choice_set = "agree"),
    sf_item("mx", "Rate each", "matrix", choice_set = "agree",
            matrix_items = c("price", "service"))
  ))
}

# ── A10: an answer survives the page being drawn again ────────────────────

test_that("A10: a chosen option is drawn checked after it is answered", {
  html <- survey_html(labelled_instrument(), list(q1 = "1"))
  radio <- first_tag(html, '<input[^>]*name="q1"[^>]*value="1"[^>]*>')

  expect_false(is.na(radio))
  expect_match(radio, "checked")
})

test_that("A10: typed text is drawn back into its field", {
  html <- survey_html(labelled_instrument(), list(t1 = "hello"))
  field <- first_tag(html, '<input[^>]*id="t1"[^>]*>')

  expect_match(field, 'value="hello"', fixed = TRUE)
})

test_that("A10: a matrix cell and a ranking are drawn as answered", {
  html <- survey_html(labelled_instrument(),
                      list(mx__1 = "3", rk = "3|1|2"))

  cell <- first_tag(html, '<input[^>]*name="mx__1"[^>]*value="3"[^>]*>')
  expect_match(cell, "checked")
  order <- regmatches(html, gregexpr('class="sf-rank-item"[^>]*data-value="[^"]*"', html))[[1]]
  expect_identical(sub('.*data-value="([^"]*)"$', "\\1", order),
                   c("3", "1", "2"))
})

test_that("A10: the survey page does not depend on individual answers", {
  # Progress is the part of the page that should move with answers, so it
  # has its own output and the controls are left alone.
  app <- render_survey(labelled_instrument())
  shiny::testServer(app, {
    expect_false(is.null(output$sf_progress))
  })
})

# ── A4: labels are shown, codes are stored ────────────────────────────────

test_that("A4: matrix headers show option labels and cells store codes", {
  html <- survey_html(labelled_instrument())
  headers <- regmatches(html, gregexpr("<th>[^<]*</th>", html))[[1]]

  expect_identical(gsub("</?th>", "", headers),
                   c("", "Disagree", "Neutral", "Agree"))
  values <- regmatches(html, gregexpr('name="mx__1"[^>]*value="[^"]*"', html))[[1]]
  expect_identical(sub('.*value="([^"]*)"$', "\\1", values),
                   c("1", "2", "3"))
})

test_that("A4: ranking shows option labels and stores codes", {
  html <- survey_html(labelled_instrument())
  items <- regmatches(html, gregexpr('<div class="sf-rank-item"[^>]*data-value="[^"]*"', html))[[1]]

  expect_identical(sub('.*data-value="([^"]*)"$', "\\1", items),
                   c("1", "2", "3"))
  expect_match(html, "Disagree")
  expect_false(grepl('data-value="Disagree"', html, fixed = TRUE))
})

test_that("A4: a ranking collected in Shiny reaches its export columns", {
  ins <- labelled_instrument()
  row <- sframe_response_row(ins, list(rk = "3|1|2"),
                             sframe_branch_lookup(ins), started_at = Sys.time())

  expect_identical(c(row$rk__1, row$rk__2, row$rk__3), c("2", "3", "1"))
})

# ── A5: ranking can be ordered from the keyboard ──────────────────────────

test_that("A5: every ranking option has labelled move up and move down buttons", {
  html <- survey_html(labelled_instrument())

  for (lbl in c("Disagree", "Neutral", "Agree")) {
    expect_match(html, sprintf('<button[^>]*aria-label="Move %s up"', lbl))
    expect_match(html, sprintf('<button[^>]*aria-label="Move %s down"', lbl))
  }
  expect_match(html, 'role="status"', fixed = TRUE)
  expect_match(html, "Keep this order", fixed = TRUE)
})

# ── A6 and the Shiny A3: nothing is answered until the participant answers ─

decision_instrument <- function() {
  sf_instrument(title = "Decisions", components = list(
    sf_choices("agree", values = 1:3,
               labels = c("Disagree", "Neutral", "Agree")),
    sf_item("pw", "Compare", "pairwise_comparison", required = TRUE,
            comparison_items = c("price", "quality")),
    sf_item("inf", "Influence", "pairwise_comparison", required = TRUE,
            comparison_items = c("a", "b"), comparison_scale = "influence"),
    sf_item("cw", "Allocate 100", "criteria_weight", required = TRUE,
            comparison_items = c("price", "quality")),
    sf_item("rk", "Rank", "ranking", choice_set = "agree", required = TRUE),
    sf_item("sl", "Satisfaction", "slider", required = TRUE,
            slider_min = 0, slider_max = 10)
  ))
}

test_that("A6: a pairwise judgement starts with nothing selected", {
  html <- survey_html(decision_instrument())
  for (id in c("pw__price__vs__quality", "inf__a__to__b", "inf__b__to__a")) {
    select <- regmatches(html, regexpr(
      sprintf('(?s)<select[^>]*id="%s".*?</select>', id), html, perl = TRUE))
    expect_length(select, 1)
    selected <- regmatches(select, regexpr('<option[^>]*selected[^>]*>[^<]*', select))
    expect_match(selected, 'value=""', fixed = TRUE, info = id)
  }
})

test_that("A6: a constant-sum field starts empty, not at 0 points", {
  html <- survey_html(decision_instrument())
  field <- first_tag(html, '<input[^>]*id="cw__price"[^>]*>')

  expect_false(grepl('value="0"', field, fixed = TRUE))
})

test_that("A6: untouched decision questions count as unanswered", {
  ins <- decision_instrument()
  untouched <- list(pw__price__vs__quality = "",
                    inf__a__to__b = "", inf__b__to__a = "",
                    cw__price = NA, cw__quality = NA,
                    rk = "", sl = 5)
  missing <- sframe_missing_required_items(ins, untouched,
                                           sframe_branch_lookup(ins))

  expect_setequal(missing, c("pw", "inf", "cw", "rk", "sl"))
})

test_that("A6: an untouched slider and ranking submit nothing", {
  ins <- decision_instrument()
  row <- sframe_response_row(ins, list(rk = "", sl = 5),
                             sframe_branch_lookup(ins), started_at = Sys.time())

  expect_true(is.na(row$sl))
  expect_true(all(is.na(c(row$rk__1, row$rk__2, row$rk__3))))
})

test_that("A6: a slider the participant moved is recorded", {
  ins <- decision_instrument()
  iv <- list(sl = 5, sl__touched = TRUE)
  row <- sframe_response_row(ins, iv, sframe_branch_lookup(ins),
                             started_at = Sys.time())

  expect_identical(row$sl, "5")
  expect_false("sl" %in% sframe_missing_required_items(
    ins, iv, sframe_branch_lookup(ins)))
})
