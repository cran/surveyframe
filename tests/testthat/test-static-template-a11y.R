# Batch 7 #10 and #11: what a participant using a screen reader or a keyboard
# is told, in the exported static survey.
#
# #10. Single-choice and multiple-choice groups carried option labels with no
#      programmatic group name, so an option was announced without its
#      question. Rating buttons announced "3 of 5" with no question name and no
#      selected state, so a participant could not hear what was chosen.
# #11. Validation marked the item and scrolled to it, and left focus where it
#      was, so a keyboard user was told about an error somewhere off-screen.
#      A page change replaced the content with no focus move either.

a11y_instrument <- function(required = TRUE) {
  sf_instrument("Access", components = list(
    sf_choices("ag3", values = 1:3, labels = c("Low", "Mid", "High")),
    sf_item("pick", "Pick one", type = "single_choice", choice_set = "ag3",
            required = required),
    sf_item("many", "Pick any", type = "multiple_choice", choice_set = "ag3",
            required = required),
    sf_item("rate", "Rate us", type = "rating", rating_max = 5,
            required = required)
  ))
}

test_that("10: a choice group carries its question as its name", {
  skip_if_not_installed("V8")
  ctx <- static_survey_context(a11y_instrument())

  single <- static_render_item(ctx, "pick")
  expect_match(single, 'role="radiogroup"', fixed = TRUE)
  expect_match(single, 'aria-labelledby="q_pick"', fixed = TRUE)

  multi <- static_render_item(ctx, "many")
  expect_match(multi, 'role="group"', fixed = TRUE)
  expect_match(multi, 'aria-labelledby="q_many"', fixed = TRUE)
})

test_that("10: a rating names its question and says what is chosen", {
  skip_if_not_installed("V8")
  ctx <- static_survey_context(a11y_instrument())
  ctx$eval("responses['rate'] = '3';")
  html <- static_render_item(ctx, "rate")

  expect_match(html, 'role="radiogroup"', fixed = TRUE)
  expect_match(html, 'aria-labelledby="q_rate"', fixed = TRUE)
  # each button is a radio that reports its own state
  expect_equal(length(gregexpr('role="radio"', html, fixed = TRUE)[[1]]), 5)
  expect_equal(length(gregexpr('aria-checked="true"', html, fixed = TRUE)[[1]]), 1)
  # and the chosen one is the third
  chosen <- regmatches(html, gregexpr('data-v="[0-9]+"[^>]*aria-checked="true"',
                                      html))[[1]]
  expect_match(chosen, 'data-v="3"', fixed = TRUE)
})

test_that("10: setting a rating moves the reported state", {
  skip_if_not_installed("V8")
  ctx <- static_survey_context(a11y_instrument())
  key <- "rate"
  ctx$eval(sprintf(
    "__registerSelector('.star-btn[data-id=\"%s\"]',
       [1,2,3,4,5].map(function(v){ var e = __mkControl(v); e.dataset.v = String(v); return e; }));",
    key))
  ctx$eval("setRating('rate', 4, 5);")
  states <- ctx$get("__selectorEls['.star-btn[data-id=\"rate\"]'].map(function(e){ return String(e.ariaChecked); })")
  expect_equal(states, c("false", "false", "false", "true", "false"))
})

test_that("11: validation focuses the control it is complaining about", {
  skip_if_not_installed("V8")
  ctx <- static_survey_context(a11y_instrument())
  ctx$eval("screen='survey'; renderScreen();")
  # nothing answered, so the first required item is the one to go to
  expect_equal(ctx$get("String(validatePage(SF.items))"), "false")
  expect_equal(ctx$get("__lastFocused"), "item_pick")
})

test_that("11: the error is tied to the control that has it", {
  skip_if_not_installed("V8")
  ctx <- static_survey_context(a11y_instrument())
  html <- static_render_item(ctx, "pick")
  # the group points at its own error container, so the message is announced
  # with the control instead of floating loose
  expect_match(html, 'aria-describedby="err_pick"', fixed = TRUE)
  expect_match(html, 'id="err_pick"', fixed = TRUE)
})

test_that("11: changing page moves focus to the new heading", {
  skip_if_not_installed("V8")
  paged <- sf_instrument("Paged", components = list(
    sf_item("a", "First", type = "text", page = 1),
    sf_item("b", "Second", type = "text", page = 2)
  ))
  ctx <- static_survey_context(paged)
  ctx$eval("screen='survey'; currentPage=1; renderScreen(); __lastFocused=null; nextPage();")
  expect_equal(ctx$get("currentPage"), 2)
  expect_equal(ctx$get("__lastFocused"), "page-heading")
})

# Batch 7 #15: the Likert strip scrolls horizontally with a minimum width per
# option. At the mobile minimum of 50px, 7 options need 350px and the builder's
# 11-point NPS preset needs 550px before gaps, so the full range cannot fit a
# 390px viewport and a participant saw part of the scale with both endpoints off
# screen. Below 600px each option becomes a labelled row, as the matrix already
# does. This is a CSS constraint, so it is asserted on the stylesheet the export
# ships and exercised again by the headless-browser test.

exported_html <- function(instrument) {
  path <- tempfile(fileext = ".html")
  suppressMessages(export_static_survey(instrument, output_path = path,
                                        open = FALSE))
  on.exit(unlink(path), add = TRUE)
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

mobile_block <- function(html) {
  at <- regmatches(html, regexpr("@media[^{]*600px[^{]*\\{(?s).*?\\n\\}", html,
                                 perl = TRUE))
  if (!length(at)) return("")
  at[[1]]
}

test_that("15: the Likert scale stacks below 600px", {
  nps <- sf_instrument("NPS", components = list(
    sf_choices("nps", values = 0:10, labels = as.character(0:10)),
    sf_item("rec", "How likely are you to recommend us?", type = "likert",
            choice_set = "nps")
  ))
  block <- mobile_block(exported_html(nps))
  expect_true(nzchar(block))
  # the row stacks, and an option fills the width instead of holding a minimum
  expect_match(block, ".likert-row{flex-direction:column", fixed = TRUE)
  expect_match(block, "overflow-x:visible", fixed = TRUE)
  expect_match(block, "min-width:0", fixed = TRUE)
  # and each option is a tap target of at least 44px
  expect_match(block, "min-height:44px", fixed = TRUE)
})

test_that("15: all 11 NPS points are rendered, so none is lost", {
  skip_if_not_installed("V8")
  nps <- sf_instrument("NPS", components = list(
    sf_choices("nps", values = 0:10, labels = as.character(0:10)),
    sf_item("rec", "How likely are you to recommend us?", type = "likert",
            choice_set = "nps")
  ))
  ctx <- static_survey_context(nps)
  html <- static_render_item(ctx, "rec")
  expect_equal(length(gregexpr("likert-opt", html, fixed = TRUE)[[1]]), 11)
  # 0 is a real answer here, and survives escaping (A2's defect class)
  expect_match(html, 'value="0"', fixed = TRUE)
})
