# tests/testthat/test-static-template-js.R
#
# Runs the exported static survey's JavaScript under V8 (helper-v8-dom.R) and
# checks what a participant's action actually stores. Three collection defects
# lived here because the template was only ever tested by matching its source.
#
# A1. Numeric and constant-sum inputs rewrote what the participant typed. A
#     cleared field with a minimum of 18 became 18, and 2.5 points became 25.
# A2. esc() collapsed the number 0 to an empty string, so an option coded 0
#     rendered as value="" and was stored and validated as unanswered.
# A3. A ranking kept 2 states. Dragging updated the submitted order and left
#     the displayed one behind, and an untouched ranking was submitted as if
#     the participant had chosen the declared order.

# Values and handler for the n-th <input> in rendered HTML.
nth_input <- function(html, n = 1) {
  inputs <- regmatches(html, gregexpr("<input[^>]*>", html))[[1]]
  tag <- inputs[[n]]
  value <- regmatches(tag, regexec('value="([^"]*)"', tag))[[1]][2]
  list(tag = tag, value = value)
}

# ── A1: numeric entry is never rewritten ──────────────────────────────────

numeric_instrument <- function(required = FALSE) {
  sf_instrument(title = "A1", components = list(
    sf_item("age", "Age in years", "numeric", required = required,
            slider_min = 18, slider_max = 99)
  ))
}

test_that("A1: clearing a bounded numeric field leaves it empty", {
  ctx <- static_survey_context(numeric_instrument())
  html <- static_render_item(ctx, "age")
  inp <- static_fire_handler(ctx, html, "onchange",
                             list(value = "", min = "18", max = "99"))

  expect_identical(inp$value, "")
  expect_identical(static_responses(ctx)$age, "")
})

test_that("A1: an out-of-range number is kept as typed and flagged", {
  ctx <- static_survey_context(numeric_instrument())
  html <- static_render_item(ctx, "age")
  inp <- static_fire_handler(ctx, html, "onchange",
                             list(value = "150", min = "18", max = "99"))

  expect_identical(inp$value, "150")
  expect_identical(static_responses(ctx)$age, "150")
  expect_false(static_validate(ctx, "age"))
  msg <- static_error_text(ctx, "age")
  expect_match(msg, "18")
  expect_match(msg, "99")
})

test_that("A1: a number that is not a number is flagged, never replaced", {
  ctx <- static_survey_context(numeric_instrument())
  html <- static_render_item(ctx, "age")
  inp <- static_fire_handler(ctx, html, "onchange",
                             list(value = "abc", min = "18", max = "99"))

  expect_identical(inp$value, "abc")
  expect_false(static_validate(ctx, "age"))
})

test_that("A1: an in-range number passes", {
  ctx <- static_survey_context(numeric_instrument(required = TRUE))
  html <- static_render_item(ctx, "age")
  static_fire_handler(ctx, html, "onchange",
                      list(value = "42", min = "18", max = "99"))

  expect_true(static_validate(ctx, "age"))
  expect_identical(static_responses(ctx)$age, "42")
})

weight_instrument <- function() {
  sf_instrument(title = "A1cw", components = list(
    sf_item("w", "Allocate 100 points", "criteria_weight", required = TRUE,
            comparison_items = c("price", "quality"))
  ))
}

test_that("A1: a constant-sum entry of 2.5 is not turned into 25", {
  ctx <- static_survey_context(weight_instrument())
  html <- static_render_item(ctx, "w")
  inp <- static_fire_handler(ctx, html, "oninput", list(value = "2.5"))

  expect_identical(inp$value, "2.5")
  expect_identical(static_responses(ctx)[["w__price"]], "2.5")
  expect_false(static_validate(ctx, "w"))
})

test_that("A1: a constant-sum entry is not rewritten against its siblings", {
  ctx <- static_survey_context(weight_instrument())
  html <- static_render_item(ctx, "w")
  # the quality field already holds 50 points in the page
  ctx$eval("document.getElementById('cw_w').querySelectorAll = function(){
    return [__inp, { value: '50' }]; };")
  ctx$eval("responses['w__quality'] = '50';")

  inp <- static_fire_handler(ctx, html, "oninput", list(value = "70"))

  expect_identical(inp$value, "70")
  expect_identical(static_responses(ctx)[["w__price"]], "70")
  # 70 + 50 is not 100, so the page says so and does not advance
  expect_false(static_validate(ctx, "w"))
  expect_match(static_error_text(ctx, "w"), "100")
})

# ── A2: an option coded zero is a real answer ─────────────────────────────

zero_instrument <- function() {
  sf_instrument(title = "A2", components = list(
    sf_choices("yn", values = c(0, 1), labels = c("No", "Yes")),
    sf_choices("nps", values = 0:10, labels = as.character(0:10)),
    sf_item("smoker", "Do you smoke?", "single_choice", choice_set = "yn",
            required = TRUE),
    sf_item("rec", "How likely are you to recommend us?", "likert",
            choice_set = "nps", required = TRUE),
    sf_item("grid", "Rate each", "matrix", choice_set = "yn",
            matrix_items = c("a", "b"), required = TRUE)
  ))
}

test_that("A2: an option coded 0 renders with the value 0", {
  ctx <- static_survey_context(zero_instrument())

  expect_identical(nth_input(static_render_item(ctx, "smoker"), 1)$value, "0")
  expect_identical(nth_input(static_render_item(ctx, "rec"), 1)$value, "0")
  expect_identical(nth_input(static_render_item(ctx, "grid"), 1)$value, "0")
  # the Likert numeral shown to the participant is 0 too
  expect_match(static_render_item(ctx, "rec"),
               '<span class="likert-num">0</span>', fixed = TRUE)
})

test_that("A2: choosing the option coded 0 satisfies a required question", {
  ctx <- static_survey_context(zero_instrument())
  html <- static_render_item(ctx, "smoker")
  chosen <- nth_input(html, 1)
  static_fire_handler(ctx, chosen$tag, "onchange", list(value = chosen$value))

  expect_identical(static_responses(ctx)$smoker, "0")
  expect_true(static_validate(ctx, "smoker"))
})

test_that("A2: every NPS option carries a distinct value", {
  ctx <- static_survey_context(zero_instrument())
  html <- static_render_item(ctx, "rec")
  inputs <- regmatches(html, gregexpr("<input[^>]*>", html))[[1]]
  values <- vapply(inputs, function(t)
    regmatches(t, regexec('value="([^"]*)"', t))[[1]][2], character(1))

  expect_identical(unname(values), as.character(0:10))
})

test_that("A2: a zero answer is submitted as 0", {
  ctx <- static_survey_context(zero_instrument())
  ctx$eval("setResp('smoker','0'); setResp('rec','0');
            setResp('grid__a','0'); setResp('grid__b','1');")
  row <- static_submit_row(ctx)

  expect_identical(row$smoker, "0")
  expect_identical(row$rec, "0")
  expect_identical(row$grid__a, "0")
})

# ── A3: one ranking state, and no invented preferences ────────────────────

ranking_instrument <- function(required = FALSE) {
  sf_instrument(title = "A3", components = list(
    sf_choices("opts", values = c("a", "b", "c"),
               labels = c("Alpha", "Beta", "Gamma")),
    sf_item("rk", "Rank these", "ranking", choice_set = "opts",
            required = required)
  ))
}

rendered_rank_order <- function(ctx) {
  html <- static_render_item(ctx, "rk")
  m <- regmatches(html, gregexpr('class="rank-item"[^>]*data-v="([^"]*)"', html))[[1]]
  sub('.*data-v="([^"]*)"$', "\\1", m)
}

test_that("A3: an untouched optional ranking submits no preference", {
  ctx <- static_survey_context(ranking_instrument())
  static_render_item(ctx, "rk")
  row <- static_submit_row(ctx)

  expect_identical(row$rk__a, "")
  expect_identical(row$rk__b, "")
  expect_identical(row$rk__c, "")
})

test_that("A3: after a drag, the displayed order is the order that is stored", {
  ctx <- static_survey_context(ranking_instrument())
  static_render_item(ctx, "rk")

  # A list of 3 rank-item elements in declared order, supporting the DOM
  # operations rankDrop() uses. Drag "c" onto "a".
  ctx$eval("
    var __kids = ['a','b','c'].map(function(v){
      var el = __mkEl('ri_' + v); el.dataset.v = v;
      el.querySelector = function(){ return { textContent: '' }; };
      return el; });
    var __list = __mkEl('rank_rk');
    __list.querySelectorAll = function(){ return __kids.slice(); };
    __list.insertBefore = function(node, ref){
      __kids.splice(__kids.indexOf(node), 1);
      var at = ref ? __kids.indexOf(ref) : __kids.length;
      __kids.splice(at, 0, node); };
    __els['rank_rk'] = __list;
    dragSrc = __kids[2];
    rankDrop({ preventDefault:function(){}, currentTarget: __kids[0] }, 'rk');
  ")

  stored <- strsplit(static_responses(ctx)$rk, ",", fixed = TRUE)[[1]]
  expect_identical(stored, c("c", "a", "b"))
  expect_identical(rendered_rank_order(ctx), stored)
})

test_that("A3: a required ranking can be confirmed without dragging", {
  ctx <- static_survey_context(ranking_instrument(required = TRUE))
  html <- static_render_item(ctx, "rk")

  # untouched, it is unanswered
  expect_false(static_validate(ctx, "rk"))

  # the participant is offered an explicit action for the order shown
  expect_match(html, "rankConfirm(", fixed = TRUE)
  ctx$eval("rankConfirm('rk');")

  expect_identical(static_responses(ctx)$rk, "a,b,c")
  expect_true(static_validate(ctx, "rk"))
  row <- static_submit_row(ctx)
  expect_identical(c(row$rk__a, row$rk__b, row$rk__c), c("1", "2", "3"))
})

test_that("A3: a keyboard move records the order and keeps it on display", {
  ctx <- static_survey_context(ranking_instrument())
  static_render_item(ctx, "rk")
  ctx$eval("rankMove('rk','c',-1);")

  stored <- strsplit(static_responses(ctx)$rk, ",", fixed = TRUE)[[1]]
  expect_identical(stored, c("a", "c", "b"))
  expect_identical(rendered_rank_order(ctx), stored)
})
