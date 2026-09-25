# Batch 7 #14: a pairwise comparison renders both a radio strip and a select
# of the same anchors, and the CSS shows one or the other by viewport width.
# Each wrote the response and left its counterpart alone, so crossing the
# breakpoint after answering revealed the other control still showing its
# render-time state. The stored judgement was right and the screen disagreed
# with it.

pairwise_instrument <- function(scale = "saaty") {
  sf_instrument("Pairwise", components = list(
    sf_item("pw", "Compare the criteria", type = "pairwise_comparison",
            comparison_items = c("cost", "speed"), comparison_scale = scale)
  ))
}

# Registers a radio strip and a select against the selectors the template
# uses, so a sync can be driven and read back.
register_controls <- function(ctx, row_key, values) {
  ctx$eval(sprintf(
    "__registerSelector('input[type=\"radio\"][name=\"%s\"]',
       %s.map(function(v){ return __mkControl(v); }));
     __registerSelector('select.pw-select[data-key=\"%s\"]',
       [__mkControl('')]);",
    row_key, jsonlite::toJSON(as.character(values)), row_key))
}

checked_value <- function(ctx, row_key) {
  ctx$get(sprintf(
    "(function(){var r=__selectorEls['input[type=\"radio\"][name=\"%s\"]']
       .filter(function(e){return e.checked;});
       return r.length ? r[0].value : '';})()", row_key))
}

select_value <- function(ctx, row_key) {
  ctx$get(sprintf("__selectorEls['select.pw-select[data-key=\"%s\"]'][0].value",
                  row_key))
}

test_that("14: both controls carry a data key naming their row", {
  skip_if_not_installed("V8")
  ctx <- static_survey_context(pairwise_instrument())
  html <- static_render_item(ctx, "pw")
  # the select can be found by the row it answers, which is what a sync needs
  expect_match(html, 'data-key="pw__cost__vs__speed"', fixed = TRUE)
  # and both controls route through the pairwise handler
  expect_equal(length(gregexpr("setPairwise(", html, fixed = TRUE)[[1]]), 18)
})

test_that("14: answering through one control moves the other", {
  skip_if_not_installed("V8")
  ctx <- static_survey_context(pairwise_instrument())
  key <- "pw__cost__vs__speed"
  register_controls(ctx, key, c(9, 3, 1, -3, -9))

  ctx$eval(sprintf("setPairwise('%s', 3);", key))
  expect_equal(ctx$get(sprintf("String(responses['%s'])", key)), "3")
  expect_equal(checked_value(ctx, key), "3")
  expect_equal(select_value(ctx, key), "3")

  # answering again moves both, leaving no stale selection behind
  ctx$eval(sprintf("setPairwise('%s', -9);", key))
  expect_equal(checked_value(ctx, key), "-9")
  expect_equal(select_value(ctx, key), "-9")
})

test_that("14: an influence scale syncs the same way", {
  skip_if_not_installed("V8")
  ctx <- static_survey_context(pairwise_instrument(scale = "influence"))
  key <- "pw__cost__to__speed"
  register_controls(ctx, key, 0:4)

  ctx$eval(sprintf("setPairwise('%s', 0);", key))
  # 0 is a real judgement here, and has to register as one
  expect_equal(ctx$get(sprintf("String(responses['%s'])", key)), "0")
  expect_equal(checked_value(ctx, key), "0")
  expect_equal(select_value(ctx, key), "0")
})

test_that("14: a sync with no controls on screen is harmless", {
  skip_if_not_installed("V8")
  ctx <- static_survey_context(pairwise_instrument())
  expect_no_error(ctx$eval("setPairwise('pw__cost__vs__speed', 1);"))
  expect_equal(ctx$get("String(responses['pw__cost__vs__speed'])"), "1")
})
