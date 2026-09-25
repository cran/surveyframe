# tests/testthat/test-scale-parameters.R
#
# Scale declarations and the values scoring reads.
#
# Batch 1, finding 8. sf_scale() accepted non-numeric, infinite and negative
#   weights, repeated item ids, and a min_valid that was negative, fractional
#   or larger than the scale, such as min_valid = 3 on 2 items, a threshold no
#   respondent can reach.
# Batch 1, finding 9. A constructed item passed inside a scale's items was
#   accepted and silently dropped from the instrument's items.
# Batch 2, finding 5. min_valid = NULL means every declared item, but scoring
#   counted only the columns present in the data, so a 3-item scale with 1
#   absent column was scored on 2 items with no warning.
# Batch 2, finding 6. as.numeric() on a factor returns level positions, so a
#   factor holding 10 and 20 was scored as 1 and 2.

agree <- function() sf_choices("ag5", 1:5, c("SD", "D", "N", "A", "SA"))

test_that("8: sf_scale() rejects weights that are not positive finite numbers", {
  expect_error(sf_scale("s", "S", items = c("a", "b"), weights = c("1", "2")),
               class = "sframe_error")
  expect_error(sf_scale("s", "S", items = c("a", "b"), weights = c(1, Inf)),
               class = "sframe_error")
  expect_error(sf_scale("s", "S", items = c("a", "b"), weights = c(1, -1)),
               class = "sframe_error")
  expect_error(sf_scale("s", "S", items = c("a", "b"), weights = c(1, 0)),
               class = "sframe_error")
})

test_that("8: sf_scale() rejects repeated items and an unreachable min_valid", {
  expect_error(sf_scale("s", "S", items = c("a", "a")), class = "sframe_error")
  expect_error(sf_scale("s", "S", items = c("a", "b"), min_valid = 3),
               class = "sframe_error")
  expect_error(sf_scale("s", "S", items = c("a", "b"), min_valid = 0),
               class = "sframe_error")
  expect_error(sf_scale("s", "S", items = c("a", "b"), min_valid = 1.5),
               class = "sframe_error")
  expect_s3_class(sf_scale("s", "S", items = c("a", "b"), min_valid = 1L),
                  "sf_scale")
})

test_that("8: validation reports the same problems in a scale that skipped the constructor", {
  ins <- sf_instrument(title = "Params", components = list(
    agree(),
    sf_item("a", "A", "likert", choice_set = "ag5", scale_id = "s"),
    sf_item("b", "B", "likert", choice_set = "ag5", scale_id = "s"),
    sf_scale("s", "S", items = c("a", "b"))
  ))
  ins$scales[[1]]$min_valid <- 5
  ins$scales[[1]]$weights <- c(1, -2)
  v <- validate_sframe(ins, strict = FALSE)
  expect_false(v$valid)
  expect_true("scale_parameters" %in% v$checks$check)
  expect_match(paste(v$problems, collapse = " "), "min_valid")
  expect_match(paste(v$problems, collapse = " "), "weights")
})

test_that("9: a constructed item inside a scale's items is rejected", {
  item <- sf_item("a", "A", "likert", choice_set = "ag5")
  expect_error(sf_scale("s", "S", items = list(item)), class = "sframe_error")
})

three_items <- function(min_valid = NULL) {
  sf_instrument(title = "Absent", components = list(
    agree(),
    sf_item("a", "A", "likert", choice_set = "ag5", scale_id = "s"),
    sf_item("b", "B", "likert", choice_set = "ag5", scale_id = "s"),
    sf_item("c", "C", "likert", choice_set = "ag5", scale_id = "s"),
    sf_scale("s", "S", items = c("a", "b", "c"), min_valid = min_valid)
  ))
}

test_that("5: an absent column is warned about and counts toward the threshold", {
  d <- data.frame(a = c(4, 2), b = c(4, 2))
  expect_warning(scored <- score_scales(d, three_items()), "c")
  # min_valid = NULL requires all 3 declared items, and 1 is absent
  expect_true(all(is.na(scored$s)))
})

test_that("5: an explicit min_valid still scores when enough items are present", {
  d <- data.frame(a = c(4, 2), b = c(4, 2))
  expect_warning(scored <- score_scales(d, three_items(min_valid = 2L)), "c")
  expect_equal(scored$s, c(4, 2))
})

test_that("6: a factor of numeric labels is scored on its labels", {
  ins <- sf_instrument(title = "Factor", components = list(
    sf_item("x", "X", "numeric", scale_id = "s"),
    sf_item("y", "Y", "numeric", scale_id = "s"),
    sf_scale("s", "S", items = c("x", "y"), method = "sum")
  ))
  d <- data.frame(x = factor(c("10", "20")), y = c(1, 2))
  expect_equal(score_scales(d, ins)$s, c(11, 22))
})

test_that("6: a factor of text labels is an error, never level positions", {
  ins <- sf_instrument(title = "Nominal", components = list(
    sf_item("x", "X", "text", scale_id = "s"),
    sf_item("y", "Y", "numeric", scale_id = "s"),
    sf_scale("s", "S", items = c("x", "y"))
  ))
  d <- data.frame(x = factor(c("low", "high")), y = c(1, 2))
  expect_error(score_scales(d, ins), class = "sframe_error")
})
