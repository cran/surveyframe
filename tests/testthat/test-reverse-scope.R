# tests/testthat/test-reverse-scope.R
#
# B2. Reverse coding was one instrument-wide map. A scale that listed an item
# in reverse_items reversed that item everywhere, including in a scale that
# owned it and never asked for reversal, and the same map reached
# reliability_report(), so alpha moved too. reverse_items was documented as a
# subset of items and never checked. Reversal is now scoped to the scale that
# declares it: a scale's reverse_items, and items whose own scale_id names
# that scale with reverse = TRUE.
#
# Batch 2, finding 7. With no declared numeric bounds, reversal used the
# observed minimum and maximum, so a response's reversed value changed as
# respondents were added. Reversal now needs declared bounds.

agree <- function() sf_choices("ag5", 1:5, c("SD", "D", "N", "A", "SA"))

two_scales <- function() {
  sf_instrument(title = "Two scales", components = list(
    agree(),
    sf_item("a1", "A one", "likert", choice_set = "ag5", scale_id = "a"),
    sf_item("a2", "A two", "likert", choice_set = "ag5", scale_id = "a"),
    sf_item("b1", "B one", "likert", choice_set = "ag5", scale_id = "b"),
    sf_item("b2", "B two", "likert", choice_set = "ag5", scale_id = "b"),
    sf_scale("a", "Scale A", items = c("a1", "a2", "b1"), reverse_items = "b1"),
    sf_scale("b", "Scale B", items = c("b1", "b2"))
  ))
}

responses <- function() {
  data.frame(a1 = c(4, 5, 2, 3), a2 = c(4, 4, 2, 3),
             b1 = c(5, 4, 1, 2), b2 = c(5, 5, 2, 2))
}

test_that("B2: scale A's reverse declaration leaves scale B's score alone", {
  scored <- score_scales(responses(), two_scales())
  # scale B owns b1 and never asked for reversal
  expect_equal(scored$b, rowMeans(responses()[, c("b1", "b2")]))
  # scale A reverses b1 on the 1 to 5 range
  expect_equal(scored$a, rowMeans(cbind(responses()$a1, responses()$a2,
                                        6 - responses()$b1)))
})

test_that("B2: scale A's reverse declaration leaves scale B's alpha alone", {
  skip_if_not_installed("psych")
  rr <- reliability_report(responses(), two_scales(), scales = "b")
  expected <- suppressWarnings(psych::alpha(responses()[, c("b1", "b2")],
                                            check.keys = FALSE,
                                            warnings = FALSE))$total$raw_alpha
  got <- as.data.frame(rr)
  expect_equal(got$alpha[got$scale_id == "b"], expected)
})

test_that("B2: an item-level reverse applies within the item's own scale only", {
  ins <- sf_instrument(title = "Item level", components = list(
    agree(),
    sf_item("x1", "X one", "likert", choice_set = "ag5", scale_id = "x",
            reverse = TRUE),
    sf_item("x2", "X two", "likert", choice_set = "ag5", scale_id = "x"),
    sf_scale("x", "Scale X", items = c("x1", "x2")),
    sf_scale("y", "Scale Y", items = c("x1", "x2"))
  ))
  d <- data.frame(x1 = c(1, 5), x2 = c(2, 4))
  scored <- score_scales(d, ins)
  expect_equal(scored$x, c((5 + 2) / 2, (1 + 4) / 2))
  expect_equal(scored$y, c((1 + 2) / 2, (5 + 4) / 2))
})

test_that("B2: sf_scale() rejects reverse_items outside its items", {
  expect_error(
    sf_scale("a", "A", items = c("a1", "a2"), reverse_items = "b1"),
    class = "sframe_error"
  )
})

test_that("B2: validation rejects reverse_items outside the scale's items", {
  ins <- two_scales()
  # a scale arriving from a file or a hand-built list skips the constructor
  ins$scales[[2]]$reverse_items <- "a1"
  v <- validate_sframe(ins, strict = FALSE)
  expect_false(v$valid)
  expect_true("reverse_item_membership" %in% v$checks$check)
  expect_match(paste(v$problems, collapse = " "), "a1")
})

test_that("B2: validation rejects an item reversed within a scale that omits it", {
  ins <- sf_instrument(title = "Omitted", components = list(
    agree(),
    sf_item("x1", "X one", "likert", choice_set = "ag5", scale_id = "x",
            reverse = TRUE),
    sf_item("x2", "X two", "likert", choice_set = "ag5", scale_id = "x"),
    sf_scale("x", "Scale X", items = "x2")
  ))
  v <- validate_sframe(ins, strict = FALSE)
  expect_false(v$valid)
  expect_match(paste(v$problems, collapse = " "), "x1")
})

test_that("finding 7: reversal uses declared bounds, never the sample's range", {
  ins <- sf_instrument(title = "Bounds", components = list(
    agree(),
    sf_item("r1", "Reversed", "likert", choice_set = "ag5", scale_id = "s"),
    sf_item("r2", "Plain", "likert", choice_set = "ag5", scale_id = "s"),
    sf_scale("s", "S", items = c("r1", "r2"), reverse_items = "r1")
  ))
  # only 2 and 4 observed, on a declared 1 to 5 scale
  few <- data.frame(r1 = c(2, 4), r2 = c(3, 3))
  more <- data.frame(r1 = c(2, 4, 1, 5), r2 = c(3, 3, 3, 3))
  expect_equal(score_scales(few, ins)$s[1:2], score_scales(more, ins)$s[1:2])
  expect_equal(score_scales(few, ins)$s, c((4 + 3) / 2, (2 + 3) / 2))
})

test_that("finding 7: a slider item reverses on its declared slider bounds", {
  ins <- sf_instrument(title = "Slider", components = list(
    sf_item("s1", "Slider", "slider", slider_min = 0, slider_max = 10,
            scale_id = "s", reverse = TRUE),
    sf_item("s2", "Other", "slider", slider_min = 0, slider_max = 10,
            scale_id = "s"),
    sf_scale("s", "S", items = c("s1", "s2"))
  ))
  d <- data.frame(s1 = c(3, 4), s2 = c(5, 5))
  expect_equal(score_scales(d, ins)$s, c((7 + 5) / 2, (6 + 5) / 2))
})

test_that("finding 7: reversing an item with no declared bounds is an error", {
  ins <- sf_instrument(title = "No bounds", components = list(
    sf_item("n1", "Number", "numeric", scale_id = "s", reverse = TRUE),
    sf_item("n2", "Number", "numeric", scale_id = "s"),
    sf_scale("s", "S", items = c("n1", "n2"))
  ))
  d <- data.frame(n1 = c(3, 9), n2 = c(5, 5))
  expect_error(score_scales(d, ins), class = "sframe_error")
})
