# tests/testthat/test-id-collision.R
#
# B1. A scale's composite is stored in a column named by the scale's id. An
# item and a scale could share an id, because duplicates were checked within
# items and within scales and never across them, so score_scales() replaced
# the item's answers with the composite. keep_items then returned the
# composite under the item's name, and an analysis plan asking for the item
# read the composite. A scale could equally overwrite an expansion column
# (item__option) or a response metadata column such as submitted_at.

agree <- function() sf_choices("ag5", 1:5, c("SD", "D", "N", "A", "SA"))

collide_item_scale <- function() {
  sf_instrument(title = "Collision", components = list(
    agree(),
    sf_item("q1", "Question one", "likert", choice_set = "ag5", scale_id = "q1"),
    sf_item("q2", "Question two", "likert", choice_set = "ag5", scale_id = "q1"),
    sf_scale("q1", "A scale sharing an item's id", items = c("q1", "q2"))
  ))
}

test_that("B1: validation rejects a scale sharing an item's id", {
  v <- validate_sframe(collide_item_scale(), strict = FALSE)
  expect_false(v$valid)
  expect_true("id_namespace" %in% v$checks$check)
  expect_match(paste(v$problems, collapse = " "), "q1")
  expect_false(v$checks$status[v$checks$check == "id_namespace"] == "ok")
})

test_that("B1: strict validation aborts on the collision", {
  expect_error(validate_sframe(collide_item_scale(), strict = TRUE),
               class = "sframe_error")
})

test_that("B1: score_scales() refuses to overwrite an item's answers", {
  data <- data.frame(q1 = c(1, 5), q2 = c(3, 3))
  expect_error(score_scales(data, collide_item_scale()), class = "sframe_error")
})

test_that("B1: a scale sharing an expansion column's name is rejected", {
  ins <- sf_instrument(title = "Expansion", components = list(
    agree(),
    sf_choices("fruit", c("a", "b"), c("Apple", "Banana")),
    sf_item("mc", "Which fruit?", "multiple_choice", choice_set = "fruit"),
    sf_item("s1", "One", "likert", choice_set = "ag5", scale_id = "mc__a"),
    sf_scale("mc__a", "Named like a column", items = "s1")
  ))
  v <- validate_sframe(ins, strict = FALSE)
  expect_false(v$valid)
  expect_match(paste(v$problems, collapse = " "), "mc__a")
})

test_that("B1: an item or scale named like response metadata is rejected", {
  ins <- sf_instrument(title = "Metadata", components = list(
    agree(),
    sf_item("submitted_at", "When?", "text")
  ))
  v <- validate_sframe(ins, strict = FALSE)
  expect_false(v$valid)
  expect_match(paste(v$problems, collapse = " "), "submitted_at")
})

test_that("B1: every original column survives scoring a valid instrument", {
  ins <- sf_instrument(title = "Valid", components = list(
    agree(),
    sf_item("q1", "One", "likert", choice_set = "ag5", scale_id = "sat"),
    sf_item("q2", "Two", "likert", choice_set = "ag5", scale_id = "sat"),
    sf_scale("sat", "Satisfaction", items = c("q1", "q2"))
  ))
  data <- data.frame(respondent_id = c("r1", "r2"), q1 = c(1, 5), q2 = c(3, 3))
  scored <- score_scales(data, ins)

  expect_identical(scored$q1, data$q1)
  expect_identical(scored$q2, data$q2)
  expect_identical(scored$respondent_id, data$respondent_id)
  expect_equal(scored$sat, c(2, 4))
})
