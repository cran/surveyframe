# tests/testthat/test-decision-item-types.R
# pairwise_comparison / criteria_weight: construction validation and sframe
# round-trip.

decision_item_instrument <- function() {
  sf_instrument("Decision item types check",
    components = list(
      sf_item("crit_pairs", "Compare criteria", type = "pairwise_comparison",
              comparison_items = c("service", "location", "price"),
              comparison_scale = "saaty", required = TRUE),
      sf_item("crit_influence", "Influence between criteria",
              type = "pairwise_comparison",
              comparison_items = c("service", "location", "price"),
              comparison_scale = "influence"),
      sf_item("crit_points", "Divide 100 points", type = "criteria_weight",
              comparison_items = c("service", "location", "price"),
              required = TRUE)
    ))
}

test_that("sf_item validates pairwise_comparison and criteria_weight", {
  it <- sf_item("p1", "Compare", type = "pairwise_comparison",
                comparison_items = c("a", "b", "c"), comparison_scale = "saaty")
  expect_identical(it$comparison_items, c("a", "b", "c"))
  expect_identical(it$comparison_scale, "saaty")

  expect_error(
    sf_item("p2", "Compare", type = "pairwise_comparison",
            comparison_items = "a", comparison_scale = "saaty"),
    class = "sframe_validation_error"
  )

  cw <- sf_item("w1", "Weights", type = "criteria_weight",
                comparison_items = c("a", "b"))
  expect_identical(cw$comparison_items, c("a", "b"))
})

test_that("pairwise_comparison and criteria_weight items survive a write/read round-trip", {
  instr <- decision_item_instrument()
  tmp <- tempfile(fileext = ".sframe")
  on.exit(unlink(tmp), add = TRUE)
  write_sframe(instr, tmp, overwrite = TRUE)
  back <- read_sframe(tmp)

  pairs <- Filter(function(i) i$id == "crit_pairs", back$items)[[1]]
  expect_identical(pairs$comparison_items, c("service", "location", "price"))
  expect_identical(pairs$comparison_scale, "saaty")

  influence <- Filter(function(i) i$id == "crit_influence", back$items)[[1]]
  expect_identical(influence$comparison_scale, "influence")

  points <- Filter(function(i) i$id == "crit_points", back$items)[[1]]
  expect_identical(points$comparison_items, c("service", "location", "price"))
})

test_that("pairwise_comparison and criteria_weight expand into the documented export columns", {
  responses <- data.frame(
    crit_pairs__service__vs__location = 3L,
    crit_pairs__service__vs__price = -2L,
    crit_pairs__location__vs__price = 1L,
    crit_influence__service__to__location = 2L,
    crit_influence__location__to__service = 0L,
    crit_influence__service__to__price = 1L,
    crit_influence__price__to__service = 3L,
    crit_influence__location__to__price = 4L,
    crit_influence__price__to__location = 0L,
    crit_points__service = 40L,
    crit_points__location = 35L,
    crit_points__price = 25L,
    check.names = FALSE
  )
  instr <- decision_item_instrument()
  cleaned <- read_responses(responses, instr)
  expect_true(all(colnames(responses) %in% colnames(cleaned)))
})
