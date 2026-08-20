# tests/testthat/test-shiny-decision-render.R
# The Shiny survey path must render the 2 decision item types and, more
# importantly, must emit the same expansion columns the static template and
# the Google Sheets collector emit. A survey that renders but produces data
# the package's own runners cannot assemble would be a hollow feature, so the
# end-to-end assembly is what these tests check.

crits <- c("service", "price", "delivery")

decision_instrument <- function(required = TRUE) {
  sf_instrument(
    title = "Decision render", version = "1.0.0",
    components = list(
      sf_item("pw", "Compare the criteria", type = "pairwise_comparison",
              comparison_items = crits, comparison_scale = "saaty",
              required = required),
      sf_item("infl", "Influence among the criteria",
              type = "pairwise_comparison", comparison_items = crits,
              comparison_scale = "influence", required = required),
      sf_item("cw", "Divide 100 points", type = "criteria_weight",
              comparison_items = crits, required = required)
    )
  )
}

saaty_answers <- function() {
  cols <- sframe_comparison_columns(
    decision_instrument()$items[[1]]
  )
  stats::setNames(as.list(c(3, 5, 2))[seq_along(cols)], cols)
}

test_that("both decision types render rather than falling through to a text box", {
  skip_if_not_installed("shiny")
  inst <- decision_instrument()

  for (i in seq_along(inst$items)) {
    ui <- surveyframe:::sframe_render_input(inst$items[[i]], list())
    html <- as.character(ui)
    expect_match(html, "sf-decision-block",
                 info = paste("item", inst$items[[i]]$id, "did not render"))
    expect_false(grepl("sf-fallback", html))
  }
})

test_that("the rendered input ids are the export contract's column names", {
  skip_if_not_installed("shiny")
  inst <- decision_instrument()

  for (item in inst$items) {
    html <- as.character(surveyframe:::sframe_render_input(item, list()))
    for (col in sframe_comparison_columns(item)) {
      expect_match(html, col, fixed = TRUE,
                   info = paste("missing input for", col))
    }
  }
})

test_that("a Shiny response row carries expansion columns, not one joined column", {
  inst <- decision_instrument()
  bl <- surveyframe:::sframe_branch_lookup(inst)

  vals <- c(
    saaty_answers(),
    stats::setNames(as.list(rep(2, 6)),
                    sframe_comparison_columns(inst$items[[2]])),
    stats::setNames(as.list(c(50, 30, 20)),
                    sframe_comparison_columns(inst$items[[3]]))
  )

  row <- surveyframe:::sframe_response_row(
    inst, vals, bl,
    started_at = as.POSIXct("2026-01-01 10:00:00", tz = "UTC"),
    submitted_at = as.POSIXct("2026-01-01 10:05:00", tz = "UTC")
  )

  expect_true("pw__service__vs__price" %in% names(row))
  expect_true("infl__service__to__price" %in% names(row))
  expect_true("cw__service" %in% names(row))
  # the bare item id must NOT appear, or the pipe-joined shape crept back
  expect_false("pw" %in% names(row))
  expect_false("cw" %in% names(row))
})

test_that("a Shiny-collected response assembles through the decision runners", {
  # This is the property that matters. If it fails, an MCDM survey run in
  # Shiny produces data the package cannot analyse.
  inst <- decision_instrument()
  bl <- surveyframe:::sframe_branch_lookup(inst)

  pw_cols <- sframe_comparison_columns(inst$items[[1]])
  cw_cols <- sframe_comparison_columns(inst$items[[3]])
  vals <- c(
    stats::setNames(as.list(c(3, 5, 2)), pw_cols),
    stats::setNames(as.list(rep(2, 6)),
                    sframe_comparison_columns(inst$items[[2]])),
    stats::setNames(as.list(c(50, 30, 20)), cw_cols)
  )

  row <- surveyframe:::sframe_response_row(
    inst, vals, bl,
    started_at = as.POSIXct("2026-01-01 10:00:00", tz = "UTC"),
    submitted_at = as.POSIXct("2026-01-01 10:05:00", tz = "UTC")
  )
  # numeric round trip, as a CSV collector would produce
  for (col in c(pw_cols, cw_cols)) row[[col]] <- as.numeric(row[[col]])

  assembled <- sframe_assemble_pairwise(row, inst, "pw")
  expect_equal(dim(assembled$matrices[[1]]), c(3L, 3L))
  expect_equal(unname(diag(assembled$matrices[[1]])), rep(1, 3))
  # a signed 3 on service vs price means service is 3x price, and the
  # reciprocal is filled rather than stored
  expect_equal(assembled$matrices[[1]]["service", "price"], 3)
  expect_equal(assembled$matrices[[1]]["price", "service"], 1 / 3)

  w <- sframe_collected_weights(row, inst, "cw")
  expect_equal(length(w$weights), 3)
  expect_equal(sum(w$weights), 1, tolerance = 1e-8)
})

test_that("a required decision item is incomplete until every pair is answered", {
  inst <- decision_instrument(required = TRUE)
  bl <- surveyframe:::sframe_branch_lookup(inst)
  pw_cols <- sframe_comparison_columns(inst$items[[1]])

  partial <- stats::setNames(as.list(c(3, 5)), pw_cols[1:2])
  missing <- surveyframe:::sframe_missing_required_items(inst, partial, bl)
  expect_true("pw" %in% missing)

  full <- stats::setNames(as.list(c(3, 5, 2)), pw_cols)
  still <- surveyframe:::sframe_missing_required_items(inst, full, bl)
  expect_false("pw" %in% still)
})

test_that("a constant-sum item that does not total 100 counts as incomplete", {
  inst <- decision_instrument(required = TRUE)
  bl <- surveyframe:::sframe_branch_lookup(inst)
  cw_cols <- sframe_comparison_columns(inst$items[[3]])

  short <- stats::setNames(as.list(c(50, 30, 10)), cw_cols)
  expect_true("cw" %in% surveyframe:::sframe_missing_required_items(inst, short, bl))

  exact <- stats::setNames(as.list(c(50, 30, 20)), cw_cols)
  expect_false("cw" %in% surveyframe:::sframe_missing_required_items(inst, exact, bl))

  over <- stats::setNames(as.list(c(60, 30, 20)), cw_cols)
  expect_true("cw" %in% surveyframe:::sframe_missing_required_items(inst, over, bl))
})

test_that("an item with too few comparison items is refused at construction", {
  # The renderer has a "not fully configured" fallback, but sf_item() refuses
  # the item before it can ever reach a page, which is the better place.
  expect_error(
    sf_item("pw2", "Compare", type = "pairwise_comparison",
            comparison_items = character(0)),
    class = "sframe_validation_error"
  )
  expect_error(
    sf_item("pw3", "Compare", type = "pairwise_comparison",
            comparison_items = "only_one"),
    class = "sframe_validation_error"
  )
})
