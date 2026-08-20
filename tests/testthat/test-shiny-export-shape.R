# tests/testthat/test-shiny-export-shape.R
# Before 0.4.0 the Shiny collector pipe-joined a matrix item's cells into a
# single column, so a matrix question answered there arrived as mx = "4|5"
# while read_responses() and the whole analysis layer expect mx__r1 and
# mx__r2. Data collected that way could not be read back by the package at
# all, silently, with no error at collection time. Ranking and multi-select
# had the same shape problem. All 3 now emit the expansion columns the static
# template and the Google Sheets collector already emit.

shape_instrument <- function() {
  sf_instrument(
    title = "Shapes", version = "1.0.0",
    components = list(
      sf_choices("ag5", 1:5, c("a", "b", "c", "d", "e")),
      sf_item("mx", "Rate", type = "matrix",
              matrix_items = c("r1", "r2"), choice_set = "ag5"),
      sf_item("ms", "Pick", type = "multiple_choice", choice_set = "ag5"),
      sf_item("rk", "Order", type = "ranking", choice_set = "ag5"),
      sf_item("txt", "Free", type = "text")
    )
  )
}

shape_row <- function(inst = shape_instrument()) {
  surveyframe:::sframe_response_row(
    inst,
    list(mx__1 = 4, mx__2 = 5, ms = c("1", "3"), rk = "3|1|2|5|4",
         txt = "hello"),
    surveyframe:::sframe_branch_lookup(inst),
    started_at = as.POSIXct("2026-01-01 10:00:00", tz = "UTC"),
    submitted_at = as.POSIXct("2026-01-01 10:05:00", tz = "UTC")
  )
}

test_that("a matrix item emits one column per sub-item, not a joined column", {
  row <- shape_row()

  expect_true(all(c("mx__r1", "mx__r2") %in% names(row)))
  expect_false("mx" %in% names(row))
  expect_equal(as.character(row$mx__r1), "4")
  expect_equal(as.character(row$mx__r2), "5")
})

test_that("a multi-select emits 0 or 1 per option", {
  row <- shape_row()

  expect_true(all(paste0("ms__", 1:5) %in% names(row)))
  expect_false("ms" %in% names(row))
  expect_equal(as.character(row$ms__1), "1")
  expect_equal(as.character(row$ms__3), "1")
  expect_equal(as.character(row$ms__2), "0")
  expect_equal(as.character(row$ms__5), "0")
})

test_that("a ranking emits each option's rank position", {
  # the input holds the order "3|1|2|5|4", so option 3 ranks first
  row <- shape_row()

  expect_true(all(paste0("rk__", 1:5) %in% names(row)))
  expect_false("rk" %in% names(row))
  expect_equal(as.character(row$rk__3), "1")
  expect_equal(as.character(row$rk__1), "2")
  expect_equal(as.character(row$rk__2), "3")
  expect_equal(as.character(row$rk__5), "4")
  expect_equal(as.character(row$rk__4), "5")
})

test_that("a single-column item is untouched", {
  row <- shape_row()
  expect_true("txt" %in% names(row))
  expect_equal(as.character(row$txt), "hello")
})

test_that("read_responses() accepts a Shiny-collected row", {
  # This is the property the whole change exists for. Before it, the row was
  # rejected because its columns did not match the declared expansion.
  inst <- shape_instrument()
  row <- shape_row(inst)

  expect_no_error(
    read_responses(row, inst, strict = TRUE,
                   meta_cols = c("started_at", "submitted_at"))
  )
})

test_that("every declared expansion column appears in the row", {
  inst <- shape_instrument()
  row <- shape_row(inst)
  expect_true(all(sframe_item_expansion_columns(inst) %in% names(row)))
})

test_that("a hidden item blanks all of its expansion columns", {
  inst <- sf_instrument(
    title = "Branched", version = "1.0.0",
    components = list(
      sf_choices("yn", c("yes", "no"), c("Yes", "No")),
      sf_choices("ag5", 1:5, c("a", "b", "c", "d", "e")),
      sf_item("gate", "Continue?", type = "single_choice", choice_set = "yn"),
      sf_item("mx", "Rate", type = "matrix",
              matrix_items = c("r1", "r2"), choice_set = "ag5"),
      sf_branch("mx", depends_on = "gate", operator = "==",
                value = "yes", action = "show")
    )
  )
  bl <- surveyframe:::sframe_branch_lookup(inst)

  hidden <- surveyframe:::sframe_response_row(
    inst, list(gate = "no", mx__1 = 4, mx__2 = 5), bl,
    started_at = Sys.time(), submitted_at = Sys.time()
  )
  expect_true(is.na(hidden$mx__r1))
  expect_true(is.na(hidden$mx__r2))
})
