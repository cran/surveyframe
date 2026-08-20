# tests/testthat/test-quality-decision-columns.R
# B10 asked for the pairwise columns to be exempted from quality_report()'s
# straight-lining and speeding checks. Checking first showed the exemption was
# already there by construction, and that the real exposure was somewhere
# else, so these tests pin both facts:
#
#   1. straight-lining runs over declared scales and timing is wall-clock, so
#      decision columns never reach either. A consistent set of pairwise
#      judgements must not be flagged as straight-lining.
#   2. missingness matched on bare item ids, and multi-column items never post
#      under those, so a respondent who skipped an entire pairwise battery was
#      reported at 0 percent missing.

crits <- c("service", "price", "delivery")

quality_instrument <- function() {
  sf_instrument(
    title = "Quality", version = "1.0.0",
    components = list(
      sf_choices("ag5", 1:5, c("a", "b", "c", "d", "e")),
      sf_item("s1", "I1", type = "likert", choice_set = "ag5", scale_id = "sc"),
      sf_item("s2", "I2", type = "likert", choice_set = "ag5", scale_id = "sc"),
      sf_scale("sc", "Scale", items = c("s1", "s2")),
      sf_item("pw", "Compare", type = "pairwise_comparison",
              comparison_items = crits, comparison_scale = "saaty",
              required = TRUE),
      sf_item("cw", "Allocate", type = "criteria_weight",
              comparison_items = crits, required = TRUE)
    )
  )
}

pw_item <- function(inst) Filter(function(i) identical(i$id, "pw"), inst$items)[[1]]
cw_item <- function(inst) Filter(function(i) identical(i$id, "cw"), inst$items)[[1]]

test_that("a constant set of pairwise judgements is not flagged as straight-lining", {
  inst <- quality_instrument()
  cols <- sframe_comparison_columns(pw_item(inst))

  dat <- data.frame(s1 = c(3, 4, 5), s2 = c(3, 5, 4))
  # every pair answered "equal", which is a legitimate judgement and would
  # look like zero variance to a naive row-variance check
  for (col in cols) dat[[col]] <- rep(1, 3)

  q <- quality_report(dat, inst)

  # only declared scales are examined, and a decision item is never in one
  expect_setequal(names(q$straightline), "sc")
  flagged_ids <- vapply(q$straightline, function(s) s$scale_id, character(1))
  expect_false(any(grepl("^pw", flagged_ids)))
})

test_that("skipping a whole pairwise battery is counted as missing", {
  inst <- quality_instrument()
  pw_cols <- sframe_comparison_columns(pw_item(inst))
  cw_cols <- sframe_comparison_columns(cw_item(inst))

  dat <- data.frame(s1 = c(3, 4, 5), s2 = c(3, 5, 4))
  for (col in pw_cols) dat[[col]] <- c(1, 1, NA)
  for (col in cw_cols) dat[[col]] <- c(50, 30, NA)

  q <- quality_report(dat, inst)

  # respondent 3 answered both Likert items and skipped all 6 decision
  # columns, so its missing rate must be well above zero
  expect_gt(q$missing$respondent_miss[3], 0)
  expect_equal(q$missing$respondent_miss[3], 6 / 8, tolerance = 1e-8)

  # and the decision columns appear in the per-item rates
  expect_true(all(pw_cols %in% names(q$missing$item_miss_rate)))
  expect_true(all(cw_cols %in% names(q$missing$item_miss_rate)))
})

test_that("a fully answered decision battery reports no missingness", {
  inst <- quality_instrument()
  pw_cols <- sframe_comparison_columns(pw_item(inst))
  cw_cols <- sframe_comparison_columns(cw_item(inst))

  dat <- data.frame(s1 = c(3, 4), s2 = c(3, 5))
  for (col in pw_cols) dat[[col]] <- c(1, 3)
  for (col in cw_cols) dat[[col]] <- c(40, 30)

  q <- quality_report(dat, inst)
  expect_equal(unname(q$missing$respondent_miss), c(0, 0))
})

test_that("matrix expansion columns also count towards missingness now", {
  # Same defect, same fix. Matrix items post as item__sub and were invisible
  # to the missingness check for the same reason.
  inst <- sf_instrument(
    title = "Matrix", version = "1.0.0",
    components = list(
      sf_choices("ag5", 1:5, c("a", "b", "c", "d", "e")),
      sf_item("mx", "Rate", type = "matrix",
              matrix_items = c("r1", "r2"), choice_set = "ag5")
    )
  )
  dat <- data.frame(mx__r1 = c(3, NA), mx__r2 = c(4, NA))

  q <- quality_report(dat, inst)
  expect_true(all(c("mx__r1", "mx__r2") %in% names(q$missing$item_miss_rate)))
  expect_equal(unname(q$missing$respondent_miss), c(0, 1))
})

test_that("the Google Sheets header row carries the decision columns", {
  inst <- quality_instrument()
  dir <- tempfile(); dir.create(dir)
  out <- suppressMessages(
    export_google_sheet(inst, sheet_url = "https://example.com/s",
                        output_dir = dir)
  )
  script <- paste(readLines(out, warn = FALSE), collapse = "\n")

  for (col in c(sframe_comparison_columns(pw_item(inst)),
                sframe_comparison_columns(cw_item(inst)))) {
    expect_match(script, col, fixed = TRUE,
                 info = paste("sheet header missing", col))
  }
  # the bare ids must not be headers, or the sheet would collect one column
  # for an item that posts several
  expect_no_match(script, '"pw"', fixed = TRUE)
  expect_no_match(script, '"cw"', fixed = TRUE)
})
