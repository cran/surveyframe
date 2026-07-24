# tests/testthat/test-v034-date-bounds.R
# date_min/date_max bounds on date items: construction validation, sframe
# round-trip, and the static-survey export.

date_bound_instrument <- function() {
  sf_instrument("Date bounds check",
    components = list(
      sf_item("visit_date", "When did you visit?", type = "date",
              required = TRUE,
              date_min = "2026-01-01", date_max = "2026-12-31"),
      sf_item("any_date", "Pick any date.", type = "date")
    ))
}

test_that("sf_item validates and normalises date bounds", {
  it <- sf_item("d1", "Date", type = "date",
                date_min = as.Date("2026-01-01"), date_max = "2026-06-30")
  expect_identical(it$date_min, "2026-01-01")
  expect_identical(it$date_max, "2026-06-30")

  expect_error(sf_item("d2", "Date", type = "date", date_min = "not-a-date"),
               class = "sframe_validation_error")
  expect_error(sf_item("d3", "Date", type = "date",
                       date_min = "2026-12-31", date_max = "2026-01-01"),
               class = "sframe_validation_error")

  no_bounds <- sf_item("d4", "Date", type = "date")
  expect_null(no_bounds$date_min)
  expect_null(no_bounds$date_max)
})

test_that("sf_item rejects an ambiguous date bound rather than silently misparsing it", {
  # A bare as.Date() without a format guesses at "01/02/2024" and returns
  # the nonsense date "1-02-20" instead of erroring, so date_min/date_max
  # must be parsed against an explicit "%Y-%m-%d" format.
  expect_error(sf_item("d5", "Date", type = "date", date_min = "01/02/2024"),
               class = "sframe_validation_error")
})

test_that("date bounds survive a write/read round-trip", {
  instr <- date_bound_instrument()
  tmp <- tempfile(fileext = ".sframe")
  on.exit(unlink(tmp), add = TRUE)
  write_sframe(instr, tmp, overwrite = TRUE)
  back <- read_sframe(tmp)
  it <- Filter(function(i) i$id == "visit_date", back$items)[[1]]
  expect_identical(it$date_min, "2026-01-01")
  expect_identical(it$date_max, "2026-12-31")
  it2 <- Filter(function(i) i$id == "any_date", back$items)[[1]]
  expect_null(it2$date_min)
})

test_that("the static survey export carries the date bounds", {
  instr <- date_bound_instrument()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  export_static_survey(instr, output_path = out, open = FALSE)
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "2026-01-01")
  expect_match(html, "2026-12-31")
  expect_match(html, "date_min")
})
