# Batch 7 #20: SurveyStudio exported the real instrument into its preview
# iframe without overriding the endpoint, while the text beside it said
# anything entered stays in the preview. The exporter falls back to the
# instrument's configured Google Sheets endpoint, so a configured instrument
# could post test answers into the researcher's live collector, mixed in with
# real participants' data and indistinguishable from it.

collector_instrument <- function() {
  instr <- sf_instrument("Isolated", components = list(
    sf_item("q1", "How was it?", type = "text")
  ))
  instr$render <- list(
    google_sheets_endpoint = "https://script.google.com/macros/s/LIVE/exec",
    thankyou = list(redirect_url = "https://example.com/next",
                    message = "Recorded.")
  )
  instr
}

exported <- function(...) {
  path <- tempfile(fileext = ".html")
  suppressMessages(export_static_survey(output_path = path, open = FALSE, ...))
  on.exit(unlink(path), add = TRUE)
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

test_that("20: a deployable export still carries its collector", {
  html <- exported(collector_instrument())
  expect_match(html, "macros/s/LIVE/exec", fixed = TRUE)
  expect_match(html, "https://example.com/next", fixed = TRUE)
})

test_that("20: a preview export carries no collector and no redirect", {
  html <- exported(collector_instrument(), preview = TRUE)
  expect_false(grepl("macros/s/LIVE/exec", html, fixed = TRUE))
  expect_false(grepl("https://example.com/next", html, fixed = TRUE))
  # and it says so on the page, so the isolation is visible
  expect_match(html, "Preview", fixed = TRUE)
})

test_that("20: a preview refuses a supplied endpoint", {
  expect_error(
    exported(collector_instrument(), preview = TRUE,
             endpoint_url = "https://example.com/collect"),
    class = "sframe_error")
})

test_that("20: the preview still renders every question", {
  html <- exported(collector_instrument(), preview = TRUE)
  expect_match(html, "How was it?", fixed = TRUE)
})

test_that("20: Studio's preview asks for an isolated export", {
  src <- sframe_installed_text("inst", "shiny", "app.R")
  expect_match(src, "preview = TRUE", fixed = TRUE)
})
