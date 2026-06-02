# tests/testthat/test-0.3.1-fixes.R
# Regression tests for surveyframe 0.3.1 bug fixes.
# One test per fix ID (A, B, C, C2, D, E/F).

library(testthat)

# ── Shared minimal instrument ─────────────────────────────────────────────────
make_instr <- function(with_matrix = FALSE, with_endpoint = NULL,
                       with_header = FALSE, with_logo = FALSE) {
  cs <- sf_choices("ag5", 1:5,
    c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
  items <- list(cs,
    sf_item("q1", "Question 1", type = "likert", choice_set = "ag5"))
  if (with_matrix) {
    items <- c(items, list(
      sf_item("mx1", "Matrix item", type = "matrix",
               matrix_items = c("row_a", "row_b"))
    ))
  }
  instr <- sf_instrument("Test Survey", components = items)
  if (!is.null(with_endpoint)) {
    if (is.null(instr$render)) instr$render <- list()
    instr$render$google_sheets_endpoint <- with_endpoint
  }
  if (with_header || with_logo) {
    if (is.null(instr$render)) instr$render <- list()
    if (is.null(instr$render$header)) instr$render$header <- list()
    instr$render$header$institution <- "Test University"
    if (with_logo) {
      # Minimal 1x1 JPEG in base64 (JFIF marker sequence)
      instr$render$header$logo_base64   <- "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAgGBgcGBQgHBwcJCQgKDBQNDAsLDBkSEw8U"
      instr$render$header$logo_media_type <- "image/jpeg"
    }
  }
  instr
}

# ── Fix A: static export includes header logo and institution ─────────────────
test_that("fix-a: exported static survey contains institution and img tag", {
  instr <- make_instr(with_header = TRUE, with_logo = TRUE)
  out   <- export_static_survey(instr,
             output_path = tempfile(fileext = ".html"),
             open = FALSE)
  html  <- paste(readLines(out, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("Test University", html, fixed = TRUE),
    info = "institution name must appear in exported HTML")
  expect_true(grepl("<img", html, fixed = TRUE),
    info = "an img tag must appear in exported HTML when logo is set")
})

# ── Fix B: builder endpoint is honoured when endpoint_url is not supplied ─────
test_that("fix-b: instrument google_sheets_endpoint used as fallback", {
  ep    <- "https://script.google.com/macros/s/TESTID/exec"
  instr <- make_instr(with_endpoint = ep)
  out   <- export_static_survey(instr,
             output_path = tempfile(fileext = ".html"),
             open = FALSE)
  html  <- paste(readLines(out, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl(ep, html, fixed = TRUE),
    info = "builder endpoint must appear in exported HTML")
})

test_that("fix-b: explicit endpoint_url wins over instrument endpoint", {
  ep_instr    <- "https://script.google.com/macros/s/BUILDER/exec"
  ep_explicit <- "https://script.google.com/macros/s/EXPLICIT/exec"
  instr <- make_instr(with_endpoint = ep_instr)
  out   <- export_static_survey(instr,
             output_path   = tempfile(fileext = ".html"),
             endpoint_url  = ep_explicit,
             open          = FALSE)
  html <- paste(readLines(out, encoding = "UTF-8"), collapse = "\n")
  # The data-endpoint attribute on <body> is what the JS reads at runtime.
  # The instrument JSON also embeds render.google_sheets_endpoint, so we
  # must check the attribute value specifically, not the full document.
  expect_true(grepl(
    paste0('data-endpoint="', ep_explicit),
    html, fixed = TRUE),
    info = "data-endpoint must carry the explicit URL")
  expect_false(grepl(
    paste0('data-endpoint="', ep_instr),
    html, fixed = TRUE),
    info = "data-endpoint must not carry the builder URL when overridden")
})

# ── Fix C: submitted row uses respondent_id, not response_id ─────────────────
test_that("fix-c: template uses respondent_id in submission object", {
  tpl_path <- system.file("static_survey", "template.html",
                           package = "surveyframe")
  skip_if(!nzchar(tpl_path), "template.html not found")
  tpl <- paste(readLines(tpl_path, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("respondent_id:respId", tpl, fixed = TRUE),
    info = "submission row must key the identifier as respondent_id")
  expect_false(grepl("response_id:respId", tpl, fixed = TRUE),
    info = "old response_id key must not be present")
})

test_that("fix-c: round-trip CSV with respondent_id column imports cleanly", {
  instr <- make_instr()
  resp_df <- data.frame(
    respondent_id = "R001",
    q1            = 4L,
    stringsAsFactors = FALSE
  )
  expect_no_warning(
    read_responses(resp_df, instr,
                   respondent_id = "respondent_id",
                   strict = FALSE)
  )
})

# ── Fix C2: Apps Script header row includes matrix sub-item columns ───────────
test_that("fix-c2: export_google_sheet includes matrix sub-item headers", {
  instr  <- make_instr(with_matrix = TRUE)
  gs_file <- export_google_sheet(instr,
               sheet_url  = "https://docs.google.com/spreadsheets/d/TEST",
               output_dir = tempdir())
  script <- paste(readLines(gs_file, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("mx1__row_a", script, fixed = TRUE),
    info = "matrix sub-item mx1__row_a must appear in Apps Script headers")
  expect_true(grepl("mx1__row_b", script, fixed = TRUE),
    info = "matrix sub-item mx1__row_b must appear in Apps Script headers")
})

# ── Fix D: read_sheet_responses does not warn about started_at ───────────────
test_that("fix-d: read_sheet_responses body declares started_at as meta_cols", {
  fn_body <- paste(deparse(body(read_sheet_responses)), collapse = "\n")
  expect_true(
    grepl("started_at", fn_body, fixed = TRUE) &&
      grepl("meta_cols", fn_body, fixed = TRUE),
    info = 'read_sheet_responses must pass meta_cols = "started_at"'
  )
})

# ── Fix E/F: logo media type stored and used correctly ───────────────────────
test_that("fix-ef: static export uses logo_media_type when present", {
  instr <- make_instr(with_logo = TRUE)
  out   <- export_static_survey(instr,
             output_path = tempfile(fileext = ".html"),
             open = FALSE)
  html  <- paste(readLines(out, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("image/jpeg", html, fixed = TRUE),
    info = "exported HTML must reference image/jpeg, not image/png, for a JPEG logo")
  expect_false(
    grepl("data:image/png;base64,/9j/", html, fixed = TRUE),
    info = "JPEG logo must not be mislabelled as image/png"
  )
})

test_that("fix-ef: static export falls back to image/png when media type absent", {
  instr <- make_instr(with_header = TRUE)
  instr$render$header$logo_base64 <- "aGVsbG8="  # base64("hello")
  # No logo_media_type set -- should default to image/png
  out  <- export_static_survey(instr,
            output_path = tempfile(fileext = ".html"),
            open = FALSE)
  html <- paste(readLines(out, encoding = "UTF-8"), collapse = "\n")
  expect_true(
    grepl("image/png", html, fixed = TRUE) || !grepl("image/jpeg", html, fixed = TRUE),
    info = "absent media type must not produce image/jpeg in the output"
  )
})
