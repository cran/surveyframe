# test-builder-analysis.R -- builder and analysis feature tests for surveyframe
# Covers: launch_builder, export_google_sheet, run_analysis_plan,
#         render_results, render_report, updated sf_item types.

library(testthat)
library(surveyframe)

# ---- helpers ----------------------------------------------------------------
make_instr <- function() {
  cs <- sf_choices("agree5",
    values = 1:5,
    labels = c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"))
  sc <- sf_scale("sat", "Satisfaction", items = c("sat1","sat2"))
  i1 <- sf_item("sat1", "I am satisfied overall.",
                 type = "likert", required = TRUE, choice_set = "agree5",
                 scale_id = "sat")
  i2 <- sf_item("sat2", "I would recommend this service.",
                 type = "likert", required = TRUE, choice_set = "agree5",
                 scale_id = "sat")
  i3 <- sf_item("gender", "What is your gender?",
                 type = "single_choice", choice_set = "agree5")
  i4 <- sf_item("age", "Your age.", type = "numeric")
  i5 <- sf_item("comments", "Any comments?", type = "textarea")
  sf_instrument("Test Survey",
    components = list(cs, sc, i1, i2, i3, i4, i5))
}

make_responses <- function(n = 30) {
  set.seed(42)
  data.frame(
    started_at   = rep("2025-01-01T10:00:00Z", n),
    submitted_at = rep("2025-01-01T10:05:00Z", n),
    sat1    = sample(1:5, n, replace = TRUE),
    sat2    = sample(1:5, n, replace = TRUE),
    gender  = sample(c("1","2","3"), n, replace = TRUE),
    age     = as.numeric(sample(18:65, n, replace = TRUE)),
    comments = rep(NA_character_, n),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

add_rq <- function(instr, id, q, vars, test) {
  instr$analysis_plan <- c(instr$analysis_plan, list(list(
    id = id, research_question = q, variables = vars, test = test,
    alpha = 0.05, citations = character(0), interpretation = "",
    result = NULL)))
  instr
}

# ---- sf_item new types ------------------------------------------------------
test_that("sf_item accepts all 13 types", {
  types <- c("likert","single_choice","multiple_choice","numeric",
             "text","textarea","date","matrix","slider","ranking",
             "rating","section_break","text_block")
  for (t in types)
    expect_s3_class(sf_item("q1", "Label", type = t), "sf_item")
})

test_that("sf_item stores matrix_items", {
  item <- sf_item("m1", "Rate aspects", type = "matrix",
                  choice_set = "agree5",
                  matrix_items = c("Speed","Quality","Value"))
  expect_equal(item$matrix_items, c("Speed","Quality","Value"))
})

test_that("sf_item stores slider parameters", {
  item <- sf_item("sl1", "Pain", type = "slider",
                  slider_min = 0, slider_max = 10, slider_step = 0.5)
  expect_equal(item$slider_min, 0)
  expect_equal(item$slider_max, 10)
  expect_equal(item$slider_step, 0.5)
})

test_that("sf_item stores rating parameters", {
  item <- sf_item("r1", "Rate us", type = "rating",
                  rating_max = 7, rating_icon = "heart")
  expect_equal(item$rating_max, 7)
  expect_equal(item$rating_icon, "heart")
})

test_that("sf_item rejects invalid type", {
  expect_error(sf_item("q1", "Label", type = "checkbox"))
})

test_that("sf_item section_break stores section_intro", {
  item <- sf_item("s1", "Demographics", type = "section_break",
                  section_intro = "Please answer.")
  expect_equal(item$section_intro, "Please answer.")
})

test_that("sf_item text_block is accepted", {
  item <- sf_item("tb1", "Read carefully.", type = "text_block")
  expect_equal(item$type, "text_block")
})

test_that("sf_item page number is stored", {
  item <- sf_item("q1", "Q", type = "text", page = 2L)
  expect_equal(item$page, 2L)
})

test_that("validate_sframe catches invalid item IDs and duplicate component IDs", {
  cs1 <- sf_choices("agree5", 1:5, c("SD", "D", "N", "A", "SA"))
  cs2 <- sf_choices("agree5", c("yes", "no"), c("Yes", "No"))
  bad_item <- sf_item("bad-id", "Bad ID", type = "likert", choice_set = "agree5")
  item <- sf_item("sat1", "Satisfaction", type = "likert",
                  choice_set = "agree5", scale_id = "sat")
  sc1 <- sf_scale("sat", "Satisfaction", items = "sat1")
  sc2 <- sf_scale("sat", "Duplicate scale", items = "sat1")
  instr <- sf_instrument("Bad Survey",
                         components = list(cs1, cs2, bad_item, item, sc1, sc2))

  result <- validate_sframe(instr, strict = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("Invalid item ID", result$problems)))
  expect_true(any(grepl("Duplicate choice set IDs", result$problems)))
  expect_true(any(grepl("Duplicate scale IDs", result$problems)))
})

# ---- launch_builder ---------------------------------------------------------
test_that("launch_builder returns a path without opening browser", {
  path <- launch_builder(open = FALSE)
  expect_true(file.exists(path))
  expect_true(grepl("\\.html$", path, ignore.case = TRUE))
})

test_that("builder HTML is complete and contains key UI elements", {
  html <- paste(readLines(launch_builder(open = FALSE), warn = FALSE),
                collapse = "\n")
  expect_true(nchar(html) > 20000)
  expect_true(grepl("SurveyBuilder", html))
  expect_true(grepl("fabMenu",       html))
  expect_true(grepl("inspector",     html))
  expect_true(grepl("m-settings",    html))
  expect_true(grepl("rq-test",       html))
  expect_true(grepl("pvFrame",       html))
  expect_true(grepl("an-split",      html))
})

test_that("builder HTML contains all item type buttons", {
  html <- paste(readLines(launch_builder(open = FALSE), warn = FALSE),
                collapse = "\n")
  for (t in c("likert","single_choice","multiple_choice","matrix",
               "numeric","slider","ranking","rating","date",
               "section_break","text_block"))
    expect_true(grepl(t, html, fixed = TRUE), info = paste("Missing:", t))
})

test_that("builder HTML has autosave and recovery banner", {
  html <- paste(readLines(launch_builder(open = FALSE), warn = FALSE),
                collapse = "\n")
  expect_true(grepl("localStorage", html))
  expect_true(grepl("recBan",       html))
})

test_that("builder HTML computes a SHA-256 hash before saving", {
  html <- paste(readLines(launch_builder(open = FALSE), warn = FALSE),
                collapse = "\n")
  expect_true(grepl("crypto\\.subtle\\.digest", html))
  expect_true(grepl("sframeHash", html))
})

test_that("builder HTML has three mode buttons", {
  html <- paste(readLines(launch_builder(open = FALSE), warn = FALSE),
                collapse = "\n")
  expect_true(grepl("data-mode=\"build\"",   html))
  expect_true(grepl("data-mode=\"preview\"", html))
  expect_true(grepl("data-mode=\"analyse\"", html))
})

# ---- export_static_survey ---------------------------------------------------
test_that("export_static_survey writes a standalone HTML file", {
  tmp <- tempfile(fileext = ".html")
  out <- export_static_survey(make_instr(), output_path = tmp, open = FALSE)
  expect_true(file.exists(out))
  html <- paste(readLines(out, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("Test Survey", html, fixed = TRUE))
  expect_true(grepl("application/json", html, fixed = TRUE))
  expect_true(grepl("sat1", html, fixed = TRUE))
  unlink(tmp)
})

test_that("export_static_survey defaults to tempdir when no path is supplied", {
  out <- export_static_survey(make_instr(), open = FALSE, overwrite = TRUE)
  expect_true(file.exists(out))
  expect_true(startsWith(normalizePath(out), normalizePath(tempdir())))
  unlink(out)
})

# ---- export_google_sheet ----------------------------------------------------
test_that("export_google_sheet writes a .gs file with correct structure", {
  instr <- make_instr()
  result <- export_google_sheet(
    instr,
    sheet_url  = "https://docs.google.com/spreadsheets/d/FAKEID",
    output_dir = tempdir()
  )
  expect_true(file.exists(result))
  gs <- paste(readLines(result, warn = FALSE), collapse = "\n")
  expect_true(grepl("doPost",         gs))
  expect_true(grepl("SpreadsheetApp", gs))
  expect_true(grepl("TARGET_SHEET_URL", gs))
  expect_true(grepl("sat1",           gs))
  unlink(result)
})

test_that("export_google_sheet reports missing output directories", {
  expect_error(
    export_google_sheet(
      make_instr(),
      sheet_url = "https://docs.google.com/spreadsheets/d/FAKEID",
      output_dir = tempfile()
    ),
    class = "sframe_error"
  )
})

# ---- run_analysis_plan: structure -------------------------------------------
test_that("run_analysis_plan returns sframe_analysis_results", {
  instr <- add_rq(make_instr(), "r1", "Q", "gender", "frequency")
  res   <- run_analysis_plan(make_responses(), instr)
  expect_s3_class(res, "sframe_analysis_results")
})

test_that("each result element carries research_question and test", {
  instr <- add_rq(make_instr(), "r1", "Frequency of gender", "gender",
                  "frequency")
  res   <- run_analysis_plan(make_responses(), instr)
  expect_equal(length(res), 1L)
  r1 <- res[[1]]
  expect_equal(r1$test,                "frequency")
  expect_equal(r1$research_question,   "Frequency of gender")
  expect_true("apa"  %in% names(r1))
  expect_true("n"    %in% names(r1))
})

test_that("each result element carries citations list", {
  instr <- add_rq(make_instr(), "r1", "Q", c("sat1","gender"),
                  "mann_whitney")
  res   <- run_analysis_plan(make_responses(), instr)
  expect_true(!is.null(res[[1]]$citations))
})

# ---- run_analysis_plan: each test type --------------------------------------
test_that("frequency test returns table and apa string", {
  instr <- add_rq(make_instr(), "r1", "Q", "gender", "frequency")
  r     <- run_analysis_plan(make_responses(), instr)[[1]]
  expect_null(r$error)
  expect_true(is.data.frame(r$table))
  expect_true(nchar(r$apa) > 0)
})

test_that("mann_whitney test returns statistic and apa string", {
  # Mann-Whitney requires exactly 2 groups: use a binary gender variable
  instr <- add_rq(make_instr(), "r1", "Q", c("gender2","sat1"),
                  "mann_whitney")
  set.seed(7)
  dat <- data.frame(
    started_at = "2025-01-01T10:00:00Z", submitted_at = "2025-01-01T10:05:00Z",
    sat1 = sample(1:5, 30, replace = TRUE),
    sat2 = sample(1:5, 30, replace = TRUE),
    gender  = sample(c("1","2","3"), 30, replace = TRUE),
    gender2 = sample(c("male","female"), 30, replace = TRUE),
    age     = as.numeric(sample(18:65, 30, replace = TRUE)),
    comments = rep(NA_character_, 30),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  # Add gender2 item to instrument temporarily
  instr$items <- c(instr$items,
    list(sf_item("gender2","Binary gender",type="single_choice")))
  r <- run_analysis_plan(dat, instr)[[1]]
  expect_null(r$error)
  expect_true(nchar(r$apa) > 0)
})

test_that("pearson correlation returns r and apa string", {
  instr <- add_rq(make_instr(), "r1", "Q", c("sat1","age"),
                  "correlation_pearson")
  r     <- run_analysis_plan(make_responses(), instr)[[1]]
  expect_null(r$error)
  expect_true(nchar(r$apa) > 0)
})

test_that("spearman correlation returns apa string", {
  instr <- add_rq(make_instr(), "r1", "Q", c("sat1","age"),
                  "correlation_spearman")
  r     <- run_analysis_plan(make_responses(), instr)[[1]]
  expect_null(r$error)
  expect_true(nchar(r$apa) > 0)
})

test_that("crosstab returns chi_sq, p, and table", {
  instr <- add_rq(make_instr(), "r1", "Q", c("gender","sat1"),
                  "crosstab")
  r     <- run_analysis_plan(make_responses(), instr)[[1]]
  expect_null(r$error)
  expect_true(!is.null(r$chi_sq))
  expect_true(!is.null(r$p))
})

test_that("linear regression returns coefficients and apa string", {
  instr <- add_rq(make_instr(), "r1", "Q", c("sat1","age"),
                  "regression_linear")
  r     <- run_analysis_plan(make_responses(), instr)[[1]]
  expect_null(r$error)
  expect_true(nchar(r$apa) > 0)
})

test_that("kruskal_wallis returns statistic and apa string", {
  instr <- add_rq(make_instr(), "r1", "Q", c("sat1","gender"),
                  "kruskal_wallis")
  r     <- run_analysis_plan(make_responses(), instr)[[1]]
  expect_null(r$error)
  expect_true(nchar(r$apa) > 0)
})

test_that("run_analysis_plan handles two RQs correctly", {
  instr <- make_instr()
  instr <- add_rq(instr, "r1", "Q1", "gender",         "frequency")
  instr <- add_rq(instr, "r2", "Q2", c("sat1","age"),  "correlation_pearson")
  res   <- run_analysis_plan(make_responses(), instr)
  expect_length(res, 2L)
  expect_equal(res[[1]]$test, "frequency")
  expect_equal(res[[2]]$test, "correlation_pearson")
  expect_null(res[[1]]$error)
  expect_null(res[[2]]$error)
})

# ---- render_results ---------------------------------------------------------
test_that("render_results writes an HTML file", {
  instr <- add_rq(make_instr(), "r1", "Freq of gender", "gender", "frequency")
  res   <- run_analysis_plan(make_responses(), instr)
  tmp   <- tempfile(fileext = ".html")
  out   <- render_results(res, instr, output_file = tmp)
  expect_true(file.exists(out))
  html  <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(nchar(html) > 200)
  unlink(tmp)
})

test_that("render_results HTML contains the research question text", {
  q     <- "Is gender associated with satisfaction level?"
  instr <- add_rq(make_instr(), "r1", q, c("gender","sat1"), "crosstab")
  res   <- run_analysis_plan(make_responses(), instr)
  tmp   <- tempfile(fileext = ".html")
  render_results(res, instr, output_file = tmp)
  html  <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("gender associated with satisfaction", html))
  unlink(tmp)
})

test_that("render_results accepts output_path alias", {
  instr <- add_rq(make_instr(), "r1", "Q", "gender", "frequency")
  res   <- run_analysis_plan(make_responses(), instr)
  tmp   <- tempfile(fileext = ".html")
  render_results(res, instr, output_path = tmp)
  expect_true(file.exists(tmp))
  unlink(tmp)
})

test_that("write_sframe/read_sframe preserve analysis plans and extended item fields", {
  instr <- make_instr()
  instr$items <- c(instr$items, list(
    sf_item("m1", "Rate aspects", type = "matrix", choice_set = "agree5",
            matrix_items = c("Speed", "Quality")),
    sf_item("r1", "Rate us", type = "rating", rating_max = 7,
            rating_icon = "heart")
  ))
  instr <- add_rq(instr, "rq1", "How often?", "gender", "frequency")
  path <- tempfile(fileext = ".sframe")
  write_sframe(instr, path, overwrite = TRUE)
  loaded <- read_sframe(path)
  expect_equal(length(loaded$analysis_plan), 1L)
  expect_equal(loaded$analysis_plan[[1]]$test, "frequency")
  expect_equal(loaded$items[[6]]$matrix_items, c("Speed", "Quality"))
  expect_equal(loaded$items[[7]]$rating_max, 7L)
  expect_equal(loaded$items[[7]]$rating_icon, "heart")
  unlink(path)
})

# ---- render_report ----------------------------------------------------------
test_that("render_report produces an HTML file", {
  tmp <- tempfile(fileext = ".html")
  render_report(make_instr(), output_file = tmp)
  expect_true(file.exists(tmp))
  html <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("Test Survey", html))
  unlink(tmp)
})

test_that("render_report codebook includes all item IDs", {
  tmp <- tempfile(fileext = ".html")
  render_report(make_instr(), output_file = tmp, include_codebook = TRUE)
  html <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("sat1",   html))
  expect_true(grepl("gender", html))
  expect_true(grepl("age",    html))
  unlink(tmp)
})

test_that("render_report accepts output_path alias", {
  tmp <- tempfile(fileext = ".html")
  render_report(make_instr(), output_path = tmp)
  expect_true(file.exists(tmp))
  unlink(tmp)
})

test_that("render_report includes analysis plan questions", {
  instr <- add_rq(make_instr(), "r1",
                  "Does age predict satisfaction?",
                  c("sat1","age"), "regression_linear")
  tmp <- tempfile(fileext = ".html")
  render_report(instr, output_file = tmp)
  html <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("age predict satisfaction", html))
  unlink(tmp)
})

test_that("render_report includes reliability when data supplied", {
  tmp  <- tempfile(fileext = ".html")
  suppressWarnings(render_report(
    make_instr(), data = make_responses(), output_file = tmp,
    include_reliability = TRUE
  ))
  html <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("Reliability|alpha|Cronbach", html, ignore.case = TRUE))
  unlink(tmp)
})

test_that("0.3.3: SurveyBuilder has one settings entry point and one Add-question control", {
  builder_path <- system.file("builder", "survey_builder.html", package = "surveyframe")
  if (!nzchar(builder_path)) builder_path <- file.path("..", "..", "inst", "builder", "survey_builder.html")
  skip_if(!file.exists(builder_path), "builder HTML not found")
  html <- paste(readLines(builder_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  # One settings entry point: the sidebar CTA (the v0.3.4 rework replaced
  # the clickable-title gear and the Welcome/Logo/Thank You strip with a
  # single primary button; the top bar carries no settings control).
  expect_false(grepl('class="btn btn-s btn-sm btn-gear"', html, fixed = TRUE))
  expect_match(html, "sb-settings-cta", fixed = TRUE)
  expect_false(grepl("sb-title-btn", html, fixed = TRUE))
  expect_false(grepl("setup-strip", html, fixed = TRUE))

  # "+ Add question" opens the type picker instead of silently adding a
  # Likert item, and the empty canvas offers exactly two secondary actions
  expect_match(html, 'id="fabMoreBtn"', fixed = TRUE)
  expect_match(html, 'onclick="toggleFabMenu()"', fixed = TRUE)
  # The primary "+ Add question" button opens the picker rather than
  # silently defaulting to Likert (the picker's own "Likert scale" menu
  # item legitimately still calls qAdd(\'likert\'), so check the specific
  # fab-row button rather than the whole file).
  fab_row <- sub(".*(<div class=\"fab-row\">.*?</div>).*", "\\1", html)
  expect_false(grepl("qAdd('likert')", fab_row, fixed = TRUE))
  expect_match(html, "sb-empty-cta", fixed = TRUE)
})
