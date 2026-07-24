# tests/testthat/test-v034-interpretations.R
# The interpretations argument on render_report() and render_results():
# written interpretations keyed by analysis-plan block id, rendered beside
# the pre-declared decision rule, never written into the instrument.

interp_fixture <- function() {
  cs <- sf_choices("ag5", 1:5,
    c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
  i1 <- sf_item("sat_1", "The service was fast.", type = "likert",
                choice_set = "ag5", required = TRUE)
  i2 <- sf_item("sat_2", "The service was friendly.", type = "likert",
                choice_set = "ag5", required = TRUE)
  instr <- sf_instrument("Interpretation check",
    components = list(cs, i1, i2),
    analysis_plan = list(
      list(id = "rq_fast", research_question = "How fast was the service?",
           family = "descriptive", method = "frequency",
           roles = list(variable = "sat_1"),
           decision_rule = "Report the modal response for speed."),
      list(id = "rq_friendly", research_question = "How friendly was the service?",
           family = "descriptive", method = "frequency",
           roles = list(variable = "sat_2"),
           decision_rule = "Report the modal response for friendliness.")
    ))
  set.seed(42)
  dat <- data.frame(
    respondent_id = paste0("R", 1:40),
    submitted_at = as.character(Sys.time()),
    sat_1 = sample(1:5, 40, replace = TRUE),
    sat_2 = sample(1:5, 40, replace = TRUE),
    stringsAsFactors = FALSE
  )
  list(instr = instr, dat = dat)
}

render_fallback <- function(instr, dat = NULL, interpretations = NULL) {
  old_opt <- options(surveyframe.use_quarto = FALSE)
  on.exit(options(old_opt), add = TRUE)
  out <- tempfile(fileext = ".html")
  render_report(instr, dat, output_file = out,
                interpretations = interpretations)
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  unlink(out)
  # Embedded base64 charts are huge and sit between a block's table and its
  # interpretation lines. Strip them so rq_chunk() spans a whole block.
  gsub("src=\"data:image/png;base64,[^\"]*\"", "src=\"\"", html)
}

rq_chunk <- function(html, marker) {
  start <- regexpr(marker, html, fixed = TRUE)
  expect_gt(start, 0)
  rest <- substr(html, start, nchar(html))
  next_block <- regexpr("<h3>RQ ", rest, fixed = TRUE)
  if (next_block > 0) substr(rest, 1, next_block - 1) else rest
}

test_that("fallback report pairs the override with the planned decision rule", {
  fx <- interp_fixture()
  html <- render_fallback(fx$instr, fx$dat,
    interpretations = list(rq_friendly = "Friendliness scored above the rule threshold."))

  friendly <- rq_chunk(html, "RQ 2: How friendly")
  expect_match(friendly, "Planned decision rule:")
  expect_match(friendly, "modal response for friendliness")
  expect_match(friendly, "Interpretation:")
  expect_match(friendly, "above the rule threshold")

  fast <- rq_chunk(html, "RQ 1: How fast")
  expect_no_match(fast, "Planned decision rule:")
  expect_no_match(fast, "Interpretation:")
})

test_that("NULL interpretations leaves the report unchanged", {
  fx <- interp_fixture()
  html <- render_fallback(fx$instr, fx$dat)
  expect_no_match(html, "Planned decision rule:")
  expect_no_match(html, "Interpretation:")
})

test_that("codebook-only report also renders the override", {
  fx <- interp_fixture()
  html <- render_fallback(fx$instr,
    interpretations = list(rq_fast = "Speed was pre-drafted before data collection."))
  fast <- rq_chunk(html, "RQ 1: How fast")
  expect_match(fast, "Planned decision rule:")
  expect_match(fast, "Interpretation:")
  expect_match(fast, "pre-drafted before data collection")
  friendly <- rq_chunk(html, "RQ 2: How friendly")
  expect_no_match(friendly, "Interpretation:")
})

test_that("block-id keying survives a plan reorder", {
  fx <- interp_fixture()
  instr <- fx$instr
  instr$analysis_plan <- rev(instr$analysis_plan)
  html <- render_fallback(instr, fx$dat,
    interpretations = list(rq_friendly = "Friendliness override after reorder."))
  # rq_friendly is now the first block, and the override follows it there.
  friendly <- rq_chunk(html, "RQ 1: How friendly")
  expect_match(friendly, "Friendliness override after reorder")
  fast <- rq_chunk(html, "RQ 2: How fast")
  expect_no_match(fast, "Interpretation:")
})

test_that("render_results shows the override in place of the prompt fallback", {
  fx <- interp_fixture()
  results <- run_analysis_plan(fx$dat, fx$instr)
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)

  render_results(results, fx$instr, output_file = out,
    interpretations = list(rq_fast = "Fast service confirmed the rule."))
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "Fast service confirmed the rule")
  # The untouched block keeps its existing fallback text.
  expect_match(html, "modal response for friendliness")
})

test_that("interpretations are cleaned and validated", {
  cleaned <- sframe_clean_interpretations(list(
    rq_a = "  keep me  ",
    rq_b = "",
    rq_c = NA_character_,
    rq_d = 5
  ))
  expect_identical(cleaned, list(rq_a = "keep me"))
  expect_identical(sframe_clean_interpretations(NULL), list())
  expect_identical(sframe_clean_interpretations(c(rq_a = "vector form")),
                   list(rq_a = "vector form"))
  expect_error(sframe_clean_interpretations(42), class = "sframe_error")

  fx <- interp_fixture()
  html <- render_fallback(fx$instr, fx$dat,
    interpretations = list(rq_fast = "   "))
  expect_no_match(html, "Interpretation:")
})

test_that("the Quarto template renders the override", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("quarto")), "Quarto is not installed")
  fx <- interp_fixture()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  old_opt <- options(surveyframe.use_quarto = TRUE)
  on.exit(options(old_opt), add = TRUE)
  render_report(fx$instr, fx$dat, output_file = out,
    interpretations = list(rq_friendly = "Quarto path override text."))
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "Quarto path override text")
  expect_match(html, "Planned decision rule")
})
