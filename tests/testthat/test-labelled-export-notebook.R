# tests/testthat/test-labelled-export-notebook.R
#
# Batch 5, findings 13 and 14.
#
# 13. sframe_export_labelled() attached value labels only to a base item column
#     holding numbers. Matrix cells, which hold the same choice codes, got none,
#     every expansion column got only the parent question as its label, and
#     numeric codes held as text, as read from Google Sheets, got no labels.
# 14. The generated notebook printed a heading and nothing else for a failed
#     analysis block, never ran its report chunk, and wrote the instrument title
#     into YAML unescaped, so a title with a double quote broke the notebook.

labelled_instrument <- function(title = "Labels") {
  sf_instrument(title = title, components = list(
    sf_choices("ag3", 1:3, c("Disagree", "Neutral", "Agree")),
    sf_choices("fruit", c("a", "b"), c("Apple", "Banana")),
    sf_item("q1", "Overall", "likert", choice_set = "ag3"),
    sf_item("mx", "Rate each", "matrix", choice_set = "ag3",
            matrix_items = c("price", "service")),
    sf_item("mc", "Which fruit?", "multiple_choice", choice_set = "fruit")
  ))
}

exported <- function(data, ext = "sav") {
  skip_if_not_installed("haven")
  path <- tempfile(fileext = paste0(".", ext))
  sframe_export_labelled(data, labelled_instrument(), path)
  if (ext == "sav") haven::read_sav(path) else haven::read_dta(path)
}

responses <- function() {
  data.frame(q1 = c(1, 3), mx__price = c(2, 3), mx__service = c(1, 1),
             mc__a = c(1, 0), mc__b = c(1, 1))
}

test_that("13: matrix cells carry value labels and their row wording", {
  back <- exported(responses())
  expect_identical(unname(attr(back$mx__price, "labels")), c(1, 2, 3))
  expect_identical(names(attr(back$mx__price, "labels")), c("Disagree", "Neutral", "Agree"))
  expect_match(attr(back$mx__price, "label"), "price")
  expect_match(attr(back$mx__service, "label"), "service")
})

test_that("13: a multiple-choice column names its option", {
  back <- exported(responses())
  expect_match(attr(back$mc__a, "label"), "Apple")
  expect_match(attr(back$mc__b, "label"), "Banana")
})

test_that("13: numeric codes held as text still receive their labels", {
  text <- responses()
  text$q1 <- as.character(text$q1)
  text$mx__price <- as.character(text$mx__price)
  back <- exported(text)
  expect_identical(names(attr(back$q1, "labels")), c("Disagree", "Neutral", "Agree"))
  expect_identical(names(attr(back$mx__price, "labels")), c("Disagree", "Neutral", "Agree"))
})

test_that("13: the Stata route carries the same labels", {
  back <- exported(responses(), "dta")
  expect_identical(names(attr(back$mx__price, "labels")), c("Disagree", "Neutral", "Agree"))
})

notebook <- function(title) {
  skip_if_not_installed("rmarkdown")
  dir <- tempfile("nb")
  paths <- suppressMessages(sframe_analysis_qmd(labelled_instrument(title), responses(), dir = dir))
  paths$qmd
}

test_that("14: a title with quotes and backslashes keeps the notebook header valid", {
  title <- "The \"best\" survey \\ 2026"
  qmd <- notebook(title)
  expect_identical(rmarkdown::yaml_front_matter(qmd)$title, title)
})

test_that("14: a failed block is shown with its error, and the report chunk runs", {
  qmd <- notebook("Plain")
  src <- paste(readLines(qmd, warn = FALSE), collapse = "\n")
  run_chunk <- regmatches(src, regexpr("(?s)```\\{r run\\}.*?```", src, perl = TRUE))
  code <- sub("(?s)^```\\{r run\\}\\n(.*)```$", "\\1", run_chunk, perl = TRUE)

  results <- structure(
    list(ok = list(block_id = "ok", research_question = "Q1", apa = "fine"),
         bad = list(block_id = "bad", research_question = "Q2", error = "Test failed.")),
    class = "sframe_analysis_results",
    status = list(blocks = 2L, succeeded = 1L, failed = 1L, failed_blocks = "bad"))
  env <- new.env()
  assign("results", results, env)
  assign("run_analysis_plan", function(...) results, env)
  out <- paste(utils::capture.output(eval(parse(text = code), envir = env)), collapse = "\n")
  expect_match(out, "Test failed.", fixed = TRUE)

  report_chunk <- regmatches(src, regexpr("(?s)```\\{r report\\}.*?```", src, perl = TRUE))
  expect_false(grepl("eval: false", report_chunk, fixed = TRUE))
  expect_match(report_chunk, "succeeded", fixed = TRUE)
})
