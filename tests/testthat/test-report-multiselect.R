# Batch 4 #10: a multiple-choice question posts one indicator column per option,
# named item__option, and never a column under its own id. The distribution
# section required the parent column and skipped the item, so every collected
# multi-select vanished from the report. A supplied joined value was also counted
# as one category rather than as separate selections.

multi_instrument <- function() {
  sf_instrument("Multi", components = list(
    sf_choices("uses", values = c("a", "b", "c"),
               labels = c("Commuting", "Shopping", "Leisure")),
    sf_item("why", "What do you use it for?", type = "multiple_choice",
            choice_set = "uses"),
    sf_item("age", "Age", type = "numeric")))
}

# The shape a collector writes: one indicator per option.
multi_responses <- function() {
  data.frame(
    why__a = c("1", "1", "0", "1"),
    why__b = c("0", "1", "1", "0"),
    why__c = c("0", "0", "1", "1"),
    age    = c(31, 44, 27, 52),
    stringsAsFactors = FALSE)
}

rendered <- function(instr, data) {
  out <- tempfile(fileext = ".html")
  suppressMessages(render_report(instr, data = data, output_file = out,
                                 include_analysis = FALSE,
                                 include_models = FALSE))
  paste(readLines(out, warn = FALSE), collapse = "\n")
}

test_that("10: a collected multi-select appears in the distributions", {
  skip_on_cran()  # renders a report or runs a full plan: slow on CRAN's machines
  html <- rendered(multi_instrument(), multi_responses())
  # the label alone proves nothing: the codebook names every item whether or
  # not the distributions section drew it. The denominator line is written by
  # that section and nowhere else.
  expect_match(html, "could pick more than one", fixed = TRUE)
  expect_match(html, "What do you use it for?", fixed = TRUE)
})

test_that("10: the report states the denominator, since answers overlap", {
  skip_on_cran()  # renders a report or runs a full plan: slow on CRAN's machines
  html <- rendered(multi_instrument(), multi_responses())
  # 4 respondents, 7 selections between them
  expect_match(html, "4 respondents, who could pick more than one", fixed = TRUE)
})

test_that("10: an item with no indicator columns is left out quietly", {
  skip_on_cran()  # renders a report or runs a full plan: slow on CRAN's machines
  instr <- multi_instrument()
  data <- multi_responses()
  data <- data[, "age", drop = FALSE]
  expect_no_error(rendered(instr, data))
})

test_that("10: a single-choice item is unaffected", {
  skip_on_cran()  # renders a report or runs a full plan: slow on CRAN's machines
  instr <- sf_instrument("Single", components = list(
    sf_choices("uses", values = c("a", "b"), labels = c("Yes", "No")),
    sf_item("pick", "Pick one", type = "single_choice", choice_set = "uses")))
  html <- rendered(instr, data.frame(pick = c("a", "b", "a"),
                                     stringsAsFactors = FALSE))
  expect_match(html, "Pick one", fixed = TRUE)
})
