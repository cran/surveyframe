# tests/testthat/test-read-responses-name-repair.R
# A matrix row or choice label containing a space produces an expansion column
# containing a space, and the collectors write that correctly. read.csv()
# rewrites it, because check.names defaults to TRUE, so "q1__Row one" arrives
# as "q1__Row.one" and read_responses() rejects it as undeclared. Nothing about
# the old message told the researcher that R had rewritten their header, so it
# read as a declaration problem they did not have. Found during the D2.6 audit.

spaced_instrument <- function() {
  sf_instrument(
    title = "Spaced", version = "1.0.0",
    components = list(
      sf_choices("cs", 1:3, c("Low", "Mid", "High")),
      sf_item("q1", "Q", type = "matrix",
              matrix_items = c("Row one", "Row two"), choice_set = "cs")
    )
  )
}

spaced_csv <- function(inst) {
  cols <- sframe_item_expansion_columns(inst)
  d <- as.data.frame(
    stats::setNames(lapply(cols, function(x) rep(1:3, length.out = 6)), cols),
    check.names = FALSE
  )
  f <- tempfile(fileext = ".csv")
  utils::write.csv(d, f, row.names = FALSE)
  f
}

test_that("the collectors write column names with the spaces intact", {
  inst <- spaced_instrument()
  header <- readLines(spaced_csv(inst), n = 1)
  expect_match(header, "q1__Row one", fixed = TRUE)
  expect_match(header, "q1__Row two", fixed = TRUE)
})

test_that("a header rewritten by read.csv() is explained, not just rejected", {
  inst <- spaced_instrument()
  mangled <- utils::read.csv(spaced_csv(inst))

  expect_equal(names(mangled), c("q1__Row.one", "q1__Row.two"))

  err <- tryCatch(read_responses(mangled, inst, strict = TRUE),
                  error = function(e) conditionMessage(e))
  expect_match(err, "name repair")
  expect_match(err, "check.names = FALSE", fixed = TRUE)
  expect_match(err, "q1__Row one", fixed = TRUE)
})

test_that("reading with check.names = FALSE just works", {
  inst <- spaced_instrument()
  clean <- utils::read.csv(spaced_csv(inst), check.names = FALSE)
  expect_no_error(read_responses(clean, inst, strict = TRUE))
})

test_that("a genuinely undeclared column does not get the repair hint", {
  # The hint must fire on the specific cause, not on every undeclared column,
  # or it becomes noise that misdirects a real declaration problem.
  inst <- spaced_instrument()
  d <- utils::read.csv(spaced_csv(inst), check.names = FALSE)
  d$junk <- 1

  err <- tryCatch(read_responses(d, inst, strict = TRUE),
                  error = function(e) conditionMessage(e))
  expect_match(err, "junk")
  expect_no_match(err, "name repair")
})

test_that("matrix and ranking items analyse end to end once names survive", {
  # D2.6 asked whether these types work without extra hand-coding. They do,
  # provided the header is not rewritten on the way in.
  for (ty in c("matrix", "ranking")) {
    comps <- list(sf_choices("cs", 1:3, c("Low", "Mid", "High")))
    args <- list(id = "q1", label = "Q", type = ty, choice_set = "cs")
    if (ty == "matrix") args$matrix_items <- c("Row one", "Row two")
    comps <- c(comps, list(do.call(sf_item, args)))

    base <- sf_instrument(title = "t", version = "1.0.0", components = comps)
    cols <- sframe_item_expansion_columns(base)

    inst <- sf_instrument(
      title = "E2E", version = "1.0.0", components = comps,
      analysis_plan = list(list(
        id = "RQ1", research_question = "Spread?", family = "descriptive",
        method = "descriptives", roles = list(variables = cols)
      ))
    )
    dat <- as.data.frame(
      stats::setNames(lapply(cols, function(x) rep(1:3, length.out = 8)), cols),
      check.names = FALSE
    )
    res <- run_analysis_plan(dat, inst)[[1]]
    expect_null(res$error, info = ty)
    expect_gt(nrow(res$table), 0)
  }
})
