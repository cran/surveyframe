# tests/testthat/test-studio-decision-roles.R
# SurveyStudio derives a role dropdown's options by matching each catalog
# entry's level against the role's declared levels. The 7 ranking methods
# read their performance matrix through a performance_items role declaring
# levels = "matrix", and studio_level_meta() had no branch for the matrix item
# type, so a matrix item carried level "identifier" and matched nothing. The
# dropdown was empty for all 7 methods, which left the rated-matrix path
# unwirable in the studio. Same failure shape as the decision item types
# before A6.

studio_env <- function() {
  app <- system.file("shiny", "app.R", package = "surveyframe")
  skip_if(!nzchar(app) || !file.exists(app), "studio app not available")
  txt <- readLines(app, warn = FALSE)
  from <- grep("^studio_level_meta <- function", txt)[1]
  to   <- grep("^studio_flatten_roles <- function", txt)[1]
  skip_if(is.na(from) || is.na(to), "studio helpers not found")
  env <- new.env()
  eval(parse(text = paste(txt[from:(to - 1)], collapse = "\n")), envir = env)
  env
}

mcdm_inst <- function() {
  p <- system.file("extdata", "hotel_supplier_mcdm.sframe",
                   package = "surveyframe")
  skip_if(!nzchar(p) || !file.exists(p), "MCDM fixture not available")
  read_sframe(p)
}

test_that("a matrix item carries the level the performance_items role wants", {
  env <- studio_env()
  inst <- mcdm_inst()
  mx <- Filter(function(i) identical(i$type, "matrix"), inst$items)[[1]]

  meta <- env$studio_level_meta(item = mx)
  expect_identical(meta$level, "matrix")
})

test_that("the performance_items role offers every matrix item", {
  env <- studio_env()
  inst <- mcdm_inst()

  catalog <- env$studio_variable_catalog(inst)
  choices <- env$studio_role_choices(
    list(id = "performance_items", levels = "matrix"), catalog, list()
  )

  # the fixture declares 4 rated criteria
  expect_length(choices, 4)
  expect_true(all(grepl("^rate_", unname(choices))))
})

test_that("the 2 decision item types keep their own separate levels", {
  env <- studio_env()
  inst <- mcdm_inst()
  by_id <- function(id) Filter(function(i) identical(i$id, id), inst$items)[[1]]

  expect_identical(env$studio_level_meta(item = by_id("crit_pairs"))$level,
                   "pairwise_saaty")
  expect_identical(env$studio_level_meta(item = by_id("crit_influence"))$level,
                   "pairwise_influence")
  expect_identical(env$studio_level_meta(item = by_id("crit_points"))$level,
                   "criteria_weight")
})

test_that("a weights_item role offers both weight sources but not the matrix", {
  env <- studio_env()
  inst <- mcdm_inst()
  catalog <- env$studio_variable_catalog(inst)

  choices <- env$studio_role_choices(
    list(id = "weights_item", levels = c("pairwise_saaty", "criteria_weight")),
    catalog, list()
  )
  expect_setequal(unname(choices), c("crit_pairs", "crit_points"))
  expect_false("crit_influence" %in% unname(choices))
})
