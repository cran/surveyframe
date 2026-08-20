# tests/testthat/test-mcdm-fixture.R
# The section 1g worked example (hotel supplier selection) is the release gate
# for the decision family and the spine of the MCDM vignette. It exercises
# every data path at once: pairwise weights, constant-sum weights, a rated
# performance matrix, a researcher-supplied matrix, and a directed influence
# matrix. These tests are the gate criteria, one test each.

mcdm_fixture <- function() {
  p <- system.file("extdata", "hotel_supplier_mcdm.sframe",
                   package = "surveyframe")
  skip_if(!nzchar(p) || !file.exists(p), "MCDM fixture not available")
  read_sframe(p)
}

mcdm_responses <- function() {
  p <- system.file("extdata", "hotel_supplier_mcdm_responses.csv",
                   package = "surveyframe")
  skip_if(!nzchar(p) || !file.exists(p), "MCDM responses not available")
  utils::read.csv(p, stringsAsFactors = FALSE)
}

test_that("gate 1 and 2: the fixture uses shipped constructors and validates", {
  inst <- mcdm_fixture()

  expect_s3_class(inst, "sframe")
  expect_equal(inst$meta$title, "Hotel supplier selection")

  v <- validate_sframe(inst, strict = FALSE)
  expect_true(v$valid)
  expect_length(v$problems, 0)

  types <- vapply(inst$items, function(i) i$type, character(1))
  expect_equal(sum(types == "pairwise_comparison"), 2)
  expect_equal(sum(types == "criteria_weight"), 1)
  expect_equal(sum(types == "matrix"), 4)
})

test_that("gate 3: the fixture hashes to the value it stores", {
  p <- system.file("extdata", "hotel_supplier_mcdm.sframe",
                   package = "surveyframe")
  skip_if(!nzchar(p) || !file.exists(p), "MCDM fixture not available")

  stored <- jsonlite::fromJSON(p, simplifyVector = FALSE)$hash$value
  expect_identical(sframe_hash_value(read_sframe(p)), stored)
})

test_that("gate 5: the instrument declares exactly the 42 expansion columns", {
  inst <- mcdm_fixture()
  cols <- sframe_item_expansion_columns(inst)

  # 6 Saaty pairs + 4 criterion weights + 20 rated cells + 12 ordered
  # influence pairs
  expect_length(cols, 42)
  expect_equal(sum(grepl("^crit_pairs__", cols)), 6)
  expect_equal(sum(grepl("^crit_points__", cols)), 4)
  expect_equal(sum(grepl("^rate_", cols)), 20)
  expect_equal(sum(grepl("^crit_influence__", cols)), 12)

  # the Saaty item stores unordered pairs, the influence item ordered ones
  expect_equal(sum(grepl("__vs__", cols)), 6)
  expect_equal(sum(grepl("__to__", cols)), 12)
})

test_that("gate 5: the bundled responses carry every declared column", {
  inst <- mcdm_fixture()
  resp <- mcdm_responses()

  expect_true(all(sframe_item_expansion_columns(inst) %in% names(resp)))
  expect_equal(nrow(resp), 12)
  expect_true("respondent_id" %in% names(resp))
})

test_that("the simulated responses stay inside the declared encodings", {
  inst <- mcdm_fixture()
  resp <- mcdm_responses()
  cols <- sframe_item_expansion_columns(inst)

  # Saaty is a signed integer in {-9..-2, 1, 2..9} and never a fraction
  saaty <- unlist(resp[, grep("^crit_pairs__", cols, value = TRUE)])
  expect_true(all(saaty %in% c(-9:-2, 1, 2:9)))

  # influence is 0 to 4
  infl <- unlist(resp[, grep("^crit_influence__", cols, value = TRUE)])
  expect_true(all(infl >= 0 & infl <= 4))

  # every constant-sum row totals exactly 100
  cw <- resp[, grep("^crit_points__", cols, value = TRUE)]
  expect_true(all(rowSums(cw) == 100))

  # rated cells sit on the declared 1 to 5 choice set
  rated <- unlist(resp[, grep("^rate_", cols, value = TRUE)])
  expect_true(all(rated >= 1 & rated <= 5))
})

test_that("gate 6: all 4 plan blocks return a table and a chart with no error", {
  skip_if_not_installed("ggplot2")
  inst <- mcdm_fixture()
  resp <- mcdm_responses()

  res <- run_analysis_plan(resp, inst, plots = TRUE)
  expect_length(res, 4)

  for (r in res) {
    expect_null(r$error, info = paste(r$block_id, "returned an error"))
    expect_false(is.null(r$table), info = paste(r$block_id, "has no table"))
    expect_s3_class(r$plot, "ggplot")
  }

  expect_equal(unname(vapply(res, function(r) r$test, character(1))),
               c("ahp", "topsis", "topsis", "dematel"))
})

test_that("the 3 weight sources reach the paths the example is meant to show", {
  inst <- mcdm_fixture()
  resp <- mcdm_responses()
  res <- run_analysis_plan(resp, inst)
  by_id <- function(id) Filter(function(r) identical(r$block_id, id), res)[[1]]

  # RQ1 derives weights from the collected Saaty matrix
  ahp <- by_id("RQ1")
  expect_equal(length(ahp$weights), 4)
  expect_equal(sum(ahp$weights), 1, tolerance = 1e-6)

  # RQ2 is the hybrid: researcher-supplied matrix, collected weights
  rq2 <- by_id("RQ2")
  expect_equal(nrow(rq2$table), 5)

  # RQ3 builds its matrix from the rated items instead
  rq3 <- by_id("RQ3")
  expect_equal(nrow(rq3$table), 5)

  # the 2 TOPSIS blocks read different weight sources and different
  # matrices, so identical scores would mean one path was not being used
  expect_false(identical(round(rq2$table$Score, 6),
                         round(rq3$table$Score, 6)))
})

test_that("AHP consistency is reported and the simulated judgements are usable", {
  inst <- mcdm_fixture()
  resp <- mcdm_responses()
  ahp <- Filter(function(r) identical(r$test, "ahp"),
                run_analysis_plan(resp, inst))[[1]]

  # a consistency ratio must be reported for a reviewer to see
  expect_false(is.null(ahp$cr))
  expect_true(is.finite(ahp$cr))
  # the fixture is drawn around a true weight vector, so it should not be
  # wildly inconsistent
  expect_lt(ahp$cr, 0.20)
})
