# The blockers a second review pass raised against the 0.4.2 candidate. Four
# were regressions or gaps in the fixes made earlier in this release, so each
# gets a test that fails on the code as it stood.
#
# 1. The field-shape gate returned before the strict abort and before the
#    validated stamp was set, so strict = TRUE carried on and the instrument it
#    returned still said it had been validated. write_sframe() then wrote it.
#    This one was introduced by the shape gate itself.
# 2. The scoring guard checked the declaration and never the data, so a scale
#    whose ID matched a collected column replaced that column with scores.
# 3. Criterion weights were aligned only where the criteria were known when
#    options were normalised. A collected performance matrix is built after
#    that, so named weights were applied by position.
# 4. The amendment boundary compared each entry's new_hash, which covers the
#    content an amendment produced and nothing about the disclosure, so an
#    entry's author or reason could be rewritten afterwards.
# 6. A numeric ID passed the scalar guard and failed later in vapply(); a
#    component that was no list failed on `$`.
# 7. A malformed plan block was diagnosed and then dereferenced; an empty role
#    list counted as an assignment.

simple_instrument <- function() {
  sf_instrument("Pass", components = list(
    sf_item("q1", "A label", type = "numeric")))
}

# ── 1. every validation exit honours strict mode and the stamp ────────────

test_that("1: strict mode aborts on a malformed field", {
  instr <- as_sframe(validate_sframe(simple_instrument(), strict = TRUE))
  instr$items[[1]]$label <- NA_character_
  expect_error(validate_sframe(instr, strict = TRUE),
               class = "sframe_validation_error")
})

test_that("1: a malformed field clears a stamp the instrument already carried", {
  instr <- as_sframe(validate_sframe(simple_instrument(), strict = TRUE))
  expect_true(isTRUE(sf_meta(instr)$validated))

  instr$items[[1]]$label <- NA_character_
  res <- validate_sframe(instr, strict = FALSE)
  expect_false(res$valid)
  expect_false(isTRUE(sf_meta(sf_object(res))$validated))
})

test_that("1: the writer refuses an instrument that failed the shape gate", {
  instr <- as_sframe(validate_sframe(simple_instrument(), strict = TRUE))
  instr$items[[1]]$label <- NA_character_
  res <- validate_sframe(instr, strict = FALSE)

  path <- tempfile(fileext = ".sframe")
  expect_error(write_sframe(sf_object(res), path))
  expect_false(file.exists(path))
})

# ── 2. scoring never replaces a column the responses already hold ─────────

scale_instrument <- function(scale_id = "site") {
  sf_instrument("Scored", components = list(
    sf_choices("ag5", 1:5, as.character(1:5)),
    sf_item("q1", "A", "likert", choice_set = "ag5", scale_id = scale_id),
    sf_item("q2", "B", "likert", choice_set = "ag5", scale_id = scale_id),
    sf_item("q3", "C", "likert", choice_set = "ag5", scale_id = scale_id),
    sf_scale(scale_id, "Scale", items = c("q1", "q2", "q3"))))
}

test_that("2: a scale ID matching a collected column is refused", {
  d <- data.frame(site = c("north", "south"), q1 = c(4, 2), q2 = c(5, 1),
                  q3 = c(3, 2), stringsAsFactors = FALSE)
  expect_error(score_scales(d, scale_instrument("site")),
               class = "sframe_validation_error")
})

test_that("2: the metadata the collision would have replaced is still there", {
  d <- data.frame(branch = c("north", "south"), q1 = c(4, 2), q2 = c(5, 1),
                  q3 = c(3, 2), stringsAsFactors = FALSE)
  out <- score_scales(d, scale_instrument("site"), keep_items = TRUE,
                      keep_meta = TRUE)
  expect_equal(out$branch, c("north", "south"))
  expect_false(is.null(out$site))
})

test_that("2: re-scoring an already-scored frame is refused, not silent", {
  d <- data.frame(q1 = c(4, 2), q2 = c(5, 1), q3 = c(3, 2))
  once <- score_scales(d, scale_instrument("site"), keep_items = TRUE)
  expect_false(is.null(once$site))
  # the score column is now in the data, so a second pass would replace it
  expect_error(score_scales(once, scale_instrument("site")),
               class = "sframe_validation_error")
})

# ── 3. weights align to the resolved matrix, however it was built ─────────

rated_instrument <- function() {
  sf_instrument("MCDA", components = list(
    sf_choices("r5", 1:5, as.character(1:5)),
    sf_item("price", "Rate the price", "matrix",
            matrix_items = c("A", "B"), choice_set = "r5"),
    sf_item("quality", "Rate the quality", "matrix",
            matrix_items = c("A", "B"), choice_set = "r5")))
}

rated_data <- function() {
  data.frame(price__A = c(5, 5), price__B = c(1, 1),
             quality__A = c(1, 1), quality__B = c(5, 5))
}

test_that("3: named weights align to a collected performance matrix", {
  res <- surveyframe:::sframe_resolve_decision_inputs(
    rated_data(), list(performance_items = c("price", "quality")),
    list(weights = c(quality = 0.9, price = 0.1)),
    rated_instrument(), method = "TOPSIS")

  expect_equal(res$criteria, c("price", "quality"))
  # the weight follows its own criterion, where it used to follow its position
  expect_equal(unname(res$weights[["price"]]), 0.1)
  expect_equal(unname(res$weights[["quality"]]), 0.9)
})

test_that("3: named criterion directions align the same way", {
  res <- surveyframe:::sframe_resolve_decision_inputs(
    rated_data(), list(performance_items = c("price", "quality")),
    list(weights = c(price = 0.5, quality = 0.5),
         criteria_types = c(quality = "benefit", price = "cost")),
    rated_instrument(), method = "TOPSIS")
  expect_equal(unname(res$criteria_types[["price"]]), "cost")
  expect_equal(unname(res$criteria_types[["quality"]]), "benefit")
})

test_that("3: an unnamed weight vector is still taken in criterion order", {
  res <- surveyframe:::sframe_resolve_decision_inputs(
    rated_data(), list(performance_items = c("price", "quality")),
    list(weights = c(0.3, 0.7)), rated_instrument(), method = "TOPSIS")
  expect_equal(unname(res$weights[["price"]]), 0.3)
  expect_equal(unname(res$weights[["quality"]]), 0.7)
})

# ── 4. a recorded amendment stays as it was written ───────────────────────

amended_file <- function() {
  instr <- sf_instrument("Amend", components = list(
    sf_item("q1", "A", type = "numeric")))
  path <- tempfile(fileext = ".sframe")
  write_sframe(instr, path)
  prev <- read_sframe(path)
  changed <- prev
  changed$items[[1]]$label <- "A changed"
  amended <- amend_sframe(prev, changed, reason_code = "instrument_revision",
                          reason_text = "clarity", author = "Alice",
                          deviation_report = "Label reworded for clarity.")
  write_sframe(amended, path, overwrite = TRUE, new_instrument = FALSE)
  path
}

test_that("4: rewriting a recorded amendment's author is refused", {
  path <- amended_file()
  loaded <- read_sframe(path)
  expect_equal(loaded$amendments[[1]]$author, "Alice")

  loaded$amendments[[1]]$author <- "Someone Else"
  expect_error(write_sframe(loaded, path, overwrite = TRUE))
  # and the file still says what it said
  expect_equal(read_sframe(path)$amendments[[1]]$author, "Alice")
})

test_that("4: rewriting a recorded reason is refused too", {
  path <- amended_file()
  loaded <- read_sframe(path)
  loaded$amendments[[1]]$reason_text <- "a different reason"
  expect_error(write_sframe(loaded, path, overwrite = TRUE))
})

test_that("4: an untouched log still writes", {
  path <- amended_file()
  loaded <- read_sframe(path)
  expect_no_error(write_sframe(loaded, path, overwrite = TRUE))
})

# ── 6 and 7. malformed fields and blocks are diagnosed, never raised ──────

test_that("6: a numeric ID is a reported problem", {
  instr <- simple_instrument()
  instr$items[[1]]$id <- 123
  res <- expect_no_error(validate_sframe(instr, strict = FALSE))
  expect_false(res$valid)
  expect_match(paste(res$problems, collapse = " "), "must be text", fixed = TRUE)
})

test_that("6: a component that is no list is a reported problem", {
  for (slot in c("items", "choices", "scales")) {
    instr <- simple_instrument()
    instr[[slot]] <- list("not a component")
    res <- expect_no_error(validate_sframe(instr, strict = FALSE))
    expect_false(res$valid)
    expect_match(paste(res$problems, collapse = " "), "must be a list of fields",
                 fixed = TRUE)
  }
})

test_that("7: a plan block that is no list is diagnosed, never dereferenced", {
  instr <- simple_instrument()
  instr$analysis_plan <- list("nonsense")
  res <- expect_no_error(validate_sframe(instr, strict = FALSE))
  expect_false(res$valid)
})

test_that("7: an empty role list assigns nothing", {
  instr <- simple_instrument()
  instr$analysis_plan <- list(list(id = "RQ1", method = "descriptives",
                                   roles = list()))
  res <- expect_no_error(validate_sframe(instr, strict = FALSE))
  expect_false(res$valid)
  expect_match(paste(res$problems, collapse = " "), "assigns no variables",
               fixed = TRUE)
})

test_that("7: a block with real roles still validates", {
  instr <- simple_instrument()
  instr$analysis_plan <- list(list(id = "RQ1", method = "descriptives",
                                   roles = list(variables = "q1")))
  expect_true(validate_sframe(instr, strict = FALSE)$valid)
})
