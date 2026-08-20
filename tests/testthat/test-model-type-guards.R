# tests/testthat/test-model-type-guards.R
# A syntax generator is specific to an estimation family. Generating PLS-SEM
# syntax from a covariance-based model, or lavaan syntax from a PLS-SEM model,
# produced a runnable script that silently estimated a different model from the
# one declared. The mismatch is now refused on both sides.

cb_model <- function() {
  sf_model(
    "cb_1", "CB-SEM model", type = "cb_sem",
    constructs = list(
      sf_construct("SAT", "Satisfaction", c("sat_1", "sat_2")),
      sf_construct("LOY", "Loyalty", c("loy_1", "loy_2"))
    ),
    paths = list(sf_path("SAT", "LOY"))
  )
}

pls_model <- function() {
  sf_model(
    "pls_1", "PLS-SEM model", type = "pls_sem",
    constructs = list(
      sf_construct("SAT", "Satisfaction", c("sat_1", "sat_2"),
                   mode = "composite"),
      sf_construct("LOY", "Loyalty", c("loy_1", "loy_2"),
                   mode = "composite")
    ),
    paths = list(sf_path("SAT", "LOY"))
  )
}

test_that("seminr_syntax() refuses a covariance-based model", {
  expect_error(seminr_syntax(cb_model()), class = "sframe_validation_error")
  expect_error(seminr_syntax(cb_model()), "cb_sem")
})

test_that("sem_lavaan_syntax() refuses a PLS-SEM model", {
  expect_error(sem_lavaan_syntax(pls_model()),
               class = "sframe_validation_error")
  expect_error(sem_lavaan_syntax(pls_model()), "pls_sem")
})

test_that("cfa_lavaan_syntax() refuses PLS-SEM, still allows model = NULL", {
  expect_error(cfa_lavaan_syntax(model = pls_model()),
               class = "sframe_validation_error")

  # deriving the measurement model from an instrument's scales is unaffected
  instr <- sf_instrument(
    title = "CFA from scales", version = "1.0.0",
    components = list(
      sf_choices("ag5", 1:5, c("SD", "D", "N", "A", "SA")),
      sf_item("sat_1", "Item 1", type = "likert", choice_set = "ag5",
              scale_id = "sat"),
      sf_item("sat_2", "Item 2", type = "likert", choice_set = "ag5",
              scale_id = "sat"),
      sf_scale("sat", "Satisfaction", items = c("sat_1", "sat_2"))
    )
  )
  expect_match(cfa_lavaan_syntax(instr), "sat =~", fixed = TRUE)
})

test_that("the matching generator still works for each family", {
  expect_match(seminr_syntax(pls_model()), "measurement_model <- constructs",
               fixed = TRUE)
  expect_match(sem_lavaan_syntax(cb_model()), "SAT =~", fixed = TRUE)
})

test_that("the error explains the consequence, not just the mismatch", {
  err <- tryCatch(seminr_syntax(cb_model()),
                  error = function(e) conditionMessage(e))
  expect_match(err, "seminr_syntax\\(\\)")
  expect_match(err, "partial-least-squares")
  expect_match(err, "different model")
})

test_that("a model with no declared type is refused rather than guessed at", {
  m <- cb_model()
  m$type <- NULL
  expect_error(seminr_syntax(m), class = "sframe_validation_error")
})

test_that("the demo instruments wire each syntax block to a fitting model", {
  # Both shipped demos pointed their seminr block at a cb_sem model, so the
  # demo data itself generated PLS-SEM syntax from a covariance-based model.
  # The guard above caught it. This pins the corrected wiring.
  demo <- sframe_demo_data()
  inst <- demo$instrument

  types <- vapply(inst$models, function(m) m$type %||% "", character(1))
  names(types) <- vapply(inst$models, function(m) m$id, character(1))

  expected <- c(cfa_lavaan_syntax = "cfa", sem_lavaan_syntax = "cb_sem",
                seminr_syntax = "pls_sem")

  for (block in inst$analysis_plan) {
    method <- block$method %||% ""
    if (!method %in% names(expected)) next
    model_id <- unlist(block$roles$model)[1]
    skip_if(is.null(model_id), paste("no model role on", method))
    if (identical(method, "seminr_syntax")) {
      expect_identical(unname(types[[model_id]]), "pls_sem",
                       info = paste(method, "must not read a",
                                    types[[model_id]], "model"))
    } else {
      expect_true(unname(types[[model_id]]) %in% c("cfa", "cb_sem"))
    }
  }
})

test_that("every syntax block in the demo plan actually generates syntax", {
  demo <- sframe_demo_data()
  res <- run_analysis_plan(demo$responses, demo$instrument, scored = FALSE)
  for (t in c("cfa_lavaan_syntax", "sem_lavaan_syntax", "seminr_syntax")) {
    blocks <- Filter(function(r) identical(r$test, t), res)
    skip_if(length(blocks) == 0, paste("demo plan has no", t, "block"))
    expect_true(nzchar(blocks[[1]]$syntax %||% ""))
    expect_null(blocks[[1]]$error)
  }
})
