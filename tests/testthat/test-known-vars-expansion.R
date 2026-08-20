# tests/testthat/test-known-vars-expansion.R
# validate_sframe()'s known_vars must recognise the same expansion columns
# read_responses() accepts (item__sub, item__option, item__a__vs__b,
# item__crit). It used to hold only the base item and scale ids, so a real
# builder export failed design-time validation with "references unknown
# variable(s)" naming columns that do exist.

vendors <- c("Alpha", "Basilica", "Coral")
crits   <- c("service", "price")

expansion_instrument <- function(plan) {
  sf_instrument(
    title   = "Expansion columns",
    version = "1.0.0",
    components = list(
      sf_choices("ag5", values = 1:5,
                 labels = c("Very poor", "Poor", "Fair", "Good", "Excellent")),
      sf_item("rate_service", "Rate each supplier: service",
              type = "matrix", matrix_items = vendors, choice_set = "ag5"),
      sf_item("pick_one", "Which do you use?",
              type = "multiple_choice", choice_set = "ag5"),
      sf_item("crit_pairs", "Compare the criteria",
              type = "pairwise_comparison", comparison_items = crits,
              comparison_scale = "saaty", required = TRUE),
      sf_item("crit_points", "Divide 100 points",
              type = "criteria_weight", comparison_items = crits,
              required = TRUE)
    ),
    analysis_plan = plan
  )
}

test_that("a matrix sub-item column validates as a known variable", {
  inst <- expansion_instrument(list(list(
    id = "RQ1", research_question = "Does service rating vary?",
    family = "descriptive", method = "frequencies",
    roles = list(variables = "rate_service__Alpha")
  )))

  res <- validate_sframe(inst, strict = FALSE)
  expect_true(res$valid)
  expect_length(res$problems, 0)
})

test_that("decision expansion columns validate as known variables", {
  inst <- expansion_instrument(list(list(
    id = "RQ2", research_question = "How do the pair judgements look?",
    family = "descriptive", method = "frequencies",
    roles = list(variables = c("crit_pairs__service__vs__price",
                               "crit_points__service"))
  )))

  res <- validate_sframe(inst, strict = FALSE)
  expect_true(res$valid)
  expect_length(res$problems, 0)
})

test_that("a genuinely unknown variable is still rejected", {
  inst <- expansion_instrument(list(list(
    id = "RQ3", research_question = "Nonsense reference",
    family = "descriptive", method = "frequencies",
    roles = list(variables = "rate_service__NotAVendor")
  )))

  res <- validate_sframe(inst, strict = FALSE)
  expect_false(res$valid)
  expect_true(any(grepl("unknown variable", res$problems)))
})

test_that("validate_sframe and read_responses agree on the accepted columns", {
  inst <- expansion_instrument(list())

  known <- sframe_item_expansion_columns(inst)

  # every expansion column the helper produces must be one read_responses()
  # tolerates without flagging it undeclared
  dat <- as.data.frame(stats::setNames(
    lapply(known, function(x) rep(1, 3)), known
  ))
  dat$respondent_id <- paste0("r", 1:3)

  expect_silent(
    suppressWarnings(read_responses(dat, inst, strict = TRUE,
                                    respondent_id = "respondent_id"))
  )
})

test_that("the helper covers all 4 expansion families", {
  inst  <- expansion_instrument(list())
  known <- sframe_item_expansion_columns(inst)

  expect_true("rate_service__Alpha" %in% known)              # matrix sub-item
  expect_true("pick_one__1" %in% known)                      # choice option
  expect_true("crit_pairs__service__vs__price" %in% known)   # Saaty pair
  expect_true("crit_points__service" %in% known)             # criterion weight
})
