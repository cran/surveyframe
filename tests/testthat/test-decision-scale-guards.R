# tests/testthat/test-decision-scale-guards.R
# The 2 comparison scales are not interchangeable: AHP and ANP read reciprocal
# relative importance on the Saaty ratio scale, DEMATEL reads a directed 0-4
# influence matrix, and a criterion-weight source must express importance
# rather than influence. Pairing them the wrong way round returns plausible
# numbers from meaningless input, so it is rejected at design time by
# validate_sframe() and again in the runners.

crits <- c("Service quality", "Price", "Delivery time")

scale_instrument <- function(plan) {
  sf_instrument(
    title   = "Comparison scale guards",
    version = "1.0.0",
    components = list(
      sf_item("pw_saaty", "Compare the criteria", type = "pairwise_comparison",
              comparison_items = crits, comparison_scale = "saaty",
              required = TRUE),
      sf_item("pw_infl", "Influence among the criteria",
              type = "pairwise_comparison", comparison_items = crits,
              comparison_scale = "influence", required = TRUE),
      sf_item("cw", "Divide 100 points", type = "criteria_weight",
              comparison_items = crits, required = TRUE)
    ),
    analysis_plan = plan
  )
}

test_that("a correctly paired decision plan validates clean", {
  inst <- scale_instrument(list(
    list(id = "RQ1", family = "decision", method = "ahp",
         research_question = "What weight does each criterion carry?",
         roles = list(pairwise = "pw_saaty")),
    list(id = "RQ2", family = "decision", method = "dematel",
         research_question = "Which criteria drive the others?",
         roles = list(pairwise = "pw_infl"))
  ))
  result <- validate_sframe(inst, strict = FALSE)
  expect_true(result$valid)
  expect_length(result$problems, 0)
})

test_that("validate_sframe rejects AHP and ANP on an influence item", {
  for (method in c("ahp", "anp")) {
    inst <- scale_instrument(list(
      list(id = "RQ1", family = "decision", method = method,
           research_question = "What weight does each criterion carry?",
           roles = list(pairwise = "pw_infl"))
    ))
    result <- validate_sframe(inst, strict = FALSE)
    expect_false(result$valid)
    expect_match(paste(result$problems, collapse = " "),
                 "influence", fixed = TRUE)
    expect_match(paste(result$problems, collapse = " "),
                 toupper(method), fixed = TRUE)
  }
})

test_that("validate_sframe rejects DEMATEL on a Saaty item", {
  inst <- scale_instrument(list(
    list(id = "RQ1", family = "decision", method = "dematel",
         research_question = "Which criteria drive the others?",
         roles = list(pairwise = "pw_saaty"))
  ))
  result <- validate_sframe(inst, strict = FALSE)
  expect_false(result$valid)
  expect_match(paste(result$problems, collapse = " "), "DEMATEL needs 'influence'",
               fixed = TRUE)
})

test_that("validate_sframe rejects an influence item as a weights source", {
  inst <- scale_instrument(list(
    list(id = "RQ1", family = "decision", method = "topsis",
         research_question = "Which supplier ranks best?",
         roles = list(weights_item = "pw_infl"),
         options = list(
           matrix = list(c(4.1, 3.0, 210), c(3.6, 4.5, 180), c(4.8, 2.5, 260)),
           alternatives = c("Alpha", "Basilica", "Coral"),
           criteria = crits,
           criteria_types = c("benefit", "cost", "cost")
         ))
  ))
  result <- validate_sframe(inst, strict = FALSE)
  expect_false(result$valid)
  expect_match(paste(result$problems, collapse = " "), "cannot supply weights",
               fixed = TRUE)
})

test_that("a criteria_weight item is accepted as a weights source", {
  inst <- scale_instrument(list(
    list(id = "RQ1", family = "decision", method = "topsis",
         research_question = "Which supplier ranks best?",
         roles = list(weights_item = "cw"),
         options = list(
           matrix = list(c(4.1, 3.0, 210), c(3.6, 4.5, 180), c(4.8, 2.5, 260)),
           alternatives = c("Alpha", "Basilica", "Coral"),
           criteria = crits,
           criteria_types = c("benefit", "cost", "cost")
         ))
  ))
  expect_true(validate_sframe(inst, strict = FALSE)$valid)
})

test_that("strict validation aborts on a scale mismatch", {
  inst <- scale_instrument(list(
    list(id = "RQ1", family = "decision", method = "ahp",
         research_question = "What weight does each criterion carry?",
         roles = list(pairwise = "pw_infl"))
  ))
  expect_error(validate_sframe(inst), "influence")
})

test_that("the AHP runner errors rather than computing on influence data", {
  inst <- scale_instrument(list(
    list(id = "RQ1", family = "decision", method = "ahp",
         research_question = "What weight does each criterion carry?",
         roles = list(pairwise = "pw_saaty"))
  ))
  pairs <- expand.grid(a = crits, b = crits, stringsAsFactors = FALSE)
  pairs <- pairs[pairs$a != pairs$b, , drop = FALSE]
  cols <- paste0("pw_infl__", pairs$a, "__to__", pairs$b)
  set.seed(42)
  data <- as.data.frame(
    stats::setNames(lapply(cols, function(x) sample(0:4, 5, replace = TRUE)), cols)
  )
  result <- sframe_run_ahp(data, list(pairwise = "pw_infl"), list(), inst)
  expect_false(is.null(result$error))
  expect_match(result$error, "Saaty", fixed = TRUE)
  expect_null(result$weights)
})

test_that("the DEMATEL runner still errors on a Saaty item", {
  inst <- scale_instrument(list(
    list(id = "RQ1", family = "decision", method = "dematel",
         research_question = "Which criteria drive the others?",
         roles = list(pairwise = "pw_infl"))
  ))
  pairs <- utils::combn(crits, 2)
  cols <- paste0("pw_saaty__", pairs[1, ], "__vs__", pairs[2, ])
  set.seed(42)
  data <- as.data.frame(
    stats::setNames(lapply(cols, function(x) sample(c(-5, -3, 1, 3, 5), 5, replace = TRUE)), cols)
  )
  result <- sframe_run_dematel(data, list(pairwise = "pw_saaty"), list(), inst)
  expect_false(is.null(result$error))
  expect_match(result$error, "influence", fixed = TRUE)
})
