# tests/testthat/test-accessors.R
# The accessor and exploration surface added in 0.4.0, and the diagnostic
# return type of validate_sframe() and validate_model().
#
# Both were raised by a statistical-software editor reading the code: the
# classes carried print, summary and format only, so user code had to reach
# into internals with `$`, and a validator returned the object it was handed
# rather than a diagnostic.

acc_instrument <- function() {
  cs <- sf_choices("ag5", 1:5,
    c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
  yn <- sf_choices("yn", c("yes", "no"), c("Yes", "No"))
  i1 <- sf_item("sat_1", "The service was fast.", type = "likert",
                choice_set = "ag5", scale_id = "sat", required = TRUE)
  i2 <- sf_item("sat_2", "The staff were helpful.", type = "likert",
                choice_set = "ag5", scale_id = "sat")
  gen <- sf_item("gender", "Gender?", type = "single_choice", choice_set = "yn")
  sc <- sf_scale("sat", "Satisfaction", items = c("sat_1", "sat_2"))
  chk <- sf_check("chk_1", item_id = "sat_1", type = "attention",
                  pass_values = 4, fail_action = "flag")
  rule <- sf_branch("sat_2", depends_on = "gender", operator = "==",
                    value = "yes", action = "show")
  sf_instrument(
    title = "Accessor Survey", version = "2.0.0",
    components = list(cs, yn, i1, i2, gen, sc, chk, rule),
    analysis_plan = list(
      list(id = "RQ1", research_question = "Is the scale reliable?",
           family = "measurement", method = "reliability_alpha",
           roles = list(items = c("sat_1", "sat_2")))
    )
  )
}

# ---------------------------------------------------------------------------
# Component accessors
# ---------------------------------------------------------------------------

test_that("component accessors return named component lists", {
  instr <- acc_instrument()

  expect_s3_class(sf_items(instr), "sf_component_list")
  expect_length(sf_items(instr), 3)
  expect_named(sf_items(instr), c("sat_1", "sat_2", "gender"))
  expect_s3_class(sf_items(instr)[["sat_1"]], "sf_item")

  expect_length(sf_scales(instr), 1)
  expect_s3_class(sf_scales(instr)[["sat"]], "sf_scale")
  expect_length(sf_choice_sets(instr), 2)
  expect_length(sf_branches(instr), 1)
  expect_length(sf_checks(instr), 1)
  expect_length(sf_models(instr), 0)
})

test_that("sf_meta() and sf_plan() reach the metadata and the plan", {
  instr <- acc_instrument()
  expect_equal(sf_meta(instr)$title, "Accessor Survey")
  expect_equal(sf_meta(instr)$version, "2.0.0")
  expect_length(sf_plan(instr), 1)
  expect_equal(sf_plan(instr)[[1]]$id, "RQ1")
})

test_that("subsetting a component list keeps the class", {
  instr <- acc_instrument()
  subset <- sf_items(instr)[1:2]
  expect_s3_class(subset, "sf_component_list")
  expect_length(subset, 2)
})

test_that("component lists print one line per component", {
  instr <- acc_instrument()
  out <- capture.output(print(sf_items(instr)))
  expect_match(out[1], "item list: 3")
  expect_length(out, 4)
})

test_that("sf_id() and sf_label() work across the component classes", {
  instr <- acc_instrument()
  expect_equal(sf_id(sf_items(instr)[["sat_1"]]), "sat_1")
  expect_equal(sf_label(sf_items(instr)[["sat_1"]]), "The service was fast.")
  expect_equal(sf_id(sf_scales(instr)[["sat"]]), "sat")
  expect_equal(sf_label(sf_scales(instr)[["sat"]]), "Satisfaction")
  expect_equal(sf_id(sf_choice_sets(instr)[["ag5"]]), "ag5")
  expect_equal(sf_id(sf_checks(instr)[["chk_1"]]), "chk_1")
})

# ---------------------------------------------------------------------------
# as.data.frame()
# ---------------------------------------------------------------------------

test_that("as.data.frame() on an instrument gives the item table", {
  instr <- acc_instrument()
  df <- as.data.frame(instr)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3)
  expect_true(all(c("id", "label", "type", "choice_set", "scale_id",
                    "reverse", "required") %in% names(df)))
  expect_equal(df$id, c("sat_1", "sat_2", "gender"))
  expect_true(df$required[1])
})

test_that("the instrument table and the codebook table agree", {
  # Both read the shared builders, so a change to one cannot silently leave
  # the other behind.
  instr <- acc_instrument()
  expect_identical(as.data.frame(instr), sf_items(codebook_report(instr)))
})

test_that("as.data.frame() on a choice set gives value and label", {
  cs <- sf_choices("ag5", 1:5, c("SD", "D", "N", "A", "SA"))
  df <- as.data.frame(cs)
  expect_equal(nrow(df), 5)
  expect_equal(df$value, as.character(1:5))
  expect_equal(df$label[5], "SA")
})

test_that("codebook accessors return the tables they hold", {
  cb <- codebook_report(acc_instrument())
  expect_s3_class(sf_items(cb), "data.frame")
  expect_equal(nrow(sf_scales(cb)), 1)
  expect_equal(nrow(sf_plan(cb)), 1)
  expect_equal(sf_meta(cb)$title, "Accessor Survey")
  expect_identical(as.data.frame(cb), sf_items(cb))
})

test_that("as.data.frame() works on the psychometric reports", {
  instr <- acc_instrument()
  set.seed(1)
  data <- data.frame(
    sat_1 = sample(1:5, 40, replace = TRUE),
    sat_2 = sample(1:5, 40, replace = TRUE)
  )
  skip_if_not_installed("psych")

  rel <- reliability_report(data, instr)
  rel_df <- as.data.frame(rel)
  expect_s3_class(rel_df, "data.frame")
  expect_equal(rel_df$scale_id, "sat")
  expect_equal(rel_df$n_items, 2L)
  expect_true(is.numeric(rel_df$alpha))
  # A 2-item scale gets no omega, and the row must still appear with NA
  # rather than being dropped from the table.
  expect_true(is.na(rel_df$omega_t))

  items <- item_report(data, instr)
  item_df <- as.data.frame(items)
  expect_equal(nrow(item_df), 2)
  expect_true("scale_id" %in% names(item_df))
  expect_equal(unique(item_df$scale_id), "sat")
})

test_that("as.data.frame() gives the quality report summary row", {
  demo <- sframe_demo_data()
  qr <- quality_report(demo$responses, demo$instrument)
  df <- as.data.frame(qr)
  expect_equal(nrow(df), 1)
  expect_equal(df$n_respondents, nrow(demo$responses))
  expect_true(df$flag_rate >= 0 && df$flag_rate <= 1)
})

test_that("sf_flagged() returns the flagged rows the summary counted", {
  demo <- sframe_demo_data()
  qr <- quality_report(demo$responses, demo$instrument)
  expect_equal(length(sf_flagged(qr)), as.data.frame(qr)$n_flagged)
})

test_that("as.data.frame() and sf_apa() work on analysis results", {
  demo <- sframe_demo_data()
  res <- run_analysis_plan(demo$responses, demo$instrument)
  df <- as.data.frame(res)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), length(res))
  expect_true(all(c("block_id", "research_question", "test", "apa") %in% names(df)))

  apa <- sf_apa(res)
  expect_length(apa, length(res))
  expect_named(apa, names(res))
})

test_that("subsetting a report keeps its class", {
  demo <- sframe_demo_data()
  res <- run_analysis_plan(demo$responses, demo$instrument)
  skip_if(length(res) < 2)
  subset <- res[1:2]
  expect_s3_class(subset, "sframe_analysis_results")
  expect_equal(nrow(as.data.frame(subset)), 2)
})

# ---------------------------------------------------------------------------
# validate_sframe() returns a diagnostic
# ---------------------------------------------------------------------------

test_that("validate_sframe() returns an sframe_validation from both branches", {
  instr <- acc_instrument()
  expect_s3_class(validate_sframe(instr, strict = TRUE), "sframe_validation")
  expect_s3_class(validate_sframe(instr, strict = FALSE), "sframe_validation")
})

test_that("the diagnostic prints the result rather than staying silent", {
  # The editor's specific ask: validate_sframe(instr) typed on its own must
  # show the user the diagnostic. Before 0.4.0 the valid case printed nothing.
  instr <- acc_instrument()
  out <- capture.output(print(validate_sframe(instr, strict = TRUE)))
  expect_match(out[1], "sframe validation")
  expect_true(any(grepl("valid", out)))
  expect_true(any(grepl("Accessor Survey", out)))
})

test_that("the diagnostic lists the failing problems", {
  broken <- sf_instrument("Broken", components = list(
    sf_item("q1", "Q", type = "likert", choice_set = "missing_cs")
  ))
  v <- validate_sframe(broken, strict = FALSE)
  expect_false(sf_is_valid(v))
  expect_length(sf_problems(v), 1)
  out <- capture.output(print(v))
  expect_true(any(grepl("1 problem", out)))
  expect_true(any(grepl("missing_cs", out)))
})

test_that("the check roster records checks that ran and found nothing", {
  # A diagnostic that lists only failures cannot distinguish a check that
  # passed from a check that never ran.
  instr <- acc_instrument()
  checks <- summary(validate_sframe(instr, strict = FALSE))
  expect_s3_class(checks, "data.frame")
  expect_equal(nrow(checks), length(surveyframe:::sframe_validation_checks))
  expect_true(all(checks$status == "ok"))
  expect_true("analysis_plan_variables" %in% checks$check)
})

test_that("as.data.frame() labels each problem with the check that raised it", {
  broken <- sf_instrument("Broken", components = list(
    sf_item("q1", "Q", type = "likert", choice_set = "missing_cs"),
    sf_item("q1", "Duplicate", type = "text")
  ))
  df <- as.data.frame(validate_sframe(broken, strict = FALSE))
  expect_true(all(c("check", "problem") %in% names(df)))
  expect_true("duplicate_item_ids" %in% df$check)
  expect_true("item_choice_set_refs" %in% df$check)
  expect_equal(nrow(df), length(sf_problems(validate_sframe(broken, strict = FALSE))))
})

test_that("strict mode still aborts on problems", {
  broken <- sf_instrument("Broken", components = list(
    sf_item("q1", "Q", type = "likert", choice_set = "missing_cs")
  ))
  expect_error(validate_sframe(broken, strict = TRUE),
               class = "sframe_validation_error")
})

test_that("$valid and $problems still work for code written before 0.4.0", {
  # The element names are kept deliberately so the widespread reading pattern
  # survives the change of return type.
  instr <- acc_instrument()
  v <- validate_sframe(instr, strict = FALSE)
  expect_true(v$valid)
  expect_length(v$problems, 0)
})

test_that("as_sframe() recovers the stamped instrument", {
  instr <- acc_instrument()
  expect_false(isTRUE(sf_meta(instr)$validated))
  validated <- as_sframe(validate_sframe(instr, strict = TRUE))
  expect_s3_class(validated, "sframe")
  expect_true(isTRUE(sf_meta(validated)$validated))
})

test_that("as_sframe() is the identity on an instrument", {
  instr <- acc_instrument()
  expect_identical(as_sframe(instr), instr)
})

test_that("passing a diagnostic where an instrument is wanted errors clearly", {
  v <- validate_sframe(acc_instrument(), strict = TRUE)
  expect_error(codebook_report(v), "as_sframe")
  expect_error(render_survey(v), "as_sframe")
})

# ---------------------------------------------------------------------------
# validate_model()
# ---------------------------------------------------------------------------

test_that("validate_model() returns the same diagnostic class", {
  model <- sf_model("m1", type = "cfa", engine = "lavaan",
                    constructs = list(sf_construct("c1", items = c("sat_1", "sat_2"))))
  v <- validate_model(model, strict = FALSE)
  expect_s3_class(v, "sframe_validation")
  expect_true(sf_is_valid(v))
  expect_equal(v$subject, "model")
  out <- capture.output(print(v))
  expect_true(any(grepl("Model:", out)))
})

test_that("a model diagnostic holds the model, not an instrument", {
  model <- sf_model("m1", type = "cfa", engine = "lavaan",
                    constructs = list(sf_construct("c1", items = c("sat_1", "sat_2"))))
  v <- validate_model(model, strict = FALSE)
  expect_equal(sf_object(v)$id, "m1")
  expect_error(as_sframe(v), "sf_object")
})

test_that("validate_model() still aborts in strict mode", {
  bad <- sf_model("m1", type = "pls_sem", engine = "seminr")
  expect_error(validate_model(bad, strict = TRUE),
               class = "sframe_validation_error")
})
