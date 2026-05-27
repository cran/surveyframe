make_v03_instrument <- function(reverse = FALSE) {
  cs <- sf_choices("ag5", 1:5,
    c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
  yn <- sf_choices("yn", c("yes", "no"), c("Yes", "No"))
  i1 <- sf_item("sat_1", "The service was fast.",
    type = "likert", choice_set = "ag5", scale_id = "sat", required = TRUE)
  i2 <- sf_item("sat_2", "The staff were helpful.",
    type = "likert", choice_set = "ag5", scale_id = "sat", required = TRUE)
  i3 <- sf_item("sat_3", "I would recommend this service.",
    type = "likert", choice_set = "ag5", scale_id = "sat",
    required = TRUE, reverse = reverse)
  age <- sf_item("age", "What is your age?", type = "numeric")
  gen <- sf_item("gender", "Gender?", type = "single_choice", choice_set = "yn")
  attn <- sf_item("attn_q", "Please select Agree (4).",
    type = "likert", choice_set = "ag5")
  scale <- sf_scale("sat", "Satisfaction",
    items = c("sat_1", "sat_2", "sat_3"), method = "mean", min_valid = 2L)
  chk <- sf_check("chk_1", item_id = "attn_q", type = "attention",
    pass_values = 4, fail_action = "flag")
  sf_instrument(
    title = "Service Quality Survey",
    version = "1.0.0",
    components = list(cs, yn, i1, i2, i3, age, gen, attn, scale, chk)
  )
}

make_v03_responses <- function(n = 50, seed = 42) {
  set.seed(seed)
  data.frame(
    id = paste0("R", seq_len(n)),
    submitted_at = as.character(Sys.time() - sample(100:3600, n, replace = TRUE)),
    sat_1 = sample(1:5, n, replace = TRUE),
    sat_2 = sample(1:5, n, replace = TRUE),
    sat_3 = sample(1:5, n, replace = TRUE),
    age = sample(18:65, n, replace = TRUE),
    gender = sample(c("yes", "no"), n, replace = TRUE),
    attn_q = c(rep(4, floor(n * 0.85)), rep(2, ceiling(n * 0.15))),
    stringsAsFactors = FALSE
  )
}

test_that("descriptives_report computes core summaries and groups", {
  dat <- data.frame(
    group = c("a", "a", "b", "b"),
    x = c(1, 2, 3, NA),
    y = c(2, 4, 6, 8)
  )

  dr <- descriptives_report(dat, variables = c("x", "y"), split_by = "group")
  expect_s3_class(dr, "sframe_descriptives_report")
  expect_true(all(c("valid_n", "missing_n", "mean", "sd", "median", "iqr",
                    "skewness", "kurtosis", "se", "ci_low", "ci_high")
                  %in% names(dr$table)))
  expect_equal(dr$table$missing_n[dr$table$variable == "x" & dr$table$group == "b"], 1)
})

test_that("missing_data_report returns item, respondent, pattern, and scale rules", {
  instr <- make_v03_instrument()
  dat <- make_v03_responses(5)
  dat$sat_1[1:2] <- NA
  dat$sat_2[2] <- NA

  mr <- missing_data_report(dat, instr)
  expect_s3_class(mr, "sframe_missing_data_report")
  expect_true(all(c("item_missing", "respondent_missing", "patterns",
                    "deletion", "scale_missing_rules", "mcar") %in% names(mr)))
  expect_equal(mr$item_missing$missing_n[mr$item_missing$variable == "sat_1"], 2)
  expect_true(nrow(mr$patterns) >= 1)
})

test_that("outlier_report flags univariate and multivariate outliers", {
  dat <- data.frame(
    x = c(rnorm(20), 99),
    y = c(rnorm(20), 99)
  )

  zr <- outlier_report(dat, variables = c("x", "y"), method = "zscore")
  expect_s3_class(zr, "sframe_outlier_report")
  expect_true(21 %in% zr$flagged_rows)

  iqr <- outlier_report(dat, variables = "x", method = "iqr")
  expect_true(21 %in% iqr$flagged_rows)

  mah <- outlier_report(dat, variables = c("x", "y"), method = "mahalanobis")
  expect_s3_class(mah, "sframe_outlier_report")
  expect_true("statistic" %in% names(mah$table))
})

test_that("assumption_report includes Shapiro and expected-count warnings", {
  dat <- data.frame(
    x = c(1, 2, 3, 4, 5, 6),
    y = c(2, 3, 4, 5, 6, 7),
    g = c("a", "a", "a", "b", "b", "b"),
    h = c("yes", "yes", "no", "yes", "no", "no")
  )

  ar <- assumption_report(
    dat,
    variables = "x",
    group = "g",
    outcome = "y",
    predictors = "x",
    table_vars = c("g", "h")
  )
  expect_s3_class(ar, "sframe_assumption_report")
  expect_true("shapiro_w" %in% names(ar$normality))
  expect_true(is.list(ar$expected_counts))
})

test_that("posthoc_report returns pairwise Wilcoxon and chi-square residuals", {
  dat <- data.frame(
    y = c(1, 2, 2, 3, 4, 5, 5, 6),
    g = rep(c("a", "b"), each = 4),
    x = rep(c("yes", "no"), 4),
    z = rep(c("low", "high"), each = 4)
  )

  ph <- posthoc_report(dat, method = "kruskal_wallis", outcome = "y", group = "g")
  expect_s3_class(ph, "sframe_posthoc_report")
  expect_true("pairwise_wilcox" %in% names(ph$tables))

  chi <- suppressWarnings(
    posthoc_report(dat, method = "chi_square", table_vars = c("x", "z"))
  )
  expect_true("adjusted_residuals" %in% names(chi$tables))
})

test_that("run_analysis_plan supports new related-sample and association methods", {
  instr <- make_v03_instrument()
  instr$analysis_plan <- list(
    list(
      id = "rq1",
      research_question = "Are two ordinal items associated?",
      method = "correlation_kendall",
      roles = list(x = "sat_1", y = "sat_2"),
      options = list(alpha = 0.05)
    ),
    list(
      id = "rq2",
      research_question = "Do repeated ratings differ?",
      method = "friedman",
      roles = list(measures = c("sat_1", "sat_2", "sat_3")),
      options = list(alpha = 0.05)
    )
  )
  res <- run_analysis_plan(make_v03_responses(30), instr, scored = FALSE)
  expect_s3_class(res, "sframe_analysis_results")
  expect_equal(res[[1]]$test, "correlation_kendall")
  expect_null(res[[1]]$error)
  expect_equal(res[[2]]$test, "friedman")
  expect_null(res[[2]]$error)
})

test_that("run_analysis_plan supports Fisher exact and McNemar", {
  instr <- make_v03_instrument()
  instr$analysis_plan <- list(
    list(
      id = "rq1",
      research_question = "Are categories associated?",
      method = "fisher_exact",
      roles = list(row = "gender", column = "attn_q"),
      options = list(alpha = 0.05)
    ),
    list(
      id = "rq2",
      research_question = "Did paired binary choices change?",
      method = "mcnemar",
      roles = list(before = "gender", after = "gender2"),
      options = list(alpha = 0.05)
    )
  )
  dat <- make_v03_responses(20)
  dat$gender2 <- sample(c("yes", "no"), 20, replace = TRUE)
  instr$items <- c(instr$items, list(sf_item("gender2", "Gender 2", type = "single_choice")))
  res <- run_analysis_plan(dat, instr, scored = FALSE)
  expect_null(res[[1]]$error)
  expect_null(res[[2]]$error)
})

test_that("analysis plan supports simple case weights for frequency and crosstab", {
  instr <- make_v03_instrument()
  instr$analysis_plan <- list(
    list(
      id = "rq1",
      research_question = "What is the weighted gender distribution?",
      method = "frequency",
      roles = list(variable = "gender"),
      options = list(weights = "case_weight")
    ),
    list(
      id = "rq2",
      research_question = "What is the weighted association?",
      method = "crosstab",
      roles = list(row = "gender", column = "attn_q"),
      options = list(weights = "case_weight")
    )
  )
  dat <- make_v03_responses(20)
  dat$case_weight <- rep(c(0.5, 1.5), length.out = nrow(dat))
  res <- run_analysis_plan(dat, instr, scored = FALSE)
  expect_true(is.data.frame(res[[1]]$weighted_table))
  expect_true(is.data.frame(res[[2]]$weighted_table))
})

test_that("model layer constructs, validates, and generates syntax", {
  instr <- make_v03_instrument()
  con <- sf_construct("SAT", "Satisfaction", c("sat_1", "sat_2", "sat_3"))
  model <- sf_model(
    "model_1",
    "Satisfaction model",
    type = "cb_sem",
    constructs = list(con),
    paths = list(),
    covariances = list()
  )

  expect_s3_class(con, "sf_construct")
  expect_s3_class(model, "sf_model")
  expect_true(validate_model(model, instr, strict = FALSE)$valid)
  expect_match(model_json(model), '"model_1"', fixed = TRUE)

  cfa <- cfa_lavaan_syntax(instr)
  expect_match(cfa, "sat =~", fixed = TRUE)

  sem <- sem_lavaan_syntax(model, instr)
  expect_match(sem, "SAT =~", fixed = TRUE)

  pls <- sf_model(
    "pls_1",
    "PLS model",
    type = "pls_sem",
    constructs = list(
      sf_construct("SAT", "Satisfaction", c("sat_1", "sat_2"), mode = "composite"),
      sf_construct("LOY", "Loyalty", "sat_3", mode = "single_item")
    ),
    paths = list(sf_path("SAT", "LOY"))
  )
  expect_match(seminr_syntax(pls), "measurement_model <- constructs", fixed = TRUE)

  pls_without_path <- sf_model(
    "pls_bad",
    type = "pls_sem",
    constructs = list(sf_construct("SAT", items = c("sat_1", "sat_2"), mode = "composite"))
  )
  expect_false(validate_model(pls_without_path, strict = FALSE)$valid)
})

test_that("model layer constructors reject invalid identifiers", {
  expect_error(sf_construct("bad id", items = "sat_1"),
               class = "sframe_validation_error")
  expect_error(sf_construct("SAT", items = "bad item"),
               class = "sframe_validation_error")
  expect_error(sf_path("bad id", "SAT"),
               class = "sframe_validation_error")
  expect_error(sf_covariance("SAT", "bad-id"),
               class = "sframe_validation_error")
  expect_error(sf_indirect("SAT", c("MED", NA_character_), "LOY"),
               class = "sframe_validation_error")
  expect_error(sf_model("bad model", type = "cfa"),
               class = "sframe_validation_error")
})

test_that("add_model and .sframe read/write preserve models and old files work", {
  instr <- add_model(
    make_v03_instrument(),
    sf_model(
      "model_1",
      type = "cfa",
      constructs = list(sf_construct("SAT", items = c("sat_1", "sat_2", "sat_3")))
    )
  )
  path <- tempfile(fileext = ".sframe")
  write_sframe(instr, path, overwrite = TRUE)
  loaded <- read_sframe(path)
  expect_length(loaded$models, 1)
  expect_equal(loaded$models[[1]]$id, "model_1")

  payload <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  payload$models <- NULL
  payload$hash$value <- ""
  payload$hash$value <- surveyframe:::sframe_hash_payload(payload)
  old_path <- tempfile(fileext = ".sframe")
  writeLines(jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null"), old_path)
  old_loaded <- read_sframe(old_path)
  expect_true(inherits(old_loaded, "sframe"))
  expect_equal(length(old_loaded$models), 0)
})

test_that("validate_sframe catches invalid model and analysis references", {
  instr <- make_v03_instrument()
  bad_model <- sf_model(
    "bad_model",
    type = "cfa",
    constructs = list(sf_construct("BAD", items = c("sat_1", "ghost")))
  )
  instr$models <- list(bad_model)
  instr$analysis_plan <- list(list(
    id = "rq1",
    research_question = "Bad role",
    method = "frequency",
    roles = list(variable = "missing_item")
  ))

  result <- validate_sframe(instr, strict = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("unknown variable", result$problems)))
  expect_true(any(grepl("unknown item", result$problems)))
})

test_that("validate_sframe accepts analysis roles that reference saved models", {
  instr <- add_model(
    make_v03_instrument(),
    sf_model(
      "model_1",
      type = "cfa",
      constructs = list(sf_construct("SAT", items = c("sat_1", "sat_2", "sat_3")))
    )
  )
  instr$analysis_plan <- list(list(
    id = "rq1",
    research_question = "What is the CFA syntax?",
    method = "cfa_lavaan_syntax",
    roles = list(model = "model_1"),
    variables = "model_1"
  ))
  expect_true(validate_sframe(instr, strict = FALSE)$valid)
})

test_that("builder registry declares roles and conditional significance level", {
  html <- paste(readLines(launch_builder(open = FALSE), warn = FALSE), collapse = "\n")
  expect_match(html, "ANALYSIS_REGISTRY", fixed = TRUE)
  expect_match(html, "showAlpha:false", fixed = TRUE)
  expect_match(html, "showAlpha:true", fixed = TRUE)
  expect_match(html, "Variables and roles", fixed = TRUE)
  expect_match(html, "Significance level", fixed = TRUE)
  expect_match(html, "Save analysis plan", fixed = TRUE)
  expect_match(html, "Model Builder", fixed = TRUE)
  expect_match(html, "modelFromScales", fixed = TRUE)
  expect_match(html, "addModelPath", fixed = TRUE)
  expect_match(html, "modelList", fixed = TRUE)
  expect_match(html, "cfa_lavaan_syntax", fixed = TRUE)
  expect_match(html, "seminr_syntax", fixed = TRUE)
})

test_that("Shiny workflow mirrors the role-based analysis planner", {
  app_path <- system.file("shiny", "app.R", package = "surveyframe")
  expect_true(file.exists(app_path))
  expect_silent(parse(app_path))

  app <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
  expect_match(app, "analysis_registry", fixed = TRUE)
  expect_match(app, "Analysis method", fixed = TRUE)
  expect_match(app, "Variables and constructs", fixed = TRUE)
  expect_match(app, "analysis_role_fields", fixed = TRUE)
  expect_match(app, "Significance level", fixed = TRUE)
  expect_match(app, "show_alpha = FALSE", fixed = TRUE)
  expect_match(app, "show_alpha = TRUE", fixed = TRUE)
  expect_match(app, "Save analysis plan", fixed = TRUE)
  expect_match(app, "Model Builder", fixed = TRUE)
  expect_match(app, "Create constructs from scales", fixed = TRUE)
  expect_match(app, "analysis_plan = rv$builder$analysis_plan", fixed = TRUE)
  expect_match(app, "models = rv$builder$models", fixed = TRUE)
})

test_that("sample_size_plan and validity_report return structured outputs", {
  ss <- sample_size_plan("correlation", r = 0.3)
  expect_s3_class(ss, "sframe_sample_size_plan")
  expect_gt(ss$estimated_n, 0)

  vr <- validity_report(list(SAT = c(sat_1 = .7, sat_2 = .8, sat_3 = .75)))
  expect_s3_class(vr, "sframe_validity_report")
  expect_true(all(c("composite_reliability", "AVE") %in% names(vr$reliability)))
})

test_that("ordinal and multinomial logistic runners fail gracefully or run when optional packages exist", {
  dat <- data.frame(
    y_ord = ordered(rep(c("low", "mid", "high"), each = 10)),
    y_nom = rep(c("a", "b", "c"), each = 10),
    x = rep(1:10, 3)
  )

  ord <- if (requireNamespace("MASS", quietly = TRUE)) {
    sframe_run_ordinal_logistic(
      dat,
      list(dependent = "y_ord", predictors = "x")
    )
  } else {
    tryCatch(
      sframe_run_ordinal_logistic(dat, list(dependent = "y_ord", predictors = "x")),
      error = function(e) e
    )
  }
  expect_true(is.list(ord) || inherits(ord, "error"))

  multi <- if (requireNamespace("nnet", quietly = TRUE)) {
    sframe_run_multinomial_logistic(
      dat,
      list(dependent = "y_nom", predictors = "x")
    )
  } else {
    tryCatch(
      sframe_run_multinomial_logistic(dat, list(dependent = "y_nom", predictors = "x")),
      error = function(e) e
    )
  }
  expect_true(is.list(multi) || inherits(multi, "error"))
})
