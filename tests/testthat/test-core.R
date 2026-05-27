# tests/testthat/test-core.R
# Full test suite for surveyframe v0.1.

# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

make_instrument <- function(reverse = FALSE) {
  cs    <- sf_choices("ag5", 1:5,
            c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"))
  yn    <- sf_choices("yn", c("yes","no"), c("Yes","No"))
  i1    <- sf_item("sat_1", "The service was fast.",
                   type = "likert", choice_set = "ag5",
                   scale_id = "sat", required = TRUE)
  i2    <- sf_item("sat_2", "The staff were helpful.",
                   type = "likert", choice_set = "ag5",
                   scale_id = "sat", required = TRUE)
  i3    <- sf_item("sat_3", "I would recommend this service.",
                   type = "likert", choice_set = "ag5",
                   scale_id = "sat", required = TRUE, reverse = reverse)
  age   <- sf_item("age",    "What is your age?",  type = "numeric")
  gen   <- sf_item("gender", "Gender?",
                   type = "single_choice", choice_set = "yn")
  attn  <- sf_item("attn_q", "Please select Agree (4).",
                   type = "likert", choice_set = "ag5")
  scale <- sf_scale("sat", "Satisfaction",
                    items = c("sat_1","sat_2","sat_3"),
                    method = "mean", min_valid = 2L)
  chk   <- sf_check("chk_1", item_id = "attn_q", type = "attention",
                    pass_values = 4, fail_action = "flag")
  rule  <- sf_branch("attn_q", depends_on = "gender",
                     operator = "==", value = "yes", action = "show")
  sf_instrument(
    title       = "Service Quality Survey",
    version     = "1.0.0",
    description = "A test instrument.",
    authors     = "MAS",
    components  = list(cs, yn, i1, i2, i3, age, gen, attn, scale, chk, rule)
  )
}

make_responses <- function(n = 50, seed = 42) {
  set.seed(seed)
  data.frame(
    id           = paste0("R", seq_len(n)),
    submitted_at = as.character(Sys.time() - sample(100:3600, n, replace = TRUE)),
    sat_1        = sample(1:5, n, replace = TRUE),
    sat_2        = sample(1:5, n, replace = TRUE),
    sat_3        = sample(1:5, n, replace = TRUE),
    age          = sample(18:65, n, replace = TRUE),
    gender       = sample(c("yes","no"), n, replace = TRUE),
    attn_q       = c(rep(4, floor(n * 0.85)), rep(2, ceiling(n * 0.15))),
    stringsAsFactors = FALSE
  )
}

load_resp <- function(n = 50, seed = 42, reverse = FALSE) {
  instr <- make_instrument(reverse = reverse)
  resp  <- make_responses(n, seed)
  list(
    instr = instr,
    resp  = suppressWarnings(
      read_responses(resp, instr,
                     respondent_id = "id",
                     submitted_at  = "submitted_at")
    )
  )
}

make_timed_responses <- function(include_bad_row = FALSE) {
  started_at <- c(
    "2024-06-01 10:00:00",
    "2024-06-01 10:00:00",
    "2024-06-01 10:00:00",
    "2024-06-01 10:00:50"
  )

  if (include_bad_row) {
    started_at[4] <- "bad-start-time"
  }

  data.frame(
    id = paste0("R", 1:4),
    started_at = started_at,
    submitted_at = c(
      "2024-06-01 10:02:00",
      "2024-06-01 10:00:20",
      "2024-06-01 10:01:10",
      "2024-06-01 10:02:00"
    ),
    sat_1 = c(4, 4, 5, 3),
    sat_2 = c(4, 4, 4, 3),
    sat_3 = c(4, 4, 4, 3),
    age = c(30, 31, 32, 33),
    gender = c("yes", "yes", "no", "yes"),
    attn_q = c(4, 4, 4, 4),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# 1. Constructors
# ---------------------------------------------------------------------------

test_that("sf_item() constructs correctly", {
  item <- sf_item("q1", "How satisfied are you?", type = "likert",
                  choice_set = "ag5", scale_id = "sat", required = TRUE)
  expect_s3_class(item, "sf_item")
  expect_equal(item$id, "q1")
  expect_equal(item$type, "likert")
  expect_true(item$required)
  expect_false(item$reverse)
})

test_that("sf_item() rejects invalid type", {
  expect_error(sf_item("q1", "Label", type = "checkbox"))
})

test_that("sf_choices() constructs correctly", {
  cs <- sf_choices("ag5", 1:5, c("SD","D","N","A","SA"))
  expect_s3_class(cs, "sf_choices")
  expect_equal(length(cs$values), 5)
})

test_that("sf_choices() validates length mismatch", {
  expect_error(
    sf_choices("bad", values = 1:3, labels = c("a","b")),
    class = "sframe_validation_error"
  )
})

test_that("sf_scale() constructs correctly", {
  s <- sf_scale("sat", "Satisfaction", items = c("q1","q2"), method = "mean")
  expect_s3_class(s, "sf_scale")
  expect_equal(s$method, "mean")
})

test_that("sf_scale() validates weight length", {
  expect_error(
    sf_scale("s", "S", items = c("q1","q2"), weights = c(1)),
    class = "sframe_validation_error"
  )
})

test_that("sf_branch() constructs correctly", {
  r <- sf_branch("q2", depends_on = "q1", operator = "==",
                 value = "yes", action = "show")
  expect_s3_class(r, "sf_branch")
  expect_equal(r$operator, "==")
})

test_that("sf_check() constructs correctly", {
  chk <- sf_check("c1", item_id = "attn_q", type = "attention",
                  pass_values = 4, fail_action = "flag")
  expect_s3_class(chk, "sf_check")
  expect_equal(chk$type, "attention")
})

# ---------------------------------------------------------------------------
# 2. sf_instrument()
# ---------------------------------------------------------------------------

test_that("sf_instrument() rejects unknown components", {
  expect_error(
    sf_instrument("T", components = list(list(x = 1))),
    class = "sframe_validation_error"
  )
})

test_that("sf_instrument() assembles sframe with correct slot counts", {
  instr <- make_instrument()
  expect_s3_class(instr, "sframe")
  expect_equal(length(instr$items),     6)
  expect_equal(length(instr$scales),    1)
  expect_equal(length(instr$choices),   2)
  expect_equal(length(instr$branching), 1)
  expect_equal(length(instr$checks),    1)
})

test_that("sf_instrument() meta fields are correct", {
  instr <- make_instrument()
  expect_equal(instr$meta$title,   "Service Quality Survey")
  expect_equal(instr$meta$version, "1.0.0")
  expect_false(isTRUE(instr$meta$validated))
})

# ---------------------------------------------------------------------------
# 3. validate_sframe()
# ---------------------------------------------------------------------------

test_that("validate_sframe() passes a clean instrument", {
  result <- validate_sframe(make_instrument(), strict = FALSE)
  expect_true(result$valid)
  expect_length(result$problems, 0)
})

test_that("validate_sframe() sets validated = TRUE", {
  out <- validate_sframe(make_instrument(), strict = TRUE)
  expect_true(isTRUE(out$meta$validated))
})

test_that("validate_sframe() catches duplicate item IDs", {
  i1    <- sf_item("q1", "One", type = "text")
  i2    <- sf_item("q1", "Dupe", type = "text")
  instr <- sf_instrument("Bad", components = list(i1, i2))
  result <- validate_sframe(instr, strict = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("Duplicate", result$problems)))
})

test_that("validate_sframe() catches orphan choice_set", {
  i <- sf_item("q1", "Q", type = "likert", choice_set = "ghost")
  instr <- sf_instrument("Bad", components = list(i))
  result <- validate_sframe(instr, strict = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("choice_set", result$problems)))
})

test_that("validate_sframe() catches orphan scale_id", {
  cs <- sf_choices("ag5", 1:5, c("SD","D","N","A","SA"))
  i  <- sf_item("q1", "Q", type = "likert",
                choice_set = "ag5", scale_id = "ghost")
  instr <- sf_instrument("Bad", components = list(cs, i))
  result <- validate_sframe(instr, strict = FALSE)
  expect_false(result$valid)
})

test_that("validate_sframe() catches reverse without scale_id", {
  cs <- sf_choices("ag5", 1:5, c("SD","D","N","A","SA"))
  i  <- sf_item("q1", "Q", type = "likert",
                choice_set = "ag5", reverse = TRUE)
  instr <- sf_instrument("Bad", components = list(cs, i))
  result <- validate_sframe(instr, strict = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("reverse", result$problems)))
})

test_that("validate_sframe() catches scale with missing item", {
  cs    <- sf_choices("ag5", 1:5, c("SD","D","N","A","SA"))
  i     <- sf_item("q1", "Q", type = "likert",
                   choice_set = "ag5", scale_id = "sat")
  scale <- sf_scale("sat", "Sat", items = c("q1","ghost"))
  instr <- sf_instrument("Bad", components = list(cs, i, scale))
  result <- validate_sframe(instr, strict = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("ghost", result$problems)))
})

test_that("validate_sframe() strict mode raises sframe_validation_error", {
  i1    <- sf_item("q1", "One", type = "text")
  i2    <- sf_item("q1", "Two", type = "text")
  instr <- sf_instrument("Bad", components = list(i1, i2))
  expect_error(validate_sframe(instr, strict = TRUE),
               class = "sframe_validation_error")
})

# ---------------------------------------------------------------------------
# 4. S3 methods
# ---------------------------------------------------------------------------

test_that("print.sframe() returns object invisibly", {
  instr <- make_instrument()
  out   <- capture.output(ret <- print(instr))
  expect_s3_class(ret, "sframe")
  expect_true(any(grepl("Service Quality Survey", out)))
})

test_that("format.sframe() returns a character string", {
  s <- format(make_instrument())
  expect_type(s, "character")
  expect_true(grepl("sframe", s))
})

test_that("summary.sframe() prints and returns invisibly", {
  instr <- make_instrument()
  out   <- capture.output(ret <- summary(instr))
  expect_s3_class(ret, "sframe")
  expect_true(any(grepl("Items", out)))
  expect_true(any(grepl("Scales", out)))
})

# ---------------------------------------------------------------------------
# 5. read_responses()
# ---------------------------------------------------------------------------

test_that("read_responses() accepts a matching data frame", {
  d <- load_resp()
  expect_s3_class(d$resp, "data.frame")
  expect_true("sat_1" %in% colnames(d$resp))
  expect_true("id"    %in% colnames(d$resp))
})

test_that("read_responses() strict mode rejects undeclared columns", {
  instr <- make_instrument()
  resp  <- make_responses(10)
  resp$extra <- "noise"
  expect_error(
    read_responses(resp, instr, respondent_id = "id", strict = TRUE),
    class = "sframe_import_error"
  )
})

test_that("read_responses() non-strict warns on undeclared columns", {
  instr <- make_instrument()
  resp  <- make_responses(10)
  resp$extra <- "noise"
  expect_warning(
    read_responses(resp, instr, respondent_id = "id", strict = FALSE),
    class = "sframe_quality_warning"
  )
})

test_that("read_responses() reports missing files and unsupported inputs clearly", {
  instr <- make_instrument()
  missing_path <- tempfile(fileext = ".csv")

  expect_error(
    read_responses(missing_path, instr),
    "Check the file path and ensure the file exists",
    class = "sframe_import_error"
  )
  expect_error(
    read_responses(list(a = 1), instr),
    "CSV file path, a data.frame, or a tibble",
    class = "sframe_import_error"
  )
})

test_that("read_responses() warns when item columns are absent", {
  instr <- make_instrument()
  resp  <- make_responses(10)
  resp$sat_3 <- NULL
  # submitted_at is declared so only the missing-item warning fires
  expect_warning(
    read_responses(resp, instr, respondent_id = "id",
                   submitted_at = "submitted_at", strict = FALSE),
    class = "sframe_missing_data_warning"
  )
})

test_that("read_responses() puts metadata columns first", {
  d    <- load_resp()
  cols <- colnames(d$resp)
  expect_equal(cols[1], "id")
  expect_equal(cols[2], "submitted_at")
})

# ---------------------------------------------------------------------------
# 6. score_scales()
# ---------------------------------------------------------------------------

test_that("score_scales() produces a scale column", {
  d   <- load_resp(50)
  out <- score_scales(d$resp, d$instr)
  expect_true("sat" %in% colnames(out))
  expect_equal(nrow(out), 50)
})

test_that("score_scales() mean equals manual mean", {
  d   <- load_resp(30)
  out <- score_scales(d$resp, d$instr)
  manual <- rowMeans(out[, c("sat_1","sat_2","sat_3")], na.rm = TRUE)
  expect_equal(out$sat, manual, tolerance = 1e-10)
})

test_that("score_scales() returns NA when valid items below min_valid", {
  instr <- make_instrument()
  resp  <- make_responses(10)
  resp[1, c("sat_1","sat_2","sat_3")] <- NA
  resp  <- suppressWarnings(
    read_responses(resp, instr, respondent_id = "id",
                   submitted_at = "submitted_at", strict = FALSE))
  out <- score_scales(resp, instr)
  expect_true(is.na(out$sat[1]))
})

test_that("score_scales() applies reverse coding", {
  instr <- make_instrument(reverse = TRUE)
  resp  <- data.frame(
    id = "R1", submitted_at = "2024-01-01",
    sat_1 = 4, sat_2 = 4, sat_3 = 2,
    age = 30, gender = "yes", attn_q = 4,
    stringsAsFactors = FALSE
  )
  resp <- suppressWarnings(
    read_responses(resp, instr, respondent_id = "id",
                   submitted_at = "submitted_at", strict = FALSE))
  out <- score_scales(resp, instr)
  # sat_3 reverse: (1+5)-2 = 4; mean(4,4,4) = 4
  expect_equal(out$sat, 4, tolerance = 1e-10)
})

test_that("score_scales() keep_items = FALSE drops item columns", {
  d   <- load_resp()
  out <- score_scales(d$resp, d$instr, keep_items = FALSE, keep_meta = FALSE)
  expect_false("sat_1" %in% colnames(out))
  expect_false("id"    %in% colnames(out))
  expect_true("sat"    %in% colnames(out))
})

test_that("score_scales() applies weighted mean scoring", {
  cs <- sf_choices("ag5", 1:5, c("SD", "D", "N", "A", "SA"))
  i1 <- sf_item("q1", "Q1", type = "likert", choice_set = "ag5", scale_id = "sat")
  i2 <- sf_item("q2", "Q2", type = "likert", choice_set = "ag5", scale_id = "sat")
  scale <- sf_scale("sat", "Weighted", items = c("q1", "q2"),
                    method = "mean", weights = c(1, 3))
  instr <- sf_instrument("Weighted mean", components = list(cs, i1, i2, scale))
  resp <- data.frame(q1 = 2, q2 = 4, stringsAsFactors = FALSE)

  out <- score_scales(resp, instr, keep_meta = FALSE)
  expect_equal(out$sat, 3.5, tolerance = 1e-10)
})

test_that("score_scales() applies weighted sum scoring", {
  cs <- sf_choices("ag5", 1:5, c("SD", "D", "N", "A", "SA"))
  i1 <- sf_item("q1", "Q1", type = "likert", choice_set = "ag5", scale_id = "sat")
  i2 <- sf_item("q2", "Q2", type = "likert", choice_set = "ag5", scale_id = "sat")
  scale <- sf_scale("sat", "Weighted", items = c("q1", "q2"),
                    method = "sum", weights = c(2, 1))
  instr <- sf_instrument("Weighted sum", components = list(cs, i1, i2, scale))
  resp <- data.frame(q1 = 2, q2 = 4, stringsAsFactors = FALSE)

  out <- score_scales(resp, instr, keep_meta = FALSE)
  expect_equal(out$sat, 8, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# 7. quality_report()
# ---------------------------------------------------------------------------

test_that("quality_report() returns sframe_quality_report", {
  d  <- load_resp(50)
  qr <- quality_report(d$resp, d$instr, respondent_id = "id")
  expect_s3_class(qr, "sframe_quality_report")
  expect_equal(qr$summary$n_respondents, 50)
})

test_that("quality_report() detects attention check failures", {
  d  <- load_resp(50)
  qr <- quality_report(d$resp, d$instr, respondent_id = "id")
  expect_true("chk_1" %in% names(qr$attention))
  expect_gt(qr$attention$chk_1$n_fail, 0)
})

test_that("quality_report() flags straight-lining respondents", {
  instr <- make_instrument()
  resp  <- make_responses(20)
  resp[1:5, c("sat_1","sat_2","sat_3")] <- 3
  resp  <- suppressWarnings(
    read_responses(resp, instr, respondent_id = "id",
                   submitted_at = "submitted_at"))
  qr    <- quality_report(resp, instr)
  expect_true("sat" %in% names(qr$straightline))
  expect_gte(length(qr$straightline$sat$flagged_rows), 5)
})

test_that("quality_report() detects duplicate respondent IDs", {
  instr <- make_instrument()
  resp  <- make_responses(10)
  resp$id[5] <- resp$id[1]
  resp  <- suppressWarnings(
    read_responses(resp, instr, respondent_id = "id",
                   submitted_at = "submitted_at"))
  qr    <- quality_report(resp, instr, respondent_id = "id")
  expect_gt(qr$duplicates$n_duplicates, 0)
})

test_that("quality_report() computes timing diagnostics", {
  instr <- make_instrument()
  resp <- suppressWarnings(
    read_responses(
      make_timed_responses(),
      instr,
      respondent_id = "id",
      submitted_at = "submitted_at",
      meta_cols = "started_at",
      strict = FALSE
    )
  )

  qr <- quality_report(
    resp,
    instr,
    respondent_id = "id",
    submitted_at = "submitted_at",
    started_at = "started_at",
    time_min = 40
  )

  expect_true(qr$timing$available)
  expect_equal(qr$timing$start_col, "started_at")
  expect_equal(qr$timing$n_flagged, 1)
  expect_equal(qr$timing$flagged_rows, 2)
  expect_equal(qr$timing$median_sec, 70)
})

test_that("quality_report() auto-detects started_at column", {
  instr <- make_instrument()
  resp <- suppressWarnings(
    read_responses(
      make_timed_responses(),
      instr,
      respondent_id = "id",
      submitted_at = "submitted_at",
      meta_cols = "started_at",
      strict = FALSE
    )
  )

  qr <- quality_report(
    resp,
    instr,
    respondent_id = "id",
    submitted_at = "submitted_at",
    time_min = 40
  )

  expect_true(qr$timing$available)
  expect_equal(qr$timing$start_col, "started_at")
})

test_that("quality_report() reports unavailable timing without start column", {
  d <- load_resp(10)
  qr <- quality_report(
    d$resp,
    d$instr,
    respondent_id = "id",
    submitted_at = "submitted_at",
    time_min = 40
  )

  expect_false(qr$timing$available)
})

test_that("quality_report() warns on malformed timing rows", {
  instr <- make_instrument()
  resp <- suppressWarnings(
    read_responses(
      make_timed_responses(include_bad_row = TRUE),
      instr,
      respondent_id = "id",
      submitted_at = "submitted_at",
      meta_cols = "started_at",
      strict = FALSE
    )
  )

  expect_warning(
    qr <- quality_report(
      resp,
      instr,
      respondent_id = "id",
      submitted_at = "submitted_at",
      started_at = "started_at",
      time_min = 40
    ),
    class = "sframe_quality_warning"
  )
  expect_equal(qr$timing$parse_fail_rows, 4)
})

test_that("print.sframe_quality_report() produces output", {
  d   <- load_resp(20)
  qr  <- quality_report(d$resp, d$instr, respondent_id = "id")
  out <- capture.output(print(qr))
  expect_true(any(grepl("Quality Report", out)))
})

# ---------------------------------------------------------------------------
# 8. reliability_report()
# ---------------------------------------------------------------------------

test_that("reliability_report() returns sframe_reliability_report", {
  d  <- load_resp(80)
  rr <- reliability_report(d$resp, d$instr, omega = FALSE)
  expect_s3_class(rr, "sframe_reliability_report")
  expect_equal(length(rr), 1)
  expect_equal(names(rr), "sat")
})

test_that("reliability_report() alpha is in [0, 1]", {
  d  <- load_resp(100)
  rr <- reliability_report(d$resp, d$instr, omega = FALSE)
  expect_gte(rr[[1]]$alpha, 0)
  expect_lte(rr[[1]]$alpha, 1)
})

test_that("reliability_report() respects reverse-coded items", {
  instr <- make_instrument(reverse = TRUE)
  resp <- data.frame(
    id = paste0("R", 1:10),
    submitted_at = rep("2024-01-01 00:00:00", 10),
    sat_1 = rep(1:5, 2),
    sat_2 = rep(1:5, 2),
    sat_3 = rep(5:1, 2),
    age = rep(30, 10),
    gender = rep("yes", 10),
    attn_q = rep(4, 10),
    stringsAsFactors = FALSE
  )
  resp <- suppressWarnings(
    read_responses(resp, instr, respondent_id = "id",
                   submitted_at = "submitted_at")
  )

  expect_no_warning(
    rr <- reliability_report(resp, instr, omega = FALSE)
  )
  expect_gt(rr[[1]]$alpha, 0.99)
})

# ---------------------------------------------------------------------------
# 9. item_report()
# ---------------------------------------------------------------------------

test_that("item_report() returns sframe_item_report with expected columns", {
  d  <- load_resp(40)
  ir <- item_report(d$resp, d$instr)
  expect_s3_class(ir, "sframe_item_report")
  expect_equal(names(ir), "sat")
  diag_cols <- colnames(ir[[1]]$diagnostics)
  expect_true("item_rest_r"  %in% diag_cols)
  expect_true("floor_pct"    %in% diag_cols)
  expect_true("ceiling_pct"  %in% diag_cols)
  expect_true("mean"         %in% diag_cols)
  expect_true("sd"           %in% diag_cols)
})

# ---------------------------------------------------------------------------
# 10. cfa_syntax()
# ---------------------------------------------------------------------------

test_that("cfa_syntax() produces lavaan model string", {
  syntax <- cfa_syntax(make_instrument())
  expect_type(syntax, "character")
  expect_true(grepl("sat =~", syntax))
  expect_true(grepl("sat_1", syntax))
})

test_that("cfa_syntax() notes reverse items in comment", {
  syntax <- cfa_syntax(make_instrument(reverse = TRUE))
  expect_true(grepl("Reverse-coded", syntax))
  expect_true(grepl("sat_3", syntax))
})

test_that("cfa_syntax() subset by scale ID works", {
  syntax <- cfa_syntax(make_instrument(), scales = "sat")
  expect_true(grepl("sat =~", syntax))
})

# ---------------------------------------------------------------------------
# 11. codebook_report()
# ---------------------------------------------------------------------------

test_that("codebook_report() returns sframe_codebook", {
  cb <- codebook_report(make_instrument())
  expect_s3_class(cb, "sframe_codebook")
  expect_equal(nrow(cb$items_table),  6)
  expect_equal(nrow(cb$scales_table), 1)
  expect_gt(nrow(cb$choices_table),   0)
})

test_that("codebook_report() items_table has required columns", {
  cb <- codebook_report(make_instrument())
  expect_true(all(c("id","label","type","scale_id","reverse","required")
                  %in% colnames(cb$items_table)))
})

test_that("print.sframe_codebook() produces output without error", {
  out <- capture.output(print(codebook_report(make_instrument())))
  expect_true(any(grepl("Codebook", out)))
})

# ---------------------------------------------------------------------------
# 12. render_report()
# ---------------------------------------------------------------------------

test_that("render_report() writes an HTML report with the fallback renderer", {
  instr <- validate_sframe(make_instrument())
  resp <- suppressWarnings(
    read_responses(
      make_responses(8),
      instr,
      respondent_id = "id",
      submitted_at = "submitted_at"
    )
  )

  out_dir <- file.path(tempdir(), paste0("surveyframe-report-", Sys.getpid()))
  out <- file.path(out_dir, "report.html")

  old <- options(surveyframe.use_quarto = FALSE)
  on.exit(options(old), add = TRUE)

  expect_no_error(render_report(
    instr,
    data = resp,
    output_file = out,
    include_reliability = FALSE
  ))
  expect_true(file.exists(out))

  html <- paste(readLines(out, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "Service Quality Survey", fixed = TRUE)
  expect_match(html, "Instrument hash", fixed = TRUE)
})

test_that("render_report() writes an HTML report through Quarto when available", {
  skip_if_not_installed("quarto")
  skip_if_not(nzchar(Sys.which("quarto")), "Quarto CLI not installed")

  instr <- validate_sframe(make_instrument())
  out <- tempfile(fileext = ".html")

  expect_no_error(render_report(instr, output_file = out))
  expect_true(file.exists(out))
})

# ---------------------------------------------------------------------------
# 13. studio builder helpers
# ---------------------------------------------------------------------------

test_that("builder helpers compose a valid instrument from draft components", {
  meta <- list(
    title = "Draft Survey",
    version = "0.1.0",
    description = "Built in the studio",
    authors = c("Builder"),
    languages = "en"
  )

  choices <- list(
    sf_choices("agree5", 1:5,
      c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
  )
  items <- list(
    sf_item("sat_1", "The service was fast.", type = "likert", choice_set = "agree5"),
    sf_item("sat_2", "The staff were helpful.", type = "likert", choice_set = "agree5")
  )
  scales <- list(
    sf_scale("sat", "Satisfaction",
      items = c("sat_1", "sat_2"), reverse_items = "sat_2")
  )

  draft <- surveyframe::sframe_builder_validate_draft(
    meta = meta,
    choices = choices,
    items = items,
    scales = scales
  )

  expect_true(draft$valid)
  expect_equal(vapply(draft$instrument$items, function(item) item$scale_id, character(1)),
               c("sat", "sat"))
  expect_equal(vapply(draft$instrument$items, function(item) item$reverse, logical(1)),
               c(FALSE, TRUE))
})

test_that("builder helpers reclassify components from a loaded sframe file", {
  instr <- make_instrument(reverse = TRUE)
  path <- tempfile(fileext = ".sframe")
  write_sframe(instr, path, overwrite = TRUE)

  loaded <- read_sframe(path)
  state <- surveyframe::sframe_builder_state_from_instrument(loaded)

  expect_true(all(vapply(state$choices, inherits, logical(1), "sf_choices")))
  expect_true(all(vapply(state$items, inherits, logical(1), "sf_item")))
  expect_true(all(vapply(state$scales, inherits, logical(1), "sf_scale")))
  expect_true(all(vapply(state$branching, inherits, logical(1), "sf_branch")))
  expect_true(all(vapply(state$checks, inherits, logical(1), "sf_check")))

  rebuilt <- surveyframe::sframe_builder_validate_draft(
    meta = state$meta,
    choices = state$choices,
    items = state$items,
    scales = state$scales,
    branching = state$branching,
    checks = state$checks
  )

  expect_true(rebuilt$valid)
})

# ---------------------------------------------------------------------------
# 14. render_survey() helpers
# ---------------------------------------------------------------------------

test_that("render_survey() returns a shiny app object", {
  app <- render_survey(make_instrument())
  expect_s3_class(app, "shiny.appobj")
})

test_that("render_survey() requires output_path for csv persistence", {
  expect_error(
    render_survey(make_instrument(), save_responses = "csv"),
    class = "sframe_error"
  )
})

test_that("render_survey() helpers identify visible required items", {
  instr <- make_instrument()
  branch_lookup <- surveyframe:::sframe_branch_lookup(instr)
  input_values <- list(
    gender = "yes",
    sat_1 = 4,
    sat_2 = NULL,
    sat_3 = 5,
    attn_q = NULL
  )

  missing <- surveyframe:::sframe_missing_required_items(
    instr,
    input_values,
    branch_lookup
  )

  expect_equal(missing, "sat_2")
  expect_true(surveyframe:::sframe_item_visible(instr$items[[6]], input_values, branch_lookup))
})

test_that("render_survey() branching handles all operators and multi-value answers", {
  rule <- function(operator, value, action = "show") {
    sf_branch("q2", depends_on = "q1", operator = operator, value = value,
              action = action)
  }

  expect_true(surveyframe:::.evaluate_branch(rule("==", "a"), c("a", "b")))
  expect_false(surveyframe:::.evaluate_branch(rule("==", "c"), c("a", "b")))
  expect_false(surveyframe:::.evaluate_branch(rule("!=", "a"), c("a", "b")))
  expect_true(surveyframe:::.evaluate_branch(rule("!=", "c"), c("a", "b")))
  expect_true(surveyframe:::.evaluate_branch(rule("%in%", c("b", "c")), c("a", "b")))
  expect_true(surveyframe:::.evaluate_branch(rule(">", "3"), c("2", "4")))
  expect_true(surveyframe:::.evaluate_branch(rule(">=", "4"), "4"))
  expect_true(surveyframe:::.evaluate_branch(rule("<", "3"), "2"))
  expect_true(surveyframe:::.evaluate_branch(rule("<=", "2"), "2"))
  expect_false(surveyframe:::.evaluate_branch(rule(">", "3"), "x"))
  expect_false(surveyframe:::.evaluate_branch(rule("==", "a"), NA_character_))
  expect_true(surveyframe:::.evaluate_branch(rule("==", "a", action = "hide"), NA_character_))
})

test_that("render_survey() response rows blank hidden items and append to csv", {
  instr <- make_instrument()
  branch_lookup <- surveyframe:::sframe_branch_lookup(instr)
  input_values <- list(
    sat_1 = 4,
    sat_2 = 5,
    sat_3 = 3,
    age = 28,
    gender = "no",
    attn_q = 4
  )

  row <- surveyframe:::sframe_response_row(
    instr,
    input_values,
    branch_lookup,
    started_at = as.POSIXct("2024-06-01 10:00:00", tz = "UTC"),
    submitted_at = as.POSIXct("2024-06-01 10:02:00", tz = "UTC")
  )

  expect_true(all(c("started_at", "submitted_at", "sat_1", "attn_q") %in% names(row)))
  expect_true(is.na(row$attn_q))

  path <- tempfile(fileext = ".csv")
  surveyframe:::sframe_append_response_csv(path, row)
  surveyframe:::sframe_append_response_csv(path, row)

  saved <- read.csv(path, stringsAsFactors = FALSE)
  expect_equal(nrow(saved), 2)
  expect_true(all(c("started_at", "submitted_at", "sat_1") %in% names(saved)))
})

test_that("render_survey() persists submissions through the Shiny server", {
  path <- tempfile(fileext = ".csv")
  app <- render_survey(make_instrument(), save_responses = "csv", output_path = path)

  shiny::testServer(app, {
    session$setInputs(
      sat_1 = "4",
      sat_2 = "5",
      sat_3 = "3",
      age = 29,
      gender = "yes",
      attn_q = "4",
      submit_btn = 1
    )
  })

  saved <- read.csv(path, stringsAsFactors = FALSE)
  expect_equal(nrow(saved), 1)
  expect_true(all(c("started_at", "submitted_at", "sat_1", "attn_q") %in% names(saved)))
})

test_that("render_survey() blocks invalid submissions through the Shiny server", {
  path <- tempfile(fileext = ".csv")
  app <- render_survey(make_instrument(), save_responses = "csv", output_path = path)

  shiny::testServer(app, {
    session$setInputs(
      sat_1 = "4",
      sat_3 = "3",
      gender = "yes",
      submit_btn = 1
    )
  })

  expect_false(file.exists(path))
})

# ---------------------------------------------------------------------------
# 15. Integration workflow
# ---------------------------------------------------------------------------

test_that("full workflow runs from instrument to scored outputs", {
  instr <- validate_sframe(make_instrument(reverse = TRUE))
  path <- tempfile(fileext = ".sframe")
  write_sframe(instr, path, overwrite = TRUE)

  loaded <- read_sframe(path)
  raw <- make_responses(20)
  raw$started_at <- format(
    as.POSIXct(raw$submitted_at, tz = "UTC") - 120,
    "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )

  resp <- suppressWarnings(
    read_responses(
      raw,
      loaded,
      respondent_id = "id",
      submitted_at = "submitted_at",
      meta_cols = "started_at",
      strict = FALSE
    )
  )

  qr <- quality_report(
    resp,
    loaded,
    respondent_id = "id",
    submitted_at = "submitted_at",
    started_at = "started_at",
    time_min = 60
  )
  scored <- score_scales(resp, loaded)
  rr <- reliability_report(resp, loaded, omega = FALSE)
  cb <- codebook_report(loaded)
  syntax <- cfa_syntax(loaded)

  expect_true(qr$timing$available)
  expect_true("sat" %in% names(scored))
  expect_s3_class(rr, "sframe_reliability_report")
  expect_s3_class(cb, "sframe_codebook")
  expect_match(syntax, "sat =~")
})
