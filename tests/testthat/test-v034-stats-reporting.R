# tests/testthat/test-v034-stats-reporting.R
# The statistics and reporting half of 0.3.4: Henseler HTMT, Little's MCAR,
# omega notes, tidy EFA frames, PDF output, report theming, and the codebook
# plan and model summaries.

test_that("validity_report computes Henseler HTMT from item-level data", {
  set.seed(11)
  f1 <- rnorm(120)
  f2 <- f1 * 0.4 + rnorm(120, 0, 0.9)
  items <- function(k, base) {
    as.data.frame(replicate(k, base + rnorm(120, 0, 0.6)))
  }
  vr <- validity_report(
    list(A = c(a1 = .8, a2 = .75, a3 = .7), B = c(b1 = .8, b2 = .7)),
    items_by_construct = list(A = items(3, f1), B = items(2, f2))
  )
  expect_identical(vr$htmt_method, "henseler")
  expect_identical(dim(vr$htmt), c(2L, 2L))
  expect_identical(unname(diag(vr$htmt)), c(1, 1))
  expect_identical(vr$htmt[1, 2], vr$htmt[2, 1])
  expect_true(vr$htmt[1, 2] > 0 && vr$htmt[1, 2] < 1)

  # A single-item construct has no monotrait correlations
  vr_single <- validity_report(
    list(A = c(a1 = .8, a2 = .75), S = c(s1 = .7)),
    items_by_construct = list(A = items(2, f1), S = items(1, f2))
  )
  expect_true(is.na(vr_single$htmt["A", "S"]))
})

test_that("validity_report keeps the correlation fallback without item data", {
  set.seed(12)
  scores <- data.frame(A = rnorm(60), B = rnorm(60))
  vr <- validity_report(list(A = c(a1 = .8), B = c(b1 = .7)),
                        construct_scores = scores)
  expect_identical(vr$htmt_method, "correlation_fallback")
  expect_equal(vr$htmt, abs(vr$inter_construct_correlations))
  vr_none <- validity_report(list(A = c(a1 = .8), B = c(b1 = .7)))
  expect_identical(vr_none$htmt_method, "none")
  expect_null(vr_none$htmt)
})

test_that("missing_data_report runs Little's MCAR test when naniar is present", {
  skip_if_not_installed("naniar")
  set.seed(13)
  d <- as.data.frame(replicate(4, rnorm(80)))
  names(d) <- paste0("v", 1:4)
  m <- as.matrix(d)
  m[sample(length(m), 30)] <- NA
  d <- as.data.frame(m)
  mr <- missing_data_report(d)
  expect_true(mr$mcar$available)
  expect_true(is.numeric(mr$mcar$p_value))
  expect_match(mr$mcar$interpretation, "missing completely at random")
})

test_that("missing_data_report keeps the unavailable MCAR result when the test cannot run", {
  # Complete data has no missingness pattern to test, so the guarded path
  # returns the pre-0.3.4 result even with naniar installed.
  d <- as.data.frame(replicate(3, rnorm(40)))
  mr <- missing_data_report(d)
  expect_false(mr$mcar$available)
  expect_match(mr$mcar$warning, "optional package")
})

test_that("reliability_report notes scales whose omega is unavailable and the plot names them", {
  skip_if_not_installed("psych")
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  rr <- reliability_report(demo$responses, demo$instrument)
  two_item <- Filter(function(s) s$n_items == 2, rr)
  skip_if(length(two_item) == 0, "demo has no 2-item scale")
  expect_match(two_item[[1]]$omega_note, "requires >= 3 items")
  gg <- plot(rr)
  expect_match(gg$labels$subtitle, "Omega unavailable for")
})

test_that("efa_solution returns tidy loadings, communalities, and variance frames", {
  skip_if_not_installed("psych")
  demo <- sframe_demo_data()
  sol <- efa_solution(demo$responses, demo$instrument, nfactors = 2)
  expect_identical(nrow(sol$loadings_long), nrow(sol$loadings) * 2L)
  expect_named(sol$loadings_long, c("item_id", "factor", "loading"))
  expect_named(sol$communalities_table,
               c("item_id", "communality", "uniqueness"))
  expect_identical(nrow(sol$variance_table), 2L)
  expect_named(sol$variance_table,
               c("factor", "ss_loadings", "proportion_var", "cumulative_var"))
  expect_true(all(diff(sol$variance_table$cumulative_var) >= 0))
})

test_that("render_report format = 'pdf' is guarded and additive", {
  fx_instr <- sf_instrument("PDF check", components = list(
    sf_choices("ag5", 1:5, c("SD", "D", "N", "A", "SA")),
    sf_item("q1", "Item 1", type = "likert", choice_set = "ag5")
  ))
  if (!requireNamespace("pagedown", quietly = TRUE)) {
    expect_error(
      render_report(fx_instr, output_file = tempfile(fileext = ".pdf"),
                    format = "pdf"),
      class = "sframe_missing_package"
    )
  } else {
    skip_on_cran()
    chrome <- tryCatch(pagedown::find_chrome(), error = function(e) NULL)
    skip_if(is.null(chrome), "No Chrome available for chrome_print()")
    out <- tempfile(fileext = ".pdf")
    on.exit(unlink(out), add = TRUE)
    old <- options(surveyframe.use_quarto = FALSE)
    on.exit(options(old), add = TRUE)
    render_report(fx_instr, output_file = out, format = "pdf")
    expect_true(file.exists(out))
    expect_gt(file.size(out), 1000)
    expect_identical(readBin(out, "raw", 4), charToRaw("%PDF"))
  }
})

test_that("the HTML fallback carries the brand variables, print styles, and table semantics", {
  demo <- sframe_demo_data()
  old <- options(surveyframe.use_quarto = FALSE)
  on.exit(options(old), add = TRUE)
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  render_report(demo$instrument, demo$responses, output_file = out)
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, ":root", fixed = TRUE)
  expect_match(html, "--sf-accent", fixed = TRUE)
  expect_match(html, "@media print", fixed = TRUE)
  expect_match(html, "scope=\"col\"", fixed = TRUE)
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    expect_match(html, "alt=\"Chart for ")
  }
})

test_that("the codebook carries the analysis plan and model summaries", {
  demo <- sframe_demo_data()
  cb <- codebook_report(demo$instrument)
  expect_named(cb$plan_table,
               c("id", "research_question", "method", "variables",
                 "decision_rule"))
  expect_named(cb$models_table,
               c("id", "label", "type", "engine", "n_constructs", "n_paths"))
  expect_gt(nrow(cb$plan_table), 0)

  old <- options(surveyframe.use_quarto = FALSE)
  on.exit(options(old), add = TRUE)
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  render_report(demo$instrument, output_file = out,
                include_analysis = FALSE, include_models = FALSE)
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "Pre-declared analysis plan")
})
