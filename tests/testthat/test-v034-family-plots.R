# tests/testthat/test-v034-family-plots.R
# v0.3.4 visualisation breadth: regression diagnostics, EFA scree/loadings,
# reliability plot, mosaic, correlation matrix heatmap, quality plot, and the
# shared dashboard/studio item and scale charts. Uses the bundled demo data
# (sframe_demo_data()) so results reflect a realistic multi-scale instrument.

test_that("regression diagnostics: run_analysis_plan attaches four ggplot panels", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  res <- run_analysis_plan(demo$responses, demo$instrument, plots = TRUE)
  reg <- Filter(function(r) identical(r$test, "regression_linear"), res)
  skip_if(length(reg) == 0, "demo plan has no regression_linear block")
  dp <- reg[[1]]$diagnostic_plots
  expect_type(dp, "list")
  expect_setequal(names(dp), c("residuals_fitted", "qq", "scale_location", "leverage"))
  expect_true(all(vapply(dp, inherits, logical(1), what = "ggplot")))
})

test_that("sframe_plot_regression_diagnostics returns NULL without a diagnostics data frame", {
  skip_if_not_installed("ggplot2")
  expect_null(sframe_plot_regression_diagnostics(list(test = "regression_linear")))
})

test_that("plot.sframe_reliability_report builds a ggplot with alpha and omega bars", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  scored <- score_scales(demo$responses, demo$instrument)
  rr <- reliability_report(scored, demo$instrument)
  gg <- plot(rr)
  expect_s3_class(gg, "ggplot")
})

test_that("plot.sframe_efa_report builds a scree plot with the suggested-factor marker", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  scored <- score_scales(demo$responses, demo$instrument)
  er <- efa_report(scored, demo$instrument)
  gg <- plot(er)
  expect_s3_class(gg, "ggplot")
})

test_that("plot.sframe_efa_solution builds a loadings heatmap", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  scored <- score_scales(demo$responses, demo$instrument)
  es <- efa_solution(scored, demo$instrument, nfactors = 2)
  gg <- plot(es)
  expect_s3_class(gg, "ggplot")
})

test_that("plot.sframe_quality_report builds a straight-lining flag-rate bar chart", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  qr <- quality_report(demo$responses, demo$instrument)
  gg <- plot(qr)
  expect_s3_class(gg, "ggplot")
})

test_that("report-level results (quality, reliability, EFA) gain a plot via run_analysis_plan", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  res <- run_analysis_plan(demo$responses, demo$instrument, plots = TRUE)
  by_test <- function(t) Filter(function(r) identical(r$test, t), res)
  for (t in c("quality", "reliability_alpha", "reliability_omega",
              "efa_readiness", "efa_solution")) {
    blocks <- by_test(t)
    skip_if(length(blocks) == 0, paste("no", t, "block in demo plan"))
    expect_s3_class(blocks[[1]]$plot, "ggplot")
  }
})

test_that("sframe_plot_correlation_matrix builds a heatmap from real response data", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  num_items <- unique(unlist(lapply(demo$instrument$scales, function(s) s$items)))
  num_items <- intersect(num_items, names(demo$responses))[1:4]
  gg <- sframe_plot_correlation_matrix(demo$responses, num_items)
  expect_s3_class(gg, "ggplot")
})

test_that("sframe_plot_correlation_matrix returns NULL on a runner error", {
  skip_if_not_installed("ggplot2")
  gg <- sframe_plot_correlation_matrix(data.frame(a = 1), c("a", "missing_col"))
  expect_null(gg)
})

test_that("sframe_draw_mosaic renders a crosstab result without error", {
  demo <- sframe_demo_data()
  res <- run_analysis_plan(demo$responses, demo$instrument, plots = FALSE)
  ct <- Filter(function(r) identical(r$test, "crosstab"), res)
  skip_if(length(ct) == 0, "demo plan has no crosstab block")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp, width = 500, height = 400)
  expect_no_error(sframe_draw_mosaic(ct[[1]]))
  grDevices::dev.off()
  expect_gt(file.size(tmp), 0)
})

test_that("sframe_draw_mosaic no-ops gracefully on an errored result", {
  expect_null(sframe_draw_mosaic(list(error = "no data")))
})

test_that("sframe_plot_item_chart covers choice, numeric, and unsupported item types", {
  skip_if_not_installed("ggplot2")
  likert_item <- list(type = "likert", label = "Satisfaction")
  cs <- list(values = 1:5, labels = c("SD", "D", "N", "A", "SA"))
  gg1 <- sframe_plot_item_chart(likert_item, sample(1:5, 40, replace = TRUE), cs)
  expect_s3_class(gg1, "ggplot")

  numeric_item <- list(type = "numeric", label = "Age")
  gg2 <- sframe_plot_item_chart(numeric_item, sample(18:70, 40, replace = TRUE))
  expect_s3_class(gg2, "ggplot")

  text_item <- list(type = "text", label = "Comments")
  expect_null(sframe_plot_item_chart(text_item, c("a", "b")))

  expect_null(sframe_plot_item_chart(numeric_item, c(NA, NA)))
})

test_that("sframe_plot_scale_chart draws a histogram with a mean line and handles empty input", {
  skip_if_not_installed("ggplot2")
  gg <- sframe_plot_scale_chart(rnorm(50, 3, 1), "Satisfaction")
  expect_s3_class(gg, "ggplot")
  expect_null(sframe_plot_scale_chart(c(NA_real_, NA_real_), "Empty"))
})

test_that("plot.sframe_descriptives_report builds a distribution-shape violin plot", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  dr <- descriptives_report(demo$responses)
  gg <- plot(dr, data = demo$responses)
  expect_s3_class(gg, "ggplot")
  expect_true(any(vapply(gg$layers, function(l) inherits(l$geom, "GeomViolin"), logical(1))))
})

test_that("sframe_plot_descriptives requires data and errors clearly without it", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  dr <- descriptives_report(demo$responses)
  expect_error(sframe_plot_descriptives(dr, data = list()), class = "sframe_error")
})

test_that("sframe_plot_descriptives facets by group when split_by is used", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  skip_if_not("visit_type" %in% names(demo$responses), "demo data has no visit_type column")
  dr <- descriptives_report(demo$responses, split_by = "visit_type")
  gg <- sframe_plot_descriptives(dr, data = demo$responses)
  expect_s3_class(gg, "ggplot")
  expect_true("FacetWrap" %in% class(gg$facet))
})

test_that("sframe_plot_descriptives returns NULL on an empty table", {
  skip_if_not_installed("ggplot2")
  empty <- structure(list(table = data.frame()), class = "sframe_descriptives_report")
  expect_null(sframe_plot_descriptives(empty, data = data.frame()))
})

test_that("sframe_plot_likert_matrix draws a grouped diverging chart for a matrix item", {
  skip_if_not_installed("ggplot2")
  cs <- sf_choices("ag5", 1:5, c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
  item <- sf_item("mx1", "Rate these", type = "matrix", choice_set = "ag5",
                  matrix_items = c("Row A", "Row B", "Row C"))
  set.seed(1)
  resp <- data.frame(check.names = FALSE,
    "mx1__Row A" = sample(1:5, 60, replace = TRUE),
    "mx1__Row B" = sample(1:5, 60, replace = TRUE),
    "mx1__Row C" = sample(1:5, 60, replace = TRUE)
  )
  gg <- sframe_plot_likert_matrix(item, resp, cs)
  expect_s3_class(gg, "ggplot")
  expect_null(sframe_plot_likert_matrix(item, data.frame(x = 1), cs))
})

test_that("sframe_likert_scale_groups finds scales whose items share a choice set", {
  cs <- sf_choices("ag5", 1:5, c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
  i1 <- sf_item("a_1", "Item A1", type = "likert", choice_set = "ag5", scale_id = "sa")
  i2 <- sf_item("a_2", "Item A2", type = "likert", choice_set = "ag5", scale_id = "sa")
  # A lone item on its own scale should not be grouped (nothing to group with).
  i3 <- sf_item("b_1", "Item B1", type = "likert", choice_set = "ag5", scale_id = "sb")
  scale_a <- sf_scale("sa", "Scale A", items = c("a_1", "a_2"))
  scale_b <- sf_scale("sb", "Scale B", items = c("b_1"))
  instr <- sf_instrument("Group test", components = list(cs, i1, i2, i3, scale_a, scale_b))

  groups <- sframe_likert_scale_groups(instr)
  expect_named(groups, "sa")
  expect_length(groups$sa$items, 2)
  expect_identical(groups$sa$title, "Scale A")
})

test_that("sframe_likert_scale_groups excludes a scale whose items use different choice sets", {
  ag5 <- sf_choices("ag5", 1:5, c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
  yn <- sf_choices("yn", c("yes", "no"), c("Yes", "No"))
  i1 <- sf_item("a_1", "Item A1", type = "likert", choice_set = "ag5", scale_id = "sa")
  i2 <- sf_item("a_2", "Item A2", type = "single_choice", choice_set = "yn", scale_id = "sa")
  scale_a <- sf_scale("sa", "Scale A", items = c("a_1", "a_2"))
  instr <- sf_instrument("Mixed scale", components = list(ag5, yn, i1, i2, scale_a))
  expect_length(sframe_likert_scale_groups(instr), 0)
})

test_that("sframe_plot_likert_scale draws a grouped diverging chart for a scale", {
  skip_if_not_installed("ggplot2")
  cs <- sf_choices("ag5", 1:5, c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
  i1 <- sf_item("a_1", "Item A1", type = "likert", choice_set = "ag5", scale_id = "sa")
  i2 <- sf_item("a_2", "Item A2", type = "likert", choice_set = "ag5", scale_id = "sa")
  set.seed(1)
  resp <- data.frame(a_1 = sample(1:5, 60, replace = TRUE), a_2 = sample(1:5, 60, replace = TRUE))
  gg <- sframe_plot_likert_scale(list(i1, i2), resp, cs, title = "Scale A")
  expect_s3_class(gg, "ggplot")
  expect_null(sframe_plot_likert_scale(list(i1, i2), data.frame(x = 1), cs, title = "Scale A"))
})

test_that("render_report groups a scale's Likert items into one chart, not one per item", {
  skip_if_not_installed("ggplot2")
  cs <- sf_choices("ag5", 1:5, c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
  i1 <- sf_item("a_1", "First scale item.", type = "likert", choice_set = "ag5", scale_id = "sa")
  i2 <- sf_item("a_2", "Second scale item.", type = "likert", choice_set = "ag5", scale_id = "sa")
  scale_a <- sf_scale("sa", "Scale A", items = c("a_1", "a_2"))
  instr <- sf_instrument("Grouped report test", components = list(cs, i1, i2, scale_a))
  set.seed(1)
  dat <- data.frame(
    respondent_id = paste0("R", 1:40),
    submitted_at = as.character(Sys.time()),
    a_1 = sample(1:5, 40, replace = TRUE),
    a_2 = sample(1:5, 40, replace = TRUE),
    stringsAsFactors = FALSE
  )
  old_opt <- options(surveyframe.use_quarto = FALSE)
  on.exit(options(old_opt), add = TRUE)
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  render_report(instr, dat, output_file = out)
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  # Item labels legitimately appear elsewhere (the codebook's Survey items
  # table lists every item regardless of charting), so scope the check to
  # the Response distributions section itself.
  start <- regexpr("<h2>Response distributions</h2>", html, fixed = TRUE)
  expect_gt(start, 0)
  dist_section <- substr(html, start, start + 20000)
  expect_match(dist_section, "Scale A", fixed = TRUE)
  expect_false(grepl("First scale item.", dist_section, fixed = TRUE))
  expect_false(grepl("Second scale item.", dist_section, fixed = TRUE))
})

test_that("descriptives results gain a plot via run_analysis_plan", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  res <- run_analysis_plan(demo$responses, demo$instrument, plots = TRUE)
  desc <- Filter(function(r) identical(r$test, "descriptives"), res)
  skip_if(length(desc) == 0, "demo plan has no descriptives block")
  expect_s3_class(desc[[1]]$plot, "ggplot")
})

test_that("group-comparison tests (t/Mann-Whitney/Kruskal-Wallis/ANOVA) gain a boxplot", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  res <- run_analysis_plan(demo$responses, demo$instrument, plots = TRUE)
  for (t in c("t_test_ind", "mann_whitney", "kruskal_wallis", "anova_one")) {
    blocks <- Filter(function(r) identical(r$test, t), res)
    skip_if(length(blocks) == 0, paste("no", t, "block in demo plan"))
    expect_s3_class(blocks[[1]]$plot, "ggplot")
  }
})

test_that("sframe_plot_group_comparison returns NULL for missing columns or a single group", {
  skip_if_not_installed("ggplot2")
  expect_null(sframe_plot_group_comparison(list(vars = c("a", "b")), data.frame(z = 1)))
  one_group <- data.frame(g = rep("x", 10), y = rnorm(10))
  expect_null(sframe_plot_group_comparison(list(vars = c("g", "y")), one_group))
})

test_that("paired tests (t_test_pair, wilcoxon_pair) gain a slope plot", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  res <- run_analysis_plan(demo$responses, demo$instrument, plots = TRUE)
  for (t in c("t_test_pair", "wilcoxon_pair")) {
    blocks <- Filter(function(r) identical(r$test, t), res)
    skip_if(length(blocks) == 0, paste("no", t, "block in demo plan"))
    expect_s3_class(blocks[[1]]$plot, "ggplot")
  }
})

test_that("sframe_plot_paired_comparison returns NULL with fewer than two complete pairs", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = c(1, NA), y = c(NA, 2))
  expect_null(sframe_plot_paired_comparison(list(vars = c("x", "y")), df))
})

test_that("sframe_plot_variable_distribution returns three ggplot panels", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  vd <- sframe_plot_variable_distribution(demo$responses, "dm_1")
  expect_setequal(names(vd), c("histogram", "boxplot", "qq"))
  expect_true(all(vapply(vd, inherits, logical(1), what = "ggplot")))
})

test_that("sframe_plot_variable_distribution returns NULL for a missing or degenerate column", {
  skip_if_not_installed("ggplot2")
  expect_null(sframe_plot_variable_distribution(data.frame(a = 1), "missing"))
  expect_null(sframe_plot_variable_distribution(data.frame(a = c(1, NA)), "a"))
})

test_that("plot_palette threads from run_analysis_plan to attached plots", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  res_p <- run_analysis_plan(demo$responses, demo$instrument,
                             plots = TRUE, plot_palette = "print")
  freq <- Filter(function(r) identical(r$test, "frequency"), res_p)
  skip_if(length(freq) == 0, "demo plan has no frequency block")
  # The print palette fills bars with the light print grey, never the teal.
  built <- ggplot2::ggplot_build(freq[[1]]$plot)
  fills <- unique(unlist(lapply(built$data, function(d) d$fill)))
  fills <- fills[!is.na(fills)]
  expect_true(any(grepl("#cccccc", fills, ignore.case = TRUE)))
  expect_false(any(grepl("#0E9694", fills, ignore.case = TRUE)))
  expect_error(
    run_analysis_plan(demo$responses, demo$instrument,
                      plots = TRUE, plot_palette = "neon"),
    "web|print"
  )
})

test_that("render_report accepts plot_palette and renders with the print theme", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  out <- tempfile(fileext = ".html")
  old <- options(surveyframe.use_quarto = FALSE)
  on.exit({options(old); unlink(out)}, add = TRUE)
  render_report(demo$instrument, demo$responses, output_file = out,
                plot_palette = "print")
  expect_true(file.exists(out))
  expect_true(grepl("<img", paste(readLines(out, warn = FALSE), collapse = "")))
})

test_that("render_report embeds regression diagnostic panels beside the main chart", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  old_opt <- options(surveyframe.use_quarto = FALSE)
  on.exit(options(old_opt), add = TRUE)
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  render_report(demo$instrument, demo$responses, output_file = out)
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  n_images <- lengths(regmatches(html, gregexpr("data:image/png;base64", html)))
  expect_gt(n_images, 30) # item distributions + family plots + 4 diagnostic panels
})

test_that("plot.sframe_validity_report builds CR and AVE bars by construct", {
  skip_if_not_installed("ggplot2")
  vr <- validity_report(list(
    SAT = c(sat_1 = 0.82, sat_2 = 0.78, sat_3 = 0.74),
    LOY = c(loy_1 = 0.69, loy_2 = 0.71)
  ))
  gg <- plot(vr)
  expect_s3_class(gg, "ggplot")
  built <- ggplot2::ggplot_build(gg)
  # 2 constructs x 2 statistics = 4 bars
  expect_identical(nrow(built$data[[3]]), 4L)
  gg_print <- plot(vr, palette = "print")
  expect_s3_class(gg_print, "ggplot")
})

test_that("plot.sframe_analysis_results draws attached charts and selects by which", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  results <- run_analysis_plan(demo$responses, demo$instrument, plots = TRUE)

  # which by RQ number: the first block with a plot returns that ggplot
  with_plot <- which(vapply(results, function(r) !is.null(r$plot), logical(1)))
  expect_gt(length(with_plot), 0)
  gg <- plot(results, which = with_plot[[1]])
  expect_s3_class(gg, "ggplot")

  # which by block id
  bid <- results[[with_plot[[1]]]]$block_id
  expect_s3_class(plot(results, which = bid), "ggplot")

  # default: all charts print and return invisibly, keyed by block id
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp, width = 600, height = 400)
  all_plots <- plot(results)
  grDevices::dev.off()
  expect_identical(length(all_plots), length(with_plot))
  expect_true(bid %in% names(all_plots))

  # errors: unknown id, out-of-range number, chartless block
  expect_error(plot(results, which = "no_such_block"), class = "sframe_error")
  expect_error(plot(results, which = 999), class = "sframe_error")
  without_plot <- which(vapply(results, function(r) is.null(r$plot), logical(1)))
  if (length(without_plot) > 0) {
    expect_error(plot(results, which = without_plot[[1]]), class = "sframe_error")
  }
})

test_that("plot.sframe_analysis_results aborts when no charts were requested", {
  demo <- sframe_demo_data()
  results <- run_analysis_plan(demo$responses, demo$instrument)
  expect_error(plot(results), class = "sframe_error")
})

test_that("plot.sframe_missing_data_report builds a missingness bar chart", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  resp <- demo$responses
  item_cols <- vapply(demo$instrument$items, `[[`, character(1), "id")
  item_cols <- intersect(item_cols, names(resp))
  resp[[item_cols[1]]][1:5] <- NA
  mr <- missing_data_report(resp, demo$instrument)
  gg <- plot(mr)
  expect_s3_class(gg, "ggplot")
  expect_s3_class(sframe_plot_missingness(mr, palette = "print"), "ggplot")

  # Nothing missing: still a chart (a reassuring "no missing responses"
  # message), not a silent NULL that reads as a rendering failure.
  mr_clean <- missing_data_report(demo$responses, demo$instrument)
  none_missing <- all(mr_clean$item_missing$missing_pct == 0)
  if (none_missing) expect_s3_class(plot(mr_clean), "ggplot")
})

test_that("every block in the demo analysis plan gets a table, a plot, or generated syntax", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  res <- run_analysis_plan(demo$responses, demo$instrument, plots = TRUE)
  has_output <- vapply(res, function(r) {
    !is.null(r$table) || !is.null(r$plot) || !is.null(r$syntax)
  }, logical(1))
  missing <- vapply(res[!has_output], function(r) r$block_id %||% r$test, character(1))
  expect_true(all(has_output),
             info = paste("blocks with no table, plot, or syntax:", paste(missing, collapse = ", ")))
})

test_that("repeated_anova computes a real F/p table instead of only fit_summary text", {
  demo <- sframe_demo_data()
  res <- run_analysis_plan(demo$responses, demo$instrument)
  ra <- Filter(function(r) identical(r$test, "repeated_anova"), res)
  skip_if(length(ra) == 0, "demo plan has no repeated_anova block")
  r <- ra[[1]]
  expect_s3_class(r$table, "data.frame")
  expect_true(all(c("df1", "df2", "Estimate", "p") %in% colnames(r$table)))
  expect_match(r$apa, "^F\\(")
})

test_that("friedman, partial_correlation, and mediation tables are populated", {
  demo <- sframe_demo_data()
  # partial_correlation and mediation reference scale-score variables, which
  # only exist when scoring runs (the default); scored = FALSE was fine for
  # the item-level tests above but breaks these two with a missing-column
  # error, not a real "no table" gap.
  res <- run_analysis_plan(demo$responses, demo$instrument)
  by_test <- function(t) Filter(function(r) identical(r$test, t), res)

  fr <- by_test("friedman")
  skip_if(length(fr) == 0, "demo plan has no friedman block")
  expect_true("Kendall's W" %in% colnames(fr[[1]]$table))

  pc <- by_test("partial_correlation")
  skip_if(length(pc) == 0, "demo plan has no partial_correlation block")
  expect_true(all(c("Estimate", "Controls") %in% colnames(pc[[1]]$table)))

  med <- by_test("mediation")
  skip_if(length(med) == 0, "demo plan has no mediation block")
  expect_identical(med[[1]]$table$Effect,
                   c("Direct (c')", "Indirect (a×b)", "Total (c)"))
})

test_that("moderation, logistic, and repeated-measures plots draw", {
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  res <- run_analysis_plan(demo$responses, demo$instrument, plots = TRUE)
  by_test <- function(t) Filter(function(r) identical(r$test, t), res)

  for (t in c("moderation", "mediation", "regression_logistic_binary",
              "regression_logistic_ordinal", "repeated_anova", "friedman",
              "partial_correlation", "ancova", "fisher_exact", "item_diagnostics")) {
    blocks <- by_test(t)
    skip_if(length(blocks) == 0, paste("demo plan has no", t, "block"))
    expect_s3_class(blocks[[1]]$plot, "ggplot")
  }
})

test_that("syntax-only blocks (CFA/SEM/PLS-SEM) carry generated syntax text", {
  demo <- sframe_demo_data()
  res <- run_analysis_plan(demo$responses, demo$instrument, scored = FALSE)
  for (t in c("cfa_lavaan_syntax", "sem_lavaan_syntax", "seminr_syntax")) {
    blocks <- Filter(function(r) identical(r$test, t), res)
    skip_if(length(blocks) == 0, paste("demo plan has no", t, "block"))
    expect_true(nzchar(blocks[[1]]$syntax %||% ""))
  }
})
