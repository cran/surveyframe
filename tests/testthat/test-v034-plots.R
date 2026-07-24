# tests/testthat/test-v034-plots.R
# v0.3.4 visualisation foundation: theme_surveyframe(), plots = TRUE on
# run_analysis_plan(), and $table on the inferential runners.

make_v034_instrument <- function() {
  cs <- sf_choices("ag5", 1:5,
    c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
  yn <- sf_choices("yn", c("yes", "no"), c("Yes", "No"))
  i1 <- sf_item("sat_1", "The service was fast.",
    type = "likert", choice_set = "ag5", scale_id = "sat", required = TRUE)
  i2 <- sf_item("sat_2", "The staff were helpful.",
    type = "likert", choice_set = "ag5", scale_id = "sat", required = TRUE)
  age <- sf_item("age", "What is your age?", type = "numeric")
  gen <- sf_item("gender", "Gender?", type = "single_choice", choice_set = "yn")
  grp <- sf_item("member", "Member?", type = "single_choice", choice_set = "yn")
  scale <- sf_scale("sat", "Satisfaction", items = c("sat_1", "sat_2"),
                    method = "mean")
  plan <- list(
    list(id = "RQF", research_question = "How old are respondents?",
         family = "descriptive", method = "frequency",
         roles = list(variable = "gender")),
    list(id = "RQC", research_question = "Does satisfaction rise with age?",
         family = "association", method = "correlation_pearson",
         roles = list(x = "age", y = "sat_1")),
    list(id = "RQX", research_question = "Is gender associated with membership?",
         family = "categorical", method = "chi_square",
         roles = list(row = "gender", column = "member")),
    list(id = "RQR", research_question = "Does age predict satisfaction?",
         family = "regression", method = "regression_linear",
         roles = list(dependent = "sat_1", predictors = "age")),
    list(id = "RQT", research_question = "Do members differ on satisfaction?",
         family = "group_comparison", method = "t_test_ind",
         roles = list(group = "member", outcome = "sat_1"))
  )
  sf_instrument("Plot test survey",
                components = list(cs, yn, i1, i2, age, gen, grp, scale),
                analysis_plan = plan)
}

make_v034_responses <- function(n = 60, seed = 7) {
  set.seed(seed)
  age <- sample(18:70, n, replace = TRUE)
  data.frame(
    respondent_id = paste0("R", seq_len(n)),
    submitted_at = as.character(Sys.time()),
    sat_1 = pmin(5, pmax(1, round(2 + age / 25 + rnorm(n, 0, 0.8)))),
    sat_2 = sample(1:5, n, replace = TRUE),
    age = age,
    gender = sample(c("yes", "no"), n, replace = TRUE),
    member = sample(c("yes", "no"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("theme_surveyframe returns a ggplot2 theme", {
  skip_if_not_installed("ggplot2")
  th <- theme_surveyframe()
  expect_s3_class(th, "theme")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() + th
  expect_s3_class(p, "ggplot")
})

test_that("run_analysis_plan attaches brand plots when plots = TRUE", {
  skip_if_not_installed("ggplot2")
  instr <- make_v034_instrument()
  dat <- make_v034_responses()
  res <- run_analysis_plan(dat, instr, scored = FALSE, plots = TRUE)

  by_id <- function(id) res[[which(vapply(res, function(r) r$block_id, "") == id)]]
  expect_s3_class(by_id("RQF")$plot, "ggplot")
  expect_s3_class(by_id("RQC")$plot, "ggplot")
  expect_s3_class(by_id("RQX")$plot, "ggplot")
  expect_s3_class(by_id("RQR")$plot, "ggplot")
  # Group-comparison boxplot added in the v0.3.4 breadth work
  # (sframe_plot_group_comparison()); t-test was outside the original
  # first plot family shipped in 0.3.3, no longer outside it now.
  expect_s3_class(by_id("RQT")$plot, "ggplot")
  expect_null(by_id("RQT")$error)
})

test_that("plots default to off and results carry no plot element", {
  instr <- make_v034_instrument()
  dat <- make_v034_responses()
  res <- run_analysis_plan(dat, instr, scored = FALSE)
  expect_true(all(vapply(res, function(r) is.null(r$plot), logical(1))))
})

test_that("inferential runners gain a kable-ready $table", {
  instr <- make_v034_instrument()
  dat <- make_v034_responses()
  res <- run_analysis_plan(dat, instr, scored = FALSE)
  by_id <- function(id) res[[which(vapply(res, function(r) r$block_id, "") == id)]]

  cor_tab <- by_id("RQC")$table
  expect_s3_class(cor_tab, "data.frame")
  expect_identical(cor_tab$Statistic, "Pearson r")
  expect_true(all(c("n", "df", "Estimate", "p") %in% colnames(cor_tab)))

  reg_tab <- by_id("RQR")$table
  expect_s3_class(reg_tab, "data.frame")
  expect_true("(Intercept)" %in% reg_tab$Term)
  # Term is humanised to the item's label ("age" -> its question text), so
  # the analysis output reads without needing the underlying variable id.
  expect_true("What is your age?" %in% reg_tab$Term)
  expect_false("age" %in% reg_tab$Term)

  t_tab <- by_id("RQT")$table
  expect_s3_class(t_tab, "data.frame")
  expect_identical(nrow(t_tab), 2L)
  expect_true(all(c("Group", "n", "Mean", "SD") %in% colnames(t_tab)))

  # frequency keeps its existing table untouched
  expect_true(all(c("Value", "Frequency", "Percent") %in%
                    colnames(by_id("RQF")$table)))
})

test_that("sframe_draw_likert_diverging renders without ggplot2", {
  counts5 <- c("Strongly disagree" = 8, "Disagree" = 12, "Neutral" = 15,
              "Agree" = 40, "Strongly agree" = 25)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp, width = 400, height = 200)
  expect_no_error(sframe_draw_likert_diverging(counts5, "#16B3B1"))
  grDevices::dev.off()
  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)

  # Even scale, no neutral category
  counts4 <- c("Strongly disagree" = 30, "Disagree" = 25,
              "Agree" = 20, "Strongly agree" = 15)
  grDevices::png(tmp, width = 400, height = 200)
  expect_no_error(sframe_draw_likert_diverging(counts4, "#7c3aed"))
  grDevices::dev.off()

  # Degenerate input does not error
  grDevices::png(tmp, width = 400, height = 200)
  expect_no_error(sframe_draw_likert_diverging(c(a = 0, b = 0), "#000"))
  grDevices::dev.off()
})

test_that("report distributions and analysis sections attach a plot beside its table", {
  skip_if_not_installed("ggplot2")
  cs <- sf_choices("ag5", 1:5,
    c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
  i1 <- sf_item("sat_1", "The service was fast.", type = "likert",
                choice_set = "ag5", required = TRUE)
  instr <- sf_instrument("Report combined check",
    components = list(cs, i1),
    analysis_plan = list(list(
      id = "RQ1", research_question = "Distribution of sat_1",
      family = "descriptive", method = "frequency",
      roles = list(variable = "sat_1")
    )))
  set.seed(1)
  dat <- data.frame(
    respondent_id = paste0("R", 1:40),
    submitted_at = as.character(Sys.time()),
    sat_1 = sample(1:5, 40, replace = TRUE),
    stringsAsFactors = FALSE
  )
  old_opt <- options(surveyframe.use_quarto = FALSE)
  on.exit(options(old_opt), add = TRUE)
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  render_report(instr, dat, output_file = out)
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "Response distributions")
  expect_match(html, "data:image/png;base64")
  # The RQ1 block's table and its chart appear as one contiguous unit
  rq_start <- regexpr("RQ 1", html)
  rq_chunk <- substr(html, rq_start, rq_start + 1200)
  expect_match(rq_chunk, "Results table")
  expect_match(rq_chunk, "data:image/png;base64")
})
