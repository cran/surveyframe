# tests/testthat/test-report-reproducibility.R
# Guards the reproducibility of a rendered report. Bootstrap intervals and the
# EFA parallel analysis once drew from the global RNG unseeded, so the same
# data gave a different interval on every run while the test statistic and p
# value stayed put, which is why nothing looked wrong.

plan_instrument <- function() {
  sf_instrument(
    title = "Repro", version = "1.0.0",
    components = list(
      sf_choices("grp", c("a", "b"), c("A", "B")),
      sf_item("g", "Group", type = "single_choice", choice_set = "grp"),
      sf_item("y", "Outcome", type = "numeric")
    ),
    analysis_plan = list(list(
      id = "RQ1", research_question = "Do the groups differ?",
      family = "group_comparison", method = "t_test_ind",
      roles = list(outcome = "y", group = "g")
    ))
  )
}

plan_data <- function(n = 60) {
  set.seed(7)
  d <- data.frame(
    respondent_id = sprintf("r%02d", seq_len(n)),
    g = rep(c("a", "b"), length.out = n),
    y = c(rnorm(n / 2, 10, 3), rnorm(n / 2, 12, 3)),
    stringsAsFactors = FALSE
  )
  read_responses(d, plan_instrument(), respondent_id = "respondent_id")
}

test_that("run_analysis_plan() gives identical results on identical inputs", {
  instr <- plan_instrument()
  dat <- plan_data()
  a <- capture.output(str(run_analysis_plan(dat, instr), max.level = 7))
  b <- capture.output(str(run_analysis_plan(dat, instr), max.level = 7))
  expect_identical(a, b)
})

test_that("the bootstrap interval itself is stable, not just the statistic", {
  # The defect moved only the interval, so a test on the statistic alone would
  # have passed throughout.
  instr <- plan_instrument()
  dat <- plan_data()
  ci <- function() run_analysis_plan(dat, instr)[[1]]$d_ci
  expect_identical(ci(), ci())
})

test_that("seed = NULL restores the pre-0.4.1 unseeded behaviour", {
  instr <- plan_instrument()
  dat <- plan_data()
  # Not an assertion that the 2 differ, which would be a coin flip on a small
  # bootstrap. The contract is that the argument is honoured and the call works.
  expect_no_error(run_analysis_plan(dat, instr, seed = NULL))
  expect_null(attr(run_analysis_plan(dat, instr, seed = NULL), "seed"))
  expect_identical(attr(run_analysis_plan(dat, instr), "seed"),
                   formals(run_analysis_plan)$seed)
})

test_that("seeding does not disturb the caller's RNG stream", {
  # sframe_with_seed() restores whatever state the caller had, so a seeded plan
  # run does not silently reset randomness for unrelated code afterwards.
  instr <- plan_instrument()
  dat <- plan_data()
  set.seed(99); before <- runif(3)
  invisible(run_analysis_plan(dat, instr))
  set.seed(99); after <- runif(3)
  expect_identical(before, after)
})

test_that("seeding leaves mc.cores as it found it", {
  # fa.parallel() splits across mclapply() workers, whose forked RNG streams
  # set.seed() does not reach, and which split by core count so the answer
  # would depend on the machine. sframe_with_seed() forces serial execution
  # and must put the option back.
  before <- getOption("mc.cores")
  invisible(surveyframe:::sframe_with_seed(1L, stats::runif(1)))
  expect_identical(getOption("mc.cores"), before)
})

test_that("the EFA path is deterministic too, not just the bootstraps", {
  # This is the mclapply half of the defect and needs an instrument with
  # scales, because psych::fa.parallel() is what splits across forked workers.
  # A fixture without an EFA path passes whether or not mc.cores is pinned, so
  # the bundled demo is used deliberately.
  skip_on_cran()
  d <- sframe_demo_data()
  once <- function() {
    r <- run_analysis_plan(d$responses, d$instrument)
    capture.output(str(r, max.level = 7))
  }
  expect_identical(once(), once())
})

test_that("chart jitter is seeded, so the pictures settle as well as the numbers", {
  # Found after the analysis was seeded: 5 lines of the rendered report still
  # moved between renders, all of them embedded charts. geom_jitter() drew its
  # offsets from the global stream at plot time, which is after
  # run_analysis_plan() restores the caller's state, so a report was
  # reproducible in its tables and not in its figures.
  #
  # Built directly from a result-shaped list so the test exercises the plot
  # builder itself and can never skip on a fixture's column names.
  skip_if_not_installed("ggplot2")
  set.seed(1)
  dat <- data.frame(
    grp = rep(c("a", "b"), each = 25),
    out = c(stats::rnorm(25, 10), stats::rnorm(25, 12))
  )
  res <- list(test = "t_test_ind", vars = c("grp", "out"), apa = "t = 1")

  build <- function() {
    p <- sframe_plot_group_comparison(res, dat)
    ggplot2::ggplot_build(p)$data
  }
  expect_s3_class(sframe_plot_group_comparison(res, dat), "ggplot")
  expect_identical(build(), build())

  # and the jitter really is applied, so the check is about a seeded jitter
  # and not about a plot that never moved
  layers <- ggplot2::ggplot_build(sframe_plot_group_comparison(res, dat))$data
  expect_true(length(layers) >= 2)
  expect_false(identical(layers[[2]]$x, as.numeric(factor(dat$grp))))
})

test_that("render_report() says which engine produced the file", {
  skip_on_cran()
  instr <- plan_instrument()
  dat <- plan_data()
  old <- options(surveyframe.use_quarto = FALSE)
  on.exit(options(old), add = TRUE)
  # expect_message() returns the condition, not the value, so the path is
  # captured by assigning inside the expectation.
  p <- NULL
  expect_message(
    p <- render_report(instr, dat, output_path = tempfile(fileext = ".html")),
    "built-in HTML engine"
  )
  expect_identical(attr(p, "engine"), "html")

  # and the artefact names its own engine and seed, for a reader who has
  # neither the console message nor the return value
  body <- paste(readLines(p, warn = FALSE), collapse = "\n")
  expect_match(body, "Rendering engine", fixed = TRUE)
  expect_match(body, "Analysis seed", fixed = TRUE)
})

test_that("a missing straight-lining chart says why", {
  # Regression from 0.4.1's own straightline_min_items fix: every scale in an
  # instrument of short scales comes back checked = FALSE, the plot drops the
  # NA rows and returns NULL, and the chart vanished with nothing said.
  d <- sframe_demo_data()
  q_default <- quality_report(d$responses, d$instrument)
  expect_null(sframe_plot_quality(q_default))
  note <- sframe_quality_plot_note(q_default)
  expect_type(note, "character")
  expect_match(note, "long enough to check", fixed = TRUE)

  # once a scale is checkable there is a chart, and so no note
  q_low <- quality_report(d$responses, d$instrument, straightline_min_items = 2)
  expect_s3_class(sframe_plot_quality(q_low), "ggplot")
  expect_null(sframe_quality_plot_note(q_low))
})
