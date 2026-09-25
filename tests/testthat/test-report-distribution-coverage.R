# Batch 4 #17 and #18: what the distribution section does when an optional
# package is missing, and which palette its figures use.
#
# #17. The report grouped a scale's Likert items into one chart, and every
#      member item hit `next` whether or not that chart was drawn. Without
#      ggplot2 no grouped chart is possible, so those items' distributions
#      disappeared, though the base-graphics single-item chart can draw each
#      one. Both report engines did this.
# #18. The single-item diverging chart was called without `plot_palette`, so a
#      report asked for the print palette mixed monochrome grouped figures with
#      colour single-item ones.

engine_sources <- function() {
  list(
    r = sframe_source_text("R", "reporting.R"),
    qmd = sframe_installed_text("inst", "templates",
                                    "report.qmd"))
}

test_that("18: both engines forward the palette to the diverging chart", {
  src <- engine_sources()
  expect_match(src$r, "sframe_draw_likert_diverging(freq, theme,", fixed = TRUE)
  expect_match(src$r, "palette = plot_palette", fixed = TRUE)
  expect_match(src$qmd,
               "sframe_draw_likert_diverging(freq, THEME, palette = plot_palette)",
               fixed = TRUE)
})

test_that("17: a grouped item falls through when ggplot2 is absent", {
  src <- engine_sources()
  # the skip is conditional on a grouped chart being possible
  expect_match(src$r, "if (has_ggplot) {", fixed = TRUE)
  expect_match(src$qmd, "if (item$id %in% names(group_of) && has_ggplot) {",
               fixed = TRUE)
  # and neither engine skips a grouped member unconditionally any more
  expect_false(grepl("if (gid %in% rendered_groups || !has_ggplot) next",
                     src$qmd, fixed = TRUE))
  expect_false(grepl("if (!gid %in% rendered_groups && has_ggplot) {",
                     src$r, fixed = TRUE))
})

test_that("17: every Likert item in a scale is drawn, with ggplot2 present", {
  skip_on_cran()  # renders a report or runs a full plan: slow on CRAN's machines
  skip_if_not_installed("ggplot2")
  demo <- sframe_demo_data()
  groups <- sframe_likert_scale_groups(demo$instrument)
  skip_if(length(groups) == 0, "no grouped scale in the demo")

  out <- tempfile(fileext = ".html")
  suppressMessages(render_report(
    demo$instrument, data = demo$responses, output_file = out,
    include_analysis = FALSE, include_models = FALSE,
    plot_palette = "print"))
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  # the grouped chart's own heading appears once per group
  for (g in groups) {
    expect_true(grepl(g$title, html, fixed = TRUE))
  }
})

# Batch 4 #23: the quanteda DFM runner's own table carries the response count,
# feature count and sparsity, and the leading features it asks a reader to
# review sit in $top_features. Neither report engine rendered that table, so the
# prompt asked for something the report did not supply.

test_that("23: both engines render a result's supplementary table", {
  src <- engine_sources()
  # one shared helper decides what a result shows besides its main table, so
  # the 2 engines cannot drift on it
  expect_match(src$r, "sframe_result_supplement(result)", fixed = TRUE)
  expect_match(src$qmd, "sframe_result_supplement(r)", fixed = TRUE)
  # and the quanteda runner's leading features are one of the things it names
  helper <- sframe_source_text("R", "analysis_plan.R")
  expect_match(helper, "Leading features", fixed = TRUE)
  expect_match(helper, "result$top_features", fixed = TRUE)
})

test_that("23: the leading features reach a rendered report", {
  skip_on_cran()  # renders a report or runs a full plan: slow on CRAN's machines
  skip_if_not_installed("quanteda")
  demo <- sframe_demo("open_text")
  skip_if(is.null(demo$instrument), "no open_text demo")

  instr <- demo$instrument
  # a real open-text item, rather than the first character column, which is
  # the respondent id
  text_col <- "what_worked"
  skip_if(!text_col %in% names(demo$responses), "no open-text column")
  sf_plan(instr) <- list(list(
    id = "RQ1", research_question = "What features lead?",
    family = "text", method = "quanteda_dfm",
    roles = list(item = text_col)))

  out <- tempfile(fileext = ".html")
  suppressMessages(render_report(instr, data = demo$responses,
                                 output_file = out, include_models = FALSE))
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "Leading features", fixed = TRUE)
})

# The Quarto template runs in a separate R session against the INSTALLED
# package, so every function it calls has to be exported. A template calling an
# internal through ::: fails there, and render_report() falls back to the
# built-in HTML engine without saying why, which batch 4 #15 describes. That
# fallback produces equivalent content, so no content assertion catches it:
# adding a call to an internal silently cost the Quarto path, and did until
# this test existed.

test_that("the Quarto template calls only exported functions", {
  qmd <- sframe_installed_text("inst", "templates",
                                   "report.qmd")
  expect_false(grepl("surveyframe:::", qmd, fixed = TRUE))
})

# Every surveyframe function the template names has to be exported by the
# INSTALLED package, because that is the namespace the Quarto session loads.
# This is the invariant behind the ::: failure above, and it also catches a
# newly exported function the template uses before the package is reinstalled,
# which is what a developer hits.
template_surveyframe_calls <- function() {
  lines <- readLines(sframe_installed_path("inst", "templates", "report.qmd"),
                     warn = FALSE)
  # A comment names a function to explain why it is avoided, so stripping
  # comments is what keeps this a check on calls rather than on prose.
  qmd <- paste(sub("#.*$", "", lines), collapse = "\n")
  called <- unique(unlist(regmatches(
    qmd, gregexpr("\\b(sframe_[a-z_0-9]+|analysis_syntax|score_scales|read_sframe|read_responses|codebook_report|quality_report|reliability_report|item_report|efa_report|validity_report|run_analysis_plan|descriptives_report|missing_data_report)\\b",
                  qmd, perl = TRUE))))
  # names used as local variables in the template rather than calls
  setdiff(called, c("sframe_demo", "sframe_demos"))
}

# What the INSTALLED package exports, read from its NAMESPACE on disk.
# getNamespaceExports() is no use here: under pkgload it answers for the tree
# being developed, which is exactly the namespace the Quarto session does NOT
# see.
installed_surveyframe_exports <- function() {
  for (lib in .libPaths()) {
    ns <- file.path(lib, "surveyframe", "NAMESPACE")
    if (file.exists(ns)) {
      lines <- readLines(ns, warn = FALSE)
      out <- sub("^export\\((.*)\\)$", "\\1",
                 grep("^export\\(", lines, value = TRUE))
      return(gsub('"', "", out))
    }
  }
  NULL
}

test_that("every surveyframe function the template calls is exported here", {
  called <- template_surveyframe_calls()
  expect_gt(length(called), 0)
  dev_exports <- sframe_exports()
  expect_equal(setdiff(called, dev_exports), character(0))
})

test_that("the Quarto engine actually renders, where the installed package can", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("quarto")), "Quarto is not installed")
  skip_if_not_installed("surveyframe")

  # The Quarto session loads the installed package, so a template calling a
  # function this tree exports and the installed one lacks cannot render here.
  # That is a reinstall away, rather than a defect, so it skips.
  exported <- installed_surveyframe_exports()
  skip_if(is.null(exported), "surveyframe is not installed in any library")
  missing <- setdiff(template_surveyframe_calls(), exported)
  skip_if(length(missing) > 0,
          paste0("the installed surveyframe lacks: ",
                 paste(missing, collapse = ", ")))

  demo <- sframe_demo_data()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  old <- options(surveyframe.use_quarto = TRUE)
  on.exit(options(old), add = TRUE)

  res <- suppressMessages(render_report(
    demo$instrument, data = demo$responses, output_file = out,
    include_models = FALSE))
  # the engine attribute, rather than the content, which the fallback matches
  expect_equal(attr(res, "engine"), "quarto")
})
