# Batch 4 #15 and #16: how the Quarto renderer is invoked, and what the report
# claims about its own provenance.
#
# #15. Parameter values were joined into "-P name:value" strings and handed to
#      system2() unquoted, so a path containing a space split into several
#      shell arguments. Every character value was also run through
#      normalizePath(), including the ones that are no path at all, so the
#      no-analysis seed text became an absolute path.
# #16. sframe_report_seed() asked only whether data and a plan existed, so a
#      report rendered with include_analysis = FALSE still printed a seed
#      beside the instrument hash, implying an analysis that never ran.

seed_instrument <- function(with_plan = TRUE) {
  instr <- sf_instrument("Seeded", components = list(
    sf_item("q1", "A number", type = "numeric")))
  if (with_plan) {
    instr$analysis_plan <- list(list(
      id = "RQ1", research_question = "What is the average?",
      family = "descriptive", method = "descriptives",
      roles = list(variables = "q1")))
  }
  instr
}

test_that("15: every parameter argument survives a space", {
  params <- list(
    instrument_path = "/tmp/a folder/instr.rds",
    include_codebook = TRUE,
    analysis_seed = "not applicable, no analysis plan run")
  args <- sframe_quarto_param_args(params)

  # one argument per parameter, each paired with its own -P
  expect_equal(sum(args == "-P"), length(params))
  # the value travels as a single argument, whatever it contains
  seed_arg <- grep("analysis_seed", args, value = TRUE)
  expect_length(seed_arg, 1)
  expect_match(seed_arg, "not applicable, no analysis plan run", fixed = TRUE)
  # and it is quoted, so the shell keeps it whole
  expect_match(seed_arg, "^[\"'].*[\"']$|^'.*'$")
})

test_that("15: only the path parameters are normalised", {
  # normalizePath() used to run over every character value. On Linux it
  # returns a non-existent relative path unchanged, so the effect is invisible
  # here; on Windows it rewrites separators and can prepend the working
  # directory. The rule is what this pins: the 3 parameters that name a file,
  # and no others.
  expect_setequal(sframe_quarto_param_names_path,
                  c("instrument_path", "data_path", "interpretations_path"))

  params <- list(
    instrument_path = "instr.rds",
    analysis_seed = "not applicable, no analysis plan run",
    plot_palette = "viridis")
  args <- sframe_quarto_param_args(params)
  # the values that are no path travel as they were written
  expect_match(grep("plot_palette", args, value = TRUE), "viridis", fixed = TRUE)
  expect_match(grep("analysis_seed", args, value = TRUE),
               "not applicable, no analysis plan run", fixed = TRUE)
})

test_that("15: a logical is passed as quarto reads it", {
  args <- sframe_quarto_param_args(list(include_codebook = TRUE,
                                        include_analysis = FALSE))
  expect_true(any(grepl("include_codebook:true", args, fixed = TRUE)))
  expect_true(any(grepl("include_analysis:false", args, fixed = TRUE)))
})

test_that("16: a report that ran no analysis claims no seed", {
  instr <- seed_instrument()
  data <- data.frame(q1 = c(1, 2, 3))

  # analysis was asked for and could run
  expect_false(identical(
    sframe_report_seed(data, instr, include_analysis = TRUE),
    "not applicable, no analysis plan run"))

  # analysis was switched off, so there is no seed to report
  expect_equal(sframe_report_seed(data, instr, include_analysis = FALSE),
               "not applicable, no analysis plan run")
})

test_that("16: the existing reasons for no seed still hold", {
  instr <- seed_instrument()
  expect_equal(sframe_report_seed(NULL, instr, include_analysis = TRUE),
               "not applicable, no analysis plan run")
  expect_equal(
    sframe_report_seed(data.frame(q1 = 1), seed_instrument(with_plan = FALSE),
                       include_analysis = TRUE),
    "not applicable, no analysis plan run")
})
