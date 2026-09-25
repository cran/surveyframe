# tests/testthat/test-doc-contracts.R
#
# Documented contracts that differed from the code, batch 8.
#
# G1 (#1).  The accessor help said the component accessors return an
#           sf_component_list and coercion gives the same content flat. On a
#           codebook they return data frames, and coercing an instrument gives
#           its items alone.
# G6 (#6).  The static-export help promised a CSV download on every submission
#           and said a failed POST cannot lose a response. The template keeps
#           the CSV in memory, offers it as a button, and moves on from a
#           failed POST without reporting it.
# G10 (#20). The Studio help advertised a screen for building a survey draft.
#           Studio has 9 screens and none of them authors an instrument, which
#           launch_builder() does. `screen = "build"` fell through to another
#           screen, and the real "amendments" screen could not be asked for.

source_of <- function(file) {
  p <- sframe_source_path("R", file)
  skip_if(is.na(p), paste("no source tree for", file))
  skip_if(!file.exists(p), "package source not available")
  paste(readLines(p, warn = FALSE), collapse = "\n")
}

demo_instrument <- function() {
  sf_instrument(title = "Docs", components = list(
    sf_choices("ag5", 1:5, as.character(1:5)),
    sf_item("q1", "One", "likert", choice_set = "ag5", scale_id = "s"),
    sf_scale("s", "S", items = "q1")))
}

test_that("G1: accessors return what the help says, per class", {
  instr <- demo_instrument()
  book <- codebook_report(instr)

  expect_s3_class(sf_items(instr), "sf_component_list")
  expect_s3_class(sf_scales(instr), "sf_component_list")
  expect_true(is.data.frame(sf_items(book)))
  expect_true(is.data.frame(sf_plan(book)))
  expect_true(is.list(sf_meta(instr)))
  # coercing an instrument gives its items table, and a component list has no
  # coercion at all, so the help cannot promise a flat table of the same content
  expect_true(all(c("id", "label", "type") %in% names(as.data.frame(instr))))
  expect_false("values" %in% names(as.data.frame(instr)))
  expect_error(as.data.frame(sf_items(instr)), "coerce")
})

test_that("G1: the accessor help states the codebook return type and the coercion limit", {
  src <- source_of("accessors.R")
  expect_match(src, "data frame", fixed = TRUE)
  expect_match(src, "codebook", fixed = TRUE)
  expect_false(grepl("For a flat table of the same content, call `as.data.frame()`", src, fixed = TRUE))
})

test_that("G6: the static-export help describes the download that exists", {
  src <- source_of("export_static_survey.R")
  expect_false(grepl("the browser downloads a", src, fixed = TRUE))
  expect_false(grepl("responses are never lost if the POST fails", src, fixed = TRUE))
  expect_match(src, "thank-you screen", fixed = TRUE)

  # what the template actually does
  tpl <- paste(readLines(system.file("static_survey", "template.html",
                                     package = "surveyframe"), warn = FALSE), collapse = "\n")
  expect_match(tpl, "window._lastCsv", fixed = TRUE)
  expect_match(tpl, "Download my response (CSV)", fixed = TRUE)
})

test_that("G10: the Studio help points authoring at launch_builder()", {
  src <- source_of("launch_studio.R")
  expect_false(grepl("to build a survey", src, fixed = TRUE))
  expect_match(src, "launch_builder", fixed = TRUE)
})

test_that("G10: the screens offered are the screens Studio has", {
  app <- paste(readLines(system.file("shiny", "app.R", package = "surveyframe"),
                         warn = FALSE), collapse = "\n")
  tabs <- unique(regmatches(app, gregexpr('`data-tab` = "[a-z]+"', app))[[1]])
  tabs <- sub('.*"([a-z]+)"$', "\\1", tabs)
  expect_setequal(sframe_studio_screens(), tabs)

  expect_identical(sframe_studio_screen("auto"), "auto")
  expect_identical(sframe_studio_screen("data"), "responses")
  expect_identical(sframe_studio_screen("amendments"), "amendments")
  expect_error(sframe_studio_screen("build"), "launch_builder", class = "sframe_error")
})
