# tests/testthat/test-builder-template-sync.R
#
# The builder carries its own copies of the static survey template and the
# Google Sheets collector, inlined between marker comments so a survey exported
# from the GUI matches one exported from R. The copies are regenerated from
# inst/static_survey/ by a maintainer script. A fix made to the template alone
# leaves every GUI-exported survey with the defect, which is how A8 survived in
# the builder after being fixed in the template. This holds the copies equal.

inlined_block <- function(builder, start_marker, end_marker) {
  start <- grep(start_marker, builder, fixed = TRUE)
  end <- grep(end_marker, builder, fixed = TRUE)
  expect_length(start, 1)
  expect_length(end, 1)
  block <- builder[(start + 1):(end - 1)]
  # drop the <script type="text/template"> wrapper lines
  block[-c(1, length(block))]
}

asset <- function(...) {
  p <- system.file(..., package = "surveyframe")
  skip_if(!nzchar(p) || !file.exists(p), "shipped asset not found")
  readLines(p, warn = FALSE, encoding = "UTF-8")
}

unescape_script <- function(lines) gsub("<\\/script>", "</script>", lines, fixed = TRUE)

test_that("the builder's static survey template matches template.html", {
  builder <- asset("builder", "survey_builder.html")
  inlined <- inlined_block(builder, "<!-- STATIC_TEMPLATE_START:",
                           "<!-- STATIC_TEMPLATE_END -->")
  expect_identical(unescape_script(inlined),
                   asset("static_survey", "template.html"),
                   info = "Run data-raw/inline_static_template.R to regenerate the builder.")
})

test_that("the builder's collector matches collector_template.gs", {
  builder <- asset("builder", "survey_builder.html")
  inlined <- inlined_block(builder, "<!-- COLLECTOR_TEMPLATE_START:",
                           "<!-- COLLECTOR_TEMPLATE_END -->")
  expect_identical(unescape_script(inlined),
                   asset("static_survey", "collector_template.gs"),
                   info = "Run data-raw/inline_static_template.R to regenerate the builder.")
})
