# Batch 7's findings on the researcher-facing entry points.
#
# #22. SurveyStudio's preview gate told a researcher to add an item in "Build
#      Survey", a screen Studio does not have. The Open screen already routes
#      question design to the builder, so the gate contradicted it.
# #23. The RStudio addin called launch_dashboard() with no arguments, and the
#      launcher refuses a missing instrument, so an advertised menu entry
#      produced an error message where a dashboard was expected. The
#      tests moved to test-rstudio-addins.R when the add-in was redirected
#      to SurveyStudio.

app_source <- function() {
  sframe_installed_text("inst", "shiny", "app.R")
}

test_that("22: the preview gate names a route Studio has", {
  src <- app_source()
  expect_false(grepl("Build Survey", src, fixed = TRUE))
  # it points at the builder, which is where questions are authored
  expect_match(src, "launch_builder()", fixed = TRUE)
})

test_that("22: every screen the app names is one it has", {
  src <- app_source()
  # the tab set the navigation builds, as launch_studio() also validates
  screens <- sframe_studio_screens()
  expect_true(all(vapply(screens, function(s) {
    grepl(paste0('"', s, '"'), src, fixed = TRUE)
  }, logical(1))))
})

# #23 is now tested in test-rstudio-addins.R. The add-in opens SurveyStudio
# on the responses screen with the chosen instrument, and turns a cancelled
# dialog or an unreadable file into silence or one message, before anything
# launches.
