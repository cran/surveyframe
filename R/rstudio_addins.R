# R/rstudio_addins.R
# RStudio Addins menu bindings. 3 launchers and one text insert, and nothing
# else.
#
# Two rules hold this file apart from the rest of the package. No other file
# in R/ may call an rstudioapi:: function, so surveyframe behaves identically
# outside RStudio. And every binding here fails soft with one message() and an
# invisible NULL, through sframe_run_addin(), because these are interactive
# conveniences rather than part of the API contract: an add-in that throws
# inside the IDE is worse than one that explains itself and stops.
#
# rstudioapi stays in Suggests. Nothing here is a hard dependency.

# Runs one add-in's action. Any error becomes a single message() naming the
# add-in and an invisible NULL, so a failure inside the IDE explains itself.
# Only the bindings use this. The exported functions they call keep raising
# their errors for programmatic use.
sframe_run_addin <- function(action) {
  tryCatch(
    action(),
    error = function(e) {
      message("surveyframe add-in: ", conditionMessage(e))
      invisible(NULL)
    }
  )
}

# TRUE where rstudioapi is installed and RStudio is running, with a message
# saying which is missing otherwise.
sframe_addin_ready <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    message(
      "surveyframe add-in: rstudioapi is required. ",
      "Install it with: install.packages(\"rstudioapi\")"
    )
    return(FALSE)
  }
  if (!isTRUE(rstudioapi::isAvailable())) {
    message("surveyframe add-in: the add-ins run inside RStudio. ",
            "Outside it, call launch_builder() or launch_studio() directly.")
    return(FALSE)
  }
  TRUE
}

#' @keywords internal
#' @noRd
addin_launch_builder <- function() {
  if (!sframe_addin_ready()) return(invisible(NULL))
  sframe_run_addin(function() launch_builder())
}

#' @keywords internal
#' @noRd
addin_launch_studio <- function() {
  if (!sframe_addin_ready()) return(invisible(NULL))
  sframe_run_addin(function() launch_studio())
}

# Asks the researcher for a .sframe file, and returns NULL where the dialog is
# cancelled. An error from the dialog itself propagates to sframe_run_addin(),
# which reports it. Its own function so a test can drive each answer.
sframe_addin_choose_sframe <- function(
    caption = "Choose an instrument (.sframe)") {
  path <- rstudioapi::selectFile(caption = caption,
                                 filter = "sframe files (*.sframe)",
                                 existing = TRUE)
  if (is.null(path) || !length(path) || is.na(path[1]) || !nzchar(path[1])) {
    return(NULL)
  }
  path[1]
}

# The menu entry "Analyse an existing instrument". It opens SurveyStudio on
# the responses screen with the chosen instrument loaded, where the
# researcher uploads a response file. It used to open the dashboard with no
# responses, which showed an empty results page, and the dashboard has no
# upload screen of its own. The binding keeps its old name so existing
# keyboard shortcuts still work.
#' @keywords internal
#' @noRd
addin_launch_dashboard <- function() {
  if (!sframe_addin_ready()) return(invisible(NULL))
  sframe_run_addin(function() {
    path <- sframe_addin_choose_sframe()
    if (is.null(path)) return(invisible(NULL))
    # Read first, so a file that will not load reports itself here with its
    # own error, before a browser opens.
    instrument <- read_sframe(path)
    launch_studio(instrument = instrument, screen = "responses")
  })
}

# The id of the open source document, or NULL with a message where there is
# none to insert into (the console has focus, or no file is open).
sframe_addin_editor <- function() {
  context <- tryCatch(rstudioapi::getSourceEditorContext(),
                      error = function(e) NULL)
  if (is.null(context) || is.null(context$id) || !nzchar(context$id)) {
    message("surveyframe add-in: open an R script in the editor, place the ",
            "cursor where the instrument should go, and run the add-in again.")
    return(NULL)
  }
  context$id
}

# The skeleton is checked against the shipped constructors by
# tests/testthat/test-rstudio-addins.R, which parses it, evaluates it, and
# validates the resulting instrument. A skeleton that does not build a valid
# instrument is worse than no skeleton, and the implementation guide's
# original version was written against an API that no longer exists: it
# passed id = to sf_instrument(), named the component list items =, and
# handed sf_item() an inline choices = argument. Choice sets are declared as
# their own component and referenced by id.
#
# 3 items per scale, not 2 (fixed 2026-08-15). Alpha on exactly 2 items
# reduces to a single pairwise correlation rather than measuring internal
# consistency, and a 2-indicator factor is not identifiable if this
# instrument is later carried into a measurement model. The skeleton is the
# first thing a new user copies, so it should not model the statistical
# floor as if it were normal practice.
sframe_addin_skeleton <- function() {
  paste(
    'instrument <- sf_instrument(',
    '  title       = "My study",',
    '  version     = "1.0.0",',
    '  description = "One line on what this instrument measures.",',
    '  components  = list(',
    '    sf_choices(',
    '      "agree5",',
    '      values = 1:5,',
    '      labels = c("Strongly disagree", "Disagree", "Neutral",',
    '                 "Agree", "Strongly agree")',
    '    ),',
    '    sf_item("q1", "First item text.", type = "likert",',
    '            choice_set = "agree5", scale_id = "construct_1"),',
    '    sf_item("q2", "Second item text.", type = "likert",',
    '            choice_set = "agree5", scale_id = "construct_1"),',
    '    sf_item("q3", "Third item text.", type = "likert",',
    '            choice_set = "agree5", scale_id = "construct_1"),',
    '    sf_scale("construct_1", "Construct one", items = c("q1", "q2", "q3"))',
    '  ),',
    '  analysis_plan = list(',
    '    list(',
    '      id                = "RQ1",',
    '      research_question = "How reliable is construct one?",',
    '      family            = "measurement",',
    '      method            = "reliability_alpha",',
    '      roles             = list(items = c("q1", "q2", "q3"))',
    '    )',
    '  )',
    ')',
    '',
    '# Check the instrument. The result lists any problems found.',
    'validation <- validate_sframe(instrument)',
    'validation',
    '',
    '# Save it for SurveyBuilder, SurveyStudio, or deployment:',
    '# write_sframe(instrument, "my-study.sframe")',
    sep = "\n"
  )
}

#' @keywords internal
#' @noRd
addin_insert_skeleton <- function() {
  if (!sframe_addin_ready()) return(invisible(NULL))
  sframe_run_addin(function() {
    id <- sframe_addin_editor()
    if (is.null(id)) return(invisible(NULL))
    rstudioapi::insertText(text = sframe_addin_skeleton(), id = id)
    invisible(NULL)
  })
}
