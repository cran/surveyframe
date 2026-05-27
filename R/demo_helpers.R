# demo_helpers.R

#' Load bundled surveyframe demo data
#'
#' Loads the bundled tourism-services `.sframe` instrument and simulated
#' response dataset used in package examples and statistical workflow demos.
#'
#' @return A list with `instrument`, `responses`, `instrument_path`, and
#'   `responses_path`.
#' @export
sframe_demo_data <- function() {
  instrument_path <- system.file(
    "extdata", "tourism_services_demo.sframe",
    package = "surveyframe"
  )

  responses_path <- system.file(
    "extdata", "tourism_services_responses.csv",
    package = "surveyframe"
  )

  if (!nzchar(instrument_path) || !file.exists(instrument_path)) {
    rlang::abort(
      "Bundled demo instrument not found. Please reinstall surveyframe.",
      class = "sframe_error"
    )
  }

  if (!nzchar(responses_path) || !file.exists(responses_path)) {
    rlang::abort(
      "Bundled demo response file not found. Please reinstall surveyframe.",
      class = "sframe_error"
    )
  }

  instrument <- read_sframe(instrument_path)

  responses <- read_responses(
    responses_path,
    instrument,
    respondent_id = "respondent_id",
    submitted_at = "submitted_at",
    meta_cols = "started_at"
  )

  list(
    instrument = instrument,
    responses = responses,
    instrument_path = instrument_path,
    responses_path = responses_path
  )
}

#' Load bundled input-types demo data
#'
#' Loads the bundled `.sframe` instrument and simulated response dataset that
#' cover all main survey input types supported by surveyframe.
#'
#' @return A list with `instrument`, `responses`, `instrument_path`, and
#'   `responses_path`.
#' @export
sframe_input_types_demo_data <- function() {
  instrument_path <- system.file(
    "extdata", "surveyframe_input_types_demo.sframe",
    package = "surveyframe"
  )

  responses_path <- system.file(
    "extdata", "surveyframe_input_types_responses.csv",
    package = "surveyframe"
  )

  if (!nzchar(instrument_path) || !file.exists(instrument_path)) {
    rlang::abort(
      "Bundled input-types demo instrument not found. Please reinstall surveyframe.",
      class = "sframe_error"
    )
  }

  if (!nzchar(responses_path) || !file.exists(responses_path)) {
    rlang::abort(
      "Bundled input-types demo response file not found. Please reinstall surveyframe.",
      class = "sframe_error"
    )
  }

  instrument <- read_sframe(instrument_path)

  responses <- read_responses(
    x = responses_path,
    instrument = instrument,
    respondent_id = "respondent_id",
    submitted_at = "submitted_at",
    meta_cols = "started_at",
    strict = TRUE
  )

  list(
    instrument = instrument,
    responses = responses,
    instrument_path = instrument_path,
    responses_path = responses_path
  )
}

#' Launch SurveyBuilder with the bundled input-types demo preloaded
#'
#' Opens a temporary copy of the SurveyBuilder with the bundled input-types
#' instrument already injected into the JavaScript state. The demo questions,
#' scales, and analysis plan are visible immediately — no manual file-load
#' step is required.
#'
#' @param open Logical. When `TRUE` (the default), the pre-populated builder
#'   HTML is opened in the system's default web browser.
#'
#' @return Invisibly returns a list with `builder_path`, `demo_file`, and
#'   `responses_path`.
#' @export
launch_builder_demo <- function(open = TRUE) {
  demo <- sframe_input_types_demo_data()

  # Build a plain-list serialisation payload (strips R S3 classes)
  payload <- sframe_serialization_payload(demo$instrument)

  # The builder state needs exactly these keys (no hash / version wrapper)
  state <- list(
    meta          = payload$meta,
    choices       = payload$choices,
    items         = payload$items,
    scales        = payload$scales,
    branching     = payload$branching,
    checks        = payload$checks,
    analysis_plan = payload$analysis_plan,
    models        = payload$models,
    render        = payload$render %||% list(
      mode            = "standard",
      theme           = "#2563eb",
      submit_label    = "Submit",
      welcome         = list(
        title            = "Welcome to this survey",
        intro_text       = "Thank you for taking part.",
        consent_text     = "I understand and agree to participate.",
        consent_required = FALSE,
        start_label      = "Start Survey"
      ),
      thankyou        = list(
        message      = "Thank you for completing this survey.",
        redirect_url = "",
        show_download = FALSE
      ),
      header          = list(
        institution  = "",
        logo_base64  = "",
        show_progress = TRUE
      ),
      google_sheets_url      = "",
      google_sheets_endpoint = ""
    )
  )

  # Compact JSON suitable for inline JavaScript assignment
  state_json <- jsonlite::toJSON(
    state, auto_unbox = TRUE, null = "null", pretty = FALSE
  )

  # Read the bundled builder HTML as a single string for injection
  src_path <- system.file("builder", "survey_builder.html",
                          package = "surveyframe")
  if (!nzchar(src_path) || !file.exists(src_path)) {
    rlang::abort(
      "SurveyBuilder HTML not found. Please reinstall surveyframe.",
      class = "sframe_error"
    )
  }

  builder_html <- paste(
    readLines(src_path, encoding = "UTF-8", warn = FALSE),
    collapse = "\n"
  )

  # 1. Replace the var S={...}; initialisation with the demo state.
  builder_html <- sub(
    "var S=\\{meta:\\{[^;]+\\};",
    paste0("var S=", state_json, ";"),
    builder_html
  )

  # 2. Before chkAutoSave() clears/restores any prior localStorage session,
  #    remove the autosave key so the recovery banner does not appear.
  init_marker <- "chkAutoSave();"
  ls_clear <- "try{localStorage.removeItem('sf_as');}catch(e){}\n"
  builder_html <- sub(
    init_marker,
    paste0(ls_clear, init_marker),
    builder_html,
    fixed = TRUE
  )

  if (!grepl(state_json, builder_html, fixed = TRUE)) {
    rlang::abort(
      paste0(
        "Could not inject the demo state into survey_builder.html. ",
        "Please reinstall surveyframe."
      ),
      class = "sframe_error"
    )
  }

  # Write to a per-session temp file
  demo_dir <- file.path(tempdir(), "surveyframe-builder-demo")
  dir.create(demo_dir, recursive = TRUE, showWarnings = FALSE)
  demo_builder_path <- file.path(demo_dir, "survey_builder_demo.html")
  writeLines(builder_html, demo_builder_path, useBytes = FALSE)

  if (isTRUE(open)) {
    utils::browseURL(paste0("file://", normalizePath(demo_builder_path)))
  }

  invisible(list(
    builder_path   = demo_builder_path,
    demo_file      = demo$instrument_path,
    responses_path = demo$responses_path
  ))
}

#' Launch SurveyStudio with the bundled input-types demo
#'
#' Opens SurveyStudio with the bundled input-types questionnaire and simulated
#' response data already loaded. The browser is opened automatically by
#' default.
#'
#' @param screen Initial studio screen. Defaults to `"preview"` so the demo
#'   content is immediately visible.
#' @param port TCP port for the Shiny server.
#' @param host Host address for the Shiny server.
#' @param launch.browser Whether to open the browser automatically. Defaults
#'   to `TRUE` for this demo helper.
#'
#' @return Called for its side effect.
#' @export
launch_studio_demo <- function(
    screen = "preview",
    port = NULL,
    host = "127.0.0.1",
    launch.browser = TRUE
) {
  demo <- sframe_input_types_demo_data()

  launch_studio(
    instrument     = demo$instrument,
    responses      = demo$responses,
    screen         = screen,
    port           = port,
    host           = host,
    launch.browser = launch.browser
  )
}

#' Launch the response dashboard with the bundled input-types demo
#'
#' Opens the dashboard with the bundled input-types questionnaire and 120
#' simulated responses already loaded. The browser is opened automatically by
#' default.
#'
#' @param port TCP port for the Shiny server.
#' @param host Host address for the Shiny server.
#' @param launch.browser Whether to open the browser automatically. Defaults
#'   to `TRUE` for this demo helper.
#'
#' @return Called for its side effect.
#' @export
launch_dashboard_demo <- function(
    port = NULL,
    host = "127.0.0.1",
    launch.browser = TRUE
) {
  demo <- sframe_input_types_demo_data()

  launch_dashboard(
    instrument     = demo$instrument,
    responses      = demo$responses,
    port           = port,
    host           = host,
    launch.browser = launch.browser
  )
}
