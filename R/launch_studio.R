# launch_studio.R

# The screens SurveyStudio has, in navigation order, matching the tabs in
# inst/shiny/app.R.
sframe_studio_screens <- function() {
  c("open", "amendments", "preview", "responses", "quality", "reliability",
    "analysis", "dashboard", "export")
}

# Resolves the `screen` argument to a screen Studio has. "build" used to be
# offered and quietly opened another screen, since Studio authors nothing.
sframe_studio_screen <- function(screen) {
  screen <- as.character(screen %||% "auto")[1]
  if (identical(screen, "data")) screen <- "responses"
  if (identical(screen, "build")) {
    rlang::abort(
      paste0("SurveyStudio has no screen for building a survey. Author an ",
             "instrument with launch_builder(), then open it here."),
      class = "sframe_error")
  }
  if (!screen %in% c("auto", sframe_studio_screens())) {
    rlang::abort(
      paste0("Unknown Studio screen '", screen, "'. Use \"auto\", or one of: ",
             paste(sframe_studio_screens(), collapse = ", "), "."),
      class = "sframe_error")
  }
  screen
}

#' Launch the SurveyStudio interface
#'
#' Opens the SurveyStudio Shiny application, the visual interface for working
#' with an instrument that already exists. Its screens open an instrument,
#' record and read amendments, preview the survey, upload responses, review
#' data quality, inspect reliability, work on the analysis plan, read the
#' dashboard, and export.
#'
#' Studio reads and analyses an instrument. To author one, question by
#' question, use [launch_builder()], and open the result here.
#'
#' @param instrument An `sframe` object or NULL.
#' @param responses A data.frame, tibble, CSV file path, or NULL.
#' @param respondent_id Character or NULL. Response ID column when `responses`
#'   is a CSV path.
#' @param submitted_at Character or NULL. Submission time column when
#'   `responses` is a CSV path.
#' @param meta_cols Character vector or NULL. Metadata columns when `responses`
#'   is a CSV path.
#' @param strict Logical. Passed to [read_responses()] when `responses` is a
#'   CSV path.
#' @param screen The screen to open on. One of `"auto"`, which picks by what
#'   you supply, or a screen name: `"open"`, `"amendments"`, `"preview"`,
#'   `"responses"`, `"quality"`, `"reliability"`, `"analysis"`,
#'   `"dashboard"` or `"export"`. `"data"` is accepted for `"responses"`.
#' @param port TCP port for the Shiny server.
#' @param host Host address passed to [shiny::runApp()].
#' @param launch.browser Whether to open the browser automatically.
#'
#' @return Called for its side effect.
#' @export
#' @seealso [launch_builder()], [launch_dashboard()], [read_sframe()],
#'   [read_responses()]
#'
#' @examples
#' \dontrun{
#' launch_studio()
#'
#' demo <- sframe_demo_data()
#' launch_studio(instrument = demo$instrument, launch.browser = FALSE)
#'
#' launch_studio(
#'   instrument    = demo$instrument,
#'   responses     = demo$responses,
#'   respondent_id = "respondent_id",
#'   submitted_at  = "submitted_at"
#' )
#' }
launch_studio <- function(
    instrument = NULL,
    responses = NULL,
    respondent_id = NULL,
    submitted_at = NULL,
    meta_cols = NULL,
    strict = TRUE,
    screen = "auto",
    port = NULL,
    host = "127.0.0.1",
    launch.browser = interactive()
) {
  sframe_require_shiny("to launch SurveyStudio")

  screen <- sframe_studio_screen(screen)

  if (!is.null(instrument)) {
    sframe_check_instrument(instrument)
  }

  if (!is.null(responses) && is.character(responses)) {
    if (is.null(instrument)) {
      rlang::abort(
        "`instrument` must be supplied when `responses` is a file path.",
        class = "sframe_error"
      )
    }

    responses <- read_responses(
      x = responses,
      instrument = instrument,
      respondent_id = respondent_id,
      submitted_at = submitted_at,
      meta_cols = meta_cols,
      strict = strict
    )
  }

  if (!is.null(responses) && !is.data.frame(responses)) {
    rlang::abort(
      "`responses` must be a data.frame, tibble, CSV file path, or NULL.",
      class = "sframe_error"
    )
  }

  app_path <- system.file("shiny", package = "surveyframe")

  if (!nzchar(app_path) || !file.exists(file.path(app_path, "app.R"))) {
    rlang::abort(
      "SurveyStudio app not found. Please reinstall surveyframe.",
      class = "sframe_error"
    )
  }

  shiny::shinyOptions(
    surveyframe_instrument = instrument,
    surveyframe_responses = responses,
    surveyframe_initial_screen = screen
  )

  on.exit(
    shiny::shinyOptions(
      surveyframe_instrument = NULL,
      surveyframe_responses = NULL,
      surveyframe_initial_screen = NULL
    ),
    add = TRUE
  )

  shiny::runApp(
    appDir = app_path,
    port = port,
    host = host,
    launch.browser = launch.browser,
    quiet = TRUE
  )
}
