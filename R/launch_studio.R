# launch_studio.R

#' Launch the SurveyStudio interface
#'
#' Opens the SurveyStudio Shiny application, a visual interface for the
#' complete surveyframe workflow. The studio includes screens to build a survey
#' draft, open an existing instrument, preview the survey, upload responses,
#' review data quality, inspect reliability, plan analyses, and export outputs.
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
#' @param screen Initial studio screen. One of `"auto"`, `"build"`,
#'   `"preview"`, `"data"`, `"quality"`, `"analysis"`, or `"dashboard"`.
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
    screen = c("auto", "build", "preview", "data", "quality", "analysis", "dashboard"),
    port = NULL,
    host = "127.0.0.1",
    launch.browser = interactive()
) {
  sframe_require_shiny("to launch SurveyStudio")

  screen <- match.arg(screen)

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
