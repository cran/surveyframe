# dashboard.R

#' Launch the interactive response dashboard
#'
#' Opens a Shiny dashboard to explore collected response data alongside the
#' instrument definition. Use this interface after response collection for
#' analysis and quality control. Use [launch_builder()] to design new
#' questionnaires. The dashboard includes five panels:
#'
#' \describe{
#'   \item{Overview}{Response count, date range, and instrument metadata.}
#'   \item{Items}{Per-item frequency bar charts, histograms, and tabulated
#'     frequency counts for choice-type questions.}
#'   \item{Scales}{Scale score distributions with mean overlay, and a
#'     summary table of scale definitions.}
#'   \item{Quality}{Attention check pass rates for each check defined in the
#'     instrument.}
#'   \item{Raw data}{Scrollable response table with a CSV download button.}
#' }
#'
#' The dashboard is read-only. Use it for descriptive exploration after
#' collecting responses and before running formal analysis with
#' [run_analysis_plan()].
#'
#' @param instrument An `sframe` object. When `NULL`, the bundled tourism
#'   services demo instrument is loaded.
#' @param responses A `data.frame` or `tibble` of survey responses, as
#'   produced by [read_responses()] or [read_sheet_responses()]. When NULL
#'   and `instrument` is also NULL, the bundled simulated demo responses are
#'   loaded. When NULL with a user-supplied instrument, the dashboard opens
#'   with instrument metadata and no response summaries.
#' @param port Integer or NULL. TCP port for the Shiny server. When NULL,
#'   Shiny selects an available port automatically.
#' @param host Character. Host address passed to [shiny::runApp()]. Defaults
#'   to `"127.0.0.1"`.
#' @param launch.browser Logical. Whether to open the dashboard in the
#'   default browser automatically. Defaults to `TRUE` in interactive
#'   sessions.
#'
#' @return Called for its side effect. Returns nothing.
#' @export
#' @seealso [run_analysis_plan()], [quality_report()], [score_scales()]
#'
#' @examples
#' \donttest{
#' # Open the bundled tourism-services response dashboard.
#' # To build a questionnaire instead, use launch_builder().
#' launch_dashboard()
#'
#' # Open the dashboard with your own instrument and responses
#' instr <- read_sframe(
#'   system.file("extdata", "tourism_services_demo.sframe",
#'               package = "surveyframe")
#' )
#' responses <- read_responses(
#'   system.file("extdata", "tourism_services_responses.csv",
#'               package = "surveyframe"),
#'   instr,
#'   respondent_id = "respondent_id",
#'   submitted_at = "submitted_at",
#'   meta_cols = "started_at"
#' )
#' launch_dashboard(instr, responses)
#' }
launch_dashboard <- function(
    instrument      = NULL,
    responses      = NULL,
    port           = NULL,
    host           = "127.0.0.1",
    launch.browser = interactive()
) {
  rlang::check_installed("shiny", reason = "to launch the response dashboard.")

  if (is.null(instrument)) {
    demo_instr_path <- system.file(
      "extdata", "tourism_services_demo.sframe",
      package = "surveyframe"
    )
    demo_resp_path <- system.file(
      "extdata", "tourism_services_responses.csv",
      package = "surveyframe"
    )
    if (!nzchar(demo_instr_path) || !file.exists(demo_instr_path)) {
      rlang::abort(
        "Bundled dashboard demo instrument not found. Please reinstall surveyframe.",
        class = "sframe_error"
      )
    }
    instrument <- read_sframe(demo_instr_path)
    if (is.null(responses) && nzchar(demo_resp_path) && file.exists(demo_resp_path)) {
      responses <- read_responses(
        demo_resp_path,
        instrument,
        respondent_id = "respondent_id",
        submitted_at = "submitted_at",
        meta_cols = "started_at"
      )
    }
  }

  sframe_check_instrument(instrument)

  if (!is.null(responses) && !is.data.frame(responses)) {
    rlang::abort(
      "`responses` must be a data.frame or NULL.",
      class = "sframe_error"
    )
  }

  app_path <- system.file("shiny", "dashboard", package = "surveyframe")
  if (!nzchar(app_path) || !file.exists(file.path(app_path, "app.R"))) {
    rlang::abort(
      "Dashboard app not found. Please reinstall surveyframe.",
      class = "sframe_error"
    )
  }

  # Expose data to the app via an isolated environment, then build a standard
  # shiny.appobj for compatibility with older Shiny releases.
  app_env <- new.env(parent = asNamespace("surveyframe"))
  app_env$SFRAME_INSTRUMENT <- instrument
  app_env$SFRAME_RESPONSES  <- responses
  sys.source(file.path(app_path, "app.R"), envir = app_env)

  shiny::runApp(
    appDir        = shiny::shinyApp(ui = app_env$ui, server = app_env$server),
    port          = port,
    host          = host,
    launch.browser= launch.browser,
    quiet         = TRUE
  )
}
