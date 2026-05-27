# builder.R

#' Launch the surveyframe visual survey builder
#'
#' Opens the SurveyBuilder, a self-contained HTML application for visual survey
#' design. The builder runs client-side without an R session or Shiny server.
#' Save instruments as `.sframe` files from the browser and load them into R
#' with [read_sframe()].
#'
#' The builder includes a three-mode interface.
#'
#' \describe{
#'   \item{Build}{An item editor with a persistent inspector panel,
#'     drag-to-reorder, undo/redo, and autosave to browser localStorage.}
#'   \item{Preview}{A full live render of the survey showing welcome, body,
#'     and thank-you pages.}
#'   \item{Analyse}{A role-based analysis planner with method-specific
#'     options, planned outputs, reporting references, and decision rules.}
#' }
#'
#' The builder includes a pure-JavaScript SHA-256 fallback for browsers or
#' security policies where `crypto.subtle` is unavailable on `file://`
#' origins. Saved `.sframe` files can be loaded and validated with
#' [read_sframe()].
#'
#' @param open Logical. When `TRUE` (the default), the builder HTML file is
#'   opened in the system's default web browser with [utils::browseURL()].
#'   Set to `FALSE` to return the file path without opening it, which is
#'   useful for automated testing.
#'
#' @return The path to the bundled builder HTML file, invisibly.
#' @export
#' @seealso [launch_studio()], [read_sframe()], [run_analysis_plan()]
#'
#' @examples
#' # Retrieve the builder path for inspection without opening the browser
#' path <- launch_builder(open = FALSE)
#' file.exists(path)
launch_builder <- function(open = TRUE) {
  builder_path <- system.file("builder", "survey_builder.html",
                              package = "surveyframe")

  if (!nzchar(builder_path) || !file.exists(builder_path)) {
    rlang::abort(
      paste0(
        "SurveyBuilder HTML not found. ",
        "Please reinstall surveyframe or check inst/builder/."
      ),
      class = "sframe_error"
    )
  }

  if (isTRUE(open)) {
    # Use a file:// URI so the browser can open it without a server.
    # crypto.subtle is available on file:// in Chrome and Edge; Firefox
    # requires --allow-file-access-from-files or serving via localhost.
    # The builder gracefully degrades when crypto.subtle is unavailable.
    utils::browseURL(paste0("file://", normalizePath(builder_path)))
  }

  invisible(builder_path)
}
