# export_static_survey.R

#' Export a self-contained static HTML survey
#'
#' Generates a single HTML file that presents the survey instrument in a
#' browser without requiring a Shiny server or any internet connection. All
#' thirteen item types, branching logic, required-field validation, and
#' multi-page navigation are handled entirely in client-side JavaScript.
#'
#' When `output_path` is `NULL`, the file is written to [tempdir()]. Supply
#' an explicit `output_path` for any production export that should be kept.
#'
#' When a respondent clicks the submit button, the browser downloads a
#' one-row CSV file named `<survey_title>_response_<id>.csv`. If
#' `endpoint_url` is supplied, the same payload is also sent as a JSON
#' POST request to that URL (for example a Google Apps Script web app or a
#' serverless function). The two mechanisms are independent: the download
#' happens regardless, so responses are never lost if the POST fails.
#'
#' The exported file works offline. It can be hosted on GitHub Pages,
#' Netlify, any static file server, or e-mailed as an attachment for
#' opening directly from disk.
#'
#' @param instrument An `sframe` object.
#' @param output_path Character. File path for the output HTML. When `NULL`,
#'   a `<survey_title>.html` file is written in [tempdir()].
#' @param open Logical. If `TRUE` (default) and the session is interactive,
#'   the file is opened in the default browser after writing.
#' @param endpoint_url Character or NULL. A URL to which responses are
#'   POSTed as JSON on submission. When NULL, CSV download is the only
#'   collection mechanism.
#' @param overwrite Logical. Whether to overwrite an existing file at
#'   `output_path`. Defaults to `FALSE`.
#'
#' @return The output path, invisibly.
#' @export
#' @seealso [launch_studio()], [launch_builder()], [render_survey()]
#'
#' @examples
#' cs    <- sf_choices("ag5", 1:5,
#'            c("Strongly disagree", "Disagree", "Neutral",
#'              "Agree", "Strongly agree"))
#' i1    <- sf_item("sat_1", "Overall I am satisfied with the service.",
#'                  type = "likert", choice_set = "ag5", required = TRUE)
#' i2    <- sf_item("comments", "Any additional comments?", type = "textarea")
#' instr <- sf_instrument("Customer Satisfaction Survey",
#'                        components = list(cs, i1, i2))
#'
#' # Write to a temp file without opening the browser
#' out <- export_static_survey(instr,
#'                              output_path = file.path(tempdir(), "sat.html"),
#'                              open = FALSE)
#' file.exists(out)
#'
#' \donttest{
#' # Write to a temp file and open in the default browser
#' export_static_survey(instr,
#'                      output_path = file.path(tempdir(), "sat_browser.html"),
#'                      overwrite = TRUE)
#'
#' # Write with a Google Apps Script endpoint for server-side collection
#' export_static_survey(
#'   instr,
#'   output_path  = file.path(tempdir(), "sat_endpoint.html"),
#'   endpoint_url = "https://script.google.com/macros/s/XXXXX/exec",
#'   open         = FALSE,
#'   overwrite    = TRUE
#' )
#' }
export_static_survey <- function(
    instrument,
    output_path  = NULL,
    open         = interactive(),
    endpoint_url = NULL,
    overwrite    = FALSE
) {
  sframe_check_instrument(instrument)
  rlang::check_installed("jsonlite", reason = "to serialise the instrument as JSON.")

  # Fix B: fall back to endpoint stored by the builder if no argument supplied
  endpoint_url <- endpoint_url %||%
    instrument$render$google_sheets_endpoint %||% ""

  # Resolve output path
  if (is.null(output_path)) {
    slug <- gsub("[^a-zA-Z0-9]", "_", instrument$meta$title %||% "survey")
    slug <- gsub("_+", "_", slug)
    output_path <- file.path(tempdir(), paste0(slug, ".html"))
  }

  if (file.exists(output_path) && !overwrite) {
    rlang::abort(
      paste0("Output file already exists: '", output_path, "'. ",
             "Set overwrite = TRUE to replace it."),
      class = "sframe_error"
    )
  }

  # Load template
  tpl_path <- system.file("static_survey", "template.html",
                           package = "surveyframe")
  if (!nzchar(tpl_path) || !file.exists(tpl_path)) {
    rlang::abort(
      "Static survey template not found. Please reinstall surveyframe.",
      class = "sframe_error"
    )
  }
  tpl <- paste(readLines(tpl_path, encoding = "UTF-8"), collapse = "\n")

  html_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x
  }
  replace_placeholder <- function(x, placeholder, value) {
    paste(strsplit(x, placeholder, fixed = TRUE)[[1]], collapse = value)
  }
  strip_classes <- function(x) {
    if (is.list(x)) {
      x <- lapply(x, strip_classes)
      class(x) <- NULL
    }
    x
  }

  # Serialise instrument, stripping the hash slot to keep the JSON clean.
  instr_list <- strip_classes(instrument)
  instr_list$hash <- NULL
  instr_json <- jsonlite::toJSON(instr_list, auto_unbox = TRUE, null = "null",
                                  digits = NA, pretty = FALSE)
  instr_json <- gsub("<", "\\u003c", instr_json, fixed = TRUE)

  # Resolve theme colour
  theme <- instrument$render$theme %||% "#2563eb"
  if (!grepl("^#[0-9A-Fa-f]{3,6}$", theme)) theme <- "#2563eb"

  # Substitute placeholders
  html <- tpl
  html <- replace_placeholder(html, "{{INSTRUMENT_JSON}}", instr_json)
  html <- replace_placeholder(
    html,
    "{{SURVEY_TITLE}}",
    html_escape(instrument$meta$title %||% "Survey")
  )
  html <- replace_placeholder(html, "{{THEME_COLOR}}", theme)
  html <- replace_placeholder(html, "{{ENDPOINT_URL}}", html_escape(endpoint_url))

  # Write output
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  writeLines(html, output_path, useBytes = FALSE)

  size_kb <- round(file.size(output_path) / 1024, 1)
  rlang::inform(
    paste0("Static survey written to '", output_path,
           "' (", size_kb, " KB).")
  )

  if (isTRUE(open)) {
    utils::browseURL(paste0("file://", normalizePath(output_path)))
  }

  invisible(output_path)
}
