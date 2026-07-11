# google_sheets.R

#' Export a survey instrument to Google Sheets collection format
#'
#' Generates a Google Apps Script file that, when run in a Google Sheet,
#' creates a response collection endpoint for a survey instrument. The builder
#' can store the deployed Apps Script URL in survey metadata, and the same
#' sheet can be read back with [read_sheet_responses()].
#'
#' @param instrument An `sframe` object.
#' @param sheet_url Character. The URL of an existing Google Sheet. The sheet
#'   must be shared so that anyone with the link can edit, or use service
#'   account credentials via `googlesheets4`.
#' @param output_dir Character. Directory to write the Apps Script file.
#'   Defaults to the current working directory.
#'
#' @return The path to the generated `.gs` Apps Script file, invisibly.
#' @export
#' @seealso [read_sheet_responses()], [read_responses()], [write_sframe()]
#'
#' @examples
#' instr <- read_sframe(
#'   system.file("extdata", "tourism_services_demo.sframe",
#'               package = "surveyframe")
#' )
#' script <- export_google_sheet(
#'   instr,
#'   sheet_url = "https://docs.google.com/spreadsheets/d/demo",
#'   output_dir = tempdir()
#' )
#' file.exists(script)
export_google_sheet <- function(instrument, sheet_url, output_dir = ".") {
  sframe_check_instrument(instrument)
  if (!dir.exists(output_dir)) {
    rlang::abort(
      paste0("Output directory does not exist: '", output_dir, "'."),
      class = "sframe_error"
    )
  }

  # Fix C2: matrix items post one column per sub-item (item_id__sub); expand headers
  # 0.3.3: display-only items collect no data, so they get no sheet column
  response_items <- Filter(
    function(i) !identical(i$type %in% c("section_break", "text_block"), TRUE),
    instrument$items
  )
  choice_values <- function(instrument, id) {
    for (cs in instrument$choices) {
      if (identical(cs$id, id)) return(as.character(cs$values))
    }
    character(0)
  }
  item_headers <- unlist(lapply(response_items, function(i) {
    if (identical(i$type, "matrix") && length(i$matrix_items) > 0L) {
      paste0(i$id, "__", i$matrix_items)
    } else if (identical(i$type, "ranking") && !is.null(i$choice_set)) {
      # 0.3.3: ranking posts one rank column per option
      vals <- choice_values(instrument, i$choice_set)
      if (length(vals) > 0L) paste0(i$id, "__", vals) else i$id
    } else if (identical(i$type, "multiple_choice") && !is.null(i$choice_set)) {
      # 0.3.3: multi-select posts one 0/1 column per option
      vals <- choice_values(instrument, i$choice_set)
      if (length(vals) > 0L) paste0(i$id, "__", vals) else i$id
    } else {
      i$id
    }
  }), use.names = FALSE)
  col_headers <- c("respondent_id", "started_at", "submitted_at", item_headers)
  headers_js <- jsonlite::toJSON(col_headers, auto_unbox = TRUE)
  sheet_url_js <- jsonlite::toJSON(sheet_url %||% "", auto_unbox = TRUE)
  sheet_url_comment <- gsub("[\r\n]+", " ", sheet_url %||% "", perl = TRUE)

  # The collector script body is the single source shared with the SurveyBuilder
  # (inst/static_survey/collector_template.gs). The builder inlines the same file
  # so the .gs it exports stays identical to this one.
  tpl_path <- system.file("static_survey", "collector_template.gs",
                          package = "surveyframe")
  if (!nzchar(tpl_path) || !file.exists(tpl_path)) {
    rlang::abort(
      "Collector template not found. Please reinstall surveyframe.",
      class = "sframe_error"
    )
  }
  tpl <- paste(readLines(tpl_path, encoding = "UTF-8", warn = FALSE),
               collapse = "\n")
  sub_ph <- function(x, ph, val) {
    paste(strsplit(x, ph, fixed = TRUE)[[1]], collapse = val)
  }
  script <- tpl
  script <- sub_ph(script, "{{SHEET_URL_COMMENT}}", sheet_url_comment)
  script <- sub_ph(script, "{{TARGET_SHEET_URL}}", sheet_url_js)
  script <- sub_ph(script, "{{EXPECTED_COLUMNS}}", headers_js)

  out_path <- file.path(output_dir, "surveyframe_collector.gs")
  writeLines(script, out_path)

  message(
    "Apps Script written to: ", out_path, "\n",
    "Follow the setup instructions inside the file to deploy it."
  )
  invisible(out_path)
}

#' Read survey responses from a Google Sheet
#'
#' Reads response data collected by the surveyframe Google Apps Script
#' endpoint and returns a validated data frame ready for the surveyframe
#' analysis pipeline.
#'
#' @param sheet_id Character. The Google Sheet ID or full URL.
#' @param instrument An `sframe` object.
#' @param sheet_name Character. The name of the sheet tab holding responses.
#'   Defaults to `"Responses"`.
#' @param respondent_id Character or NULL. Column holding respondent IDs.
#'   Defaults to `"respondent_id"`.
#' @param submitted_at Character or NULL. Column holding submission
#'   timestamps. Defaults to `"submitted_at"`.
#' @param meta_cols Character vector or NULL. Additional sheet columns to
#'   accept as metadata without a warning, for example bridge fields a host
#'   application appends to each submission. `"started_at"` is always
#'   included.
#'
#' @return A `data.frame` validated against the instrument, ready for
#'   [quality_report()], [score_scales()], and [reliability_report()].
#' @export
#' @seealso [export_google_sheet()], [read_responses()], [quality_report()]
#'
#' @examples
#' \dontrun{
#' responses <- read_sheet_responses(
#'   sheet_id   = "your-sheet-id",
#'   instrument = instr
#' )
#' qr <- quality_report(responses, instr, respondent_id = "respondent_id")
#' }
read_sheet_responses <- function(
    sheet_id,
    instrument,
    sheet_name    = "Responses",
    respondent_id = "respondent_id",
    submitted_at  = "submitted_at",
    meta_cols     = NULL
) {
  rlang::check_installed("googlesheets4",
    reason = "to read responses from Google Sheets")
  sframe_check_instrument(instrument)

  raw <- googlesheets4::read_sheet(sheet_id, sheet = sheet_name,
                                    col_types = "c")

  # Fix D: declare started_at as a meta column so it is not flagged as unknown
  read_responses(
    x             = raw,
    instrument    = instrument,
    respondent_id = respondent_id,
    submitted_at  = submitted_at,
    meta_cols     = unique(c("started_at", meta_cols)),
    strict        = FALSE
  )
}
