# read_responses.R

#' Read and validate survey responses
#'
#' Loads survey response data and checks that it conforms to the instrument
#' specification. Column names in the response file must match item IDs defined
#' in the instrument. Non-item columns are allowed only when declared through
#' `respondent_id`, `submitted_at`, or `meta_cols`.
#'
#' @param x A file path to a CSV file, a `data.frame`, or a `tibble`.
#' @param instrument An `sframe` object created by [sf_instrument()].
#' @param respondent_id Character or NULL. The name of the column containing
#'   unique respondent identifiers. If NULL, no respondent ID column is
#'   expected.
#' @param submitted_at Character or NULL. The name of the column containing
#'   submission timestamps.
#' @param meta_cols Character vector or NULL. Additional column names that are
#'   not item IDs but should be retained (for example, condition assignment or
#'   source URL).
#' @param strict Logical. When `TRUE` (default), columns in the response data
#'   outside the declared item IDs and metadata columns raise an error.
#'   When `FALSE`, undeclared columns are retained with a warning.
#'
#' @return A `data.frame` with columns ordered as: metadata columns first, then
#'   item columns in instrument order. Unrecognised columns are dropped when
#'   `strict = TRUE` or appended with a warning when `strict = FALSE`.
#' @export
#' @seealso [quality_report()], [score_scales()]
#'
#' @examples
#' responses <- read_responses(
#'   x = system.file("extdata", "tourism_services_responses.csv",
#'                   package = "surveyframe"),
#'   instrument = read_sframe(
#'     system.file("extdata", "tourism_services_demo.sframe",
#'                 package = "surveyframe")
#'   ),
#'   respondent_id = "respondent_id",
#'   submitted_at = "submitted_at",
#'   meta_cols = "started_at"
#' )
#' head(responses[, c("respondent_id", "visit_type", "dm_1")])
read_responses <- function(
    x,
    instrument,
    respondent_id = NULL,
    submitted_at  = NULL,
    meta_cols     = NULL,
    strict        = TRUE
) {
  sframe_check_instrument(instrument)

  # Load data
  if (is.character(x)) {
    if (!file.exists(x)) {
      sframe_abort_import(
        paste0("Response file not found: '", x, "'. Check the file path and ensure the file exists."),
        path = x
      )
    }
    data <- utils::read.csv(
      x,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else if (is.data.frame(x)) {
    data <- sframe_as_data_frame(x)
  } else {
    rlang::abort(
      "`x` must be a CSV file path, a data.frame, or a tibble.",
      class = c("sframe_import_error", "sframe_error")
    )
  }

  all_item_ids <- vapply(instrument$items, function(i) i$id, character(1))
  display_only_types <- c("section_break", "text_block")
  response_items <- Filter(
    function(i) !identical(i$type %in% display_only_types, TRUE),
    instrument$items
  )
  item_ids <- vapply(response_items, function(i) i$id, character(1))
  display_item_ids <- setdiff(all_item_ids, item_ids)
  declared  <- c(respondent_id, submitted_at, meta_cols)
  data_cols <- colnames(data)

  # Matrix and ranking items arrive from the collectors as one column per
  # sub-item or option (item__sub, item__option). Accept those expansions
  # alongside the base id: an expanded multi-column item is not "missing"
  # when its base column is absent, and its expansion columns are never
  # "undeclared".
  choice_values_for <- function(id) {
    for (cs in instrument$choices) {
      if (identical(cs$id, id)) return(as.character(cs$values))
    }
    character(0)
  }
  expanded_ids <- unlist(lapply(response_items, function(i) {
    if (identical(i$type, "matrix") && length(i$matrix_items) > 0L) {
      paste0(i$id, "__", i$matrix_items)
    } else if (identical(i$type, "ranking") && !is.null(i$choice_set)) {
      vals <- choice_values_for(i$choice_set)
      if (length(vals) > 0L) paste0(i$id, "__", vals) else character(0)
    } else if (identical(i$type, "multiple_choice") && !is.null(i$choice_set)) {
      vals <- choice_values_for(i$choice_set)
      if (length(vals) > 0L) paste0(i$id, "__", vals) else character(0)
    } else {
      character(0)
    }
  }), use.names = FALSE)
  multi_ids <- vapply(
    Filter(function(i) identical(i$type, "matrix") ||
             identical(i$type, "ranking") ||
             identical(i$type, "multiple_choice"), response_items),
    function(i) i$id, character(1)
  )
  covered_by_expansion <- multi_ids[vapply(multi_ids, function(id) {
    any(startsWith(data_cols, paste0(id, "__")))
  }, logical(1))]

  # Check required item columns are present
  missing_items <- setdiff(item_ids, c(data_cols, covered_by_expansion))
  if (length(missing_items) > 0) {
    sframe_warn_missing(
      paste0(
        length(missing_items),
        " item column(s) are absent from the response data: ",
        paste(missing_items, collapse = ", ")
      )
    )
  }

  # Handle undeclared columns
  undeclared <- setdiff(data_cols,
                        c(item_ids, expanded_ids, display_item_ids, declared))
  if (length(undeclared) > 0) {
    if (strict) {
      sframe_abort_import(
        paste0(
          length(undeclared),
          " undeclared column(s) found in response data: ",
          paste(undeclared, collapse = ", "),
          ". Declare them in meta_cols or set strict = FALSE."
        )
      )
    } else {
      sframe_warn_quality(
        paste0(
          length(undeclared),
          " undeclared column(s) retained with a warning: ",
          paste(undeclared, collapse = ", ")
        )
      )
    }
  }

  # Reorder: metadata first, then items in instrument order (expanded
  # columns follow their base item), then undeclared
  ordered_item_cols <- unlist(lapply(response_items, function(i) {
    c(i$id, expanded_ids[startsWith(expanded_ids, paste0(i$id, "__"))])
  }), use.names = FALSE)
  ordered_cols <- intersect(
    c(declared, ordered_item_cols, display_item_ids, undeclared),
    data_cols
  )

  sframe_as_data_frame(data[, ordered_cols, drop = FALSE])
}
