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
#' @param meta_cols Character vector or NULL. Additional column names, outside
#'   the item IDs, to retain (for example, condition assignment or
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
  # Shared with validate_sframe() so the accepted expansion columns cannot
  # drift between what the reader accepts and what design-time validation
  # recognises. See sframe_item_expansion_columns() in R/decision_data.R.
  expanded_ids <- sframe_item_expansion_columns(instrument, response_items)
  multi_ids <- vapply(
    Filter(function(i) identical(i$type, "matrix") ||
             identical(i$type, "ranking") ||
             identical(i$type, "multiple_choice") ||
             i$type %in% sframe_expanded_comparison_types, response_items),
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
    # A matrix row or choice label containing a space produces an expansion
    # column with a space, which the collectors write correctly. read.csv()
    # then rewrites it: "q1__Row one" arrives as "q1__Row.one", because
    # check.names defaults to TRUE. The columns are then undeclared through no
    # fault of the researcher, and the plain message sends them looking for a
    # declaration problem that does not exist. Name the real cause instead.
    known <- c(item_ids, expanded_ids, display_item_ids, declared)
    mangled <- undeclared[make.names(undeclared) == undeclared &
                            undeclared %in% make.names(known)]
    hint <- if (length(mangled) > 0) {
      originals <- known[make.names(known) %in% mangled]
      paste0(
        " ", length(mangled), " of these match a declared column after R's",
        " name repair (for example '", originals[1], "' became '",
        mangled[1], "'), so the header was most likely rewritten on import.",
        " Re-read the file with check.names = FALSE, as in",
        " read.csv(path, check.names = FALSE)."
      )
    } else {
      ""
    }

    if (strict) {
      sframe_abort_import(
        paste0(
          length(undeclared),
          " undeclared column(s) found in response data: ",
          paste(undeclared, collapse = ", "),
          ". Declare them in meta_cols or set strict = FALSE.",
          hint
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
