# read_responses.R

#' Read and validate survey responses
#'
#' Loads survey response data and checks that it conforms to the instrument
#' specification. Column names in the response file must match item IDs defined
#' in the instrument. Non-item columns are allowed only when declared through
#' `respondent_id`, `submitted_at`, or `meta_cols`.
#'
#' # Columns
#'
#' A single-answer item has one column named by its ID. A matrix, ranking,
#' multiple-choice or decision item has one column per row, option, pair or
#' criterion, named `item__sub`, and each is checked: a battery with some of its
#' columns absent is reported by name. Two columns with the same name are
#' refused, since one would otherwise be lost.
#'
#' # Values
#'
#' A CSV file is read as text, so identifiers such as `001`, dates and text
#' answers arrive exactly as written, a literal `NA` included. Columns of items
#' with numeric responses (numeric, slider and rating items, choice items whose
#' codes are all numbers, and ranking, multiple-choice and decision expansion
#' columns) are then converted to numbers, with an empty cell or `NA` read as
#' missing. A column that does not convert cleanly is kept as text. Data frames
#' go through the same conversion, so a CSV file and a data frame holding the
#' same responses read the same.
#'
#' @param x A file path to a CSV file, a `data.frame`, or a `tibble`.
#' @param instrument An `sframe` object created by [sf_instrument()].
#' @param respondent_id Character or NULL. The name of the column containing
#'   unique respondent identifiers. If NULL, no respondent ID column is
#'   expected.
#' @param submitted_at Character or NULL. The name of the column containing
#'   submission timestamps.
#' The metadata columns surveyframe's own collectors write, `respondent_id`,
#' `response_id`, `started_at` and `submitted_at`, are recognised without being
#' declared: a file this package collected reads back without naming the columns
#' it wrote. Anything else outside the instrument still has to be declared, or
#' `strict = FALSE` used.
#'
#' @param meta_cols Character vector or NULL. Additional column names, outside
#'   the item IDs, to retain (for example, condition assignment or
#'   source URL).
#' @param strict Logical. When `TRUE` (default), a column outside the declared
#'   item IDs, their expansion columns and the metadata columns is an error,
#'   naming the columns. When `FALSE`, such columns are kept, placed last, with
#'   a warning.
#'
#' @return A `data.frame` with columns ordered as: metadata columns first, then
#'   item columns in instrument order, each item followed by its expansion
#'   columns, then any undeclared columns kept under `strict = FALSE`. To keep
#'   an extra column under `strict = TRUE`, name it in `meta_cols`, or select
#'   the columns you need before reading.
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
    # Read as text, with only an empty cell as missing. Type inference turned
    # an id of 001 into 1 and a text answer of NA into a missing value.
    # Declared numeric columns are converted below, for both routes.
    data <- utils::read.csv(
      x,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character",
      na.strings = character(0)
    )
  } else if (is.data.frame(x)) {
    data <- sframe_as_data_frame(x)
  } else {
    rlang::abort(
      "`x` must be a CSV file path, a data.frame, or a tibble.",
      class = c("sframe_import_error", "sframe_error")
    )
  }

  dup <- unique(colnames(data)[duplicated(colnames(data))])
  if (length(dup) > 0) {
    sframe_abort_import(paste0(
      "The response data has more than one column named ",
      paste0("'", dup, "'", collapse = ", "),
      ". Rename or remove the duplicate, since one of them would be lost."))
  }

  all_item_ids <- vapply(instrument$items, function(i) i$id, character(1))
  display_only_types <- c("section_break", "text_block")
  response_items <- Filter(
    function(i) !identical(i$type %in% display_only_types, TRUE),
    instrument$items
  )
  item_ids <- vapply(response_items, function(i) i$id, character(1))
  display_item_ids <- setdiff(all_item_ids, item_ids)
  # surveyframe's own collectors write respondent_id, response_id, started_at
  # and submitted_at, so those names count as declared without the researcher
  # naming columns the package itself produced. Before this, reading a
  # collected file strictly meant listing started_at in meta_cols, and adding
  # respondent_id to the Shiny row would have broken every caller who had.
  declared  <- c(respondent_id, submitted_at, meta_cols,
                 sframe_reserved_response_columns)
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
  # An item is present through its expected expansion columns, never through
  # any column that happens to share its prefix. Some expected columns present
  # and some absent is reported separately, by name.
  partial <- character(0)
  covered_by_expansion <- multi_ids[vapply(multi_ids, function(id) {
    item <- response_items[[which(item_ids == id)]]
    expected <- sframe_item_expansion_columns(instrument, list(item))
    if (length(expected) == 0) return(any(startsWith(data_cols, paste0(id, "__"))))
    present <- intersect(expected, data_cols)
    if (length(present) > 0 && length(present) < length(expected)) {
      partial <<- c(partial, setdiff(expected, present))
    }
    length(present) > 0
  }, logical(1))]
  if (length(partial) > 0) {
    sframe_warn_missing(paste0(
      length(partial), " expansion column(s) are absent from the response data ",
      "for items whose other columns are present: ", paste(partial, collapse = ", ")))
  }

  # Detect R's automatic name repair before reporting the corresponding
  # declared expansion columns as missing. In strict mode the import cannot
  # continue, so emitting a missing-column warning immediately before the
  # more useful name-repair error only obscures the actionable diagnosis.
  undeclared <- setdiff(data_cols,
                        c(item_ids, expanded_ids, display_item_ids, declared))
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
  if (strict && length(mangled) > 0) {
    sframe_abort_import(paste0(
      length(undeclared), " undeclared column(s) found in response data: ",
      paste(undeclared, collapse = ", "),
      ". Declare them in meta_cols or set strict = FALSE.", hint
    ))
  }

  # Check required item columns are present.
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

  # Handle other undeclared columns.
  if (length(undeclared) > 0) {
    # A matrix row or choice label containing a space produces an expansion
    # column with a space, which the collectors write correctly. read.csv()
    # then rewrites it: "q1__Row one" arrives as "q1__Row.one", because
    # check.names defaults to TRUE. The columns are then undeclared through no
    # fault of the researcher, and the plain message sends them looking for a
    # declaration problem that does not exist. Name the real cause instead.
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

  out <- sframe_as_data_frame(data[, ordered_cols, drop = FALSE])
  sframe_convert_declared_columns(out, instrument, response_items)
}

# The response columns whose values are numbers, from the instrument, so only
# they are converted and identifiers, dates and text stay as written.
sframe_numeric_response_columns <- function(instrument, response_items) {
  numeric_codes <- function(item) {
    for (cs in instrument$choices %||% list()) {
      if (identical(cs$id, item$choice_set)) {
        return(all(!is.na(suppressWarnings(as.numeric(as.character(cs$values))))))
      }
    }
    FALSE
  }
  cols <- character(0)
  for (item in response_items) {
    type <- item$type
    if (type %in% c("numeric", "slider", "rating") ||
        (type %in% c("likert", "single_choice") && numeric_codes(item))) {
      cols <- c(cols, item$id)
    }
    if (identical(type, "matrix") && numeric_codes(item)) {
      cols <- c(cols, sframe_item_expansion_columns(instrument, list(item)))
    }
    if (type %in% c("ranking", "multiple_choice", sframe_expanded_comparison_types)) {
      cols <- c(cols, sframe_item_expansion_columns(instrument, list(item)))
    }
  }
  unique(cols)
}

sframe_convert_declared_columns <- function(data, instrument, response_items) {
  for (col in intersect(sframe_numeric_response_columns(instrument, response_items),
                        colnames(data))) {
    x <- data[[col]]
    if (is.numeric(x)) {
      data[[col]] <- as.numeric(x)
      next
    }
    if (is.factor(x)) x <- as.character(x)
    if (!is.character(x)) next
    text <- trimws(x)
    text[!is.na(text) & text %in% c("", "NA")] <- NA_character_
    converted <- suppressWarnings(as.numeric(text))
    if (all(is.na(text) | !is.na(converted))) data[[col]] <- converted
  }
  for (col in setdiff(colnames(data),
                      sframe_numeric_response_columns(instrument, response_items))) {
    x <- data[[col]]
    if (is.character(x)) {
      x[!is.na(x) & x == ""] <- NA_character_
      data[[col]] <- x
    }
  }
  data
}
