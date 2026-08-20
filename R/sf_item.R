# sf_item.R

#' Define a survey item
#'
#' Creates a single survey item object for inclusion in an `sframe` instrument.
#' Items are the atomic units of a survey instrument. Every item must have a
#' unique `id` within the instrument it is added to.
#'
#' @param id Character. A unique identifier for this item. Used as the column
#'   name in response data. Must contain only letters, numbers, and `_` characters.
#' @param label Character. The question text or content displayed to the
#'   respondent.
#' @param type Character. The response type. One of `"likert"`,
#'   `"single_choice"`, `"multiple_choice"`, `"numeric"`, `"text"`,
#'   `"textarea"`, `"date"`, `"matrix"`, `"slider"`, `"ranking"`, `"rating"`,
#'   `"pairwise_comparison"`, `"criteria_weight"`, `"section_break"`, or
#'   `"text_block"`.
#' @param required Logical. Whether the respondent must answer this item.
#' @param choice_set Character or NULL. The `id` of a choice set defined with
#'   [sf_choices()].
#' @param scale_id Character or NULL. The `id` of the scale this item belongs to.
#' @param reverse Logical. Whether this item is reverse-coded within its scale.
#' @param help Character or NULL. Help text displayed beneath the question.
#' @param placeholder Character or NULL. Placeholder text for text inputs.
#' @param matrix_items Character vector or NULL. Row labels for `"matrix"` type.
#' @param comparison_items Character vector or NULL. The things being compared
#'   or weighted, for `"pairwise_comparison"` and `"criteria_weight"` types.
#'   At least 2 entries, all distinct. A `"saaty"` pairwise item renders
#'   `n(n-1)/2` unordered pair rows, an `"influence"` item renders `n(n-1)`
#'   ordered rows, and a `"criteria_weight"` item renders one numeric input
#'   per entry. An advisory warning is raised above 7 items (`"saaty"`) or 6
#'   (`"influence"`), and above 10 the item is rejected.
#' @param comparison_scale Character or NULL. For `"pairwise_comparison"`
#'   only. `"saaty"` (the default) gives the bipolar 1-9 importance scale used
#'   by AHP and ANP, while `"influence"` gives the unipolar 0-4 directed
#'   influence scale used by DEMATEL.
#' @param slider_min Numeric or NULL. Minimum value for `"slider"` type.
#' @param slider_max Numeric or NULL. Maximum value for `"slider"` type.
#' @param slider_step Numeric or NULL. Step size for `"slider"` type.
#' @param rating_max Integer or NULL. Maximum rating for `"rating"` type.
#' @param rating_icon Character or NULL. Icon type: `"star"` or `"heart"`.
#' @param date_min Character or Date or NULL. Earliest selectable date for
#'   `"date"` type, as `"YYYY-MM-DD"`.
#' @param date_max Character or Date or NULL. Latest selectable date for
#'   `"date"` type, as `"YYYY-MM-DD"`.
#' @param section_intro Character or NULL. Intro text for `"section_break"` type.
#' @param page Integer or NULL. Page number for multi-page surveys.
#'
#' @return An object of class `sf_item` (a named list).
#' @export
#' @seealso [sf_instrument()], [sf_choices()], [sf_scale()]
#'
#' @examples
#' item <- sf_item(
#'   id = "sat_overall", label = "Overall, how satisfied are you?",
#'   type = "likert", required = TRUE, choice_set = "agree5",
#'   scale_id = "satisfaction"
#' )
#'
#' sec <- sf_item("sec_1", "Demographic Information", type = "section_break",
#'                section_intro = "Please answer the following questions.")
sf_item <- function(
    id,
    label,
    type          = c("likert", "single_choice", "multiple_choice",
                      "numeric", "text", "textarea", "date",
                      "matrix", "slider", "ranking", "rating",
                      "pairwise_comparison", "criteria_weight",
                      "section_break", "text_block"),
    required      = FALSE,
    choice_set    = NULL,
    scale_id      = NULL,
    reverse       = FALSE,
    help          = NULL,
    placeholder   = NULL,
    matrix_items  = NULL,
    comparison_items  = NULL,
    comparison_scale  = NULL,
    slider_min    = NULL,
    slider_max    = NULL,
    slider_step   = NULL,
    rating_max    = NULL,
    rating_icon   = NULL,
    date_min      = NULL,
    date_max      = NULL,
    section_intro = NULL,
    page          = NULL
) {
  type <- rlang::arg_match(type)
  date_min <- sframe_check_date_bound(date_min, "date_min")
  date_max <- sframe_check_date_bound(date_max, "date_max")
  if (!is.null(date_min) && !is.null(date_max) &&
      as.Date(date_min, format = "%Y-%m-%d") >
      as.Date(date_max, format = "%Y-%m-%d")) {
    rlang::abort("`date_min` must not be later than `date_max`.",
                 class = c("sframe_validation_error", "sframe_error"))
  }
  comparison_scale <- sframe_check_comparison_scale(comparison_scale, type)
  comparison_items <- sframe_check_comparison_items(
    comparison_items, comparison_scale, type, id
  )

  structure(
    list(
      id = id,
      label = label,
      type = type,
      required = required,
      choice_set = choice_set,
      scale_id = scale_id,
      reverse = reverse,
      help = help,
      placeholder = placeholder,
      matrix_items = matrix_items,
      comparison_items = comparison_items,
      comparison_scale = comparison_scale,
      slider_min = slider_min,
      slider_max = slider_max,
      slider_step = slider_step,
      rating_max = rating_max,
      rating_icon = rating_icon,
      date_min = date_min,
      date_max = date_max,
      section_intro = section_intro,
      page = page
    ),
    class = "sf_item"
  )
}

# Normalise a date bound to a "YYYY-MM-DD" string, or abort with a typed
# validation error when the value cannot be read as a date.
sframe_check_date_bound <- function(value, arg) {
  if (is.null(value)) return(NULL)
  # A bare as.Date(value) guesses the format and silently misparses an
  # ambiguous or wrong-order string (e.g. "01/02/2024" becomes the year-1
  # date "1-02-20" rather than an error), so the bound is only ever accepted
  # if it matches "YYYY-MM-DD" exactly.
  parsed <- tryCatch(as.Date(as.character(value), format = "%Y-%m-%d"),
                     error = function(e) NA)
  if (length(parsed) != 1 || is.na(parsed)) {
    rlang::abort(
      sprintf("`%s` must be a single date in YYYY-MM-DD form.", arg),
      class = c("sframe_validation_error", "sframe_error")
    )
  }
  as.character(parsed)
}

# The comparison scale only means something for a pairwise item. It is
# defaulted rather than declared through arg_match() in the signature so that
# every other item type keeps a NULL field instead of inheriting "saaty".
sframe_check_comparison_scale <- function(value, type) {
  if (!identical(type, "pairwise_comparison")) {
    if (!is.null(value)) {
      rlang::abort(
        "`comparison_scale` applies only to `type = \"pairwise_comparison\"`.",
        class = c("sframe_validation_error", "sframe_error")
      )
    }
    return(NULL)
  }
  if (is.null(value)) return("saaty")
  value <- as.character(value)
  if (length(value) != 1 || !value %in% c("saaty", "influence")) {
    rlang::abort(
      "`comparison_scale` must be either \"saaty\" or \"influence\".",
      class = c("sframe_validation_error", "sframe_error")
    )
  }
  value
}

# Size limits come from respondent burden, not from the maths: a saaty item
# renders n(n-1)/2 rows and an influence item n(n-1), so 10 items is 45 or 90
# comparison rows. The Saaty random index also stops at n = 10, past which no
# consistency ratio can be computed at all.
sframe_check_comparison_items <- function(items, scale, type, id = "") {
  comparison_types <- c("pairwise_comparison", "criteria_weight")
  if (!type %in% comparison_types) {
    if (!is.null(items)) {
      rlang::abort(
        paste0("`comparison_items` applies only to items of type ",
               "\"pairwise_comparison\" or \"criteria_weight\"."),
        class = c("sframe_validation_error", "sframe_error")
      )
    }
    return(NULL)
  }
  items <- as.character(items %||% character(0))
  if (length(items) < 2) {
    rlang::abort(
      sprintf("Item '%s' of type \"%s\" needs at least 2 `comparison_items`.",
              id, type),
      class = c("sframe_validation_error", "sframe_error")
    )
  }
  if (anyDuplicated(items) > 0) {
    rlang::abort(
      sprintf("Item '%s' has duplicated `comparison_items`: %s.", id,
              paste(unique(items[duplicated(items)]), collapse = ", ")),
      class = c("sframe_validation_error", "sframe_error")
    )
  }
  if (any(!nzchar(trimws(items)))) {
    rlang::abort(
      sprintf("Item '%s' has an empty `comparison_items` entry.", id),
      class = c("sframe_validation_error", "sframe_error")
    )
  }
  if (length(items) > 10) {
    rlang::abort(
      sprintf(paste0("Item '%s' declares %d `comparison_items`. The maximum ",
                     "is 10."), id, length(items)),
      class = c("sframe_validation_error", "sframe_error")
    )
  }
  limit <- if (identical(scale, "influence")) 6L else 7L
  if (identical(type, "pairwise_comparison") && length(items) > limit) {
    rows <- if (identical(scale, "influence")) {
      length(items) * (length(items) - 1)
    } else {
      length(items) * (length(items) - 1) / 2
    }
    sframe_warn_design(
      sprintf(paste0("Item '%s' declares %d `comparison_items`, which renders ",
                     "%d comparison rows. Above %d items respondent fatigue ",
                     "and inconsistent judgements rise sharply."),
              id, length(items), rows, limit),
      item_id = id
    )
  }
  items
}
