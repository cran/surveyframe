# sf_check.R

#' Define a design-time survey check
#'
#' Specifies an attention, instructional, or trap check item at instrument
#' design time. The check is stored in the instrument object and evaluated
#' against collected response data by [quality_report()]. This function only
#' defines the check. Evaluation happens later in [quality_report()].
#'
#' @param id Character. A unique identifier for this check.
#' @param item_id Character. The `id` of the item used as the check. The item
#'   must be defined separately with [sf_item()] and included in the same
#'   instrument.
#' @param type Character. The check type. One of:
#'   - `"attention"`: the item has a stated correct answer and flags
#'     respondents who answer incorrectly.
#'   - `"instructional"`: a manipulation check item used to test whether
#'     instructions were followed.
#'   - `"trap"`: an item designed to be selected only by inattentive
#'     respondents (e.g. "Please select Strongly agree for this item.").
#' @param pass_values Vector or NULL. The response value or values that
#'   constitute a pass. For `"attention"` and `"instructional"` types, at
#'   least one value should be supplied. For `"trap"` types, this is the
#'   value that should NOT be selected.
#' @param fail_action Character. What [quality_report()] does with respondents
#'   who fail this check. Either `"flag"` (mark in the report but retain) or
#'   `"exclude"` (mark for exclusion).
#' @param label Character or NULL. An optional human-readable label for the
#'   check, used in the quality report.
#' @param notes Character or NULL. Optional free-text notes about the purpose
#'   or rationale of this check.
#'
#' @return An object of class `sf_check` (a named list).
#' @export
#' @seealso [sf_item()], [sf_instrument()], [quality_report()]
#'
#' @examples
#' # An attention check: respondent must select 4
#' chk <- sf_check(
#'   id          = "attn_1",
#'   item_id     = "attention_check_q",
#'   type        = "attention",
#'   pass_values = 4,
#'   fail_action = "flag",
#'   label       = "Attention check 1"
#' )
sf_check <- function(
    id,
    item_id,
    type        = c("attention", "instructional", "trap"),
    pass_values = NULL,
    fail_action = c("flag", "exclude"),
    label       = NULL,
    notes       = NULL
) {
  type        <- rlang::arg_match(type)
  fail_action <- rlang::arg_match(fail_action)

  # An attention or instructional check tests membership of pass_values, so
  # leaving them empty failed every respondent. A trap declares the value that
  # marks a failure, and so needs one just as much.
  usable <- as.character(pass_values %||% character(0))
  usable <- usable[!is.na(usable) & nzchar(trimws(usable))]
  if (length(usable) == 0) {
    rlang::abort(
      paste0("Check '", as.character(id)[1], "' of type '", type,
             "' needs at least one value in `pass_values`. Every response is ",
             "measured against them."),
      class = c("sframe_validation_error", "sframe_error")
    )
  }

  structure(
    list(
      id          = id,
      item_id     = item_id,
      type        = type,
      pass_values = pass_values,
      fail_action = fail_action,
      label       = label,
      notes       = notes
    ),
    class = "sf_check"
  )
}
