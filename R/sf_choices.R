# sf_choices.R

#' Define a reusable choice set
#'
#' Creates a named set of response options that can be referenced by one or
#' more items. Defining choices once and referencing them by `id` keeps the
#' instrument consistent and reduces the risk of label mismatches across items
#' that share the same response format.
#'
#' @param id Character. A unique identifier for this choice set. Referenced in
#'   the `choice_set` argument of [sf_item()].
#' @param values Character or numeric vector. The stored values corresponding
#'   to each response option. Must have the same length as `labels`.
#' @param labels Character vector. The display labels shown to respondents.
#'   Must have the same length as `values`.
#' @param allow_other Logical. Whether to append an open-text "Other" option
#'   at the end of the choice list. Defaults to `FALSE`.
#' @param randomise Logical. Whether to randomise the display order of options
#'   at render time. Defaults to `FALSE`.
#'
#' @return An object of class `sf_choices` (a named list).
#' @export
#' @seealso [sf_item()], [sf_instrument()]
#'
#' @examples
#' # A five-point agreement scale
#' agree5 <- sf_choices(
#'   id     = "agree5",
#'   values = 1:5,
#'   labels = c("Strongly disagree", "Disagree", "Neutral",
#'              "Agree", "Strongly agree")
#' )
#'
#' # A yes/no set
#' yn <- sf_choices(
#'   id     = "yn",
#'   values = c("yes", "no"),
#'   labels = c("Yes", "No")
#' )
sf_choices <- function(
    id,
    values,
    labels,
    allow_other = FALSE,
    randomise   = FALSE
) {
  if (length(values) != length(labels)) {
    rlang::abort(
      "`values` and `labels` must have the same length.",
      class = c("sframe_validation_error", "sframe_error")
    )
  }

  structure(
    list(
      id          = id,
      values      = values,
      labels      = labels,
      allow_other = allow_other,
      randomise   = randomise
    ),
    class = "sf_choices"
  )
}
