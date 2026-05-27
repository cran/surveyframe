# sf_branch.R

#' Define a branching rule
#'
#' Creates a single-condition branching rule that shows or hides a survey item
#' depending on the value of a preceding item. In v0.1, only single-condition
#' rules are supported. Multi-condition AND/OR logic is planned for a later
#' release.
#'
#' @param item_id Character. The `id` of the item whose visibility this rule
#'   controls.
#' @param depends_on Character. The `id` of the item whose response value
#'   triggers this rule.
#' @param operator Character. The comparison operator. One of `"=="`, `"!="`,
#'   `"%in%"`, `">"`, `">="`, `"<"`, or `"<="`.
#' @param value The value to compare against the response to `depends_on`.
#'   For `"%in%"`, supply a character or numeric vector.
#' @param action Character. What to do when the condition is met. Either
#'   `"show"` (default) or `"hide"`.
#'
#' @return An object of class `sf_branch` (a named list).
#' @export
#' @seealso [sf_instrument()], [validate_sframe()]
#'
#' @examples
#' # Show an open-text follow-up only when the respondent selects "Other"
#' rule <- sf_branch(
#'   item_id    = "gender_other",
#'   depends_on = "gender",
#'   operator   = "==",
#'   value      = "other",
#'   action     = "show"
#' )
sf_branch <- function(
    item_id,
    depends_on,
    operator = c("==", "!=", "%in%", ">", ">=", "<", "<="),
    value,
    action   = c("show", "hide")
) {
  operator <- rlang::arg_match(operator)
  action   <- rlang::arg_match(action)

  structure(
    list(
      item_id    = item_id,
      depends_on = depends_on,
      operator   = operator,
      value      = value,
      action     = action
    ),
    class = "sf_branch"
  )
}
