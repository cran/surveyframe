# sf_branch.R

# A `%in%` value arrives either as a vector, which is what sf_branch()
# documents and what serialises to a JSON array, or as one comma-separated
# string from a hand-written file. All 3 evaluators share this so they agree.
sframe_branch_in_values <- function(value) {
  chr <- as.character(value)
  chr <- chr[!is.na(chr)]
  if (length(chr) == 0L) {
    return(character(0))
  }
  if (length(chr) == 1L) {
    chr <- strsplit(chr, ",", fixed = TRUE)[[1]]
  }
  trimws(chr)
}

#' Define a branching rule
#'
#' Creates a single-condition branching rule that shows or hides a survey item
#' depending on the value of a preceding item. Only single-condition
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
