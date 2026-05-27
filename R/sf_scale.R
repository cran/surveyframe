# sf_scale.R

#' Define a scored scale
#'
#' Creates a scale definition that groups items and specifies how composite
#' scores are computed. The scale carries scoring rules used by [score_scales()]
#' and measurement structure used by [reliability_report()], [item_report()],
#' and [cfa_syntax()].
#'
#' @param id Character. A unique identifier for this scale. Referenced in the
#'   `scale_id` argument of [sf_item()].
#' @param label Character. A human-readable name for the scale, used in
#'   reports and codebooks.
#' @param items Character vector. The `id` values of items that belong to this
#'   scale. Order controls presentation in reports; scoring uses the same item
#'   IDs regardless of order.
#' @param method Character. Scoring method. Either `"mean"` (default) or
#'   `"sum"`.
#' @param min_valid Integer or NULL. The minimum number of non-missing items
#'   required to compute a score for a respondent. When `NULL`, all items must
#'   be present. Used by [score_scales()].
#' @param reverse_items Character vector or NULL. A subset of `items` that are
#'   reverse-coded. These can also be flagged at the item level with the
#'   `reverse` argument in [sf_item()]. Both sources are respected.
#' @param weights Numeric vector or NULL. Item weights for weighted scoring.
#'   Must have the same length as `items` if supplied. [score_scales()]
#'   applies the weights to either `method = "mean"` or `method = "sum"`.
#'
#' @return An object of class `sf_scale` (a named list).
#' @export
#' @seealso [sf_item()], [score_scales()], [reliability_report()]
#'
#' @examples
#' sat_scale <- sf_scale(
#'   id            = "satisfaction",
#'   label         = "Customer Satisfaction",
#'   items         = c("sat_overall", "sat_speed", "sat_quality"),
#'   method        = "mean",
#'   min_valid     = 2,
#'   reverse_items = NULL
#' )
sf_scale <- function(
    id,
    label,
    items,
    method        = c("mean", "sum"),
    min_valid     = NULL,
    reverse_items = NULL,
    weights       = NULL
) {
  method <- rlang::arg_match(method)

  if (!is.null(weights) && length(weights) != length(items)) {
    rlang::abort(
      "`weights` must have the same length as `items`.",
      class = c("sframe_validation_error", "sframe_error")
    )
  }

  structure(
    list(
      id            = id,
      label         = label,
      items         = items,
      method        = method,
      min_valid     = min_valid,
      reverse_items = reverse_items,
      weights       = weights
    ),
    class = "sf_scale"
  )
}
