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
#'   scale, each listed once. Order controls presentation in reports, while
#'   scoring uses the same item IDs regardless of order. Items themselves are
#'   passed to [sf_instrument()] as separate components.
#' @param method Character. Scoring method. Either `"mean"` (default) or
#'   `"sum"`.
#' @param min_valid Integer or NULL. The minimum number of answered items
#'   required to compute a score for a respondent, a whole number from 1 to
#'   the number of items. When `NULL`, every item must be answered. An item
#'   whose column is absent from the data counts as unanswered. Used by
#'   [score_scales()].
#' @param reverse_items Character vector or NULL. A subset of `items` that
#'   this scale reverse-codes. Reversal applies within this scale only, so the
#'   same item can be reversed in one scale and scored as answered in another.
#'   An item can also be flagged with `reverse = TRUE` in [sf_item()], which
#'   reverses it within the scale named by its `scale_id`. A reversed item
#'   needs declared response bounds, from a numeric choice set, slider limits
#'   or a rating maximum.
#' @param weights Numeric vector or NULL. Item weights for weighted scoring,
#'   one positive finite number per item, in the order of `items`.
#'   [score_scales()] applies the weights to either `method = "mean"` or
#'   `method = "sum"`.
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

  scale <- structure(
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

  problems <- c(sframe_scale_parameter_problems(scale),
                sframe_scale_reverse_problems(scale))
  if (length(problems) > 0) {
    rlang::abort(problems, class = c("sframe_validation_error", "sframe_error"))
  }
  scale
}

# Problems with a scale's scoring parameters, shared by sf_scale() and
# validate_sframe() so a scale that skipped the constructor, for example one
# read from a file, is held to the same rules.
sframe_scale_parameter_problems <- function(scale) {
  id <- as.character(scale$id %||% "?")[1]
  items <- scale$items
  if (!is.character(items) || length(items) == 0) {
    return(paste0("Scale '", id, "' needs `items` as a character vector of ",
                  "item IDs. Pass each item to sf_instrument() as its own ",
                  "component and list its ID here."))
  }
  out <- character(0)
  if (anyDuplicated(items) > 0) {
    out <- c(out, paste0("Scale '", id, "' lists item(s) more than once: ",
                         paste(unique(items[duplicated(items)]), collapse = ", "),
                         "."))
  }
  mv <- scale$min_valid
  if (!is.null(mv)) {
    ok <- is.numeric(mv) && length(mv) == 1 && !is.na(mv) &&
      mv == round(mv) && mv >= 1 && mv <= length(items)
    if (!ok) {
      out <- c(out, paste0("Scale '", id, "' has min_valid ",
                           paste(format(mv), collapse = ", "),
                           ". It must be a whole number from 1 to ",
                           length(items), ", the number of items."))
    }
  }
  w <- scale$weights
  if (!is.null(w)) {
    ok <- is.numeric(w) && length(w) == length(items) && all(is.finite(w)) &&
      all(w > 0)
    if (!ok) {
      out <- c(out, paste0("Scale '", id, "' has unusable weights. weights ",
                           "must be ", length(items), " positive finite ",
                           "numbers, one per item."))
    }
  }
  out
}

# reverse_items that are not members of the scale declaring them.
sframe_scale_reverse_problems <- function(scale) {
  stray <- setdiff(as.character(scale$reverse_items %||% character(0)),
                   as.character(scale$items))
  if (length(stray) == 0) return(character(0))
  paste0("Scale '", as.character(scale$id %||% "?")[1], "' lists ",
         paste0("'", stray, "'", collapse = ", "),
         " in reverse_items, which are not among its items. Reversal applies ",
         "within the scale that declares it.")
}
