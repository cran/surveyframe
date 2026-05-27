# score_scales.R

sframe_reverse_context <- function(instrument) {
  item_ids <- vapply(instrument$items, function(i) i$id, character(1))

  reverse_map <- stats::setNames(
    vapply(instrument$items, function(i) isTRUE(i$reverse), logical(1)),
    item_ids
  )

  for (scale in instrument$scales) {
    if (is.null(scale$reverse_items)) {
      next
    }
    for (rid in scale$reverse_items) {
      if (rid %in% names(reverse_map)) {
        reverse_map[rid] <- TRUE
      }
    }
  }

  choice_ranges <- stats::setNames(
    lapply(instrument$choices, function(cs) {
      vals <- suppressWarnings(as.numeric(cs$values))
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) {
        return(NULL)
      }
      c(min(vals), max(vals))
    }),
    vapply(instrument$choices, function(cs) cs$id, character(1))
  )

  item_choice_sets <- stats::setNames(
    vapply(instrument$items, function(i) i$choice_set %||% "", character(1)),
    item_ids
  )

  list(
    reverse_map = reverse_map,
    choice_ranges = choice_ranges,
    item_choice_sets = item_choice_sets
  )
}

sframe_numeric_scale_data <- function(data, item_ids, reverse_context = NULL) {
  scale_num <- as.data.frame(lapply(data[, item_ids, drop = FALSE], function(col) {
    suppressWarnings(as.numeric(col))
  }))

  if (is.null(reverse_context)) {
    return(scale_num)
  }

  for (col in item_ids) {
    if (!isTRUE(reverse_context$reverse_map[[col]])) {
      next
    }

    vals <- scale_num[[col]]
    cs_id <- reverse_context$item_choice_sets[[col]]
    rng <- if (nzchar(cs_id) && !is.null(reverse_context$choice_ranges[[cs_id]])) {
      reverse_context$choice_ranges[[cs_id]]
    } else {
      observed <- vals[!is.na(vals)]
      if (length(observed) == 0) {
        next
      }
      c(min(observed), max(observed))
    }

    scale_num[[col]] <- (rng[1] + rng[2]) - vals
  }

  scale_num
}

sframe_scale_weights <- function(scale, scale_item_ids) {
  if (is.null(scale$weights)) {
    return(rep(1, length(scale_item_ids)))
  }

  scale$weights[match(scale_item_ids, scale$items)]
}

sframe_composite_score <- function(scale_num, scale, scale_item_ids) {
  weights <- sframe_scale_weights(scale, scale_item_ids)

  if (!is.null(scale$weights) && any(is.na(weights))) {
    sframe_warn_scoring(
      paste0("Scale '", scale$id, "' has weights that must align with its items."),
      scale_id = scale$id
    )
    weights[is.na(weights)] <- 1
  }

  if (scale$method == "sum") {
    return(rowSums(sweep(scale_num, 2, weights, `*`), na.rm = TRUE))
  }

  weighted_values <- sweep(scale_num, 2, weights, `*`)
  denom <- rowSums(sweep(!is.na(scale_num), 2, weights, `*`), na.rm = TRUE)
  scores <- rowSums(weighted_values, na.rm = TRUE) / denom
  scores[denom == 0] <- NA_real_
  scores
}

#' Score defined scales from survey responses
#'
#' Applies scale scoring rules from the instrument to response data. Handles
#' reverse coding, optional weighted composite score computation, and minimum
#' valid item thresholds. Returns a data frame with one scored column per
#' scale.
#'
#' @param data A `tibble` or `data.frame` of responses.
#' @param instrument An `sframe` object.
#' @param keep_items Logical. Whether to retain individual item columns in the
#'   output. Defaults to `TRUE`.
#' @param keep_meta Logical. Whether to retain non-item columns (metadata) in
#'   the output. Defaults to `TRUE`.
#'
#' @return A `data.frame` with scored scale columns appended. Scale columns are
#'   named using the scale `id`.
#' @export
#' @seealso [sf_scale()], [reliability_report()]
#'
#' @examples
#' cs    <- sf_choices("ag5", 1:5,
#'            c("Strongly disagree", "Disagree", "Neutral",
#'              "Agree", "Strongly agree"))
#' i1    <- sf_item("sat_1", "Item 1", type = "likert",
#'                  choice_set = "ag5", scale_id = "sat")
#' i2    <- sf_item("sat_2", "Item 2", type = "likert",
#'                  choice_set = "ag5", scale_id = "sat")
#' i3    <- sf_item("sat_3", "Item 3 (reverse)", type = "likert",
#'                  choice_set = "ag5", scale_id = "sat", reverse = TRUE)
#' scale <- sf_scale("sat", "Satisfaction",
#'                   items = c("sat_1", "sat_2", "sat_3"), min_valid = 2L)
#' instr <- sf_instrument("Demo", components = list(cs, i1, i2, i3, scale))
#'
#' responses <- data.frame(
#'   sat_1 = c(4, 5, 3),
#'   sat_2 = c(4, 4, 3),
#'   sat_3 = c(2, 1, 3),
#'   stringsAsFactors = FALSE
#' )
#'
#' scored <- score_scales(responses, instr)
#' scored$sat
score_scales <- function(data, instrument, keep_items = TRUE, keep_meta = TRUE) {
  stopifnot(inherits(instrument, "sframe"))
  stopifnot(is.data.frame(data))

  item_ids <- vapply(instrument$items, function(i) i$id, character(1))
  reverse_context <- sframe_reverse_context(instrument)

  scored <- data

  for (scale in instrument$scales) {
    scale_item_ids <- intersect(scale$items, colnames(data))
    if (length(scale_item_ids) == 0) {
      sframe_warn_scoring(
        paste0("Scale '", scale$id, "' has no matching columns in data."),
        scale_id = scale$id
      )
      next
    }

    scale_num <- sframe_numeric_scale_data(data, scale_item_ids, reverse_context)

    # Minimum valid items
    valid_counts <- rowSums(!is.na(scale_num))
    min_valid    <- scale$min_valid %||% length(scale_item_ids)

    composite <- sframe_composite_score(scale_num, scale, scale_item_ids)
    composite[valid_counts < min_valid] <- NA

    scored[[scale$id]] <- composite
  }

  # Optionally drop items and metadata
  all_meta  <- setdiff(colnames(data), item_ids)
  keep_cols <- character(0)
  if (keep_meta)  keep_cols <- c(keep_cols, all_meta)
  if (keep_items) keep_cols <- c(keep_cols, intersect(item_ids, colnames(scored)))
  scale_cols <- vapply(instrument$scales, function(s) s$id, character(1))
  keep_cols  <- c(keep_cols, intersect(scale_cols, colnames(scored)))

  sframe_as_data_frame(scored[, unique(keep_cols), drop = FALSE])
}
