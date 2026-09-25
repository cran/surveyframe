# score_scales.R

# Column names response data already carries, which an item or scale id must
# never take, since score_scales() writes each score under its scale's id.
sframe_reserved_response_columns <- c("respondent_id", "response_id",
                                      "started_at", "submitted_at")

# The ids of items reversed within `scale`. Reversal belongs to the scale that
# declares it: the scale's own reverse_items, and items whose scale_id names
# this scale with reverse = TRUE. One instrument-wide map let a scale reverse
# an item that another scale owned and never asked to reverse, which moved
# that scale's scores and its alpha.
sframe_scale_reverse_ids <- function(instrument, scale) {
  members <- as.character(scale$items)
  declared <- intersect(as.character(scale$reverse_items %||% character(0)),
                        members)
  item_level <- vapply(instrument$items, function(i) {
    isTRUE(i$reverse) && identical(i$scale_id, scale$id) && i$id %in% members
  }, logical(1))
  item_ids <- vapply(instrument$items, function(i) i$id, character(1))
  unique(c(declared, item_ids[item_level]))
}

# The declared response bounds for an item, or NULL when it declares none:
# the numeric range of its choice set, its slider limits, or 1 to rating_max.
sframe_item_bounds <- function(instrument, item_id) {
  item <- NULL
  for (i in instrument$items) if (identical(i$id, item_id)) item <- i
  if (is.null(item)) return(NULL)
  if (!is.null(item$choice_set)) {
    for (cs in instrument$choices) {
      if (!identical(cs$id, item$choice_set)) next
      vals <- suppressWarnings(as.numeric(cs$values))
      vals <- vals[!is.na(vals)]
      if (length(vals) > 0) return(c(min(vals), max(vals)))
    }
  }
  if (identical(item$type, "slider") && !is.null(item$slider_min) &&
      !is.null(item$slider_max)) {
    return(c(as.numeric(item$slider_min), as.numeric(item$slider_max)))
  }
  if (identical(item$type, "rating")) {
    return(c(1, as.numeric(item$rating_max %||% 5)))
  }
  NULL
}

# A response column as numbers. A factor is read through its labels: as.numeric()
# on a factor returns level positions, so a factor holding 10 and 20 scored as
# 1 and 2. A factor whose labels are not numbers is refused, since its levels
# have no measurement meaning. Character values that are not numbers become NA.
sframe_as_measure <- function(x, column) {
  if (is.factor(x)) {
    labels <- as.character(x)
    out <- suppressWarnings(as.numeric(labels))
    bad <- unique(labels[!is.na(labels) & is.na(out)])
    if (length(bad) > 0) {
      rlang::abort(
        c(paste0("Column '", column, "' is a factor with non-numeric levels, ",
                 "so it cannot be scored."),
          i = paste0("Levels that are not numbers: ",
                     paste(utils::head(bad, 5), collapse = ", "), "."),
          i = "Convert the column to its numeric codes before scoring."),
        class = "sframe_error"
      )
    }
    return(out)
  }
  suppressWarnings(as.numeric(x))
}

# The numeric item matrix for one scale, reversed within that scale on declared
# bounds. Reversing on the sample's own minimum and maximum gave the same answer
# a different reversed value as respondents were added, so an item with no
# declared bounds cannot be reversed.
sframe_scale_matrix <- function(data, instrument, scale, cols) {
  out <- as.data.frame(
    lapply(stats::setNames(cols, cols),
           function(col) sframe_as_measure(data[[col]], col)),
    check.names = FALSE
  )
  for (col in intersect(sframe_scale_reverse_ids(instrument, scale), cols)) {
    rng <- sframe_item_bounds(instrument, col)
    if (is.null(rng)) {
      rlang::abort(
        c(paste0("Item '", col, "' is reverse-coded in scale '", scale$id,
                 "' but declares no response bounds."),
          i = paste0("Reversal needs the scale's fixed endpoints. Give the ",
                     "item a numeric choice set, or slider_min and slider_max.")),
        class = "sframe_error"
      )
    }
    out[[col]] <- (rng[1] + rng[2]) - out[[col]]
  }
  out
}

# Refuses scoring when a scale id names data score_scales() would overwrite: an
# item, an expansion column, or response metadata. validate_sframe() reports the
# same collision, and this guards instruments that were never validated.
#
# `data` matters as much as the declaration. The declared ids alone missed a
# collision with a column the responses actually carry and the instrument never
# mentions, so a scale called "site" replaced a collected "site" column with
# its own scores and the metadata was gone.
#
# The rule is explicit: a scale's score column has to be a new column. Scoring
# an already-scored frame is done by dropping those columns first, which says
# what is being replaced instead of replacing it silently.
sframe_check_score_columns <- function(instrument, data = NULL) {
  scale_ids <- vapply(instrument$scales, function(s) s$id, character(1))
  item_ids <- vapply(instrument$items, function(i) i$id, character(1))
  taken <- c(item_ids, sframe_item_expansion_columns(instrument),
             sframe_reserved_response_columns)
  clash <- intersect(scale_ids, taken)
  if (length(clash) > 0) {
    rlang::abort(
      c(paste0("Scale ID ", paste0("'", clash, "'", collapse = ", "),
               " matches an item, response or metadata column."),
        i = paste0("score_scales() stores each score in a column named by ",
                   "its scale ID, so scoring would overwrite that data. ",
                   "Rename the scale.")),
      class = c("sframe_validation_error", "sframe_error")
    )
  }
  if (!is.null(data)) {
    in_data <- intersect(scale_ids, colnames(data))
    if (length(in_data) > 0) {
      rlang::abort(
        c(paste0("The responses already hold a column named ",
                 paste0("'", in_data, "'", collapse = ", "),
                 ", which is also a scale ID."),
          i = paste0("score_scales() stores each score in a column named by ",
                     "its scale ID, so scoring would replace that data. ",
                     "Rename the scale, or drop the column first where it ",
                     "holds scores from an earlier run.")),
        class = c("sframe_validation_error", "sframe_error")
      )
    }
  }
  invisible(TRUE)
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
  sframe_check_instrument(instrument)
  stopifnot(is.data.frame(data))

  item_ids <- vapply(instrument$items, function(i) i$id, character(1))
  sframe_check_score_columns(instrument, data)

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
    absent <- setdiff(scale$items, colnames(data))
    if (length(absent) > 0) {
      sframe_warn_scoring(
        paste0("Scale '", scale$id, "' has no column for item(s) ",
               paste0("'", absent, "'", collapse = ", "),
               ". They count as unanswered toward min_valid."),
        scale_id = scale$id
      )
    }

    scale_num <- sframe_scale_matrix(data, instrument, scale, scale_item_ids)

    # Minimum valid items. The default is every declared item, whether or not
    # its column is present: counting only the columns present let a 3-item
    # scale with an absent column be scored on 2 items.
    valid_counts <- rowSums(!is.na(scale_num))
    min_valid    <- scale$min_valid %||% length(scale$items)

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
