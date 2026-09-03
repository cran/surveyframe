# R/decision_data.R
# The data layer between collected responses and the MCDM computation layer.
# Getting judgement data from a phone screen into a square reciprocal matrix
# is where MCDM implementations fail, so assembly, aggregation and weight
# derivation live here, are unit tested on their own, and are the single
# source every decision runner resolves its inputs through.

# Item types whose responses arrive as one column per pair or per criterion.
sframe_expanded_comparison_types <- c("pairwise_comparison", "criteria_weight")

# ---------------------------------------------------------------------------
# Column encoding (the export contract, todo_text_analysis.md section 1c)
# ---------------------------------------------------------------------------

# The unordered (saaty) or ordered (influence) pairs an item renders, in the
# declaration order of comparison_items. Returns a data frame of a/b columns
# so callers never have to re-derive the ordering.
sframe_comparison_pairs <- function(items, scale = "saaty") {
  items <- as.character(items)
  n <- length(items)
  if (n < 2) {
    return(data.frame(a = character(0), b = character(0),
                      stringsAsFactors = FALSE))
  }
  a <- character(0)
  b <- character(0)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next
      if (identical(scale, "influence") || i < j) {
        a <- c(a, items[i])
        b <- c(b, items[j])
      }
    }
  }
  data.frame(a = a, b = b, stringsAsFactors = FALSE)
}

# The expansion column names for a decision item, following the item__sub
# convention already accepted by read_responses().
sframe_comparison_columns <- function(item) {
  items <- as.character(item$comparison_items %||% character(0))
  if (length(items) == 0) return(character(0))
  if (identical(item$type, "criteria_weight")) {
    return(paste0(item$id, "__", items))
  }
  if (!identical(item$type, "pairwise_comparison")) return(character(0))
  scale <- item$comparison_scale %||% "saaty"
  pairs <- sframe_comparison_pairs(items, scale)
  sep <- if (identical(scale, "influence")) "__to__" else "__vs__"
  paste0(item$id, "__", pairs$a, sep, pairs$b)
}

# Every expansion column an instrument's items produce in collected data,
# following the item__sub convention: matrix sub-items, ranking and
# multiple-choice options, and decision pairs and criteria.
#
# read_responses() and validate_sframe() both need this. They used to derive
# it separately, and validate_sframe()'s copy was never extended past the base
# item ids, so a real builder export failed design-time validation with
# "references unknown variable(s)" for columns the reader accepted happily.
# One definition, used by both, so they cannot drift apart again.
sframe_item_expansion_columns <- function(instrument, items = NULL) {
  items <- items %||% (instrument$items %||% list())
  choice_values_for <- function(id) {
    for (cs in instrument$choices %||% list()) {
      if (identical(cs$id, id)) return(as.character(cs$values))
    }
    character(0)
  }
  out <- lapply(items, function(i) {
    if (identical(i$type, "matrix") && length(i$matrix_items) > 0L) {
      paste0(i$id, "__", i$matrix_items)
    } else if (identical(i$type, "ranking") && !is.null(i$choice_set)) {
      vals <- choice_values_for(i$choice_set)
      if (length(vals) > 0L) paste0(i$id, "__", vals) else character(0)
    } else if (identical(i$type, "multiple_choice") && !is.null(i$choice_set)) {
      vals <- choice_values_for(i$choice_set)
      if (length(vals) > 0L) paste0(i$id, "__", vals) else character(0)
    } else if (i$type %in% sframe_expanded_comparison_types) {
      sframe_comparison_columns(i)
    } else {
      character(0)
    }
  })
  as.character(unlist(out, use.names = FALSE))
}

sframe_decision_item <- function(instrument, item_id, types) {
  items <- instrument$items %||% list()
  for (item in items) {
    if (identical(item$id, item_id)) {
      if (!item$type %in% types) {
        sframe_abort_validation(sprintf(
          "Item '%s' is of type '%s'. This step needs an item of type %s.",
          item_id, item$type,
          paste0("'", types, "'", collapse = " or ")
        ))
      }
      return(item)
    }
  }
  sframe_abort_validation(sprintf(
    "Item '%s' is not declared in the instrument.", item_id
  ))
}

# ---------------------------------------------------------------------------
# Assembly: response columns to one square matrix per respondent
# ---------------------------------------------------------------------------

#' Assemble per-respondent comparison matrices
#'
#' Builds one square judgement matrix per respondent from the pair columns of
#' a `"pairwise_comparison"` item. A `"saaty"` item stores one signed integer
#' per unordered pair, so the reciprocal half of each matrix is reconstructed
#' here rather than stored (a collected value of +5 becomes `m[a, b] = 5` and
#' `m[b, a] = 1/5`, and the diagonal is 1). An `"influence"` item stores one
#' unsigned integer per ordered pair, fills the directed cell only, and has a
#' zero diagonal, because the influence of a on b says nothing about the
#' influence of b on a.
#'
#' Respondents who left any pair blank, or whose answer falls outside the
#' declared scale, are dropped whole and counted. Completing partial matrices
#' (the Harker approach) is deliberately out of scope.
#'
#' @param data A data frame of responses, as produced by [read_responses()].
#' @param instrument An `sframe` instrument declaring `item_id`.
#' @param item_id Character. The id of a `"pairwise_comparison"` item.
#' @return A list with `matrices` (a list of square numeric matrices with
#'   dimnames), `n_respondents`, `n_dropped`, `dropped` (a data frame of row
#'   number and reason), `items`, and `scale`.
#' @export
#' @seealso [sframe_aggregate_judgements()], [sframe_collected_weights()]
#' @examples
#' crits <- c("price", "speed")
#' study <- sf_instrument(
#'   title = "Demo", components = list(
#'     sf_item("pairs", "Compare the criteria", type = "pairwise_comparison",
#'             comparison_items = crits)
#'   )
#' )
#' responses <- data.frame(pairs__price__vs__speed = c(3, -5))
#' sframe_assemble_pairwise(responses, study, "pairs")$matrices[[1]]
sframe_assemble_pairwise <- function(data, instrument, item_id) {
  item <- sframe_decision_item(instrument, item_id, "pairwise_comparison")
  items <- as.character(item$comparison_items %||% character(0))
  scale <- item$comparison_scale %||% "saaty"
  cols <- sframe_comparison_columns(item)
  pairs <- sframe_comparison_pairs(items, scale)

  missing_cols <- setdiff(cols, colnames(data))
  if (length(missing_cols) > 0) {
    sframe_abort_import(sprintf(
      "Item '%s' is missing %d of its %d comparison column(s): %s.",
      item_id, length(missing_cols), length(cols),
      paste(missing_cols, collapse = ", ")
    ))
  }

  n <- length(items)
  values <- lapply(cols, function(cl) suppressWarnings(as.numeric(data[[cl]])))
  names(values) <- cols

  matrices <- list()
  dropped_rows <- integer(0)
  dropped_reason <- character(0)

  for (r in seq_len(nrow(data))) {
    v <- vapply(values, function(x) x[r], numeric(1), USE.NAMES = FALSE)
    if (anyNA(v)) {
      dropped_rows <- c(dropped_rows, r)
      dropped_reason <- c(dropped_reason, "incomplete")
      next
    }
    if (identical(scale, "influence")) {
      ok <- all(v == round(v)) && all(v >= 0 & v <= 4)
    } else {
      ok <- all(v == round(v)) && all(abs(v) >= 1 & abs(v) <= 9) &&
        !any(v == -1)
    }
    if (!ok) {
      dropped_rows <- c(dropped_rows, r)
      dropped_reason <- c(dropped_reason, "out of range")
      next
    }
    m <- matrix(if (identical(scale, "influence")) 0 else 1,
                nrow = n, ncol = n, dimnames = list(items, items))
    for (k in seq_len(nrow(pairs))) {
      a <- pairs$a[k]
      b <- pairs$b[k]
      if (identical(scale, "influence")) {
        m[a, b] <- v[k]
      } else if (v[k] >= 1) {
        m[a, b] <- v[k]
        m[b, a] <- 1 / v[k]
      } else {
        m[a, b] <- 1 / abs(v[k])
        m[b, a] <- abs(v[k])
      }
    }
    if (identical(scale, "influence")) diag(m) <- 0 else diag(m) <- 1
    matrices[[length(matrices) + 1L]] <- m
  }

  list(
    matrices      = matrices,
    n_respondents = length(matrices),
    n_dropped     = length(dropped_rows),
    dropped       = data.frame(row = dropped_rows, reason = dropped_reason,
                               stringsAsFactors = FALSE),
    items         = items,
    scale         = scale,
    item_id       = item_id
  )
}

# ---------------------------------------------------------------------------
# Consistency (Saaty)
# ---------------------------------------------------------------------------

# Saaty's random index, defined for n = 1..10 only. Beyond 10 no consistency
# ratio exists, so callers get NA rather than a clamped stand-in.
sframe_saaty_ri <- c(0, 0, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49)

sframe_principal_eigen <- function(m) {
  ev <- eigen(m)
  idx <- which.max(Re(ev$values))
  vec <- abs(Re(ev$vectors[, idx]))
  total <- sum(vec)
  weights <- if (total > 0) vec / total else rep(1 / nrow(m), nrow(m))
  names(weights) <- rownames(m)
  list(weights = weights, lambda_max = Re(ev$values[idx]))
}

sframe_consistency_ratio <- function(m) {
  n <- nrow(m)
  if (n > length(sframe_saaty_ri)) return(NA_real_)
  if (n < 3) return(0)
  e <- sframe_principal_eigen(m)
  ci <- (e$lambda_max - n) / (n - 1)
  ri <- sframe_saaty_ri[n]
  if (ri <= 0) return(0)
  ci / ri
}

# The per-respondent consistency distribution. Always computed for a saaty
# item, whether or not it is used to filter, because reviewers ask for it.
sframe_consistency_summary <- function(matrices) {
  if (length(matrices) == 0) {
    return(list(cr = numeric(0), min = NA_real_, median = NA_real_,
                max = NA_real_, share_above = NA_real_, threshold = 0.10))
  }
  cr <- vapply(matrices, sframe_consistency_ratio, numeric(1))
  list(
    cr          = cr,
    min         = min(cr, na.rm = TRUE),
    median      = stats::median(cr, na.rm = TRUE),
    max         = max(cr, na.rm = TRUE),
    share_above = mean(cr >= 0.10, na.rm = TRUE),
    threshold   = 0.10
  )
}

# ---------------------------------------------------------------------------
# Aggregation across respondents
# ---------------------------------------------------------------------------

#' Aggregate individual judgement matrices
#'
#' Aggregation of individual judgements (AIJ) across respondents. The
#' element-wise geometric mean is the standard choice for reciprocal AHP
#' matrices, because it is the only mean that preserves reciprocity: the
#' arithmetic mean of `m[a, b]` and the arithmetic mean of `m[b, a]` are not
#' reciprocals of each other. DEMATEL matrices are not reciprocal, so they
#' aggregate arithmetically.
#'
#' @param matrices Either a list of square numeric matrices or the object
#'   returned by [sframe_assemble_pairwise()].
#' @param method `"geometric"` (the AHP default) or `"arithmetic"` (DEMATEL).
#' @param cr_filter Logical. When `TRUE`, individual matrices with a
#'   consistency ratio at or above 0.10 are dropped before aggregation.
#'   Applies to reciprocal matrices only. Default `FALSE`.
#' @return A list with `matrix`, `method`, `n_respondents`, `n_dropped`,
#'   `n_dropped_consistency`, and `consistency` (the per-respondent CR
#'   distribution, or `NULL` for a non-reciprocal set).
#' @export
#' @seealso [sframe_assemble_pairwise()]
#' @examples
#' m1 <- matrix(c(1, 3, 1 / 3, 1), 2, 2)
#' m2 <- matrix(c(1, 5, 1 / 5, 1), 2, 2)
#' sframe_aggregate_judgements(list(m1, m2))$matrix
sframe_aggregate_judgements <- function(matrices,
                                        method = c("geometric", "arithmetic"),
                                        cr_filter = FALSE) {
  method <- match.arg(method)
  assembly <- NULL
  if (is.list(matrices) && !is.null(matrices$matrices)) {
    assembly <- matrices
    matrices <- matrices$matrices
  }
  if (length(matrices) == 0) {
    sframe_abort_validation(
      "No complete comparison matrices are available to aggregate."
    )
  }
  reciprocal <- identical(method, "geometric")
  consistency <- if (reciprocal) sframe_consistency_summary(matrices) else NULL

  n_dropped_consistency <- 0L
  if (isTRUE(cr_filter) && reciprocal) {
    keep <- !is.na(consistency$cr) & consistency$cr < consistency$threshold
    n_dropped_consistency <- sum(!keep)
    if (!any(keep)) {
      sframe_abort_validation(sprintf(
        paste0("All %d respondent matrices have a consistency ratio at or ",
               "above %.2f, so `cr_filter = TRUE` leaves nothing to ",
               "aggregate."),
        length(matrices), consistency$threshold
      ))
    }
    matrices <- matrices[keep]
  }

  dims <- vapply(matrices, function(m) paste(dim(m), collapse = "x"),
                 character(1))
  if (length(unique(dims)) > 1) {
    sframe_abort_validation(
      "The comparison matrices do not all have the same dimensions."
    )
  }

  stacked <- simplify2array(matrices)
  agg <- if (identical(method, "geometric")) {
    if (any(stacked <= 0)) {
      sframe_abort_validation(
        "Geometric aggregation needs strictly positive judgements."
      )
    }
    exp(apply(log(stacked), c(1, 2), mean))
  } else {
    apply(stacked, c(1, 2), mean)
  }
  dimnames(agg) <- dimnames(matrices[[1]])

  list(
    matrix                = agg,
    method                = method,
    n_respondents         = length(matrices),
    n_dropped             = assembly$n_dropped %||% 0L,
    n_dropped_consistency = n_dropped_consistency,
    consistency           = consistency
  )
}

# ---------------------------------------------------------------------------
# Collected weights
# ---------------------------------------------------------------------------

#' Criterion weights collected from respondents
#'
#' Resolves a weight vector from either kind of collected weight item, so that
#' a ranking runner never has to know which one the researcher used. A
#' `"criteria_weight"` item is renormalised to sum 1 per respondent before the
#' arithmetic mean is taken, so a respondent who allocated 90 points in total
#' carries the same influence as one who allocated exactly 100. A
#' `"pairwise_comparison"` item is assembled, aggregated geometrically, and
#' reduced to its principal eigenvector.
#'
#' @param data A data frame of responses.
#' @param instrument An `sframe` instrument declaring `item_id`.
#' @param item_id Character. A `"criteria_weight"` or `"pairwise_comparison"`
#'   item id.
#' @param cr_filter Logical. Passed to [sframe_aggregate_judgements()] for a
#'   pairwise item. Ignored otherwise.
#' @return A list with `weights` (a named numeric vector summing to 1),
#'   `criteria`, `source`, `item_id`, `n_respondents`, `n_dropped`, and
#'   `consistency` (pairwise items only).
#' @export
#' @seealso [sframe_assemble_pairwise()]
#' @examples
#' study <- sf_instrument(
#'   title = "Demo", components = list(
#'     sf_item("pts", "Divide 100 points", type = "criteria_weight",
#'             comparison_items = c("price", "speed"))
#'   )
#' )
#' sframe_collected_weights(
#'   data.frame(pts__price = c(60, 40), pts__speed = c(40, 60)), study, "pts"
#' )$weights
sframe_collected_weights <- function(data, instrument, item_id,
                                     cr_filter = FALSE) {
  item <- sframe_decision_item(instrument, item_id,
                               sframe_expanded_comparison_types)

  if (identical(item$type, "pairwise_comparison")) {
    if (identical(item$comparison_scale %||% "saaty", "influence")) {
      sframe_abort_validation(sprintf(
        paste0("Item '%s' uses the influence scale, which measures directed ",
               "influence rather than relative importance, so it cannot ",
               "supply criterion weights."),
        item_id
      ))
    }
    assembly <- sframe_assemble_pairwise(data, instrument, item_id)
    agg <- sframe_aggregate_judgements(assembly, "geometric",
                                       cr_filter = cr_filter)
    eig <- sframe_principal_eigen(agg$matrix)
    return(list(
      weights       = eig$weights,
      criteria      = names(eig$weights),
      source        = "pairwise",
      item_id       = item_id,
      n_respondents = agg$n_respondents,
      n_dropped     = agg$n_dropped + agg$n_dropped_consistency,
      consistency   = agg$consistency,
      matrix        = agg$matrix
    ))
  }

  criteria <- as.character(item$comparison_items %||% character(0))
  cols <- sframe_comparison_columns(item)
  missing_cols <- setdiff(cols, colnames(data))
  if (length(missing_cols) > 0) {
    sframe_abort_import(sprintf(
      "Item '%s' is missing %d of its %d allocation column(s): %s.",
      item_id, length(missing_cols), length(cols),
      paste(missing_cols, collapse = ", ")
    ))
  }

  m <- vapply(cols, function(cl) suppressWarnings(as.numeric(data[[cl]])),
              numeric(nrow(data)))
  m <- matrix(m, nrow = nrow(data), ncol = length(cols),
              dimnames = list(NULL, criteria))
  keep <- stats::complete.cases(m) & rowSums(m) > 0 & apply(m >= 0, 1, all)
  n_dropped <- sum(!keep)
  if (!any(keep)) {
    sframe_abort_validation(sprintf(
      "Item '%s' has no usable point allocations.", item_id
    ))
  }
  m <- m[keep, , drop = FALSE]
  m <- m / rowSums(m)
  weights <- colMeans(m)
  weights <- weights / sum(weights)
  names(weights) <- criteria

  list(
    weights       = weights,
    criteria      = criteria,
    source        = "criteria_weight",
    item_id       = item_id,
    n_respondents = nrow(m),
    n_dropped     = n_dropped,
    consistency   = NULL
  )
}

# ---------------------------------------------------------------------------
# Path C: the rated performance matrix
# ---------------------------------------------------------------------------

#' Build a performance matrix from rated matrix items
#'
#' The third collection path: respondents rate every alternative on every
#' criterion with ordinary matrix items, one item per criterion with the
#' alternatives as its rows, and the decision matrix is the per-cell
#' aggregate. No new item type is needed. Per-cell counts and standard
#' deviations are kept so the report can show how firm each cell is.
#'
#' @param data A data frame of responses.
#' @param instrument An `sframe` instrument declaring every id in `items`.
#' @param items Character vector of `"matrix"` item ids, one per criterion, in
#'   the intended criterion order. Every item must declare the same
#'   `matrix_items` (the alternatives) in the same order.
#' @param statistic `"mean"` or `"median"`.
#' @return A list with `matrix` (alternatives x criteria, with dimnames), `n`,
#'   `sd`, `alternatives`, `criteria`, and `statistic`.
#' @export
#' @seealso [sframe_collected_weights()]
sframe_rated_matrix <- function(data, instrument, items,
                                statistic = c("mean", "median")) {
  statistic <- match.arg(statistic)
  items <- as.character(items)
  if (length(items) == 0) {
    sframe_abort_validation(
      "A rated performance matrix needs at least one matrix item."
    )
  }
  declared <- lapply(items, function(id) {
    sframe_decision_item(instrument, id, "matrix")
  })
  alternatives <- as.character(declared[[1]]$matrix_items %||% character(0))
  if (length(alternatives) == 0) {
    sframe_abort_validation(sprintf(
      "Item '%s' declares no matrix_items, so it has no alternatives to rate.",
      items[1]
    ))
  }
  for (k in seq_along(declared)) {
    rows <- as.character(declared[[k]]$matrix_items %||% character(0))
    if (!identical(rows, alternatives)) {
      sframe_abort_validation(sprintf(
        paste0("Item '%s' rates %s, but item '%s' rates %s. Every rated ",
               "criterion must list the same alternatives in the same ",
               "order."),
        items[k], paste(rows, collapse = ", "),
        items[1], paste(alternatives, collapse = ", ")
      ))
    }
  }

  agg_fun <- if (identical(statistic, "median")) stats::median else mean
  out <- matrix(NA_real_, nrow = length(alternatives), ncol = length(items),
                dimnames = list(alternatives, items))
  counts <- out
  sds <- out
  for (j in seq_along(items)) {
    for (i in seq_along(alternatives)) {
      col <- paste0(items[j], "__", alternatives[i])
      if (!col %in% colnames(data)) {
        sframe_abort_import(sprintf(
          "Rated performance matrix needs column '%s', which is absent.", col
        ))
      }
      x <- suppressWarnings(as.numeric(data[[col]]))
      x <- x[!is.na(x)]
      counts[i, j] <- length(x)
      if (length(x) == 0) next
      out[i, j] <- agg_fun(x)
      sds[i, j] <- if (length(x) > 1) stats::sd(x) else NA_real_
    }
  }
  empty <- which(is.na(out), arr.ind = TRUE)
  if (nrow(empty) > 0) {
    sframe_abort_validation(sprintf(
      "No usable ratings for alternative '%s' on criterion '%s'.",
      alternatives[empty[1, 1]], items[empty[1, 2]]
    ))
  }

  list(
    matrix       = out,
    n            = counts,
    sd           = sds,
    alternatives = alternatives,
    criteria     = items,
    statistic    = statistic
  )
}

# ---------------------------------------------------------------------------
# Serialisation of matrix-valued options (todo_text_analysis.md section 1e)
# ---------------------------------------------------------------------------

#' Normalise the decision options of an analysis-plan block
#'
#' A researcher-supplied performance matrix round-trips through JSON as a list
#' of numeric row vectors with its dimnames dropped, so it is stored as
#' `options$matrix` plus the `options$alternatives` and `options$criteria`
#' label vectors and rebuilt here. Every length agreement between the matrix,
#' its labels, the weights, and the criterion types is checked once, in one
#' place, with the exact mismatch named.
#'
#' @section PROMETHEE preference functions:
#' A PROMETHEE block takes `options$preference_function`, one of `"usual"`,
#' `"linear"`, or `"level"`, and `options$thresholds` for the 2 that need
#' them. The default is `"usual"`, Brans and Vincke's type I step function,
#' which needs no thresholds and so adds no researcher degrees of freedom.
#'
#' This default differs from several other MCDM implementations, which
#' default to the linear (V-shape) function and derive its thresholds from
#' the range of the supplied data. Deriving thresholds that way makes the
#' result depend on choices the researcher never declared, which is what this
#' package exists to prevent, so surveyframe requires a threshold-bearing
#' preference function to be asked for explicitly.
#'
#' The choice changes the answer. Net flows always differ between the 2
#' functions, and the ranking itself changed in 226 of 400 randomly drawn
#' 4-alternative by 3-criterion matrices. So a ranking cross-checked against
#' an implementation that defaults to `"linear"` will often disagree unless
#' `preference_function = "linear"` is set here and the same thresholds are
#' supplied on both sides. The preference function actually used is always
#' named in the block's APA sentence. Note also that `"usual"` produces tied
#' ranks readily, because a step function scores every non-zero difference
#' identically.
#'
#' @param options The `options` list of a decision analysis block.
#' @return The same list with `matrix` rebuilt as a numeric matrix carrying
#'   dimnames, and `weights` and `criteria_types` coerced and checked.
#' @export
#' @seealso [run_analysis_plan()]
#' @examples
#' sframe_decision_options(list(
#'   matrix = list(c(4, 210), c(3, 180)),
#'   alternatives = c("Alpha", "Basilica"),
#'   criteria = c("service", "price"),
#'   criteria_types = c("benefit", "cost")
#' ))$matrix
sframe_decision_options <- function(options) {
  options <- options %||% list()
  alternatives <- as.character(options[["alternatives"]] %||% character(0))
  criteria <- as.character(options[["criteria"]] %||% character(0))

  m <- options[["matrix"]]
  if (!is.null(m)) {
    if (is.data.frame(m)) {
      # A data.frame is also is.list()==TRUE, but its list elements are
      # columns, not rows: unlist()-ing it the way a genuine list-of-rows is
      # unlisted below would silently transpose the matrix (each data.frame
      # column read out in full before moving to the next). as.matrix()
      # preserves the row/column orientation the caller actually wrote.
      m <- as.matrix(m)
      storage.mode(m) <- "double"
    } else if (is.list(m)) {
      widths <- vapply(m, length, integer(1))
      if (length(unique(widths)) > 1) {
        sframe_abort_validation(sprintf(
          paste0("`options$matrix` rows have differing lengths (%s). Every ",
                 "row must have one value per criterion."),
          paste(widths, collapse = ", ")
        ))
      }
      m <- matrix(suppressWarnings(as.numeric(unlist(m, use.names = FALSE))),
                  nrow = length(m), byrow = TRUE)
    } else {
      m <- as.matrix(m)
      storage.mode(m) <- "double"
    }
    if (anyNA(m)) {
      sframe_abort_validation(
        "`options$matrix` contains values that are not numeric."
      )
    }
    if (length(alternatives) > 0 && length(alternatives) != nrow(m)) {
      sframe_abort_validation(sprintf(
        paste0("`options$alternatives` names %d alternative(s) but ",
               "`options$matrix` has %d row(s)."),
        length(alternatives), nrow(m)
      ))
    }
    if (length(criteria) > 0 && length(criteria) != ncol(m)) {
      sframe_abort_validation(sprintf(
        paste0("`options$criteria` names %d criterion/criteria but ",
               "`options$matrix` has %d column(s)."),
        length(criteria), ncol(m)
      ))
    }
    if (length(alternatives) == 0) {
      alternatives <- paste0("A", seq_len(nrow(m)))
    }
    if (length(criteria) == 0) {
      criteria <- paste0("C", seq_len(ncol(m)))
    }
    dimnames(m) <- list(alternatives, criteria)
    options[["matrix"]] <- m
    options[["alternatives"]] <- alternatives
    options[["criteria"]] <- criteria
  }

  n_criteria <- if (!is.null(m)) ncol(m) else length(criteria)

  if (!is.null(options[["weights"]])) {
    w <- suppressWarnings(as.numeric(unlist(options[["weights"]],
                                            use.names = FALSE)))
    if (anyNA(w)) {
      sframe_abort_validation(
        "`options$weights` contains values that are not numeric."
      )
    }
    if (n_criteria > 0 && length(w) != n_criteria) {
      sframe_abort_validation(sprintf(
        "`options$weights` has %d entry/entries but there are %d criteria.",
        length(w), n_criteria
      ))
    }
    if (length(criteria) == length(w)) names(w) <- criteria
    options[["weights"]] <- w
  }

  if (!is.null(options[["criteria_types"]])) {
    ct <- as.character(unlist(options[["criteria_types"]], use.names = FALSE))
    bad <- setdiff(unique(ct), c("benefit", "cost"))
    if (length(bad) > 0) {
      sframe_abort_validation(sprintf(
        paste0("`options$criteria_types` must be \"benefit\" or \"cost\". ",
               "Found: %s."),
        paste(bad, collapse = ", ")
      ))
    }
    if (n_criteria > 0 && length(ct) != n_criteria) {
      sframe_abort_validation(sprintf(
        paste0("`options$criteria_types` has %d entry/entries but there are ",
               "%d criteria."),
        length(ct), n_criteria
      ))
    }
    options[["criteria_types"]] <- ct
  }

  options
}
