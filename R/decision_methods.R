# R/decision_methods.R
# The MCDM computation layer and its runners. Computation helpers are pure
# base R, take (matrix, weights, criteria_types), and return raw outputs. The
# runners wrap them, resolve their two inputs through the fixed order
# (researcher-supplied options first, collected item roles second, typed error
# third), record where each input came from, and return the flat named list
# every other runner in the package returns.

# The decision family's method ids. Descriptive metadata elsewhere in the
# package dispatches on `test`, never on `family`, so this vector is what the
# engine uses to recognise a decision block. Ids are added here as each
# method lands.
sframe_decision_methods <- c("topsis", "ahp", "anp", "dematel", "vikor",
                             "moora", "smart", "waspas", "promethee",
                             "electre")

# ---------------------------------------------------------------------------
# Shared input validation
# ---------------------------------------------------------------------------

# Returns a list carrying the cleaned inputs plus `note`, or a single `error`
# string. Runners never stop(): they hand their error back to the caller so
# the report can show it beside the research question.
sframe_check_decision_input <- function(x, weights, criteria_types) {
  if (!is.matrix(x) || !is.numeric(x)) {
    return(list(error = "The decision matrix must be a numeric matrix."))
  }
  if (nrow(x) < 2) {
    return(list(error = "A ranking needs at least 2 alternatives."))
  }
  if (anyNA(x)) {
    return(list(error = "The decision matrix contains missing values."))
  }
  if (!all(is.finite(x))) {
    return(list(error = "The decision matrix contains infinite values."))
  }
  weights <- suppressWarnings(as.numeric(weights))
  if (length(weights) != ncol(x)) {
    return(list(error = sprintf(
      "There are %d weight(s) for %d criteria.", length(weights), ncol(x)
    )))
  }
  if (anyNA(weights) || any(weights < 0)) {
    return(list(error = "Weights must be non-negative numbers."))
  }
  if (sum(weights) <= 0) {
    return(list(error = "The weights sum to zero."))
  }
  note <- NULL
  if (abs(sum(weights) - 1) > 1e-6) {
    note <- sprintf(
      "The supplied weights summed to %.4f and were renormalised to 1.",
      sum(weights)
    )
    weights <- weights / sum(weights)
  }
  criteria_types <- as.character(criteria_types)
  if (length(criteria_types) != ncol(x)) {
    return(list(error = sprintf(
      "There are %d criterion type(s) for %d criteria.",
      length(criteria_types), ncol(x)
    )))
  }
  bad <- setdiff(unique(criteria_types), c("benefit", "cost"))
  if (length(bad) > 0) {
    return(list(error = sprintf(
      "Criterion types must be \"benefit\" or \"cost\". Found: %s.",
      paste(bad, collapse = ", ")
    )))
  }
  list(matrix = x, weights = weights, criteria_types = criteria_types,
       note = note)
}

# Bounds for the tuning values the ranking methods read. Returns an error
# string, or NULL. Used by the runners and by sensitivity analysis, so a value
# outside a method's range is refused wherever it arrives. Before this, a v or
# lambda above 1 turned a blend into an extrapolation, and PROMETHEE and
# ELECTRE thresholds were used whatever their sign or order.
sframe_check_decision_tuning <- function(method, options, n_criteria) {
  options <- options %||% list()
  in_unit <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x) && x >= 0 && x <= 1
  if (identical(method, "vikor") && !is.null(options[["v"]]) && !in_unit(options[["v"]])) {
    return("VIKOR's v must be a single number from 0 to 1.")
  }
  if (identical(method, "waspas") && !is.null(options[["lambda"]]) && !in_unit(options[["lambda"]])) {
    return("WASPAS's lambda must be a single number from 0 to 1.")
  }
  if (identical(method, "electre")) {
    for (key in c("concordance_threshold", "discordance_threshold")) {
      if (!is.null(options[[key]]) && !in_unit(options[[key]])) {
        return(sprintf("ELECTRE's %s must be a single number from 0 to 1.", key))
      }
    }
  }
  if (identical(method, "promethee")) {
    fn <- options[["preference_function"]] %||% "usual"
    if (!fn %in% c("usual", "linear", "level")) {
      return(sprintf("PROMETHEE's preference_function must be usual, linear or level, not '%s'.", fn))
    }
    thr <- options[["thresholds"]]
    if (!is.null(thr)) {
      if (!is.list(thr) || length(thr) != n_criteria) {
        return(sprintf("PROMETHEE needs one threshold set per criterion, %d in all.", n_criteria))
      }
      for (j in seq_along(thr)) {
        p <- thr[[j]][["preference"]]; q <- thr[[j]][["indifference"]] %||% 0
        ok <- is.numeric(p) && length(p) == 1 && is.finite(p) && p > 0 &&
          is.numeric(q) && length(q) == 1 && is.finite(q) && q >= 0 && q < p
        if (!ok) {
          return(sprintf(paste0("PROMETHEE threshold %d must have a positive preference ",
                                "threshold above a non-negative indifference threshold."), j))
        }
      }
    }
  }
  NULL
}

# ---------------------------------------------------------------------------
# TOPSIS computation
# ---------------------------------------------------------------------------

# Vector (Euclidean) normalisation, the weighted ideal and anti-ideal points,
# the two separation distances, and the closeness coefficient. Direction is
# handled once, at the ideal/anti-ideal step: normalisation itself never
# flips a cost column, since doing both cancels out and silently scores a
# cost criterion as a benefit.
sframe_topsis_compute <- function(x, weights, criteria_types) {
  norms <- sqrt(colSums(x^2))
  norms[norms == 0] <- 1
  normalised <- sweep(x, 2, norms, "/")
  weighted <- sweep(normalised, 2, weights, "*")

  benefit <- criteria_types == "benefit"
  ideal <- ifelse(benefit, apply(weighted, 2, max), apply(weighted, 2, min))
  anti_ideal <- ifelse(benefit, apply(weighted, 2, min),
                       apply(weighted, 2, max))

  d_plus <- sqrt(rowSums(sweep(weighted, 2, ideal, "-")^2))
  d_minus <- sqrt(rowSums(sweep(weighted, 2, anti_ideal, "-")^2))
  denom <- d_plus + d_minus
  scores <- ifelse(denom > 0, d_minus / denom, 0)
  names(scores) <- rownames(x)

  list(
    scores       = scores,
    ranks        = rank(-scores, ties.method = "min"),
    normalised   = normalised,
    weighted     = weighted,
    ideal        = ideal,
    anti_ideal   = anti_ideal,
    d_plus       = d_plus,
    d_minus      = d_minus
  )
}

# ---------------------------------------------------------------------------
# Input resolution, shared by every ranking runner
# ---------------------------------------------------------------------------

# The resolution order for the decision family, in one place so all 10 methods
# behave identically: researcher-supplied options first, collected item roles
# second, a typed error naming what is missing third. Provenance is recorded
# for both inputs so the report can say where the numbers came from.
sframe_resolve_decision_inputs <- function(data, roles, options, instrument,
                                           method = "this method") {
  # Whether a supplied matrix names its criteria, before defaults fill them.
  named_matrix <- !is.null((options %||% list())[["criteria"]]) ||
    !is.null(colnames((options %||% list())[["matrix"]]))
  options <- sframe_decision_options(options)
  out <- list(notes = character(0))

  # 1. The performance matrix.
  if (!is.null(options[["matrix"]])) {
    out$matrix <- options[["matrix"]]
    out$matrix_source <- "supplied"
  } else {
    performance_items <- sframe_role_values(roles, "performance_items")
    if (length(performance_items) == 0) {
      return(list(error = sprintf(
        paste0("%s needs a performance matrix. Supply `options$matrix` with ",
               "`options$alternatives` and `options$criteria`, or declare a ",
               "`performance_items` role naming one matrix item per ",
               "criterion."),
        method
      )))
    }
    rated <- sframe_rated_matrix(data, instrument, performance_items)
    out$matrix <- rated$matrix
    out$matrix_source <- "collected"
    out$rated <- rated
    out$notes <- c(out$notes, sprintf(
      "The performance matrix is the respondent %s of %d rated item(s).",
      rated$statistic, length(performance_items)
    ))
  }
  criteria <- colnames(out$matrix)

  # 2. The criterion weights.
  if (!is.null(options[["weights"]])) {
    out$weights <- options[["weights"]]
    out$weights_source <- "supplied"
  } else {
    weights_item <- sframe_role_values(roles, "weights_item")
    if (length(weights_item) == 0) {
      return(list(error = sprintf(
        paste0("%s needs criterion weights. Supply `options$weights`, or ",
               "declare a `weights_item` role naming a criteria_weight or ",
               "pairwise_comparison item."),
        method
      )))
    }
    collected <- sframe_collected_weights(
      data, instrument, weights_item[1],
      cr_filter = isTRUE(options[["cr_filter"]])
    )
    if (length(collected$weights) != ncol(out$matrix)) {
      return(list(error = sprintf(
        paste0("Item '%s' weights %d criterion/criteria but the performance ",
               "matrix has %d column(s)."),
        weights_item[1], length(collected$weights), ncol(out$matrix)
      )))
    }
    # Weights follow the matrix by criterion name. A matrix supplied with no
    # criterion names takes the item's criteria in the item's order.
    if (identical(out$matrix_source, "supplied") && !named_matrix) {
      colnames(out$matrix) <- names(collected$weights)
      criteria <- colnames(out$matrix)
      out$notes <- c(out$notes, sprintf(
        "The supplied matrix names no criteria, so its columns take item '%s' criteria in order.",
        weights_item[1]))
    } else if (setequal(names(collected$weights), criteria)) {
      # aligned by name below
    } else if (identical(out$matrix_source, "collected")) {
      # Rated items are declared one per criterion in criterion order, and
      # their ids need not match the weight item's criterion names. They pair
      # in declared order, and every pairing is written into the notes.
      out$notes <- c(out$notes, sprintf(
        "The rated items pair with item '%s' criteria in declared order: %s.",
        weights_item[1],
        paste(paste(criteria, "with", names(collected$weights)), collapse = ", ")))
      names(collected$weights) <- criteria
    } else {
      return(list(error = sprintf(
        "Item '%s' weights %s, but the performance matrix criteria are %s.",
        weights_item[1], paste(names(collected$weights), collapse = ", "),
        paste(criteria, collapse = ", "))))
    }
    out$weights <- collected$weights[criteria]
    out$weights_source <- "collected"
    out$collected_weights <- collected
    out$notes <- c(out$notes, sprintf(
      "Weights come from %d respondent(s) via item '%s'%s.",
      collected$n_respondents, collected$item_id,
      if (collected$n_dropped > 0) {
        sprintf(", with %d dropped as incomplete or inconsistent",
                collected$n_dropped)
      } else ""
    ))
  }
  # Align by name against the criteria the resolved matrix actually has.
  # sframe_decision_options() aligns whatever it can, but a collected
  # performance matrix is built here, after options were normalised, so at that
  # point there were no criterion names to align against. Named weights were
  # then carried through and applied by position, which is how a matrix ordered
  # price, quality took weights supplied as quality, price the wrong way round.
  if (length(criteria) > 0 && length(out$weights) == length(criteria)) {
    out$weights <- sframe_align_to_criteria(
      out$weights, names(out$weights), criteria, "`options$weights`")
  } else if (is.null(names(out$weights)) &&
             length(criteria) == length(out$weights)) {
    names(out$weights) <- criteria
  }

  # 3. Criterion directions. All benefit unless declared otherwise, and aligned
  # by name for the same reason as the weights.
  out$criteria_types <- options[["criteria_types"]] %||%
    rep("benefit", ncol(out$matrix))
  if (length(criteria) > 0 && length(out$criteria_types) == length(criteria) &&
      !is.null(names(out$criteria_types))) {
    out$criteria_types <- sframe_align_to_criteria(
      out$criteria_types, names(out$criteria_types), criteria,
      "`options$criteria_types`")
  }
  out$alternatives <- rownames(out$matrix)
  out$criteria <- criteria
  out$options <- options
  out
}

# ---------------------------------------------------------------------------
# TOPSIS runner
# ---------------------------------------------------------------------------

sframe_run_topsis <- function(data, roles, options, instrument) {
  resolved <- sframe_resolve_decision_inputs(data, roles, options, instrument,
                                             method = "TOPSIS")
  if (!is.null(resolved$error)) {
    return(list(test = "topsis", error = resolved$error))
  }
  checked <- sframe_check_decision_input(resolved$matrix, resolved$weights,
                                         resolved$criteria_types)
  if (!is.null(checked$error)) {
    return(list(test = "topsis", error = checked$error))
  }
  fit <- sframe_topsis_compute(checked$matrix, checked$weights,
                               checked$criteria_types)

  alternatives <- resolved$alternatives
  order_by_rank <- order(fit$ranks)
  table <- data.frame(
    Alternative = alternatives[order_by_rank],
    Score       = round(as.numeric(fit$scores)[order_by_rank], 4),
    Rank        = as.integer(fit$ranks)[order_by_rank],
    stringsAsFactors = FALSE
  )
  best <- alternatives[which.min(fit$ranks)]
  notes <- c(resolved$notes, checked$note)

  list(
    test           = "topsis",
    apa            = sprintf(
      paste0("TOPSIS ranked %d alternatives on %d criteria. %s ranked first ",
             "with a closeness coefficient of %.3f."),
      length(alternatives), ncol(checked$matrix), best, max(fit$scores)
    ),
    prompt         = paste0(
      "Report the full ranking, not the winner alone. Closeness ",
      "coefficients close together mean the alternatives are hard to ",
      "separate, so state where the weights came from and check how far the ",
      "ranking moves when they are perturbed."
    ),
    table          = table,
    score_label    = "Closeness coefficient",
    scores         = fit$scores,
    ranks          = fit$ranks,
    alternatives   = alternatives,
    criteria       = resolved$criteria,
    criteria_types = checked$criteria_types,
    weights        = checked$weights,
    weights_source = resolved$weights_source,
    matrix_source  = resolved$matrix_source,
    separation_positive = fit$d_plus,
    separation_negative = fit$d_minus,
    ideal          = fit$ideal,
    anti_ideal     = fit$anti_ideal,
    consistency    = resolved$collected_weights$consistency,
    notes          = notes[!vapply(notes, is.null, logical(1))]
  )
}

# ---------------------------------------------------------------------------
# AHP: consistency ratio and computation
# ---------------------------------------------------------------------------

# Saaty's consistency ratio for a single square reciprocal judgement matrix,
# named after the 06 guide's `.ahp_cr()` but kept on the file's `sframe_`
# prefix convention rather than a leading dot. Builds on the principal
# eigenvector and CR machinery already unit-tested in R/decision_data.R
# (`sframe_principal_eigen()`, `sframe_consistency_ratio()`) rather than
# duplicating them; the only new behaviour is turning the silent `NA` that
# `sframe_consistency_ratio()` returns past n = 10 into a typed error, since
# Saaty's random index table simply stops there and a runner must say so
# rather than carry on with an undefined ratio.
sframe_ahp_cr <- function(m) {
  n <- nrow(m)
  if (n > length(sframe_saaty_ri)) {
    return(list(error = sprintf(
      paste0("AHP's consistency ratio is only defined for up to %d ",
             "criteria (Saaty's random index table stops there). This ",
             "matrix compares %d."),
      length(sframe_saaty_ri), n
    )))
  }
  list(cr = sframe_consistency_ratio(m), error = NULL)
}

# The AHP priority-vector computation: the principal eigenvector of a square
# reciprocal judgement matrix, normalised to sum 1, plus its consistency
# ratio. `m` must already be square and reciprocal (the output of
# `sframe_assemble_pairwise()` + `sframe_aggregate_judgements()`, or a
# researcher-supplied matrix). Returns `error` instead of computing when the
# matrix is too large for Saaty's RI table; never aborts on inconsistency,
# only flags it, since a poorly-judged matrix is still informative and the
# researcher should see the weights to judge for themselves.
sframe_ahp_compute <- function(m) {
  if (!is.matrix(m) || !is.numeric(m)) {
    return(list(error = "The AHP judgement matrix must be a numeric matrix."))
  }
  if (nrow(m) != ncol(m)) {
    return(list(error = "The AHP judgement matrix must be square."))
  }
  if (nrow(m) < 2) {
    return(list(error = "AHP needs at least 2 criteria to compare."))
  }
  if (anyNA(m)) {
    return(list(error = "The AHP judgement matrix contains missing values."))
  }
  # A judgement matrix must be positive, finite, unit-diagonal and reciprocal.
  # Without these checks a matrix such as rows (1, 9) and (9, 1) received a
  # consistency ratio of 0 and equal weights.
  if (!all(is.finite(m)) || any(m <= 0)) {
    return(list(error = "The AHP judgement matrix must hold positive finite numbers."))
  }
  if (max(abs(diag(m) - 1)) > 1e-6) {
    return(list(error = "The AHP judgement matrix must have 1 on its diagonal."))
  }
  if (max(abs(m * t(m) - 1)) > 1e-6) {
    return(list(error = paste0(
      "The AHP judgement matrix must be reciprocal, with m[a, b] equal to ",
      "1 / m[b, a] for every pair.")))
  }
  cr_result <- sframe_ahp_cr(m)
  if (!is.null(cr_result$error)) {
    return(list(error = cr_result$error))
  }
  eig <- sframe_principal_eigen(m)
  list(
    weights             = eig$weights,
    lambda_max          = eig$lambda_max,
    cr                  = cr_result$cr,
    consistency_warning = !is.na(cr_result$cr) && cr_result$cr >= 0.10
  )
}

# ---------------------------------------------------------------------------
# AHP: matrix resolution shared with ANP
# ---------------------------------------------------------------------------

# AHP and ANP both start from a square judgement matrix rather than TOPSIS's
# performance-matrix-plus-weights shape, so they resolve their input outside
# `sframe_resolve_decision_inputs()` (that helper's `options$matrix` path
# assumes an alternatives x criteria rectangle and would auto-label a square
# reciprocal matrix's rows and columns independently, losing the fact that
# both dimensions name the same criteria). The order matches every other
# decision method: researcher-supplied `options$matrix` first, a collected
# `pairwise` role second, a typed error naming what to supply third.
sframe_resolve_pairwise_matrix <- function(data, roles, options, instrument,
                                           method = "this method") {
  options <- options %||% list()
  out <- list(notes = character(0))

  if (!is.null(options[["matrix"]])) {
    raw <- options[["matrix"]]
    # A data frame is a list of columns, so it must be converted before the
    # list-of-rows branch, which read it transposed and reversed every
    # judgement while keeping perfect consistency.
    if (is.data.frame(raw)) {
      raw <- as.matrix(raw)
      storage.mode(raw) <- "double"
    } else if (is.list(raw)) {
      widths <- vapply(raw, length, integer(1))
      if (length(unique(widths)) > 1) {
        return(list(error = sprintf(
          paste0("%s's `options$matrix` rows have differing lengths (%s). A ",
                 "pairwise judgement matrix must be square."),
          method, paste(widths, collapse = ", ")
        )))
      }
      raw <- matrix(suppressWarnings(as.numeric(unlist(raw, use.names = FALSE))),
                    nrow = length(raw), byrow = TRUE)
    } else {
      raw <- as.matrix(raw)
      storage.mode(raw) <- "double"
    }
    if (nrow(raw) != ncol(raw)) {
      return(list(error = sprintf(
        paste0("%s needs a square pairwise judgement matrix, but ",
               "`options$matrix` has %d row(s) and %d column(s)."),
        method, nrow(raw), ncol(raw)
      )))
    }
    if (anyNA(raw)) {
      return(list(error = sprintf(
        "%s's `options$matrix` contains values that are not numeric.", method
      )))
    }
    criteria <- as.character(options[["criteria"]] %||% colnames(raw) %||%
                             paste0("C", seq_len(nrow(raw))))
    if (length(criteria) != nrow(raw)) {
      return(list(error = sprintf(
        paste0("`options$criteria` names %d criterion/criteria but ",
               "`options$matrix` has %d row(s)."),
        length(criteria), nrow(raw)
      )))
    }
    dimnames(raw) <- list(criteria, criteria)
    out$matrix <- raw
    out$matrix_source <- "supplied"
    return(out)
  }

  pairwise_item <- sframe_role_values(roles, "pairwise")
  if (length(pairwise_item) == 0) {
    return(list(error = sprintf(
      paste0("%s needs a pairwise judgement matrix. Supply `options$matrix` ",
             "as a square reciprocal matrix, or declare a `pairwise` role ",
             "naming a pairwise_comparison item."),
      method
    )))
  }
  # The 2 comparison scales are not interchangeable. AHP and ANP need
  # reciprocal relative importance on the Saaty ratio scale. An influence item
  # collects directed 0-4 influence with a zero diagonal and no reciprocity,
  # which would hand the eigenvector step a matrix it cannot interpret and
  # return plausible but meaningless weights. DEMATEL enforces the mirror of
  # this check on its own role.
  scale_item <- sframe_decision_item(instrument, pairwise_item[1],
                                     "pairwise_comparison")
  item_scale <- scale_item$comparison_scale %||% "saaty"
  if (!identical(item_scale, "saaty")) {
    return(list(error = sprintf(
      paste0("Item '%s' uses the '%s' comparison scale. %s needs relative ",
             "importance on the Saaty ratio scale, so its `pairwise` role ",
             "needs a pairwise_comparison item declared with ",
             "`comparison_scale = \"saaty\"`. An influence item belongs to a ",
             "DEMATEL block."),
      pairwise_item[1], item_scale, method
    )))
  }
  assembly <- sframe_assemble_pairwise(data, instrument, pairwise_item[1])
  if (assembly$n_respondents == 0) {
    return(list(error = sprintf(
      paste0("Item '%s' produced no usable comparison matrices: every ",
             "respondent left a pair blank or answered out of range."),
      pairwise_item[1]
    )))
  }
  agg <- sframe_aggregate_judgements(assembly, method = "geometric",
                                     cr_filter = isTRUE(options[["cr_filter"]]))
  out$matrix <- agg$matrix
  out$matrix_source <- "collected"
  out$aggregate <- agg
  out$notes <- c(out$notes, sprintf(
    "Weights come from %d respondent judgement matrice(s) via item '%s'%s.",
    agg$n_respondents, pairwise_item[1],
    if (agg$n_dropped_consistency > 0) {
      sprintf(", with %d dropped for a consistency ratio at or above 0.10",
              agg$n_dropped_consistency)
    } else ""
  ))
  out
}

# ---------------------------------------------------------------------------
# AHP runner
# ---------------------------------------------------------------------------

sframe_run_ahp <- function(data, roles, options, instrument) {
  resolved <- sframe_resolve_pairwise_matrix(data, roles, options, instrument,
                                             method = "AHP")
  if (!is.null(resolved$error)) {
    return(list(test = "ahp", error = resolved$error))
  }
  fit <- sframe_ahp_compute(resolved$matrix)
  if (!is.null(fit$error)) {
    return(list(test = "ahp", error = fit$error))
  }

  criteria <- names(fit$weights)
  order_by_weight <- order(fit$weights, decreasing = TRUE)
  table <- data.frame(
    Criterion = criteria[order_by_weight],
    Weight    = round(as.numeric(fit$weights)[order_by_weight], 4),
    Rank      = seq_along(criteria),
    stringsAsFactors = FALSE
  )
  best <- criteria[which.max(fit$weights)]

  notes <- resolved$notes
  if (isTRUE(fit$consistency_warning)) {
    notes <- c(notes, sprintf(
      paste0("The pairwise judgements are inconsistent (CR = %.3f, at or ",
             "above the 0.10 threshold Saaty recommends). Treat these ",
             "weights cautiously and consider revisiting the comparisons."),
      fit$cr
    ))
  }

  list(
    test                = "ahp",
    apa                 = sprintf(
      paste0("AHP derived priority weights for %d criteria from a pairwise ",
             "judgement matrix. '%s' carried the highest weight (%.3f)%s."),
      length(criteria), best, max(fit$weights),
      if (isTRUE(fit$consistency_warning)) {
        sprintf(", though the judgements' consistency ratio (%.3f) is at or ",
                fit$cr)
      } else ""
    ),
    prompt              = paste0(
      "Report the full weight vector, not the top criterion alone, and ",
      "state the consistency ratio: a CR at or above 0.10 means the ",
      "judgements contradict each other enough that the weights should be ",
      "read cautiously or the comparisons revisited before they feed a ",
      "downstream ranking method."
    ),
    table               = table,
    score_label         = "Priority weight",
    weights             = fit$weights,
    criteria            = criteria,
    matrix_source       = resolved$matrix_source,
    cr                  = fit$cr,
    lambda_max          = fit$lambda_max,
    consistency_warning = fit$consistency_warning,
    notes               = notes[!vapply(notes, is.null, logical(1))]
  )
}

# ---------------------------------------------------------------------------
# ANP: limiting supermatrix computation
# ---------------------------------------------------------------------------

# ANP generalises AHP's single hierarchy to a network of interdependent
# comparisons, resolved here as the limiting priority vector of a column-
# stochastic supermatrix under power iteration (Saaty 1996's limit
# supermatrix, W^k as k -> infinity, is the matrix every column of which
# equals this same limiting vector once the chain is primitive, so iterating
# the vector form is equivalent and far cheaper than iterating the matrix
# itself). `m` is any square non-negative matrix of relative influence
# between network nodes; it need not already be reciprocal (unlike AHP's
# input, ANP's supermatrix blocks are not required to be symmetric-
# reciprocal). Columns that sum to zero are treated as uninformative and
# spread evenly, matching the convention `mcdm::anp_method()` used for the
# same edge case. Guards at `max_iter` (a floor of 1000 per the harvest
# audit, not a target) and returns an `error` rather than a partial result
# if the vector has not settled within `tol`, so a badly conditioned network
# never hangs the caller.
sframe_anp_compute <- function(m, max_iter = 1000, tol = 1e-8) {
  if (!is.matrix(m) || !is.numeric(m)) {
    return(list(error = "The ANP supermatrix must be a numeric matrix."))
  }
  if (nrow(m) != ncol(m)) {
    return(list(error = "The ANP supermatrix must be square."))
  }
  n <- nrow(m)
  if (n < 2) {
    return(list(error = "ANP needs at least 2 network nodes to compare."))
  }
  if (anyNA(m)) {
    return(list(error = "The ANP supermatrix contains missing values."))
  }
  if (any(m < 0)) {
    return(list(error = "The ANP supermatrix must not contain negative values."))
  }

  col_sums <- colSums(m)
  normalised <- m
  for (j in seq_len(n)) {
    normalised[, j] <- if (col_sums[j] > 0) m[, j] / col_sums[j] else 1 / n
  }

  # The limit of the supermatrix powers exists only for a primitive network.
  # A uniform starting vector that stops moving proves nothing on its own: the
  # periodic network rows (0, 1), (1, 0) and the reducible identity both leave
  # it unchanged while their powers never settle. So the structure is checked
  # first. An irreducible network has one stationary priority vector. When it
  # is also aperiodic, power iteration reaches it. When it is periodic, it is
  # the Cesaro limit of the powers (Saaty 2004). A reducible network has no
  # single answer, since its limit depends on the starting node.
  reach <- normalised > 0
  closure <- reach | diag(n) > 0
  for (step in seq_len(n)) closure <- (closure %*% closure) > 0
  if (!all(closure)) {
    return(list(error = paste0(
      "The ANP network is reducible: some nodes cannot reach others through ",
      "the supermatrix, so its limiting priorities depend on where the chain ",
      "starts. Check that every node is connected.")))
  }
  power <- reach
  primitive <- FALSE
  for (step in seq_len((n - 1)^2 + 1)) {
    if (all(power)) { primitive <- TRUE; break }
    power <- (power %*% reach) > 0
  }
  if (!primitive) {
    ev <- eigen(normalised)
    idx <- which.min(Mod(ev$values - 1))
    vec <- abs(Re(ev$vectors[, idx]))
    priorities <- vec / sum(vec)
    names(priorities) <- rownames(m) %||% colnames(m)
    return(list(weights = priorities, normalised = normalised,
                iterations = 0L, converged = TRUE, limit_method = "cesaro"))
  }

  priorities <- rep(1 / n, n)
  converged <- FALSE
  iterations <- 0L
  for (iter in seq_len(max_iter)) {
    iterations <- iter
    updated <- as.numeric(normalised %*% priorities)
    total <- sum(updated)
    updated <- if (total > 0) updated / total else rep(1 / n, n)
    if (max(abs(updated - priorities)) < tol) {
      priorities <- updated
      converged <- TRUE
      break
    }
    priorities <- updated
  }
  if (!converged) {
    return(list(error = sprintf(
      paste0("ANP's limiting supermatrix did not converge within %d ",
             "iterations. The network of comparisons may be cyclical in a ",
             "way that never settles; check the supplied matrix."),
      max_iter
    )))
  }
  names(priorities) <- rownames(m) %||% colnames(m)

  list(
    weights      = priorities,
    normalised   = normalised,
    iterations   = iterations,
    converged    = converged,
    limit_method = "power"
  )
}

# ---------------------------------------------------------------------------
# ANP runner
# ---------------------------------------------------------------------------

sframe_run_anp <- function(data, roles, options, instrument) {
  resolved <- sframe_resolve_pairwise_matrix(data, roles, options, instrument,
                                             method = "ANP")
  if (!is.null(resolved$error)) {
    return(list(test = "anp", error = resolved$error))
  }
  max_iter <- options[["max_iter"]] %||% 1000
  fit <- sframe_anp_compute(resolved$matrix, max_iter = max_iter)
  if (!is.null(fit$error)) {
    return(list(test = "anp", error = fit$error))
  }

  criteria <- names(fit$weights)
  order_by_weight <- order(fit$weights, decreasing = TRUE)
  table <- data.frame(
    Node   = criteria[order_by_weight],
    Weight = round(as.numeric(fit$weights)[order_by_weight], 4),
    Rank   = seq_along(criteria),
    stringsAsFactors = FALSE
  )
  best <- criteria[which.max(fit$weights)]

  list(
    test          = "anp",
    apa           = sprintf(
      paste0("ANP resolved the limiting priorities of a %d-node comparison ",
             "network after %d power-iteration step(s). '%s' carried the ",
             "highest limiting weight (%.3f)."),
      length(criteria), fit$iterations, best, max(fit$weights)
    ),
    prompt        = paste0(
      "Report the full limiting-priority vector, not the top node alone, ",
      "and note that ANP weights reflect feedback between nodes rather ",
      "than a one-way hierarchy, so a small change anywhere in the network ",
      "can move every weight."
    ),
    table         = table,
    score_label   = "Limiting priority weight",
    weights       = fit$weights,
    criteria      = criteria,
    matrix_source = resolved$matrix_source,
    iterations    = fit$iterations,
    converged     = fit$converged,
    notes         = resolved$notes[!vapply(resolved$notes, is.null, logical(1))]
  )
}
