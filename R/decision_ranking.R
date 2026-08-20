# R/decision_ranking.R
# Four MCDM ranking methods added for v0.5: VIKOR, MOORA, SMART, WASPAS. Each
# is a computation helper (matrix, weights, criteria_types) plus a runner in
# the same shape as sframe_run_topsis() in R/decision_methods.R, reusing that
# file's sframe_check_decision_input() and sframe_resolve_decision_inputs()
# rather than reimplementing input handling. `sframe_decision_methods` (also
# in decision_methods.R) still needs "vikor", "moora", "smart", "waspas"
# added to its registry; that is left to the consolidation pass.

# ---------------------------------------------------------------------------
# Shared min-max normalisation, used by SMART and WASPAS
# ---------------------------------------------------------------------------

# Linear scale (min-max) normalisation into [0, 1], direction handled per
# column so a cost criterion is inverted once at the normalisation step
# rather than again downstream (the same principle as the vector-norm split
# in sframe_topsis_compute()). A constant column normalises to 1 for every
# alternative, since there is nothing to discriminate on either direction.
sframe_minmax_normalise <- function(x, criteria_types) {
  benefit <- criteria_types == "benefit"
  out <- x
  for (j in seq_len(ncol(x))) {
    col <- x[, j]
    mn <- min(col)
    mx <- max(col)
    if (mx == mn) {
      out[, j] <- 1
    } else if (benefit[j]) {
      out[, j] <- (col - mn) / (mx - mn)
    } else {
      out[, j] <- (mx - col) / (mx - mn)
    }
  }
  out
}

# ---------------------------------------------------------------------------
# VIKOR computation
# ---------------------------------------------------------------------------

# Opricovic & Tzeng's compromise ranking. S is the group-utility measure
# (weighted sum of normalised regret from the best value on each criterion),
# R is the individual-regret measure (the worst of those per-criterion
# regrets), and Q blends the two via v (default 0.5, equal weight on group
# utility and individual regret). Ranking is on Q ascending (lower is
# better), so `scores` is returned as 1 - Q for consistency with every other
# ranking method in the family, where a higher score is always better.
sframe_vikor_compute <- function(x, weights, criteria_types, v = 0.5) {
  benefit <- criteria_types == "benefit"
  f_best  <- ifelse(benefit, apply(x, 2, max), apply(x, 2, min))
  f_worst <- ifelse(benefit, apply(x, 2, min), apply(x, 2, max))
  spread  <- f_best - f_worst

  utility <- matrix(0, nrow(x), ncol(x), dimnames = dimnames(x))
  for (j in seq_len(ncol(x))) {
    if (spread[j] != 0) {
      utility[, j] <- weights[j] * (f_best[j] - x[, j]) / spread[j]
    }
  }

  S <- rowSums(utility)
  R <- apply(utility, 1, max)
  names(S) <- rownames(x)
  names(R) <- rownames(x)

  S_best <- min(S); S_worst <- max(S)
  R_best <- min(R); R_worst <- max(R)
  S_spread <- S_worst - S_best
  R_spread <- R_worst - R_best

  S_term <- if (S_spread > 0) (S - S_best) / S_spread else rep(0, length(S))
  R_term <- if (R_spread > 0) (R - R_best) / R_spread else rep(0, length(R))
  Q <- v * S_term + (1 - v) * R_term
  names(Q) <- rownames(x)

  ranks  <- rank(Q, ties.method = "min")
  scores <- 1 - Q
  names(scores) <- rownames(x)

  n <- nrow(x)
  order_q    <- order(Q)
  best_idx   <- order_q[1]
  second_idx <- order_q[2]
  dq <- 1 / (n - 1)
  acceptable_advantage <- unname((Q[second_idx] - Q[best_idx]) >= dq)
  best_alt <- rownames(x)[best_idx]
  acceptable_stability <- best_alt %in% rownames(x)[which(S == min(S))] ||
    best_alt %in% rownames(x)[which(R == min(R))]

  list(
    scores = scores, ranks = ranks, Q = Q, S = S, R = R,
    f_best = f_best, f_worst = f_worst, v = v, dq = dq,
    acceptable_advantage = acceptable_advantage,
    acceptable_stability  = acceptable_stability
  )
}

# ---------------------------------------------------------------------------
# MOORA computation
# ---------------------------------------------------------------------------

# Brauers & Zavadskas's ratio system: vector-normalise, weight, then subtract
# the summed cost-criteria contribution from the summed benefit-criteria
# contribution. The reference-point variant (deviation from the best
# per-criterion weighted value, lower is better) is computed alongside it as
# a cross-check, per the spec's "ratio system plus reference-point variant".
sframe_moora_compute <- function(x, weights, criteria_types) {
  benefit <- criteria_types == "benefit"
  norms <- sqrt(colSums(x^2))
  norms[norms == 0] <- 1
  normalised <- sweep(x, 2, norms, "/")
  weighted   <- sweep(normalised, 2, weights, "*")

  beneficial_sum <- if (any(benefit)) {
    rowSums(weighted[, benefit, drop = FALSE])
  } else {
    rep(0, nrow(x))
  }
  cost_sum <- if (any(!benefit)) {
    rowSums(weighted[, !benefit, drop = FALSE])
  } else {
    rep(0, nrow(x))
  }
  scores <- beneficial_sum - cost_sum
  names(scores) <- rownames(x)
  ranks <- rank(-scores, ties.method = "min")

  reference_point <- ifelse(benefit, apply(weighted, 2, max),
                            apply(weighted, 2, min))
  deviations <- abs(sweep(weighted, 2, reference_point, "-"))
  reference_scores <- apply(deviations, 1, max)
  names(reference_scores) <- rownames(x)
  reference_ranks <- rank(reference_scores, ties.method = "min")

  list(
    scores = scores, ranks = ranks,
    beneficial_sum = beneficial_sum, cost_sum = cost_sum,
    weighted = weighted,
    reference_point   = reference_point,
    reference_scores  = reference_scores,
    reference_ranks   = reference_ranks
  )
}

# ---------------------------------------------------------------------------
# SMART computation
# ---------------------------------------------------------------------------

# Edwards's simple multi-attribute rating technique: min-max normalise each
# criterion into [0, 1] with direction handled at normalisation, weight, and
# sum. The weighted-value model with no separate aggregation step.
sframe_smart_compute <- function(x, weights, criteria_types) {
  normalised <- sframe_minmax_normalise(x, criteria_types)
  utility <- sweep(normalised, 2, weights, "*")
  scores <- rowSums(utility)
  names(scores) <- rownames(x)
  ranks <- rank(-scores, ties.method = "min")
  list(scores = scores, ranks = ranks, normalised = normalised,
       utility = utility)
}

# ---------------------------------------------------------------------------
# WASPAS computation
# ---------------------------------------------------------------------------

# WASPAS's own normalisation (Zavadskas et al. 2012), not the min-max scale
# used for SMART: a benefit column divides by its own maximum, a cost column
# divides its minimum by each value. This is the ratio form the published
# method and RMCDA's apply.WASPAS() both use; min-max normalisation (tried
# first, following the harvested mcdm source) gave a different, non-standard
# answer that did not reproduce RMCDA's oracle values, so it was replaced
# with this normaliser for WASPAS specifically.
sframe_waspas_normalise <- function(x, criteria_types) {
  benefit <- criteria_types == "benefit"
  out <- x
  for (j in seq_len(ncol(x))) {
    col <- x[, j]
    if (benefit[j]) {
      mx <- max(col)
      out[, j] <- if (mx != 0) col / mx else rep(1, length(col))
    } else {
      mn <- min(col)
      out[, j] <- ifelse(col != 0, mn / col, 1)
    }
  }
  out
}

# Zavadskas et al.'s blend of the weighted sum model (WSM) and weighted
# product model (WPM) on the same ratio-normalised matrix, combined via
# lambda (default 0.5, an equal blend). The product is computed as
# exp(sum(w * log(x))) rather than a running product, which is equivalent
# but avoids per-row loops; a zero cell is nudged to a small constant first
# so its log is finite, matching the harvested reference's epsilon guard.
sframe_waspas_compute <- function(x, weights, criteria_types, lambda = 0.5) {
  normalised <- sframe_waspas_normalise(x, criteria_types)

  wsm <- rowSums(sweep(normalised, 2, weights, "*"))

  positive <- normalised
  positive[positive == 0] <- 1e-10
  wpm <- exp(rowSums(sweep(log(positive), 2, weights, "*")))

  scores <- lambda * wsm + (1 - lambda) * wpm
  names(scores) <- rownames(x)
  ranks <- rank(-scores, ties.method = "min")

  list(scores = scores, ranks = ranks, wsm_scores = wsm, wpm_scores = wpm,
       normalised = normalised, lambda = lambda)
}

# ---------------------------------------------------------------------------
# VIKOR runner
# ---------------------------------------------------------------------------

sframe_run_vikor <- function(data, roles, options, instrument, v = 0.5) {
  resolved <- sframe_resolve_decision_inputs(data, roles, options, instrument,
                                             method = "VIKOR")
  if (!is.null(resolved$error)) {
    return(list(test = "vikor", error = resolved$error))
  }
  checked <- sframe_check_decision_input(resolved$matrix, resolved$weights,
                                         resolved$criteria_types)
  if (!is.null(checked$error)) {
    return(list(test = "vikor", error = checked$error))
  }
  v <- resolved$options[["v"]] %||% v
  fit <- sframe_vikor_compute(checked$matrix, checked$weights,
                              checked$criteria_types, v = v)

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
  if (!fit$acceptable_advantage) {
    notes <- c(notes, paste0(
      "The acceptable-advantage condition failed: the gap between the top ",
      "two VIKOR indices is smaller than 1/(n - 1), so treat the leading ",
      "alternatives as a compromise set rather than a single winner."
    ))
  }
  if (!fit$acceptable_stability) {
    notes <- c(notes, paste0(
      "The acceptable-stability condition failed: the top-ranked ",
      "alternative is not also best on S or R alone, so the compromise ",
      "ranking is sensitive to the choice of v."
    ))
  }

  list(
    test           = "vikor",
    apa            = sprintf(
      paste0("VIKOR ranked %d alternatives on %d criteria (v = %.2f). %s ",
             "ranked first with a VIKOR index of %.3f.%s"),
      length(alternatives), ncol(checked$matrix), v, best,
      fit$Q[[best]],
      if (fit$acceptable_advantage && fit$acceptable_stability) "" else {
        " The ranking did not meet both VIKOR acceptance conditions."
      }
    ),
    prompt         = paste0(
      "VIKOR proposes a compromise ranking rather than a guaranteed single ",
      "winner. Report both acceptance conditions (acceptable advantage and ",
      "acceptable stability) alongside the ranking, state the v used and why ",
      "(v = 0.5 weights group utility and individual regret equally), and ",
      "check how far the top of the ranking moves under a different v."
    ),
    table          = table,
    score_label    = "Compromise score (1 - Q)",
    scores         = fit$scores,
    ranks          = fit$ranks,
    alternatives   = alternatives,
    criteria       = resolved$criteria,
    criteria_types = checked$criteria_types,
    weights        = checked$weights,
    weights_source = resolved$weights_source,
    matrix_source  = resolved$matrix_source,
    Q                     = fit$Q,
    S                     = fit$S,
    R                     = fit$R,
    v                     = v,
    acceptable_advantage  = fit$acceptable_advantage,
    acceptable_stability  = fit$acceptable_stability,
    consistency    = resolved$collected_weights$consistency,
    notes          = notes[!vapply(notes, is.null, logical(1))]
  )
}

# ---------------------------------------------------------------------------
# MOORA runner
# ---------------------------------------------------------------------------

sframe_run_moora <- function(data, roles, options, instrument) {
  resolved <- sframe_resolve_decision_inputs(data, roles, options, instrument,
                                             method = "MOORA")
  if (!is.null(resolved$error)) {
    return(list(test = "moora", error = resolved$error))
  }
  checked <- sframe_check_decision_input(resolved$matrix, resolved$weights,
                                         resolved$criteria_types)
  if (!is.null(checked$error)) {
    return(list(test = "moora", error = checked$error))
  }
  fit <- sframe_moora_compute(checked$matrix, checked$weights,
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
    test           = "moora",
    apa            = sprintf(
      paste0("MOORA (ratio system) ranked %d alternatives on %d criteria. ",
             "%s ranked first with a net ratio score of %.3f."),
      length(alternatives), ncol(checked$matrix), best, max(fit$scores)
    ),
    prompt         = paste0(
      "Report the full ranking alongside the reference-point variant's ",
      "ranking; the two agreeing is a useful robustness check on the ratio ",
      "system, and a disagreement is worth naming explicitly rather than ",
      "picking whichever ranking favours the expected answer."
    ),
    table          = table,
    score_label    = "Ratio-system score",
    scores         = fit$scores,
    ranks          = fit$ranks,
    alternatives   = alternatives,
    criteria       = resolved$criteria,
    criteria_types = checked$criteria_types,
    weights        = checked$weights,
    weights_source = resolved$weights_source,
    matrix_source  = resolved$matrix_source,
    reference_point  = fit$reference_point,
    reference_scores = fit$reference_scores,
    reference_ranks  = fit$reference_ranks,
    consistency    = resolved$collected_weights$consistency,
    notes          = notes[!vapply(notes, is.null, logical(1))]
  )
}

# ---------------------------------------------------------------------------
# SMART runner
# ---------------------------------------------------------------------------

sframe_run_smart <- function(data, roles, options, instrument) {
  resolved <- sframe_resolve_decision_inputs(data, roles, options, instrument,
                                             method = "SMART")
  if (!is.null(resolved$error)) {
    return(list(test = "smart", error = resolved$error))
  }
  checked <- sframe_check_decision_input(resolved$matrix, resolved$weights,
                                         resolved$criteria_types)
  if (!is.null(checked$error)) {
    return(list(test = "smart", error = checked$error))
  }
  fit <- sframe_smart_compute(checked$matrix, checked$weights,
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
    test           = "smart",
    apa            = sprintf(
      paste0("SMART ranked %d alternatives on %d criteria. %s ranked first ",
             "with a weighted utility value of %.3f."),
      length(alternatives), ncol(checked$matrix), best, max(fit$scores)
    ),
    prompt         = paste0(
      "SMART's weights are a direct judgement of importance rather than ",
      "derived from pairwise comparisons, so report where the weights came ",
      "from and treat the ranking as sensitive to that judgement, ",
      "especially where scores sit close together."
    ),
    table          = table,
    score_label    = "Weighted utility value",
    scores         = fit$scores,
    ranks          = fit$ranks,
    alternatives   = alternatives,
    criteria       = resolved$criteria,
    criteria_types = checked$criteria_types,
    weights        = checked$weights,
    weights_source = resolved$weights_source,
    matrix_source  = resolved$matrix_source,
    normalised_matrix = fit$normalised,
    consistency    = resolved$collected_weights$consistency,
    notes          = notes[!vapply(notes, is.null, logical(1))]
  )
}

# ---------------------------------------------------------------------------
# WASPAS runner
# ---------------------------------------------------------------------------

sframe_run_waspas <- function(data, roles, options, instrument, lambda = 0.5) {
  resolved <- sframe_resolve_decision_inputs(data, roles, options, instrument,
                                             method = "WASPAS")
  if (!is.null(resolved$error)) {
    return(list(test = "waspas", error = resolved$error))
  }
  checked <- sframe_check_decision_input(resolved$matrix, resolved$weights,
                                         resolved$criteria_types)
  if (!is.null(checked$error)) {
    return(list(test = "waspas", error = checked$error))
  }
  lambda <- resolved$options[["lambda"]] %||% lambda
  fit <- sframe_waspas_compute(checked$matrix, checked$weights,
                               checked$criteria_types, lambda = lambda)

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
    test           = "waspas",
    apa            = sprintf(
      paste0("WASPAS ranked %d alternatives on %d criteria (lambda = %.2f, ",
             "blending the weighted sum and weighted product models). %s ",
             "ranked first with a joint measure of %.3f."),
      length(alternatives), ncol(checked$matrix), lambda, best,
      max(fit$scores)
    ),
    prompt         = paste0(
      "Report lambda alongside the ranking: lambda = 1 is a pure weighted ",
      "sum, lambda = 0 a pure weighted product, and the two components can ",
      "disagree on the top alternative even when the blended score does ",
      "not move much, so check the WSM and WPM components separately when ",
      "the ranking is close."
    ),
    table          = table,
    score_label    = "WASPAS joint measure",
    scores         = fit$scores,
    ranks          = fit$ranks,
    alternatives   = alternatives,
    criteria       = resolved$criteria,
    criteria_types = checked$criteria_types,
    weights        = checked$weights,
    weights_source = resolved$weights_source,
    matrix_source  = resolved$matrix_source,
    wsm_scores     = fit$wsm_scores,
    wpm_scores     = fit$wpm_scores,
    lambda         = lambda,
    consistency    = resolved$collected_weights$consistency,
    notes          = notes[!vapply(notes, is.null, logical(1))]
  )
}
