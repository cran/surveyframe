# R/decision_preference.R
# PROMETHEE II and ELECTRE I: the two outranking methods in the decision
# family. Both share the input-resolution and validation helpers in
# R/decision_methods.R (sframe_resolve_decision_inputs(),
# sframe_check_decision_input()) but need method-specific options
# (preference-function type and thresholds for PROMETHEE, concordance and
# discordance thresholds for ELECTRE) that the shared resolver does not know
# about, so those are read out of `resolved$options` directly inside each
# runner.

# ---------------------------------------------------------------------------
# PROMETHEE II computation
# ---------------------------------------------------------------------------

# Per-criterion preference degree P_j(a, b) in [0, 1], one of Brans' six
# standard preference functions. Only the three the harvested source
# implemented are ported (usual, linear/V-shape, level); "usual" (type I,
# the step function with no thresholds) is the default per todo_0.5.md
# section 3, not "linear" as the harvested mcdm source defaulted to (that
# source also derived its thresholds from the data range, a hidden
# researcher degree of freedom the harvest audit flagged as a defect to
# drop; here a non-default preference function requires either
# `options$thresholds` or the same range-derived fallback, but only ever
# when a threshold-bearing function is explicitly requested).
.sframe_promethee_preference <- function(diff, fn, p = NULL, q = NULL) {
  if (fn == "usual") {
    return(ifelse(diff > 0, 1, 0))
  }
  if (fn == "linear") {
    out <- ifelse(diff <= 0, 0, ifelse(diff >= p, 1, diff / p))
    return(out)
  }
  if (fn == "level") {
    out <- ifelse(diff <= q, 0, ifelse(diff >= p, 1, 0.5))
    return(out)
  }
  stop(sprintf("Unknown PROMETHEE preference function '%s'.", fn),
       call. = FALSE)
}

# Preference-degree array, weighted global preference index, leaving
# (positive) and entering (negative) flows, and the PROMETHEE II net flow.
# Net flow is `positive_flow - negative_flow` by construction, so it is
# never computed independently; a test fixture below re-derives it from the
# two flows to check that identity holds for a real matrix, not just
# algebraically.
sframe_promethee_compute <- function(x, weights, criteria_types,
                                     preference_function = "usual",
                                     thresholds = NULL) {
  n <- nrow(x)
  p <- ncol(x)
  benefit <- criteria_types == "benefit"

  if (is.null(thresholds) && preference_function != "usual") {
    thresholds <- lapply(seq_len(p), function(j) {
      range_j <- max(x[, j]) - min(x[, j])
      list(preference = range_j * 0.3, indifference = range_j * 0.1)
    })
  }

  preference <- array(0, dim = c(n, n, p))
  for (j in seq_len(p)) {
    col <- x[, j]
    diffs <- outer(col, col, function(a, b) a - b)
    if (!benefit[j]) diffs <- -diffs
    thr <- if (!is.null(thresholds)) thresholds[[j]] else NULL
    deg <- .sframe_promethee_preference(
      diffs, preference_function,
      p = thr[["preference"]], q = thr[["indifference"]]
    )
    diag(deg) <- 0
    preference[, , j] <- deg
  }

  global <- matrix(0, n, n, dimnames = list(rownames(x), rownames(x)))
  for (j in seq_len(p)) global <- global + weights[j] * preference[, , j]
  diag(global) <- 0

  positive_flow <- rowSums(global) / (n - 1)
  negative_flow <- colSums(global) / (n - 1)
  net_flow <- positive_flow - negative_flow
  names(net_flow) <- names(positive_flow) <- names(negative_flow) <-
    rownames(x)

  list(
    scores               = net_flow,
    ranks                = rank(-net_flow, ties.method = "min"),
    positive_flow        = positive_flow,
    negative_flow        = negative_flow,
    net_flow             = net_flow,
    preference_array     = preference,
    global_preference    = global,
    preference_function  = preference_function,
    thresholds           = thresholds
  )
}

# ---------------------------------------------------------------------------
# ELECTRE I computation
# ---------------------------------------------------------------------------

# Classical ELECTRE I (Roy, 1968), not the pseudo-criteria indifference/
# preference/veto variant the harvested mcdm source actually implemented
# under that name (that source's own veto check was a no-op loop, and the
# file is internally labelled "ELECTRE III"; the harvest audit calls for a
# rewrite against the published ELECTRE I definition rather than a port).
# Concordance index C(a,b): the share of total weight on criteria where a is
# at least as good as b. Discordance index D(a,b): the largest weighted
# disagreement, on the raw scale range of the criterion where b beats a,
# normalised to 0 when a is at least as good as b everywhere. a outranks b
# when C(a,b) exceeds the concordance threshold and D(a,b) is below the
# discordance threshold.
sframe_electre_compute <- function(x, weights, criteria_types,
                                   concordance_threshold = 0.7,
                                   discordance_threshold = 0.3) {
  n <- nrow(x)
  p <- ncol(x)
  benefit <- criteria_types == "benefit"
  alt_names <- rownames(x) %||% paste0("A", seq_len(n))

  ranges <- apply(x, 2, function(col) max(col) - min(col))
  ranges[ranges == 0] <- 1

  concordance <- matrix(0, n, n, dimnames = list(alt_names, alt_names))
  discordance <- matrix(0, n, n, dimnames = list(alt_names, alt_names))

  for (i in seq_len(n)) {
    for (k in seq_len(n)) {
      if (i == k) next
      at_least_as_good <- logical(p)
      disagreement <- numeric(p)
      for (j in seq_len(p)) {
        if (benefit[j]) {
          at_least_as_good[j] <- x[i, j] >= x[k, j]
          disagreement[j] <- max(0, x[k, j] - x[i, j]) / ranges[j]
        } else {
          at_least_as_good[j] <- x[i, j] <= x[k, j]
          disagreement[j] <- max(0, x[i, j] - x[k, j]) / ranges[j]
        }
      }
      concordance[i, k] <- sum(weights[at_least_as_good])
      discordance[i, k] <- max(disagreement)
    }
  }

  outranking <- matrix(FALSE, n, n, dimnames = list(alt_names, alt_names))
  outranking[!diag(n) & concordance >= concordance_threshold &
               discordance <= discordance_threshold] <- TRUE
  diag(outranking) <- FALSE

  outrank_count <- rowSums(outranking)
  outranked_count <- colSums(outranking)
  # Best-effort score for the general Score/Rank reporting pattern: net
  # outranking count, matching the harvested source's own convention. This
  # is an approximation, not a total ranking: ELECTRE I's real output is
  # the outranking relation and the kernel below, and two alternatives with
  # the same net count can be genuinely incomparable rather than tied.
  scores <- outrank_count - outranked_count
  names(scores) <- alt_names

  # Kernel: alternatives no other alternative outranks (no incoming edge in
  # the outranking graph). This is a simplified, commonly used reading of
  # ELECTRE I's kernel (a dominant, stable subset); it does not implement
  # graph-cycle resolution for the general non-acyclic case.
  kernel <- outranked_count == 0
  names(kernel) <- alt_names

  list(
    scores                 = scores,
    ranks                  = rank(-scores, ties.method = "min"),
    concordance            = concordance,
    discordance            = discordance,
    outranking             = outranking,
    outrank_count          = outrank_count,
    outranked_count        = outranked_count,
    kernel                 = kernel,
    concordance_threshold  = concordance_threshold,
    discordance_threshold  = discordance_threshold
  )
}

# ---------------------------------------------------------------------------
# PROMETHEE II runner
# ---------------------------------------------------------------------------

sframe_run_promethee <- function(data, roles, options, instrument) {
  resolved <- sframe_resolve_decision_inputs(data, roles, options, instrument,
                                             method = "PROMETHEE II")
  if (!is.null(resolved$error)) {
    return(list(test = "promethee", error = resolved$error))
  }
  checked <- sframe_check_decision_input(resolved$matrix, resolved$weights,
                                         resolved$criteria_types)
  if (!is.null(checked$error)) {
    return(list(test = "promethee", error = checked$error))
  }

  preference_function <- resolved$options[["preference_function"]] %||% "usual"
  thresholds <- resolved$options[["thresholds"]]

  fit <- sframe_promethee_compute(checked$matrix, checked$weights,
                                  checked$criteria_types,
                                  preference_function = preference_function,
                                  thresholds = thresholds)

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
    test           = "promethee",
    apa            = sprintf(
      paste0("PROMETHEE II ranked %d alternatives on %d criteria using the ",
             "%s preference function. %s ranked first with a net flow of ",
             "%.3f."),
      length(alternatives), ncol(checked$matrix), preference_function, best,
      max(fit$scores)
    ),
    prompt         = paste0(
      "Report the full ranking, not the winner alone. Net flow near zero ",
      "for several alternatives means the outranking relation barely ",
      "separates them, so state where the weights and any preference ",
      "thresholds came from and check how far the ranking moves when they ",
      "are perturbed."
    ),
    table          = table,
    score_label    = "Net flow",
    scores         = fit$scores,
    ranks          = fit$ranks,
    alternatives   = alternatives,
    criteria       = resolved$criteria,
    criteria_types = checked$criteria_types,
    weights        = checked$weights,
    weights_source = resolved$weights_source,
    matrix_source  = resolved$matrix_source,
    positive_flow  = fit$positive_flow,
    negative_flow  = fit$negative_flow,
    preference_function = fit$preference_function,
    thresholds     = fit$thresholds,
    consistency    = resolved$collected_weights$consistency,
    notes          = notes[!vapply(notes, is.null, logical(1))]
  )
}

# ---------------------------------------------------------------------------
# ELECTRE I runner
# ---------------------------------------------------------------------------

sframe_run_electre <- function(data, roles, options, instrument) {
  resolved <- sframe_resolve_decision_inputs(data, roles, options, instrument,
                                             method = "ELECTRE I")
  if (!is.null(resolved$error)) {
    return(list(test = "electre", error = resolved$error))
  }
  checked <- sframe_check_decision_input(resolved$matrix, resolved$weights,
                                         resolved$criteria_types)
  if (!is.null(checked$error)) {
    return(list(test = "electre", error = checked$error))
  }

  # Standard textbook ELECTRE I thresholds. Neither of the two reference
  # sources supplies a directly reusable default: the harvested mcdm source
  # uses a bare concordance threshold of 0.75 alongside a discordance/veto
  # check the harvest audit flagged as a no-op bug, and RMCDA's
  # apply.ELECTRE1() derives both thresholds from the data itself (their
  # mean concordance/discordance), which is a hidden researcher degree of
  # freedom of the same kind the audit already ruled out for PROMETHEE.
  # 0.7 / 0.3 is the commonly taught textbook convention for the
  # concordance/discordance cutoff pair and is used here as an explicit,
  # overridable default.
  concordance_threshold <- resolved$options[["concordance_threshold"]] %||% 0.7
  discordance_threshold <- resolved$options[["discordance_threshold"]] %||% 0.3

  fit <- sframe_electre_compute(checked$matrix, checked$weights,
                                checked$criteria_types,
                                concordance_threshold = concordance_threshold,
                                discordance_threshold = discordance_threshold)

  alternatives <- resolved$alternatives
  order_by_rank <- order(fit$ranks)
  table <- data.frame(
    Alternative = alternatives[order_by_rank],
    Outranks    = as.integer(fit$outrank_count)[order_by_rank],
    OutrankedBy = as.integer(fit$outranked_count)[order_by_rank],
    Kernel      = ifelse(fit$kernel[order_by_rank], "Yes", "No"),
    Score       = as.integer(fit$scores)[order_by_rank],
    Rank        = as.integer(fit$ranks)[order_by_rank],
    stringsAsFactors = FALSE
  )
  kernel_members <- alternatives[fit$kernel]
  notes <- c(resolved$notes, checked$note)

  # An empty outranking relation is a non-result, not a tie for first place.
  # It happens when no alternative clears the concordance threshold against
  # any other, which gets more likely as the criterion count rises: on a 9
  # by 9 problem at the default 0.70 and 0.30 every score comes back 0 and
  # every alternative ranks 1. Left unsaid, the table reads as "all 9
  # alternatives are jointly best" and the APA sentence reports a kernel
  # containing everything, both of which sound like findings.
  if (all(fit$outrank_count == 0) && all(fit$outranked_count == 0)) {
    notes <- c(notes, paste0(
      "No alternative outranked any other at concordance ",
      sprintf("%.2f", concordance_threshold), " and discordance ",
      sprintf("%.2f", discordance_threshold),
      ", so ELECTRE I did not separate these alternatives at all. The equal ",
      "ranks below are an absence of evidence rather than a tie for first ",
      "place. Report it as a non-result, or re-run with thresholds declared ",
      "for this problem: a concordance cutoff that suits 4 criteria is often ",
      "unreachable with 9."
    ))
  }

  list(
    test           = "electre",
    apa            = sprintf(
      paste0("ELECTRE I built the outranking relation for %d alternatives ",
             "on %d criteria (concordance threshold %.2f, discordance ",
             "threshold %.2f). The kernel (non-dominated) set contains %d ",
             "alternative(s): %s."),
      length(alternatives), ncol(checked$matrix), concordance_threshold,
      discordance_threshold, length(kernel_members),
      paste(kernel_members, collapse = ", ")
    ),
    prompt         = paste0(
      "ELECTRE I does not always produce a strict total ranking: two ",
      "alternatives can be genuinely incomparable rather than tied, so ",
      "report the outranking relation and the kernel set, not the Rank ",
      "column alone. Check how the kernel changes when the concordance ",
      "and discordance thresholds are perturbed, and state where the ",
      "weights came from."
    ),
    table          = table,
    score_label    = "Net outranking count",
    scores         = fit$scores,
    ranks          = fit$ranks,
    alternatives   = alternatives,
    criteria       = resolved$criteria,
    criteria_types = checked$criteria_types,
    weights        = checked$weights,
    weights_source = resolved$weights_source,
    matrix_source  = resolved$matrix_source,
    concordance    = fit$concordance,
    discordance    = fit$discordance,
    outranking     = fit$outranking,
    kernel         = fit$kernel,
    concordance_threshold = concordance_threshold,
    discordance_threshold = discordance_threshold,
    consistency    = resolved$collected_weights$consistency,
    notes          = notes[!vapply(notes, is.null, logical(1))]
  )
}
