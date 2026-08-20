# R/decision_sensitivity.R
# Weight-sensitivity analysis for the 7 ranking methods in the decision
# family. A ranking is only as trustworthy as the weights that produced it,
# and collected weights carry sampling error, so the question a reviewer asks
# is how far the ranking moves when a weight is nudged. This perturbs one
# criterion weight at a time, renormalises, reruns the same method, and
# reports how much the ranking shifted.

# The 7 ranking methods share a compute(x, weights, criteria_types, ...)
# signature and all return $ranks, so one driver covers them all. AHP, ANP,
# and DEMATEL are absent deliberately: they produce weights or influence
# structure rather than consuming a weight vector to rank alternatives, so
# perturbing a weight has nothing to act on.
.sframe_sensitivity_methods <- c("topsis", "vikor", "moora", "smart",
                                 "waspas", "promethee", "electre")

.sframe_sensitivity_compute <- function(method) {
  switch(
    method,
    topsis    = sframe_topsis_compute,
    vikor     = sframe_vikor_compute,
    moora     = sframe_moora_compute,
    smart     = sframe_smart_compute,
    waspas    = sframe_waspas_compute,
    promethee = sframe_promethee_compute,
    electre   = sframe_electre_compute,
    sframe_abort_validation(sprintf(
      paste0("Sensitivity analysis does not cover '%s'. It applies to the ",
             "ranking methods: %s."),
      method, paste(.sframe_sensitivity_methods, collapse = ", ")
    ))
  )
}

#' Test how far a decision ranking moves when the weights are perturbed
#'
#' Perturbs each criterion weight up and down by `delta`, renormalises the
#' weight vector to sum to 1, reruns the same ranking method, and compares
#' the perturbed ranking against the base ranking. A ranking that survives
#' this unchanged is one a reviewer can be told is robust to the weights. A
#' ranking whose leader changes under a 5 percent nudge is not.
#'
#' @param x Numeric performance matrix, alternatives in rows and criteria in
#'   columns.
#' @param weights Numeric weight vector, one per criterion. Renormalised to
#'   sum to 1 before use.
#' @param criteria_types Character vector of `"benefit"` or `"cost"`, one per
#'   criterion.
#' @param method Ranking method. One of `"topsis"`, `"vikor"`, `"moora"`,
#'   `"smart"`, `"waspas"`, `"promethee"`, or `"electre"`.
#' @param delta Perturbation size as a proportion of the weight, default
#'   `0.05`. A weight of 0.40 with `delta = 0.05` is tested at 0.42 and 0.38
#'   before renormalisation.
#' @param alternatives Optional labels for the rows of `x`.
#' @param criteria Optional labels for the columns of `x`.
#' @param ... Passed to the underlying method, for example `v` for VIKOR or
#'   `lambda` for WASPAS.
#'
#' @return An object of class `sframe_sensitivity`, a list with `$table` (one
#'   row per criterion and direction, carrying `criterion`, `direction`,
#'   `rho`, `rank_changed`, and `top_changed`), `$base_ranks`, `$method`,
#'   `$delta`, and `$stable`, a single logical that is `TRUE` when no
#'   perturbation changed the ranking.
#' @export
#' @seealso [run_analysis_plan()], [sframe_decision_options()]
#' @examples
#' x <- matrix(c(4.1, 3.0, 210, 3.6, 4.5, 180, 4.8, 2.5, 260),
#'             nrow = 3, byrow = TRUE)
#' sa <- sensitivity_analysis(
#'   x,
#'   weights        = c(0.4, 0.3, 0.3),
#'   criteria_types = c("benefit", "benefit", "cost"),
#'   method         = "topsis",
#'   alternatives   = c("Alpha", "Basilica", "Coral"),
#'   criteria       = c("service", "location", "price")
#' )
#' sa$stable
#' as.data.frame(sa)
sensitivity_analysis <- function(x,
                                 weights,
                                 criteria_types,
                                 method = "topsis",
                                 delta = 0.05,
                                 alternatives = NULL,
                                 criteria = NULL,
                                 ...) {
  method <- tolower(as.character(method)[1])
  compute <- .sframe_sensitivity_compute(method)

  x <- as.matrix(x)
  if (!is.numeric(x)) {
    sframe_abort_validation("`x` must be a numeric performance matrix.")
  }
  weights <- as.numeric(weights)
  criteria_types <- as.character(criteria_types)

  n_criteria <- ncol(x)
  if (length(weights) != n_criteria) {
    sframe_abort_validation(sprintf(
      "`weights` has %d value(s) for %d criteria.", length(weights), n_criteria
    ))
  }
  if (length(criteria_types) != n_criteria) {
    sframe_abort_validation(sprintf(
      "`criteria_types` has %d value(s) for %d criteria.",
      length(criteria_types), n_criteria
    ))
  }
  if (!is.numeric(delta) || length(delta) != 1L || is.na(delta) ||
      delta <= 0 || delta >= 1) {
    sframe_abort_validation(
      "`delta` must be a single number greater than 0 and less than 1."
    )
  }
  if (any(!is.finite(weights)) || any(weights < 0) || sum(weights) <= 0) {
    sframe_abort_validation(
      "`weights` must be finite, non-negative, and not all zero."
    )
  }
  if (n_criteria < 2L) {
    sframe_abort_validation(
      paste0("Sensitivity analysis needs at least 2 criteria. With one ",
             "criterion the renormalised weight is always 1, so a ",
             "perturbation cannot change anything.")
    )
  }

  criteria <- criteria %||% colnames(x) %||%
    paste0("criterion_", seq_len(n_criteria))
  alternatives <- alternatives %||% rownames(x) %||%
    paste0("alternative_", seq_len(nrow(x)))

  weights <- weights / sum(weights)
  base <- compute(x, weights, criteria_types, ...)
  base_ranks <- as.integer(base$ranks)

  rows <- list()
  for (j in seq_len(n_criteria)) {
    for (dir in c("up", "down")) {
      w <- weights
      w[j] <- w[j] * if (identical(dir, "up")) (1 + delta) else (1 - delta)
      if (sum(w) <= 0) next
      w <- w / sum(w)

      perturbed <- tryCatch(compute(x, w, criteria_types, ...),
                            error = function(e) NULL)
      if (is.null(perturbed)) next
      new_ranks <- as.integer(perturbed$ranks)

      # A constant rank vector has zero variance, so cor() would warn and
      # return NA. Two identical constant vectors are perfectly concordant,
      # which is what rho = 1 means here.
      rho <- if (stats::sd(base_ranks) == 0 || stats::sd(new_ranks) == 0) {
        if (identical(base_ranks, new_ranks)) 1 else NA_real_
      } else {
        suppressWarnings(stats::cor(base_ranks, new_ranks, method = "spearman"))
      }

      rows[[length(rows) + 1L]] <- data.frame(
        criterion    = criteria[j],
        direction    = dir,
        weight       = round(w[j], 4),
        rho          = round(rho, 4),
        rank_changed = !identical(base_ranks, new_ranks),
        top_changed  = !identical(
          alternatives[which(base_ranks == min(base_ranks))],
          alternatives[which(new_ranks == min(new_ranks))]
        ),
        stringsAsFactors = FALSE
      )
    }
  }

  table <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    data.frame(criterion = character(0), direction = character(0),
               weight = numeric(0), rho = numeric(0),
               rank_changed = logical(0), top_changed = logical(0),
               stringsAsFactors = FALSE)
  }
  rownames(table) <- NULL

  # A base ranking that never separated the alternatives cannot be moved by
  # perturbing a weight, so nothing changes and `stable` comes out TRUE. That
  # is the strongest robustness signal this function can give, produced by
  # the weakest result it can be handed. ELECTRE I reaches it whenever no
  # alternative outranks any other, which a 9-criterion problem does at the
  # default thresholds. Flagged separately so a non-result is not read as a
  # robust one.
  degenerate <- length(unique(base_ranks)) <= 1L

  structure(
    list(
      table        = table,
      method       = method,
      delta        = delta,
      base_ranks   = stats::setNames(base_ranks, alternatives),
      alternatives = alternatives,
      criteria     = criteria,
      stable       = nrow(table) > 0 && !any(table$rank_changed),
      degenerate   = degenerate,
      n_changed    = sum(table$rank_changed),
      n_top_changed = sum(table$top_changed)
    ),
    class = "sframe_sensitivity"
  )
}

# Attach a sensitivity analysis to a decision result when the plan block asked
# for one with options$sensitivity = TRUE. Called once from
# sframe_run_one_block() rather than from each of the 7 ranking runners, the
# same way label substitution is handled there.
#
# The inputs are resolved a second time rather than threaded out of the
# runner. That costs one extra resolve on a block that opted in, and keeps
# all 7 runners untouched.
sframe_attach_sensitivity <- function(result, data, roles, options,
                                      instrument) {
  if (!isTRUE(options$sensitivity)) return(result)
  test <- result$test %||% ""
  if (!test %in% .sframe_sensitivity_methods) return(result)
  if (!is.null(result$error)) return(result)

  resolved <- tryCatch(
    sframe_resolve_decision_inputs(data, roles, options, instrument,
                                   method = toupper(test)),
    error = function(e) NULL
  )
  if (is.null(resolved) || !is.null(resolved$error)) return(result)

  checked <- tryCatch(
    sframe_check_decision_input(resolved$matrix, resolved$weights,
                                resolved$criteria_types),
    error = function(e) NULL
  )
  if (is.null(checked) || !is.null(checked$error)) return(result)

  # Method-specific tuning has to travel with the perturbation, or the
  # sensitivity run would silently rank under different settings from the
  # result it is testing.
  extra <- list()
  if (identical(test, "vikor") && !is.null(options$v)) {
    extra$v <- options$v
  }
  if (identical(test, "waspas") && !is.null(options$lambda)) {
    extra$lambda <- options$lambda
  }
  if (identical(test, "promethee")) {
    extra$preference_function <- options$preference_function %||% "usual"
    if (!is.null(options$thresholds)) extra$thresholds <- options$thresholds
  }
  if (identical(test, "electre") && !is.null(options$thresholds)) {
    extra$thresholds <- options$thresholds
  }

  sa <- tryCatch(
    do.call(sensitivity_analysis, c(
      list(
        x              = checked$matrix,
        weights        = checked$weights,
        criteria_types = checked$criteria_types,
        method         = test,
        delta          = options$sensitivity_delta %||% 0.05,
        alternatives   = resolved$alternatives,
        criteria       = resolved$criteria
      ),
      extra
    )),
    error = function(e) NULL
  )
  if (is.null(sa)) return(result)

  result$sensitivity <- sa
  result
}

#' @exportS3Method print sframe_sensitivity
print.sframe_sensitivity <- function(x, ...) {
  cat(sprintf("Weight sensitivity: %s, delta = %.0f%%\n\n",
              toupper(x$method), x$delta * 100))
  cat(sprintf("Base ranking: %s\n\n",
              paste(names(sort(x$base_ranks)), collapse = " > ")))
  print(x$table)
  cat("\n")
  if (isTRUE(x$degenerate)) {
    cat(paste0(
      "No result to test. The base ranking placed every alternative at the\n",
      "same rank, so nothing could move and every check passed vacuously.\n",
      "This is an absence of discrimination, not a robust ranking. Check the\n",
      "method's own output before reading anything into the table above.\n"
    ))
  } else if (x$stable) {
    cat(sprintf(
      paste0("Stable. No %.0f%% perturbation of any single weight changed ",
             "the ranking.\n"),
      x$delta * 100))
  } else {
    cat(sprintf(
      "Not stable. %d of %d perturbations changed the ranking",
      x$n_changed, nrow(x$table)))
    if (x$n_top_changed > 0) {
      cat(sprintf(", and %d changed which alternative ranked first",
                  x$n_top_changed))
    }
    cat(".\nReport this alongside the ranking rather than the ranking alone.\n")
  }
  invisible(x)
}
