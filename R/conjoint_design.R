# R/conjoint_design.R
# Declared choice-experiment (conjoint) designs.
#
# This is a design generator, not an estimator. It produces and stores the
# profile set and the task schedule as part of the instrument's contract, so
# the design a study ran is fixed and hashed before collection rather than
# reconstructed afterwards. Analysis of conjoint responses is deliberately out
# of scope here: there is no runner, and none is implied.
#
# Reproducibility is the point. The seed is always stored, generated if the
# caller did not supply one, so regenerating from the declared contract gives
# back the identical design.

.sframe_conjoint_methods <- c("full", "balanced", "random")

# Full factorial: every combination of every attribute's levels.
sframe_conjoint_full_factorial <- function(attributes) {
  grid <- expand.grid(attributes, stringsAsFactors = FALSE,
                      KEEP.OUT.ATTRS = FALSE)
  grid[order(do.call(order, unname(as.list(grid)))), , drop = FALSE]
}

# How far a candidate subset departs from an even spread of levels, plus how
# strongly its attributes move together. Lower is better on both counts.
# This is a transparent heuristic, not a catalogued orthogonal array, and the
# documentation says so.
sframe_conjoint_imbalance <- function(df) {
  level_penalty <- sum(vapply(df, function(col) {
    counts <- table(col)
    sum((counts - mean(counts))^2)
  }, numeric(1)))

  pair_penalty <- 0
  if (ncol(df) > 1L) {
    combos <- utils::combn(seq_len(ncol(df)), 2L)
    pair_penalty <- sum(apply(combos, 2L, function(ij) {
      tab <- table(df[[ij[1]]], df[[ij[2]]])
      expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
      sum((tab - expected)^2 / pmax(expected, 1e-9))
    }))
  }

  level_penalty + pair_penalty
}

sframe_conjoint_select <- function(full, n_profiles, method, tries = 200L) {
  n_full <- nrow(full)
  if (identical(method, "full") || n_profiles >= n_full) {
    return(full)
  }
  if (identical(method, "random")) {
    return(full[sort(sample.int(n_full, n_profiles)), , drop = FALSE])
  }

  # "balanced": sample repeatedly and keep the most even subset seen. Honest
  # about what it is, a search rather than a construction, so the achieved
  # balance travels on the object for the researcher to inspect.
  best <- NULL
  best_score <- Inf
  for (i in seq_len(tries)) {
    idx <- sort(sample.int(n_full, n_profiles))
    cand <- full[idx, , drop = FALSE]
    score <- sframe_conjoint_imbalance(cand)
    if (score < best_score) {
      best_score <- score
      best <- cand
    }
    if (best_score == 0) break
  }
  best
}

#' Declare a choice-experiment (conjoint) design
#'
#' Generates and stores a conjoint profile set and task schedule as part of
#' the instrument's pre-declared contract. This is a design generator, not an
#' estimator: it fixes what respondents will be shown, and it does not fit or
#' analyse choice models.
#'
#' The design is reproducible by construction. `seed` is always recorded, and
#' generated when not supplied, so regenerating from the stored declaration
#' returns the identical profiles and tasks.
#'
#' @section Choosing a method:
#' `"full"` enumerates every combination, which is exact but grows fast: 4
#' attributes at 3 levels each is 81 profiles. `"random"` takes a seeded
#' random subset of that size. `"balanced"` samples repeatedly and keeps the
#' subset with the most even level spread and the weakest association between
#' attributes.
#'
#' `"balanced"` is a search, not a construction. It does not produce a
#' catalogued orthogonal fractional factorial and does not claim the
#' guarantees of one. The achieved balance is reported in `$balance` so the
#' design can be inspected rather than trusted. A study needing a specific
#' D-optimal or orthogonal design should generate it elsewhere and pass it in
#' through `profiles`, which keeps the declaration in the contract either way.
#'
#' @param id Design identifier. Must start with a letter and contain only
#'   letters, numbers, and `_` characters.
#' @param attributes Named list of character vectors, one per attribute,
#'   giving that attribute's levels. At least 2 attributes, each with at
#'   least 2 levels.
#' @param method One of `"full"`, `"balanced"`, or `"random"`. Ignored when
#'   `profiles` is supplied.
#' @param n_profiles Number of profiles to keep. Required for `"balanced"`
#'   and `"random"`, ignored for `"full"`.
#' @param n_alternatives Alternatives shown per choice task, default 2.
#' @param n_tasks Choice tasks per block. Defaults to as many whole tasks as
#'   the profile set supports.
#' @param blocks Number of blocks the tasks are split across, default 1.
#' @param seed Integer seed. Generated and stored when not supplied.
#' @param profiles Optional data frame of pre-built profiles, one column per
#'   attribute. Supplying this bypasses generation and declares the design as
#'   given.
#' @param label Human-readable label.
#'
#' @return An object of class `sf_conjoint_design` with `$profiles`, `$tasks`
#'   (long format, one row per block, task, and alternative, ready for a
#'   choice model), `$balance`, and the declaration that produced them.
#' @export
#' @seealso [sf_instrument()]
#' @examples
#' design <- sf_conjoint_design(
#'   "hotel_dce",
#'   attributes = list(
#'     price    = c("50", "100", "150"),
#'     board    = c("room only", "breakfast"),
#'     distance = c("beachfront", "10 min walk")
#'   ),
#'   method = "balanced", n_profiles = 6, n_alternatives = 2, seed = 42
#' )
#' design$profiles
#' design$tasks
sf_conjoint_design <- function(id,
                               attributes,
                               method = c("full", "balanced", "random"),
                               n_profiles = NULL,
                               n_alternatives = 2L,
                               n_tasks = NULL,
                               blocks = 1L,
                               seed = NULL,
                               profiles = NULL,
                               label = NULL) {
  method <- match.arg(method)
  sframe_model_check_id(id, "id")

  if (!is.list(attributes) || is.null(names(attributes)) ||
      any(!nzchar(names(attributes)))) {
    sframe_abort_validation(
      "`attributes` must be a named list, one entry per attribute."
    )
  }
  if (length(attributes) < 2L) {
    sframe_abort_validation(
      "A conjoint design needs at least 2 attributes. With one attribute there is nothing to trade off."
    )
  }
  attributes <- lapply(attributes, as.character)
  short <- names(attributes)[vapply(attributes, length, integer(1)) < 2L]
  if (length(short) > 0) {
    sframe_abort_validation(sprintf(
      "Every attribute needs at least 2 levels. These have fewer: %s.",
      paste(short, collapse = ", ")
    ))
  }
  dup <- names(attributes)[vapply(attributes, anyDuplicated, integer(1)) > 0]
  if (length(dup) > 0) {
    sframe_abort_validation(sprintf(
      "These attributes have duplicated levels: %s.", paste(dup, collapse = ", ")
    ))
  }

  n_alternatives <- as.integer(n_alternatives)
  blocks <- as.integer(blocks)
  if (is.na(n_alternatives) || n_alternatives < 2L) {
    sframe_abort_validation(
      "`n_alternatives` must be at least 2. A choice task needs something to choose between."
    )
  }
  if (is.na(blocks) || blocks < 1L) {
    sframe_abort_validation("`blocks` must be at least 1.")
  }

  # Recorded whether supplied or not, so the design always regenerates.
  seed <- as.integer(seed %||% sample.int(.Machine$integer.max, 1L))
  old_seed <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else {
    NULL
  }
  on.exit({
    if (!is.null(old_seed)) {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)
  set.seed(seed)

  supplied <- !is.null(profiles)
  if (supplied) {
    profiles <- as.data.frame(profiles, stringsAsFactors = FALSE)
    missing_cols <- setdiff(names(attributes), names(profiles))
    if (length(missing_cols) > 0) {
      sframe_abort_validation(sprintf(
        "`profiles` is missing a column for these attributes: %s.",
        paste(missing_cols, collapse = ", ")
      ))
    }
    profiles <- profiles[, names(attributes), drop = FALSE]
    for (a in names(attributes)) {
      bad <- setdiff(unique(as.character(profiles[[a]])), attributes[[a]])
      if (length(bad) > 0) {
        sframe_abort_validation(sprintf(
          "`profiles` column '%s' contains levels not declared for that attribute: %s.",
          a, paste(bad, collapse = ", ")
        ))
      }
      profiles[[a]] <- as.character(profiles[[a]])
    }
  } else {
    full <- sframe_conjoint_full_factorial(attributes)
    if (!identical(method, "full")) {
      if (is.null(n_profiles)) {
        sframe_abort_validation(sprintf(
          "`n_profiles` is required when method is '%s'.", method
        ))
      }
      n_profiles <- as.integer(n_profiles)
      if (is.na(n_profiles) || n_profiles < n_alternatives) {
        sframe_abort_validation(sprintf(
          "`n_profiles` must be at least `n_alternatives` (%d).", n_alternatives
        ))
      }
      if (n_profiles > nrow(full)) {
        sframe_abort_validation(sprintf(
          "`n_profiles` is %d but the full factorial has only %d profiles.",
          n_profiles, nrow(full)
        ))
      }
    }
    profiles <- sframe_conjoint_select(full, n_profiles %||% nrow(full), method)
  }

  rownames(profiles) <- NULL
  profiles <- cbind(
    profile_id = paste0("p", seq_len(nrow(profiles))),
    profiles,
    stringsAsFactors = FALSE
  )

  # Task schedule. Profiles are shuffled once and dealt into tasks, so no
  # profile repeats within a task.
  per_block <- nrow(profiles) %/% n_alternatives
  n_tasks <- as.integer(n_tasks %||% per_block)
  if (is.na(n_tasks) || n_tasks < 1L) {
    sframe_abort_validation("`n_tasks` must be at least 1.")
  }
  if (n_tasks > per_block) {
    sframe_abort_validation(sprintf(
      "`n_tasks` is %d but %d profiles at %d alternatives per task supports at most %d whole task(s) per block.",
      n_tasks, nrow(profiles), n_alternatives, per_block
    ))
  }

  task_rows <- list()
  for (b in seq_len(blocks)) {
    dealt <- sample(profiles$profile_id)
    k <- 1L
    for (t in seq_len(n_tasks)) {
      for (a in seq_len(n_alternatives)) {
        task_rows[[length(task_rows) + 1L]] <- data.frame(
          block       = b,
          task        = t,
          alternative = a,
          profile_id  = dealt[k],
          stringsAsFactors = FALSE
        )
        k <- k + 1L
      }
    }
  }
  tasks <- do.call(rbind, task_rows)
  rownames(tasks) <- NULL

  balance <- list(
    level_counts = lapply(profiles[, names(attributes), drop = FALSE], function(col) {
      as.list(table(col))
    }),
    imbalance = round(
      sframe_conjoint_imbalance(profiles[, names(attributes), drop = FALSE]), 6
    ),
    n_full_factorial = prod(vapply(attributes, length, integer(1)))
  )

  structure(
    list(
      id             = id,
      label          = label %||% id,
      attributes     = attributes,
      method         = if (supplied) "supplied" else method,
      seed           = seed,
      blocks         = blocks,
      n_alternatives = n_alternatives,
      n_tasks        = n_tasks,
      profiles       = profiles,
      tasks          = tasks,
      balance        = balance
    ),
    class = "sf_conjoint_design"
  )
}

#' @exportS3Method print sf_conjoint_design
print.sf_conjoint_design <- function(x, ...) {
  cat(sprintf("Conjoint design: %s\n", x$label))
  cat(sprintf("  Method: %s (seed %d)\n", x$method, x$seed))
  cat(sprintf("  Attributes: %d, full factorial %d profiles\n",
              length(x$attributes), x$balance$n_full_factorial))
  cat(sprintf("  Profiles declared: %d\n", nrow(x$profiles)))
  cat(sprintf("  Tasks: %d block(s) x %d task(s) x %d alternative(s) = %d rows\n",
              x$blocks, x$n_tasks, x$n_alternatives, nrow(x$tasks)))
  if (!identical(x$method, "supplied")) {
    cat(sprintf("  Imbalance score: %.4f (0 is a perfectly even spread)\n",
                x$balance$imbalance))
  }
  cat("\nThis declares the design only. surveyframe does not fit choice models.\n")
  invisible(x)
}
