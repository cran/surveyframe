# R/decision_dematel.R
# DEMATEL (Decision Making Trial and Evaluation Laboratory): the computation
# helper, its runner, and a dedicated prominence-vs-relation plot. DEMATEL
# takes a square directed influence matrix over the criteria themselves (no
# alternatives, no weights) and classifies each criterion as a net cause or
# a net effect, so its shape does not fit `sframe_check_decision_input()` or
# `sframe_resolve_decision_inputs()` in R/decision_methods.R, both built for
# a rectangular alternatives x criteria matrix with weights. This file keeps
# its own small validator and resolver instead of force-fitting the TOPSIS
# shape onto it.

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

# A DEMATEL direct-influence matrix is square, has a zero diagonal (a
# criterion does not influence itself), and its off-diagonal entries are
# non-negative (the "influence" comparison scale is unsigned, 0-4). Unlike
# `sframe_check_decision_input()` there are no weights or criteria_types to
# check, and no minimum of 2 rows distinct from columns since row count and
# column count are the same number.
sframe_check_dematel_input <- function(x) {
  if (!is.matrix(x) || !is.numeric(x)) {
    return(list(error = "The influence matrix must be a numeric matrix."))
  }
  if (nrow(x) != ncol(x)) {
    return(list(error = sprintf(
      "The influence matrix must be square. Got %d row(s) and %d column(s).",
      nrow(x), ncol(x)
    )))
  }
  if (nrow(x) < 2) {
    return(list(error = "DEMATEL needs at least 2 criteria."))
  }
  if (anyNA(x)) {
    return(list(error = "The influence matrix contains missing values."))
  }
  if (any(x < 0)) {
    return(list(error = "The influence matrix must be non-negative."))
  }
  note <- NULL
  if (any(diag(x) != 0)) {
    note <- "The diagonal was non-zero and has been set to 0."
    diag(x) <- 0
  }
  list(matrix = x, note = note)
}

# ---------------------------------------------------------------------------
# DEMATEL computation
# ---------------------------------------------------------------------------

#' The DEMATEL total-relation classification
#'
#' Normalises the direct-influence matrix `X` by the larger of its greatest
#' row sum and its greatest column sum (the standard DEMATEL normalisation,
#' which keeps the Neumann series `N + N^2 + N^3 + ...` convergent), then
#' solves the total-relation matrix `T = N(I - N)^-1` in closed form rather
#' than by truncating the series. `D` is each criterion's row sum of `T`
#' (how much it influences the others, direct and indirect combined) and `R`
#' its column sum (how much it is influenced). Prominence `D + R` is overall
#' involvement in the system, while relation `D - R` is net direction,
#' positive for a net cause and negative or zero for a net effect. The
#' threshold is the arithmetic mean of every entry of `T`: relations at or
#' above it are considered significant enough to draw in an influence
#' diagram.
#'
#' `solve()` fails outright if `I - N` is exactly singular, which does not
#' arise for a matrix normalised this way in ordinary use. No fallback series
#' truncation is implemented, unlike the harvested source, because a singular
#' `I - N` here would signal a malformed matrix rather than a case to work
#' around silently.
#'
#' @param x A square numeric matrix of direct influence, zero diagonal.
#' @return A list with `normalised` (N), `total_relation` (T), `D`, `R`,
#'   `prominence` (D + R), `relation` (D - R), `threshold` (mean of T), and
#'   `role` (a character vector, `"cause"` where relation > 0, else
#'   `"effect"`).
sframe_dematel_compute <- function(x) {
  n <- nrow(x)
  max_sum <- max(max(rowSums(x)), max(colSums(x)))
  normalised <- if (max_sum > 0) x / max_sum else x
  total_relation <- normalised %*% solve(diag(n) - normalised)
  dimnames(total_relation) <- dimnames(x)

  D <- rowSums(total_relation)
  R <- colSums(total_relation)
  prominence <- D + R
  relation <- D - R
  threshold <- mean(total_relation)
  role <- ifelse(relation > 0, "cause", "effect")
  names(role) <- rownames(x)

  list(
    normalised     = normalised,
    total_relation = total_relation,
    D              = D,
    R              = R,
    prominence     = prominence,
    relation       = relation,
    threshold      = threshold,
    role           = role
  )
}

# ---------------------------------------------------------------------------
# Input resolution
# ---------------------------------------------------------------------------

# Resolves the square directed influence matrix DEMATEL needs, following the
# same two-source order as the rest of the decision family (researcher-
# supplied options first, a collected role second, a typed error naming what
# is missing third), but over a `pairwise` role rather than
# `performance_items`/`weights_item`, since DEMATEL has neither alternatives
# nor weights.
sframe_resolve_dematel_matrix <- function(data, roles, options, instrument) {
  options <- options %||% list()
  notes <- character(0)

  if (!is.null(options[["matrix"]])) {
    m <- options[["matrix"]]
    if (is.data.frame(m)) {
      # A data.frame is also is.list()==TRUE, but its list elements are
      # columns, not rows: unlist()-ing it the way a genuine list-of-rows is
      # unlisted below would silently transpose the matrix (same failure
      # mode fixed in sframe_decision_options(), R/decision_data.R).
      m <- as.matrix(m)
      storage.mode(m) <- "double"
    } else if (is.list(m)) {
      widths <- vapply(m, length, integer(1))
      if (length(unique(widths)) > 1) {
        return(list(error = sprintf(
          paste0("`options$matrix` rows have differing lengths (%s). A ",
                 "DEMATEL matrix must be square."),
          paste(widths, collapse = ", ")
        )))
      }
      m <- matrix(suppressWarnings(as.numeric(unlist(m, use.names = FALSE))),
                  nrow = length(m), byrow = TRUE)
    } else {
      m <- as.matrix(m)
      storage.mode(m) <- "double"
    }
    criteria <- as.character(options[["criteria"]] %||% character(0))
    if (length(criteria) == 0) criteria <- paste0("C", seq_len(ncol(m)))
    if (length(criteria) != nrow(m) || length(criteria) != ncol(m)) {
      return(list(error = sprintf(
        paste0("`options$criteria` names %d criterion/criteria but ",
               "`options$matrix` is %d x %d. DEMATEL needs a square matrix ",
               "with one row and one column per criterion."),
        length(criteria), nrow(m), ncol(m)
      )))
    }
    dimnames(m) <- list(criteria, criteria)
    return(list(matrix = m, source = "supplied", notes = notes))
  }

  pairwise_item <- sframe_role_values(roles, "pairwise")
  if (length(pairwise_item) == 0) {
    return(list(error = paste0(
      "DEMATEL needs a directed influence matrix. Supply `options$matrix` ",
      "with `options$criteria`, or declare a `pairwise` role naming a ",
      "pairwise_comparison item with `comparison_scale = \"influence\"`."
    )))
  }
  item <- sframe_decision_item(instrument, pairwise_item[1],
                               "pairwise_comparison")
  scale <- item$comparison_scale %||% "saaty"
  if (!identical(scale, "influence")) {
    return(list(error = sprintf(
      paste0("Item '%s' uses the \"%s\" comparison scale. DEMATEL needs a ",
             "pairwise_comparison item declared with ",
             "`comparison_scale = \"influence\"`."),
      pairwise_item[1], scale
    )))
  }
  assembly <- sframe_assemble_pairwise(data, instrument, pairwise_item[1])
  agg <- sframe_aggregate_judgements(assembly, method = "arithmetic")
  notes <- c(notes, sprintf(
    paste0("The influence matrix is the arithmetic mean of %d ",
           "respondent(s)' directed judgements via item '%s'%s."),
    agg$n_respondents, pairwise_item[1],
    if (agg$n_dropped > 0) {
      sprintf(", with %d dropped as incomplete or out of range",
              agg$n_dropped)
    } else ""
  ))
  list(matrix = agg$matrix, source = "collected", notes = notes,
       n_respondents = agg$n_respondents, n_dropped = agg$n_dropped)
}

# ---------------------------------------------------------------------------
# DEMATEL runner
# ---------------------------------------------------------------------------

sframe_run_dematel <- function(data, roles, options, instrument) {
  resolved <- sframe_resolve_dematel_matrix(data, roles, options, instrument)
  if (!is.null(resolved$error)) {
    return(list(test = "dematel", error = resolved$error))
  }
  checked <- sframe_check_dematel_input(resolved$matrix)
  if (!is.null(checked$error)) {
    return(list(test = "dematel", error = checked$error))
  }
  fit <- sframe_dematel_compute(checked$matrix)

  criteria <- rownames(checked$matrix)
  order_by_prominence <- order(fit$prominence, decreasing = TRUE)
  table <- data.frame(
    Criterion  = criteria[order_by_prominence],
    D          = round(as.numeric(fit$D)[order_by_prominence], 4),
    R          = round(as.numeric(fit$R)[order_by_prominence], 4),
    Prominence = round(as.numeric(fit$prominence)[order_by_prominence], 4),
    Relation   = round(as.numeric(fit$relation)[order_by_prominence], 4),
    Role       = fit$role[order_by_prominence],
    stringsAsFactors = FALSE
  )

  causes <- criteria[fit$role == "cause"]
  top_cause <- criteria[which.max(fit$relation)]
  notes <- c(resolved$notes, checked$note)

  list(
    test           = "dematel",
    apa            = sprintf(
      paste0("DEMATEL classified %d criteria by total relation. %s had the ",
             "strongest net causal role (D - R = %.3f), and %d of %d ",
             "criteria were net causes overall."),
      length(criteria), top_cause, max(fit$relation), length(causes),
      length(criteria)
    ),
    prompt         = paste0(
      "Report cause and effect roles together, not prominence alone: a ",
      "criterion can be central to the system (high D + R) while being a ",
      "net effect (D - R <= 0) driven by the others. Relations at or above ",
      "the mean threshold are the ones worth drawing in an influence ",
      "diagram; weaker ones are noise in the judgement data."
    ),
    table          = table,
    criteria       = criteria,
    D              = fit$D,
    R              = fit$R,
    prominence     = fit$prominence,
    relation       = fit$relation,
    role           = fit$role,
    threshold      = fit$threshold,
    total_relation = fit$total_relation,
    normalised     = fit$normalised,
    matrix_source  = resolved$source,
    notes          = notes[!vapply(notes, is.null, logical(1))]
  )
}

# ---------------------------------------------------------------------------
# Plot: prominence vs relation
# ---------------------------------------------------------------------------

#' Prominence-relation scatter for a DEMATEL result
#'
#' The dedicated DEMATEL chart: one point per criterion, prominence (D + R)
#' on x and relation (D - R) on y, with a horizontal quadrant line at
#' relation = 0 separating causes (above) from effects (below). This is
#' deliberately a different shape from [sframe_plot_decision_ranking()]:
#' DEMATEL classifies criteria on two axes rather than producing a single
#' ranked score, so a ranking bar chart would misrepresent it.
#'
#' @param result A `dematel`-family result list from [run_analysis_plan()] or
#'   `sframe_run_dematel()`, carrying `prominence`, `relation`, and
#'   `criteria`.
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object, or `NULL` when the result carries no DEMATEL
#'   fields to plot.
#' @export
#' @seealso [sframe_plot_decision_ranking()]
sframe_plot_dematel_influence <- function(result, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot DEMATEL influence.")
  palette <- match.arg(palette)
  brand <- sframe_brand(palette)

  if (is.null(result$prominence) || is.null(result$relation) ||
      is.null(result$criteria) ||
      length(result$prominence) != length(result$criteria) ||
      length(result$relation) != length(result$criteria)) {
    return(NULL)
  }

  df <- data.frame(
    label      = as.character(result$criteria),
    prominence = as.numeric(result$prominence),
    relation   = as.numeric(result$relation),
    role       = as.character(result$role %||%
      ifelse(as.numeric(result$relation) > 0, "cause", "effect")),
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$prominence) & !is.na(df$relation), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$prominence, y = .data$relation,
                                   colour = .data$role)) +
    ggplot2::geom_hline(yintercept = 0, colour = brand$muted,
                        linewidth = 0.4, linetype = "dashed") +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label),
                       colour = brand$ink, vjust = -0.9, size = 3.2,
                       show.legend = FALSE) +
    ggplot2::scale_colour_manual(
      values = c(cause = brand$fill_duo[1], effect = brand$fill_duo[2])
    ) +
    ggplot2::labs(
      title = "DEMATEL influence map",
      x = "Prominence (D + R)", y = "Relation (D - R)", colour = "Role"
    ) +
    theme_surveyframe(palette = palette)
}
