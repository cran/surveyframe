# as_data_frame.R
# as.data.frame() methods for the surveyframe classes, and class-preserving
# subsetting for the list-backed report classes.
#
# Before 0.4.0 as.data.frame() failed on every one of these with "cannot
# coerce class ... to a data.frame", so `$` was the only route to their
# contents. Each method returns the object's primary table. Secondary tables
# stay reachable through the named accessors.

# ---------------------------------------------------------------------------
# Shared table builders. codebook_report() and as.data.frame.sframe() both
# read these, so the two views of an instrument cannot drift apart.
# ---------------------------------------------------------------------------

sframe_items_table <- function(instrument) {
  data.frame(
    id         = vapply(instrument$items, function(i) i$id,    character(1)),
    label      = vapply(instrument$items, function(i) i$label, character(1)),
    type       = vapply(instrument$items, function(i) i$type,  character(1)),
    choice_set = vapply(instrument$items, function(i) i$choice_set %||% "", character(1)),
    scale_id   = vapply(instrument$items, function(i) i$scale_id %||% "", character(1)),
    reverse    = vapply(instrument$items, function(i) isTRUE(i$reverse), logical(1)),
    required   = vapply(instrument$items, function(i) isTRUE(i$required), logical(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

sframe_choices_table <- function(instrument) {
  if (length(instrument$choices) == 0) {
    return(data.frame(
      choice_set_id = character(0), value = character(0), label = character(0),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
  }
  rows <- lapply(instrument$choices, function(cs) {
    data.frame(
      choice_set_id = cs$id,
      value         = as.character(cs$values),
      label         = cs$labels,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

sframe_scales_table <- function(instrument) {
  if (length(instrument$scales) == 0) {
    return(data.frame(
      id = character(0), label = character(0), method = character(0),
      n_items = integer(0), items = character(0),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
  }
  data.frame(
    id      = vapply(instrument$scales, function(s) s$id,    character(1)),
    label   = vapply(instrument$scales, function(s) s$label, character(1)),
    method  = vapply(instrument$scales, function(s) s$method, character(1)),
    n_items = vapply(instrument$scales, function(s) length(s$items), integer(1)),
    items   = vapply(instrument$scales, function(s) paste(s$items, collapse = ", "), character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

sframe_plan_table <- function(instrument) {
  plan <- instrument$analysis_plan %||% list()
  if (length(plan) == 0) {
    return(data.frame(
      id = character(0), research_question = character(0),
      method = character(0), variables = character(0),
      decision_rule = character(0),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
  }
  data.frame(
    id = vapply(plan, function(b) b$id %||% "", character(1)),
    research_question = vapply(plan, function(b) b$research_question %||% "", character(1)),
    method = vapply(plan, sframe_analysis_method, character(1)),
    variables = vapply(plan, function(b) paste(sframe_analysis_vars(b), collapse = ", "), character(1)),
    decision_rule = vapply(plan,
      function(b) b$decision_rule %||% b$interpretation %||% "", character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

sframe_models_table <- function(instrument) {
  models <- instrument$models %||% list()
  if (length(models) == 0) {
    return(data.frame(
      id = character(0), label = character(0), type = character(0),
      engine = character(0), n_constructs = integer(0), n_paths = integer(0),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
  }
  data.frame(
    id     = vapply(models, function(m) m$id %||% "", character(1)),
    label  = vapply(models, function(m) m$label %||% "", character(1)),
    type   = vapply(models, function(m) m$type %||% "", character(1)),
    engine = vapply(models, function(m) m$engine %||% "", character(1)),
    n_constructs = vapply(models, function(m) length(sframe_model_constructs(m)), integer(1)),
    n_paths = vapply(models, function(m) length(m$structural$paths %||% list()), integer(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# ---------------------------------------------------------------------------
# Instrument and components
# ---------------------------------------------------------------------------

#' Coerce a surveyframe object to a data frame
#'
#' Every surveyframe class returns its primary table. For an instrument that
#' is the item table, for a validation result the problems, for a report the
#' table it is mainly about. Where an object holds more than one table, the
#' others are reachable through the named accessors in [sf_accessors] or,
#' for a full tabular record of an instrument, through [codebook_report()].
#'
#' @param x A surveyframe object.
#' @param row.names Passed to [base::as.data.frame()].
#' @param optional Passed to [base::as.data.frame()].
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return A data frame.
#' @name sframe_as_data_frame
#' @seealso [sf_accessors], [codebook_report()]
#'
#' @examples
#' cs    <- sf_choices("ag5", 1:5,
#'            c("Strongly disagree", "Disagree", "Neutral",
#'              "Agree", "Strongly agree"))
#' item  <- sf_item("sat_1", "The service met my expectations.",
#'                  type = "likert", choice_set = "ag5", scale_id = "sat")
#' scale <- sf_scale("sat", "Satisfaction", items = "sat_1")
#' instr <- sf_instrument("Demo Survey", components = list(cs, item, scale))
#'
#' as.data.frame(instr)
#' as.data.frame(cs)
NULL

#' @rdname sframe_as_data_frame
#' @exportS3Method as.data.frame sframe
as.data.frame.sframe <- function(x, row.names = NULL, optional = FALSE, ...) {
  out <- sframe_items_table(x)
  if (!is.null(row.names)) rownames(out) <- row.names
  out
}

#' @rdname sframe_as_data_frame
#' @exportS3Method as.data.frame sf_choices
as.data.frame.sf_choices <- function(x, row.names = NULL, optional = FALSE, ...) {
  data.frame(
    value = as.character(x$values),
    label = as.character(x$labels),
    stringsAsFactors = FALSE,
    row.names = row.names
  )
}

#' @rdname sframe_as_data_frame
#' @exportS3Method as.data.frame sframe_codebook
as.data.frame.sframe_codebook <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$items_table
}

# ---------------------------------------------------------------------------
# Psychometric reports
# ---------------------------------------------------------------------------

# A scalar field that may be absent on some scales, returned as NA so every
# scale still contributes a row rather than being dropped from the table.
sframe_num_or_na <- function(x, field) {
  value <- x[[field]]
  if (is.null(value) || length(value) == 0) return(NA_real_)
  as.numeric(value)[1]
}

#' @rdname sframe_as_data_frame
#' @exportS3Method as.data.frame sframe_reliability_report
as.data.frame.sframe_reliability_report <- function(x, row.names = NULL,
                                                    optional = FALSE, ...) {
  if (length(x) == 0) {
    return(data.frame(
      scale_id = character(0), label = character(0), n_items = integer(0),
      n = integer(0), alpha = numeric(0), alpha_std = numeric(0),
      omega_h = numeric(0), omega_t = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    scale_id  = vapply(x, function(s) as.character(s$scale_id)[1], character(1)),
    label     = vapply(x, function(s) as.character(s$label %||% "")[1], character(1)),
    n_items   = vapply(x, function(s) as.integer(s$n_items)[1], integer(1)),
    n         = vapply(x, function(s) as.integer(s$n)[1], integer(1)),
    alpha     = vapply(x, sframe_num_or_na, numeric(1), "alpha"),
    alpha_std = vapply(x, sframe_num_or_na, numeric(1), "alpha_std"),
    omega_h   = vapply(x, sframe_num_or_na, numeric(1), "omega_h"),
    omega_t   = vapply(x, sframe_num_or_na, numeric(1), "omega_t"),
    stringsAsFactors = FALSE,
    row.names = row.names
  )
}

#' @rdname sframe_as_data_frame
#' @exportS3Method as.data.frame sframe_item_report
as.data.frame.sframe_item_report <- function(x, row.names = NULL,
                                             optional = FALSE, ...) {
  if (length(x) == 0) {
    return(data.frame(
      scale_id = character(0), item_id = character(0), mean = numeric(0),
      sd = numeric(0), item_rest_r = numeric(0), floor_pct = numeric(0),
      ceiling_pct = numeric(0), n_missing = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(x, function(s) {
    diag <- s$diagnostics
    cbind(scale_id = rep(s$scale_id, nrow(diag)), diag,
          stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- row.names
  out
}

#' @rdname sframe_as_data_frame
#' @exportS3Method as.data.frame sframe_efa_report
as.data.frame.sframe_efa_report <- function(x, row.names = NULL,
                                            optional = FALSE, ...) {
  data.frame(
    n_items            = as.integer(x$n_items),
    n                  = as.integer(x$n),
    kmo                = as.numeric(x$kmo$MSA),
    bartlett_chisq     = as.numeric(x$bartlett$chisq),
    bartlett_df        = as.integer(x$bartlett$df),
    bartlett_p         = as.numeric(x$bartlett$p.value),
    suggested_nfactors = as.integer(x$suggested_nfactors),
    rotation           = as.character(x$rotation_note),
    stringsAsFactors = FALSE,
    row.names = row.names
  )
}

#' @rdname sframe_as_data_frame
#' @exportS3Method as.data.frame sframe_efa_solution
as.data.frame.sframe_efa_solution <- function(x, row.names = NULL,
                                              optional = FALSE, ...) {
  x$loadings_long
}

# ---------------------------------------------------------------------------
# Statistics reports
# ---------------------------------------------------------------------------

#' @rdname sframe_as_data_frame
#' @exportS3Method as.data.frame sframe_descriptives_report
as.data.frame.sframe_descriptives_report <- function(x, row.names = NULL,
                                                     optional = FALSE, ...) {
  x$table
}

#' @rdname sframe_as_data_frame
#' @exportS3Method as.data.frame sframe_missing_data_report
as.data.frame.sframe_missing_data_report <- function(x, row.names = NULL,
                                                     optional = FALSE, ...) {
  x$item_missing
}

#' @rdname sframe_as_data_frame
#' @exportS3Method as.data.frame sframe_validity_report
as.data.frame.sframe_validity_report <- function(x, row.names = NULL,
                                                 optional = FALSE, ...) {
  x$reliability
}

#' @rdname sframe_as_data_frame
#' @exportS3Method as.data.frame sframe_assumption_report
as.data.frame.sframe_assumption_report <- function(x, row.names = NULL,
                                                   optional = FALSE, ...) {
  # The three families carry different columns, so they stack into a long
  # table with the family named rather than being forced into one wide shape.
  parts <- list(
    normality   = x$normality,
    homogeneity = x$homogeneity,
    regression  = x$regression
  )
  rows <- list()
  for (nm in names(parts)) {
    part <- parts[[nm]]
    if (is.null(part) || !is.data.frame(part) || nrow(part) == 0) next
    rows[[nm]] <- data.frame(
      family   = nm,
      variable = as.character(part$variable %||% rep(NA_character_, nrow(part))),
      statistic = as.character(part$test %||% rep(nm, nrow(part))),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) {
    return(data.frame(
      family = character(0), variable = character(0),
      statistic = character(0), stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- row.names
  out
}

#' @rdname sframe_as_data_frame
#' @exportS3Method as.data.frame sframe_sample_size_plan
as.data.frame.sframe_sample_size_plan <- function(x, row.names = NULL,
                                                  optional = FALSE, ...) {
  data.frame(
    type        = as.character(x$type),
    estimated_n = as.numeric(x$estimated_n),
    alpha       = as.numeric(x$alpha),
    power       = as.numeric(x$power),
    stringsAsFactors = FALSE,
    row.names = row.names
  )
}

#' @rdname sframe_as_data_frame
#' @exportS3Method as.data.frame sframe_quality_report
as.data.frame.sframe_quality_report <- function(x, row.names = NULL,
                                                optional = FALSE, ...) {
  data.frame(
    n_respondents = as.integer(x$summary$n_respondents),
    n_items       = as.integer(x$summary$n_items),
    n_flagged     = as.integer(x$summary$n_flagged),
    flag_rate     = as.numeric(x$summary$flag_rate),
    stringsAsFactors = FALSE,
    row.names = row.names
  )
}

#' @rdname sframe_as_data_frame
#' @exportS3Method as.data.frame sframe_sensitivity
as.data.frame.sframe_sensitivity <- function(x, row.names = NULL,
                                             optional = FALSE, ...) {
  x$table
}

#' @rdname sframe_as_data_frame
#' @exportS3Method as.data.frame sframe_analysis_results
as.data.frame.sframe_analysis_results <- function(x, row.names = NULL,
                                                  optional = FALSE, ...) {
  if (length(x) == 0) {
    return(data.frame(
      block_id = character(0), research_question = character(0),
      test = character(0), apa = character(0), error = character(0),
      stringsAsFactors = FALSE
    ))
  }
  chr <- function(field) {
    vapply(x, function(r) as.character(r[[field]] %||% "")[1], character(1))
  }
  data.frame(
    block_id          = names(x) %||% rep("", length(x)),
    research_question = chr("research_question"),
    test              = chr("test"),
    apa               = chr("apa"),
    error             = chr("error"),
    stringsAsFactors  = FALSE,
    row.names = row.names
  )
}

# ---------------------------------------------------------------------------
# Class-preserving subsetting
#
# `[` on a classed list drops the class, so results[1:2] silently degraded to
# a bare list and lost its print method. These keep the class so a subset of
# a report is still a report.
# ---------------------------------------------------------------------------

#' Subset a surveyframe report
#'
#' Keeps the report class, so a subset still prints as a report and still
#' answers `as.data.frame()`.
#'
#' @param x An `sframe_analysis_results`, `sframe_reliability_report`, or
#'   `sframe_item_report` object.
#' @param i Index, name, or logical vector.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return An object of the same class as `x`.
#' @name sframe_subset
NULL

#' @rdname sframe_subset
#' @exportS3Method `[` sframe_analysis_results
`[.sframe_analysis_results` <- function(x, i, ...) {
  structure(NextMethod(), class = "sframe_analysis_results")
}

#' @rdname sframe_subset
#' @exportS3Method `[` sframe_reliability_report
`[.sframe_reliability_report` <- function(x, i, ...) {
  structure(NextMethod(), class = "sframe_reliability_report")
}

#' @rdname sframe_subset
#' @exportS3Method `[` sframe_item_report
`[.sframe_item_report` <- function(x, i, ...) {
  structure(NextMethod(), class = "sframe_item_report")
}
