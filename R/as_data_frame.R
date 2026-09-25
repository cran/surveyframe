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
    # Reversal is declared either on the item or in its scale's reverse_items,
    # and both are scored. Reading the item flag alone showed an item its scale
    # reverses as though it were scored in the same direction as the rest.
    reverse    = vapply(instrument$items, function(i) {
      isTRUE(i$reverse) || any(vapply(instrument$scales %||% list(), function(s) {
        i$id %in% sframe_scale_reverse_ids(instrument, s)
      }, logical(1)))
    }, logical(1)),
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
#' Every surveyframe class returns its primary table, and each class has its
#' own columns. Where an object holds more than one table, the others are
#' reachable through the named accessors in [sf_accessors], or, for a full
#' tabular record of an instrument, through [codebook_report()].
#'
#' # What each class gives
#'
#' * `sframe`: one row per item, with `id`, `label`, `type`, `choice_set`,
#'   `scale_id`, `reverse` and `required`.
#' * `sframe_codebook`: one row per item, the codebook's item table.
#' * `sframe_validation`: one row per problem, with `check` and `problem`.
#' * `sframe_analysis_results`: one row per block, with `block`,
#'   `research_question`, `method` and `apa`.
#' * `sframe_reliability_report`: one row per scale, with `scale_id`, `label`,
#'   `n_items`, `n`, `alpha` and `omega`.
#' * `sframe_item_report`: one row per item, with `scale_id` and the item
#'   diagnostics.
#' * `sframe_quality_report`: one row per check, the flattened quality checks.
#' * `sframe_efa_report`: one row per measure, the readiness measures.
#' * `sframe_sensitivity`: one row per perturbation, with `criterion`,
#'   `direction`, `weight`, `rho`, `rank_changed` and `top_changed`.
#'
# The item and scale tables are a summary, and say so on the page, since an
# integration reading one as the full declaration loses the type settings.
#' # A summary, and where the full record is
#'
#' These tables are a summary of the columns a reader scans first. The item
#' table leaves out help text, placeholder, matrix rows, comparison items and
#' scale, slider and rating settings, date bounds, section introduction and
#' page; the scale table leaves out `min_valid`, the reverse key and the
#' weights. Read the stored declaration in full through [sf_items()],
#' [sf_scales()] and the rest of [sf_accessors], each of which returns the
#' component objects themselves, or through [write_sframe()] for the
#' interchange record.
#'
#' A class holding one table returns it directly, so it keeps that table's own
#' row names and `row.names` has no effect. Pass `row.names` to
#' [base::as.data.frame()] on the returned frame where you need to set them.
#' The coercion gives one view of an object. An instrument, for example,
#' returns its items, and its choice sets and scales come from
#' [sf_choice_sets()], [sf_scales()] or [codebook_report()].
#'
#'
#' @return A data frame, with the columns listed above for the class given.
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

#' Coerce an instrument to its item summary
#'
#' Returns one row per item in a surveyframe instrument.
#'
#' @param x A surveyframe object.
#' @param row.names Passed to [base::as.data.frame()] by the methods that
#'   build a frame. Ignored by the methods that return a stored table.
#' @param optional Passed to [base::as.data.frame()].
#' @param ... Ignored. Present for S3 consistency.
#' @return A data frame summarising the instrument's items.
#' @seealso [sframe_as_data_frame], [sf_items()], [codebook_report()]
#' @exportS3Method as.data.frame sframe
as.data.frame.sframe <- function(x, row.names = NULL, optional = FALSE, ...) {
  out <- sframe_items_table(x)
  if (!is.null(row.names)) rownames(out) <- row.names
  out
}

#' Coerce a choice set to a data frame
#'
#' Returns the stored values and respondent-facing labels in a choice set.
#'
#' @inheritParams as.data.frame.sframe
#' @return A data frame with `value` and `label` columns.
#' @seealso [sframe_as_data_frame], [sf_choices()]
#' @exportS3Method as.data.frame sf_choices
as.data.frame.sf_choices <- function(x, row.names = NULL, optional = FALSE, ...) {
  data.frame(
    value = as.character(x$values),
    label = as.character(x$labels),
    stringsAsFactors = FALSE,
    row.names = row.names
  )
}

#' Extract the item table from a codebook report
#'
#' @inheritParams as.data.frame.sframe
#' @return The codebook report's item table as a data frame.
#' @seealso [sframe_as_data_frame], [codebook_report()]
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

#' Coerce a reliability report to a data frame
#'
#' Returns one row per scale with alpha and omega estimates.
#'
#' @inheritParams as.data.frame.sframe
#' @return A data frame of scale reliability statistics.
#' @seealso [sframe_as_data_frame], [reliability_report()]
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

#' Coerce an item report to a data frame
#'
#' Stacks item diagnostics from every reported scale into one table.
#'
#' @inheritParams as.data.frame.sframe
#' @return A data frame of item-level diagnostics.
#' @seealso [sframe_as_data_frame], [item_report()]
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

#' Coerce an EFA readiness report to a data frame
#'
#' @inheritParams as.data.frame.sframe
#' @return A one-row data frame containing readiness measures and the suggested
#'   number of factors.
#' @seealso [sframe_as_data_frame], [efa_report()]
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

#' Extract the loading table from an EFA solution
#'
#' @inheritParams as.data.frame.sframe
#' @return The solution's long-form factor-loading data frame.
#' @seealso [sframe_as_data_frame], [efa_solution()]
#' @exportS3Method as.data.frame sframe_efa_solution
as.data.frame.sframe_efa_solution <- function(x, row.names = NULL,
                                              optional = FALSE, ...) {
  x$loadings_long
}

# ---------------------------------------------------------------------------
# Statistics reports
# ---------------------------------------------------------------------------

#' Extract the descriptives results table
#'
#' @inheritParams as.data.frame.sframe
#' @return The primary table from a descriptives report.
#' @seealso [sframe_as_data_frame], [descriptives_report()]
#' @exportS3Method as.data.frame sframe_descriptives_report
as.data.frame.sframe_descriptives_report <- function(x, row.names = NULL,
                                                     optional = FALSE, ...) {
  x$table
}

#' Extract item missingness results
#'
#' @inheritParams as.data.frame.sframe
#' @return The item-level missingness table from a missing-data report.
#' @seealso [sframe_as_data_frame], [missing_data_report()]
#' @exportS3Method as.data.frame sframe_missing_data_report
as.data.frame.sframe_missing_data_report <- function(x, row.names = NULL,
                                                     optional = FALSE, ...) {
  x$item_missing
}

#' Extract reliability evidence from a validity report
#'
#' @inheritParams as.data.frame.sframe
#' @return The reliability table stored in a validity report.
#' @seealso [sframe_as_data_frame], [validity_report()]
#' @exportS3Method as.data.frame sframe_validity_report
as.data.frame.sframe_validity_report <- function(x, row.names = NULL,
                                                 optional = FALSE, ...) {
  x$reliability
}

#' Coerce an assumption report to a data frame
#'
#' Combines the available normality, homogeneity, and regression checks into a
#' long-form summary.
#'
#' @inheritParams as.data.frame.sframe
#' @return A data frame naming each assumption family, variable, and statistic.
#' @seealso [sframe_as_data_frame], [assumption_report()]
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

#' Coerce a sample-size plan to a data frame
#'
#' @inheritParams as.data.frame.sframe
#' @return A one-row data frame containing the analysis type, estimated sample
#'   size, alpha, and power.
#' @seealso [sframe_as_data_frame], [sample_size_plan()]
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

#' Coerce a response-quality report to a data frame
#'
#' @inheritParams as.data.frame.sframe
#' @return A one-row data frame summarising respondents, items, and quality
#'   flags.
#' @seealso [sframe_as_data_frame], [quality_report()]
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

#' Extract a sensitivity-analysis results table
#'
#' @inheritParams as.data.frame.sframe
#' @return The primary results table from a sensitivity analysis.
#' @seealso [sframe_as_data_frame]
#' @exportS3Method as.data.frame sframe_sensitivity
as.data.frame.sframe_sensitivity <- function(x, row.names = NULL,
                                             optional = FALSE, ...) {
  x$table
}

#' Summarise analysis-plan results as a data frame
#'
#' Returns one row per analysis block, including its research question, method,
#' APA summary, and any error.
#'
#' @inheritParams as.data.frame.sframe
#' @return A data frame with one row per analysis block.
#' @seealso [sframe_as_data_frame], [run_analysis_plan()]
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
#'
#' @return An object of the same class as `x`.
#' @name sframe_subset
NULL

#' @noRd
#' @exportS3Method `[` sframe_analysis_results
`[.sframe_analysis_results` <- function(x, i, ...) {
  structure(NextMethod(), class = "sframe_analysis_results")
}

#' @noRd
#' @exportS3Method `[` sframe_reliability_report
`[.sframe_reliability_report` <- function(x, i, ...) {
  structure(NextMethod(), class = "sframe_reliability_report")
}

#' @noRd
#' @exportS3Method `[` sframe_item_report
`[.sframe_item_report` <- function(x, i, ...) {
  structure(NextMethod(), class = "sframe_item_report")
}
