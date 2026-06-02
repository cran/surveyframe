# conditions.R
# Custom condition classes for surveyframe.
# All validators and exported functions must use these classes.
# Use typed helpers for exported-code errors.

# Friendly sframe type check used by every exported function that takes
# an instrument argument.  Replaces bare stopifnot(inherits(...)) calls
# so new users see an actionable message instead of a raw condition string.
sframe_check_instrument <- function(instrument, arg = "instrument") {
  if (!inherits(instrument, "sframe")) {
    rlang::abort(
      paste0(
        "The `", arg, "` argument must be an `sframe` object. ",
        "Build one with `sf_instrument()` and its component constructors, ",
        "or load a saved instrument from disk with `read_sframe()`."
      ),
      class = "sframe_error"
    )
  }
}

sframe_require_shiny <- function(reason) {
  rlang::check_installed("shiny", reason = reason)
}

sframe_require_psych <- function(reason) {
  rlang::check_installed("psych", reason = reason)
}

sframe_require_MASS <- function(reason) {
  rlang::check_installed("MASS", reason = reason)
}

sframe_require_nnet <- function(reason) {
  rlang::check_installed("nnet", reason = reason)
}

#' Abort with a validation error
#'
#' @param message Character. The error message.
#' @param instrument_title Character or NULL. Title of the instrument being
#'   validated, included in the condition metadata when supplied.
#' @param ... Additional named fields passed to `rlang::abort()`.
#' @keywords internal
sframe_abort_validation <- function(message, instrument_title = NULL, ...) {
  rlang::abort(
    message  = message,
    class    = c("sframe_validation_error", "sframe_error"),
    instrument_title = instrument_title,
    ...
  )
}

#' Abort with an import error
#'
#' @param message Character. The error message.
#' @param path Character or NULL. The file path that failed to import.
#' @param ... Additional named fields passed to `rlang::abort()`.
#' @keywords internal
sframe_abort_import <- function(message, path = NULL, ...) {
  rlang::abort(
    message = message,
    class   = c("sframe_import_error", "sframe_error"),
    path    = path,
    ...
  )
}

#' Abort with a branching error
#'
#' @param message Character. The error message.
#' @param item_id Character or NULL. The item ID involved in the broken rule.
#' @param ... Additional named fields passed to `rlang::abort()`.
#' @keywords internal
sframe_abort_branching <- function(message, item_id = NULL, ...) {
  rlang::abort(
    message = message,
    class   = c("sframe_branching_error", "sframe_error"),
    item_id = item_id,
    ...
  )
}

#' Warn about a data quality issue
#'
#' @param message Character. The warning message.
#' @param respondent_ids Character vector or NULL. IDs of affected respondents.
#' @param ... Additional named fields passed to `rlang::warn()`.
#' @keywords internal
sframe_warn_quality <- function(message, respondent_ids = NULL, ...) {
  rlang::warn(
    message        = message,
    class          = c("sframe_quality_warning", "sframe_warning"),
    respondent_ids = respondent_ids,
    ...
  )
}

#' Warn about missing data
#'
#' @param message Character. The warning message.
#' @param item_id Character or NULL. The item ID with missing data.
#' @param rate Numeric or NULL. The observed missing rate.
#' @param ... Additional named fields passed to `rlang::warn()`.
#' @keywords internal
sframe_warn_missing <- function(message, item_id = NULL, rate = NULL, ...) {
  rlang::warn(
    message = message,
    class   = c("sframe_missing_data_warning", "sframe_warning"),
    item_id = item_id,
    rate    = rate,
    ...
  )
}

#' Warn about a scoring issue
#'
#' @param message Character. The warning message.
#' @param scale_id Character or NULL. The scale ID affected.
#' @param ... Additional named fields passed to `rlang::warn()`.
#' @keywords internal
sframe_warn_scoring <- function(message, scale_id = NULL, ...) {
  rlang::warn(
    message  = message,
    class    = c("sframe_scoring_warning", "sframe_warning"),
    scale_id = scale_id,
    ...
  )
}
