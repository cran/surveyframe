# validation_result.R
# The sframe_validation class and its methods. Both validate_sframe() and
# validate_model() return one of these, so a validator returns a diagnostic
# result rather than the object it was handed.

# A small accumulator so every problem is recorded against the check that
# raised it. Without this the function can say what is wrong but not which
# check found it, and it cannot say which checks ran and passed.
sframe_new_problem_log <- function() {
  log <- new.env(parent = emptyenv())
  log$check    <- character(0)
  log$problems <- character(0)
  log
}

sframe_log_problem <- function(log, check, messages) {
  if (length(messages) == 0) return(invisible(NULL))
  log$check    <- c(log$check, rep(check, length(messages)))
  log$problems <- c(log$problems, messages)
  invisible(NULL)
}

# The roster is passed in rather than derived from the log, so a check that
# found nothing still appears with status "ok". A diagnostic that lists only
# failures cannot tell a user whether a check ran at all.
sframe_check_table <- function(log, roster) {
  counts <- vapply(roster, function(nm) sum(log$check == nm), integer(1))
  data.frame(
    check      = roster,
    status     = ifelse(counts > 0L, "problem", "ok"),
    n_problems = as.integer(counts),
    stringsAsFactors = FALSE,
    row.names  = NULL
  )
}

sframe_new_validation <- function(log, roster, subject, title = NULL,
                                  version = NULL, object = NULL) {
  structure(
    list(
      valid      = length(log$problems) == 0,
      problems   = log$problems,
      n_problems = length(log$problems),
      subject    = subject,
      title      = title,
      version    = version,
      checks     = sframe_check_table(log, roster),
      # The check that raised each problem, parallel to `problems`. Kept so
      # as.data.frame() can label every row without re-deriving it.
      problem_checks = log$check,
      object     = object
    ),
    class = "sframe_validation"
  )
}

#' Report on a validation result
#'
#' `sframe_validation` is the diagnostic object returned by
#' [validate_sframe()] and [validate_model()]. It records whether the object
#' passed, every problem found, and every check that ran, including the checks
#' that found nothing.
#'
#' Use [sf_is_valid()] for the pass or fail flag, [sf_problems()] for the
#' messages, `as.data.frame()` for a problem-per-row table, `summary()` for
#' the full check roster, and [as_sframe()] to recover the validated
#' instrument.
#'
#' @param x,object An `sframe_validation` object.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return `print()` returns `x` invisibly. `format()` returns a single
#'   character string. `summary()` returns the check table as a data frame.
#'   `as.data.frame()` returns one row per problem.
#' @name sframe_validation
#' @seealso [validate_sframe()], [validate_model()], [sf_problems()],
#'   [sf_is_valid()], [as_sframe()]
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
#' v <- validate_sframe(instr, strict = FALSE)
#' v
#' sf_is_valid(v)
#' sf_problems(v)
#' as.data.frame(v)
#' summary(v)
NULL

#' @rdname sframe_validation
#' @exportS3Method print sframe_validation
print.sframe_validation <- function(x, ...) {
  cat("<sframe validation>\n")
  subject_line <- if (identical(x$subject, "model")) "Model:" else "Instrument:"
  label <- x$title %||% "(untitled)"
  if (!is.null(x$version) && nzchar(x$version)) {
    label <- paste0(label, " (", x$version, ")")
  }
  cat(sprintf("  %-12s %s\n", subject_line, label))
  cat(sprintf("  %-12s %s\n", "Status:",
              if (isTRUE(x$valid)) {
                "valid"
              } else {
                sprintf("%d problem(s) found", x$n_problems)
              }))
  n_checks <- nrow(x$checks)
  n_failed <- sum(x$checks$status == "problem")
  cat(sprintf("  %-12s %d run, %d with problems\n", "Checks:",
              n_checks, n_failed))

  if (!isTRUE(x$valid)) {
    cat("\nProblems:\n")
    for (i in seq_along(x$problems)) {
      cat(sprintf("  %d. %s\n", i, x$problems[i]))
    }
    cat("\nUse summary() for the full check roster.\n")
  }
  invisible(x)
}

#' @rdname sframe_validation
#' @exportS3Method format sframe_validation
format.sframe_validation <- function(x, ...) {
  sprintf(
    "<sframe validation: %s | %s | %d problem(s) across %d check(s)>",
    x$title %||% "(untitled)",
    if (isTRUE(x$valid)) "valid" else "invalid",
    x$n_problems,
    nrow(x$checks)
  )
}

#' @rdname sframe_validation
#' @exportS3Method summary sframe_validation
summary.sframe_validation <- function(object, ...) {
  object$checks
}

#' @rdname sframe_validation
#' @param row.names Passed to [base::as.data.frame()].
#' @param optional Passed to [base::as.data.frame()].
#' @exportS3Method as.data.frame sframe_validation
as.data.frame.sframe_validation <- function(x, row.names = NULL,
                                            optional = FALSE, ...) {
  data.frame(
    check   = as.character(x$problem_checks %||% character(0)),
    problem = as.character(x$problems),
    stringsAsFactors = FALSE,
    row.names = row.names
  )
}
