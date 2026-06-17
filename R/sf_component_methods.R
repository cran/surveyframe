# sf_component_methods.R
# S3 methods (print, format, summary) for the sframe component classes:
# sf_choices, sf_item, sf_scale, sf_branch, sf_check, sf_model.
# These give every exported component constructor a visible, documented S3
# surface so that methods(class = "sf_choices") and friends are non-empty.

# sf_choices -----------------------------------------------------------------

#' Print an sf_choices object
#'
#' @param x An object of class `sf_choices`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return `x`, invisibly.
#' @exportS3Method print sf_choices
#' @examples
#' cs <- sf_choices("agree5", 1:5,
#'                  c("Strongly disagree", "Disagree", "Neutral",
#'                    "Agree", "Strongly agree"))
#' print(cs)
print.sf_choices <- function(x, ...) {
  cat(sprintf("<sf_choices: %s | %d option(s)>\n", x$id, length(x$values)))
  if (length(x$values) > 0) {
    df <- data.frame(value = x$values, label = x$labels,
                     stringsAsFactors = FALSE, check.names = FALSE)
    print(df, row.names = FALSE)
  }
  invisible(x)
}

#' Format an sf_choices object as a string
#'
#' @param x An object of class `sf_choices`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return A single character string.
#' @exportS3Method format sf_choices
format.sf_choices <- function(x, ...) {
  sprintf("<sf_choices: %s | %d option(s)>", x$id, length(x$values))
}

#' Summarise an sf_choices object
#'
#' @param object An object of class `sf_choices`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return `object`, invisibly.
#' @exportS3Method summary sf_choices
summary.sf_choices <- function(object, ...) {
  cat(format(object), "\n")
  cat(sprintf("  allow_other: %s | randomise: %s\n",
              isTRUE(object$allow_other), isTRUE(object$randomise)))
  invisible(object)
}

# sf_item --------------------------------------------------------------------

#' Print an sf_item object
#'
#' @param x An object of class `sf_item`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return `x`, invisibly.
#' @exportS3Method print sf_item
#' @examples
#' it <- sf_item("q1", "How satisfied are you?", type = "likert",
#'               choice_set = "agree5")
#' print(it)
print.sf_item <- function(x, ...) {
  cat(sprintf("<sf_item: %s | type: %s>\n", x$id, x$type))
  cat(sprintf("  Label: %s\n", x$label))
  if (!is.null(x$choice_set)) cat(sprintf("  Choice set: %s\n", x$choice_set))
  if (!is.null(x$scale_id))   cat(sprintf("  Scale: %s\n", x$scale_id))
  invisible(x)
}

#' Format an sf_item object as a string
#'
#' @param x An object of class `sf_item`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return A single character string.
#' @exportS3Method format sf_item
format.sf_item <- function(x, ...) {
  sprintf("<sf_item: %s | type: %s>", x$id, x$type)
}

#' Summarise an sf_item object
#'
#' @param object An object of class `sf_item`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return `object`, invisibly.
#' @exportS3Method summary sf_item
summary.sf_item <- function(object, ...) {
  cat(format(object), "\n")
  cat(sprintf("  required: %s | reverse: %s\n",
              isTRUE(object$required), isTRUE(object$reverse)))
  invisible(object)
}

# sf_scale -------------------------------------------------------------------

#' Print an sf_scale object
#'
#' @param x An object of class `sf_scale`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return `x`, invisibly.
#' @exportS3Method print sf_scale
#' @examples
#' sc <- sf_scale("sat", "Satisfaction", items = c("q1", "q2", "q3"))
#' print(sc)
print.sf_scale <- function(x, ...) {
  cat(sprintf("<sf_scale: %s | %d item(s)>\n", x$id, length(x$items)))
  cat(sprintf("  Label: %s\n", x$label))
  cat(sprintf("  Items: %s\n", paste(x$items, collapse = ", ")))
  cat(sprintf("  Scoring: %s\n", x$method %||% "mean"))
  invisible(x)
}

#' Format an sf_scale object as a string
#'
#' @param x An object of class `sf_scale`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return A single character string.
#' @exportS3Method format sf_scale
format.sf_scale <- function(x, ...) {
  sprintf("<sf_scale: %s | %d item(s)>", x$id, length(x$items))
}

#' Summarise an sf_scale object
#'
#' @param object An object of class `sf_scale`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return `object`, invisibly.
#' @exportS3Method summary sf_scale
summary.sf_scale <- function(object, ...) {
  cat(format(object), "\n")
  cat(sprintf("  scoring: %s | min valid: %s\n",
              object$method %||% "mean",
              object$min_valid %||% length(object$items)))
  invisible(object)
}

# sf_branch ------------------------------------------------------------------

#' Print an sf_branch object
#'
#' @param x An object of class `sf_branch`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return `x`, invisibly.
#' @exportS3Method print sf_branch
#' @examples
#' br <- sf_branch("q2", depends_on = "q1", operator = "==",
#'                 value = "yes", action = "show")
#' print(br)
print.sf_branch <- function(x, ...) {
  cat(sprintf("<sf_branch: %s>\n", x$item_id))
  cat(sprintf("  Rule: %s when %s %s %s\n",
              x$action %||% "show",
              x$depends_on, x$operator,
              paste(x$value, collapse = ", ")))
  invisible(x)
}

#' Format an sf_branch object as a string
#'
#' @param x An object of class `sf_branch`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return A single character string.
#' @exportS3Method format sf_branch
format.sf_branch <- function(x, ...) {
  sprintf("<sf_branch: %s on %s %s %s>",
          x$item_id, x$depends_on, x$operator,
          paste(x$value, collapse = ", "))
}

#' Summarise an sf_branch object
#'
#' @param object An object of class `sf_branch`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return `object`, invisibly.
#' @exportS3Method summary sf_branch
summary.sf_branch <- function(object, ...) {
  cat(format(object), "\n")
  invisible(object)
}

# sf_check -------------------------------------------------------------------

#' Print an sf_check object
#'
#' @param x An object of class `sf_check`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return `x`, invisibly.
#' @exportS3Method print sf_check
#' @examples
#' ck <- sf_check("attn1", item_id = "q5", type = "attention",
#'                pass_values = 3)
#' print(ck)
print.sf_check <- function(x, ...) {
  cat(sprintf("<sf_check: %s | type: %s>\n", x$id, x$type))
  cat(sprintf("  Item: %s\n", x$item_id))
  if (!is.null(x$pass_values)) {
    cat(sprintf("  Pass values: %s\n", paste(x$pass_values, collapse = ", ")))
  }
  invisible(x)
}

#' Format an sf_check object as a string
#'
#' @param x An object of class `sf_check`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return A single character string.
#' @exportS3Method format sf_check
format.sf_check <- function(x, ...) {
  sprintf("<sf_check: %s | type: %s | item: %s>", x$id, x$type, x$item_id)
}

#' Summarise an sf_check object
#'
#' @param object An object of class `sf_check`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return `object`, invisibly.
#' @exportS3Method summary sf_check
summary.sf_check <- function(object, ...) {
  cat(format(object), "\n")
  cat(sprintf("  fail action: %s\n", object$fail_action %||% "none"))
  invisible(object)
}

# sf_model -------------------------------------------------------------------

#' Print an sf_model object
#'
#' @param x An object of class `sf_model`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return `x`, invisibly.
#' @exportS3Method print sf_model
print.sf_model <- function(x, ...) {
  n_con <- length(x$measurement$constructs %||% list())
  cat(sprintf("<sf_model: %s | type: %s | %d construct(s)>\n",
              x$id, x$type, n_con))
  cat(sprintf("  Label: %s\n", x$label))
  cat(sprintf("  Engine: %s\n", x$engine %||% "lavaan"))
  invisible(x)
}

#' Format an sf_model object as a string
#'
#' @param x An object of class `sf_model`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return A single character string.
#' @exportS3Method format sf_model
format.sf_model <- function(x, ...) {
  n_con <- length(x$measurement$constructs %||% list())
  sprintf("<sf_model: %s | type: %s | %d construct(s)>", x$id, x$type, n_con)
}

#' Summarise an sf_model object
#'
#' @param object An object of class `sf_model`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return `object`, invisibly.
#' @exportS3Method summary sf_model
summary.sf_model <- function(object, ...) {
  cat(format(object), "\n")
  n_paths <- length(object$structural$paths %||% list())
  cat(sprintf("  engine: %s | structural paths: %d\n",
              object$engine %||% "lavaan", n_paths))
  invisible(object)
}
