# sframe_methods.R
# S3 methods for the sframe class.

#' Print an sframe instrument object
#'
#' Displays a compact summary of an `sframe` instrument object, showing
#' the title, version, item count, scale count, and validation status.
#'
#' @param x An object of class `sframe`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return `x`, invisibly.
#' @exportS3Method print sframe
#' @examples
#' item <- sf_item("q1", "How satisfied are you?", type = "likert",
#'                 choice_set = "agree5")
#' instr <- sf_instrument("My Survey", components = list(item))
#' print(instr)
print.sframe <- function(x, ...) {
  n_items  <- length(x$items)
  n_scales <- length(x$scales)
  status   <- if (isTRUE(x$meta$validated)) "valid" else "not validated"

  cat(
    sprintf(
      "<sframe>\n  Title:      %s\n  Version:    %s\n  Items:      %d\n  Scales:     %d\n  Status:     %s\n",
      x$meta$title,
      x$meta$version,
      n_items,
      n_scales,
      status
    )
  )
  invisible(x)
}

#' Format an sframe instrument object as a string
#'
#' @param x An object of class `sframe`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return A single character string.
#' @exportS3Method format sframe
format.sframe <- function(x, ...) {
  sprintf(
    "<sframe: %s v%s | %d items | %d scales>",
    x$meta$title,
    x$meta$version,
    length(x$items),
    length(x$scales)
  )
}

#' Summarise an sframe instrument object
#'
#' Prints a structured summary of an `sframe` object including metadata,
#' item type counts, scale definitions, branching rules, and check
#' specifications.
#'
#' @param object An object of class `sframe`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return `object`, invisibly.
#' @exportS3Method summary sframe
#' @examples
#' item <- sf_item("q1", "How satisfied are you?", type = "likert",
#'                 choice_set = "agree5")
#' instr <- sf_instrument("My Survey", components = list(item))
#' summary(instr)
summary.sframe <- function(object, ...) {
  types <- vapply(object$items, function(i) i$type, character(1))
  type_counts <- sort(table(types), decreasing = TRUE)

  cat(sprintf("Survey Instrument: %s\n", object$meta$title))
  cat(sprintf("Version:           %s\n", object$meta$version))
  if (!is.null(object$meta$description)) {
    cat(sprintf("Description:       %s\n", object$meta$description))
  }
  cat(sprintf("Languages:         %s\n",
              paste(object$meta$languages, collapse = ", ")))
  cat("\nItems:\n")
  for (nm in names(type_counts)) {
    cat(sprintf("  %-20s %d\n", nm, type_counts[[nm]]))
  }
  cat(sprintf("  %-20s %d\n", "TOTAL", length(object$items)))
  cat(sprintf("\nScales:    %d\n", length(object$scales)))
  cat(sprintf("Branches:  %d\n", length(object$branching)))
  cat(sprintf("Checks:    %d\n", length(object$checks)))
  cat(sprintf("Status:    %s\n",
              if (isTRUE(object$meta$validated)) "valid" else "not validated"))
  invisible(object)
}
