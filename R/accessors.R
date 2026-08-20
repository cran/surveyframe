# accessors.R
# Dedicated accessor and exploration methods for the surveyframe classes.
#
# Before 0.4.0 the only way to reach the contents of an instrument or a report
# was `$` on the underlying list. That makes the internal layout part of the
# public contract and gives a user no discoverable way in. These generics and
# the as.data.frame() methods in as_data_frame.R are the supported route.

# ---------------------------------------------------------------------------
# Component lists
# ---------------------------------------------------------------------------

# Components are returned as a classed list so they print as a readable
# summary rather than dumping their internals. Names are the component ids,
# so `sf_items(instr)[["sat_1"]]` is the lookup path.
sframe_component_list <- function(x, what = "component") {
  ids <- vapply(x, function(el) as.character(el$id %||% "")[1], character(1))
  names(x) <- ids
  structure(x, class = "sf_component_list", what = what)
}

#' A list of instrument components
#'
#' The value returned by [sf_items()], [sf_scales()], [sf_choice_sets()],
#' [sf_branches()], [sf_checks()] and [sf_models()]. It is a list of component
#' objects named by their IDs, so a single component is reached with `[[`.
#'
#' @param x An `sf_component_list`.
#' @param ... Ignored. Present for S3 consistency.
#'
#' @return `print()` returns `x` invisibly. `[` returns an `sf_component_list`.
#' @name sf_component_list
#'
#' @examples
#' item1 <- sf_item("q1", "First question", type = "text")
#' item2 <- sf_item("q2", "Second question", type = "text")
#' instr <- sf_instrument("Demo", components = list(item1, item2))
#'
#' sf_items(instr)
#' sf_items(instr)[["q2"]]
NULL

#' @rdname sf_component_list
#' @exportS3Method print sf_component_list
print.sf_component_list <- function(x, ...) {
  what <- attr(x, "what") %||% "component"
  cat(sprintf("<%s list: %d>\n", what, length(x)))
  for (el in x) {
    # format() is already defined for every component class, so the one-line
    # summary is theirs rather than a second description that could drift.
    line <- tryCatch(format(el), error = function(e) paste0("<", class(el)[1], ">"))
    cat(" ", line, "\n", sep = "")
  }
  invisible(x)
}

#' @rdname sf_component_list
#' @param i Index, name, or logical vector selecting components.
#' @exportS3Method `[` sf_component_list
`[.sf_component_list` <- function(x, i, ...) {
  what <- attr(x, "what")
  out <- NextMethod()
  structure(out, class = "sf_component_list", what = what)
}

# ---------------------------------------------------------------------------
# Generics
# ---------------------------------------------------------------------------

#' Explore a surveyframe object
#'
#' Accessors for the parts of an instrument, a codebook, or a report. They
#' replace reaching into the object with `$`, which ties user code to the
#' internal layout.
#'
#' `sf_items()`, `sf_scales()`, `sf_choice_sets()`, `sf_branches()`,
#' `sf_checks()` and `sf_models()` return the component objects as an
#' [sf_component_list]. `sf_meta()` returns the metadata as a list and
#' `sf_plan()` returns the pre-declared analysis plan. For a flat table of the
#' same content, call `as.data.frame()` on the object instead.
#'
#' @param x A surveyframe object.
#' @param ... Passed to methods.
#'
#' @return `sf_items()`, `sf_scales()`, `sf_choice_sets()`, `sf_branches()`,
#'   `sf_checks()` and `sf_models()` return an [sf_component_list].
#'   `sf_meta()` and `sf_plan()` return lists.
#' @name sf_accessors
#' @seealso [as_sframe()], [sf_problems()], [sframe_validation]
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
#' sf_meta(instr)$title
#' sf_items(instr)
#' sf_scales(instr)[["sat"]]
#' as.data.frame(instr)
NULL

#' @rdname sf_accessors
#' @export
sf_meta <- function(x, ...) UseMethod("sf_meta")

#' @rdname sf_accessors
#' @export
sf_items <- function(x, ...) UseMethod("sf_items")

#' @rdname sf_accessors
#' @export
sf_scales <- function(x, ...) UseMethod("sf_scales")

#' @rdname sf_accessors
#' @export
sf_choice_sets <- function(x, ...) UseMethod("sf_choice_sets")

#' @rdname sf_accessors
#' @export
sf_branches <- function(x, ...) UseMethod("sf_branches")

#' @rdname sf_accessors
#' @export
sf_checks <- function(x, ...) UseMethod("sf_checks")

#' @rdname sf_accessors
#' @export
sf_models <- function(x, ...) UseMethod("sf_models")

#' @rdname sf_accessors
#' @export
sf_plan <- function(x, ...) UseMethod("sf_plan")

# ---------------------------------------------------------------------------
# sframe methods
# ---------------------------------------------------------------------------

#' @rdname sf_accessors
#' @exportS3Method sf_meta sframe
sf_meta.sframe <- function(x, ...) x$meta

#' @rdname sf_accessors
#' @exportS3Method sf_items sframe
sf_items.sframe <- function(x, ...) sframe_component_list(x$items, "item")

#' @rdname sf_accessors
#' @exportS3Method sf_scales sframe
sf_scales.sframe <- function(x, ...) sframe_component_list(x$scales, "scale")

#' @rdname sf_accessors
#' @exportS3Method sf_choice_sets sframe
sf_choice_sets.sframe <- function(x, ...) {
  sframe_component_list(x$choices, "choice set")
}

#' @rdname sf_accessors
#' @exportS3Method sf_branches sframe
sf_branches.sframe <- function(x, ...) {
  sframe_component_list(x$branching %||% list(), "branch rule")
}

#' @rdname sf_accessors
#' @exportS3Method sf_checks sframe
sf_checks.sframe <- function(x, ...) {
  sframe_component_list(x$checks %||% list(), "attention check")
}

#' @rdname sf_accessors
#' @exportS3Method sf_models sframe
sf_models.sframe <- function(x, ...) {
  sframe_component_list(x$models %||% list(), "model")
}

#' @rdname sf_accessors
#' @exportS3Method sf_plan sframe
sf_plan.sframe <- function(x, ...) x$analysis_plan %||% list()

#' Set the pre-declared analysis plan
#'
#' The replacement counterpart to [sf_plan()]. Declaring the plan is the step
#' the whole workflow turns on, so it has a named function rather than
#' assignment into the object's internals.
#'
#' @param x An `sframe` object.
#' @param value A list of analysis blocks.
#'
#' @return The updated `sframe` object.
#' @export
#' @seealso [sf_plan()], [validate_sframe()], [run_analysis_plan()]
#'
#' @examples
#' item  <- sf_item("q1", "How satisfied are you?", type = "numeric")
#' instr <- sf_instrument("Demo", components = list(item))
#'
#' sf_plan(instr) <- list(
#'   list(id = "RQ1", research_question = "What is the average?",
#'        family = "descriptive", method = "descriptives",
#'        roles = list(variables = "q1"))
#' )
#' length(sf_plan(instr))
`sf_plan<-` <- function(x, value) UseMethod("sf_plan<-")

#' @rdname sf_plan-set
#' @exportS3Method `sf_plan<-` sframe
`sf_plan<-.sframe` <- function(x, value) {
  if (!is.list(value)) {
    rlang::abort("An analysis plan must be a list of blocks.",
                 class = "sframe_error")
  }
  x$analysis_plan <- value
  x
}

# ---------------------------------------------------------------------------
# Codebook methods. A codebook is the tabular view of an instrument, so the
# same verbs answer for it and return the tables it already holds.
# ---------------------------------------------------------------------------

#' @rdname sf_accessors
#' @exportS3Method sf_meta sframe_codebook
sf_meta.sframe_codebook <- function(x, ...) x$instrument_meta

#' @rdname sf_accessors
#' @exportS3Method sf_items sframe_codebook
sf_items.sframe_codebook <- function(x, ...) x$items_table

#' @rdname sf_accessors
#' @exportS3Method sf_scales sframe_codebook
sf_scales.sframe_codebook <- function(x, ...) x$scales_table

#' @rdname sf_accessors
#' @exportS3Method sf_choice_sets sframe_codebook
sf_choice_sets.sframe_codebook <- function(x, ...) x$choices_table

#' @rdname sf_accessors
#' @exportS3Method sf_models sframe_codebook
sf_models.sframe_codebook <- function(x, ...) x$models_table

#' @rdname sf_accessors
#' @exportS3Method sf_plan sframe_codebook
sf_plan.sframe_codebook <- function(x, ...) x$plan_table

# ---------------------------------------------------------------------------
# Component identity
# ---------------------------------------------------------------------------

#' The ID and label of an instrument component
#'
#' @param x An [sf_item()], [sf_choices()], [sf_scale()], [sf_branch()],
#'   [sf_check()] or [sf_model()] object.
#' @param ... Passed to methods.
#'
#' @return A single character string. `sf_label()` returns `""` when the
#'   component carries no label.
#' @name sf_identity
#'
#' @examples
#' item <- sf_item("q1", "How satisfied are you?", type = "likert",
#'                 choice_set = "agree5")
#' sf_id(item)
#' sf_label(item)
NULL

#' @rdname sf_identity
#' @export
sf_id <- function(x, ...) UseMethod("sf_id")

#' @rdname sf_identity
#' @export
sf_label <- function(x, ...) UseMethod("sf_label")

sframe_component_id <- function(x, ...) as.character(x$id %||% "")[1]
sframe_component_label <- function(x, ...) as.character(x$label %||% "")[1]

#' @rdname sf_identity
#' @exportS3Method sf_id sf_item
sf_id.sf_item <- sframe_component_id
#' @rdname sf_identity
#' @exportS3Method sf_id sf_choices
sf_id.sf_choices <- sframe_component_id
#' @rdname sf_identity
#' @exportS3Method sf_id sf_scale
sf_id.sf_scale <- sframe_component_id
#' @rdname sf_identity
#' @exportS3Method sf_id sf_branch
sf_id.sf_branch <- sframe_component_id
#' @rdname sf_identity
#' @exportS3Method sf_id sf_check
sf_id.sf_check <- sframe_component_id
#' @rdname sf_identity
#' @exportS3Method sf_id sf_model
sf_id.sf_model <- sframe_component_id

#' @rdname sf_identity
#' @exportS3Method sf_label sf_item
sf_label.sf_item <- sframe_component_label
#' @rdname sf_identity
#' @exportS3Method sf_label sf_choices
sf_label.sf_choices <- sframe_component_label
#' @rdname sf_identity
#' @exportS3Method sf_label sf_scale
sf_label.sf_scale <- sframe_component_label
#' @rdname sf_identity
#' @exportS3Method sf_label sf_branch
sf_label.sf_branch <- sframe_component_label
#' @rdname sf_identity
#' @exportS3Method sf_label sf_check
sf_label.sf_check <- sframe_component_label
#' @rdname sf_identity
#' @exportS3Method sf_label sf_model
sf_label.sf_model <- sframe_component_label

# ---------------------------------------------------------------------------
# Validation accessors
# ---------------------------------------------------------------------------

#' Read a validation diagnostic
#'
#' `sf_is_valid()` reports whether the object passed. `sf_problems()` returns
#' the problem messages. `sf_object()` returns the object that was validated.
#'
#' @param x An [sframe_validation] object.
#' @param ... Passed to methods.
#'
#' @return `sf_is_valid()` returns a single logical. `sf_problems()` returns a
#'   character vector, empty when the object is valid. `sf_object()` returns
#'   the validated object.
#' @name sf_validation_accessors
#' @seealso [validate_sframe()], [sframe_validation], [as_sframe()]
#'
#' @examples
#' item  <- sf_item("q1", "How satisfied are you?", type = "text")
#' instr <- sf_instrument("Demo", components = list(item))
#' v <- validate_sframe(instr, strict = FALSE)
#'
#' sf_is_valid(v)
#' sf_problems(v)
NULL

#' @rdname sf_validation_accessors
#' @export
sf_is_valid <- function(x, ...) UseMethod("sf_is_valid")

#' @rdname sf_validation_accessors
#' @export
sf_problems <- function(x, ...) UseMethod("sf_problems")

#' @rdname sf_validation_accessors
#' @export
sf_object <- function(x, ...) UseMethod("sf_object")

#' @rdname sf_validation_accessors
#' @exportS3Method sf_is_valid sframe_validation
sf_is_valid.sframe_validation <- function(x, ...) isTRUE(x$valid)

#' @rdname sf_validation_accessors
#' @exportS3Method sf_problems sframe_validation
sf_problems.sframe_validation <- function(x, ...) as.character(x$problems)

#' @rdname sf_validation_accessors
#' @exportS3Method sf_object sframe_validation
sf_object.sframe_validation <- function(x, ...) x$object

# ---------------------------------------------------------------------------
# Coercion
# ---------------------------------------------------------------------------

#' Coerce to an instrument
#'
#' Recovers the `sframe` instrument from a validation diagnostic. This is the
#' migration path for code that used the `strict = TRUE` return of
#' [validate_sframe()] as an instrument, which it no longer is.
#'
#' @param x An [sframe_validation] object or an `sframe`.
#' @param ... Passed to methods.
#'
#' @return An `sframe` object. When the validation passed, its
#'   `meta$validated` is `TRUE`.
#' @export
#' @seealso [validate_sframe()], [sframe_validation]
#'
#' @examples
#' item  <- sf_item("q1", "How satisfied are you?", type = "text")
#' instr <- sf_instrument("Demo", components = list(item))
#'
#' validated <- as_sframe(validate_sframe(instr, strict = TRUE))
#' isTRUE(sf_meta(validated)$validated)
as_sframe <- function(x, ...) UseMethod("as_sframe")

#' @rdname as_sframe
#' @exportS3Method as_sframe sframe
as_sframe.sframe <- function(x, ...) x

#' @rdname as_sframe
#' @exportS3Method as_sframe sframe_validation
as_sframe.sframe_validation <- function(x, ...) {
  if (!identical(x$subject, "instrument")) {
    rlang::abort(
      paste0(
        "This validation result describes a ", x$subject,
        ", so it holds no instrument. Use `sf_object()` to get the ",
        x$subject, " back."
      ),
      class = "sframe_error"
    )
  }
  x$object
}

# ---------------------------------------------------------------------------
# Report accessors
# ---------------------------------------------------------------------------

#' Read the reportable parts of an analysis or quality result
#'
#' `sf_apa()` returns the APA-formatted sentence for each analysis block.
#' `sf_flagged()` returns the row numbers a quality report flagged.
#'
#' @param x An `sframe_analysis_results` object for `sf_apa()`, or an
#'   `sframe_quality_report` for `sf_flagged()`.
#' @param ... Passed to methods.
#'
#' @return `sf_apa()` returns a named character vector, one element per
#'   analysis block. `sf_flagged()` returns an integer vector of row numbers.
#' @name sf_report_accessors
#'
#' @examples
#' demo <- sframe_demo_data()
#' qr <- quality_report(demo$responses, demo$instrument)
#' head(sf_flagged(qr))
NULL

#' @rdname sf_report_accessors
#' @export
sf_apa <- function(x, ...) UseMethod("sf_apa")

#' @rdname sf_report_accessors
#' @export
sf_flagged <- function(x, ...) UseMethod("sf_flagged")

#' @rdname sf_report_accessors
#' @exportS3Method sf_apa sframe_analysis_results
sf_apa.sframe_analysis_results <- function(x, ...) {
  out <- vapply(x, function(r) as.character(r$apa %||% "")[1], character(1))
  stats::setNames(out, names(x))
}

# The statistics reports each carry a single APA sentence, so one method
# body serves them all.
sframe_report_apa <- function(x, ...) as.character(x$apa %||% "")[1]

#' @rdname sf_report_accessors
#' @exportS3Method sf_apa sframe_descriptives_report
sf_apa.sframe_descriptives_report <- sframe_report_apa

#' @rdname sf_report_accessors
#' @exportS3Method sf_apa sframe_missing_data_report
sf_apa.sframe_missing_data_report <- sframe_report_apa

#' @rdname sf_report_accessors
#' @exportS3Method sf_apa sframe_validity_report
sf_apa.sframe_validity_report <- sframe_report_apa

#' @rdname sf_report_accessors
#' @exportS3Method sf_apa sframe_assumption_report
sf_apa.sframe_assumption_report <- sframe_report_apa

#' @rdname sf_report_accessors
#' @exportS3Method sf_flagged sframe_quality_report
sf_flagged.sframe_quality_report <- function(x, ...) {
  rows <- unique(c(
    unlist(lapply(x$attention, function(a) a$failed_rows), use.names = FALSE),
    x$timing$flagged_rows,
    x$missing$flagged_rows,
    unlist(lapply(x$straightline, function(s) s$flagged_rows), use.names = FALSE),
    x$duplicates$flagged_rows
  ))
  sort(as.integer(rows))
}
