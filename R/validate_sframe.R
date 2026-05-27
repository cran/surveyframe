# validate_sframe.R

#' Validate an instrument object
#'
#' Checks the internal consistency of an `sframe` instrument object and
#' reports all detected problems. Validation is performed automatically by
#' [write_sframe()] and optionally by [read_sframe()]. It can also be run
#' independently at any point during instrument construction.
#'
#' The following checks are performed:
#' - Duplicate item IDs
#' - Invalid item IDs
#' - Duplicate choice-set IDs
#' - Duplicate scale IDs
#' - Items with missing labels
#' - Items referencing a missing `choice_set` in the instrument
#' - Items referencing a missing `scale_id` in the instrument
#' - Items marked `reverse = TRUE` without a `scale_id`
#' - Choice sets referenced by items but not present in the instrument
#' - Scale `items` vectors containing IDs not present in the instrument
#' - Branching rules referencing item IDs not present in the instrument
#' - Attention checks referencing item IDs not present in the instrument
#' - Analysis plan roles referencing missing variables or models
#' - Model specifications referencing missing indicators or constructs
#'
#' @param instrument An `sframe` object created by [sf_instrument()].
#' @param strict Logical. When `TRUE` (default), any detected problem raises
#'   an error of class `sframe_validation_error`. When `FALSE`, problems are
#'   returned as a character vector of messages without stopping.
#'
#' @return When `strict = TRUE` and the instrument is valid, the instrument
#'   is returned invisibly with `meta$validated` set to `TRUE`. When
#'   `strict = FALSE`, a named list with elements `valid` (logical) and
#'   `problems` (character vector) is returned.
#' @export
#' @seealso [sf_instrument()], [write_sframe()]
#'
#' @examples
#' # Build a minimal valid instrument and validate it
#' cs    <- sf_choices("ag5", 1:5,
#'            c("Strongly disagree", "Disagree", "Neutral",
#'              "Agree", "Strongly agree"))
#' item  <- sf_item("sat_1", "The service met my expectations.",
#'                  type = "likert", choice_set = "ag5", scale_id = "sat")
#' scale <- sf_scale("sat", "Satisfaction", items = "sat_1")
#' instr <- sf_instrument("Demo Survey", components = list(cs, item, scale))
#'
#' # Non-strict: returns a list without stopping
#' result <- validate_sframe(instr, strict = FALSE)
#' result$valid
#' result$problems
#'
#' # Strict: returns instrument invisibly when valid
#' validated <- validate_sframe(instr, strict = TRUE)
#' isTRUE(validated$meta$validated)
validate_sframe <- function(instrument, strict = TRUE) {
  stopifnot(inherits(instrument, "sframe"))

  problems <- character(0)

  item_ids    <- vapply(instrument$items,    function(x) x$id, character(1))
  choice_ids  <- vapply(instrument$choices,  function(x) x$id, character(1))
  scale_ids   <- vapply(instrument$scales,   function(x) x$id, character(1))
  model_ids   <- vapply(instrument$models %||% list(), function(x) x$id %||% "", character(1))
  valid_id <- function(x) grepl("^[A-Za-z][A-Za-z0-9_]*$", x)
  known_vars <- unique(c(item_ids, scale_ids))

  # Duplicate item IDs
  dupes <- item_ids[duplicated(item_ids)]
  if (length(dupes) > 0) {
    problems <- c(problems,
      paste0("Duplicate item IDs: ", paste(dupes, collapse = ", ")))
  }

  bad_item_ids <- item_ids[!valid_id(item_ids)]
  if (length(bad_item_ids) > 0) {
    problems <- c(
      problems,
      paste0(
        "Invalid item ID(s): ",
        paste(unique(bad_item_ids), collapse = ", "),
        ". IDs must start with a letter and contain only letters, numbers, and `_` characters."
      )
    )
  }

  dup_choice_ids <- choice_ids[duplicated(choice_ids)]
  if (length(dup_choice_ids) > 0) {
    problems <- c(
      problems,
      paste0("Duplicate choice set IDs: ", paste(unique(dup_choice_ids), collapse = ", "))
    )
  }

  dup_scale_ids <- scale_ids[duplicated(scale_ids)]
  if (length(dup_scale_ids) > 0) {
    problems <- c(
      problems,
      paste0("Duplicate scale IDs: ", paste(unique(dup_scale_ids), collapse = ", "))
    )
  }

  for (item in instrument$items) {
    # Missing labels
    if (is.null(item$label) || nchar(trimws(item$label)) == 0) {
      problems <- c(problems,
        paste0("Item '", item$id, "' has an empty label."))
    }
    # Orphan choice set references
    if (!is.null(item$choice_set) && !item$choice_set %in% choice_ids) {
      problems <- c(problems,
        paste0("Item '", item$id, "' references choice_set '",
               item$choice_set, "' which is missing from the instrument."))
    }
    # Orphan scale references
    if (!is.null(item$scale_id) && !item$scale_id %in% scale_ids) {
      problems <- c(problems,
        paste0("Item '", item$id, "' references scale_id '",
               item$scale_id, "' which is missing from the instrument."))
    }
    # Reverse coded without scale
    if (isTRUE(item$reverse) && is.null(item$scale_id)) {
      problems <- c(problems,
        paste0("Item '", item$id,
               "' is reverse = TRUE but has no scale_id."))
    }
  }

  # Scale item membership
  for (scale in instrument$scales) {
    missing_items <- setdiff(scale$items, item_ids)
    if (length(missing_items) > 0) {
      problems <- c(problems,
        paste0("Scale '", scale$id, "' references unknown item(s): ",
               paste(missing_items, collapse = ", ")))
    }
  }

  # Branching rule integrity
  for (rule in instrument$branching) {
    if (!rule$item_id %in% item_ids) {
      problems <- c(problems,
        paste0("Branch rule targets unknown item '", rule$item_id, "'."))
    }
    if (!rule$depends_on %in% item_ids) {
      problems <- c(problems,
        paste0("Branch rule depends_on unknown item '",
               rule$depends_on, "'."))
    }
  }

  # Check item references
  check_item_ids <- vapply(instrument$checks,
                           function(x) x$item_id, character(1))
  missing_check_items <- setdiff(check_item_ids, item_ids)
  if (length(missing_check_items) > 0) {
    problems <- c(problems,
      paste0("Check(s) reference unknown item(s): ",
             paste(missing_check_items, collapse = ", ")))
  }

  # Analysis plan references. Old plans use `variables`; v0.3 plans use
  # role-based assignments, but both formats must remain valid.
  for (block in instrument$analysis_plan %||% list()) {
    block_id <- block$id %||% "(unnamed)"
    block_method <- as.character(block$method %||% block$test %||% "")
    refs <- character(0)
    model_ref_values <- character(0)
    if (!is.null(block$variables)) {
      variable_refs <- as.character(unlist(block$variables, use.names = FALSE))
      if (block_method %in% c("cfa_lavaan_syntax", "sem_lavaan_syntax", "seminr_syntax")) {
        model_ref_values <- c(model_ref_values, variable_refs)
      } else {
        refs <- c(refs, variable_refs)
      }
    }
    if (!is.null(block$roles) && is.list(block$roles)) {
      role_refs <- unlist(block$roles, recursive = TRUE, use.names = FALSE)
      role_refs <- as.character(role_refs[!is.na(role_refs)])
      model_roles <- c("model", "models", "measurement_model", "structural_model")
      role_names <- names(block$roles) %||% character(0)
      model_ref_values <- c(model_ref_values, unlist(block$roles[intersect(role_names, model_roles)],
                                                     recursive = TRUE, use.names = FALSE))
      model_ref_values <- as.character(model_ref_values[!is.na(model_ref_values)])
      data_refs <- setdiff(role_refs, model_ref_values)
      refs <- c(refs, data_refs)
    }
    model_ref_values <- model_ref_values[nzchar(model_ref_values)]
    missing_models <- setdiff(unique(model_ref_values), model_ids)
    if (length(missing_models) > 0) {
      problems <- c(
        problems,
        paste0(
          "Analysis plan '", block_id, "' references missing model(s): ",
          paste(unique(missing_models), collapse = ", ")
        )
      )
    }
    refs <- refs[nzchar(refs)]
    missing_refs <- setdiff(unique(refs), known_vars)
    if (length(missing_refs) > 0) {
      problems <- c(
        problems,
        paste0(
          "Analysis plan '", block_id, "' references unknown variable(s): ",
          paste(missing_refs, collapse = ", ")
        )
      )
    }
  }

  # Model layer integrity.
  dup_model_ids <- model_ids[nzchar(model_ids) & duplicated(model_ids)]
  if (length(dup_model_ids) > 0) {
    problems <- c(
      problems,
      paste0("Duplicate model IDs: ", paste(unique(dup_model_ids), collapse = ", "))
    )
  }
  bad_model_ids <- model_ids[nzchar(model_ids) & !valid_id(model_ids)]
  if (length(bad_model_ids) > 0) {
    problems <- c(
      problems,
      paste0("Invalid model ID(s): ", paste(unique(bad_model_ids), collapse = ", "))
    )
  }
  for (model in instrument$models %||% list()) {
    model_check <- tryCatch(
      validate_model(model, instrument = instrument, strict = FALSE),
      error = function(e) list(valid = FALSE, problems = conditionMessage(e))
    )
    if (!isTRUE(model_check$valid)) {
      problems <- c(
        problems,
        paste0(
          "Model '", model$id %||% "(unnamed)", "': ",
          model_check$problems
        )
      )
    }
  }

  if (strict && length(problems) > 0) {
    sframe_abort_validation(
      paste0(
        "Instrument validation failed with ",
        length(problems),
        " problem(s):\n",
        paste0("  - ", problems, collapse = "\n")
      ),
      instrument_title = instrument$meta$title
    )
  }

  if (!strict) {
    return(list(valid = length(problems) == 0, problems = problems))
  }

  # Mark as validated and return invisibly
  instrument$meta$validated <- TRUE
  invisible(instrument)
}
