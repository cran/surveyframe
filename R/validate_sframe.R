# validate_sframe.R

# A field every check below reads as a length-1 string. Anything else has to
# be reported, because the checks reach it through vapply(..., character(1))
# and an ordinary R error there leaves no diagnostic at all.
sframe_scalar_problem <- function(value, what, allow_empty = FALSE,
                                  require_character = TRUE) {
  if (is.null(value)) return(paste0(what, " is missing."))
  if (!is.atomic(value)) {
    return(paste0(what, " must be one value, and is ", class(value)[1], "."))
  }
  if (length(value) != 1) {
    return(paste0(what, " must be one value, and has length ",
                  length(value), "."))
  }
  if (is.na(value)) return(paste0(what, " is NA."))
  # Every check downstream reads these through vapply(..., character(1)), so a
  # numeric id passed the old length-and-NA test and then failed there with an
  # ordinary R type error instead of a diagnostic.
  if (require_character && !is.character(value)) {
    return(paste0(what, " must be text, and is ", class(value)[1],
                  ". Quote it."))
  }
  if (!allow_empty && !nzchar(as.character(value))) {
    return(paste0(what, " is empty."))
  }
  NULL
}

sframe_field_shape_problems <- function(instrument) {
  out <- character(0)
  keep <- function(p) if (!is.null(p)) out <<- c(out, p)
  # A component that is no list cannot be read with $ at all, so it is reported
  # here and its fields are left alone.
  component_problem <- function(x, what) {
    if (!is.list(x)) {
      return(paste0(what, " must be a list of fields, and is ",
                    class(x)[1], "."))
    }
    NULL
  }
  for (i in seq_along(instrument$items)) {
    it <- instrument$items[[i]]
    bad <- component_problem(it, paste0("Item ", i))
    if (!is.null(bad)) { keep(bad); next }
    keep(sframe_scalar_problem(it$id, paste0("Item ", i, "'s id")))
    keep(sframe_scalar_problem(it$label, paste0("Item ", i, "'s label"),
                               allow_empty = TRUE))
    keep(sframe_scalar_problem(it$type, paste0("Item ", i, "'s type")))
  }
  for (i in seq_along(instrument$choices)) {
    bad <- component_problem(instrument$choices[[i]],
                             paste0("Choice set ", i))
    if (!is.null(bad)) { keep(bad); next }
    keep(sframe_scalar_problem(instrument$choices[[i]]$id,
                               paste0("Choice set ", i, "'s id")))
  }
  for (i in seq_along(instrument$scales)) {
    bad <- component_problem(instrument$scales[[i]], paste0("Scale ", i))
    if (!is.null(bad)) { keep(bad); next }
    keep(sframe_scalar_problem(instrument$scales[[i]]$id,
                               paste0("Scale ", i, "'s id")))
  }
  # A plan block that cannot be read, and a branch or check the same, belong
  # here too: the reference walk below dereferences all of them.
  for (i in seq_along(instrument$analysis_plan)) {
    keep(component_problem(instrument$analysis_plan[[i]],
                           paste0("Analysis plan block ", i)))
  }
  for (i in seq_along(instrument$branching)) {
    keep(component_problem(instrument$branching[[i]],
                           paste0("Branching rule ", i)))
  }
  for (i in seq_along(instrument$checks)) {
    keep(component_problem(instrument$checks[[i]], paste0("Check ", i)))
  }
  for (i in seq_along(instrument$models)) {
    keep(component_problem(instrument$models[[i]], paste0("Model ", i)))
  }
  keep(sframe_scalar_problem(instrument$meta$title, "The instrument title"))
  # The format requires meta.title and leaves meta.version optional, so an
  # absent version is a legal minimal file. A version that IS supplied still
  # has to be one string: a vector one used to reach the diagnostic's own
  # printing and fail there.
  if (!is.null(instrument$meta$version)) {
    keep(sframe_scalar_problem(instrument$meta$version,
                               "The instrument version"))
  }
  out
}

# Title and version reach the diagnostic's own printing, which reads them as
# single values. A malformed one is already reported, so a placeholder here
# keeps the report readable.
sframe_meta_display <- function(value, fallback) {
  if (length(value) == 1 && !is.na(value) && nzchar(as.character(value))) {
    return(as.character(value))
  }
  fallback
}

# Whether a method takes its variables from the block's roles at all. The
# instrument-level methods read the whole instrument instead: a reliability or
# quality block names no variable and is right not to. Probing the resolver
# with every role name it knows separates the two without a hand-kept list
# that would drift as methods are added.
sframe_method_needs_variables <- function(method) {
  bag <- list(
    variables = "x", variable = "x", items = "x", scales = "x",
    group = "g", outcome = "o", dependent = "o", x = "a", y = "b",
    item = "i", row = "r", column = "c", predictors = "p",
    before = "b", after = "a2", measures = c("m1", "m2"),
    weights_item = "w", performance_items = "pi", mediator = "m",
    moderator = "mo", controls = "c2", covariates = "c3"
  )
  resolved <- tryCatch(
    sframe_vars_for_method(method, bag, list(method = method)),
    error = function(e) character(0))
  length(resolved) > 0
}

# The shape a plan block has to have before its references mean anything.
# Reading references alone let analysis_plan = list(list()) validate clean,
# and let roles = "q1" skip reference checking because it was no list.
sframe_plan_block_problems <- function(plan, known_methods) {
  out <- character(0)
  keep <- function(p) out <<- c(out, p)
  ids <- character(0)
  for (i in seq_along(plan)) {
    block <- plan[[i]]
    where <- paste0("Analysis plan block ", i)
    if (!is.list(block) || length(block) == 0) {
      keep(paste0(where, " is empty or is no list of fields."))
      next
    }
    id <- block$id %||% NULL
    p <- sframe_scalar_problem(id, paste0(where, "'s id"))
    if (is.null(p)) {
      where <- paste0("Analysis plan '", as.character(id)[1], "'")
      ids <- c(ids, as.character(id)[1])
    } else {
      keep(p)
    }
    method <- block$method %||% block$test %||% NULL
    method_name <- ""
    p <- sframe_scalar_problem(method, paste0(where, "'s method"))
    if (is.null(p)) {
      method <- as.character(method)[1]
      method_name <- method
      if (!method %in% known_methods) {
        keep(paste0(where, " names the unknown method '", method,
                    "'. See ?run_analysis_plan for the methods available."))
      }
    } else {
      keep(p)
    }
    roles_ok <- is.list(block$roles) && length(unlist(block$roles)) > 0
    vars_ok <- length(unlist(block$variables)) > 0
    if (!is.null(block$roles) && !is.list(block$roles)) {
      keep(paste0(where, "'s roles must be a named list of role assignments."))
    }
    # An empty roles list is no assignment, and it passed the old presence
    # test. Only the methods that read variables from roles need one: a
    # reliability, quality or item-diagnostics block works from the whole
    # instrument and names no variable.
    if (!roles_ok && !vars_ok && nzchar(method_name) &&
        sframe_method_needs_variables(method_name)) {
      keep(paste0(where, " assigns no variables. Give it roles, or the legacy ",
                  "variables field."))
    }
  }
  dup <- unique(ids[duplicated(ids)])
  if (length(dup) > 0) {
    keep(paste0("Duplicate analysis plan block IDs: ",
                paste(dup, collapse = ", "), ". A block ID has to be unique, ",
                "since results are keyed by it."))
  }
  out
}

# The full roster of checks, in the order they run. Every one appears in the
# returned diagnostic whether or not it found anything, so a user can tell a
# check that passed from a check that never ran.
sframe_validation_checks <- c(
  "field_shapes",
  "duplicate_item_ids",
  "item_id_format",
  "duplicate_choice_ids",
  "choice_set_contents",
  "duplicate_scale_ids",
  "id_namespace",
  "item_config",
  "item_labels",
  "item_choice_set_refs",
  "item_scale_refs",
  "reverse_without_scale",
  "decision_item_shape",
  "comparison_scale",
  "scale_membership",
  "scale_parameters",
  "reverse_item_membership",
  "branching_refs",
  "branching_values",
  "check_refs",
  "duplicate_check_ids",
  "branch_targets",
  "analysis_plan_blocks",
  "analysis_plan_models",
  "analysis_plan_variables",
  "decision_scale_compatibility",
  "model_ids",
  "model_specs"
)

#' Validate an instrument object
#'
#' Checks the internal consistency of an `sframe` instrument object and
#' returns a diagnostic result. Validation is performed automatically by
#' [write_sframe()] and optionally by [read_sframe()]. It can also be run
#' independently at any point during instrument construction.
#'
#' The following checks are performed:
#' - Duplicate item IDs
#' - Invalid item IDs
#' - Duplicate choice-set IDs
#' - Duplicate scale IDs
#' - Item IDs, scale IDs and response columns that share a name, or take a
#'   reserved metadata name such as `submitted_at`. A scale's score is stored
#'   in a column named by its ID, so a shared name would overwrite data
#' - Items with missing labels
#' - Items referencing a missing `choice_set` in the instrument
#' - Items referencing a missing `scale_id` in the instrument
#' - Items marked `reverse = TRUE` without a `scale_id`
#' - Choice sets referenced by items but not present in the instrument
#' - Scale `items` vectors containing IDs not present in the instrument
#' - Scale scoring parameters: repeated items, a `min_valid` outside 1 to the
#'   number of items, and weights that are not positive finite numbers
#' - Reverse coding declared for an item outside the scale that declares it
#' - Branching rules referencing item IDs not present in the instrument
#' - `%in%` branching rules whose `value` no evaluator can consume
#' - Attention checks referencing item IDs not present in the instrument
#' - Analysis plan roles referencing missing variables or models
#' - Model specifications referencing missing indicators or constructs
#'
#' @section Changed in 0.4.0:
#' Earlier versions returned two different things depending on `strict`: the
#' instrument itself, invisibly, when `strict = TRUE`, and a bare unclassed
#' list when `strict = FALSE`. A validator should report a diagnostic, so
#' both paths now return an [sframe_validation] object, and they return it
#' visibly, so `validate_sframe(instrument)` typed at the console shows the
#' result. Code that read `$valid` and `$problems` keeps working. Code that
#' used the `strict = TRUE` return as an instrument should now wrap the call
#' in [as_sframe()].
#'
#' @param instrument An `sframe` object created by [sf_instrument()].
#' @param strict Logical. When `TRUE` (default), any detected problem raises
#'   an error of class `sframe_validation_error`. When `FALSE`, problems are
#'   reported in the returned diagnostic without stopping.
#'
#' @return An [sframe_validation] object. When the instrument is valid, the
#'   instrument carried inside it has `meta$validated` set to `TRUE` and can
#'   be recovered with [as_sframe()].
#' @export
#' @seealso [sframe_validation], [as_sframe()], [sf_problems()],
#'   [sf_is_valid()], [sf_instrument()], [write_sframe()]
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
#' # The result prints its own diagnostic
#' validate_sframe(instr, strict = FALSE)
#'
#' # Explore it with dedicated methods rather than reaching in with `$`
#' v <- validate_sframe(instr, strict = FALSE)
#' sf_is_valid(v)
#' sf_problems(v)
#' summary(v)
#'
#' # Recover the validated instrument
#' validated <- as_sframe(validate_sframe(instr, strict = TRUE))
#' isTRUE(sf_meta(validated)$validated)
validate_sframe <- function(instrument, strict = TRUE) {
  sframe_check_instrument(instrument)

  log <- sframe_new_problem_log()
  add <- function(check, messages) sframe_log_problem(log, check, messages)

  # The one exit every path takes. It aborts under strict mode, sets the
  # validated stamp from this run, and builds the diagnostic. The shape gate
  # below used to return on its own, which skipped both the strict abort and
  # the stamp: a malformed field gave a diagnostic while strict = TRUE carried
  # on, and the instrument it returned still said it had been validated, so
  # write_sframe() wrote it.
  finish <- function() {
    if (strict && length(log$problems) > 0) {
      sframe_abort_validation(
        paste0(
          "Instrument validation failed with ",
          length(log$problems),
          " problem(s):\n",
          paste0("  - ", log$problems, collapse = "\n")
        ),
        instrument_title = sframe_meta_display(instrument$meta$title,
                                               "(untitled)")
      )
    }
    instrument$meta$validated <- length(log$problems) == 0
    sframe_new_validation(
      log,
      roster  = sframe_validation_checks,
      subject = "instrument",
      title   = sframe_meta_display(instrument$meta$title, "(untitled)"),
      version = sframe_meta_display(instrument$meta$version, "(unversioned)"),
      object  = instrument
    )
  }

  # Shape gate. Every check below reads an id as a length-1 string, so a
  # malformed field has to become a problem here. Reaching vapply() with one
  # raised an ordinary R error and the diagnostic was never built, which broke
  # the promise that strict = FALSE reports problems and returns.
  shape <- sframe_field_shape_problems(instrument)
  if (length(shape) > 0) {
    add("field_shapes", shape)
    return(finish())
  }

  item_ids    <- vapply(instrument$items,    function(x) x$id, character(1))
  choice_ids  <- vapply(instrument$choices,  function(x) x$id, character(1))
  scale_ids   <- vapply(instrument$scales,   function(x) x$id, character(1))
  model_ids   <- vapply(instrument$models %||% list(), function(x) x$id %||% "", character(1))
  valid_id <- function(x) grepl("^[A-Za-z][A-Za-z0-9_]*$", x)
  # Analysis-plan roles may name an expansion column (item__sub, item__option,
  # item__a__vs__b, item__crit) rather than the base item id, exactly as the
  # builder exports them and read_responses() already accepts them. Without
  # these a real builder export fails validation for variables that do exist.
  known_vars <- unique(c(item_ids, scale_ids,
                         sframe_item_expansion_columns(instrument)))

  # Comparison scale per decision item, keyed by item id. Used by the
  # analysis-plan checks below to reject a method-scale mismatch before data
  # collection rather than at analysis time.
  item_scales <- list()
  for (it in instrument$items) {
    if (identical(it$type, "pairwise_comparison")) {
      item_scales[[it$id]] <- as.character(it$comparison_scale %||% "saaty")[1]
    } else if (identical(it$type, "criteria_weight")) {
      item_scales[[it$id]] <- "criteria_weight"
    }
  }

  # Duplicate item IDs
  dupes <- item_ids[duplicated(item_ids)]
  if (length(dupes) > 0) {
    add("duplicate_item_ids",
      paste0("Duplicate item IDs: ", paste(dupes, collapse = ", ")))
  }

  bad_item_ids <- item_ids[!valid_id(item_ids)]
  if (length(bad_item_ids) > 0) {
    add(
      "item_id_format",
      paste0(
        "Invalid item ID(s): ",
        paste(unique(bad_item_ids), collapse = ", "),
        ". IDs must start with a letter and contain only letters, numbers, and `_` characters."
      )
    )
  }

  # Per-type item configuration, shared with the constructor. Checking it at
  # construction alone let a mutated item revalidate clean.
  item_config <- unlist(lapply(instrument$items, sframe_item_config_problems),
                        use.names = FALSE)
  if (length(item_config) > 0) add("item_config", item_config)

  # A choice set's contents, which validation used to read past entirely.
  choice_content <- unlist(lapply(instrument$choices %||% list(), function(cs) {
    p <- sframe_choice_content_problems(cs$values, cs$labels)
    if (length(p) == 0) return(character(0))
    paste0("Choice set '", as.character(cs$id %||% "(unnamed)")[1], "': ", p)
  }), use.names = FALSE)
  if (length(choice_content) > 0) add("choice_set_contents", choice_content)

  # A check's results are keyed by its id, so a repeat overwrote the first.
  check_ids <- vapply(instrument$checks %||% list(),
                      function(k) as.character(k$id %||% "")[1], character(1))
  dup_checks <- unique(check_ids[nzchar(check_ids) & duplicated(check_ids)])
  if (length(dup_checks) > 0) {
    add("duplicate_check_ids", paste0(
      "Duplicate check IDs: ", paste(dup_checks, collapse = ", "),
      ". Results are keyed by the ID, so the later check replaces the first."))
  }

  # One branching rule per target item, which is what names a branch.
  branch_targets <- vapply(instrument$branching %||% list(),
                           function(b) as.character(b$item_id %||% "")[1],
                           character(1))
  dup_targets <- unique(branch_targets[nzchar(branch_targets) &
                                         duplicated(branch_targets)])
  if (length(dup_targets) > 0) {
    add("branch_targets", paste0(
      "More than one branching rule controls item(s): ",
      paste(dup_targets, collapse = ", "),
      ". A rule is identified by the item it controls, so declare one each."))
  }

  dup_choice_ids <- choice_ids[duplicated(choice_ids)]
  if (length(dup_choice_ids) > 0) {
    add(
      "duplicate_choice_ids",
      paste0("Duplicate choice set IDs: ", paste(unique(dup_choice_ids), collapse = ", "))
    )
  }

  dup_scale_ids <- scale_ids[duplicated(scale_ids)]
  if (length(dup_scale_ids) > 0) {
    add(
      "duplicate_scale_ids",
      paste0("Duplicate scale IDs: ", paste(unique(dup_scale_ids), collapse = ", "))
    )
  }

  # One namespace for everything that becomes a column in response data.
  # Duplicates within items or within scales are reported above, so this looks
  # for names claimed by more than one kind, and for reserved metadata names.
  expansion_cols <- sframe_item_expansion_columns(instrument)
  claims <- unique(data.frame(
    name = c(item_ids, scale_ids, expansion_cols),
    kind = c(rep("an item ID", length(item_ids)),
             rep("a scale ID", length(scale_ids)),
             rep("a response column of another item", length(expansion_cols))),
    stringsAsFactors = FALSE
  ))
  for (nm in unique(claims$name[duplicated(claims$name)])) {
    add("id_namespace",
      paste0("'", nm, "' is used as ",
             paste(claims$kind[claims$name == nm], collapse = " and as "),
             ". Each becomes a column in response data, and a scale's score ",
             "would overwrite the other. Rename one of them."))
  }
  for (nm in intersect(unique(c(item_ids, scale_ids)),
                       sframe_reserved_response_columns)) {
    add("id_namespace",
      paste0("'", nm, "' is reserved for response metadata, so an item or ",
             "scale cannot use it as an ID."))
  }

  for (item in instrument$items) {
    # Missing labels
    if (is.null(item$label) || nchar(trimws(item$label)) == 0) {
      add("item_labels",
        paste0("Item '", item$id, "' has an empty label."))
    }
    # Orphan choice set references
    if (!is.null(item$choice_set) && !item$choice_set %in% choice_ids) {
      add("item_choice_set_refs",
        paste0("Item '", item$id, "' references choice_set '",
               item$choice_set, "' which is missing from the instrument."))
    }
    # Orphan scale references
    if (!is.null(item$scale_id) && !item$scale_id %in% scale_ids) {
      add("item_scale_refs",
        paste0("Item '", item$id, "' references scale_id '",
               item$scale_id, "' which is missing from the instrument."))
    }
    # Reverse coded without scale
    if (isTRUE(item$reverse) && is.null(item$scale_id)) {
      add("reverse_without_scale",
        paste0("Item '", item$id,
               "' is reverse = TRUE but has no scale_id."))
    }
    # Decision item types (v0.5). These carry their response options in
    # comparison_items rather than a choice set, so a choice_set here means
    # the item was built from the wrong template.
    if (item$type %in% c("pairwise_comparison", "criteria_weight")) {
      n_comparison <- length(item$comparison_items %||% character(0))
      if (n_comparison < 2) {
        add("decision_item_shape",
          paste0("Item '", item$id, "' of type '", item$type,
                 "' needs at least 2 comparison_items."))
      }
      if (n_comparison > 10) {
        add("decision_item_shape",
          paste0("Item '", item$id, "' declares ", n_comparison,
                 " comparison_items. The maximum is 10."))
      }
      if (anyDuplicated(item$comparison_items %||% character(0)) > 0) {
        add("decision_item_shape",
          paste0("Item '", item$id, "' has duplicated comparison_items."))
      }
      if (!is.null(item$choice_set)) {
        add("decision_item_shape",
          paste0("Item '", item$id, "' of type '", item$type,
                 "' must not reference a choice_set."))
      }
    }
    if (identical(item$type, "pairwise_comparison") &&
        !(item$comparison_scale %||% "saaty") %in% c("saaty", "influence")) {
      add("comparison_scale",
        paste0("Item '", item$id, "' has comparison_scale '",
               item$comparison_scale,
               "'. It must be either 'saaty' or 'influence'."))
    }
    if (!item$type %in% c("pairwise_comparison", "criteria_weight") &&
        length(item$comparison_items %||% character(0)) > 0) {
      add("decision_item_shape",
        paste0("Item '", item$id, "' of type '", item$type,
               "' must not declare comparison_items."))
    }
  }

  # Scale item membership
  for (scale in instrument$scales) {
    missing_items <- setdiff(scale$items, item_ids)
    if (length(missing_items) > 0) {
      add("scale_membership",
        paste0("Scale '", scale$id, "' references unknown item(s): ",
               paste(missing_items, collapse = ", ")))
    }
    add("scale_parameters", sframe_scale_parameter_problems(scale))
    add("reverse_item_membership", sframe_scale_reverse_problems(scale))
  }
  # An item reversed at item level is reversed within the scale its scale_id
  # names, so that scale must list it.
  for (item in instrument$items) {
    if (!isTRUE(item$reverse) || is.null(item$scale_id)) next
    owner <- Filter(function(s) identical(s$id, item$scale_id), instrument$scales)
    if (length(owner) == 1 && !item$id %in% owner[[1]]$items) {
      add("reverse_item_membership",
        paste0("Item '", item$id, "' is reverse = TRUE within scale '",
               item$scale_id, "', which does not list it among its items."))
    }
  }

  # Branching rule integrity
  for (rule in instrument$branching) {
    if (!rule$item_id %in% item_ids) {
      add("branching_refs",
        paste0("Branch rule targets unknown item '", rule$item_id, "'."))
    }
    if (!rule$depends_on %in% item_ids) {
      add("branching_refs",
        paste0("Branch rule depends_on unknown item '",
               rule$depends_on, "'."))
    }
    # A `%in%` rule whose value no evaluator can consume is a rule that can
    # never fire, and a rule that never fires hides its target for good. That
    # went unsaid from 0.3.0 to 0.4.0 while multi-value rules were dead in
    # every exported survey, so it is named here rather than left to be found
    # by a respondent who cannot reach the questions.
    if (identical(rule$operator, "%in%")) {
      if (!is.null(rule$value) && !is.atomic(rule$value)) {
        add("branching_values",
          paste0("Branch rule on item '", rule$item_id, "' uses %in% with a ",
                 "non-atomic value. Supply a character or numeric vector."))
      } else if (length(sframe_branch_in_values(rule$value)) == 0) {
        add("branching_values",
          paste0("Branch rule on item '", rule$item_id, "' uses %in% with no ",
                 "values to match, so the rule can never be satisfied and the ",
                 "item would stay hidden. Supply a character or numeric ",
                 "vector."))
      }
    }
  }

  # Check item references
  check_item_ids <- vapply(instrument$checks,
                           function(x) x$item_id, character(1))
  missing_check_items <- setdiff(check_item_ids, item_ids)
  if (length(missing_check_items) > 0) {
    add("check_refs",
      paste0("Check(s) reference unknown item(s): ",
             paste(missing_check_items, collapse = ", ")))
  }

  plan_problems <- sframe_plan_block_problems(
    instrument$analysis_plan %||% list(), sframe_dispatch_methods())
  if (length(plan_problems) > 0) add("analysis_plan_blocks", plan_problems)

  # Analysis plan references. Old plans use `variables`; v0.3 plans use
  # role-based assignments, and both formats stay valid.
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
      add(
        "analysis_plan_models",
        paste0(
          "Analysis plan '", block_id, "' references missing model(s): ",
          paste(unique(missing_models), collapse = ", ")
        )
      )
    }
    refs <- refs[nzchar(refs)]
    missing_refs <- setdiff(unique(refs), known_vars)
    if (length(missing_refs) > 0) {
      add(
        "analysis_plan_variables",
        paste0(
          "Analysis plan '", block_id, "' references unknown variable(s): ",
          paste(missing_refs, collapse = ", ")
        )
      )
    }
    # Decision-family scale compatibility, checked at design time rather than
    # left to the runner. The 2 comparison scales are not interchangeable:
    # AHP and ANP read reciprocal relative importance on the Saaty ratio
    # scale, DEMATEL reads a directed 0-4 influence matrix, and a
    # criteria-weight source must express importance rather than influence.
    # Pairing them the wrong way round produces plausible numbers from
    # meaningless input, so it belongs in the pre-collection contract.
    method_id <- block_method[1]
    roles <- if (is.list(block$roles)) block$roles else list()
    if (nzchar(method_id) && length(item_scales) > 0) {
      needed <- switch(
        method_id,
        ahp = "saaty", anp = "saaty", dematel = "influence", NULL
      )
      role_pairwise <- as.character(unlist(roles[["pairwise"]] %||% character(0)))
      for (ref in role_pairwise[nzchar(role_pairwise)]) {
        got <- item_scales[[ref]]
        if (!is.null(needed) && !is.null(got) && !identical(got, needed)) {
          add("decision_scale_compatibility", paste0(
            "Analysis plan '", block_id, "' runs ", toupper(method_id),
            " on item '", ref, "', which uses the '", got,
            "' comparison scale. ", toupper(method_id), " needs '", needed,
            "'."
          ))
        }
      }
      role_weights <- as.character(unlist(roles[["weights_item"]] %||% character(0)))
      for (ref in role_weights[nzchar(role_weights)]) {
        if (identical(item_scales[[ref]], "influence")) {
          add("decision_scale_compatibility", paste0(
            "Analysis plan '", block_id, "' takes criterion weights from item '",
            ref, "', which uses the 'influence' comparison scale. Influence ",
            "measures directed effect rather than relative importance, so it ",
            "cannot supply weights."
          ))
        }
      }
    }
  }

  # Model layer integrity.
  dup_model_ids <- model_ids[nzchar(model_ids) & duplicated(model_ids)]
  if (length(dup_model_ids) > 0) {
    add(
      "model_ids",
      paste0("Duplicate model IDs: ", paste(unique(dup_model_ids), collapse = ", "))
    )
  }
  bad_model_ids <- model_ids[nzchar(model_ids) & !valid_id(model_ids)]
  if (length(bad_model_ids) > 0) {
    add(
      "model_ids",
      paste0("Invalid model ID(s): ", paste(unique(bad_model_ids), collapse = ", "))
    )
  }
  for (model in instrument$models %||% list()) {
    model_check <- tryCatch(
      validate_model(model, instrument = instrument, strict = FALSE),
      error = function(e) list(valid = FALSE, problems = conditionMessage(e))
    )
    if (!isTRUE(model_check$valid)) {
      add(
        "model_specs",
        paste0(
          "Model '", model$id %||% "(unnamed)", "': ",
          model_check$problems
        )
      )
    }
  }

  # The validated stamp travels on the instrument carried by the result, so
  # as_sframe() hands back an instrument that records the outcome, and strict
  # mode aborts. Both happen in finish(), which every exit shares.
  finish()
}
