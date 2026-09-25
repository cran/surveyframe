# studio_builder.R
# Internal helper functions for the SurveyStudio Shiny application.

sframe_builder_as_choice <- function(choice) {
  if (inherits(choice, "sf_choices")) {
    return(choice)
  }

  sf_choices(
    id = choice$id,
    values = choice$values,
    labels = choice$labels,
    allow_other = isTRUE(choice$allow_other),
    randomise = isTRUE(choice$randomise)
  )
}

sframe_builder_as_item <- function(item) {
  if (inherits(item, "sf_item")) {
    return(item)
  }

  # Every field sf_item() accepts is forwarded. A hand-kept list of fields
  # dropped date limits, comparison items and the comparison scale, and would
  # drop any field added to sf_item() later.
  fields <- intersect(names(item), names(formals(sf_item)))
  args <- item[fields]
  args <- args[!vapply(args, is.null, logical(1))]
  args$type <- args$type %||% "text"
  args$required <- isTRUE(item$required)
  args$reverse <- isTRUE(item$reverse)
  do.call(sf_item, args)
}

# Applies the fields SurveyStudio's item form holds to an existing item,
# keeping everything the form does not show. Replacing the item with a fresh
# one built from the form erased reverse coding, scale membership, the page
# number and every type setting, so editing an item's wording silently
# changed how it was scored. Settings belonging to the previous type are
# dropped when the type changes, since they no longer apply.
sframe_builder_update_item <- function(existing, edits) {
  if (is.null(existing)) return(sframe_builder_as_item(edits))
  type_fields <- list(
    matrix = "matrix_items",
    slider = c("slider_min", "slider_max", "slider_step"),
    rating = c("rating_max", "rating_icon"),
    date = c("date_min", "date_max"),
    section_break = "section_intro",
    pairwise_comparison = c("comparison_items", "comparison_scale"),
    criteria_weight = c("comparison_items", "comparison_scale")
  )
  merged <- unclass(existing)
  old_type <- merged$type %||% "text"
  new_type <- edits$type %||% old_type
  if (!identical(old_type, new_type)) {
    for (f in type_fields[[old_type]] %||% character(0)) merged[[f]] <- NULL
  }
  for (f in names(edits)) merged[f] <- list(edits[[f]])
  merged$type <- new_type
  sframe_builder_as_item(merged)
}

sframe_builder_as_scale <- function(scale) {
  if (inherits(scale, "sf_scale")) {
    return(scale)
  }

  sf_scale(
    id = scale$id,
    label = scale$label,
    items = scale$items %||% character(0),
    method = scale$method %||% "mean",
    min_valid = scale$min_valid %||% NULL,
    reverse_items = scale$reverse_items %||% NULL,
    weights = scale$weights %||% NULL
  )
}

sframe_builder_as_branch <- function(branch) {
  if (inherits(branch, "sf_branch")) {
    return(branch)
  }

  sf_branch(
    item_id = branch$item_id,
    depends_on = branch$depends_on,
    operator = branch$operator %||% "==",
    value = branch$value,
    action = branch$action %||% "show"
  )
}

sframe_builder_as_check <- function(check) {
  if (inherits(check, "sf_check")) {
    return(check)
  }

  sf_check(
    id = check$id,
    item_id = check$item_id,
    type = check$type %||% "attention",
    pass_values = check$pass_values %||% NULL,
    fail_action = check$fail_action %||% "flag",
    label = check$label %||% NULL,
    notes = check$notes %||% NULL
  )
}

#' Create an empty SurveyStudio builder state
#'
#' @return A list containing empty metadata, choice, item, scale, branching,
#'   and check collections suitable for SurveyStudio.
#' @export
#' @examples
#' state <- sframe_builder_empty_state()
#' state$meta$title
#' length(state$items)
sframe_builder_empty_state <- function() {
  list(
    meta = list(
      title = "Untitled Survey",
      version = "0.1.0",
      description = NULL,
      authors = NULL,
      languages = "en"
    ),
    choices = list(),
    items = list(),
    scales = list(),
    branching = list(),
    checks = list(),
    analysis_plan = list(),
    models = list(),
    render = list(),
    designs = list()
  )
}

#' Convert an instrument into a SurveyStudio builder state
#'
#' @param instrument An `sframe` object or `NULL`.
#'
#' @return A builder state list. Component classes are restored so the state
#'   can be edited or validated by SurveyStudio.
#' @export
#' @examples
#' demo <- sframe_demo_data()
#' state <- sframe_builder_state_from_instrument(demo$instrument)
#' length(state$items)
#' length(state$scales)
sframe_builder_state_from_instrument <- function(instrument = NULL) {
  if (is.null(instrument)) {
    return(sframe_builder_empty_state())
  }

  sframe_check_instrument(instrument)

  list(
    meta = list(
      title = instrument$meta$title %||% "Untitled Survey",
      version = instrument$meta$version %||% "0.1.0",
      description = instrument$meta$description %||% NULL,
      authors = instrument$meta$authors %||% NULL,
      languages = instrument$meta$languages %||% "en",
      # When the instrument was created is a fact about it, and the content
      # hash covers it. Dropping it here made compose stamp the current time,
      # so a rebuild changed the creation date and moved the hash the
      # amendment boundary rests on. The 2 usually landed in the same second,
      # which is why it showed as a 1-in-25 test failure.
      created_at = instrument$meta$created_at %||% NULL
    ),
    choices = lapply(instrument$choices %||% list(), sframe_builder_as_choice),
    items = lapply(instrument$items %||% list(), sframe_builder_as_item),
    scales = lapply(instrument$scales %||% list(), sframe_builder_as_scale),
    branching = lapply(instrument$branching %||% list(), sframe_builder_as_branch),
    checks = lapply(instrument$checks %||% list(), sframe_builder_as_check),
    analysis_plan = instrument$analysis_plan %||% list(),
    models = instrument$models %||% list(),
    render = instrument$render %||% list(),
    # Carried through so a round trip via the builder (any observer or flow
    # that rebuilds rv$instrument from rv$builder, e.g. SurveyStudio's
    # standing draft_result() sync) does not silently drop a previously
    # disclosed amendment log. sf_instrument() itself takes no amendments
    # argument; it is reattached after composing, in
    # sframe_builder_compose_instrument().
    amendments = instrument$amendments %||% list(),
    # Conjoint designs have no Studio editor, and are carried through untouched
    # so a rebuild keeps them. They were dropped here.
    designs = instrument$designs %||% list(),
    # What the instrument was when read from a file, reattached on compose, so
    # a Studio edit of a loaded file is still recognised as a revision.
    origin = attr(instrument, "sframe_origin")
  )
}

sframe_builder_compose_instrument <- function(
    meta,
    choices = list(),
    items = list(),
    scales = list(),
    branching = list(),
    checks = list(),
    analysis_plan = list(),
    models = list(),
    render = list(),
    amendments = list(),
    designs = list(),
    origin = NULL
) {
  choices <- lapply(choices, sframe_builder_as_choice)
  items <- lapply(items, sframe_builder_as_item)
  scales <- lapply(scales, sframe_builder_as_scale)
  branching <- lapply(branching, sframe_builder_as_branch)
  checks <- lapply(checks, sframe_builder_as_check)

  # Scale membership is edited on the scales, so each item's scale_id follows
  # them: an item keeps its scale while that scale still lists it, and
  # otherwise takes the first scale that does. Reverse coding is left where it
  # was declared. An item-level reverse flag stays with its item while the item
  # stays in the same scale, and a scale's reverse_items stay on the scale.
  # Clearing every item's flag and rebuilding it from reverse_items silently
  # un-reversed items declared reverse = TRUE.
  if (length(items) > 0) {
    items <- lapply(items, function(item) {
      listing <- Filter(function(s) item$id %in% (s$items %||% character(0)), scales)
      listing_ids <- vapply(listing, function(s) s$id, character(1))
      previous <- item$scale_id
      item$scale_id <- if (!is.null(previous) && previous %in% listing_ids) {
        previous
      } else if (length(listing_ids) > 0) {
        listing_ids[[1]]
      }
      item$reverse <- isTRUE(item$reverse) && identical(item$scale_id, previous)
      class(item) <- "sf_item"
      item
    })
  }

  instrument <- sf_instrument(
    title = meta$title %||% "Untitled Survey",
    version = meta$version %||% "0.1.0",
    description = meta$description %||% NULL,
    authors = meta$authors %||% NULL,
    languages = meta$languages %||% "en",
    components = c(choices, items, scales, branching, checks,
                   designs %||% list()),
    analysis_plan = analysis_plan,
    models = models,
    render = render %||% list()
  )

  # sf_instrument() stamps a fresh created_at, which is right for a new
  # instrument and wrong for one being rebuilt from an existing state.
  if (!is.null(meta$created_at)) {
    instrument$meta$created_at <- as.character(meta$created_at)[1]
  }

  # sf_instrument() has no amendments argument (see its own definition); a
  # freshly built instrument legitimately has none, but one round-tripped
  # through the builder from an already-amended instrument must keep its
  # disclosed history rather than silently losing it here.
  if (length(amendments) > 0) {
    instrument$amendments <- amendments
  }
  if (!is.null(origin)) {
    attr(instrument, "sframe_origin") <- origin
  }

  instrument
}

# The reason write_sframe() would refuse a valid draft as an undisclosed or
# inconsistent revision, or NULL when it would write.
sframe_builder_revision_problem <- function(instrument) {
  checked <- tryCatch(as_sframe(validate_sframe(instrument, strict = TRUE)),
                      error = function(e) NULL)
  if (is.null(checked)) return(NULL)
  tryCatch({
    sframe_check_amendment_boundary(checked)
    NULL
  }, error = function(e) conditionMessage(e))
}

#' Validate a SurveyStudio draft state
#'
#' @param meta List of instrument metadata.
#' @param choices,items,scales,branching,checks Lists of draft components.
#' @param analysis_plan List of draft analysis-plan blocks.
#' @param models List of draft model specifications.
#' @param render List of rendering settings (welcome, header/logo, thankyou,
#'   theme) carried from the loaded instrument so previews and exports match.
#' @param amendments List of previously disclosed amendment entries, carried
#'   through unchanged so a draft round trip does not drop them.
#' @param designs List of conjoint designs, carried through unchanged, since
#'   Studio has no editor for them.
#' @param origin The load record of an instrument read with [read_sframe()],
#'   from the builder state, or `NULL`. Carried onto the draft so an edit of a
#'   loaded file is recognised as a revision.
#'
#' @return A list with `valid`, `problems`, `instrument`, and
#'   `revision_problem`: the reason [write_sframe()] would refuse the draft as
#'   an undisclosed revision, or `NULL`.
#' @export
#' @examples
#' demo  <- sframe_demo_data()
#' state <- sframe_builder_state_from_instrument(demo$instrument)
#' draft <- sframe_builder_validate_draft(
#'   meta = state$meta, choices = state$choices, items = state$items,
#'   scales = state$scales, branching = state$branching, checks = state$checks
#' )
#' draft$valid
sframe_builder_validate_draft <- function(
    meta,
    choices = list(),
    items = list(),
    scales = list(),
    branching = list(),
    checks = list(),
    analysis_plan = list(),
    models = list(),
    render = list(),
    amendments = list(),
    designs = list(),
    origin = NULL
) {
  instrument <- sframe_builder_compose_instrument(
    meta = meta,
    choices = choices,
    items = items,
    scales = scales,
    branching = branching,
    checks = checks,
    analysis_plan = analysis_plan,
    models = models,
    render = render,
    amendments = amendments,
    designs = designs,
    origin = origin
  )

  validation <- validate_sframe(instrument, strict = FALSE)
  problems <- validation$problems

  if (!nzchar(trimws(instrument$meta$title %||% ""))) {
    problems <- c(problems, "Survey title is required.")
  }

  if (length(instrument$items) == 0) {
    problems <- c(problems, "Survey must contain at least one item.")
  }

  component_dupes <- function(components) {
    ids <- vapply(components, function(component) component$id, character(1))
    ids[duplicated(ids)]
  }

  dup_choices <- component_dupes(instrument$choices)
  if (length(dup_choices) > 0) {
    problems <- c(
      problems,
      paste0("Duplicate choice set IDs: ", paste(unique(dup_choices), collapse = ", "))
    )
  }

  dup_scales <- component_dupes(instrument$scales)
  if (length(dup_scales) > 0) {
    problems <- c(
      problems,
      paste0("Duplicate scale IDs: ", paste(unique(dup_scales), collapse = ", "))
    )
  }

  dup_checks <- component_dupes(instrument$checks)
  if (length(dup_checks) > 0) {
    problems <- c(
      problems,
      paste0("Duplicate check IDs: ", paste(unique(dup_checks), collapse = ", "))
    )
  }

  if (length(instrument$scales) > 0) {
    scale_membership <- unlist(
      lapply(instrument$scales, function(scale) {
        stats::setNames(rep(scale$id, length(scale$items)), scale$items)
      }),
      use.names = TRUE
    )
    dup_membership <- unique(names(scale_membership)[duplicated(names(scale_membership))])
    if (length(dup_membership) > 0) {
      problems <- c(
        problems,
        paste0(
          "Items assigned to multiple scales: ",
          paste(dup_membership, collapse = ", ")
        )
      )
    }
  }

  problems <- unique(problems)

  list(
    valid = length(problems) == 0,
    problems = problems,
    instrument = instrument,
    revision_problem = if (length(problems) == 0) {
      sframe_builder_revision_problem(instrument)
    }
  )
}
