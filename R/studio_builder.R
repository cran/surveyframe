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

  sf_item(
    id = item$id,
    label = item$label,
    type = item$type %||% "text",
    required = isTRUE(item$required),
    choice_set = item$choice_set %||% NULL,
    scale_id = item$scale_id %||% NULL,
    reverse = isTRUE(item$reverse),
    help = item$help %||% NULL,
    placeholder = item$placeholder %||% NULL,
    matrix_items = item$matrix_items %||% NULL,
    slider_min = item$slider_min %||% NULL,
    slider_max = item$slider_max %||% NULL,
    slider_step = item$slider_step %||% NULL,
    rating_max = item$rating_max %||% NULL,
    rating_icon = item$rating_icon %||% NULL,
    section_intro = item$section_intro %||% NULL,
    page = item$page %||% NULL
  )
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
    render = list()
  )
}

#' Convert an instrument into a SurveyStudio builder state
#'
#' @param instrument An `sframe` object or `NULL`.
#'
#' @return A builder state list. Component classes are restored so the state
#'   can be edited or validated by SurveyStudio.
#' @export
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
      languages = instrument$meta$languages %||% "en"
    ),
    choices = lapply(instrument$choices %||% list(), sframe_builder_as_choice),
    items = lapply(instrument$items %||% list(), sframe_builder_as_item),
    scales = lapply(instrument$scales %||% list(), sframe_builder_as_scale),
    branching = lapply(instrument$branching %||% list(), sframe_builder_as_branch),
    checks = lapply(instrument$checks %||% list(), sframe_builder_as_check),
    analysis_plan = instrument$analysis_plan %||% list(),
    models = instrument$models %||% list(),
    render = instrument$render %||% list()
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
    render = list()
) {
  choices <- lapply(choices, sframe_builder_as_choice)
  items <- lapply(items, sframe_builder_as_item)
  scales <- lapply(scales, sframe_builder_as_scale)
  branching <- lapply(branching, sframe_builder_as_branch)
  checks <- lapply(checks, sframe_builder_as_check)

  if (length(items) > 0) {
    item_ids <- vapply(items, function(item) item$id, character(1))

    items <- lapply(items, function(item) {
      item$scale_id <- NULL
      item$reverse <- FALSE
      class(item) <- "sf_item"
      item
    })

    for (scale in scales) {
      reverse_ids <- scale$reverse_items %||% character(0)
      for (item_id in scale$items %||% character(0)) {
        idx <- match(item_id, item_ids)
        if (is.na(idx)) {
          next
        }
        items[[idx]]$scale_id <- scale$id
        items[[idx]]$reverse <- item_id %in% reverse_ids
      }
    }
  }

  sf_instrument(
    title = meta$title %||% "Untitled Survey",
    version = meta$version %||% "0.1.0",
    description = meta$description %||% NULL,
    authors = meta$authors %||% NULL,
    languages = meta$languages %||% "en",
    components = c(choices, items, scales, branching, checks),
    analysis_plan = analysis_plan,
    models = models,
    render = render %||% list()
  )
}

#' Validate a SurveyStudio draft state
#'
#' @param meta List of instrument metadata.
#' @param choices,items,scales,branching,checks Lists of draft components.
#' @param analysis_plan List of draft analysis-plan blocks.
#' @param models List of draft model specifications.
#' @param render List of rendering settings (welcome, header/logo, thankyou,
#'   theme) carried from the loaded instrument so previews and exports match.
#'
#' @return A list with `valid`, `problems`, and `instrument`.
#' @export
sframe_builder_validate_draft <- function(
    meta,
    choices = list(),
    items = list(),
    scales = list(),
    branching = list(),
    checks = list(),
    analysis_plan = list(),
    models = list(),
    render = list()
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
    render = render
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
    instrument = instrument
  )
}
