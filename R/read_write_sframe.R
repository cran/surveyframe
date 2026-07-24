# read_sframe.R and write_sframe.R

sframe_strip_component_class <- function(component) {
  class(component) <- NULL
  component
}

sframe_serialization_payload <- function(instrument, hash_value = "") {
  # Strip list-level names before serialisation so items, choices, scales,
  # branching, and checks are always written as JSON arrays. Without unname(),
  # instruments built with Map() (which attaches item IDs as list names) produce
  # a JSON object keyed by item ID, while the round-tripped structure produces
  # integer-keyed or differently keyed objects, causing a hash mismatch on read.
  list(
    hash = list(algo = "sha256", value = hash_value),
    version = instrument$meta$version,
    meta = instrument$meta,
    items    = unname(lapply(instrument$items,    sframe_strip_component_class)),
    choices  = unname(lapply(instrument$choices,  sframe_strip_component_class)),
    scales   = unname(lapply(instrument$scales,   sframe_strip_component_class)),
    branching = unname(lapply(instrument$branching, sframe_strip_component_class)),
    checks   = unname(lapply(instrument$checks,   sframe_strip_component_class)),
    analysis_plan = instrument$analysis_plan %||% list(),
    models = unname(lapply(instrument$models %||% list(), sframe_model_plain)),
    render = instrument$render
  )
}

sframe_json_object <- function() {
  structure(list(), names = character())
}

sframe_canonical_payload <- function(x) {
  if (is.null(x)) {
    return(sframe_json_object())
  }

  if (is.list(x)) {
    # Keep this in sync with the SurveyBuilder sframeCanon() implementation:
    # recurse into arrays/objects and sort object keys alphabetically.
    x <- lapply(x, sframe_canonical_payload)
    nm <- names(x)
    if (!is.null(nm)) {
      x <- x[order(nm)]
    }
  }

  x
}

sframe_hash_json <- function(payload, canonical = TRUE) {
  if (isTRUE(canonical)) {
    payload <- sframe_canonical_payload(payload)
  }
  jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    pretty = FALSE,
    null = "null",
    digits = NA
  )
}

sframe_hash_payload <- function(payload) {
  payload$hash$value <- ""
  json_for_hash <- sframe_hash_json(payload, canonical = TRUE)
  as.character(openssl::sha256(json_for_hash))
}

sframe_legacy_hash_payload <- function(payload) {
  payload$hash$value <- ""
  json_for_hash <- jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = FALSE)
  as.character(openssl::sha256(json_for_hash))
}

sframe_hash_value <- function(instrument) {
  sframe_hash_payload(sframe_serialization_payload(instrument))
}

#' Write an instrument to a .sframe file
#'
#' Serialises an `sframe` instrument object to a UTF-8 JSON file with a
#' SHA-256 integrity hash. The instrument is validated before writing unless
#' the object already carries a valid status. The hash is computed over the
#' full serialised content with the `hash.value` field set to an empty string.
#'
#' @param instrument An `sframe` object created by [sf_instrument()].
#' @param path Character. The file path to write to. The `.sframe` extension
#'   is appended automatically if not already present.
#' @param pretty Logical. Whether to write formatted JSON with indentation.
#'   Defaults to `TRUE`. Set to `FALSE` for compact files.
#' @param overwrite Logical. Whether to overwrite an existing file. Defaults
#'   to `FALSE`.
#'
#' @return The file path, invisibly.
#' @export
#' @seealso [read_sframe()], [validate_sframe()]
#'
#' @examples
#' instr <- read_sframe(
#'   system.file("extdata", "tourism_services_demo.sframe",
#'               package = "surveyframe")
#' )
#' out <- write_sframe(instr, tempfile(fileext = ".sframe"))
#' file.exists(out)
write_sframe <- function(instrument, path, pretty = TRUE, overwrite = FALSE) {
  sframe_check_instrument(instrument)

  if (!endsWith(path, ".sframe")) {
    path <- paste0(path, ".sframe")
  }

  if (file.exists(path) && !overwrite) {
    sframe_abort_import(
      paste0("File already exists: '", path,
             "'. Use overwrite = TRUE to replace it."),
      path = path
    )
  }

  # Validate before writing and persist the validation flag.
  instrument <- validate_sframe(instrument, strict = TRUE)

  # Build the full JSON payload with an empty hash placeholder
  payload <- sframe_serialization_payload(instrument)

  # Insert real hash
  payload$hash$value <- sframe_hash_payload(payload)

  json_out <- jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = pretty,
                               null = "null")
  writeLines(json_out, con = path, useBytes = FALSE)

  invisible(path)
}

sframe_as_vector <- function(x, mode = NULL) {
  if (is.null(x)) {
    return(NULL)
  }

  out <- unlist(x, recursive = TRUE, use.names = FALSE)
  if (is.null(mode)) {
    return(out)
  }

  switch(
    mode,
    character = as.character(out),
    numeric = as.numeric(out),
    logical = as.logical(out),
    out
  )
}

sframe_empty_to_null <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  if (is.list(x) && length(x) == 0) {
    return(NULL)
  }

  x
}

sframe_restore_item <- function(item) {
  item$required <- isTRUE(item$required)
  item$reverse <- isTRUE(item$reverse)
  item$choice_set <- sframe_empty_to_null(item$choice_set)
  item$scale_id <- sframe_empty_to_null(item$scale_id)
  item$help <- sframe_empty_to_null(item$help)
  item$placeholder <- sframe_empty_to_null(item$placeholder)
  item$matrix_items <- sframe_as_vector(
    sframe_empty_to_null(item$matrix_items),
    "character"
  )
  item$slider_min <- if (!is.null(sframe_empty_to_null(item$slider_min))) {
    as.numeric(item$slider_min)
  } else {
    NULL
  }
  item$slider_max <- if (!is.null(sframe_empty_to_null(item$slider_max))) {
    as.numeric(item$slider_max)
  } else {
    NULL
  }
  item$slider_step <- if (!is.null(sframe_empty_to_null(item$slider_step))) {
    as.numeric(item$slider_step)
  } else {
    NULL
  }
  item$rating_max <- if (!is.null(sframe_empty_to_null(item$rating_max))) {
    as.integer(item$rating_max)
  } else {
    NULL
  }
  item$rating_icon <- sframe_empty_to_null(item$rating_icon)
  item$date_min <- if (!is.null(sframe_empty_to_null(item$date_min))) {
    as.character(item$date_min)
  } else {
    NULL
  }
  item$date_max <- if (!is.null(sframe_empty_to_null(item$date_max))) {
    as.character(item$date_max)
  } else {
    NULL
  }
  item$section_intro <- sframe_empty_to_null(item$section_intro)
  item$page <- if (!is.null(sframe_empty_to_null(item$page))) {
    as.integer(item$page)
  } else {
    NULL
  }
  class(item) <- "sf_item"
  item
}

sframe_restore_choices <- function(choice) {
  choice$values <- sframe_as_vector(choice$values)
  choice$labels <- sframe_as_vector(choice$labels, "character")
  choice$allow_other <- isTRUE(choice$allow_other)
  choice$randomise <- isTRUE(choice$randomise)
  class(choice) <- "sf_choices"
  choice
}

sframe_restore_scale <- function(scale) {
  scale$items <- sframe_as_vector(scale$items, "character")
  scale$reverse_items <- sframe_as_vector(scale$reverse_items, "character")
  scale$weights <- sframe_as_vector(scale$weights, "numeric")
  if (!is.null(scale$min_valid)) {
    scale$min_valid <- as.integer(scale$min_valid)
  }
  class(scale) <- "sf_scale"
  scale
}

sframe_restore_branch <- function(branch) {
  if (is.list(branch$value) && length(branch$value) > 0) {
    branch$value <- sframe_as_vector(branch$value)
  }
  class(branch) <- "sf_branch"
  branch
}

sframe_restore_check <- function(check) {
  check$pass_values <- sframe_as_vector(check$pass_values)
  class(check) <- "sf_check"
  check
}

sframe_restore_analysis_block <- function(block) {
  block$id <- as.character(block$id %||% "")
  block$research_question <- as.character(block$research_question %||% "")
  block$family <- as.character(block$family %||% "")
  block$method <- as.character(block$method %||% block$test %||% "")
  block$roles <- sframe_empty_to_null(block$roles) %||% list()
  block$options <- sframe_empty_to_null(block$options) %||% list()
  block$hypotheses <- sframe_empty_to_null(block$hypotheses)
  block$decision_rule <- as.character(block$decision_rule %||% block$interpretation %||% "")
  block$reporting_references <- sframe_as_vector(
    sframe_empty_to_null(block$reporting_references %||% block$citations),
    "character"
  ) %||% character(0)
  block$status <- as.character(block$status %||% "draft")
  block$requires_data <- if (is.null(block$requires_data)) TRUE else isTRUE(block$requires_data)
  block$variables <- sframe_as_vector(
    sframe_empty_to_null(block$variables),
    "character"
  )
  block$test <- as.character(block$test %||% block$method %||% "")
  block$alpha <- if (!is.null(sframe_empty_to_null(block$alpha))) {
    as.numeric(block$alpha)
  } else {
    sframe_empty_to_null(block$options$alpha)
  }
  block$interpretation <- as.character(block$interpretation %||% "")
  block$citations <- sframe_as_vector(
    sframe_empty_to_null(block$citations),
    "character"
  ) %||% character(0)
  block$result <- sframe_empty_to_null(block$result)
  block
}

sframe_restore_model <- function(model) {
  if (is.null(model$type)) model$type <- "cfa"
  if (identical(model$type, "sem")) model$type <- "cb_sem"
  if (is.null(model$engine)) {
    model$engine <- switch(
      model$type,
      cfa = "lavaan",
      cb_sem = "lavaan",
      pls_sem = "seminr",
      sem = "lavaan",
      "lavaan"
    )
  }
  if (is.null(model$measurement)) model$measurement <- list(constructs = list())
  if (is.null(model$measurement$constructs)) model$measurement$constructs <- list()
  if (is.null(model$structural)) {
    model$structural <- list(paths = list(), covariances = list(), indirect = list())
  }
  if (is.null(model$structural$paths)) model$structural$paths <- list()
  if (is.null(model$structural$covariances)) model$structural$covariances <- list()
  if (is.null(model$structural$indirect)) model$structural$indirect <- list()
  if (is.null(model$options)) model$options <- list()

  model$measurement$constructs <- lapply(model$measurement$constructs, function(con) {
    con$items <- sframe_as_vector(sframe_empty_to_null(con$items), "character") %||% character(0)
    con$mode <- as.character(con$mode %||% "reflective")
    class(con) <- "sf_construct"
    con
  })
  model$structural$paths <- lapply(model$structural$paths, function(path) {
    class(path) <- "sf_path"
    path
  })
  model$structural$covariances <- lapply(model$structural$covariances, function(cov) {
    class(cov) <- "sf_covariance"
    cov
  })
  model$structural$indirect <- lapply(model$structural$indirect, function(ind) {
    ind$through <- sframe_as_vector(sframe_empty_to_null(ind$through), "character") %||% character(0)
    class(ind) <- "sf_indirect"
    ind
  })
  class(model) <- "sf_model"
  model
}

sframe_validate_parsed_payload <- function(parsed, path) {
  required <- c("hash", "version", "meta", "items", "choices", "scales")
  missing <- setdiff(required, names(parsed))
  if (length(missing) > 0) {
    sframe_abort_import(
      paste0(
        "Invalid .sframe file: missing required field(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      path = path
    )
  }

  if (!is.list(parsed$hash) ||
      !identical(parsed$hash$algo %||% "", "sha256") ||
      is.null(parsed$hash$value)) {
    sframe_abort_import(
      "Invalid .sframe file: missing SHA-256 hash metadata.",
      path = path
    )
  }

  if (!is.list(parsed$meta) ||
      is.null(parsed$meta$title) ||
      !is.list(parsed$items) ||
      !is.list(parsed$choices) ||
      !is.list(parsed$scales)) {
    sframe_abort_import(
      "Invalid .sframe file: expected a recognised payload structure.",
      path = path
    )
  }
}

#' Read an instrument from a .sframe file
#'
#' Reads a `.sframe` JSON file and reconstructs an `sframe` instrument object.
#' The SHA-256 integrity hash is verified on load unless `validate = FALSE`.
#'
#' @param path Character. The path to a `.sframe` file.
#' @param validate Logical. Whether to validate the loaded instrument with
#'   [validate_sframe()]. Defaults to `TRUE`.
#'
#' @return An `sframe` object.
#' @export
#' @seealso [write_sframe()], [validate_sframe()]
#'
#' @examples
#' instr <- read_sframe(
#'   system.file("extdata", "tourism_services_demo.sframe",
#'               package = "surveyframe")
#' )
#' print(instr)
read_sframe <- function(path, validate = TRUE) {
  if (!file.exists(path)) {
    sframe_abort_import(
      paste0("File not found: '", path, "'."),
      path = path
    )
  }

  raw_text <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) {
      sframe_abort_import(
        paste0("Could not read file: '", path, "'. ", conditionMessage(e)),
        path = path
      )
    }
  )

  raw_json <- paste(raw_text, collapse = "\n")

  parsed <- tryCatch(
    jsonlite::fromJSON(raw_json, simplifyVector = FALSE),
    error = function(e) {
      sframe_abort_import(
        paste0("Failed to parse JSON in '", path, "'. ", conditionMessage(e)),
        path = path
      )
    }
  )

  sframe_validate_parsed_payload(parsed, path)

  # Verify hash
  stored_hash <- parsed$hash$value
  parsed$hash$value <- ""
  computed_hash  <- sframe_hash_payload(parsed)
  legacy_hash    <- sframe_legacy_hash_payload(parsed)

  if (!identical(stored_hash, computed_hash) &&
      !identical(stored_hash, legacy_hash)) {
    sframe_abort_import(
      paste0(
        "Integrity check failed for '", path, "'. ",
        "The file may have been modified after it was written. ",
        "Expected hash: ", computed_hash, ". ",
        "Stored hash: ", stored_hash, "."
      ),
      path = path
    )
  }

  # Reconstruct the sframe object
  instrument <- structure(
    list(
      meta      = within(parsed$meta, {
        authors <- sframe_as_vector(authors, "character")
        languages <- sframe_as_vector(languages, "character")
      }),
      items     = lapply(parsed$items, sframe_restore_item),
      choices   = lapply(parsed$choices, sframe_restore_choices),
      scales    = lapply(parsed$scales, sframe_restore_scale),
      branching = lapply(parsed$branching, sframe_restore_branch),
      checks    = lapply(parsed$checks, sframe_restore_check),
      analysis_plan = lapply(
        parsed$analysis_plan %||% list(),
        sframe_restore_analysis_block
      ),
      models    = lapply(parsed$models %||% list(), sframe_restore_model),
      render    = parsed$render %||% list()
    ),
    class = "sframe"
  )

  if (validate) {
    instrument <- validate_sframe(instrument, strict = TRUE)
  }

  instrument
}
