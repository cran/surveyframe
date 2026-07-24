# model_layer.R
# Model specification objects and syntax generators for surveyframe.

sframe_model_safe_id <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x) &&
    grepl("^[A-Za-z][A-Za-z0-9_]*$", x)
}

sframe_model_abort_bad_id <- function(arg) {
  rlang::abort(
    paste0(
      "`", arg, "` must start with a letter and contain only letters, ",
      "numbers, and `_` characters."
    ),
    class = c("sframe_validation_error", "sframe_error")
  )
}

sframe_model_check_id <- function(value, arg) {
  if (!sframe_model_safe_id(value)) {
    sframe_model_abort_bad_id(arg)
  }
  invisible(value)
}

sframe_model_check_ids <- function(value, arg, allow_empty = FALSE) {
  if (is.null(value) && isTRUE(allow_empty)) {
    return(invisible(character(0)))
  }
  if (!is.character(value) || anyNA(value) ||
      (!allow_empty && length(value) == 0L) ||
      any(!vapply(value, sframe_model_safe_id, logical(1)))) {
    sframe_model_abort_bad_id(arg)
  }
  invisible(value)
}

sframe_model_as_list <- function(x) {
  if (is.null(x)) {
    return(list())
  }
  if (inherits(x, "sf_model")) {
    return(x)
  }
  if (is.list(x)) {
    return(x)
  }
  rlang::abort("`model` must be an sf_model object or a model list.",
               class = c("sframe_validation_error", "sframe_error"))
}

sframe_constructs_from_scales <- function(instrument, scales = NULL) {
  target <- instrument$scales
  if (!is.null(scales)) {
    target <- Filter(function(s) s$id %in% scales, target)
  }
  lapply(target, function(scale) {
    sf_construct(
      id = scale$id,
      label = scale$label,
      items = scale$items,
      mode = "reflective"
    )
  })
}

sframe_model_constructs <- function(model) {
  model <- sframe_model_as_list(model)
  model$measurement$constructs %||% list()
}

sframe_model_paths <- function(model) {
  model <- sframe_model_as_list(model)
  model$structural$paths %||% list()
}

sframe_model_covariances <- function(model) {
  model <- sframe_model_as_list(model)
  model$structural$covariances %||% list()
}

sframe_model_indirect <- function(model) {
  model <- sframe_model_as_list(model)
  model$structural$indirect %||% list()
}

sframe_model_plain <- function(x) {
  if (is.list(x)) {
    class(x) <- NULL
    x <- lapply(x, sframe_model_plain)
  }
  x
}

sframe_lavaan_indicator_line <- function(con) {
  paste0(con$id, " =~ ", paste(con$items, collapse = " + "))
}

sframe_lavaan_path_label <- function(from, through, to) {
  paste(c(from, through, to), collapse = "_")
}

# Turn a free-text path label into a valid lavaan parameter name. A label
# like "H1: AIA positively influences PEOU" becomes "H1"; anything else is
# reduced to letters, digits, underscores, and dots.
sframe_lavaan_safe_label <- function(label) {
  if (is.null(label) || !nzchar(label)) return(NULL)
  head_tag <- sub("^\\s*([A-Za-z][A-Za-z0-9_.]*)\\s*:.*$", "\\1", label)
  if (!identical(head_tag, label)) return(head_tag)
  if (grepl("^[A-Za-z][A-Za-z0-9_.]*$", label)) return(label)
  out <- gsub("[^A-Za-z0-9_.]+", "_", label)
  out <- gsub("^_+|_+$", "", out)
  if (!grepl("^[A-Za-z]", out)) out <- paste0("p_", out)
  out
}

#' Define a latent or composite construct
#'
#' @param id Construct identifier. Must start with a letter and contain only
#'   letters, numbers, and `_` characters.
#' @param label Human-readable construct label.
#' @param items Character vector of indicator item IDs.
#' @param mode Measurement mode. One of `"reflective"`, `"composite"`,
#'   `"formative"`, or `"single_item"`.
#' @param weights Optional indicator weights for later PLS-SEM planning.
#'
#' @return An object of class `sf_construct`.
#' @export
sf_construct <- function(
    id,
    label = NULL,
    items = character(0),
    mode = c("reflective", "composite", "formative", "single_item"),
    weights = NULL
) {
  mode <- rlang::arg_match(mode)
  sframe_model_check_id(id, "id")
  items <- as.character(items %||% character(0))
  sframe_model_check_ids(items, "items", allow_empty = TRUE)
  structure(
    list(
      id = id,
      label = label %||% id,
      mode = mode,
      items = items,
      weights = weights
    ),
    class = "sf_construct"
  )
}

#' Define a structural path between constructs
#'
#' @param from Source construct ID.
#' @param to Target construct ID.
#' @param label Optional lavaan label for the path.
#'
#' @return An object of class `sf_path`.
#' @export
sf_path <- function(from, to, label = NULL) {
  sframe_model_check_id(from, "from")
  sframe_model_check_id(to, "to")
  structure(
    list(from = from, to = to, label = label),
    class = "sf_path"
  )
}

#' Define a covariance between constructs
#'
#' @param from First construct ID.
#' @param to Second construct ID.
#' @param label Optional label.
#'
#' @return An object of class `sf_covariance`.
#' @export
sf_covariance <- function(from, to, label = NULL) {
  sframe_model_check_id(from, "from")
  sframe_model_check_id(to, "to")
  structure(
    list(from = from, to = to, label = label),
    class = "sf_covariance"
  )
}

#' Define an indirect effect path
#'
#' @param from Source construct ID.
#' @param through Character vector of mediator construct IDs.
#' @param to Target construct ID.
#' @param label Optional effect label.
#'
#' @return An object of class `sf_indirect`.
#' @export
sf_indirect <- function(from, through, to, label = NULL) {
  sframe_model_check_id(from, "from")
  through <- as.character(through)
  sframe_model_check_ids(through, "through")
  sframe_model_check_id(to, "to")
  structure(
    list(from = from, through = through, to = to, label = label),
    class = "sf_indirect"
  )
}

#' Create a surveyframe model specification
#'
#' @param id Model identifier.
#' @param label Human-readable model label.
#' @param type Model type. One of `"efa"`, `"cfa"`, `"cb_sem"`, or
#'   `"pls_sem"`.
#' @param engine Optional engine name. Defaults to `"lavaan"` for CFA/CB-SEM
#'   and `"seminr"` for PLS-SEM.
#' @param constructs List of [sf_construct()] objects.
#' @param paths List of [sf_path()] objects.
#' @param covariances List of [sf_covariance()] objects.
#' @param indirect List of [sf_indirect()] objects.
#' @param options List of model options, such as `estimator`, `missing`,
#'   `bootstrap`, or `standardised`.
#'
#' @return An object of class `sf_model`.
#' @export
sf_model <- function(
    id,
    label = NULL,
    type = c("efa", "cfa", "cb_sem", "pls_sem"),
    engine = NULL,
    constructs = list(),
    paths = list(),
    covariances = list(),
    indirect = list(),
    options = list()
) {
  type <- rlang::arg_match(type)
  sframe_model_check_id(id, "id")
  engine <- engine %||% switch(
    type,
    efa = "psych",
    cfa = "lavaan",
    cb_sem = "lavaan",
    pls_sem = "seminr"
  )
  structure(
    list(
      id = id,
      label = label %||% id,
      type = type,
      engine = engine,
      measurement = list(constructs = constructs %||% list()),
      structural = list(
        paths = paths %||% list(),
        covariances = covariances %||% list(),
        indirect = indirect %||% list()
      ),
      options = options %||% list()
    ),
    class = "sf_model"
  )
}

#' Validate a surveyframe model specification
#'
#' Checks model IDs, construct IDs, indicators, structural path endpoints,
#' duplicate paths, indirect paths, and engine/type compatibility.
#'
#' @param model An [sf_model()] object or compatible list.
#' @param instrument Optional `sframe` object. When supplied, model indicators
#'   must match instrument item IDs.
#' @param strict Logical. When `TRUE`, invalid models raise an error. When
#'   `FALSE`, a list with `valid` and `problems` is returned.
#'
#' @return The model invisibly when valid and `strict = TRUE`, otherwise a
#'   validation result list.
#' @export
validate_model <- function(model, instrument = NULL, strict = TRUE) {
  model <- sframe_model_as_list(model)
  problems <- character(0)

  if (!sframe_model_safe_id(model$id %||% "")) {
    problems <- c(problems,
      "Model ID must start with a letter and contain only letters, numbers, and `_` characters.")
  }

  valid_types <- c("efa", "cfa", "cb_sem", "pls_sem")
  if (!identical(model$type %in% valid_types, TRUE)) {
    problems <- c(problems,
      paste0("Model type must be one of: ", paste(valid_types, collapse = ", "), "."))
  }

  constructs <- sframe_model_constructs(model)
  if ((model$type %||% "") %in% c("cfa", "cb_sem", "pls_sem") &&
      length(constructs) == 0) {
    problems <- c(problems, "Model must contain at least one construct.")
  }
  construct_ids <- vapply(constructs, function(x) x$id %||% "", character(1))
  missing_construct_ids <- which(!nzchar(construct_ids))
  if (length(missing_construct_ids) > 0) {
    problems <- c(problems, "Every construct must have an ID.")
  }
  bad_construct_ids <- construct_ids[nzchar(construct_ids) & !grepl("^[A-Za-z][A-Za-z0-9_]*$", construct_ids)]
  if (length(bad_construct_ids) > 0) {
    problems <- c(problems,
      paste0("Invalid construct ID(s): ", paste(unique(bad_construct_ids), collapse = ", "), "."))
  }
  dup_constructs <- construct_ids[nzchar(construct_ids) & duplicated(construct_ids)]
  if (length(dup_constructs) > 0) {
    problems <- c(problems,
      paste0("Duplicate construct ID(s): ", paste(unique(dup_constructs), collapse = ", "), "."))
  }

  item_ids <- if (!is.null(instrument) && inherits(instrument, "sframe")) {
    vapply(instrument$items, function(item) item$id, character(1))
  } else {
    NULL
  }

  for (con in constructs) {
    mode <- con$mode %||% "reflective"
    if (!mode %in% c("reflective", "composite", "formative", "single_item")) {
      problems <- c(problems,
        paste0("Construct '", con$id, "' uses unrecognised mode '", mode, "'."))
    }
    indicators <- as.character(con$items %||% character(0))
    if (length(indicators) == 0) {
      problems <- c(problems, paste0("Construct '", con$id, "' has no indicators."))
    }
    if (!is.null(item_ids)) {
      missing_items <- setdiff(indicators, item_ids)
      if (length(missing_items) > 0) {
        problems <- c(problems,
          paste0("Construct '", con$id, "' references unknown item(s): ",
                 paste(missing_items, collapse = ", "), "."))
      }
    }
    if (mode != "single_item" && length(indicators) < 2) {
      problems <- c(problems,
        paste0("Construct '", con$id, "' should have at least two indicators."))
    }
    if ((model$type %||% "") %in% c("cfa", "cb_sem") && mode %in% c("formative", "composite")) {
      problems <- c(problems,
        paste0("lavaan syntax generation for model '", model$id,
               "' supports reflective or single-item construct modes. ",
               "Construct '", con$id, "' uses mode '", mode, "'."))
    }
  }

  paths <- sframe_model_paths(model)
  if ((model$type %||% "") == "pls_sem" && length(paths) == 0) {
    problems <- c(problems, "PLS-SEM models require at least one structural path.")
  }
  path_keys <- character(0)
  for (path in paths) {
    if (!path$from %in% construct_ids) {
      problems <- c(problems,
        paste0("Path source '", path$from, "' does not match any construct."))
    }
    if (!path$to %in% construct_ids) {
      problems <- c(problems,
        paste0("Path target '", path$to, "' does not match any construct."))
    }
    if (identical(path$from, path$to)) {
      problems <- c(problems,
        paste0("Path '", path$from, " -> ", path$to, "' is a self-path."))
    }
    path_keys <- c(path_keys, paste(path$from, path$to, sep = "->"))
  }
  dup_paths <- path_keys[duplicated(path_keys)]
  if (length(dup_paths) > 0) {
    problems <- c(problems,
      paste0("Duplicate structural path(s): ", paste(unique(dup_paths), collapse = ", "), "."))
  }

  for (cov in sframe_model_covariances(model)) {
    if (!cov$from %in% construct_ids || !cov$to %in% construct_ids) {
      problems <- c(problems,
        paste0("Covariance '", cov$from, " ~~ ", cov$to,
               "' references an unknown construct."))
    }
    if (identical(cov$from, cov$to)) {
      problems <- c(problems,
        paste0("Covariance '", cov$from, " ~~ ", cov$to, "' is a self-covariance."))
    }
  }

  for (ind in sframe_model_indirect(model)) {
    refs <- c(ind$from, ind$through, ind$to)
    missing_refs <- setdiff(refs, construct_ids)
    if (length(missing_refs) > 0) {
      problems <- c(problems,
        paste0("Indirect effect references unknown construct(s): ",
               paste(missing_refs, collapse = ", "), "."))
    }
    if (length(ind$through %||% character(0)) == 0) {
      problems <- c(problems,
        paste0("Indirect effect from '", ind$from, "' to '", ind$to,
               "' must include at least one mediator."))
    }
  }

  if ((model$type %||% "") == "pls_sem" && !(model$engine %||% "") %in% c("seminr", "plspm")) {
    problems <- c(problems, "PLS-SEM models should use engine 'seminr' in v0.3.")
  }
  if ((model$type %||% "") %in% c("cfa", "cb_sem") && !(model$engine %||% "") %in% c("lavaan")) {
    problems <- c(problems, "CFA and CB-SEM models should use engine 'lavaan' in v0.3.")
  }

  if (strict && length(problems) > 0) {
    sframe_abort_validation(
      paste0(
        "Model validation failed with ", length(problems), " problem(s):\n",
        paste0("  - ", problems, collapse = "\n")
      )
    )
  }

  if (!strict) {
    return(list(valid = length(problems) == 0, problems = problems))
  }

  invisible(model)
}

#' Serialise a model specification to JSON
#'
#' @param model An [sf_model()] object.
#' @param pretty Logical. Whether to pretty-print the JSON.
#'
#' @return A JSON string.
#' @export
model_json <- function(model, pretty = TRUE) {
  model <- sframe_model_as_list(model)
  model <- sframe_model_plain(model)
  jsonlite::toJSON(model, auto_unbox = TRUE, pretty = pretty, null = "null")
}

#' Add a model specification to an instrument
#'
#' @param instrument An `sframe` object.
#' @param model An [sf_model()] object.
#' @param validate Logical. Whether to validate the model against the
#'   instrument before adding it.
#' @param replace Logical. Whether to replace an existing model with the same
#'   ID. Defaults to `TRUE`.
#'
#' @return The updated `sframe` object.
#' @export
add_model <- function(instrument, model, validate = TRUE, replace = TRUE) {
  sframe_check_instrument(instrument)
  if (isTRUE(validate)) {
    validate_model(model, instrument = instrument, strict = TRUE)
  }
  if (is.null(instrument$models)) {
    instrument$models <- list()
  }
  existing <- vapply(instrument$models, function(x) x$id %||% "", character(1))
  idx <- match(model$id, existing)
  if (!is.na(idx)) {
    if (!isTRUE(replace)) {
      sframe_abort_validation(
        paste0("A model with ID '", model$id, "' already exists.")
      )
    }
    instrument$models[[idx]] <- model
  } else {
    instrument$models <- c(instrument$models, list(model))
  }
  instrument$meta$validated <- FALSE
  instrument
}

#' Estimate an exploratory factor solution
#'
#' Runs `psych::fa()` on selected item columns and returns loadings,
#' communalities, uniqueness, variance summaries, and simple item retention
#' flags. The `psych` package is optional and is only required when this
#' function is called.
#'
#' @param data A data.frame of responses.
#' @param instrument An `sframe` object.
#' @param items Character vector of item IDs. When `NULL`, scale items are used.
#' @param scales Optional scale IDs used to select item columns.
#' @param nfactors Number of factors.
#' @param extraction Extraction method passed to `psych::fa()`.
#' @param rotation Rotation method passed to `psych::fa()`.
#' @param min_loading Minimum salient loading.
#' @param cross_loading Maximum secondary loading before a warning is raised.
#'
#' @return An object of class `sframe_efa_solution`. Alongside the psych
#'   objects it carries three tidy data frames ready for plotting and
#'   reporting: `loadings_long` (item_id, factor, loading),
#'   `communalities_table` (item_id, communality, uniqueness), and
#'   `variance_table` (factor, ss_loadings, proportion_var,
#'   cumulative_var).
#' @export
efa_solution <- function(
    data,
    instrument,
    items = NULL,
    scales = NULL,
    nfactors = 1L,
    extraction = c("minres", "pa", "ml"),
    rotation = c("oblimin", "promax", "varimax"),
    min_loading = 0.30,
    cross_loading = 0.30
) {
  sframe_check_instrument(instrument)
  stopifnot(is.data.frame(data))
  extraction <- rlang::arg_match(extraction)
  rotation <- rlang::arg_match(rotation)
  sframe_require_psych("to estimate an EFA solution")

  if (is.null(items)) {
    target_scales <- instrument$scales
    if (!is.null(scales)) {
      target_scales <- Filter(function(s) s$id %in% scales, target_scales)
    }
    items <- unique(unlist(lapply(target_scales, function(s) s$items)))
  }
  cols <- intersect(items, colnames(data))
  if (length(cols) < 2) {
    rlang::abort("EFA solution requires at least two available item columns.",
                 class = c("sframe_validation_error", "sframe_error"))
  }
  item_data <- as.data.frame(lapply(data[, cols, drop = FALSE], function(x) {
    suppressWarnings(as.numeric(x))
  }))
  item_data <- item_data[stats::complete.cases(item_data), , drop = FALSE]
  if (nrow(item_data) < 3) {
    rlang::abort("EFA solution requires at least three complete rows.",
                 class = c("sframe_validation_error", "sframe_error"))
  }

  # psych::fa() can print "R was not square, finding R from data" and warn;
  # keep it quiet so reports and the console stay clean.
  invisible(utils::capture.output(
    fit <- suppressWarnings(suppressMessages(psych::fa(
      item_data,
      nfactors = nfactors,
      fm = extraction,
      rotate = rotation
    )))
  ))
  loadings <- as.data.frame(unclass(fit$loadings))
  loadings$item_id <- rownames(loadings)
  loadings <- loadings[, c("item_id", setdiff(names(loadings), "item_id")),
                       drop = FALSE]

  loading_matrix <- as.matrix(loadings[, setdiff(names(loadings), "item_id"),
                                       drop = FALSE])
  abs_load <- abs(loading_matrix)
  max_load <- apply(abs_load, 1, max, na.rm = TRUE)
  second_load <- apply(abs_load, 1, function(x) {
    sx <- sort(x, decreasing = TRUE)
    if (length(sx) < 2) 0 else sx[[2]]
  })
  item_flags <- data.frame(
    item_id = loadings$item_id,
    max_loading = as.numeric(max_load),
    second_loading = as.numeric(second_load),
    low_loading = as.numeric(max_load) < min_loading,
    cross_loading = as.numeric(second_load) >= cross_loading,
    retain = as.numeric(max_load) >= min_loading &
      as.numeric(second_load) < cross_loading,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Tidy companions to the psych objects above, additive so nothing that
  # reads the original keys changes: long loadings for the heatmap, one row
  # per item for communalities, one row per factor for variance explained.
  factor_names <- colnames(loading_matrix)
  loadings_long <- data.frame(
    item_id = rep(loadings$item_id, times = length(factor_names)),
    factor = rep(factor_names, each = nrow(loadings)),
    loading = as.numeric(loading_matrix),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  communalities_table <- data.frame(
    item_id = names(fit$communality),
    communality = as.numeric(fit$communality),
    uniqueness = as.numeric(fit$uniquenesses[names(fit$communality)]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  va <- fit$Vaccounted
  variance_table <- data.frame(
    factor = colnames(va),
    ss_loadings = as.numeric(va["SS loadings", ]),
    proportion_var = as.numeric(va["Proportion Var", ]),
    cumulative_var = if ("Cumulative Var" %in% rownames(va)) {
      as.numeric(va["Cumulative Var", ])
    } else {
      cumsum(as.numeric(va["Proportion Var", ]))
    },
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  out <- structure(
    list(
      method = extraction,
      rotation = rotation,
      nfactors = as.integer(nfactors),
      n = nrow(item_data),
      n_items = length(cols),
      loadings = loadings,
      loadings_long = loadings_long,
      communalities = fit$communality,
      communalities_table = communalities_table,
      uniqueness = fit$uniquenesses,
      factor_correlations = fit$Phi,
      variance_explained = fit$Vaccounted,
      variance_table = variance_table,
      item_flags = item_flags,
      warnings = item_flags$item_id[item_flags$low_loading | item_flags$cross_loading]
    ),
    class = "sframe_efa_solution"
  )
  out
}

#' @exportS3Method print sframe_efa_solution
print.sframe_efa_solution <- function(x, ...) {
  cat("EFA Solution\n\n")
  cat(sprintf("  Method: %s   Rotation: %s   Factors: %d\n",
              x$method, x$rotation, x$nfactors))
  cat(sprintf("  Complete cases: %d   Items: %d\n\n", x$n, x$n_items))
  print(x$loadings)
  if (length(x$warnings) > 0) {
    cat("\nReview item(s): ", paste(x$warnings, collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

#' Generate EFA planning syntax
#'
#' @param items Character vector of item IDs.
#' @param nfactors Number of factors.
#' @param extraction Extraction method.
#' @param rotation Rotation method.
#' @param data_name Name of the data object in generated R code.
#'
#' @return A character string with R syntax.
#' @export
efa_syntax <- function(
    items,
    nfactors = 1L,
    extraction = c("minres", "pa", "ml"),
    rotation = c("oblimin", "promax", "varimax"),
    data_name = "data"
) {
  extraction <- rlang::arg_match(extraction)
  rotation <- rlang::arg_match(rotation)
  cols <- paste(sprintf('"%s"', items), collapse = ", ")
  paste(
    "# EFA syntax generated by surveyframe",
    "rlang::check_installed(\"psych\", reason = \"to estimate an EFA solution\")",
    sprintf("efa_items <- %s[, c(%s)]", data_name, cols),
    sprintf("psych::fa(efa_items, nfactors = %d, fm = \"%s\", rotate = \"%s\")",
            as.integer(nfactors), extraction, rotation),
    sep = "\n"
  )
}

#' Generate lavaan CFA syntax
#'
#' @param instrument Optional `sframe` object used to derive constructs from
#'   scales when `model` is not supplied.
#' @param model Optional [sf_model()] object.
#' @param scales Optional scale IDs when deriving a model from an instrument.
#' @param ordered Logical. Whether to add an ordered-item note.
#' @param std_lv Logical. Whether to add a `std.lv = TRUE` note.
#' @param residual_covariances Optional list of [sf_covariance()] objects for
#'   correlated residuals.
#' @param latent_covariances Logical. Whether to include model-level latent
#'   covariances supplied in `model`.
#'
#' @return A lavaan syntax string.
#' @export
cfa_lavaan_syntax <- function(
    instrument = NULL,
    model = NULL,
    scales = NULL,
    ordered = FALSE,
    std_lv = TRUE,
    residual_covariances = NULL,
    latent_covariances = TRUE
) {
  if (is.null(model)) {
    if (is.null(instrument) || !inherits(instrument, "sframe")) {
      rlang::abort("Provide either `model` or an `sframe` instrument.",
                   class = c("sframe_validation_error", "sframe_error"))
    }
    model <- sf_model(
      id = "cfa_model",
      label = paste0(instrument$meta$title, " CFA"),
      type = "cfa",
      constructs = sframe_constructs_from_scales(instrument, scales)
    )
  }
  if (!is.null(instrument)) {
    validate_model(model, instrument = instrument, strict = TRUE)
  } else {
    validate_model(model, strict = TRUE)
  }

  constructs <- sframe_model_constructs(model)
  reverse_items <- character(0)
  if (!is.null(instrument) && inherits(instrument, "sframe")) {
    reverse_items <- vapply(instrument$items, function(i) {
      if (isTRUE(i$reverse)) i$id else NA_character_
    }, character(1))
    reverse_items <- reverse_items[!is.na(reverse_items)]
  }

  lines <- c(
    "# lavaan CFA syntax generated by surveyframe",
    paste0("# Model: ", model$label %||% model$id),
    if (std_lv) "# Recommended fitting option: std.lv = TRUE" else NULL,
    if (ordered) "# Ordered-item option: pass ordered = c(...) to lavaan::cfa()" else NULL,
    "# Fit with lavaan only when lavaan is installed.",
    ""
  )
  for (con in constructs) {
    lines <- c(
      lines,
      paste0("# ", con$label %||% con$id, " (", con$mode %||% "reflective", ")")
    )
    rev <- intersect(con$items, reverse_items)
    if (length(rev) > 0) {
      lines <- c(lines, paste0("# Reverse-coded indicators: ", paste(rev, collapse = ", ")))
    }
    lines <- c(lines, sframe_lavaan_indicator_line(con), "")
  }
  covs <- c(
    if (isTRUE(latent_covariances)) sframe_model_covariances(model) else list(),
    residual_covariances %||% list()
  )
  if (length(covs) > 0) {
    lines <- c(lines, "# Covariances")
    for (cov in covs) {
      lines <- c(lines, paste0(cov$from, " ~~ ", cov$to))
    }
  }
  paste(lines, collapse = "\n")
}

#' Generate lavaan CB-SEM syntax
#'
#' @param model An [sf_model()] object of type `"cb_sem"`.
#' @param instrument Optional `sframe` object for indicator validation.
#' @param standardised Logical. Adds a standardised-estimates fitting note.
#'
#' @return A lavaan syntax string.
#' @export
sem_lavaan_syntax <- function(model, instrument = NULL, standardised = TRUE) {
  if (!is.null(instrument)) {
    validate_model(model, instrument = instrument, strict = TRUE)
  } else {
    validate_model(model, strict = TRUE)
  }
  constructs <- sframe_model_constructs(model)
  paths <- sframe_model_paths(model)
  covs <- sframe_model_covariances(model)
  indirect <- sframe_model_indirect(model)

  lines <- c(
    "# lavaan CB-SEM syntax generated by surveyframe",
    paste0("# Model: ", model$label %||% model$id),
    if (standardised) "# Recommended summary option: standardized = TRUE" else NULL,
    if (!is.null(model$options$estimator)) paste0("# Estimator: ", model$options$estimator) else NULL,
    if (!is.null(model$options$missing)) paste0("# Missing data method: ", model$options$missing) else NULL,
    ""
  )
  lines <- c(lines, vapply(constructs, sframe_lavaan_indicator_line, character(1)), "")

  if (length(paths) > 0) {
    lines <- c(lines, "# Structural paths")
    incoming <- split(paths, vapply(paths, function(p) p$to, character(1)))
    for (target in names(incoming)) {
      rhs <- vapply(incoming[[target]], function(path) {
        safe <- sframe_lavaan_safe_label(path$label)
        if (!is.null(safe)) {
          paste0(safe, "*", path$from)
        } else {
          path$from
        }
      }, character(1))
      lines <- c(lines, paste0(target, " ~ ", paste(rhs, collapse = " + ")))
    }
    lines <- c(lines, "")
  }

  if (length(covs) > 0) {
    lines <- c(lines, "# Covariances")
    for (cov in covs) {
      lines <- c(lines, paste0(cov$from, " ~~ ", cov$to))
    }
    lines <- c(lines, "")
  }

  if (length(indirect) > 0) {
    lines <- c(lines, "# Indirect and total effects")
    for (ind in indirect) {
      nodes <- c(ind$from, ind$through, ind$to)
      edges <- paste(utils::head(nodes, -1), utils::tail(nodes, -1), sep = "->")
      labels <- vapply(edges, function(edge) {
        hit <- paths[vapply(paths, function(path) {
          identical(edge, paste(path$from, path$to, sep = "->"))
        }, logical(1))]
        if (length(hit) && !is.null(hit[[1]]$label) && nzchar(hit[[1]]$label)) {
          sframe_lavaan_safe_label(hit[[1]]$label)
        } else {
          gsub("[^A-Za-z0-9_]", "_", edge)
        }
      }, character(1))
      effect_name <- sframe_lavaan_safe_label(ind$label) %||%
        paste0("indirect_", sframe_lavaan_path_label(ind$from, ind$through, ind$to))
      lines <- c(lines, paste0(effect_name, " := ", paste(labels, collapse = "*")))
      direct_key <- paste(ind$from, ind$to, sep = "->")
      direct <- paths[vapply(paths, function(path) {
        identical(direct_key, paste(path$from, path$to, sep = "->"))
      }, logical(1))]
      if (length(direct) && !is.null(direct[[1]]$label) && nzchar(direct[[1]]$label)) {
        lines <- c(lines,
          paste0("total_", ind$from, "_", ind$to, " := ",
                 sframe_lavaan_safe_label(direct[[1]]$label), " + ", effect_name))
      }
    }
  }

  paste(lines, collapse = "\n")
}

#' Generate seminr PLS-SEM syntax
#'
#' @param model An [sf_model()] object of type `"pls_sem"`.
#' @param data_name Name of the data object in generated R code.
#' @param nboot Number of bootstrap samples.
#' @param seed Random seed for bootstrap syntax.
#'
#' @return An R syntax string for `seminr`.
#' @export
seminr_syntax <- function(model, data_name = "data", nboot = NULL, seed = 123) {
  validate_model(model, strict = TRUE)
  constructs <- sframe_model_constructs(model)
  paths <- sframe_model_paths(model)
  nboot <- nboot %||% model$options$bootstrap %||% 5000L

  construct_lines <- vapply(constructs, function(con) {
    items <- con$items
    if (length(items) == 1L || identical(con$mode, "single_item")) {
      sprintf('  composite("%s", single_item("%s"))', con$id, items[[1]])
    } else {
      prefix <- sub("[0-9]+$", "", items[[1]])
      suffixes <- suppressWarnings(as.integer(sub(paste0("^", prefix), "", items)))
      if (all(!is.na(suffixes)) && identical(items, paste0(prefix, suffixes))) {
        sprintf('  composite("%s", multi_items("%s", %s))',
                con$id, prefix, paste(range(suffixes), collapse = ":"))
      } else {
        sprintf('  composite("%s", c(%s))',
                con$id, paste(sprintf('"%s"', items), collapse = ", "))
      }
    }
  }, character(1))

  path_groups <- split(paths, vapply(paths, function(path) path$from, character(1)))
  path_lines <- vapply(names(path_groups), function(from) {
    tos <- vapply(path_groups[[from]], function(path) path$to, character(1))
    sprintf('  paths(from = "%s", to = c(%s))',
            from, paste(sprintf('"%s"', tos), collapse = ", "))
  }, character(1))

  paste(
    "# seminr PLS-SEM syntax generated by surveyframe",
    "rlang::check_installed(\"seminr\", reason = \"to fit PLS-SEM models\")",
    "library(seminr)",
    "",
    "measurement_model <- constructs(",
    paste(construct_lines, collapse = ",\n"),
    ")",
    "",
    "structural_model <- relationships(",
    paste(path_lines, collapse = ",\n"),
    ")",
    "",
    "pls_model <- estimate_pls(",
    sprintf("  data = %s,", data_name),
    "  measurement_model = measurement_model,",
    "  structural_model = structural_model",
    ")",
    "",
    "boot_model <- bootstrap_model(",
    "  seminr_model = pls_model,",
    sprintf("  nboot = %d,", as.integer(nboot)),
    "  cores = 1,",
    sprintf("  seed = %d", as.integer(seed)),
    ")",
    "",
    "# Measurement assessment: reliability, AVE, and HTMT discriminant validity",
    "model_summary <- summary(pls_model)",
    "model_summary$reliability",
    "model_summary$validity$htmt",
    "",
    "# Bootstrapped structural paths",
    "boot_summary <- summary(boot_model)",
    "boot_summary$bootstrapped_paths",
    sep = "\n"
  )
}

#' Create a model reporting template
#'
#' @param model An [sf_model()] object.
#' @param include_json Logical. Whether to include the JSON schema block.
#'
#' @return A character string.
#' @export
model_report_template <- function(model, include_json = TRUE) {
  validate_model(model, strict = TRUE)
  constructs <- sframe_model_constructs(model)
  paths <- sframe_model_paths(model)
  parts <- c(
    paste0("# Model: ", model$label %||% model$id),
    "",
    paste0("Type: ", model$type),
    paste0("Engine: ", model$engine),
    "",
    "## Constructs",
    paste0(
      "- ", vapply(constructs, function(con) {
        paste0(con$id, " (", con$mode, "): ", paste(con$items, collapse = ", "))
      }, character(1)),
      collapse = "\n"
    ),
    "",
    "## Structural Paths",
    if (length(paths)) {
      paste0(
        "- ", vapply(paths, function(path) paste0(path$from, " -> ", path$to), character(1)),
        collapse = "\n"
      )
    } else {
      "No structural paths specified."
    },
    "",
    "## Reporting Notes",
    "Report estimator, missing-data handling, fit indices, standardised path estimates, indirect effects, reliability, validity, and any item-retention decisions."
  )
  if (isTRUE(include_json)) {
    parts <- c(parts, "", "## Model JSON", "```json", model_json(model, pretty = TRUE), "```")
  }
  paste(parts, collapse = "\n")
}
