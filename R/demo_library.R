# R/demo_library.R
# The demo library: 22 small, single-purpose demos that serve twice over, as
# fixtures that point at one method when something breaks, and as worked
# examples somebody can follow for their own survey.
#
# Every demo belongs to one study of an event, its attendees and its sessions,
# so a reader meets a single setting. An event reads as a conference, a
# training day, a health promotion event, a product launch or a community
# meeting, so marketing, management, psychology, public health and health
# science each meet a recognisable version of it.

# The index. Kept here rather than derived from the files so the order and the
# teaching notes are deliberate, and checked against the files by
# tests/testthat/test-demo-library.R, which fails when the 2 drift apart.
sframe_demo_index <- function() {
  d <- function(name, focus, teaches, fields, technique, reuse = NA_character_) {
    data.frame(name = name, focus = focus, teaches = teaches, fields = fields,
               technique = technique, reuse = reuse, stringsAsFactors = FALSE)
  }
  rbind(
    d("first_survey", "analysis", "Design, export, collect, describe",
      "single_choice, numeric, date, text, section_break, text_block",
      "frequency, descriptives, missing_data, quality"),
    d("likert_scale", "analysis", "A scale and whether it holds together",
      "likert", "scale_descriptives, reliability_alpha, reliability_omega, item_diagnostics"),
    d("matrix_likert", "analysis", "A matrix item and the columns it expands into",
      "matrix, single_choice", "descriptives, frequency, crosstab"),
    d("two_group", "analysis", "Two groups on one outcome",
      "single_choice, numeric", "t_test_ind, mann_whitney"),
    d("paired", "analysis", "The same people measured twice",
      "slider, single_choice", "t_test_pair, wilcoxon_pair, mcnemar"),
    d("multi_group", "analysis", "Three groups, a second factor, a covariate",
      "single_choice, numeric", "anova_one, kruskal_wallis, anova_two, ancova"),
    d("repeated", "analysis", "Three measurements on the same people",
      "rating", "repeated_anova, friedman"),
    d("categorical", "analysis", "Two categorical items and their association",
      "single_choice", "crosstab, chi_square, fisher_exact"),
    d("correlation_regression", "analysis", "What goes with what, and how much it explains",
      "numeric, slider",
      "correlation_pearson, correlation_spearman, correlation_kendall, partial_correlation, regression_linear"),
    d("logistic", "analysis", "One predictor against 3 outcome shapes",
      "numeric, single_choice, likert",
      "regression_logistic_binary, regression_logistic_ordinal, regression_logistic_multinomial"),
    d("factor_structure", "analysis", "Whether the data supports the factors you assumed",
      "likert", "efa_readiness, efa_solution"),
    d("sem_pls", "analysis", "A declared path model, and the syntax it generates",
      "likert", "sem_lavaan_syntax, seminr_syntax, mediation"),
    d("branching", "analysis", "Skip logic, and why a skipped question differs from a missing answer",
      "single_choice, numeric, text", "frequency, descriptives"),
    d("open_text", "analysis", "What surveyframe can do with free text",
      "textarea, single_choice",
      "term_freq, ngram_freq, term_context, co_occurrence"),
    d("mcdm_choice", "analysis", "One decision, 10 methods, and whether they agree",
      "pairwise_comparison, criteria_weight",
      "ahp, anp, dematel, topsis, vikor, moora, smart, waspas, promethee, electre"),
    d("small_sample", "analysis", "A defensible answer when n is below 30",
      "single_choice, numeric", "mann_whitney, fisher_exact, firth_logistic"),
    d("multi_response", "analysis", "Items that expand into one column per option",
      "multiple_choice, ranking", "descriptives, cochran_q"),
    d("branded_survey", "presentation", "A welcome page, a logo, a colour and a thank you page",
      "as first_survey", "frequency", "first_survey"),
    d("conversational_survey", "presentation", "One question at a time",
      "as first_survey", "frequency", "first_survey"),
    d("conversational_branching", "presentation", "One question at a time, with skip logic",
      "as branching", "frequency", "branching"),
    d("instrument_revision", "provenance", "Changing an instrument mid-study, on the record",
      "as first_survey", "frequency, descriptives, missing_data, quality", "first_survey"),
    d("verification", "provenance", "Proving a file is the one you think it is",
      "as first_survey", "frequency, descriptives, missing_data, quality", "first_survey")
  )
}

#' List the bundled demos
#'
#' Every demo does one job, so a failure points at one method and a reader can
#' hold the whole questionnaire in view. Use [sframe_demo()] to load one.
#'
#' @return A data frame with 1 row per demo: its `name`, whether its `focus`
#'   is analysis, presentation or provenance, what it `teaches`, the input
#'   `fields` it uses, the statistical `technique` it demonstrates, and the
#'   demo whose responses it reuses, if any.
#' @export
#' @seealso [sframe_demo()], [sframe_demo_branding()], [sframe_demo_qmd()]
#' @examples
#' head(sframe_demos(), 3)
#' subset(sframe_demos(), focus == "provenance")
sframe_demos <- function() {
  sframe_demo_index()
}

sframe_demo_dir <- function() {
  p <- system.file("extdata", "demos", package = "surveyframe")
  if (!nzchar(p) || !dir.exists(p)) {
    rlang::abort(
      "Bundled demos not found. Please reinstall surveyframe.",
      class = "sframe_error")
  }
  p
}

#' Load one bundled demo
#'
#' One of the 22-item teaching library, each demonstrating one analysis
#' method end to end. For the demo that instead exercises every input type
#' SurveyBuilder and SurveyStudio support in a single instrument, see
#' [sframe_input_types_demo_data()].
#'
#' @param name Character. A demo name, as listed by [sframe_demos()].
#' @param branded Logical. When `TRUE`, the instrument comes back with the
#'   standard welcome page, logo, theme colour and thank you page spliced into
#'   its `render` block. The bundled file on disk is left unchanged, so the
#'   same branding can be shown on whichever demo matches your own survey. See
#'   [sframe_demo_branding()].
#'
#' @return A list with `instrument`, `responses`, and the paths behind them:
#'   `instrument_path`, `responses_path`, `codebook_path` and `results_path`.
#'   The codebook carries variable and value labels, so the data means
#'   something outside R. The results table is what surveyframe reports for
#'   this demo, which is the reference to compare against when you run the
#'   same data through another package.
#' @export
#' @seealso [sframe_demos()], [sframe_export_labelled()],
#'   [sframe_input_types_demo_data()]
#' @examples
#' demo <- sframe_demo("two_group")
#' demo$instrument
#' head(demo$responses)
sframe_demo <- function(name, branded = FALSE) {
  idx <- sframe_demo_index()
  if (missing(name)) {
    rlang::abort(
      paste0(
        "`name` is required: pick one of the ", length(idx$name),
        " demo names from sframe_demos(), e.g. sframe_demo(\"",
        idx$name[[1]], "\")."
      ),
      class = "sframe_error")
  }
  if (!isTRUE(name %in% idx$name)) {
    rlang::abort(
      paste0("Unknown demo: '", name, "'. See sframe_demos() for the ",
             length(idx$name), " available."),
      class = "sframe_error")
  }
  dir <- sframe_demo_dir()
  row <- idx[match(name, idx$name), ]
  data_name <- if (is.na(row$reuse)) name else row$reuse

  instrument_path <- file.path(dir, paste0(name, ".sframe"))
  responses_path  <- file.path(dir, paste0(data_name, "_responses.csv"))
  instrument <- read_sframe(instrument_path)
  if (isTRUE(branded)) {
    instrument$render <- sframe_demo_branding()
  }
  # check.names = FALSE matters. A matrix item whose rows carry spaces expands
  # into columns like `session__Opening keynote`, and read.csv()'s default
  # turns that into `session__Opening.keynote`, which no longer matches the
  # column contract the instrument declares. Anyone reading a surveyframe CSV
  # by hand needs the same argument.
  raw <- utils::read.csv(responses_path, stringsAsFactors = FALSE,
                         check.names = FALSE)
  responses <- read_responses(raw, instrument, respondent_id = "respondent_id",
                              meta_cols = c("started_at", "submitted_at"))
  list(instrument = instrument, responses = responses,
       instrument_path = instrument_path, responses_path = responses_path,
       codebook_path = file.path(dir, paste0(name, "_codebook.csv")),
       results_path  = file.path(dir, paste0(name, "_results.csv")))
}

#' The standard demo branding
#'
#' The `render` block [sframe_demo()] splices in when `branded = TRUE`. Read
#' it, change a colour, and paste it into your own instrument.
#'
#' @return A named list suitable for `sf_instrument(render = )`: the display
#'   `mode`, the `theme` colour, the `submit_label`, and the `welcome`,
#'   `thankyou` and `header` blocks.
#' @export
#' @examples
#' branding <- sframe_demo_branding()
#' branding$welcome$title
#' branding$theme
sframe_demo_branding <- function() {
  list(
    mode = "standard",
    theme = "#2563eb",
    submit_label = "Send my feedback",
    welcome = list(
      title = "Thank you for coming",
      intro_text = paste("This takes about 3 minutes. Your answers help us",
                         "plan next year's event."),
      consent_text = "I am happy for my answers to be used in the event report.",
      consent_required = TRUE,
      start_label = "Start"),
    thankyou = list(
      message = "Thank you. Your feedback has been recorded.",
      redirect_url = "",
      show_download = TRUE),
    header = list(
      institution = "Riverside Conference Centre",
      logo_base64 = "",
      show_progress = TRUE)
  )
}

#' Write responses to SPSS or Stata with the labels attached
#'
#' A plain CSV carries codes. `dm_1` arrives in SPSS as a column of integers
#' with no variable label and no value labels, and the reader has to
#' reconstruct all of it from the questionnaire. This attaches both from the
#' instrument: the variable label is the item's `label`, and the value labels
#' come from the item's choice set, which stores its `values` and `labels`
#' side by side.
#'
#' @param data A response data frame, as returned by [read_responses()].
#' @param instrument The `sframe` the responses were collected with.
#' @param path Output file. An `.sav` is written for SPSS, a `.dta` for Stata,
#'   chosen from the extension.
#'
#' @return The path, invisibly.
#' @export
#' @seealso [sframe_demo()], [read_responses()]
#' @examples
#' \donttest{
#' demo <- sframe_demo("two_group")
#' out <- file.path(tempdir(), "two_group.sav")
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   sframe_export_labelled(demo$responses, demo$instrument, out)
#' }
#' }
sframe_export_labelled <- function(data, instrument, path) {
  rlang::check_installed("haven", reason = "to write a labelled SPSS or Stata file.")
  sframe_check_instrument(instrument)
  stopifnot(is.data.frame(data), is.character(path), length(path) == 1)

  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("sav", "dta")) {
    rlang::abort(
      paste0("`path` must end in .sav for SPSS or .dta for Stata, not '",
             ext, "'."),
      class = "sframe_error")
  }

  # sf_choice_sets() gives the sf_choices objects, whose `values` and
  # `labels` sit side by side. sframe_choices_lookup() returns a named
  # character vector instead, which is a different shape.
  lookup <- sf_choice_sets(instrument)
  for (item in instrument$items) {
    if (item$type %in% c("section_break", "text_block")) next
    # An expanding item writes one column per option, so every column starting
    # with the item id takes the item's label.
    cols <- intersect(
      c(item$id, grep(paste0("^", item$id, "__"), names(data), value = TRUE)),
      names(data))
    if (!length(cols)) next
    set <- if (!is.null(item$choice_set)) lookup[[item$choice_set]] else NULL
    for (cn in cols) {
      x <- data[[cn]]
      attr(x, "label") <- item$label
      # Value labels apply where the column holds the choice codes themselves.
      # A multi-select or ranking expansion holds an indicator or a rank, so it
      # keeps the variable label and takes no value labels.
      if (!is.null(set) && identical(cn, item$id) &&
          !item$type %in% c("multiple_choice", "ranking")) {
        vals <- set$values
        labs <- as.character(set$labels)
        if (is.numeric(x) || all(!is.na(suppressWarnings(as.numeric(vals))))) {
          v <- suppressWarnings(as.numeric(vals))
          if (!anyNA(v) && is.numeric(x)) {
            x <- haven::labelled(as.numeric(x), stats::setNames(v, labs))
            attr(x, "label") <- item$label
          }
        } else {
          x <- haven::labelled(as.character(x),
                               stats::setNames(as.character(vals), labs))
          attr(x, "label") <- item$label
        }
      }
      data[[cn]] <- x
    }
  }

  if (identical(ext, "sav")) haven::write_sav(data, path)
  else haven::write_dta(data, path)
  invisible(path)
}

#' Copy a demo's Quarto notebook so you can run and change it
#'
#' The code route through a demo is a notebook that renders to a report, which
#' is what a research workflow looks like. This writes one for the named demo:
#' load the instrument, read the responses, run the pre-declared plan, render
#' the report, and export the data for checking elsewhere.
#'
#' @param name Character. A demo name, as listed by [sframe_demos()].
#' @param dir Directory to write into. Defaults to the working directory.
#' @param overwrite Logical. Overwrite an existing file of the same name.
#'
#' @return The path written, invisibly.
#' @export
#' @seealso [sframe_demo()], [sframe_demos()]
#' @examples
#' out <- sframe_demo_qmd("two_group", dir = tempdir())
#' basename(out)
sframe_demo_qmd <- function(name, dir = ".", overwrite = FALSE) {
  idx <- sframe_demo_index()
  if (missing(name)) {
    rlang::abort(
      paste0(
        "`name` is required: pick one of the ", length(idx$name),
        " demo names from sframe_demos(), e.g. sframe_demo_qmd(\"",
        idx$name[[1]], "\")."
      ),
      class = "sframe_error")
  }
  if (!isTRUE(name %in% idx$name)) {
    rlang::abort(
      paste0("Unknown demo: '", name, "'. See sframe_demos()."),
      class = "sframe_error")
  }
  tpl <- system.file("demos", "analysis.qmd", package = "surveyframe")
  if (!nzchar(tpl)) {
    rlang::abort("Notebook template not found. Please reinstall surveyframe.",
                 class = "sframe_error")
  }
  row <- idx[match(name, idx$name), ]
  txt <- readLines(tpl, warn = FALSE)
  # One skeleton filled per demo rather than 22 near-identical files, so the
  # notebook a reader opens is theirs and there is 1 template to keep current.
  txt <- gsub("{{DEMO_NAME}}", name, txt, fixed = TRUE)
  txt <- gsub("{{DEMO_TITLE}}", row$teaches, txt, fixed = TRUE)
  txt <- gsub("{{DEMO_TEACHES}}", row$teaches, txt, fixed = TRUE)

  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dir, paste0(name, ".qmd"))
  if (file.exists(dest) && !isTRUE(overwrite)) {
    rlang::abort(
      paste0("'", dest, "' already exists. Use overwrite = TRUE to replace it."),
      class = "sframe_error")
  }
  writeLines(txt, dest)
  message("Notebook written to: ", dest)
  invisible(dest)
}

#' Write a Quarto analysis notebook for any instrument
#'
#' Unlike [sframe_demo_qmd()], which only works for one of the bundled demo
#' instruments (it looks `name` up in [sframe_demos()]), this writes a
#' runnable Quarto notebook for any instrument, using its own responses. The
#' notebook reads the instrument and its responses back from 2 companion
#' files written alongside it, so all 3 files must stay together.
#'
#' @param instrument An `sframe` object.
#' @param data A `data.frame` of responses, read by [read_responses()] when
#'   the notebook runs.
#' @param dir Directory to write into. Defaults to the working directory.
#' @param basename Character or `NULL`. File base name shared by the `.qmd`,
#'   `.sframe`, and `_responses.csv` files. Defaults to a slug of the
#'   instrument's title.
#' @param overwrite Logical. Overwrite existing files of the same name.
#'
#' @return A list with `qmd`, `sframe`, and `csv` paths, invisibly.
#' @export
#' @seealso [sframe_demo_qmd()], [write_sframe()], [read_responses()]
#' @examples
#' item  <- sf_item("q1", "How satisfied are you?", type = "text")
#' instr <- sf_instrument("Demo", components = list(item))
#' resp  <- data.frame(q1 = c("Great", "Fine"))
#' out <- sframe_analysis_qmd(instr, resp, dir = tempdir())
#' basename(out$qmd)
sframe_analysis_qmd <- function(instrument, data, dir = ".", basename = NULL,
                                 overwrite = FALSE) {
  if (missing(instrument)) {
    rlang::abort(
      "`instrument` is required: an sframe object, e.g. from sf_instrument() or read_sframe().",
      class = "sframe_error")
  }
  sframe_check_instrument(instrument)
  if (missing(data)) {
    rlang::abort(
      "`data` is required: a data.frame of responses, e.g. from read_responses().",
      class = "sframe_error")
  }
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data.frame of responses.",
                 class = "sframe_error")
  }
  tpl <- system.file("demos", "analysis_general.qmd", package = "surveyframe")
  if (!nzchar(tpl)) {
    rlang::abort("Notebook template not found. Please reinstall surveyframe.",
                 class = "sframe_error")
  }

  title <- sf_meta(instrument)$title %||% "survey"
  slug  <- basename %||% gsub("[^A-Za-z0-9]+", "_", title)
  slug  <- gsub("^_+|_+$", "", slug)
  if (!nzchar(slug)) slug <- "survey"

  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  sframe_path <- file.path(dir, paste0(slug, ".sframe"))
  csv_path    <- file.path(dir, paste0(slug, "_responses.csv"))
  qmd_path    <- file.path(dir, paste0(slug, ".qmd"))
  for (dest in c(sframe_path, csv_path, qmd_path)) {
    if (file.exists(dest) && !isTRUE(overwrite)) {
      rlang::abort(
        paste0("'", dest, "' already exists. Use overwrite = TRUE to replace it."),
        class = "sframe_error")
    }
  }

  write_sframe(instrument, sframe_path, overwrite = overwrite)
  utils::write.csv(data, csv_path, row.names = FALSE, na = "")

  # sframe_path/csv_path are file.path(dir, "<slug>.<ext>"), so their file
  # names are exactly these 2 suffixes on slug; built this way instead of via
  # base::basename() because the `basename` argument above shadows it.
  txt <- readLines(tpl, warn = FALSE)
  txt <- gsub("{{TITLE}}", title, txt, fixed = TRUE)
  txt <- gsub("{{SFRAME_FILE}}", paste0(slug, ".sframe"), txt, fixed = TRUE)
  txt <- gsub("{{CSV_FILE}}", paste0(slug, "_responses.csv"), txt, fixed = TRUE)
  writeLines(txt, qmd_path)

  message(
    "Notebook written to: ", qmd_path, "\n",
    "Instrument and responses written alongside it: ",
    paste0(slug, ".sframe"), ", ", paste0(slug, "_responses.csv"), "\n",
    "Keep all 3 files together, then open and render the notebook."
  )
  invisible(list(qmd = qmd_path, sframe = sframe_path, csv = csv_path))
}

# The analysis methods sframe_run_one_block() actually dispatches on, read out
# of the function itself rather than transcribed into a list beside it.
#
# A hand-maintained list shares the blind spot of the code it claims to
# describe: it looks complete right up to the moment somebody adds a method and
# forgets to add it here. Walking the parsed body keeps the coverage test in
# tests/testthat/test-demo-library.R honest, because a method added tomorrow
# appears here on the next run and fails that test until it has a demo.
#
# The AST is walked rather than the deparsed text. Deparsing reflows long
# branches onto continuation lines, so a text scan found 17 of 59 methods and
# picked up a field name that is not a method at all.
sframe_dispatch_methods <- function() {
  found <- character(0)
  walk <- function(node) {
    if (is.call(node)) {
      if (identical(node[[1]], as.name("switch")) && length(node) > 2) {
        nms <- names(node)[-c(1, 2)]
        found <<- c(found, nms[nzchar(nms %||% "")])
      }
      for (i in seq_along(node)) {
        # A switch branch can be empty, the fall-through form `a = ,`, and
        # indexing one gives the missing argument rather than an error.
        el <- tryCatch(node[[i]], error = function(e) NULL)
        ok <- tryCatch({ force(el); TRUE }, error = function(e) FALSE)
        if (ok && !is.null(el)) walk(el)
      }
    }
  }
  walk(body(sframe_run_one_block))
  setdiff(unique(found), c("error", ""))
}
