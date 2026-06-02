# codebook_report.R

#' Generate a survey codebook from an instrument object
#'
#' Produces a structured codebook listing all items, their types, choice sets,
#' scale membership, and reverse-coding status. The codebook can be rendered
#' as HTML or Markdown.
#'
#' @param instrument An `sframe` object.
#' @param format Character. Output format. Either `"html"` or `"md"`.
#'
#' @return An object of class `sframe_codebook`, a list with elements
#'   `instrument_meta`, `items_table`, `choices_table`, and `scales_table`.
#'   Call `print()` to display a compact summary or use `render_report()` to
#'   include the codebook in a full report.
#' @export
#' @seealso [render_report()]
#'
#' @examples
#' cs    <- sf_choices("ag5", 1:5,
#'            c("Strongly disagree", "Disagree", "Neutral",
#'              "Agree", "Strongly agree"))
#' i1    <- sf_item("sat_1", "Item 1", type = "likert",
#'                  choice_set = "ag5", scale_id = "sat")
#' i2    <- sf_item("sat_2", "Item 2", type = "likert",
#'                  choice_set = "ag5", scale_id = "sat")
#' scale <- sf_scale("sat", "Satisfaction", items = c("sat_1", "sat_2"))
#' instr <- sf_instrument("Demo Survey", components = list(cs, i1, i2, scale))
#'
#' cb <- codebook_report(instr)
#' print(cb)
#' nrow(cb$items_table)
#' nrow(cb$scales_table)
codebook_report <- function(instrument, format = c("html", "md")) {
  sframe_check_instrument(instrument)
  format <- rlang::arg_match(format)

  item_ids <- vapply(instrument$items, function(i) i$id, character(1))

  # Build reverse map
  reverse_map <- stats::setNames(
    vapply(instrument$items, function(i) isTRUE(i$reverse), logical(1)),
    item_ids
  )
  scale_map <- stats::setNames(
    vapply(instrument$items, function(i) i$scale_id %||% "", character(1)),
    item_ids
  )

  items_table <- data.frame(
    id         = vapply(instrument$items, function(i) i$id,          character(1)),
    label      = vapply(instrument$items, function(i) i$label,       character(1)),
    type       = vapply(instrument$items, function(i) i$type,        character(1)),
    choice_set = vapply(instrument$items, function(i) i$choice_set %||% "", character(1)),
    scale_id   = vapply(instrument$items, function(i) i$scale_id %||% "", character(1)),
    reverse    = vapply(instrument$items, function(i) isTRUE(i$reverse), logical(1)),
    required   = vapply(instrument$items, function(i) isTRUE(i$required), logical(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  choices_table <- if (length(instrument$choices) > 0) {
    rows <- lapply(instrument$choices, function(cs) {
      data.frame(
        choice_set_id = cs$id,
        value         = as.character(cs$values),
        label         = cs$labels,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })
    do.call(rbind, rows)
  } else {
    data.frame(
      choice_set_id = character(0),
      value = character(0),
      label = character(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  scales_table <- if (length(instrument$scales) > 0) {
    data.frame(
      id     = vapply(instrument$scales, function(s) s$id,    character(1)),
      label  = vapply(instrument$scales, function(s) s$label, character(1)),
      method = vapply(instrument$scales, function(s) s$method, character(1)),
      n_items = vapply(instrument$scales, function(s) length(s$items), integer(1)),
      items  = vapply(instrument$scales, function(s) paste(s$items, collapse = ", "), character(1)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    data.frame(
      id = character(0),
      label = character(0),
      method = character(0),
      n_items = integer(0),
      items = character(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  structure(
    list(
      instrument_meta = instrument$meta,
      items_table     = items_table,
      choices_table   = choices_table,
      scales_table    = scales_table,
      format          = format
    ),
    class = "sframe_codebook"
  )
}

#' @exportS3Method print sframe_codebook
print.sframe_codebook <- function(x, ...) {
  cat(sprintf("Codebook: %s v%s\n", x$instrument_meta$title,
              x$instrument_meta$version))
  cat(sprintf("  %d items  |  %d choice sets  |  %d scales\n",
              nrow(x$items_table), nrow(unique(x$choices_table["choice_set_id"])),
              nrow(x$scales_table)))
  cat("\nItems:\n")
  print(x$items_table[, c("id", "label", "type", "scale_id", "reverse")])
  invisible(x)
}


# render_report.R

#' Render a reproducible survey report
#'
#' Generates an HTML report that includes the instrument codebook, data
#' quality summary, reliability diagnostics, and analysis-plan content.
#' When Quarto and the bundled template are available, the report is rendered
#' through Quarto. Otherwise, surveyframe writes an internal HTML fallback so
#' the reporting workflow still runs on machines without Quarto.
#'
#' @param instrument An `sframe` object.
#' @param data A `tibble` or `data.frame` of responses, or NULL to generate a
#'   codebook-only report.
#' @param output_file Character or NULL. The output file path. When NULL, a
#'   temporary file is written and its path returned.
#' @param output_path Character or NULL. Alias for `output_file`. If both are
#'   supplied, `output_file` takes precedence.
#' @param format Character. Output format. Currently `"html"`.
#' @param include_quality Logical. Whether to include the data quality report.
#'   Requires `data`. Defaults to `TRUE`.
#' @param include_reliability Logical. Whether to include reliability
#'   diagnostics. Requires `data`. Defaults to `TRUE`.
#' @param include_codebook Logical. Whether to include the instrument codebook.
#'   Defaults to `TRUE`.
#' @param include_missing Logical. Whether to include the missing-data report.
#'   Requires `data`. Defaults to `TRUE`.
#' @param include_descriptives Logical. Whether to include descriptive
#'   statistics. Requires `data`. Defaults to `TRUE`.
#' @param include_analysis Logical. Whether to include analysis-plan results
#'   when `data` are supplied and the instrument has an `analysis_plan`.
#' @param include_models Logical. Whether to include saved model JSON and
#'   generated syntax blocks. Defaults to `TRUE`.
#'
#' @return The output file path, invisibly.
#' @export
#' @seealso [codebook_report()], [quality_report()], [reliability_report()]
#'
#' @examples
#' instr <- read_sframe(
#'   system.file("extdata", "tourism_services_demo.sframe",
#'               package = "surveyframe")
#' )
#' responses <- read_responses(
#'   system.file("extdata", "tourism_services_responses.csv",
#'               package = "surveyframe"),
#'   instr,
#'   respondent_id = "respondent_id",
#'   submitted_at = "submitted_at",
#'   meta_cols = "started_at"
#' )
#' old <- options(surveyframe.use_quarto = FALSE)
#' out <- tryCatch(
#'   render_report(
#'     instr,
#'     data = responses,
#'     output_file = tempfile(fileext = ".html"),
#'     include_reliability = FALSE,
#'     include_analysis = FALSE
#'   ),
#'   finally = options(old)
#' )
#' file.exists(out)
render_report <- function(
    instrument,
    data              = NULL,
    output_file       = NULL,
    output_path       = NULL,
    format            = c("html"),
    include_quality   = TRUE,
    include_reliability = TRUE,
    include_codebook  = TRUE,
    include_missing   = TRUE,
    include_descriptives = TRUE,
    include_analysis  = TRUE,
    include_models    = TRUE
) {
  sframe_check_instrument(instrument)

  format <- rlang::arg_match(format)
  dest <- output_file %||% output_path %||% tempfile(fileext = ".html")
  dest <- path.expand(dest)

  output_dir <- dirname(dest)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  output_name <- basename(dest)

  use_quarto <- isTRUE(getOption("surveyframe.use_quarto", TRUE))
  quarto_bin <- Sys.which("quarto")
  has_quarto <- use_quarto && nzchar(quarto_bin)
  template <- system.file("templates", "report.qmd", package = "surveyframe")
  if (has_quarto && file.exists(template)) {
    render_dir <- tempfile("surveyframe-report-")
    dir.create(render_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(render_dir, recursive = TRUE, force = TRUE), add = TRUE)
    render_input <- file.path(render_dir, "report.qmd")
    if (!file.copy(template, render_input, overwrite = TRUE)) {
      rlang::abort(
        "Could not prepare the Quarto report template for rendering.",
        class = "sframe_error"
      )
    }

    # Write instrument and data to temp RDS for the template to read
    tmp_instr <- tempfile(fileext = ".rds")
    tmp_data  <- tempfile(fileext = ".rds")
    temp_rds <- tmp_instr
    saveRDS(instrument, tmp_instr)
    if (!is.null(data)) {
      saveRDS(data, tmp_data)
      temp_rds <- c(temp_rds, tmp_data)
    } else {
      tmp_data <- ""
    }
    on.exit(unlink(temp_rds, force = TRUE), add = TRUE)

    params <- list(
      instrument_path     = tmp_instr,
      data_path           = tmp_data,
      include_quality     = include_quality && !is.null(data),
      include_reliability = include_reliability && !is.null(data),
      include_codebook    = include_codebook,
      include_missing     = include_missing && !is.null(data),
      include_descriptives = include_descriptives && !is.null(data),
      include_analysis    = include_analysis,
      include_models      = include_models,
      instrument_hash     = sframe_hash_value(instrument)
    )

    param_args <- unlist(lapply(names(params), function(name) {
      value <- params[[name]]
      if (is.logical(value)) {
        value <- tolower(as.character(value))
      }
      if (is.character(value)) {
        value <- normalizePath(value, winslash = "/", mustWork = FALSE)
      }
      c("-P", paste0(name, ":", value))
    }), use.names = FALSE)
    quarto_ok <- tryCatch({
      status <- suppressWarnings(system2(
        quarto_bin,
        args = c(
          "render", render_input,
          "--output", output_name,
          "--output-dir", output_dir,
          "--execute-dir", render_dir,
          param_args
        ),
        stdout = TRUE,
        stderr = TRUE
      ))
      is.null(attr(status, "status")) || identical(attr(status, "status"), 0L)
    }, error = function(e) FALSE)

    if (isTRUE(quarto_ok) && file.exists(dest)) {
      return(invisible(dest))
    }
  }

  .render_report_html(
    instrument          = instrument,
    data                = data,
    output_path         = dest,
    include_quality     = include_quality && !is.null(data),
    include_reliability = include_reliability && !is.null(data),
    include_codebook    = include_codebook,
    include_missing     = include_missing && !is.null(data),
    include_descriptives = include_descriptives && !is.null(data),
    include_analysis    = include_analysis,
    include_models      = include_models
  )

  invisible(dest)
}

.render_report_html <- function(
    instrument,
    data = NULL,
    output_path,
    include_quality = TRUE,
    include_reliability = TRUE,
    include_codebook = TRUE,
    include_missing = TRUE,
    include_descriptives = TRUE,
    include_analysis = TRUE,
    include_models = TRUE
) {
  meta <- instrument$meta %||% list()
  sections <- character(0)

  if (isTRUE(include_codebook)) {
    cb <- codebook_report(instrument)
    codebook_parts <- c(
      "<h2>Codebook</h2>",
      .render_report_table(cb$items_table, "Survey items")
    )
    if (nrow(cb$choices_table) > 0) {
      codebook_parts <- c(
        codebook_parts,
        .render_report_table(cb$choices_table, "Choice sets")
      )
    }
    if (nrow(cb$scales_table) > 0) {
      codebook_parts <- c(
        codebook_parts,
        .render_report_table(cb$scales_table, "Scale definitions")
      )
    }
    sections <- c(sections, sprintf("<section>%s</section>", paste(codebook_parts, collapse = "\n")))
  }

  if (isTRUE(include_quality)) {
    qr <- tryCatch(quality_report(data, instrument), error = function(e) e)
    quality_html <- if (inherits(qr, "error")) {
      sprintf("<p>%s</p>", htmltools_escape(conditionMessage(qr)))
    } else {
      summary_tbl <- data.frame(
        Metric = c("Respondents", "Items", "Flagged", "Flag rate"),
        Value = c(
          qr$summary$n_respondents,
          qr$summary$n_items,
          qr$summary$n_flagged,
          sprintf("%.1f%%", qr$summary$flag_rate * 100)
        ),
        stringsAsFactors = FALSE
      )
      paste(
        "<h2>Data Quality</h2>",
        .render_report_table(summary_tbl, "Quality summary"),
        collapse = "\n"
      )
    }
    sections <- c(sections, sprintf("<section>%s</section>", quality_html))
  }

  if (isTRUE(include_missing)) {
    mr <- tryCatch(missing_data_report(data, instrument), error = function(e) e)
    missing_html <- if (inherits(mr, "error")) {
      sprintf("<h2>Missing Data</h2><p>%s</p>", htmltools_escape(conditionMessage(mr)))
    } else {
      paste(
        "<h2>Missing Data</h2>",
        .render_report_table(mr$item_missing, "Item-wise missingness"),
        .render_report_table(mr$respondent_missing, "Respondent-wise missingness"),
        .render_report_table(mr$patterns, "Missing-data patterns"),
        .render_report_table(mr$scale_missing_rules, "Scale missing rules"),
        collapse = "\n"
      )
    }
    sections <- c(sections, sprintf("<section>%s</section>", missing_html))
  }

  if (isTRUE(include_descriptives)) {
    dr <- tryCatch(
      descriptives_report(data, variables = intersect(vapply(instrument$items, function(i) i$id, character(1)), colnames(data))),
      error = function(e) e
    )
    descriptives_html <- if (inherits(dr, "error")) {
      sprintf("<h2>Descriptives</h2><p>%s</p>", htmltools_escape(conditionMessage(dr)))
    } else {
      paste(
        "<h2>Descriptives</h2>",
        .render_report_table(dr$table, "Descriptive statistics"),
        collapse = "\n"
      )
    }
    sections <- c(sections, sprintf("<section>%s</section>", descriptives_html))
  }

  if (isTRUE(include_reliability)) {
    rr <- tryCatch(reliability_report(data, instrument), error = function(e) e)
    reliability_html <- if (inherits(rr, "error")) {
      sprintf(
        "<h2>Reliability</h2><p>%s</p>",
        htmltools_escape(conditionMessage(rr))
      )
    } else if (length(rr) == 0) {
      "<h2>Reliability</h2><p>No scales with enough data were available for reliability analysis.</p>"
    } else {
      rel_tbl <- do.call(
        rbind,
        lapply(rr, function(scale) {
          data.frame(
            Scale = paste0(scale$label, " (", scale$scale_id, ")"),
            Items = scale$n_items,
            N = scale$n,
            Alpha = if (!is.null(scale$alpha)) sprintf("%.3f", scale$alpha) else "n/a",
            Omega_h = if (!is.null(scale$omega_h)) sprintf("%.3f", scale$omega_h) else "n/a",
            Omega_t = if (!is.null(scale$omega_t)) sprintf("%.3f", scale$omega_t) else "n/a",
            stringsAsFactors = FALSE
          )
        })
      )
      paste(
        "<h2>Reliability</h2>",
        "<p>Cronbach alpha and McDonald's omega by scale.</p>",
        .render_report_table(rel_tbl, "Scale reliability"),
        collapse = "\n"
      )
    }
    sections <- c(sections, sprintf("<section>%s</section>", reliability_html))
  }

  if (isTRUE(include_analysis) && length(instrument$analysis_plan %||% list()) > 0) {
    analysis_html <- .render_report_analysis_section(instrument, data)
    sections <- c(sections, sprintf("<section>%s</section>", analysis_html))
  }

  if (isTRUE(include_models) && length(instrument$models %||% list()) > 0) {
    sections <- c(
      sections,
      sprintf("<section>%s</section>", .render_report_model_section(instrument))
    )
  }

  repro_tbl <- data.frame(
    Field = c("Instrument hash", "Generated"),
    Value = c(
      sframe_hash_value(instrument),
      format(Sys.time(), "%Y-%m-%d %H:%M %Z")
    ),
    stringsAsFactors = FALSE
  )
  sections <- c(
    sections,
    sprintf(
      "<section><h2>Reproducibility</h2>%s</section>",
      .render_report_table(repro_tbl, "Reproducibility details")
    )
  )

  title <- htmltools_escape(meta$title %||% "Survey Report")
  subtitle <- sprintf(
    "Version %s",
    htmltools_escape(meta$version %||% "")
  )

  html <- sprintf(
    paste(
      "<!DOCTYPE html>",
      "<html lang=\"en\">",
      "<head>",
      "<meta charset=\"UTF-8\">",
      "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">",
      "<title>%s</title>",
      "<style>",
      "body { font-family: Arial, sans-serif; max-width: 960px; margin: 0 auto; padding: 32px 20px; color: #1f2933; line-height: 1.6; }",
      "h1, h2, h3 { color: #102a43; }",
      "section { margin: 0 0 28px; padding-bottom: 12px; border-bottom: 1px solid #d9e2ec; }",
      "table { width: 100%%; border-collapse: collapse; margin: 16px 0 2px; font-size: .93em; }",
      "caption { text-align: left; font-style: italic; font-size: .95em; margin-bottom: 4px; }",
      "thead tr { border-top: 2px solid #000; border-bottom: 1px solid #000; }",
      "tbody tr:last-child td { border-bottom: 2px solid #000; }",
      "th, td { border: none; padding: 5px 12px; text-align: left; vertical-align: top; }",
      "th { font-weight: 700; background: none; }",
      ".tbl-note { font-size: .85em; font-style: italic; color: #333; margin-top: 3px; margin-bottom: 16px; }",
      ".meta { color: #52606d; margin-bottom: 24px; }",
      ".rq-block { margin: 18px 0; padding: 14px; background: #f8fafc; border: 1px solid #d9e2ec; border-radius: 6px; }",
      ".apa { font-family: Georgia, serif; }",
      "</style>",
      "</head>",
      "<body>",
      "<h1>%s</h1>",
      "<p class=\"meta\">%s</p>",
      "%s",
      "</body>",
      "</html>",
      sep = "\n"
    ),
    title,
    title,
    subtitle,
    paste(sections, collapse = "\n")
  )

  writeLines(html, output_path)
  invisible(output_path)
}

.render_report_analysis_section <- function(instrument, data = NULL) {
  plan <- instrument$analysis_plan %||% list()
  if (length(plan) == 0) {
    return("")
  }

  blocks <- if (is.null(data)) {
    vapply(seq_along(plan), function(i) {
      rq <- plan[[i]]
      sprintf(
        paste(
          "<div class=\"rq-block\">",
          "<h3>RQ %d: %s</h3>",
          "<p><strong>Planned test:</strong> %s</p>",
          "<p><strong>Variables:</strong> %s</p>",
          "</div>",
          sep = "\n"
        ),
        i,
        htmltools_escape(rq$research_question %||% paste("Research Question", i)),
        htmltools_escape(sframe_analysis_method(rq)),
        htmltools_escape(paste(sframe_analysis_vars(rq), collapse = ", "))
      )
    }, character(1))
  } else {
    results <- tryCatch(run_analysis_plan(data, instrument), error = function(e) e)
    if (inherits(results, "error")) {
      return(sprintf(
        "<h2>Analysis Plan</h2><p>%s</p>",
        htmltools_escape(conditionMessage(results))
      ))
    }

    vapply(seq_along(results), function(i) {
      result <- results[[i]]
      table_html <- ""
      if (is.data.frame(result$table)) {
        table_html <- .render_report_table(result$table, "Results table")
      }
      sprintf(
        paste(
          "<div class=\"rq-block\">",
          "<h3>RQ %d: %s</h3>",
          "<p class=\"apa\"><strong>Result:</strong> %s</p>",
          "%s",
          "</div>",
          sep = "\n"
        ),
        i,
        htmltools_escape(result$research_question %||% paste("Research Question", i)),
        htmltools_escape(result$apa %||% result$error %||% ""),
        table_html
      )
    }, character(1))
  }

  paste(c("<h2>Analysis Plan</h2>", blocks), collapse = "\n")
}

.render_report_model_section <- function(instrument) {
  models <- instrument$models %||% list()
  blocks <- vapply(models, function(model) {
    json <- htmltools_escape(model_json(model, pretty = TRUE))
    syntax <- tryCatch(
      switch(
        model$type,
        cfa = cfa_lavaan_syntax(instrument = instrument, model = model),
        cb_sem = sem_lavaan_syntax(model, instrument = instrument),
        pls_sem = seminr_syntax(model),
        efa = efa_syntax(unlist(lapply(sframe_model_constructs(model), function(con) con$items))),
        ""
      ),
      error = function(e) paste0("Model syntax could not be generated: ", conditionMessage(e))
    )
    sprintf(
      paste(
        "<div class=\"rq-block\">",
        "<h3>%s</h3>",
        "<p><strong>Type:</strong> %s &nbsp; <strong>Engine:</strong> %s</p>",
        "<h4>Syntax</h4><pre>%s</pre>",
        "<h4>Model JSON</h4><pre>%s</pre>",
        "</div>",
        sep = "\n"
      ),
      htmltools_escape(model$label %||% model$id),
      htmltools_escape(model$type %||% ""),
      htmltools_escape(model$engine %||% ""),
      htmltools_escape(syntax),
      json
    )
  }, character(1))

  paste(c("<h2>Model Appendix</h2>", blocks), collapse = "\n")
}

.render_report_table <- function(x, caption = NULL, note = NULL) {
  if (!is.data.frame(x) || nrow(x) == 0) {
    return("")
  }

  has_pval <- any(grepl(
    "^p$|^p\\.value$|^p_value$|^Pr\\(>",
    colnames(x),
    ignore.case = TRUE
  ))

  header <- paste(
    sprintf("<th>%s</th>", htmltools_escape(colnames(x))),
    collapse = ""
  )
  rows <- paste(
    apply(x, 1, function(row) {
      cells <- paste(
        sprintf("<td>%s</td>", htmltools_escape(as.character(row))),
        collapse = ""
      )
      sprintf("<tr>%s</tr>", cells)
    }),
    collapse = "\n"
  )

  cap_html <- if (!is.null(caption) && nzchar(caption)) {
    sprintf("<caption>%s</caption>", htmltools_escape(caption))
  } else ""

  tbl_html <- sprintf(
    "<table>%s<thead><tr>%s</tr></thead><tbody>%s</tbody></table>",
    cap_html, header, rows
  )

  note_text <- note %||% if (has_pval) {
    "* p < .05, ** p < .01, *** p < .001"
  } else NULL

  note_html <- if (!is.null(note_text) && nzchar(note_text)) {
    sprintf('<p class="tbl-note"><em>Note.</em> %s</p>',
            htmltools_escape(note_text))
  } else ""

  paste0(tbl_html, note_html)
}
