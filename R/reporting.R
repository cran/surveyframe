# codebook_report.R

# Remove the scratch directories Chrome leaves in tempdir() after a
# chrome_print() call. `before` is the listing taken immediately before the
# call, so only entries created during it are considered, and of those only
# the ones matching Chrome's own naming are removed.
sframe_clean_chrome_detritus <- function(before) {
  after <- list.files(tempdir(), all.files = TRUE, no.. = TRUE)
  new   <- setdiff(after, before)
  chrome <- grep("^(com\\.google\\.Chrome\\.|scoped_dir|\\.com\\.google\\.Chrome\\.)",
                 new, value = TRUE)
  if (length(chrome)) {
    unlink(file.path(tempdir(), chrome), recursive = TRUE, force = TRUE)
  }
  invisible(chrome)
}

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
#' nrow(sf_items(cb))
#' nrow(sf_scales(cb))
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

  # The 5 tables come from the shared builders in as_data_frame.R, so the
  # codebook and as.data.frame() on the instrument cannot drift apart.
  items_table <- sframe_items_table(instrument)

  choices_table <- sframe_choices_table(instrument)

  scales_table <- sframe_scales_table(instrument)

  # The pre-declared analysis plan and the measurement models belong in the
  # codebook, so one document fully records the instrument a study used.
  plan_table <- sframe_plan_table(instrument)

  models_table <- sframe_models_table(instrument)

  structure(
    list(
      instrument_meta = instrument$meta,
      items_table     = items_table,
      choices_table   = choices_table,
      scales_table    = scales_table,
      plan_table      = plan_table,
      models_table    = models_table,
      format          = format
    ),
    class = "sframe_codebook"
  )
}

#' Enrich a codebook's items table for display
#'
#' Replaces `items_table`'s `choice_set` id with the choice set's actual
#' response options ("1 = Strongly disagree; 2 = Disagree; ...") and its
#' `scale_id` with the scale's label, so each row of the printed codebook is
#' self-contained. [codebook_report()] itself keeps the raw ids, for joining
#' `items_table` to `choices_table`/`scales_table` programmatically. This
#' enrichment is for the rendered document, where a reader should not need
#' to cross-reference a separate table just to see what "1" means on a
#' scale shared by many items.
#'
#' @param cb An `sframe_codebook` object from [codebook_report()].
#' @return A data.frame, `cb$items_table` with `choice_set` and `scale_id`
#'   replaced by display text.
#' @export
#' @seealso [codebook_report()]
sframe_codebook_items_display <- function(cb) {
  stopifnot(inherits(cb, "sframe_codebook"))
  items_d <- cb$items_table
  scale_label_of <- function(id) {
    if (is.null(id) || !nzchar(id)) return("")
    hit <- cb$scales_table$label[cb$scales_table$id == id]
    if (length(hit)) hit[1] else id
  }
  options_of <- function(cs_id) {
    if (is.null(cs_id) || !nzchar(cs_id)) return("")
    rows <- cb$choices_table[cb$choices_table$choice_set_id == cs_id, , drop = FALSE]
    if (nrow(rows) == 0) return(cs_id)
    paste(sprintf("%s = %s", rows$value, rows$label), collapse = "; ")
  }
  items_d$scale_id <- vapply(items_d$scale_id, scale_label_of, character(1))
  items_d$choice_set <- vapply(items_d$choice_set, options_of, character(1))
  items_d
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
#' @param format Character. Output format: `"html"` (default) or `"pdf"`.
#'   PDF output renders the HTML report and prints it through
#'   [pagedown::chrome_print()], which requires the pagedown package (in
#'   Suggests) and a local Chrome or Chromium installation. The HTML path
#'   is unchanged.
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
#' @param plot_palette One of `"web"` (brand colours, for on-screen reading)
#'   or `"print"` (black, grey, and white, for a journal-ready or
#'   print-friendly report). Applied to every chart the report embeds.
#'   See `sframe_brand()`.
#' @param interpretations Named list or NULL. Written interpretations keyed
#'   by analysis-plan block id, added after the results are known. When a
#'   block has an entry, its report section shows the pre-declared decision
#'   rule under a "Planned decision rule" label followed by the written
#'   text under an "Interpretation" label. Blocks without an entry render
#'   exactly as they do when this argument is NULL. Interpretations are
#'   report content only and are never written into the instrument.
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
#' \donttest{
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
#' }
render_report <- function(
    instrument,
    data              = NULL,
    output_file       = NULL,
    output_path       = NULL,
    format            = c("html", "pdf"),
    include_quality   = TRUE,
    include_reliability = TRUE,
    include_codebook  = TRUE,
    include_missing   = TRUE,
    include_descriptives = TRUE,
    include_analysis  = TRUE,
    include_models    = TRUE,
    plot_palette      = c("web", "print"),
    interpretations   = NULL
) {
  sframe_check_instrument(instrument)
  plot_palette <- rlang::arg_match(plot_palette)
  interpretations <- sframe_clean_interpretations(interpretations)

  format <- rlang::arg_match(format)
  dest <- output_file %||% output_path %||%
    tempfile(fileext = paste0(".", format))
  dest <- path.expand(dest)

  # PDF is strictly additive: build the HTML report exactly as the html
  # path does, then print it through Chrome. Requires pagedown (Suggests)
  # and a local Chrome or Chromium.
  if (identical(format, "pdf")) {
    if (!requireNamespace("pagedown", quietly = TRUE)) {
      rlang::abort(
        "Package 'pagedown' is needed for PDF output. Install it, or use format = 'html'.",
        class = c("sframe_missing_package", "sframe_error")
      )
    }
    html_tmp <- tempfile(fileext = ".html")
    on.exit(unlink(html_tmp, force = TRUE), add = TRUE)
    render_report(
      instrument, data = data, output_file = html_tmp, format = "html",
      include_quality = include_quality,
      include_reliability = include_reliability,
      include_codebook = include_codebook,
      include_missing = include_missing,
      include_descriptives = include_descriptives,
      include_analysis = include_analysis,
      include_models = include_models,
      plot_palette = plot_palette,
      interpretations = interpretations
    )
    # Chrome writes its own scratch directories into tempdir() and does not
    # remove them, which R CMD check reports as detritus in the temp
    # directory. Only entries that appear during this call and match Chrome's
    # own naming are removed, so nothing else in tempdir() is touched.
    before <- list.files(tempdir(), all.files = TRUE, no.. = TRUE)
    on.exit(sframe_clean_chrome_detritus(before), add = TRUE)

    printed <- tryCatch(
      pagedown::chrome_print(input = html_tmp, output = dest),
      error = function(e) {
        rlang::abort(
          paste0("PDF printing failed: ", conditionMessage(e),
                 ". pagedown::chrome_print() needs a local Chrome or Chromium; use format = 'html' if none is available."),
          class = "sframe_error"
        )
      }
    )
    return(invisible(dest))
  }

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
    tmp_interp <- tempfile(fileext = ".rds")
    temp_rds <- tmp_instr
    saveRDS(instrument, tmp_instr)
    if (!is.null(data)) {
      saveRDS(data, tmp_data)
      temp_rds <- c(temp_rds, tmp_data)
    } else {
      tmp_data <- ""
    }
    if (length(interpretations) > 0) {
      saveRDS(interpretations, tmp_interp)
      temp_rds <- c(temp_rds, tmp_interp)
    } else {
      tmp_interp <- ""
    }
    on.exit(unlink(temp_rds, force = TRUE), add = TRUE)

    params <- list(
      instrument_path     = tmp_instr,
      data_path           = tmp_data,
      interpretations_path = tmp_interp,
      include_quality     = include_quality && !is.null(data),
      include_reliability = include_reliability && !is.null(data),
      include_codebook    = include_codebook,
      include_missing     = include_missing && !is.null(data),
      include_descriptives = include_descriptives && !is.null(data),
      include_analysis    = include_analysis,
      include_models      = include_models,
      plot_palette        = plot_palette,
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
    # Render with the working directory set to the render dir so Quarto creates
    # its intermediate `*_files/libs` there and can inline them for
    # embed-resources. Output next to the input, then copy to the destination.
    # Using --output-dir elsewhere makes the self-contained bundling fail to
    # find quarto.js (it is created relative to the working directory).
    rendered <- file.path(render_dir, "report.html")
    quarto_ok <- tryCatch({
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(render_dir)
      status <- suppressWarnings(system2(
        quarto_bin,
        args = c(
          "render", "report.qmd",
          "--output", "report.html",
          param_args
        ),
        stdout = TRUE,
        stderr = TRUE
      ))
      setwd(old_wd)
      (is.null(attr(status, "status")) || identical(attr(status, "status"), 0L)) &&
        file.exists(rendered)
    }, error = function(e) FALSE)

    if (isTRUE(quarto_ok)) {
      if (file.copy(rendered, dest, overwrite = TRUE)) {
        return(invisible(dest))
      }
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
    include_models      = include_models,
    plot_palette        = plot_palette,
    interpretations     = interpretations
  )

  invisible(dest)
}

# Drop entries that cannot be rendered: unnamed values, non-character
# values, and empty strings. Returns a named list of single strings, or an
# empty list, so downstream code can index it without further checks.
sframe_clean_interpretations <- function(interpretations) {
  if (is.null(interpretations)) return(list())
  if (!is.list(interpretations) && !is.character(interpretations)) {
    rlang::abort(
      "`interpretations` must be a named list or named character vector keyed by analysis-plan block id.",
      class = "sframe_error"
    )
  }
  interpretations <- as.list(interpretations)
  keys <- names(interpretations) %||% character(0)
  out <- list()
  for (i in seq_along(interpretations)) {
    key <- if (i <= length(keys)) keys[[i]] else ""
    value <- interpretations[[i]]
    if (!nzchar(key %||% "")) next
    if (!is.character(value) || length(value) != 1 || is.na(value)) next
    value <- trimws(value)
    if (!nzchar(value)) next
    out[[key]] <- value
  }
  out
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
    include_models = TRUE,
    plot_palette = "web",
    interpretations = list()
) {
  meta <- instrument$meta %||% list()
  sections <- character(0)

  if (isTRUE(include_codebook)) {
    cb <- codebook_report(instrument)
    codebook_parts <- c(
      "<h2>Codebook</h2>",
      .render_report_table(sframe_codebook_items_display(cb), "Survey items",
        col.names = c("ID", "Label", "Type", "Response options", "Scale",
                      "Reverse", "Required"))
    )
    if (nrow(cb$scales_table) > 0) {
      codebook_parts <- c(
        codebook_parts,
        .render_report_table(cb$scales_table, "Scale definitions",
          col.names = c("ID", "Label", "Method", "Items (n)", "Item IDs"))
      )
    }
    if (!is.null(cb$plan_table) && nrow(cb$plan_table) > 0) {
      codebook_parts <- c(
        codebook_parts,
        .render_report_table(cb$plan_table, "Pre-declared analysis plan",
          col.names = c("ID", "Research question", "Method", "Variables",
                        "Decision rule"))
      )
    }
    if (!is.null(cb$models_table) && nrow(cb$models_table) > 0) {
      codebook_parts <- c(
        codebook_parts,
        .render_report_table(cb$models_table, "Measurement and structural models",
          col.names = c("ID", "Label", "Type", "Engine", "Constructs", "Paths"))
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
      patterns_display <- mr$patterns[, intersect(c("description", "n", "percent"), names(mr$patterns)), drop = FALSE]
      paste(
        "<h2>Missing Data</h2>",
        .render_report_table(mr$item_missing, "Item-wise missingness"),
        .render_report_table(mr$respondent_missing, "Respondent-wise missingness"),
        .render_report_table(patterns_display, "Missing-data patterns",
                             col.names = c("Pattern", "n", "Percent")),
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

    dist_html <- tryCatch(
      .render_report_distributions(instrument, data, plot_palette = plot_palette),
      error = function(e) ""
    )
    if (nzchar(dist_html)) sections <- c(sections, dist_html)
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
    analysis_html <- .render_report_analysis_section(instrument, data,
                                                     plot_palette = plot_palette,
                                                     interpretations = interpretations)
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
      "/* Brand palette as CSS variables: a theme is a one-line change here.",
      "   --sf-accent is the AA-safe teal (>= 4.5:1 on white). */",
      ":root {",
      "  --sf-ink: #1a1a2e;",
      "  --sf-accent: #0e7c7a;",
      "  --sf-muted: #52606d;",
      "  --sf-faint: #5b6b80;",
      "  --sf-line: #d9e2ec;",
      "  --sf-panel: #f8fafc;",
      "  --sf-rule: #000;",
      "}",
      "body { font-family: system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif; max-width: 900px; margin: 0 auto; padding: 32px 20px; color: var(--sf-ink); line-height: 1.6; }",
      "h1, h2, h3 { color: var(--sf-ink); }",
      "a { color: var(--sf-accent); }",
      "section { margin: 0 0 28px; padding-bottom: 12px; border-bottom: 1px solid var(--sf-line); }",
      "table { display: block; overflow-x: auto; max-width: 100%%; border-collapse: collapse; margin: 16px 0 2px; font-size: .93em; }",
      "caption { text-align: left; font-style: italic; font-size: .95em; margin-bottom: 4px; }",
      "thead tr { border-top: 2px solid var(--sf-rule); border-bottom: 1px solid var(--sf-rule); }",
      "tbody tr:last-child td { border-bottom: 2px solid var(--sf-rule); }",
      "th, td { border: none; padding: 5px 12px; text-align: left; vertical-align: top; }",
      "th { font-weight: 700; background: none; }",
      ".tbl-note { font-size: .85em; font-style: italic; color: #333; margin-top: 3px; margin-bottom: 16px; }",
      ".meta { color: var(--sf-muted); margin-bottom: 24px; }",
      ".rq-block { margin: 18px 0; padding: 14px; background: var(--sf-panel); border: 1px solid var(--sf-line); border-radius: 6px; }",
      ".apa { color: var(--sf-ink); }",
      ".sf-foot { text-align: center; font-size: 12px; color: var(--sf-faint); margin-top: 48px; padding-top: 16px; border-top: 1px solid #eee; }",
      ".sf-foot a { color: var(--sf-accent); font-weight: 600; text-decoration: none; }",
      "/* Print (and PDF via pagedown): paginate cleanly, keep a result",
      "   block and a table's header with its rows, drop the panel tint. */",
      "@media print {",
      "  body { max-width: none; padding: 0; }",
      "  section { border-bottom: none; }",
      "  .rq-block { break-inside: avoid; background: none; border: 1px solid var(--sf-line); }",
      "  table { display: table; overflow-x: visible; }",
      "  thead { display: table-header-group; }",
      "  tr, img { break-inside: avoid; }",
      "  h2, h3 { break-after: avoid; }",
      "  a { color: var(--sf-ink); text-decoration: none; }",
      "}",
      "</style>",
      "</head>",
      "<body>",
      "<h1>%s</h1>",
      "<p class=\"meta\">%s</p>",
      "%s",
      "<div class=\"sf-foot\">Built with <a href=\"https://cran.r-project.org/package=surveyframe\" target=\"_blank\" rel=\"noopener\">surveyframe</a></div>",
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

.render_report_analysis_section <- function(instrument, data = NULL,
                                            plot_palette = "web",
                                            interpretations = list()) {
  plan <- instrument$analysis_plan %||% list()
  if (length(plan) == 0) {
    return("")
  }

  # The written interpretation and the pre-declared decision rule render as
  # a pair, so the post-hoc narrative never displaces the prospective
  # contract. Without an override, output is unchanged.
  interp_html <- function(block_id, decision_rule) {
    override <- interpretations[[block_id %||% ""]] %||% ""
    if (!nzchar(override)) return("")
    rule_html <- if (nzchar(decision_rule %||% "")) {
      sprintf("<p><strong>Planned decision rule:</strong> %s</p>",
              htmltools_escape(decision_rule))
    } else ""
    paste0(rule_html,
           sprintf("<p><strong>Interpretation:</strong> %s</p>",
                   htmltools_escape(override)))
  }

  blocks <- if (is.null(data)) {
    vapply(seq_along(plan), function(i) {
      rq <- plan[[i]]
      extra <- interp_html(rq$id, rq$decision_rule %||% rq$interpretation)
      paste(
        c(
          "<div class=\"rq-block\">",
          sprintf("<h3>RQ %d: %s</h3>", i,
                  htmltools_escape(rq$research_question %||% paste("Research Question", i))),
          sprintf("<p><strong>Planned test:</strong> %s</p>",
                  htmltools_escape(sframe_analysis_method(rq))),
          sprintf("<p><strong>Variables:</strong> %s</p>",
                  htmltools_escape(paste(sframe_analysis_vars(rq), collapse = ", "))),
          if (nzchar(extra)) extra,
          "</div>"
        ),
        collapse = "\n"
      )
    }, character(1))
  } else {
    results <- tryCatch(
      run_analysis_plan(data, instrument,
                        plots = requireNamespace("ggplot2", quietly = TRUE),
                        plot_palette = plot_palette),
      error = function(e) e
    )
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
      # The chart for this result renders directly under its table, as one
      # unit per research question, rather than in a separate section.
      rq_label <- result$research_question %||% paste("research question", i)
      plot_html <- ""
      if (!is.null(result$plot)) {
        img <- tryCatch(
          .render_report_ggplot_png(result$plot,
                                    alt = paste("Chart for", rq_label)),
          error = function(e) NULL
        )
        if (!is.null(img)) plot_html <- img
      }
      # Regression diagnostics are four separate panels, not one plot, so
      # they render as four stacked images beneath the main chart rather
      # than forcing a combined-grob dependency just to arrange them.
      if (is.list(result$diagnostic_plots)) {
        diag_imgs <- vapply(seq_along(result$diagnostic_plots), function(di) {
          img <- tryCatch(
            .render_report_ggplot_png(
              result$diagnostic_plots[[di]],
              alt = sprintf("Regression diagnostic panel %d for %s", di, rq_label)),
            error = function(e) NULL
          )
          img %||% ""
        }, character(1))
        plot_html <- paste(c(plot_html, diag_imgs[diag_imgs != ""]), collapse = "\n")
      }
      extra <- interp_html(result$block_id %||% result$id, result$decision_rule)
      # A CFA/SEM/PLS-SEM syntax block has no table or chart to show: its
      # entire output is the generated syntax text, so that goes where a
      # table or chart normally would rather than leaving the block blank.
      syntax_html <- if (!is.null(result$syntax)) {
        sprintf("<pre><code>%s</code></pre>", htmltools_escape(result$syntax))
      } else ""
      # extract_quotes() attaches $quotes (topic, rank, respondent, quote) to
      # a topic-model result: a second, separate table alongside the main
      # $table rather than a substitute for it, so it renders through the
      # same generic table builder under its own heading (todo_0.5.md's
      # ground-truth note: mirror the $syntax pattern, don't build a new
      # render_text_section()).
      quotes_html <- if (is.data.frame(result$quotes) && nrow(result$quotes) > 0) {
        .render_report_table(result$quotes, "Representative quotes")
      } else ""
      paste(
        c(
          "<div class=\"rq-block\">",
          sprintf("<h3>RQ %d: %s</h3>", i,
                  htmltools_escape(result$research_question %||% paste("Research Question", i))),
          sprintf("<p class=\"apa\"><strong>Result:</strong> %s</p>",
                  htmltools_escape(result$apa %||% result$error %||% "")),
          table_html,
          plot_html,
          syntax_html,
          quotes_html,
          if (nzchar(extra)) extra,
          "</div>"
        ),
        collapse = "\n"
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

# Render a base-R plot to a base64-embedded PNG <img> for the HTML fallback.
.render_report_plot_png <- function(draw, height = 320) {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 720, height = height, res = 96, bg = "white")
  ok <- tryCatch({ draw(); TRUE }, error = function(e) FALSE)
  grDevices::dev.off()
  if (!isTRUE(ok) || !file.exists(tmp)) return(NULL)
  raw <- readBin(tmp, "raw", file.info(tmp)$size)
  unlink(tmp)
  sprintf(
    "<img alt=\"distribution\" style=\"max-width:100%%;height:auto\" src=\"data:image/png;base64,%s\">",
    openssl::base64_encode(raw)
  )
}

# Render a ggplot object to a base64-embedded PNG <img> for the HTML
# fallback, same embedding pattern as .render_report_plot_png() above but
# for the ggplot2 objects run_analysis_plan(plots = TRUE) attaches.
.render_report_ggplot_png <- function(gg, alt = "result chart") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  tmp <- tempfile(fileext = ".png")
  ok <- tryCatch({
    ggplot2::ggsave(tmp, gg, width = 7.2, height = 4.2, dpi = 110, bg = "white")
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ok) || !file.exists(tmp)) return(NULL)
  raw <- readBin(tmp, "raw", file.info(tmp)$size)
  unlink(tmp)
  sprintf(
    "<img alt=\"%s\" style=\"max-width:100%%;height:auto\" src=\"data:image/png;base64,%s\">",
    htmltools_escape(alt), openssl::base64_encode(raw)
  )
}

# Distribution plots (item bar charts, scale histograms) for the HTML fallback,
# matching the dashboard. Used only when Quarto is unavailable.
.render_report_distributions <- function(instrument, data, plot_palette = "web") {
  if (is.null(data)) return("")
  theme <- instrument$render$theme %||% "#16B3B1"
  if (!grepl("^#[0-9A-Fa-f]{3,6}$", theme)) theme <- "#16B3B1"
  has_ggplot <- requireNamespace("ggplot2", quietly = TRUE)
  choice_by <- function(id) {
    for (c in instrument$choices %||% list()) if (identical(c$id, id)) return(c)
    NULL
  }
  q_items <- Filter(
    function(i) !(i$type %in% c("section_break", "text_block")),
    instrument$items %||% list()
  )
  blocks <- character(0)

  # A scale's separate Likert items get one grouped diverging chart, the
  # same way a matrix question's rows do, instead of scattering a related
  # batch of items across several single-item charts. group_of[[item_id]]
  # points back to which scale's group it belongs to; rendered tracks which
  # groups have already produced their one chart, so only the first member
  # item encountered triggers it.
  scale_groups <- sframe_likert_scale_groups(instrument)
  group_of <- character(0)
  for (g in scale_groups) {
    ids <- vapply(g$items, function(i) i$id, character(1))
    group_of[ids] <- g$scale_id
  }
  rendered_groups <- character(0)

  for (item in q_items) {
    t <- item$type
    img <- NULL
    if (item$id %in% names(group_of)) {
      gid <- group_of[[item$id]]
      if (!gid %in% rendered_groups && has_ggplot) {
        rendered_groups <- c(rendered_groups, gid)
        g <- scale_groups[[gid]]
        gg <- sframe_plot_likert_scale(g$items, data, g$choice_set, g$title,
                                       palette = plot_palette)
        img <- .render_report_ggplot_png(gg, alt = paste("Distribution of", g$title))
        if (!is.null(img)) {
          blocks <- c(blocks, sprintf("<h3>%s</h3>%s", htmltools_escape(g$title), img))
        }
      }
      next
    }
    if (identical(t, "matrix")) {
      # A matrix question has no base response column, only one expanded
      # <id>__<row> column per row, so it needs its own existence check and
      # its own chart: every row grouped together, not one chart per row.
      if (has_ggplot) {
        cs <- choice_by(item$choice_set %||% "")
        gg <- sframe_plot_likert_matrix(item, data, cs, palette = plot_palette)
        img <- .render_report_ggplot_png(gg, alt = paste("Distribution of", item$label %||% item$id))
      }
      if (!is.null(img)) {
        blocks <- c(blocks, sprintf("<h3>%s</h3>%s",
          htmltools_escape(item$label %||% item$id), img))
      }
      next
    }
    if (!item$id %in% names(data)) next
    col <- data[[item$id]]
    if (t %in% c("likert", "single_choice", "multiple_choice")) {
      cs <- choice_by(item$choice_set %||% "")
      freq <- if (!is.null(cs)) {
        f <- table(factor(col, levels = as.character(cs$values)))
        names(f) <- cs$labels
        f
      } else {
        table(col)
      }
      if (!sum(freq)) next
      if (identical(t, "likert") && length(freq) >= 2) {
        # A Likert item is an ordered agree/disagree scale: a diverging bar
        # shows which way opinion leans, unlike a plain frequency bar.
        img <- .render_report_plot_png(
          function() sframe_draw_likert_diverging(freq, theme),
          height = if (length(freq) <= 5) 320 else 320 + 22 * length(freq)
        )
      } else if (has_ggplot) {
        # Shares theme_surveyframe() (theme_classic()-based) with every
        # other chart in the report instead of a differently styled base-R
        # bar chart.
        gg <- sframe_plot_item_chart(item, col, cs, palette = plot_palette)
        img <- .render_report_ggplot_png(gg, alt = paste("Distribution of", item$label %||% item$id))
      } else {
        img <- .render_report_plot_png(function() {
          op <- graphics::par(mar = c(4, 11, 1, 1)); on.exit(graphics::par(op))
          graphics::barplot(freq, horiz = TRUE, las = 1, col = theme,
                            border = NA, xlab = "Frequency", cex.names = .8)
        })
      }
    } else if (t %in% c("numeric", "slider", "rating")) {
      num <- suppressWarnings(as.numeric(col)); num <- num[!is.na(num)]
      if (!length(num)) next
      if (has_ggplot) {
        gg <- sframe_plot_item_chart(item, col, NULL, palette = plot_palette)
        img <- .render_report_ggplot_png(gg, alt = paste("Distribution of", item$label %||% item$id))
      } else {
        img <- .render_report_plot_png(function() {
          op <- graphics::par(mar = c(4, 4, 1, 1)); on.exit(graphics::par(op))
          graphics::hist(num, col = theme, border = "white", main = NULL,
                         xlab = item$label, ylab = "Count", las = 1)
        })
      }
    }
    if (!is.null(img)) {
      blocks <- c(blocks, sprintf("<h3>%s</h3>%s",
        htmltools_escape(item$label %||% item$id), img))
    }
  }

  for (sc in instrument$scales %||% list()) {
    cols <- intersect(sc$items, names(data))
    if (!length(cols)) next
    nums <- lapply(data[cols], function(x) suppressWarnings(as.numeric(x)))
    scores <- rowMeans(do.call(cbind, nums), na.rm = TRUE)
    scores <- scores[!is.na(scores)]
    if (!length(scores)) next
    if (has_ggplot) {
      gg <- sframe_plot_scale_chart(scores, sc$label %||% sc$id, palette = plot_palette)
      img <- .render_report_ggplot_png(gg, alt = paste((sc$label %||% sc$id), "scale score distribution"))
    } else {
      img <- .render_report_plot_png(function() {
        op <- graphics::par(mar = c(4, 4, 1, 1)); on.exit(graphics::par(op))
        graphics::hist(scores, col = theme, border = "white", main = NULL,
          xlab = paste0(sc$label %||% sc$id, " score"), ylab = "Count", las = 1)
        graphics::abline(v = mean(scores), col = "#dc2626", lwd = 2, lty = 2)
      })
    }
    if (!is.null(img)) {
      blocks <- c(blocks, sprintf("<h3>%s (scale score)</h3>%s",
        htmltools_escape(sc$label %||% sc$id), img))
    }
  }

  if (!length(blocks)) return("")
  sprintf("<section><h2>Response distributions</h2>%s</section>",
          paste(blocks, collapse = "\n"))
}

# Turn snake_case data-frame names into "Title Case" table headings, e.g.
# "research_question" -> "Research question", "n_items" -> "N items".
.sframe_title_case_names <- function(names) {
  vapply(names, function(nm) {
    words <- strsplit(gsub("_", " ", nm), " ")[[1]]
    if (!length(words)) return(nm)
    words[1] <- paste0(toupper(substring(words[1], 1, 1)), substring(words[1], 2))
    paste(words, collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

.render_report_table <- function(x, caption = NULL, note = NULL, col.names = NULL) {
  if (!is.data.frame(x) || nrow(x) == 0) {
    return("")
  }

  # Round numeric columns to two decimal places for readability.
  num_cols <- vapply(x, is.numeric, logical(1))
  if (any(num_cols)) {
    x[num_cols] <- lapply(x[num_cols], function(col) round(col, 2))
  }

  has_pval <- any(grepl(
    "^p$|^p\\.value$|^p_value$|^Pr\\(>",
    colnames(x),
    ignore.case = TRUE
  ))

  # Raw data frame names (e.g. "research_question", "n_items") are internal
  # identifiers, not reader-facing labels: title-case them with spaces unless
  # the caller supplies its own headings.
  display_names <- col.names %||% .sframe_title_case_names(colnames(x))

  header <- paste(
    sprintf("<th scope=\"col\">%s</th>", htmltools_escape_each(display_names)),
    collapse = ""
  )
  rows <- paste(
    apply(x, 1, function(row) {
      cells <- paste(
        sprintf("<td>%s</td>", htmltools_escape_each(as.character(row))),
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
