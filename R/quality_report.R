# quality_report.R

sframe_detect_started_at <- function(data, started_at = NULL) {
  if (!is.null(started_at)) {
    if (started_at %in% colnames(data)) {
      return(started_at)
    }
    return(NULL)
  }

  candidates <- c("started_at", "start_time", "started", ".started_at")
  matches <- candidates[candidates %in% colnames(data)]
  if (length(matches) == 0) {
    return(NULL)
  }

  matches[[1]]
}

sframe_parse_time <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = "UTC"))
  }

  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = "UTC"))
  }

  parsed <- lapply(as.character(x), function(value) {
    if (is.na(value) || !nzchar(trimws(value))) {
      return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
    }

    attempt <- try(
      suppressWarnings(as.POSIXct(
        value,
        tz = "UTC",
        tryFormats = c(
          "%Y-%m-%dT%H:%M:%OSZ",
          "%Y-%m-%d %H:%M:%OS",
          "%Y-%m-%d %H:%M",
          "%Y-%m-%d"
        )
      )),
      silent = TRUE
    )

    if (inherits(attempt, "try-error")) {
      return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
    }

    attempt
  })

  as.POSIXct(do.call(c, parsed), origin = "1970-01-01", tz = "UTC")
}

sframe_timing_report <- function(
    data,
    submitted_at = NULL,
    started_at = NULL,
    time_min = NULL
) {
  n <- nrow(data)

  empty_report <- function(reason) {
    list(
      available = FALSE,
      reason = reason,
      start_col = NULL,
      submitted_col = submitted_at,
      durations_sec = rep(NA_real_, n),
      median_sec = NA_real_,
      threshold_seconds = time_min,
      flagged_rows = integer(0),
      n_flagged = 0L,
      flag_rate = if (n == 0) 0 else 0,
      parse_fail_rows = integer(0),
      negative_rows = integer(0)
    )
  }

  if (is.null(submitted_at) || !submitted_at %in% colnames(data)) {
    return(empty_report("No submitted_at column available for timing analysis."))
  }

  started_col <- sframe_detect_started_at(data, started_at)
  if (is.null(started_col)) {
    return(empty_report("No start-time column available for timing analysis."))
  }

  started_raw <- data[[started_col]]
  submitted_raw <- data[[submitted_at]]
  started_time <- sframe_parse_time(started_raw)
  submitted_time <- sframe_parse_time(submitted_raw)

  started_present <- !is.na(started_raw) & nzchar(trimws(as.character(started_raw)))
  submitted_present <- !is.na(submitted_raw) & nzchar(trimws(as.character(submitted_raw)))
  attempted_rows <- which(started_present & submitted_present)

  parse_fail_rows <- attempted_rows[
    is.na(started_time[attempted_rows]) | is.na(submitted_time[attempted_rows])
  ]

  durations_sec <- as.numeric(difftime(submitted_time, started_time, units = "secs"))
  negative_rows <- which(!is.na(durations_sec) & durations_sec < 0)

  if (length(parse_fail_rows) > 0) {
    sframe_warn_quality(
      paste0(
        "Timing analysis could not parse timestamps for ",
        length(parse_fail_rows),
        " row(s)."
      )
    )
  }

  if (length(negative_rows) > 0) {
    sframe_warn_quality(
      paste0(
        "Timing analysis found ",
        length(negative_rows),
        " row(s) with negative completion times."
      )
    )
  }

  durations_sec[negative_rows] <- NA_real_
  flagged_rows <- if (is.null(time_min)) integer(0) else {
    which(!is.na(durations_sec) & durations_sec < time_min)
  }

  list(
    available = TRUE,
    reason = NULL,
    start_col = started_col,
    submitted_col = submitted_at,
    durations_sec = durations_sec,
    median_sec = if (all(is.na(durations_sec))) {
      NA_real_
    } else {
      stats::median(durations_sec, na.rm = TRUE)
    },
    threshold_seconds = time_min,
    flagged_rows = flagged_rows,
    n_flagged = length(flagged_rows),
    flag_rate = if (n == 0) 0 else length(flagged_rows) / n,
    parse_fail_rows = parse_fail_rows,
    negative_rows = negative_rows
  )
}

#' Generate a data quality report for survey responses
#'
#' Evaluates collected response data against the instrument specification and
#' produces a structured quality report. The report covers attention check
#' performance, completion time, straight-lining within scale blocks,
#' item-level missingness, respondent-level missingness, and duplicate
#' respondent IDs where supplied.
#'
#' Timing analysis is available when the data contain a submission timestamp
#' column and either an explicit `started_at` column or one of the recognised
#' defaults: `started_at`, `start_time`, `started`, or `.started_at`.
#'
#' @param data A `tibble` or `data.frame` of responses, typically produced by
#'   [read_responses()].
#' @param instrument An `sframe` object created by [sf_instrument()].
#' @param respondent_id Character or NULL. The column name holding unique
#'   respondent identifiers. Used for duplicate detection.
#' @param submitted_at Character or NULL. The column name holding submission
#'   timestamps. Used for completion time analysis.
#' @param started_at Character or NULL. The column name holding survey start
#'   timestamps. When `NULL`, `quality_report()` looks for a recognised
#'   start-time column automatically.
#' @param time_min Numeric or NULL. Minimum acceptable completion time in
#'   seconds. Respondents with a submission time below this threshold are
#'   flagged as speeders when timing data are available.
#' @param straightline_scales Logical. Whether to check for straight-lining
#'   within each defined scale block. Defaults to `TRUE`.
#' @param missing_threshold Numeric. The proportion of missing item responses
#'   above which a respondent is flagged. Defaults to `0.2`.
#'
#' @return An object of class `sframe_quality_report`, a named list with
#'   elements: `summary`, `attention`, `timing`, `straightline`, `missing`,
#'   and `duplicates`. Use `print()` for a formatted summary.
#' @export
#' @seealso [sf_check()], [read_responses()], [score_scales()]
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
#' qr <- quality_report(
#'   responses,
#'   instr,
#'   respondent_id = "respondent_id",
#'   submitted_at = "submitted_at",
#'   started_at = "started_at",
#'   straightline_scales = FALSE
#' )
#' print(qr)
quality_report <- function(
    data,
    instrument,
    respondent_id         = NULL,
    submitted_at          = NULL,
    started_at            = NULL,
    time_min              = NULL,
    straightline_scales   = TRUE,
    missing_threshold     = 0.2
) {
  stopifnot(inherits(instrument, "sframe"))
  stopifnot(is.data.frame(data))

  item_ids <- vapply(instrument$items, function(i) i$id, character(1))
  n        <- nrow(data)

  # --- Attention checks ---
  attn_results <- list()
  for (chk in instrument$checks) {
    col <- chk$item_id
    if (!col %in% colnames(data)) next
    responses  <- data[[col]]
    pass       <- responses %in% chk$pass_values
    attn_results[[chk$id]] <- list(
      check_id    = chk$id,
      item_id     = col,
      type        = chk$type,
      fail_action = chk$fail_action,
      n_pass      = sum(pass, na.rm = TRUE),
      n_fail      = sum(!pass, na.rm = TRUE),
      pass_rate   = mean(pass, na.rm = TRUE),
      failed_rows = which(!pass)
    )
  }

  # --- Timing ---
  timing_result <- sframe_timing_report(
    data = data,
    submitted_at = submitted_at,
    started_at = started_at,
    time_min = time_min
  )

  # --- Missingness ---
  item_cols     <- intersect(item_ids, colnames(data))
  item_data     <- data[, item_cols, drop = FALSE]
  item_miss     <- colMeans(is.na(item_data))
  resp_miss     <- rowMeans(is.na(item_data))
  flagged_resp  <- which(resp_miss > missing_threshold)

  missing_result <- list(
    item_miss_rate    = item_miss,
    respondent_miss   = resp_miss,
    flagged_threshold = missing_threshold,
    flagged_rows      = flagged_resp
  )

  # --- Straight-lining ---
  sl_results <- list()
  if (straightline_scales && length(instrument$scales) > 0) {
    for (scale in instrument$scales) {
      scale_cols <- intersect(scale$items, colnames(data))
      if (length(scale_cols) < 2) next
      scale_data <- data[, scale_cols, drop = FALSE]
      row_vars <- apply(scale_data, 1, function(row) {
        vals <- suppressWarnings(as.numeric(row))
        if (sum(!is.na(vals)) < 2) return(NA)
        stats::var(vals, na.rm = TRUE)
      })
      sl_results[[scale$id]] <- list(
        scale_id      = scale$id,
        n_items       = length(scale_cols),
        flagged_rows  = which(!is.na(row_vars) & row_vars == 0),
        flag_rate     = mean(!is.na(row_vars) & row_vars == 0)
      )
    }
  }

  # --- Duplicates ---
  dup_result <- list(flagged_rows = integer(0), n_duplicates = 0L)
  if (!is.null(respondent_id) && respondent_id %in% colnames(data)) {
    ids <- data[[respondent_id]]
    dup_result <- list(
      flagged_rows = which(duplicated(ids) | duplicated(ids, fromLast = TRUE)),
      n_duplicates = sum(duplicated(ids))
    )
  }

  # --- Summary ---
  total_flags <- length(unique(c(
    unlist(lapply(attn_results, function(x) x$failed_rows)),
    timing_result$flagged_rows,
    missing_result$flagged_rows,
    unlist(lapply(sl_results, function(x) x$flagged_rows)),
    dup_result$flagged_rows
  )))

  report <- structure(
    list(
      summary = list(
        n_respondents  = n,
        n_items        = length(item_cols),
        n_flagged      = total_flags,
        flag_rate      = if (n == 0) 0 else total_flags / n
      ),
      attention    = attn_results,
      timing       = timing_result,
      straightline = sl_results,
      missing      = missing_result,
      duplicates   = dup_result
    ),
    class = "sframe_quality_report"
  )

  report
}

#' @exportS3Method print sframe_quality_report
print.sframe_quality_report <- function(x, ...) {
  cat("Survey Data Quality Report\n")
  cat(sprintf("  Respondents:  %d\n", x$summary$n_respondents))
  cat(sprintf("  Items:        %d\n", x$summary$n_items))
  cat(sprintf("  Flagged:      %d (%.1f%%)\n",
              x$summary$n_flagged,
              x$summary$flag_rate * 100))
  if (length(x$attention) > 0) {
    cat("\nAttention checks:\n")
    for (chk in x$attention) {
      cat(sprintf("  %-20s pass %.0f%%  fail %d\n",
                  chk$check_id,
                  chk$pass_rate * 100,
                  chk$n_fail))
    }
  }
  if (isTRUE(x$timing$available)) {
    cat("\nTiming:\n")
    cat(sprintf("  Median completion time: %.1f seconds\n", x$timing$median_sec))
    if (!is.null(x$timing$threshold_seconds)) {
      cat(sprintf("  Below threshold: %d (%.1f%%) under %.0f seconds\n",
                  x$timing$n_flagged,
                  x$timing$flag_rate * 100,
                  x$timing$threshold_seconds))
    }
  }
  cat(sprintf("\nMissingness:  %.1f%% of respondents exceed %.0f%% threshold\n",
              mean(x$missing$respondent_miss > x$missing$flagged_threshold,
                   na.rm = TRUE) * 100,
              x$missing$flagged_threshold * 100))
  invisible(x)
}
