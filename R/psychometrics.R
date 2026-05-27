# reliability_report.R

#' Compute reliability statistics for scored scales
#'
#' Produces Cronbach's alpha and McDonald's omega for each scale defined in
#' the instrument, along with the number of items and sample size.
#'
#' @param data A `tibble` or `data.frame` of responses. Item columns must be
#'   present.
#' @param instrument An `sframe` object.
#' @param scales Character vector or NULL. A subset of scale IDs to analyse.
#'   When NULL (default), all scales in the instrument are included.
#' @param alpha Logical. Whether to compute Cronbach's alpha. Defaults to
#'   `TRUE`.
#' @param omega Logical. Whether to compute McDonald's omega. Defaults to
#'   `TRUE`.
#'
#' @return An object of class `sframe_reliability_report`, a list with one
#'   element per scale. Each element is a list of statistics and a summary
#'   tibble.
#' @export
#' @seealso [sf_scale()], [item_report()]
#'
#' @examples
#' \donttest{
#' if (requireNamespace("psych", quietly = TRUE)) {
#'   demo <- sframe_demo_data()
#'   rr <- reliability_report(demo$responses, demo$instrument, omega = FALSE)
#'   print(rr)
#' }
#' }
reliability_report <- function(
    data,
    instrument,
    scales = NULL,
    alpha  = TRUE,
    omega  = TRUE
) {
  stopifnot(inherits(instrument, "sframe"))
  stopifnot(is.data.frame(data))
  if (isTRUE(alpha) || isTRUE(omega)) {
    sframe_require_psych("to compute reliability statistics")
  }

  target_scales <- instrument$scales
  if (!is.null(scales)) {
    target_scales <- Filter(function(s) s$id %in% scales, target_scales)
  }
  reverse_context <- sframe_reverse_context(instrument)

  results <- lapply(target_scales, function(scale) {
    cols <- intersect(scale$items, colnames(data))
    if (length(cols) < 2) {
      sframe_warn_scoring(
        paste0("Scale '", scale$id,
               "' needs at least 2 items for reliability analysis."),
        scale_id = scale$id
      )
      return(NULL)
    }
    scale_data <- sframe_numeric_scale_data(data, cols, reverse_context)
    scale_data <- scale_data[complete.cases(scale_data), , drop = FALSE]

    result <- list(
      scale_id = scale$id,
      label    = scale$label,
      n_items  = length(cols),
      n        = nrow(scale_data)
    )

    if (alpha) {
      a <- suppressWarnings(
        psych::alpha(scale_data, check.keys = FALSE, warnings = FALSE)
      )
      result$alpha      <- a$total$raw_alpha
      result$alpha_std  <- a$total$std.alpha
    }
    if (omega) {
      tryCatch({
        o <- psych::omega(scale_data, nfactors = 1, plot = FALSE)
        result$omega_h <- o$omega_h
        result$omega_t <- o$omega.tot
      }, error = function(e) {
        sframe_warn_scoring(
          paste0("Omega could not be computed for scale '", scale$id,
                 "': ", conditionMessage(e)),
          scale_id = scale$id
        )
      })
    }

    result
  })

  results <- Filter(Negate(is.null), results)
  names(results) <- vapply(results, `[[`, character(1), "scale_id")

  structure(results, class = "sframe_reliability_report")
}

#' @exportS3Method print sframe_reliability_report
print.sframe_reliability_report <- function(x, ...) {
  cat("Reliability Report\n\n")
  for (s in x) {
    cat(sprintf("Scale: %s (%s)\n", s$scale_id, s$label))
    cat(sprintf("  Items: %d   N: %d\n", s$n_items, s$n))
    if (!is.null(s$alpha))   cat(sprintf("  Alpha:   %.3f\n", s$alpha))
    if (!is.null(s$omega_h)) cat(sprintf("  Omega h: %.3f\n", s$omega_h))
    if (!is.null(s$omega_t)) cat(sprintf("  Omega t: %.3f\n", s$omega_t))
    cat("\n")
  }
  invisible(x)
}


# item_report.R

#' Generate item-level diagnostics
#'
#' Produces item-total correlations, floor and ceiling effect proportions,
#' and item means and standard deviations for each item within each scale.
#'
#' @param data A `tibble` or `data.frame` of responses.
#' @param instrument An `sframe` object.
#' @param scales Character vector or NULL. A subset of scale IDs to analyse.
#'   When NULL (default), all scales are included.
#'
#' @return An object of class `sframe_item_report`, a list with one data.frame
#'   per scale.
#' @export
#' @seealso [reliability_report()], [sf_scale()]
#'
#' @examples
#' \donttest{
#' demo <- sframe_demo_data()
#' ir <- item_report(demo$responses, demo$instrument)
#' print(ir)
#' }
item_report <- function(data, instrument, scales = NULL) {
  stopifnot(inherits(instrument, "sframe"))
  stopifnot(is.data.frame(data))

  target_scales <- instrument$scales
  if (!is.null(scales)) {
    target_scales <- Filter(function(s) s$id %in% scales, target_scales)
  }

  results <- lapply(target_scales, function(scale) {
    cols <- intersect(scale$items, colnames(data))
    if (length(cols) < 2) return(NULL)

    scale_data <- as.data.frame(lapply(data[, cols, drop = FALSE],
                                       function(x) suppressWarnings(as.numeric(x))))
    total_score <- rowMeans(scale_data, na.rm = TRUE)

    diagnostics <- lapply(cols, function(col) {
      vals      <- scale_data[[col]]
      item_rest <- stats::cor(vals, total_score - vals, use = "complete.obs")
      col_min   <- min(vals, na.rm = TRUE)
      col_max   <- max(vals, na.rm = TRUE)
      list(
        item_id      = col,
        mean         = mean(vals, na.rm = TRUE),
        sd           = stats::sd(vals, na.rm = TRUE),
        item_rest_r  = item_rest,
        floor_pct    = mean(vals == col_min, na.rm = TRUE),
        ceiling_pct  = mean(vals == col_max, na.rm = TRUE),
        n_missing    = sum(is.na(vals))
      )
    })

    list(
      scale_id    = scale$id,
      label       = scale$label,
      diagnostics = sframe_as_data_frame(do.call(rbind, lapply(diagnostics, as.data.frame)))
    )
  })

  results <- Filter(Negate(is.null), results)
  names(results) <- vapply(results, `[[`, character(1), "scale_id")

  structure(results, class = "sframe_item_report")
}

#' @exportS3Method print sframe_item_report
print.sframe_item_report <- function(x, ...) {
  for (s in x) {
    cat(sprintf("Item diagnostics: %s (%s)\n\n", s$scale_id, s$label))
    print(s$diagnostics)
    cat("\n")
  }
  invisible(x)
}


# efa_report.R

#' Prepare a survey instrument for exploratory factor analysis
#'
#' Reports KMO sampling adequacy, Bartlett's test of sphericity, and a
#' parallel analysis scree plot to inform factor number selection. The report
#' prepares the researcher to estimate an EFA solution with a separate package
#' such as `psych` or `lavaan`.
#'
#' @param data A `tibble` or `data.frame` of responses.
#' @param instrument An `sframe` object.
#' @param scales Character vector or NULL. Scale IDs whose items to include.
#'   When NULL, all scale items are pooled.
#' @param nfactors Integer or NULL. Suggested number of factors to highlight
#'   on the scree plot. When NULL, the parallel analysis recommendation is
#'   used.
#' @param rotation Character. The rotation method to display in the diagnostic
#'   notes. Does not affect the diagnostics themselves. Defaults to
#'   `"oblimin"`.
#'
#' @return An object of class `sframe_efa_report` with elements `kmo`,
#'   `bartlett`, `parallel`, and `suggested_nfactors`.
#' @export
#' @seealso [reliability_report()], [cfa_syntax()]
#'
#' @examples
#' \donttest{
#' if (requireNamespace("psych", quietly = TRUE)) {
#'   demo <- sframe_demo_data()
#'   er <- efa_report(demo$responses, demo$instrument)
#'   print(er)
#' }
#' }
efa_report <- function(
    data,
    instrument,
    scales    = NULL,
    nfactors  = NULL,
    rotation  = "oblimin"
) {
  stopifnot(inherits(instrument, "sframe"))
  stopifnot(is.data.frame(data))
  sframe_require_psych("to prepare exploratory factor analysis diagnostics")

  target_scales <- instrument$scales
  if (!is.null(scales)) {
    target_scales <- Filter(function(s) s$id %in% scales, target_scales)
  }

  all_items <- unique(unlist(lapply(target_scales, function(s) s$items)))
  cols      <- intersect(all_items, colnames(data))

  item_data <- as.data.frame(lapply(data[, cols, drop = FALSE],
                                    function(x) suppressWarnings(as.numeric(x))))
  item_data <- item_data[complete.cases(item_data), , drop = FALSE]

  kmo      <- psych::KMO(item_data)
  bart     <- psych::cortest.bartlett(item_data)
  parallel <- psych::fa.parallel(item_data, plot = FALSE)

  suggested <- nfactors %||% parallel$nfact

  structure(
    list(
      kmo               = kmo,
      bartlett          = bart,
      parallel          = parallel,
      suggested_nfactors = suggested,
      rotation_note     = rotation,
      n_items           = length(cols),
      n                 = nrow(item_data)
    ),
    class = "sframe_efa_report"
  )
}

#' @exportS3Method print sframe_efa_report
print.sframe_efa_report <- function(x, ...) {
  cat("EFA Readiness Diagnostics\n\n")
  cat(sprintf("  Items:          %d\n", x$n_items))
  cat(sprintf("  Complete cases: %d\n", x$n))
  cat(sprintf("  KMO overall:    %.3f\n", x$kmo$MSA))
  cat(sprintf("  Bartlett chi-sq: %.2f  df: %d  p: %.4f\n",
              x$bartlett$chisq, x$bartlett$df, x$bartlett$p.value))
  cat(sprintf("  Suggested factors (parallel analysis): %d\n",
              x$suggested_nfactors))
  cat(sprintf("  Planned rotation: %s\n", x$rotation_note))
  cat("\nNote: estimate the EFA solution with a dedicated modelling package.\n")
  invisible(x)
}


# cfa_syntax.R

#' Generate lavaan CFA syntax from an instrument object
#'
#' Produces a character string of `lavaan` model syntax derived from the
#' scale structure in the instrument. The syntax can be passed directly to
#' `lavaan::cfa()`. Reverse-coded items are noted in a comment but are not
#' transformed in the syntax; recoding should be applied to the data before
#' fitting the model.
#'
#' @param instrument An `sframe` object.
#' @param scales Character vector or NULL. A subset of scale IDs to include.
#'   When NULL, all scales are included.
#' @param std_lv Logical. Whether to include the `std.lv = TRUE` argument note
#'   in the output comment header. Defaults to `TRUE`.
#'
#' @return A character string of `lavaan` CFA model syntax.
#' @export
#' @seealso [efa_report()], [reliability_report()]
#'
#' @examples
#' cs    <- sf_choices("ag5", 1:5,
#'            c("Strongly disagree", "Disagree", "Neutral",
#'              "Agree", "Strongly agree"))
#' i1    <- sf_item("sat_1", "Item 1", type = "likert",
#'                  choice_set = "ag5", scale_id = "sat")
#' i2    <- sf_item("sat_2", "Item 2", type = "likert",
#'                  choice_set = "ag5", scale_id = "sat")
#' i3    <- sf_item("sat_3", "Item 3 (reverse)", type = "likert",
#'                  choice_set = "ag5", scale_id = "sat", reverse = TRUE)
#' scale <- sf_scale("sat", "Satisfaction",
#'                   items = c("sat_1", "sat_2", "sat_3"))
#' instr <- sf_instrument("Demo Survey", components = list(cs, i1, i2, i3, scale))
#'
#' syntax <- cfa_syntax(instr)
#' cat(syntax)
#'
#' \dontrun{
#' # lavaan is not installed by default; install it before fitting.
#' demo   <- sframe_demo_data()
#' scored <- score_scales(demo$responses, demo$instrument)
#' fit    <- lavaan::cfa(syntax, data = scored, std.lv = TRUE)
#' summary(fit, fit.measures = TRUE)
#' }
cfa_syntax <- function(instrument, scales = NULL, std_lv = TRUE) {
  cfa_lavaan_syntax(instrument = instrument, scales = scales, std_lv = std_lv)
}
