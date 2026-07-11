# plots.R
# v0.3.4 visualisation foundation: the surveyframe brand theme and the first
# family of analysis plots. ggplot2 lives in Suggests, so every entry point
# is guarded with rlang::check_installed().

#' @importFrom rlang .data
NULL

# Brand palette shared with the HTML report template (inst/templates/
# report.qmd): ink for text and axis lines, teal for marks and accents.
# The series colours are a fixed-order categorical set anchored on a darker
# brand teal, validated for lightness, chroma, adjacent-pair colour-vision
# separation, and 3:1 contrast on a white chart surface. Assign them in
# order; never reshuffle by rank.
sframe_brand <- function() {
  list(
    ink    = "#1a1a2e",
    teal   = "#0E9694",
    grey   = "#94a3b8",
    grid   = "#e2e8f0",
    accent = "#dc2626",
    series = c("#0E9694", "#d97706", "#2563eb", "#db2777", "#7c3aed")
  )
}

# Fixed-order series colours for k categories. Beyond the validated five the
# set is extended by interpolation as a bounded fallback.
sframe_series_colours <- function(k) {
  brand <- sframe_brand()
  if (k <= length(brand$series)) {
    brand$series[seq_len(k)]
  } else {
    grDevices::colorRampPalette(brand$series)(k)
  }
}

#' surveyframe brand theme for ggplot2
#'
#' A light, publication-oriented ggplot2 theme matching the surveyframe
#' report brand: dark ink typography, a teal accent, and quiet horizontal
#' grid lines. Apply it to any ggplot object, including the plots returned
#' by [run_analysis_plan()] when `plots = TRUE`.
#'
#' @param base_size Numeric. Base font size in points. Defaults to 12.
#' @param base_family Character. Base font family. Defaults to `""` (the
#'   device default).
#'
#' @return A ggplot2 theme object.
#' @export
#' @seealso [run_analysis_plan()]
#'
#' @examplesIf rlang::is_installed("ggplot2")
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(colour = "#16B3B1") +
#'   theme_surveyframe()
theme_surveyframe <- function(base_size = 12, base_family = "") {
  rlang::check_installed("ggplot2", reason = "to use theme_surveyframe().")
  brand <- sframe_brand()
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text             = ggplot2::element_text(colour = brand$ink),
      plot.title       = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle    = ggplot2::element_text(colour = brand$grey),
      plot.caption     = ggplot2::element_text(colour = brand$grey, size = base_size - 3),
      axis.text        = ggplot2::element_text(colour = brand$ink),
      axis.title       = ggplot2::element_text(colour = brand$ink),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = brand$grid, linewidth = 0.4),
      axis.line.x      = ggplot2::element_line(colour = brand$ink, linewidth = 0.5),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_text(face = "bold"),
      plot.title.position = "plot"
    )
}

# ---------------------------------------------------------------------------
# First plot family (v0.3.4): bar charts for the categorical runners and
# scatter/regression overlays for the correlation and regression runners.
# Each builder takes the runner result (and the analysis data where the plot
# needs raw values) and returns a ggplot object, or NULL when the result
# cannot be plotted.

sframe_plot_frequency <- function(result) {
  tbl <- result$table
  if (!is.data.frame(tbl) || nrow(tbl) == 0) return(NULL)
  # The table counts missing values; the chart shows responses only
  tbl <- tbl[!is.na(tbl$Value) & tbl$Value != "NA", , drop = FALSE]
  if (nrow(tbl) == 0) return(NULL)
  brand <- sframe_brand()
  tbl$Value <- factor(tbl$Value, levels = rev(tbl$Value))
  ggplot2::ggplot(tbl, ggplot2::aes(x = .data$Frequency, y = .data$Value)) +
    ggplot2::geom_col(fill = brand$teal, width = 0.72) +
    ggplot2::labs(
      title = paste("Distribution of", result$variable %||% ""),
      x = "Frequency", y = NULL
    ) +
    theme_surveyframe()
}

sframe_plot_crosstab <- function(result) {
  tbl <- result$table
  if (!is.data.frame(tbl) || nrow(tbl) == 0) return(NULL)
  brand <- sframe_brand()
  long <- as.data.frame(as.table(as.matrix(tbl)), stringsAsFactors = FALSE)
  names(long) <- c("Row", "Column", "Count")
  ggplot2::ggplot(long, ggplot2::aes(x = .data$Row, y = .data$Count,
                                     fill = .data$Column)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.78),
                      width = 0.7, colour = "white", linewidth = 0.4) +
    ggplot2::scale_fill_manual(
      values = sframe_series_colours(length(unique(long$Column)))) +
    ggplot2::labs(
      title = paste("Association between", result$vars[1], "and", result$vars[2]),
      x = result$vars[1], y = "Count", fill = result$vars[2]
    ) +
    theme_surveyframe()
}

sframe_plot_correlation <- function(result, data) {
  vars <- result$vars
  if (length(vars) < 2 || !all(vars[1:2] %in% colnames(data))) return(NULL)
  brand <- sframe_brand()
  df <- data.frame(
    x = suppressWarnings(as.numeric(data[[vars[1]]])),
    y = suppressWarnings(as.numeric(data[[vars[2]]]))
  )
  df <- df[stats::complete.cases(df), , drop = FALSE]
  if (nrow(df) < 3) return(NULL)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point(colour = brand$teal, alpha = 0.75, size = 2) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                         colour = brand$ink, fill = brand$grid,
                         linewidth = 0.7) +
    ggplot2::labs(
      title    = paste("Relationship between", vars[1], "and", vars[2]),
      subtitle = result$apa %||% NULL,
      x = vars[1], y = vars[2]
    ) +
    theme_surveyframe()
}

sframe_plot_regression <- function(result, data) {
  vars <- result$vars
  if (length(vars) < 2 || !all(vars %in% colnames(data))) return(NULL)
  outcome <- vars[length(vars)]
  predictors <- vars[-length(vars)]
  brand <- sframe_brand()
  num <- as.data.frame(lapply(data[vars], function(v) {
    suppressWarnings(as.numeric(v))
  }))
  num <- num[stats::complete.cases(num), , drop = FALSE]
  if (nrow(num) < 3) return(NULL)
  if (length(predictors) == 1) {
    p <- ggplot2::ggplot(num, ggplot2::aes(x = .data[[predictors]],
                                           y = .data[[outcome]])) +
      ggplot2::geom_point(colour = brand$teal, alpha = 0.75, size = 2) +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                           colour = brand$ink, fill = brand$grid,
                           linewidth = 0.7) +
      ggplot2::labs(
        title    = paste(outcome, "predicted by", predictors),
        subtitle = result$apa %||% NULL,
        x = predictors, y = outcome
      )
  } else {
    fit <- stats::lm(
      stats::as.formula(paste(outcome, "~", paste(predictors, collapse = " + "))),
      data = num
    )
    df <- data.frame(fitted = stats::fitted(fit), observed = num[[outcome]])
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$fitted, y = .data$observed)) +
      ggplot2::geom_point(colour = brand$teal, alpha = 0.75, size = 2) +
      ggplot2::geom_abline(colour = brand$ink, linetype = "dashed") +
      ggplot2::labs(
        title    = paste("Observed against fitted values for", outcome),
        subtitle = result$apa %||% NULL,
        x = "Fitted values", y = "Observed values"
      )
  }
  p + theme_surveyframe()
}

# Diverging stacked bar for a single Likert item, base graphics only (no
# ggplot2 dependency), so it works in the report's distributions section
# regardless of whether ggplot2 is installed. `counts` is a named numeric
# vector in scale order (names are the response labels, e.g. "Strongly
# disagree" .. "Strongly agree"), not sorted alphabetically or by frequency.
# The middle category of an odd-length scale is treated as neutral and
# split evenly across the zero line; an even-length scale has no neutral
# category. This is the standard survey-report convention (Pew Research,
# SurveyMonkey) for visualising an ordered agree/disagree scale, and reads
# in one glance which way opinion leans, unlike a plain frequency bar.
sframe_draw_likert_diverging <- function(counts, theme_color = "#16B3B1") {
  counts <- counts[!is.na(counts)]
  n <- length(counts)
  if (n < 2 || sum(counts) == 0) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "Not enough data to plot.", col = "#94a3b8")
    return(invisible(NULL))
  }
  pct <- 100 * as.numeric(counts) / sum(counts)
  labels <- names(counts) %||% paste0("Level ", seq_len(n))

  half <- n %/% 2
  neg_idx <- seq_len(half)
  pos_idx <- seq.int(n - half + 1L, n)
  has_neutral <- (n %% 2L) == 1L
  neu_idx <- if (has_neutral) half + 1L else integer(0)

  # Darkest at the pole (Strongly disagree / Strongly agree), lightest next
  # to neutral, so saturation itself signals intensity of opinion.
  neg_ramp <- grDevices::colorRampPalette(c("#b3261e", "#f2b6ae"))(max(1L, half))
  pos_ramp <- grDevices::colorRampPalette(c("#a6ded9", theme_color))(max(1L, half))
  neu_col  <- "#c7cdd6"

  # Reserve enough bottom margin for the legend before plotting: one row
  # when the scale is short enough to fit across, one row per category
  # (in a single column) otherwise.
  op <- graphics::par(mar = c(if (n <= 5L) 4 else 1 + 1.15 * n, 2, 1, 2))
  on.exit(graphics::par(op), add = TRUE)

  # Both blocks are drawn starting from their outer edge moving toward
  # zero, so the most extreme category (index 1 on the left, index n on
  # the right) always sits at the far edge and the neutral-adjacent
  # category always sits next to the zero line.
  left_widths  <- pct[neg_idx]
  left_colors  <- neg_ramp
  right_widths <- pct[pos_idx]
  right_colors <- pos_ramp
  neu_half <- if (has_neutral) pct[neu_idx] / 2 else 0

  left_total  <- sum(left_widths) + neu_half
  right_total <- sum(right_widths) + neu_half
  xmax <- max(left_total, right_total) * 1.08 + 1

  graphics::plot.new()
  graphics::plot.window(xlim = c(-xmax, xmax), ylim = c(0, 1))
  x <- -left_total
  for (i in seq_along(left_widths)) {
    graphics::rect(x, 0.28, x + left_widths[i], 0.72, col = left_colors[i], border = "white")
    x <- x + left_widths[i]
  }
  if (has_neutral) {
    graphics::rect(-neu_half, 0.28, neu_half, 0.72, col = neu_col, border = "white")
    x <- neu_half
  }
  for (i in seq_along(right_widths)) {
    graphics::rect(x, 0.28, x + right_widths[i], 0.72, col = right_colors[i], border = "white")
    x <- x + right_widths[i]
  }
  graphics::segments(0, 0.15, 0, 0.85, col = "#1a1a2e", lwd = 1.2)
  # Each segment already carries its own percentage in the legend below, so
  # a numeric axis would only repeat that; the zero line alone shows where
  # opinion divides, which is what a diverging chart is for.

  # A single row keeps the legend in the same left-to-right scale order as
  # the bar; base graphics' legend() fills multi-column layouts column-major,
  # which would scramble that order, so five or fewer categories (the
  # common case) get one row and longer scales fall back to one column
  # (top-to-bottom, still in scale order) rather than a misleading grid.
  ord_colors <- c(neg_ramp, if (has_neutral) neu_col else NULL, pos_ramp)
  leg_labels <- sprintf("%s (%.0f%%)", labels, pct)
  usr <- graphics::par("usr")
  if (n <= 5L) {
    graphics::legend(x = mean(usr[1:2]), y = usr[3], xjust = 0.5, yjust = 1,
                     legend = leg_labels, fill = ord_colors, border = NA,
                     bty = "n", cex = 0.72, ncol = n, xpd = NA, x.intersp = 0.6)
  } else {
    graphics::legend(x = mean(usr[1:2]), y = usr[3], xjust = 0.5, yjust = 1,
                     legend = leg_labels, fill = ord_colors, border = NA,
                     bty = "n", cex = 0.68, ncol = 1, xpd = NA, x.intersp = 0.6)
  }
}

# Dispatch a runner result to its plot builder. Returns NULL for runner
# types outside the v0.3.4 plot family so callers can attach conditionally.
sframe_plot_for_result <- function(result, data) {
  if (!is.list(result) || !is.null(result$error)) return(NULL)
  test <- result$test %||% ""
  builder <- switch(
    test,
    frequency           = function() sframe_plot_frequency(result),
    crosstab            = ,
    chi_square          = function() sframe_plot_crosstab(result),
    correlation_pearson = ,
    correlation_spearman = ,
    correlation_kendall = function() sframe_plot_correlation(result, data),
    regression_linear   = function() sframe_plot_regression(result, data),
    NULL
  )
  if (is.null(builder)) return(NULL)
  tryCatch(builder(), error = function(e) NULL)
}
