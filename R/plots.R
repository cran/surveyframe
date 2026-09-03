# plots.R

# Why sframe_plot_quality() has nothing to draw.
#
# A NULL return alone cannot say whether nothing was flagged or nothing was
# long enough to check, which are opposite conclusions. NULL carries no
# attributes, so the reason is a separate call that render_report() prints in
# place of the missing chart.
sframe_quality_plot_note <- function(x) {
  stopifnot(inherits(x, "sframe_quality_report"))
  sl <- x$straightline %||% list()
  if (length(sl) == 0) {
    return("No scales were defined, so straight-lining was not checked.")
  }
  rates <- vapply(sl, function(s) s$flag_rate %||% NA_real_, numeric(1))
  if (any(!is.na(rates))) {
    return(NULL)
  }
  if (any(vapply(sl, function(s) isTRUE(s$checked), logical(1)))) {
    "No scale was flagged for straight-lining, so there is no chart to draw."
  } else {
    paste0("No scale was long enough to check for straight-lining, so there ",
           "is no chart to draw. Straight-lining needs at least ",
           "`straightline_min_items` items in a scale, 4 by default, because ",
           "identical answers to 2 or 3 items are what a consistent ",
           "respondent gives rather than evidence of inattention.")
  }
}

# v0.3.4 visualisation foundation: the surveyframe brand theme and the first
# family of analysis plots. ggplot2 lives in Suggests, so every entry point
# is guarded with rlang::check_installed().

#' @importFrom rlang .data
NULL

# Two colour systems, chosen and verified against WCAG 2.2 contrast
# thresholds (relative-luminance formula, WCAG 1.4.3/1.4.11), not eyeballed:
#   - "web": the surveyframe brand palette, for on-screen use.
#   - "print": black/grey/white only, for journal-ready print figures.
# Every colour used for TEXT (titles, axis labels, captions) or for a
# meaningful line/mark (reference lines, series, points) meets at least the
# relevant WCAG minimum: 4.5:1 for normal text, 3:1 for large text and for
# non-text graphical objects (WCAG 1.4.11). Only `grid` (gridlines) is
# treated as pure decoration and exempt, matching WCAG's own treatment of
# decorative content. Verified contrast ratios (against white):
#   ink (web)    #1a1a2e  17.06:1   ink (print)    #000000  21.00:1
#   teal (web)   #0E9694   3.62:1   teal (print)   #333333  12.63:1
#   muted (web)  #526070   6.43:1   muted (print)  #595959   7.00:1
#   accent (web) #dc2626   4.83:1   accent (print) #262626  15.13:1
#   series (web): 3.19-5.70:1 each (teal/orange/blue/pink/purple)
#   series (print): a 5-step grey ramp (2.68-18.42:1)
#
# `teal`, `ink`, `accent`, and `series` above are for POINTS, LINES, and TEXT:
# small marks where a dark print tone costs almost no ink and the WCAG
# boundary requirement is satisfied directly by the mark's own colour.
# `fill` and `fill_series` below are a SEPARATE, deliberately lighter set for
# large FILLED areas (bars, tiles, histogram/boxplot bodies): printing a
# large near-black area is heavy on toner and reads harshly on paper, so
# these stay light-to-mid grey with a black outline doing the boundary-
# contrast work instead of the fill itself (WCAG 1.4.11 is satisfied by that
# outline, same principle as the stroke rule already used throughout this
# file). Web keeps its brand colours for fills; only print's fills changed.
sframe_brand <- function(palette = c("web", "print")) {
  palette <- match.arg(palette)
  if (palette == "web") {
    list(
      ink    = "#1a1a2e",
      teal   = "#0E9694",
      muted  = "#526070",
      grid   = "#e2e8f0",
      accent = "#dc2626",
      series = c("#0E9694", "#d97706", "#2563eb", "#db2777", "#7c3aed"),
      fill        = "#0E9694",
      fill_duo    = c("#0E9694", "#1a1a2e"),
      fill_series = c("#0E9694", "#d97706", "#2563eb", "#db2777", "#7c3aed")
    )
  } else {
    list(
      ink    = "#000000",
      teal   = "#333333",
      muted  = "#595959",
      grid   = "#d9d9d9",
      accent = "#262626",
      series = c("#141414", "#555555", "#747474", "#8B8B8B", "#9E9E9E"),
      fill        = "#cccccc",
      fill_duo    = c("#999999", "#e0e0e0"),
      fill_series = c("#8C8C8C", "#AAAAAA", "#C2C2C2", "#D8D8D8", "#EBEBEB")
    )
  }
}

# Fixed-order series colours for k categories, for POINTS and LINES (small
# marks; dark is fine and desirable in print). Beyond the validated five the
# set is extended by interpolation as a bounded fallback.
sframe_series_colours <- function(k, palette = c("web", "print")) {
  palette <- match.arg(palette)
  brand <- sframe_brand(palette)
  if (k <= length(brand$series)) {
    brand$series[seq_len(k)]
  } else {
    grDevices::colorRampPalette(brand$series)(k)
  }
}

# Fixed-order series colours for k categories, for large FILLED areas (bars,
# tiles): identical to sframe_series_colours() on web, a lighter ramp on
# print. See the sframe_brand() comment above for why fills and marks are
# deliberately different in print mode.
sframe_series_fill_colours <- function(k, palette = c("web", "print")) {
  palette <- match.arg(palette)
  brand <- sframe_brand(palette)
  if (k <= length(brand$fill_series)) {
    brand$fill_series[seq_len(k)]
  } else {
    grDevices::colorRampPalette(brand$fill_series)(k)
  }
}

#' surveyframe brand theme for ggplot2
#'
#' A `theme_classic()`-based ggplot2 theme (visible axis lines, no floating
#' panel), verified against WCAG 2.2 contrast minimums: 4.5:1 for text,
#' 3:1 for non-text graphical objects. Apply it to any ggplot object,
#' including the plots returned by [run_analysis_plan()] when
#' `plots = TRUE`.
#'
#' @param base_size Numeric. Base font size in points. Defaults to 12.
#' @param base_family Character. Base font family. Defaults to `""` (the
#'   device default).
#' @param palette One of `"web"` (brand colours, for on-screen use) or
#'   `"print"` (black/grey/white only, for journal-ready figures). See
#'   `sframe_brand()` for the verified contrast ratios behind each.
#'
#' @return A ggplot2 theme object.
#' @export
#' @seealso [run_analysis_plan()]
#'
#' @examplesIf rlang::is_installed("ggplot2")
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(colour = "#0E9694") +
#'   theme_surveyframe()
theme_surveyframe <- function(base_size = 12, base_family = "",
                              palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to use theme_surveyframe().")
  palette <- match.arg(palette)
  brand <- sframe_brand(palette)
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text             = ggplot2::element_text(colour = brand$ink),
      plot.title       = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle    = ggplot2::element_text(colour = brand$muted),
      plot.caption     = ggplot2::element_text(colour = brand$muted, size = base_size - 3),
      axis.text        = ggplot2::element_text(colour = brand$ink),
      axis.title       = ggplot2::element_text(colour = brand$ink),
      axis.ticks       = ggplot2::element_line(colour = brand$ink, linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      axis.line        = ggplot2::element_line(colour = brand$ink, linewidth = 0.5),
      legend.position  = "bottom",
      legend.text      = ggplot2::element_text(colour = brand$ink),
      legend.title     = ggplot2::element_text(face = "bold", colour = brand$ink),
      strip.text       = ggplot2::element_text(colour = brand$ink, face = "bold"),
      strip.background = ggplot2::element_rect(fill = brand$grid, colour = NA),
      plot.title.position = "plot"
    )
}

# Angled, right-justified category labels for vertical bar charts whose
# category names can run long (scale ids, choice labels). Applied only to
# the bar-chart builders below, not globally in theme_surveyframe(), since
# it is wrong for scatter/heatmap/line plots.
sframe_theme_angled_x <- function() {
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
}

# Per-cell text colour for a heatmap tile, so labels stay legible against
# both light and dark fills (WCAG 1.4.3): white text on the darker tiles,
# ink text everywhere else. `magnitude` is the value driving the fill scale
# (signed for the web diverging gradient, already abs() for print).
sframe_heatmap_label_colour <- function(magnitude, ink) {
  ifelse(abs(magnitude) > 0.55, "white", ink)
}

# ---------------------------------------------------------------------------
# First plot family (v0.3.4): bar charts for the categorical runners and
# scatter/regression overlays for the correlation and regression runners.
# Each builder takes the runner result (and the analysis data where the plot
# needs raw values) and returns a ggplot object, or NULL when the result
# cannot be plotted.

sframe_plot_frequency <- function(result, palette = c("web", "print")) {
  palette <- match.arg(palette)
  tbl <- result$table
  if (!is.data.frame(tbl) || nrow(tbl) == 0) return(NULL)
  # The table counts missing values; the chart shows responses only
  tbl <- tbl[!is.na(tbl$Value) & tbl$Value != "NA", , drop = FALSE]
  if (nrow(tbl) == 0) return(NULL)
  brand <- sframe_brand(palette)
  tbl$Value <- factor(tbl$Value, levels = tbl$Value)
  ggplot2::ggplot(tbl, ggplot2::aes(x = .data$Value, y = .data$Frequency)) +
    ggplot2::geom_col(fill = brand$fill, colour = brand$ink, linewidth = 0.3, width = 0.72) +
    ggplot2::labs(
      title = paste("Distribution of", result$variable %||% ""),
      x = NULL, y = "Frequency"
    ) +
    theme_surveyframe(palette = palette) + sframe_theme_angled_x()
}

# Word-cloud layout via the actual algorithm the wordcloud/ggwordcloud
# packages use (Jonathan Feinberg's Wordle placement, as described at
# cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html
# and r-graph-gallery.com/wordcloud.html): words are placed largest first,
# each one walking an outward spiral from the centre until it finds a
# position whose bounding box does not overlap any word already placed.
# The previous version was a bare golden-angle spiral with NO collision
# check at all, so a large word could and did overlap its neighbours
# whenever positions happened to land close together (visible in the
# 0.5 vignette/demo's word clouds: "comfortable" overlapping "respond").
#
# Still no new dependency (todo_text_analysis.md: "do not add a wordcloud/
# ggwordcloud package for this"): text is measured with base
# `grDevices::pdf(NULL)` (a null device, writes no file, the standard R
# trick for off-screen `strwidth()`/`strheight()`) plus base graphics
# string-metric functions, not the wordcloud/ggwordcloud packages
# themselves.
#
# `sizes` must be the same values the caller will later map to the
# `geom_text()` `size` aesthetic (via `scale_size_identity()`, so the
# rendered size matches exactly what was measured here).
#
# Measured via `grid::textGrob()`/`grid::grobWidth()`/`grobHeight()`, not
# base graphics `strwidth()`/`strheight()` (the first version's approach):
# ggplot2 draws `geom_text()` through the grid graphics system, so
# measuring through grid tracks the actual rendered glyph size far more
# closely than base graphics' `pdf(NULL)` + `strwidth()`/`strheight()`
# trick did — that mismatch was exactly why the first fix needed a huge
# (85%) padding buffer to avoid overlap, which produced the "still looks
# scattered" complaint: most of the visible whitespace was safety margin
# against a measurement the algorithm didn't actually trust. `padding`
# drops to a normal ~15% now that the measurement is accurate enough to
# trust, giving the tight packing a real word cloud has.
#
# `centers` (optional, one `x`/`y` row per word) lets the SAME
# spiral-and-collide engine build a grouped cloud (see
# `sframe_plot_sentiment()`'s comparison cloud): each word spirals
# outward from its own group's anchor point rather than a shared origin,
# while collision detection stays global, so the two groups never
# overlap each other at the boundary. Defaults to every word sharing the
# origin, the single-cloud case `sframe_plot_term_frequency()` uses.
#
# `aspect` scales the spiral's x-growth relative to its (fixed) y-growth:
# `aspect = 1` is a circular spiral (`sframe_plot_term_frequency()`'s
# cloud); `aspect < 1` grows taller and narrower than it does wide, which
# is what keeps 2 side-by-side clusters (`sframe_plot_sentiment()`'s
# left/right comparison cloud) from spreading into each other
# horizontally as readily as a wide ellipse would.
#
# `shape = "circle"` additionally caps the spiral radius at a disc sized
# to roughly hold the words' total rendered area (accounting for a
# packing-inefficiency factor, since rectangular bounding boxes and a
# spiral search never tile perfectly), and wraps the search back toward
# the centre instead of growing past that radius once a word's natural
# spiral would exceed it — so a smaller word placed later fills a real
# interior gap near the centre rather than spilling out past a ragged
# organic edge. `shape = "organic"` (default) is uncapped, the original
# freeform behaviour.
#
# Still no new dependency (todo_text_analysis.md: "do not add a wordcloud/
# ggwordcloud package for this"): `grid` and `grDevices::pdf(NULL)` (a
# null device, writes no file) are both base R, not the wordcloud/
# ggwordcloud packages themselves.
.sframe_wordcloud_layout <- function(words, sizes, centers = NULL, padding = 0.15,
                                     aspect = 1.5, shape = c("organic", "circle")) {
  shape <- match.arg(shape)
  n <- length(words)
  if (n == 0) {
    return(data.frame(term = character(0), x = numeric(0), y = numeric(0)))
  }
  if (is.null(centers)) centers <- data.frame(x = rep(0, n), y = rep(0, n))

  # Largest word first: it anchors its cluster, and each later (smaller)
  # word has an easier time finding a gap than the reverse order would.
  ord <- order(-sizes)
  words <- words[ord]
  sizes <- sizes[ord]
  centers <- centers[ord, , drop = FALSE]

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  # The `size` aesthetic ggplot2's geom_text() takes is in mm; converting
  # to points (the unit grid::gpar(fontsize=) wants) with the same
  # mm-to-pt factor ggplot2 itself uses internally (72.27/25.4) is what
  # makes this measurement track the real render, not just a
  # self-consistent but arbitrarily-scaled estimate.
  pt_size <- sizes * (72.27 / 25.4)
  dims <- lapply(seq_along(words), function(i) {
    g <- grid::textGrob(words[i], gp = grid::gpar(fontsize = pt_size[i], fontface = "bold"))
    c(w = as.numeric(grid::convertWidth(grid::grobWidth(g), "inches")),
      h = as.numeric(grid::convertHeight(grid::grobHeight(g), "inches")))
  })
  half_w <- vapply(dims, `[[`, numeric(1), "w") / 2 * (1 + padding)
  half_h <- vapply(dims, `[[`, numeric(1), "h") / 2 * (1 + padding)

  # Packing-efficiency factor: an Archimedean spiral search over
  # rectangular bounding boxes fills roughly half a disc's true area in
  # practice, not all of it, so the target radius is inflated to
  # compensate rather than coming out too cramped.
  max_r <- if (shape == "circle") sqrt(sum(4 * half_w * half_h) / pi / 0.5) else Inf

  placed_x <- placed_y <- placed_hw <- placed_hh <- numeric(0)
  overlaps <- function(x, y, hw, hh) {
    if (!length(placed_x)) return(FALSE)
    any(abs(x - placed_x) < (hw + placed_hw) & abs(y - placed_y) < (hh + placed_hh))
  }

  x <- y <- numeric(n)
  for (i in seq_len(n)) {
    theta <- 0
    r <- 0
    attempts <- 0L
    repeat {
      cand_x <- centers$x[i] + r * cos(theta) * aspect
      cand_y <- centers$y[i] + r * sin(theta)
      if (!overlaps(cand_x, cand_y, half_w[i], half_h[i])) break
      theta <- theta + 0.1
      r <- r + 0.012
      if (r > max_r) {
        # Wrap back toward the centre rather than growing past the
        # target disc: jump to a substantially different angle so the
        # retry does not just re-walk the same failed trajectory.
        r <- 0.02
        theta <- theta + pi / 3
      }
      attempts <- attempts + 1L
      if (attempts > 4000L) break  # pathological fallback; not hit in practice
    }
    x[i] <- cand_x
    y[i] <- cand_y
    placed_x  <- c(placed_x,  cand_x)
    placed_y  <- c(placed_y,  cand_y)
    placed_hw <- c(placed_hw, half_w[i])
    placed_hh <- c(placed_hh, half_h[i])
  }

  # half_w/half_h ride along so a caller can compute the plot's actual
  # extent (x +/- half_w, y +/- half_h), not just the anchor points: a
  # coord_fixed() built from the anchor points alone clips every word's
  # far edge, which is exactly what happened before this was added (long
  # words at the outer edge of the sentiment comparison cloud were cut
  # off mid-word).
  data.frame(term = words, x = x, y = y, half_w = half_w, half_h = half_h,
             stringsAsFactors = FALSE)
}

#' Term-frequency plot: horizontal bar or word cloud
#'
#' Top terms from a `term_freq` result as a horizontal bar chart, or a word
#' cloud when `result$options$wordcloud` is `TRUE` (opt-in, default
#' `FALSE`). Facets by group when the result carries a `group` role
#' (todo_text_analysis.md section 1a).
#'
#' @param result A `term_freq` result list from [run_analysis_plan()].
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object, or `NULL` when the result carries no table.
#' @export
#' @seealso [run_analysis_plan()], [term_frequency()]
sframe_plot_term_frequency <- function(result, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot term frequency.")
  palette <- match.arg(palette)
  tbl <- result$table
  if (!is.data.frame(tbl) || nrow(tbl) == 0 || !"term" %in% names(tbl)) return(NULL)
  brand <- sframe_brand(palette)
  grouped <- "group" %in% names(tbl)
  wordcloud <- isTRUE(result$options$wordcloud)

  if (wordcloud) {
    # The word cloud shows the overall top terms; a per-group cloud is not
    # a legible shape, so the grouped table is collapsed back to overall
    # frequency first when needed. Capped at 40 (not 60): the collision-
    # avoiding layout below trades word count for legibility on purpose,
    # and 40 already matches what other captions in this package call
    # "the top terms" for a word cloud.
    plot_tbl <- if (grouped) {
      stats::aggregate(n ~ term, data = tbl, FUN = sum)
    } else {
      tbl
    }
    plot_tbl <- plot_tbl[order(-plot_tbl$n), , drop = FALSE]
    plot_tbl <- utils::head(plot_tbl, 40)
    # Area-proportional sizing (size ~ sqrt(n), per the ggwordcloud
    # vignette's "true proportionality" recommendation: printed AREA
    # should track the value, not printed height) rescaled to a legible
    # point range, then held fixed via scale_size_identity() so the size
    # actually rendered is exactly the size .sframe_wordcloud_layout()
    # measured collisions against.
    rescale01 <- function(v) {
      rng <- range(v)
      if (diff(rng) == 0) return(rep(0.5, length(v)))
      (v - rng[1]) / diff(rng)
    }
    plot_tbl$size <- 5 + rescale01(sqrt(plot_tbl$n)) * 13
    # aspect = 1 (equal x/y growth) plus shape = "circle" (radius-capped,
    # wrapped fill) is the circular word cloud shape.
    layout <- .sframe_wordcloud_layout(plot_tbl$term, plot_tbl$size,
                                       aspect = 1, shape = "circle")
    plot_tbl <- merge(plot_tbl, layout, by = "term")
    # Tight coordinate limits from the actual placed extents (each word's
    # anchor point +/- its OWN measured half-width/half-height, not just
    # the anchor points themselves), rather than letting ggplot2's default
    # expansion add a wide empty band, or clipping a word whose far edge
    # extends past its anchor point.
    xlim <- range(c(plot_tbl$x - plot_tbl$half_w, plot_tbl$x + plot_tbl$half_w))
    ylim <- range(c(plot_tbl$y - plot_tbl$half_h, plot_tbl$y + plot_tbl$half_h))
    return(
      ggplot2::ggplot(plot_tbl, ggplot2::aes(x = .data$x, y = .data$y,
                                             label = .data$term, size = .data$size,
                                             alpha = .data$n)) +
        # Colour is a fixed hue (brand$teal); ALPHA is what varies
        # dark-to-light with frequency, so the more frequent (already
        # bigger) a term is, the darker it also reads, and a term is
        # never lightened past a WCAG-conscious floor (0.5, not down to
        # near-invisible) even at the bottom of the frequency range.
        ggplot2::geom_text(colour = brand$teal, fontface = "bold") +
        ggplot2::scale_size_identity() +
        ggplot2::scale_alpha_continuous(range = c(0.5, 1), guide = "none") +
        ggplot2::coord_fixed(xlim = xlim, ylim = ylim, expand = TRUE) +
        ggplot2::labs(title = paste("Term cloud for", result$variable %||% "")) +
        ggplot2::theme_void() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
    )
  }

  .sframe_plot_term_bar(tbl, title = paste("Top terms for", result$variable %||% ""),
                        palette = palette, brand = brand, grouped = grouped)
}

# Shared horizontal-bar builder behind sframe_plot_term_frequency() (the
# non-word-cloud path) and sframe_plot_ngram_frequency(): top-20-per-group
# term bars, term/n-gram terms ordered by frequency, with optional group
# faceting. `tbl` needs `term` and `n` columns and, when `grouped` is `TRUE`,
# a `group` column.
.sframe_plot_term_bar <- function(tbl, title, palette, brand, grouped = FALSE) {
  bar_tbl <- if (grouped) tbl else within(tbl, group <- "all")
  bar_tbl <- do.call(rbind, lapply(split(bar_tbl, bar_tbl$group), function(d) {
    utils::head(d[order(-d$n), , drop = FALSE], 20)
  }))
  # A single global factor level list (one position per distinct term,
  # largest anywhere in the combined table wins) is wrong for a faceted
  # chart with `scales = "free_y"`: the same term can appear in more than
  # one group at a different frequency, but a shared factor only has ONE
  # position for it, so a facet's own bars come out sorted by whichever
  # group happened to set that position, not that facet's own values --
  # confirmed by rendering a real 2-group case (a term with n=18 in one
  # facet was not visually near the top of that facet at all). The
  # standard fix (the same one `tidytext::reorder_within()` automates):
  # make the factor level itself carry the group, sort per group, then
  # strip the group suffix back off only for the printed label.
  bar_tbl$term_facet <- paste(bar_tbl$term, bar_tbl$group, sep = "\r")
  bar_tbl <- bar_tbl[order(bar_tbl$group, bar_tbl$n), ]
  # Ascending factor levels (smallest first, largest last): after
  # coord_flip(), ggplot2 draws the LAST level at the top, so this is
  # what puts the largest bar at the top of the chart, matching the
  # standard word-frequency convention (e.g. the LADAL tutorial's
  # frequency plots). The previous rev() here put the smallest bar on
  # top instead -- confirmed by rendering, not just read.
  bar_tbl$term_facet <- factor(bar_tbl$term_facet, levels = unique(bar_tbl$term_facet))
  p <- ggplot2::ggplot(bar_tbl, ggplot2::aes(x = .data$term_facet, y = .data$n)) +
    ggplot2::geom_col(fill = brand$fill, colour = brand$ink, linewidth = 0.3, width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(labels = function(x) sub("\r.*$", "", x)) +
    ggplot2::labs(title = title, x = NULL, y = "Frequency") +
    theme_surveyframe(palette = palette)
  if (grouped) p <- p + ggplot2::facet_wrap(~ group, scales = "free_y")
  p
}

#' N-gram-frequency plot: horizontal bar
#'
#' Top 20 n-grams from an `ngram_freq` result as a horizontal bar chart.
#' Shares its bar-building logic with [sframe_plot_term_frequency()]'s bar
#' path via the internal `.sframe_plot_term_bar()` helper; unlike that
#' function, there is no word-cloud mode and no group faceting for this id.
#'
#' @param result An `ngram_freq` result list from [run_analysis_plan()].
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object, or `NULL` when the result carries no table.
#' @export
#' @seealso [run_analysis_plan()], [ngram_frequency()]
sframe_plot_ngram_frequency <- function(result, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot n-gram frequency.")
  palette <- match.arg(palette)
  tbl <- result$table
  if (!is.data.frame(tbl) || nrow(tbl) == 0 || !"term" %in% names(tbl)) return(NULL)
  brand <- sframe_brand(palette)
  .sframe_plot_term_bar(tbl, title = paste("Top n-grams for", result$variable %||% ""),
                        palette = palette, brand = brand, grouped = FALSE)
}

#' Term co-occurrence heatmap
#'
#' Tile heatmap of pairwise within-response term co-occurrence counts for a
#' `co_occurrence` result. The result's edge list (`term_a`, `term_b`, `n`)
#' is pivoted into a full symmetric term-by-term grid before plotting, so
#' each pair's tile appears twice, once on either side of the diagonal, the
#' way the other tile heatmaps in this file (`sframe_plot_correlation_matrix()`,
#' `sframe_plot_efa_loadings()`) read as a full grid rather than a triangle.
#'
#' @param result A `co_occurrence` result list from [run_analysis_plan()].
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object, or `NULL` when the result carries no table.
#' @export
#' @seealso [run_analysis_plan()], [term_frequency()]
sframe_plot_cooccurrence <- function(result, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot term co-occurrence.")
  palette <- match.arg(palette)
  tbl <- result$table
  if (!is.data.frame(tbl) || nrow(tbl) == 0 ||
      !all(c("term_a", "term_b", "n") %in% names(tbl))) {
    return(NULL)
  }
  brand <- sframe_brand(palette)
  terms <- sort(unique(c(tbl$term_a, tbl$term_b)))
  # Mirror every pair into both triangles so the grid reads symmetrically;
  # the diagonal (a term against itself) carries no co-occurrence, so it is
  # left at 0 rather than showing a term's own frequency.
  long <- rbind(
    data.frame(term_a = tbl$term_a, term_b = tbl$term_b, n = tbl$n),
    data.frame(term_a = tbl$term_b, term_b = tbl$term_a, n = tbl$n)
  )
  long$term_a <- factor(long$term_a, levels = terms)
  long$term_b <- factor(long$term_b, levels = rev(terms))
  # sframe_heatmap_label_colour() expects a magnitude on roughly a 0-1 (or
  # -1 to 1) scale; n is an unbounded count, so normalise against the
  # largest count in the table before asking it which tiles need white text.
  long$label_colour <- sframe_heatmap_label_colour(long$n / max(tbl$n), brand$ink)
  fill_high <- if (palette == "web") brand$teal else brand$muted
  ggplot2::ggplot(long, ggplot2::aes(x = .data$term_a, y = .data$term_b)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$n), colour = brand$ink, linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = .data$n, colour = .data$label_colour), size = 3) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_gradient(low = "white", high = fill_high,
                                 limits = c(0, max(tbl$n))) +
    ggplot2::labs(title = paste("Term co-occurrence for", result$variable %||% ""),
                  x = NULL, y = NULL, fill = "n") +
    theme_surveyframe(palette = palette) + sframe_theme_angled_x()
}

sframe_plot_crosstab <- function(result, palette = c("web", "print")) {
  palette <- match.arg(palette)
  tbl <- result$table
  if (!is.data.frame(tbl) || nrow(tbl) == 0) return(NULL)
  brand <- sframe_brand(palette)
  long <- as.data.frame(as.table(as.matrix(tbl)), stringsAsFactors = FALSE)
  names(long) <- c("Row", "Column", "Count")
  ggplot2::ggplot(long, ggplot2::aes(x = .data$Row, y = .data$Count,
                                     fill = .data$Column)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.78),
                      width = 0.7, colour = brand$ink, linewidth = 0.3) +
    ggplot2::scale_fill_manual(
      values = sframe_series_fill_colours(length(unique(long$Column)), palette)) +
    ggplot2::labs(
      title = paste("Association between", result$vars[1], "and", result$vars[2]),
      x = result$vars[1], y = "Count", fill = result$vars[2]
    ) +
    theme_surveyframe(palette = palette) + sframe_theme_angled_x()
}

sframe_plot_correlation <- function(result, data, palette = c("web", "print")) {
  palette <- match.arg(palette)
  vars <- result$vars
  if (length(vars) < 2 || !all(vars[1:2] %in% colnames(data))) return(NULL)
  brand <- sframe_brand(palette)
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
      title    = paste("Relationship between", .sframe_title_case_names(vars[1]),
                       "and", .sframe_title_case_names(vars[2])),
      subtitle = result$apa %||% NULL,
      x = .sframe_title_case_names(vars[1]), y = .sframe_title_case_names(vars[2])
    ) +
    theme_surveyframe(palette = palette)
}

sframe_plot_regression <- function(result, data, palette = c("web", "print")) {
  palette <- match.arg(palette)
  vars <- result$vars
  if (length(vars) < 2 || !all(vars %in% colnames(data))) return(NULL)
  outcome <- vars[length(vars)]
  predictors <- vars[-length(vars)]
  brand <- sframe_brand(palette)
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
        title    = paste(.sframe_title_case_names(outcome), "predicted by",
                         .sframe_title_case_names(predictors)),
        subtitle = result$apa %||% NULL,
        x = .sframe_title_case_names(predictors), y = .sframe_title_case_names(outcome)
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
        title    = paste("Observed against fitted values for", .sframe_title_case_names(outcome)),
        subtitle = result$apa %||% NULL,
        x = "Fitted values", y = "Observed values"
      )
  }
  p + theme_surveyframe(palette = palette)
}

#' Diverging stacked bar for a single Likert item (base graphics)
#'
#' Base graphics only (no ggplot2 dependency), so it draws in the report's
#' distributions section regardless of whether ggplot2 is installed,
#' including from the Quarto report template, which runs in its own
#' `library(surveyframe)` session and cannot see unexported functions.
#' `counts` is a named numeric vector in scale order (names are the response
#' labels, e.g. "Strongly disagree" .. "Strongly agree"), not sorted
#' alphabetically or by frequency. The middle category of an odd-length
#' scale is treated as neutral and split evenly across the zero line. An
#' even-length scale has no neutral category. This is the standard
#' survey-report convention (Pew Research, SurveyMonkey) for visualising an
#' ordered agree/disagree scale, and reads in one glance which way opinion
#' leans, unlike a plain frequency bar.
#'
#' Kept horizontal deliberately: this is the one chart in the package where
#' the horizontal orientation is the domain convention, not an accident, and
#' a vertical diverging stack is materially harder to read for this specific
#' shape (see the file-level note in the roxygen docs of the ggplot2
#' equivalents above). Position (left of zero vs right of zero) carries the
#' primary signal either way, so it also satisfies "do not rely on colour
#' alone" regardless of palette. In print mode, the two poles are further
#' distinguished by a diagonal hatch on the "disagree" side, not colour tone
#' alone.
#'
#' @param counts Named numeric vector of response counts, in scale order.
#' @param theme_color Character. Hex colour for the "agree" pole.
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return Invisibly `NULL`, called for its plotting side effect on the
#'   current graphics device.
#' @export
#' @keywords internal
#' @seealso [sframe_plot_item_chart()]
sframe_draw_likert_diverging <- function(counts, theme_color = "#16B3B1",
                                         palette = c("web", "print")) {
  palette <- match.arg(palette)
  brand <- sframe_brand(palette)
  counts <- counts[!is.na(counts)]
  n <- length(counts)
  if (n < 2 || sum(counts) == 0) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "Not enough data to plot.", col = brand$muted)
    return(invisible(NULL))
  }
  pct <- 100 * as.numeric(counts) / sum(counts)
  labels <- names(counts) %||% paste0("Level ", seq_len(n))

  half <- n %/% 2
  neg_idx <- seq_len(half)
  pos_idx <- seq.int(n - half + 1L, n)
  has_neutral <- (n %% 2L) == 1L
  neu_idx <- if (has_neutral) half + 1L else integer(0)

  if (palette == "web") {
    # Darkest at the pole (Strongly disagree / Strongly agree), lightest next
    # to neutral, so saturation itself signals intensity of opinion.
    neg_ramp <- grDevices::colorRampPalette(c("#b3261e", "#f2b6ae"))(max(1L, half))
    pos_ramp <- grDevices::colorRampPalette(c("#a6ded9", theme_color))(max(1L, half))
    neu_col  <- "#c7cdd6"
    neg_density <- NA; pos_density <- NA # solid fills
  } else {
    # Lightened deliberately: these are large solid-filled segments (can be
    # most of the bar for a lopsided distribution), so even the darkest pole
    # stays well short of black to keep print ink usage reasonable. The
    # disagree side's hatching (below) is the primary way that side reads as
    # distinct, not fill darkness alone.
    neg_ramp <- grDevices::colorRampPalette(c("#595959", "#d9d9d9"))(max(1L, half))
    pos_ramp <- grDevices::colorRampPalette(c("#e8e8e8", "#8c8c8c"))(max(1L, half))
    neu_col  <- "#ececec"
    neg_density <- 22; pos_density <- NA # hatched disagree side, solid agree side
  }

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
    graphics::rect(x, 0.28, x + left_widths[i], 0.72, col = left_colors[i],
                   border = brand$ink, density = neg_density, angle = 45)
    x <- x + left_widths[i]
  }
  if (has_neutral) {
    graphics::rect(-neu_half, 0.28, neu_half, 0.72, col = neu_col, border = brand$ink)
    x <- neu_half
  }
  for (i in seq_along(right_widths)) {
    graphics::rect(x, 0.28, x + right_widths[i], 0.72, col = right_colors[i],
                   border = brand$ink, density = pos_density)
    x <- x + right_widths[i]
  }
  graphics::segments(0, 0.15, 0, 0.85, col = brand$ink, lwd = 1.2)
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
                     legend = leg_labels, fill = ord_colors, border = brand$ink,
                     bty = "n", cex = 0.72, ncol = n, xpd = NA, x.intersp = 0.6)
  } else {
    graphics::legend(x = mean(usr[1:2]), y = usr[3], xjust = 0.5, yjust = 1,
                     legend = leg_labels, fill = ord_colors, border = brand$ink,
                     bty = "n", cex = 0.68, ncol = 1, xpd = NA, x.intersp = 0.6)
  }
}

# Shared core for both grouped-Likert chart builders below: draws one
# diverging bar per row inside a single chart, sharing one x scale and one
# legend. `counts_by_row` is a named list (name = the row's display label,
# in the desired top-to-bottom-reversed drawing order; value = a table()/
# named-vector of raw response counts over `scale_values`). Splitting this
# out means a matrix question's rows and a scale's separate items can share
# exactly the same diverging-stack maths and styling.
.sframe_likert_grouped_plot <- function(counts_by_row, scale_values, scale_labels,
                                        title, palette) {
  brand <- sframe_brand(palette)
  n <- length(scale_values)
  if (n < 2 || length(counts_by_row) == 0) return(NULL)
  half <- n %/% 2
  neg_idx <- seq_len(half)
  pos_idx <- seq.int(n - half + 1L, n)
  has_neutral <- (n %% 2L) == 1L
  neu_idx <- if (has_neutral) half + 1L else integer(0)

  # Each row's segments are computed independently, exactly like the
  # single-item chart's own maths, then stacked outward from zero so every
  # row shares the same zero line regardless of how lopsided its own
  # distribution is.
  segs <- list()
  for (row in names(counts_by_row)) {
    counts <- counts_by_row[[row]]
    if (sum(counts) == 0) next
    pct <- 100 * as.numeric(counts) / sum(counts)

    x <- 0
    for (i in rev(neg_idx)) {
      w <- pct[i]
      segs[[length(segs) + 1]] <- data.frame(
        row = row, category = scale_labels[i],
        xmin = -(x + w), xmax = -x, stringsAsFactors = FALSE
      )
      x <- x + w
    }
    if (has_neutral) {
      w <- pct[neu_idx] / 2
      segs[[length(segs) + 1]] <- data.frame(
        row = row, category = scale_labels[neu_idx],
        xmin = -w, xmax = w, stringsAsFactors = FALSE
      )
    }
    x <- if (has_neutral) pct[neu_idx] / 2 else 0
    for (i in pos_idx) {
      w <- pct[i]
      segs[[length(segs) + 1]] <- data.frame(
        row = row, category = scale_labels[i],
        xmin = x, xmax = x + w, stringsAsFactors = FALSE
      )
      x <- x + w
    }
  }
  if (!length(segs)) return(NULL)
  df <- do.call(rbind, segs)
  drawn_rows <- names(counts_by_row)[names(counts_by_row) %in% df$row]
  df$row <- factor(df$row, levels = rev(drawn_rows))
  df$category <- factor(df$category, levels = scale_labels)
  df$ypos <- as.numeric(df$row)

  if (palette == "web") {
    neg_ramp <- grDevices::colorRampPalette(c("#b3261e", "#f2b6ae"))(max(1L, half))
    pos_ramp <- grDevices::colorRampPalette(c("#a6ded9", brand$teal))(max(1L, half))
    neu_col  <- "#c7cdd6"
  } else {
    neg_ramp <- grDevices::colorRampPalette(c("#595959", "#d9d9d9"))(max(1L, half))
    pos_ramp <- grDevices::colorRampPalette(c("#e8e8e8", "#8c8c8c"))(max(1L, half))
    neu_col  <- "#ececec"
  }
  fill_values <- stats::setNames(
    c(neg_ramp, if (has_neutral) neu_col else NULL, pos_ramp),
    scale_labels
  )

  ggplot2::ggplot(df) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                   ymin = .data$ypos - 0.4, ymax = .data$ypos + 0.4,
                   fill = .data$category),
      colour = brand$ink, linewidth = 0.3
    ) +
    ggplot2::geom_vline(xintercept = 0, colour = brand$ink, linewidth = 0.5) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(levels(df$row)), labels = levels(df$row),
      limits = c(0.5, length(levels(df$row)) + 0.5), expand = c(0, 0)
    ) +
    ggplot2::scale_fill_manual(
      values = fill_values, breaks = scale_labels,
      # A long category label ("Neither agree nor disagree") wrapped onto
      # 2 lines takes less horizontal space per legend key than one long
      # line, which matters at a fixed report width.
      labels = function(l) vapply(l, function(s) paste(strwrap(s, width = 14), collapse = "\n"), character(1))
    ) +
    # Wrapped into 2 rows once there are more than 3 categories: a single
    # row of 4-5 category labels plus their percentages routinely overflows
    # a report-width chart, and ggplot2 does not reflow a bottom legend on
    # its own at a fixed export width.
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = if (n > 3) 2 else 1, byrow = TRUE)) +
    ggplot2::labs(title = title, x = "Percent", y = NULL, fill = NULL) +
    theme_surveyframe(palette = palette) +
    ggplot2::theme(legend.position = "bottom", panel.grid = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 9))
}

#' Grouped diverging chart for a Likert matrix question
#'
#' A matrix question asks several rows against one shared response scale
#' (a "grid" of Likert items). Plotting each row as its own separate
#' [sframe_draw_likert_diverging()] chart loses the grouping the question
#' was designed with, so this draws every row as one diverging bar inside a
#' single chart, sharing one x scale and one legend, the standard way a
#' Likert matrix is reported (compare a typical multi-item satisfaction
#' grid). Same diverging-stack convention as the single-item chart: the
#' middle category of an odd-length scale is neutral and split evenly
#' across the zero line, and colour saturation increases toward each pole.
#'
#' @param item A `"matrix"` sframe item, with `matrix_items` (the row
#'   labels) and a `choice_set` naming the shared response scale.
#' @param data The response data.frame, with one expanded
#'   `<item id>__<row label>` column per matrix row, as produced by
#'   [read_responses()].
#' @param choice_set The item's choice set object (`values`, `labels`),
#'   typically looked up from `instrument$choices` by `item$choice_set`.
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object, or `NULL` if no row has response data.
#' @export
#' @seealso [sframe_draw_likert_diverging()]
sframe_plot_likert_matrix <- function(item, data, choice_set, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot a Likert matrix.")
  palette <- match.arg(palette)
  rows <- item$matrix_items %||% character(0)
  if (!length(rows) || is.null(choice_set)) return(NULL)
  scale_values <- as.character(choice_set$values)
  scale_labels <- choice_set$labels %||% scale_values

  counts_by_row <- stats::setNames(lapply(rows, function(row) {
    col <- paste0(item$id, "__", row)
    if (!col %in% colnames(data)) return(NULL)
    table(factor(data[[col]], levels = scale_values))
  }), rows)
  counts_by_row <- Filter(Negate(is.null), counts_by_row)

  .sframe_likert_grouped_plot(counts_by_row, scale_values, scale_labels,
                              title = item$label %||% item$id, palette = palette)
}

#' Grouped diverging chart for a scale's Likert items
#'
#' Several *separate* Likert items that make up one [sf_scale()] (unlike a
#' `"matrix"` item's rows, which are one question) are, by default, each
#' reported as their own single-item diverging chart. That scatters a
#' related batch of items (a satisfaction scale's 2-3 items, say) across
#' several charts instead of showing them the way a Likert matrix or a
#' typical multi-item satisfaction grid is reported: one grouped chart, one
#' diverging bar per item, sharing an x scale and a legend. Applies only
#' when every item in the scale shares the same choice set. Scales that mix
#' response scales fall back to one chart per item.
#'
#' @param items A list of `"likert"` sframe items belonging to one scale, in
#'   display order.
#' @param data The response data.frame, with one column per item id.
#' @param choice_set The shared choice set object (`values`, `labels`).
#' @param title Chart title, typically the scale's label.
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object, or `NULL` if no item has response data.
#' @export
#' @seealso [sframe_plot_likert_matrix()], [sf_scale()]
sframe_plot_likert_scale <- function(items, data, choice_set, title, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot a scale's Likert items.")
  palette <- match.arg(palette)
  if (!length(items) || is.null(choice_set)) return(NULL)
  scale_values <- as.character(choice_set$values)
  scale_labels <- choice_set$labels %||% scale_values

  row_labels <- vapply(items, function(i) i$label %||% i$id, character(1))
  counts_by_row <- stats::setNames(lapply(items, function(i) {
    if (!i$id %in% colnames(data)) return(NULL)
    table(factor(data[[i$id]], levels = scale_values))
  }), row_labels)
  counts_by_row <- Filter(Negate(is.null), counts_by_row)

  .sframe_likert_grouped_plot(counts_by_row, scale_values, scale_labels,
                              title = title, palette = palette)
}

#' Group a scale's Likert items for a combined diverging chart
#'
#' Identifies which of an instrument's scales are eligible for one grouped
#' diverging chart across their member items ([sframe_plot_likert_scale()]),
#' the same way a `"matrix"` question's rows are grouped
#' ([sframe_plot_likert_matrix()]): every member item is `"likert"` type and
#' all share one choice set. Scales that mix response scales, that resolve
#' to fewer than 2 qualifying items, or whose choice set cannot be found are
#' left out and fall back to one chart per item in the report's Response
#' distributions section.
#'
#' @param instrument An `sframe` object.
#' @return A named list, one entry per eligible scale (named by scale id),
#'   each a list with `scale_id`, `title` (the scale's label), `items` (the
#'   member item objects, in scale order), and `choice_set` (the shared
#'   choice set object). Empty list if no scale qualifies.
#' @export
#' @seealso [sframe_plot_likert_scale()], [sf_scale()]
sframe_likert_scale_groups <- function(instrument) {
  choice_by <- function(id) {
    for (cs in instrument$choices %||% list()) if (identical(cs$id, id)) return(cs)
    NULL
  }
  item_by <- function(id) {
    for (i in instrument$items %||% list()) if (identical(i$id, id)) return(i)
    NULL
  }
  groups <- list()
  for (scale in instrument$scales %||% list()) {
    items <- Filter(Negate(is.null), lapply(scale$items %||% character(0), item_by))
    items <- Filter(function(i) identical(i$type, "likert"), items)
    if (length(items) < 2) next
    cs_ids <- unique(vapply(items, function(i) i$choice_set %||% "", character(1)))
    if (length(cs_ids) != 1 || !nzchar(cs_ids)) next
    cs <- choice_by(cs_ids)
    if (is.null(cs)) next
    groups[[scale$id]] <- list(
      scale_id = scale$id, title = scale$label %||% scale$id,
      items = items, choice_set = cs
    )
  }
  groups
}

# Dispatch a runner result to its plot builder. Returns NULL for runner
# types outside the v0.3.4 plot family so callers can attach conditionally.
sframe_plot_for_result <- function(result, data, palette = c("web", "print")) {
  palette <- match.arg(palette)
  if (!is.list(result) || !is.null(result$error)) return(NULL)
  test <- result$test %||% ""
  builder <- switch(
    test,
    frequency           = function() sframe_plot_frequency(result, palette),
    crosstab            = ,
    chi_square          = function() sframe_plot_crosstab(result, palette),
    correlation_pearson = ,
    correlation_spearman = ,
    correlation_kendall = function() sframe_plot_correlation(result, data, palette),
    regression_linear   = function() sframe_plot_regression(result, data, palette),
    t_test_ind          = ,
    mann_whitney        = ,
    kruskal_wallis      = ,
    anova_one           = function() sframe_plot_group_comparison(result, data, palette),
    t_test_pair         = ,
    wilcoxon_pair       = function() sframe_plot_paired_comparison(result, data, palette),
    ancova              = function() sframe_plot_group_comparison(result, data, palette),
    fisher_exact        = function() sframe_plot_crosstab(result, palette),
    repeated_anova      = ,
    friedman            = function() sframe_plot_repeated_measures(result, data, palette),
    partial_correlation = function() sframe_plot_partial_correlation(result, data, palette),
    regression_logistic_binary  = ,
    firth_logistic              = ,
    regression_logistic_ordinal = function() sframe_plot_logistic_coefficients(result, palette),
    topsis              = ,
    ahp                 = ,
    anp                 = ,
    vikor               = ,
    moora               = ,
    smart               = ,
    waspas              = ,
    promethee           = ,
    electre             = function() sframe_plot_decision_ranking(result, palette),
    dematel             = function() sframe_plot_dematel_influence(result, palette),
    moderation          = function() sframe_plot_moderation(result, data, palette),
    mediation           = function() sframe_plot_mediation(result, palette),
    missing_data        = function() {
      if (is.null(result$report_obj)) return(NULL)
      graphics::plot(result$report_obj, palette = palette)
    },
    # quality/reliability_*/efa_*/item_diagnostics results come from
    # sframe_result_from_report(), which keeps the original classed report
    # object in $report_obj so plot() can dispatch normally instead of
    # re-iterating the analysis-plan-field-merged result list as if it were
    # still report-shaped.
    quality             = ,
    reliability_alpha   = ,
    reliability_omega   = ,
    efa_readiness       = ,
    efa_solution        = ,
    descriptives        = function() {
      if (is.null(result$report_obj)) return(NULL)
      graphics::plot(result$report_obj, data = data, palette = palette)
    },
    item_diagnostics    = function() sframe_plot_item_diagnostics(result, palette),
    term_freq           = function() sframe_plot_term_frequency(result, palette),
    co_occurrence       = function() sframe_plot_cooccurrence(result, palette),
    topic_model_lda     = ,
    stm_topics          = function() sframe_plot_topics(result, palette),
    ngram_freq          = function() sframe_plot_ngram_frequency(result, palette),
    co_occurrence_network = function() sframe_plot_cooccurrence_network(result, palette),
    tidy_sentiment      = function() sframe_plot_sentiment(result, palette),
    NULL
  )
  if (is.null(builder)) return(NULL)
  tryCatch(builder(), error = function(e) NULL)
}

#' Ranked-score bar chart for a decision-family result
#'
#' The shared chart for every MCDM ranking method: one horizontal bar per
#' alternative, ordered best first, with the leading alternative picked out.
#' It is generic over the method rather than tied to one, so AHP criterion
#' weights and any ranking method's scores all draw through it. The score
#' column is whatever the method reports as its headline quantity (a
#' closeness coefficient, a net flow, a priority weight), so the axis is
#' labelled from the result rather than hard-coded.
#'
#' @param result A decision-family result list from [run_analysis_plan()],
#'   carrying either `scores` and `alternatives` or a ranking `table`.
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object, or `NULL` when the result carries no ranking.
#' @export
#' @seealso [run_analysis_plan()]
sframe_plot_decision_ranking <- function(result, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot a decision ranking.")
  palette <- match.arg(palette)
  brand <- sframe_brand(palette)

  unit <- "Alternative"
  if (!is.null(result$scores) && !is.null(result$alternatives) &&
      length(result$scores) == length(result$alternatives)) {
    df <- data.frame(
      label = as.character(result$alternatives),
      score = as.numeric(result$scores),
      stringsAsFactors = FALSE
    )
  } else {
    tbl <- result$table
    if (!is.data.frame(tbl) || nrow(tbl) == 0 || ncol(tbl) < 2) return(NULL)
    unit <- names(tbl)[1]
    df <- data.frame(
      label = as.character(tbl[[1]]),
      score = suppressWarnings(as.numeric(tbl[[2]])),
      stringsAsFactors = FALSE
    )
  }
  df <- df[!is.na(df$score), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  df <- df[order(df$score, decreasing = TRUE), , drop = FALSE]
  # Ordering carries the ranking, so the bars stay one colour: a second
  # encoding for "best" would add a colour-only distinction for no gain.
  df$label <- factor(df$label, levels = rev(df$label))
  score_label <- result$score_label %||% "Score"
  method_label <- toupper(result$test %||% "decision")

  ggplot2::ggplot(df, ggplot2::aes(x = .data$label, y = .data$score)) +
    ggplot2::geom_col(fill = brand$fill, colour = brand$ink,
                      linewidth = 0.3, width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste(method_label, "ranking"),
      x = unit, y = score_label
    ) +
    theme_surveyframe(palette = palette)
}

# Weight-sensitivity plot. One bar per criterion and direction, showing the
# Spearman correlation between the base ranking and the ranking after that
# weight is nudged. A short bar marks a criterion the result leans on. The
# dashed reference at rho = 1 is where the ranking did not move at all, so
# the visible gap from that line is the whole message.
sframe_plot_sensitivity <- function(result, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot a sensitivity analysis.")
  palette <- match.arg(palette)
  brand <- sframe_brand(palette)

  tbl <- result$table
  if (!is.data.frame(tbl) || nrow(tbl) == 0) return(NULL)
  df <- tbl[!is.na(tbl$rho), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  df$label <- paste0(df$criterion, " (", df$direction, ")")
  df <- df[order(df$rho, df$label), , drop = FALSE]
  df$label <- factor(df$label, levels = rev(unique(df$label)))

  ggplot2::ggplot(df, ggplot2::aes(x = .data$label, y = .data$rho)) +
    ggplot2::geom_col(fill = brand$fill, colour = brand$ink,
                      linewidth = 0.3, width = 0.72) +
    ggplot2::geom_hline(yintercept = 1, colour = brand$ink,
                        linetype = "dashed", linewidth = 0.4) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = sprintf("%s ranking stability under a %.0f%% weight change",
                      toupper(result$method %||% "decision"),
                      (result$delta %||% 0.05) * 100),
      subtitle = paste0("Spearman correlation with the base ranking. The ",
                        "dashed line is an unchanged ranking."),
      x = "Criterion perturbed", y = "Rank correlation"
    ) +
    theme_surveyframe(palette = palette)
}

# ---------------------------------------------------------------------------
# v0.3.4 visualisation breadth: regression diagnostics, EFA, reliability,
# mosaic, and correlation-matrix plots, plus the plot() S3 methods that
# dispatch to them from report objects.
# ---------------------------------------------------------------------------

#' Regression diagnostic plots for a regression_linear result
#'
#' The four standard diagnostic panels (residuals vs fitted, normal Q-Q,
#' scale-location, residuals vs leverage), built from the plain data frame
#' [run_analysis_plan()] attaches to a `regression_linear` result rather than
#' the `lm` object itself, so the result stays JSON-serialisable.
#'
#' @param result A `regression_linear` result list containing a `diagnostics`
#'   data frame (as produced internally by [run_analysis_plan()]).
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A named list of four ggplot2 objects (`residuals_fitted`, `qq`,
#'   `scale_location`, `leverage`), or `NULL` if diagnostics are unavailable.
#' @export
#' @seealso [run_analysis_plan()]
sframe_plot_regression_diagnostics <- function(result, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot regression diagnostics.")
  palette <- match.arg(palette)
  d <- result$diagnostics
  if (!is.data.frame(d) || nrow(d) == 0) return(NULL)
  brand <- sframe_brand(palette)

  residuals_fitted <- ggplot2::ggplot(d, ggplot2::aes(x = .data$fitted, y = .data$resid)) +
    ggplot2::geom_hline(yintercept = 0, colour = brand$muted, linetype = "dashed") +
    ggplot2::geom_point(colour = brand$teal, alpha = 0.75, size = 2) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                         colour = brand$ink, linewidth = 0.6) +
    ggplot2::labs(title = "Residuals vs fitted", x = "Fitted values", y = "Residuals") +
    theme_surveyframe(palette = palette)

  qq_theoretical <- stats::qqnorm(d$std_resid, plot.it = FALSE)
  qq_df <- data.frame(theoretical = qq_theoretical$x, sample = qq_theoretical$y)
  qq <- ggplot2::ggplot(qq_df, ggplot2::aes(x = .data$theoretical, y = .data$sample)) +
    ggplot2::geom_abline(colour = brand$muted, linetype = "dashed") +
    ggplot2::geom_point(colour = brand$teal, alpha = 0.75, size = 2) +
    ggplot2::labs(title = "Normal Q-Q", x = "Theoretical quantiles",
                 y = "Standardised residuals") +
    theme_surveyframe(palette = palette)

  scale_location <- ggplot2::ggplot(
      d, ggplot2::aes(x = .data$fitted, y = sqrt(abs(.data$std_resid)))) +
    ggplot2::geom_point(colour = brand$teal, alpha = 0.75, size = 2) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                         colour = brand$ink, linewidth = 0.6) +
    ggplot2::labs(title = "Scale-location", x = "Fitted values",
                 y = expression(sqrt("|Standardised residuals|"))) +
    theme_surveyframe(palette = palette)

  leverage <- ggplot2::ggplot(d, ggplot2::aes(x = .data$hat, y = .data$std_resid)) +
    ggplot2::geom_hline(yintercept = 0, colour = brand$muted, linetype = "dashed") +
    ggplot2::geom_point(ggplot2::aes(size = .data$cooksd), colour = brand$teal, alpha = 0.75) +
    ggplot2::scale_size_continuous(range = c(1, 5), guide = "none") +
    ggplot2::labs(title = "Residuals vs leverage", x = "Leverage",
                 y = "Standardised residuals") +
    theme_surveyframe(palette = palette)

  list(residuals_fitted = residuals_fitted, qq = qq,
       scale_location = scale_location, leverage = leverage)
}

#' Scree plot from an EFA readiness report
#'
#' Plots the parallel-analysis eigenvalues from [efa_report()] (both the
#' observed factor-analysis eigenvalues and the simulated comparison line),
#' with the suggested factor count marked.
#'
#' @param x An `sframe_efa_report` object from [efa_report()].
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object.
#' @export
#' @seealso [efa_report()]
sframe_plot_efa_scree <- function(x, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot an EFA scree plot.")
  palette <- match.arg(palette)
  stopifnot(inherits(x, "sframe_efa_report"))
  brand <- sframe_brand(palette)
  fa_values <- x$parallel$fa.values
  fa_sim    <- x$parallel$fa.sim
  n <- length(fa_values)
  df <- data.frame(
    factor  = rep(seq_len(n), 2),
    value   = c(fa_values, if (length(fa_sim) == n) fa_sim else rep(NA_real_, n)),
    series  = rep(c("Observed", "Simulated (95th percentile)"), each = n)
  )
  df <- df[!is.na(df$value), , drop = FALSE]
  ggplot2::ggplot(df, ggplot2::aes(x = .data$factor, y = .data$value,
                                   colour = .data$series)) +
    ggplot2::geom_vline(xintercept = x$suggested_nfactors, colour = brand$muted,
                        linetype = "dotted") +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_colour_manual(values = c("Observed" = brand$teal,
                                            "Simulated (95th percentile)" = brand$muted)) +
    ggplot2::labs(title = "EFA scree plot",
                 subtitle = sprintf("Suggested factors: %d", x$suggested_nfactors),
                 x = "Factor", y = "Eigenvalue", colour = NULL) +
    theme_surveyframe(palette = palette)
}

#' Loadings heatmap from a fitted EFA solution
#'
#' @param x An `sframe_efa_solution` object from [efa_solution()].
#' @param palette One of `"web"` (diverging red/teal gradient) or `"print"`
#'   (white-to-black gradient by magnitude, with sign conveyed by the
#'   printed label rather than colour, so it stays legible in monochrome). See
#'   `sframe_brand()`.
#' @return A ggplot2 object.
#' @export
#' @seealso [efa_solution()]
sframe_plot_efa_loadings <- function(x, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot an EFA loadings heatmap.")
  palette <- match.arg(palette)
  stopifnot(inherits(x, "sframe_efa_solution"))
  brand <- sframe_brand(palette)
  # The solution's tidy long frame (added 0.3.4); reshape only for
  # solutions serialised before it existed.
  long <- x$loadings_long
  if (is.null(long)) {
    loadings <- x$loadings
    factor_cols <- setdiff(names(loadings), "item_id")
    long <- stats::reshape(
      loadings, direction = "long", varying = factor_cols,
      v.names = "loading", timevar = "factor", times = factor_cols,
      idvar = "item_id"
    )
  }
  long$item_id <- factor(long$item_id, levels = rev(x$loadings$item_id))
  long$label_colour <- sframe_heatmap_label_colour(long$loading, brand$ink)
  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$factor, y = .data$item_id)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$loading), colour = brand$ink, linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$loading),
                                    colour = .data$label_colour), size = 3) +
    ggplot2::scale_colour_identity() +
    ggplot2::labs(title = "EFA loadings", x = "Factor", y = NULL, fill = "Loading") +
    ggplot2::scale_y_discrete(labels = .sframe_title_case_names) +
    theme_surveyframe(palette = palette)
  if (palette == "web") {
    p + ggplot2::scale_fill_gradient2(low = "#b91c1c", mid = "white", high = brand$teal,
                                      midpoint = 0, limits = c(-1, 1))
  } else {
    # Capped at muted rather than pure ink: a full page of near-black tiles
    # (a loadings/correlation heatmap can have many cells) is exactly the
    # heavy-toner problem the print palette is meant to avoid.
    p + ggplot2::aes(fill = abs(.data$loading)) +
      ggplot2::scale_fill_gradient(low = "white", high = brand$muted, limits = c(0, 1))
  }
}

#' Reliability plot: alpha and omega by scale
#'
#' @param x An `sframe_reliability_report` object from [reliability_report()].
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object.
#' @export
#' @seealso [reliability_report()]
sframe_plot_reliability <- function(x, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot a reliability report.")
  palette <- match.arg(palette)
  stopifnot(inherits(x, "sframe_reliability_report"))
  brand <- sframe_brand(palette)
  rows <- lapply(x, function(s) {
    data.frame(
      scale = s$label %||% s$scale_id,
      Alpha = s$alpha %||% NA_real_,
      Omega = s$omega_t %||% NA_real_
    )
  })
  df <- do.call(rbind, rows)
  long <- stats::reshape(df, direction = "long", varying = c("Alpha", "Omega"),
                         v.names = "value", timevar = "statistic",
                         times = c("Alpha", "Omega"), idvar = "scale")
  long <- long[!is.na(long$value), , drop = FALSE]
  # Scales whose omega could not be computed carry an omega_note; name them
  # in the subtitle so a missing bar reads as a known limitation.
  noted <- vapply(x, function(s) {
    if (is.null(s$omega_note)) "" else s$label %||% s$scale_id
  }, character(1))
  noted <- noted[nzchar(noted)]
  subtitle <- "Dashed line: 0.70 threshold"
  if (length(noted) > 0) {
    subtitle <- paste0(subtitle, ". Omega unavailable for: ",
                       paste(noted, collapse = ", "))
  }
  ggplot2::ggplot(long, ggplot2::aes(x = .data$scale, y = .data$value,
                                     fill = .data$statistic)) +
    ggplot2::geom_hline(yintercept = 0.70, colour = brand$muted, linetype = "dashed") +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.65,
                      colour = brand$ink, linewidth = 0.3) +
    ggplot2::scale_fill_manual(values = c(Alpha = brand$fill_duo[1], Omega = brand$fill_duo[2])) +
    ggplot2::labs(title = "Reliability by scale", subtitle = subtitle,
                 x = NULL, y = NULL, fill = NULL) +
    theme_surveyframe(palette = palette) + sframe_theme_angled_x()
}

#' Mosaic plot for a two-way categorical result
#'
#' Base-graphics mosaic plot (via [graphics::mosaicplot()]), matching the
#' existing base-graphics precedent in this file
#' ([sframe_draw_likert_diverging()]) so it renders without ggplot2. An
#' alternative view of the same crosstab data
#' `sframe_plot_crosstab()` renders as a grouped bar. Use whichever reads
#' better for the table's shape (mosaic scales better to unbalanced group
#' sizes).
#'
#' @param result A `crosstab`/`chi_square` result list with a contingency
#'   `table`.
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return Invisibly `NULL`, called for its plotting side effect on the
#'   current graphics device.
#' @export
#' @keywords internal
#' @seealso `sframe_plot_crosstab()`
sframe_draw_mosaic <- function(result, palette = c("web", "print")) {
  palette <- match.arg(palette)
  tbl <- result$table
  if (!is.data.frame(tbl) || nrow(tbl) == 0) return(invisible(NULL))
  brand <- sframe_brand(palette)
  mat <- as.matrix(tbl)
  graphics::mosaicplot(
    mat, main = NULL,
    xlab = result$vars[1] %||% "", ylab = result$vars[2] %||% "",
    color = sframe_series_fill_colours(ncol(mat), palette), border = brand$ink, cex.axis = 0.8
  )
  invisible(NULL)
}

#' Correlation matrix heatmap
#'
#' Computes and plots a full pairwise correlation matrix, independent of
#' [run_analysis_plan()]'s pairwise `correlation_pearson`/`_spearman`/
#' `_kendall` runners (which plot one variable pair at a time via
#' `sframe_plot_correlation()`). Useful directly, and as the visual
#' companion to [validity_report()]'s discriminant-validity checks.
#'
#' @param data A data frame of survey responses.
#' @param vars Character vector of column names to correlate.
#' @param method One of `"pearson"`, `"spearman"`, `"kendall"`.
#' @param palette One of `"web"` (diverging red/teal gradient) or `"print"`
#'   (white-to-black gradient by magnitude, signed label). See
#'   `sframe_brand()`.
#' @return A ggplot2 object.
#' @export
#' @seealso [validity_report()]
sframe_plot_correlation_matrix <- function(data, vars, method = "pearson",
                                           palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot a correlation matrix heatmap.")
  palette <- match.arg(palette)
  res <- sframe_run_correlation_matrix(data, vars, method = method)
  if (!is.null(res$error)) return(NULL)
  brand <- sframe_brand(palette)
  mat <- res$correlation_matrix
  long <- as.data.frame(as.table(mat), stringsAsFactors = FALSE)
  names(long) <- c("row", "col", "r")
  long$row <- factor(long$row, levels = rev(vars))
  long$col <- factor(long$col, levels = vars)
  long$label_colour <- sframe_heatmap_label_colour(long$r, brand$ink)
  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$col, y = .data$row)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$r), colour = brand$ink, linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$r),
                                    colour = .data$label_colour), size = 3) +
    ggplot2::scale_colour_identity() +
    ggplot2::labs(title = sprintf("%s correlation matrix",
                                  tools::toTitleCase(method)),
                 x = NULL, y = NULL, fill = "r") +
    ggplot2::scale_x_discrete(labels = .sframe_title_case_names) +
    ggplot2::scale_y_discrete(labels = .sframe_title_case_names) +
    theme_surveyframe(palette = palette)
  if (palette == "web") {
    p + ggplot2::scale_fill_gradient2(low = "#b91c1c", mid = "white", high = brand$teal,
                                      midpoint = 0, limits = c(-1, 1))
  } else {
    p + ggplot2::aes(fill = abs(.data$r)) +
      ggplot2::scale_fill_gradient(low = "white", high = brand$muted, limits = c(0, 1))
  }
}

#' Quality report plot: straight-lining flag rate by scale
#'
#' @param x An `sframe_quality_report` object from [quality_report()].
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object.
#' @export
#' @seealso [quality_report()]
sframe_plot_quality <- function(x, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot a quality report.")
  palette <- match.arg(palette)
  stopifnot(inherits(x, "sframe_quality_report"))
  brand <- sframe_brand(palette)
  rows <- lapply(x$straightline, function(s) {
    data.frame(scale = s$scale_id, flag_rate = s$flag_rate %||% NA_real_)
  })
  df <- do.call(rbind, rows)
  df <- df[!is.na(df$flag_rate), , drop = FALSE]
  # Returning NULL here reads as "nothing was flagged" and can mean "no scale
  # was long enough to check", which are opposite things. Since 0.4.1
  # straight-lining skips scales shorter than straightline_min_items, and an
  # instrument whose scales are all short (the bundled demo's are all 2 or 3
  # items) produced an empty frame and a silently missing chart. The NULL is
  # kept, because there is genuinely nothing to draw, but it now carries the
  # reason so the caller can say which case it is instead of guessing.
  if (nrow(df) == 0) return(NULL)
  # Web keeps the deliberate red "flagged" warning colour; print swaps to
  # the light neutral fill (a large solid red-analogue area would be just
  # as ink-heavy as black, and the point here is the bar height, not colour).
  bar_fill <- if (palette == "web") brand$accent else brand$fill
  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(.data$scale, -.data$flag_rate),
                                   y = .data$flag_rate)) +
    ggplot2::geom_col(fill = bar_fill, colour = brand$ink, linewidth = 0.3, width = 0.65) +
    ggplot2::scale_x_discrete(labels = .sframe_title_case_names) +
    ggplot2::scale_y_continuous(labels = scales_percent_fallback) +
    ggplot2::labs(title = "Straight-lining flag rate by scale", x = NULL, y = "Flag rate") +
    theme_surveyframe(palette = palette) + sframe_theme_angled_x()
}

# Minimal percent-label formatter so the quality plot does not need the
# `scales` package (not a dependency) just for one axis label format.
scales_percent_fallback <- function(x) sprintf("%.0f%%", x * 100)

#' @export
plot.sframe_quality_report <- function(x, ..., palette = c("web", "print")) {
  sframe_plot_quality(x, palette = match.arg(palette))
}

#' @export
plot.sframe_sensitivity <- function(x, ..., palette = c("web", "print")) {
  sframe_plot_sensitivity(x, palette = match.arg(palette))
}

#' @export
plot.sframe_reliability_report <- function(x, ..., palette = c("web", "print")) {
  sframe_plot_reliability(x, palette = match.arg(palette))
}

#' @export
plot.sframe_efa_report <- function(x, ..., palette = c("web", "print")) {
  sframe_plot_efa_scree(x, palette = match.arg(palette))
}

#' @export
plot.sframe_efa_solution <- function(x, ..., palette = c("web", "print")) {
  sframe_plot_efa_loadings(x, palette = match.arg(palette))
}

#' Validity report plot: composite reliability and AVE by construct
#'
#' @param x An `sframe_validity_report` object from [validity_report()].
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object.
#' @export
#' @seealso [validity_report()]
sframe_plot_validity <- function(x, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot a validity report.")
  palette <- match.arg(palette)
  stopifnot(inherits(x, "sframe_validity_report"))
  brand <- sframe_brand(palette)
  df <- x$reliability
  long <- stats::reshape(
    df[, c("construct", "composite_reliability", "AVE")],
    direction = "long", varying = c("composite_reliability", "AVE"),
    v.names = "value", timevar = "statistic",
    times = c("CR", "AVE"), idvar = "construct"
  )
  long <- long[!is.na(long$value), , drop = FALSE]
  ggplot2::ggplot(long, ggplot2::aes(x = .data$construct, y = .data$value,
                                     fill = .data$statistic)) +
    ggplot2::geom_hline(yintercept = 0.70, colour = brand$muted, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0.50, colour = brand$muted, linetype = "dotted") +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.65,
                      colour = brand$ink, linewidth = 0.3) +
    ggplot2::scale_fill_manual(values = c(CR = brand$fill_duo[1], AVE = brand$fill_duo[2])) +
    ggplot2::labs(title = "Construct validity",
                  subtitle = "Dashed line: 0.70 CR threshold. Dotted line: 0.50 AVE threshold.",
                  x = NULL, y = NULL, fill = NULL) +
    ggplot2::scale_x_discrete(labels = .sframe_title_case_names) +
    theme_surveyframe(palette = palette) + sframe_theme_angled_x()
}

#' @export
plot.sframe_validity_report <- function(x, ..., palette = c("web", "print")) {
  sframe_plot_validity(x, palette = match.arg(palette))
}

#' Missing-data report plot: missingness rate by item
#'
#' @param x An `sframe_missing_data_report` object from
#'   [missing_data_report()].
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object. When no item has missing values, this is a
#'   short "no missing responses" message rather than an empty bar chart.
#' @export
#' @seealso [missing_data_report()]
sframe_plot_missingness <- function(x, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot a missing-data report.")
  palette <- match.arg(palette)
  stopifnot(inherits(x, "sframe_missing_data_report"))
  brand <- sframe_brand(palette)
  df <- x$item_missing
  df <- df[!is.na(df$missing_pct) & df$missing_pct > 0, , drop = FALSE]
  if (nrow(df) == 0) {
    # An empty bar chart with no bars reads as a rendering failure, not a
    # result, so a completely clean dataset gets its own reassuring chart
    # rather than a silent NULL.
    return(
      ggplot2::ggplot(data.frame(x = 0, y = 0, label = "No missing responses in any item")) +
        ggplot2::geom_text(ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
                           size = 4.2, colour = brand$ink) +
        ggplot2::labs(title = "Missing responses by item", x = NULL, y = NULL) +
        theme_surveyframe(palette = palette) +
        ggplot2::theme(
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          axis.line = ggplot2::element_blank()
        )
    )
  }
  bar_fill <- if (palette == "web") brand$teal else brand$fill
  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(.data$variable, -.data$missing_pct),
                                   y = .data$missing_pct)) +
    ggplot2::geom_col(fill = bar_fill, colour = brand$ink, linewidth = 0.3, width = 0.65) +
    ggplot2::scale_x_discrete(labels = .sframe_title_case_names) +
    ggplot2::scale_y_continuous(labels = scales_percent_fallback) +
    ggplot2::labs(title = "Missing responses by item", x = NULL, y = "Missing") +
    theme_surveyframe(palette = palette) + sframe_theme_angled_x()
}

#' @export
plot.sframe_missing_data_report <- function(x, ..., palette = c("web", "print")) {
  sframe_plot_missingness(x, palette = match.arg(palette))
}

#' Plot analysis-plan results
#'
#' Draws the charts that [run_analysis_plan()] attaches when called with
#' `plots = TRUE`. With `which` supplied, returns that single chart. With
#' `which` omitted, prints every attached chart in queue order and returns
#' the list invisibly. Regression diagnostic panels stay on the result's
#' `diagnostic_plots` element and are not drawn here.
#'
#' @param x An `sframe_analysis_results` object from [run_analysis_plan()].
#' @param ... Ignored.
#' @param which A research-question number or a plan block id selecting one
#'   chart, or NULL for all.
#' @return A ggplot2 object when `which` is supplied, otherwise an invisible
#'   named list of ggplot2 objects keyed by plan block id.
#' @export
plot.sframe_analysis_results <- function(x, ..., which = NULL) {
  plots <- list()
  for (i in seq_along(x)) {
    r <- x[[i]]
    if (is.null(r$plot)) next
    key <- r$block_id %||% ""
    if (!nzchar(key)) key <- paste0("rq_", i)
    plots[[key]] <- r$plot
  }
  if (length(plots) == 0) {
    rlang::abort(
      "No charts are attached to these results. Re-run run_analysis_plan() with plots = TRUE (requires ggplot2).",
      class = "sframe_error"
    )
  }
  if (!is.null(which)) {
    if (is.numeric(which)) {
      if (length(which) != 1 || which < 1 || which > length(x)) {
        rlang::abort("`which` must select one research question by number or block id.",
                     class = "sframe_error")
      }
      r <- x[[which]]
      if (is.null(r$plot)) {
        rlang::abort(sprintf("Research question %d has no chart attached.", which),
                     class = "sframe_error")
      }
      return(r$plot)
    }
    key <- as.character(which)[[1]]
    if (is.null(plots[[key]])) {
      rlang::abort(sprintf("No chart is attached for block id '%s'.", key),
                   class = "sframe_error")
    }
    return(plots[[key]])
  }
  for (p in plots) print(p)
  invisible(plots)
}

#' Item distribution chart, ggplot2 equivalent of the dashboard/studio panel
#'
#' Shared by `launch_dashboard()` (`inst/shiny/dashboard/app.R`) and the
#' SurveyStudio dashboard tab (`inst/shiny/app.R`), which otherwise
#' duplicated this base-graphics chart. Callers fall back to their own base
#' graphics when this returns `NULL` (ggplot2 not installed, unsupported
#' item type, or no data), so the dashboard keeps working without ggplot2.
#'
#' @param item A list with at least `type` and `label` (an sframe item).
#' @param col_data The response column for this item.
#' @param choice_set A list with `values` and `labels` (an sframe choice
#'   set), or `NULL` if the item has none.
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object, or `NULL` if this item type/data is unsupported.
#' @keywords internal
#' @export
sframe_plot_item_chart <- function(item, col_data, choice_set = NULL,
                                   palette = c("web", "print")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  palette <- match.arg(palette)
  brand <- sframe_brand(palette)
  t <- item$type %||% ""
  if (t %in% c("likert", "single_choice", "multiple_choice")) {
    if (!is.null(choice_set)) {
      freq <- table(factor(col_data, levels = as.character(choice_set$values)))
      names(freq) <- choice_set$labels
    } else {
      freq <- table(col_data)
    }
    if (!sum(freq)) return(NULL)
    df <- data.frame(label = names(freq), freq = as.numeric(freq))
    df$label <- factor(df$label, levels = df$label)
    ggplot2::ggplot(df, ggplot2::aes(x = .data$label, y = .data$freq)) +
      ggplot2::geom_col(fill = brand$fill, colour = brand$ink, linewidth = 0.3, width = 0.72) +
      ggplot2::labs(x = NULL, y = "Frequency") +
      theme_surveyframe(palette = palette) + sframe_theme_angled_x()
  } else if (t %in% c("numeric", "slider", "rating")) {
    num <- suppressWarnings(as.numeric(col_data)); num <- num[!is.na(num)]
    if (!length(num)) return(NULL)
    ggplot2::ggplot(data.frame(x = num), ggplot2::aes(x = .data$x)) +
      ggplot2::geom_histogram(fill = brand$fill, colour = brand$ink, linewidth = 0.3,
                              bins = min(30, max(5, length(unique(num))))) +
      ggplot2::labs(x = item$label %||% "", y = "Count") +
      theme_surveyframe(palette = palette)
  } else {
    NULL
  }
}

#' Scale score distribution chart, ggplot2 equivalent of the dashboard panel
#'
#' Same sharing rationale as [sframe_plot_item_chart()].
#'
#' @param scores Numeric vector of scale scores (already averaged/summed).
#' @param label Character. Scale label, used as the x-axis title.
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object, or `NULL` if ggplot2 is unavailable or `scores`
#'   is empty.
#' @keywords internal
#' @export
sframe_plot_scale_chart <- function(scores, label, palette = c("web", "print")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  palette <- match.arg(palette)
  scores <- scores[!is.na(scores)]
  if (!length(scores)) return(NULL)
  brand <- sframe_brand(palette)
  m <- mean(scores)
  ggplot2::ggplot(data.frame(x = scores), ggplot2::aes(x = .data$x)) +
    ggplot2::geom_histogram(fill = brand$fill, colour = brand$ink, linewidth = 0.3,
                            bins = min(20, max(5, length(unique(scores))))) +
    ggplot2::geom_vline(xintercept = m, colour = brand$accent, linewidth = 0.8,
                        linetype = "dashed") +
    ggplot2::labs(x = paste0(label, " score"), y = "Count",
                 subtitle = sprintf("M = %.2f", m)) +
    theme_surveyframe(palette = palette)
}

#' Distribution shape by variable, standardised
#'
#' One violin per variable in a [descriptives_report()] table, built from
#' the underlying response data rather than from the summary skewness and
#' kurtosis numbers, so the reader sees the actual shape (asymmetry,
#' multimodality, tails) instead of reading it off a bar height. Each
#' variable is standardised (z-scored) before plotting so variables on
#' different original scales (a 5-point Likert item next to a 0-100 slider)
#' share one comparable y-axis. Standardising is a linear transform and does
#' not change skewness. Each violin's subtitle-free panel keeps the
#' variable's skewness value in its axis label. Grouped `descriptives_report()`
#' output (one row per variable per `split_by` group) is faceted by group.
#'
#' @param x An `sframe_descriptives_report` object from [descriptives_report()].
#' @param data The same data.frame passed to [descriptives_report()]. Required:
#'   `x` only carries the summary table, not the raw values the violins need.
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object, or `NULL` if none of the report's variables have
#'   enough data to draw.
#' @export
#' @seealso [descriptives_report()]
sframe_plot_descriptives <- function(x, data, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot distribution shape by variable.")
  palette <- match.arg(palette)
  stopifnot(inherits(x, "sframe_descriptives_report"))
  tbl <- x$table
  if (!is.data.frame(tbl) || nrow(tbl) == 0) return(NULL)
  if (!is.data.frame(data)) {
    rlang::abort(
      "sframe_plot_descriptives() needs `data` (the data.frame passed to descriptives_report()) to draw the distribution shape.",
      class = "sframe_error"
    )
  }
  brand <- sframe_brand(palette)
  split_by <- x$split_by
  has_groups <- !is.null(split_by) && split_by %in% colnames(data)

  rows <- lapply(seq_len(nrow(tbl)), function(i) {
    var <- tbl$variable[i]
    if (!var %in% colnames(data)) return(NULL)
    vals <- suppressWarnings(as.numeric(data[[var]]))
    if (has_groups) {
      idx <- as.character(data[[split_by]]) == tbl$group[i]
      idx[is.na(idx)] <- FALSE
      vals <- vals[idx]
    }
    vals <- vals[!is.na(vals)]
    if (length(vals) < 2 || stats::sd(vals) == 0) return(NULL)
    data.frame(
      variable = var, group = tbl$group[i],
      value = as.numeric(scale(vals)),
      stringsAsFactors = FALSE
    )
  })
  long <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(long) || nrow(long) == 0) return(NULL)
  long$variable <- factor(long$variable, levels = unique(tbl$variable))

  # One skewness label per variable (per group, when grouped), placed above
  # its violin: a per-panel annotation reads unambiguously in a faceted plot,
  # unlike folding the number into a shared x-axis label.
  skew_tbl <- tbl[!is.na(tbl$skewness), c("variable", "group", "skewness"), drop = FALSE]
  skew_tbl$variable <- factor(skew_tbl$variable, levels = levels(long$variable))
  skew_tbl$label <- sprintf("skew %.2f", skew_tbl$skewness)
  ymax <- stats::aggregate(value ~ variable + group, long, max)
  skew_tbl <- merge(skew_tbl, ymax, by = c("variable", "group"), all.x = TRUE)

  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$variable, y = .data$value)) +
    ggplot2::geom_violin(fill = brand$fill, colour = brand$ink, linewidth = 0.3,
                         trim = TRUE) +
    ggplot2::geom_boxplot(width = 0.12, outlier.shape = NA, fill = "white",
                          colour = brand$ink, linewidth = 0.3) +
    ggplot2::geom_text(data = skew_tbl,
                       ggplot2::aes(x = .data$variable, y = .data$value, label = .data$label),
                       vjust = -0.6, size = 3, colour = brand$muted, inherit.aes = FALSE) +
    ggplot2::labs(title = "Distribution shape by variable (standardised)",
                 subtitle = "Skewness shown above each violin",
                 x = NULL, y = "Standardised value") +
    ggplot2::scale_x_discrete(labels = .sframe_title_case_names) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.15))) +
    theme_surveyframe(palette = palette) + sframe_theme_angled_x()
  if (has_groups && length(unique(long$group)) > 1) {
    p <- p + ggplot2::facet_wrap(~group)
  }
  p
}

#' @export
plot.sframe_descriptives_report <- function(x, data, ..., palette = c("web", "print")) {
  sframe_plot_descriptives(x, data = data, palette = match.arg(palette))
}

#' Group-comparison boxplot
#'
#' Boxplot with jittered points, shared across every runner whose result
#' carries `vars = c(group_column, outcome_column)`: `t_test_ind`,
#' `mann_whitney`, `kruskal_wallis`, and `anova_one`. One function instead of
#' four, since the underlying comparison (an outcome split by a grouping
#' factor) and the data shape needed to plot it are identical across all
#' four tests, and only the inferential statistic differs.
#'
#' @param result A result list from one of the four runners above, with
#'   `vars = c(group_column, outcome_column)`.
#' @param data The response data frame the result was computed from.
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object, or `NULL` if the columns are missing, fewer
#'   than two groups remain after removing missing values, or ggplot2 is
#'   unavailable.
#' @export
#' @seealso [run_analysis_plan()]
sframe_plot_group_comparison <- function(result, data, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot a group comparison.")
  palette <- match.arg(palette)
  vars <- result$vars
  if (length(vars) < 2 || !all(vars[1:2] %in% colnames(data))) return(NULL)
  group_col <- vars[1]; outcome_col <- vars[2]
  df <- data.frame(
    group   = as.character(data[[group_col]]),
    outcome = suppressWarnings(as.numeric(data[[outcome_col]]))
  )
  df <- df[!is.na(df$group) & !is.na(df$outcome), , drop = FALSE]
  if (nrow(df) < 2 || length(unique(df$group)) < 2) return(NULL)
  brand <- sframe_brand(palette)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$group, y = .data$outcome,
                                   fill = .data$group)) +
    ggplot2::geom_boxplot(outlier.shape = NA, width = 0.55, alpha = 0.85,
                          colour = brand$ink, linewidth = 0.35) +
    # Seeded so 2 renders of the same report place the points identically.
    # geom_jitter() otherwise draws offsets at plot time, after
    # run_analysis_plan() has restored the caller's RNG stream.
    ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.08,
                                                            height = 0,
                                                            seed = 20260828L),
                        alpha = 0.6, size = 1.6, colour = brand$ink) +
    ggplot2::scale_fill_manual(values = sframe_series_fill_colours(length(unique(df$group)), palette),
                               guide = "none") +
    ggplot2::labs(title = sprintf("%s by %s", .sframe_title_case_names(outcome_col),
                                  .sframe_title_case_names(group_col)),
                 subtitle = result$apa %||% NULL,
                 x = .sframe_title_case_names(group_col),
                 y = .sframe_title_case_names(outcome_col)) +
    theme_surveyframe(palette = palette)
}

#' Paired-comparison slope plot
#'
#' One line per respondent connecting their two paired values, shared by
#' `t_test_pair` and `wilcoxon_pair` (both carry `vars = c(x_column,
#' y_column)` on the same respondents). The standard visual for a paired
#' design: it shows the direction and consistency of individual change,
#' which a plain bar-of-means would hide.
#'
#' @param result A result list from `t_test_pair`/`wilcoxon_pair`, with
#'   `vars = c(x_column, y_column)`.
#' @param data The response data frame the result was computed from.
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object, or `NULL` if fewer than two complete pairs
#'   remain, or ggplot2 is unavailable.
#' @export
#' @seealso [run_analysis_plan()]
sframe_plot_paired_comparison <- function(result, data, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot a paired comparison.")
  palette <- match.arg(palette)
  vars <- result$vars
  if (length(vars) < 2 || !all(vars[1:2] %in% colnames(data))) return(NULL)
  x <- suppressWarnings(as.numeric(data[[vars[1]]]))
  y <- suppressWarnings(as.numeric(data[[vars[2]]]))
  complete <- !is.na(x) & !is.na(y)
  x <- x[complete]; y <- y[complete]
  if (length(x) < 2) return(NULL)
  brand <- sframe_brand(palette)
  labels <- .sframe_title_case_names(vars[1:2])
  long <- data.frame(
    id        = rep(seq_along(x), 2),
    condition = factor(rep(labels, each = length(x)), levels = labels),
    value     = c(x, y)
  )
  ggplot2::ggplot(long, ggplot2::aes(x = .data$condition, y = .data$value,
                                     group = .data$id)) +
    ggplot2::geom_line(colour = brand$muted, alpha = 0.55) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$condition), size = 2) +
    ggplot2::scale_colour_manual(values = sframe_series_colours(2, palette), guide = "none") +
    ggplot2::labs(title = sprintf("%s vs %s (paired)", labels[1], labels[2]),
                 subtitle = result$apa %||% NULL, x = NULL, y = "Value") +
    theme_surveyframe(palette = palette)
}

# Mean (+/- SE) or median profile across 3+ repeated conditions, shared by
# repeated_anova and friedman: their vars are the same shape (2+ measure
# columns on the same respondents) and both are asking "does the rating
# move across conditions", which a line-and-point profile answers directly.
# Friedman is a rank test, so it uses the median rather than the mean.
sframe_plot_repeated_measures <- function(result, data, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot ratings across repeated conditions.")
  palette <- match.arg(palette)
  vars <- result$vars
  if (length(vars) < 2 || !all(vars %in% colnames(data))) return(NULL)
  use_median <- identical(result$test, "friedman")
  rows <- lapply(vars, function(v) {
    x <- suppressWarnings(as.numeric(data[[v]]))
    x <- x[!is.na(x)]
    if (!length(x)) return(NULL)
    if (use_median) {
      data.frame(condition = v, value = stats::median(x), ymin = NA_real_, ymax = NA_real_)
    } else {
      se <- if (length(x) > 1) stats::sd(x) / sqrt(length(x)) else NA_real_
      m <- mean(x)
      data.frame(condition = v, value = m, ymin = m - se, ymax = m + se)
    }
  })
  df <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(df) || nrow(df) < 2) return(NULL)
  df$condition <- factor(df$condition, levels = vars)
  brand <- sframe_brand(palette)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$condition, y = .data$value, group = 1)) +
    ggplot2::geom_line(colour = brand$teal, linewidth = 0.6) +
    ggplot2::geom_point(colour = brand$teal, size = 2.6)
  if (!use_median) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$ymin, ymax = .data$ymax),
      width = 0.12, colour = brand$ink
    )
  }
  p +
    ggplot2::scale_x_discrete(labels = .sframe_title_case_names) +
    ggplot2::labs(
      title = "Ratings across conditions", subtitle = result$apa %||% NULL,
      x = NULL, y = if (use_median) "Median" else "Mean (\u00B1 SE)"
    ) +
    theme_surveyframe(palette = palette)
}

# Scatter of the residualised x/y values a partial correlation actually
# tests (x and y each with the control variable(s)' effect removed), rather
# than the raw, unadjusted pair. Recomputed from `data` rather than stored
# on the result, matching every other data-driven plot in this file.
sframe_plot_partial_correlation <- function(result, data, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot a partial correlation.")
  palette <- match.arg(palette)
  vars <- result$vars
  if (length(vars) < 2 || !all(vars %in% colnames(data))) return(NULL)
  x_col <- vars[1]; y_col <- vars[2]
  controls <- result$controls %||% character(0)
  df <- as.data.frame(lapply(data[, vars, drop = FALSE], function(v) suppressWarnings(as.numeric(v))))
  df <- df[stats::complete.cases(df), , drop = FALSE]
  if (nrow(df) < 4) return(NULL)
  if (length(controls) > 0) {
    rx <- stats::residuals(stats::lm(
      stats::as.formula(paste(x_col, "~", paste(controls, collapse = " + "))), data = df))
    ry <- stats::residuals(stats::lm(
      stats::as.formula(paste(y_col, "~", paste(controls, collapse = " + "))), data = df))
  } else {
    rx <- df[[x_col]]; ry <- df[[y_col]]
  }
  brand <- sframe_brand(palette)
  ggplot2::ggplot(data.frame(x = rx, y = ry), ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point(colour = brand$teal, alpha = 0.65, size = 2) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, colour = brand$ink,
                         fill = brand$grid, linewidth = 0.8) +
    ggplot2::labs(
      title = sprintf("Partial relationship between %s and %s",
                      .sframe_title_case_names(x_col), .sframe_title_case_names(y_col)),
      subtitle = result$apa %||% NULL,
      x = sprintf("%s (residualised)", .sframe_title_case_names(x_col)),
      y = sprintf("%s (residualised)", .sframe_title_case_names(y_col))
    ) +
    theme_surveyframe(palette = palette)
}

# Odds-ratio forest plot shared by regression_logistic_binary and
# regression_logistic_ordinal: both carry a $coefficients data.frame with
# odds_ratio/or_ci_low/or_ci_high columns (see sframe_logistic_fit_table()
# and sframe_run_ordinal_logistic()). The intercept and, for the ordinal
# model, the "1|2"-style threshold terms are cut points rather than
# predictor effects, so both are left off the effect plot.
sframe_plot_logistic_coefficients <- function(result, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot odds ratios.")
  palette <- match.arg(palette)
  co <- result$coefficients
  if (!is.data.frame(co) || !all(c("odds_ratio", "or_ci_low", "or_ci_high") %in% colnames(co))) {
    return(NULL)
  }
  terms <- rownames(co)
  keep <- terms != "(Intercept)" & !grepl("\\|", terms)
  if (!any(keep)) return(NULL)
  df <- data.frame(
    term = terms[keep], or = co$odds_ratio[keep],
    lo = co$or_ci_low[keep], hi = co$or_ci_high[keep],
    stringsAsFactors = FALSE
  )
  df$term <- factor(df$term, levels = rev(df$term))
  brand <- sframe_brand(palette)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$or, y = .data$term)) +
    ggplot2::geom_vline(xintercept = 1, colour = brand$muted, linetype = "dashed") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$lo, xmax = .data$hi), height = 0.18, colour = brand$ink) +
    ggplot2::geom_point(colour = brand$teal, size = 2.6) +
    ggplot2::scale_y_discrete(labels = .sframe_title_case_names) +
    ggplot2::labs(
      title = "Odds ratios with 95% confidence intervals",
      subtitle = result$apa %||% NULL, x = "Odds ratio", y = NULL
    ) +
    theme_surveyframe(palette = palette)
}

# Simple-slopes interaction plot: predicted outcome across the predictor's
# range at 3 moderator levels (-1 SD, mean, +1 SD), the standard way to
# show a moderated relationship. Refits the interaction model from `data`
# rather than reusing result$coefficients directly, since predict() needs
# the fitted model object, not just its coefficient table.
sframe_plot_moderation <- function(result, data, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot a moderation interaction.")
  palette <- match.arg(palette)
  vars <- result$vars
  if (length(vars) < 3 || !all(vars %in% colnames(data))) return(NULL)
  outcome <- vars[1]; predictor <- vars[2]; moderator <- vars[3]
  df <- as.data.frame(lapply(data[, vars, drop = FALSE], function(v) suppressWarnings(as.numeric(v))))
  df <- df[stats::complete.cases(df), , drop = FALSE]
  if (nrow(df) < 4) return(NULL)
  fit <- tryCatch(
    stats::lm(stats::as.formula(paste(outcome, "~", predictor, "*", moderator)), data = df),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  mod_mean <- mean(df[[moderator]]); mod_sd <- stats::sd(df[[moderator]])
  levels_df <- data.frame(
    level = factor(c("-1 SD", "Mean", "+1 SD"), levels = c("-1 SD", "Mean", "+1 SD")),
    value = c(mod_mean - mod_sd, mod_mean, mod_mean + mod_sd)
  )
  pred_range <- range(df[[predictor]], na.rm = TRUE)
  pred_seq <- seq(pred_range[1], pred_range[2], length.out = 25)
  # predict(..., interval = "confidence") gives the ribbon band for free, no
  # separate se.fit arithmetic needed.
  grid <- do.call(rbind, lapply(seq_len(nrow(levels_df)), function(i) {
    nd <- data.frame(v1 = pred_seq)
    names(nd) <- predictor
    nd[[moderator]] <- levels_df$value[i]
    pr <- as.data.frame(stats::predict(fit, newdata = nd, interval = "confidence"))
    nd$fitted <- pr$fit
    nd$lo <- pr$lwr
    nd$hi <- pr$upr
    nd$level <- levels_df$level[i]
    nd
  }))

  # Simple-slope significance at each moderator level (the delta-method SE
  # of b1 + b12 * level), the same conditional-effects test processR-style
  # interaction plots annotate alongside the lines.
  b <- stats::coef(fit)
  vc <- stats::vcov(fit)
  b12_name <- paste0(predictor, ":", moderator)
  b1  <- b[[predictor]]
  b12 <- if (b12_name %in% names(b)) b[[b12_name]] else 0
  var1  <- vc[predictor, predictor]
  var12 <- if (b12_name %in% rownames(vc)) vc[b12_name, b12_name] else 0
  cov112 <- if (b12_name %in% rownames(vc)) vc[predictor, b12_name] else 0
  slopes <- vapply(levels_df$value, function(lv) b1 + b12 * lv, numeric(1))
  slope_se <- vapply(levels_df$value, function(lv) {
    sqrt(var1 + (lv^2) * var12 + 2 * lv * cov112)
  }, numeric(1))
  slope_t <- slopes / slope_se
  slope_p <- 2 * stats::pt(-abs(slope_t), fit$df.residual)
  caption <- paste(
    sprintf("%s slope: b = %.2f, p %s", levels_df$level, slopes,
            vapply(slope_p, sframe_p_string, character(1))),
    collapse = "   |   "
  )

  brand <- sframe_brand(palette)
  series <- sframe_series_colours(3, palette)
  ggplot2::ggplot(grid, ggplot2::aes(x = .data[[predictor]], y = .data$fitted, colour = .data$level)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo, ymax = .data$hi, fill = .data$level),
                         alpha = 0.15, colour = NA) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::scale_colour_manual(values = series) +
    ggplot2::scale_fill_manual(values = series) +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(
      title = sprintf("%s moderates the effect of %s on %s",
                      .sframe_title_case_names(moderator), .sframe_title_case_names(predictor),
                      .sframe_title_case_names(outcome)),
      subtitle = result$apa %||% NULL,
      caption = caption,
      x = .sframe_title_case_names(predictor), y = .sframe_title_case_names(outcome),
      colour = .sframe_title_case_names(moderator)
    ) +
    theme_surveyframe(palette = palette)
}

# Direct, indirect, and total effect sizes as one bar chart, with the
# bootstrap CI shown on the indirect effect (the only one mediation
# analysis usually bootstraps).
sframe_plot_mediation <- function(result, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot mediation effects.")
  palette <- match.arg(palette)
  if (is.null(result$direct) || is.null(result$indirect) || is.null(result$total)) return(NULL)
  ci <- result$indirect_ci
  df <- data.frame(
    effect = factor(c("Direct (c\u2032)", "Indirect (a\u00D7b)", "Total (c)"),
                    levels = c("Total (c)", "Indirect (a\u00D7b)", "Direct (c\u2032)")),
    estimate = c(result$direct, result$indirect, result$total),
    lo = c(NA_real_, ci[[1]], NA_real_),
    hi = c(NA_real_, ci[[2]], NA_real_)
  )
  brand <- sframe_brand(palette)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$estimate, y = .data$effect)) +
    ggplot2::geom_vline(xintercept = 0, colour = brand$muted, linetype = "dashed") +
    ggplot2::geom_col(fill = brand$fill, colour = brand$ink, width = 0.55, linewidth = 0.3) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$lo, xmax = .data$hi), height = 0.15,
                            colour = brand$ink, na.rm = TRUE) +
    ggplot2::labs(
      title = "Direct, indirect, and total effects", subtitle = result$apa %||% NULL,
      x = "Estimate", y = NULL
    ) +
    theme_surveyframe(palette = palette)
}

# Item-rest correlation by scale, from item_report()'s per-scale
# diagnostics: the standard way to spot a weak or miskeyed item (commonly
# flagged below 0.30) at a glance, across every scale at once.
sframe_plot_item_diagnostics <- function(result, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot item-rest correlations.")
  palette <- match.arg(palette)
  ir <- result$report_obj
  if (is.null(ir)) return(NULL)
  tbl <- sframe_item_diagnostics_table(ir)
  if (is.null(tbl) || !"item_rest_r" %in% colnames(tbl)) return(NULL)
  tbl$item_id <- factor(tbl$item_id, levels = rev(tbl$item_id))
  brand <- sframe_brand(palette)
  ggplot2::ggplot(tbl, ggplot2::aes(x = .data$item_rest_r, y = .data$item_id, fill = .data$Scale)) +
    ggplot2::geom_vline(xintercept = 0.3, colour = brand$muted, linetype = "dashed") +
    ggplot2::geom_col(colour = brand$ink, linewidth = 0.3, width = 0.6) +
    ggplot2::scale_fill_manual(values = sframe_series_fill_colours(length(unique(tbl$Scale)), palette)) +
    ggplot2::scale_y_discrete(labels = .sframe_title_case_names) +
    ggplot2::labs(
      title = "Item-rest correlation by scale",
      subtitle = "Dashed line: commonly used 0.30 acceptability guideline",
      x = "Item-rest correlation", y = NULL, fill = "Scale"
    ) +
    theme_surveyframe(palette = palette)
}

#' Raw-variable distribution panels: histogram, boxplot, and Q-Q
#'
#' Unlike [sframe_plot_descriptives()], which summarises skewness and
#' kurtosis *across* the variables in a [descriptives_report()] table, this
#' operates on one variable's raw values directly (the report table only
#' stores summary statistics, not the underlying vector), matching the
#' pattern [sframe_plot_correlation_matrix()] already uses for
#' report-independent, data-driven plots.
#'
#' @param data A data frame of survey responses.
#' @param variable Character. Column name of the variable to plot.
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A named list of three ggplot2 objects (`histogram`, `boxplot`,
#'   `qq`), or `NULL` if fewer than two complete values remain.
#' @export
#' @seealso [descriptives_report()], [sframe_plot_descriptives()]
sframe_plot_variable_distribution <- function(data, variable, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot a variable's distribution.")
  palette <- match.arg(palette)
  if (!variable %in% colnames(data)) return(NULL)
  x <- suppressWarnings(as.numeric(data[[variable]]))
  x <- x[!is.na(x)]
  if (length(x) < 2) return(NULL)
  brand <- sframe_brand(palette)
  var_label <- .sframe_title_case_names(variable)

  histogram <- ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x = .data$x)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            fill = brand$fill, colour = brand$ink, linewidth = 0.3,
                            bins = min(30, max(5, length(unique(x))))) +
    ggplot2::geom_density(colour = brand$ink, linewidth = 0.7) +
    ggplot2::labs(title = paste("Distribution of", var_label), x = var_label, y = "Density") +
    theme_surveyframe(palette = palette)

  boxplot <- ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x = "", y = .data$x)) +
    ggplot2::geom_boxplot(fill = brand$fill, colour = brand$ink, linewidth = 0.35,
                          width = 0.35, alpha = 0.85, outlier.colour = brand$accent) +
    ggplot2::labs(title = paste("Boxplot of", var_label), x = NULL, y = var_label) +
    theme_surveyframe(palette = palette)

  qq_theoretical <- stats::qqnorm(x, plot.it = FALSE)
  qq <- ggplot2::ggplot(
      data.frame(theoretical = qq_theoretical$x, sample = qq_theoretical$y),
      ggplot2::aes(x = .data$theoretical, y = .data$sample)) +
    ggplot2::geom_abline(colour = brand$muted, linetype = "dashed") +
    ggplot2::geom_point(colour = brand$teal, alpha = 0.75, size = 2) +
    ggplot2::labs(title = paste("Normal Q-Q of", var_label),
                 x = "Theoretical quantiles", y = "Sample quantiles") +
    theme_surveyframe(palette = palette)

  list(histogram = histogram, boxplot = boxplot, qq = qq)
}

#' Topic-model top-terms plot: faceted bars, one facet per topic
#'
#' Serves both [sframe_run_topic_model_lda()] and [sframe_run_stm_topics()]
#' results with no dispatch on `result$test`: both runners emit a `$table`
#' with the same `topic`/`term`/`beta` columns (LDA's beta from
#' `tidytext::tidy()`, STM's from its fitted word-topic distribution), so
#' this function reads that shared shape directly.
#'
#' @param result A `topic_model_lda` or `stm_topics` result list from
#'   [run_analysis_plan()].
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object, or `NULL` when the result carries no usable
#'   table.
#' @export
#' @seealso [sframe_run_topic_model_lda()], [sframe_run_stm_topics()]
sframe_plot_topics <- function(result, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot topic terms.")
  palette <- match.arg(palette)
  tbl <- result$table
  if (!is.data.frame(tbl) || nrow(tbl) == 0 ||
      !all(c("topic", "term", "beta") %in% names(tbl))) {
    return(NULL)
  }
  brand <- sframe_brand(palette)

  plot_tbl <- do.call(rbind, lapply(split(tbl, tbl$topic), function(d) {
    utils::head(d[order(-d$beta), , drop = FALSE], 10)
  }))
  plot_tbl$topic <- factor(paste("Topic", plot_tbl$topic),
                            levels = paste("Topic", sort(unique(plot_tbl$topic))))
  # Same fix as .sframe_plot_term_bar()'s: a single global factor level
  # per term is wrong once the same word appears in more than one topic
  # (a common case) at a different beta, since a shared factor only has
  # one position for it, so a topic facet's own bars would not actually
  # sort by that facet's own beta values. Carry the topic in the factor
  # level itself, sort per topic, then strip the topic suffix back off
  # only for the printed label (the same pattern
  # `tidytext::reorder_within()` automates, done by hand here since
  # tidytext is Suggests-only and this chart also serves the base-R LDA
  # path). Ascending within each topic, largest last: coord_flip() draws
  # the last level at the top.
  plot_tbl$term_facet <- paste(plot_tbl$term, plot_tbl$topic, sep = "\r")
  plot_tbl <- plot_tbl[order(plot_tbl$topic, plot_tbl$beta), ]
  plot_tbl$term_facet <- factor(plot_tbl$term_facet, levels = unique(plot_tbl$term_facet))

  p <- ggplot2::ggplot(plot_tbl, ggplot2::aes(x = .data$term_facet, y = .data$beta)) +
    ggplot2::geom_col(fill = brand$fill, colour = brand$ink, linewidth = 0.3, width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(labels = function(x) sub("\r.*$", "", x)) +
    ggplot2::facet_wrap(~ topic, scales = "free_y") +
    ggplot2::labs(title = paste("Top terms per topic for", result$variable %||% ""),
                  x = NULL, y = "Term probability") +
    theme_surveyframe(palette = palette)
  p
}


# Fixed 8-slot categorical palette for cluster identity in the co-occurrence
# network plot, plus a 9th "Other" grey bucket for a 9th-or-later cluster.
# Deliberately NOT sframe_series_colours()/sframe_series_fill_colours():
# those interpolate a colour ramp past their fixed 5-colour set, which is
# exactly the "hue-cycling" the dataviz skill says not to do for a
# categorical channel. Per the dataviz skill's colour-formula guidance,
# more series than the fixed hue count should fold into an explicit "Other"
# bucket rather than generate a new, unvalidated hue.
#
# The `web` 8 hues are the dataviz skill's own documented default
# categorical palette (references/palette.md, light-mode column), already
# validated there: all 8 pass the *adjacent*-pair CVD/contrast gates used
# for bar/stack/line charts. This network plot draws points, an *all-pairs*
# form (any two nodes can sit side by side), where the same reference
# documents that no ordering of the full eight clears the all-pairs floor
# past the first three slots; a true all-pairs-safe cap would be 3, not 8.
# The brief for this method id fixes the boundary at 8 explicitly, so that
# is what is implemented and tested here; the shortfall past slot 3 is
# mitigated, not eliminated, by three secondary encodings already in the
# plot (point size = term frequency, edges = topology, and the legend/table
# = text) rather than colour alone carrying cluster identity. Flagged for
# the lead rather than silently narrowed to 3.
.sframe_cluster_palette <- function(n_clusters, palette = c("web", "print")) {
  palette <- match.arg(palette)
  hues <- if (palette == "web") {
    c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
      "#e87ba4", "#008300", "#4a3aa7", "#e34948")
  } else {
    # Print: an 8-step black-to-light grey ramp, consistent with
    # sframe_brand("print")'s existing achromatic convention for series.
    c("#141414", "#333333", "#4d4d4d", "#666666",
      "#808080", "#999999", "#b3b3b3", "#cccccc")
  }
  other <- if (palette == "web") "#8a8a86" else "#a6a6a6"
  n_clusters <- max(1L, as.integer(n_clusters))
  if (n_clusters <= length(hues)) {
    out <- hues[seq_len(n_clusters)]
    names(out) <- as.character(seq_len(n_clusters))
    return(out)
  }
  out <- c(hues, other)
  names(out) <- c(as.character(seq_along(hues)), "Other")
  out
}

#' Term co-occurrence network plot
#'
#' Plots a `co_occurrence_network` result's node table (`term`, `frequency`,
#' `cluster`, `x`, `y`) as a network diagram: edges (from `result$edges`) as
#' line segments underneath, nodes as points sized by term frequency and
#' coloured by Louvain cluster, with term labels on the larger points only.
#' Labelling every point on a dense network risks overlap chaos, so only
#' the top 15 nodes by frequency are labelled; the full term list stays
#' available in `result$table`.
#'
#' Clusters beyond the first 8 (ranked largest first) are folded into a
#' single "Other" bucket rather than cycling or interpolating a new hue,
#' per the dataviz skill's categorical-colour guidance; see
#' `.sframe_cluster_palette()`.
#'
#' @param result A `co_occurrence_network` result list from
#'   [run_analysis_plan()], carrying `table` and `edges`.
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object, or `NULL` when the result carries no table.
#' @export
#' @seealso [run_analysis_plan()]
sframe_plot_cooccurrence_network <- function(result, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot the co-occurrence network.")
  palette <- match.arg(palette)
  tbl <- result$table
  if (!is.data.frame(tbl) || nrow(tbl) == 0 || !all(c("term", "x", "y", "cluster") %in% names(tbl))) {
    return(NULL)
  }
  brand <- sframe_brand(palette)
  edges <- result$edges
  has_edges <- is.data.frame(edges) && nrow(edges) > 0 &&
    all(c("term_a", "term_b") %in% names(edges))

  # Rank clusters largest-first (by member count) and fold anything past
  # rank 8 into "Other", so the palette above is never asked for more than
  # 8 real hues.
  sizes <- sort(table(tbl$cluster), decreasing = TRUE)
  rank_of <- stats::setNames(seq_along(sizes), names(sizes))
  n_clusters <- length(sizes)
  tbl$cluster_rank <- as.integer(rank_of[as.character(tbl$cluster)])
  tbl$cluster_label <- if (n_clusters > 8) {
    ifelse(tbl$cluster_rank <= 8, as.character(tbl$cluster_rank), "Other")
  } else {
    as.character(tbl$cluster_rank)
  }
  pal <- .sframe_cluster_palette(n_clusters, palette)
  level_order <- if (n_clusters > 8) c(as.character(1:8), "Other") else as.character(seq_len(n_clusters))
  level_order <- level_order[level_order %in% unique(tbl$cluster_label)]
  tbl$cluster_label <- factor(tbl$cluster_label, levels = level_order)

  p <- ggplot2::ggplot()

  if (has_edges) {
    edge_xy <- merge(edges, tbl[c("term", "x", "y")], by.x = "term_a", by.y = "term")
    edge_xy <- merge(edge_xy, tbl[c("term", "x", "y")], by.x = "term_b", by.y = "term",
                      suffixes = c("_a", "_b"))
    p <- p + ggplot2::geom_segment(
      data = edge_xy,
      ggplot2::aes(x = .data$x_a, y = .data$y_a, xend = .data$x_b, yend = .data$y_b),
      colour = brand$grid, linewidth = 0.4, alpha = 0.7
    )
  }

  p <- p +
    ggplot2::geom_point(
      data = tbl,
      ggplot2::aes(x = .data$x, y = .data$y, size = .data$frequency,
                   colour = .data$cluster_label)
    ) +
    ggplot2::scale_size(range = c(2, 10), guide = "none") +
    ggplot2::scale_colour_manual(values = pal, name = "Cluster", drop = TRUE)

  # Label only the top 15 nodes by frequency: a legible plot without labels
  # on every node beats an unreadable one with them, and the full term list
  # is already in result$table for anyone who wants it.
  label_tbl <- utils::head(tbl[order(-tbl$frequency), , drop = FALSE], 15)
  p <- p + ggplot2::geom_text(
    data = label_tbl,
    ggplot2::aes(x = .data$x, y = .data$y, label = .data$term),
    colour = brand$ink, size = 3, vjust = -1, fontface = "bold"
  )

  p +
    ggplot2::labs(title = paste("Term co-occurrence network for", result$variable %||% ""),
                  x = NULL, y = NULL) +
    theme_surveyframe(palette = palette) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}


# Comparison-cloud layout: negative words spiral outward from an anchor
# above the origin, positive words from an anchor below it, using the
# SAME spiral-and-collide engine sframe_plot_term_frequency()'s word
# cloud does (.sframe_wordcloud_layout()'s `centers` argument exists for
# exactly this), so the two groups never overlap each other or
# themselves. This is the base-R/ggplot2 equivalent of the classic
# tidytext comparison cloud (bookdown.org/jdholster1/idsr/text-analysis.html
# section 8.4: count(word, sentiment) %>% acast() %>% comparison.cloud()),
# without adding the wordcloud/reshape2 dependency that reference code
# uses — consistent with todo_text_analysis.md's "no wordcloud/ggwordcloud package"
# rule for the plain term-frequency cloud.
.sframe_sentiment_cloud_layout <- function(word_sentiment, max_per_side = 25) {
  neg <- utils::head(word_sentiment[word_sentiment$sentiment == "negative", , drop = FALSE], max_per_side)
  pos <- utils::head(word_sentiment[word_sentiment$sentiment == "positive", , drop = FALSE], max_per_side)
  both <- rbind(neg, pos)
  if (nrow(both) == 0) return(NULL)
  # Sized off the COMBINED range, not per group, so a count of 20 looks
  # the same size whichever side it lands on.
  rng <- range(both$n)
  size01 <- if (diff(rng) == 0) rep(0.5, nrow(both)) else (sqrt(both$n) - sqrt(rng[1])) / (sqrt(rng[2]) - sqrt(rng[1]))
  both$size <- 5 + size01 * 13
  # Negative left, positive right: reads the same direction as the
  # diverging bar chart (negative extends left of zero, positive right).
  # aspect < 1 grows each cluster taller/narrower than a circular or
  # wide-elliptical spiral would, which is what keeps the 2 clusters from
  # spreading into each other horizontally between the anchors.
  anchor_offset <- 1.4
  centers <- data.frame(x = ifelse(both$sentiment == "negative", -anchor_offset, anchor_offset), y = 0)
  layout <- .sframe_wordcloud_layout(both$word, both$size, centers = centers, aspect = 0.65)
  merge(both, layout, by.x = "word", by.y = "term")
}

#' Sentiment plot: diverging bar, or a positive/negative comparison cloud
#'
#' A ggplot2 diverging bar for a `tidy_sentiment` result by default:
#' positive counts extend one direction, negative counts the other, so bar
#' position (not colour alone) carries the primary polarity signal, the
#' same convention [sframe_draw_likert_diverging()] uses for Likert
#' agreement (dark ramp toward the pole) rebuilt here in ggplot2 rather
#' than called directly, since that helper is base-graphics and
#' Likert-scale-specific. Facets by group when `result$table` carries a
#' `group` column, mirroring [sframe_plot_term_frequency()]'s grouped
#' branch.
#'
#' When `result$options$wordcloud` is `TRUE` (opt-in, default `FALSE`,
#' matching [sframe_plot_term_frequency()]'s own word-cloud toggle),
#' draws a comparison cloud instead: negative-sentiment words above the
#' centre line, positive-sentiment words below it, each word sized by how
#' often it occurred, using the internal `tidy_sentiment` runner's
#' `$word_sentiment` word-by-sentiment counts. Answers a different
#' question from the diverging bar: not "how many responses leaned
#' positive," but "which *words* drove that."
#'
#' @param result A `tidy_sentiment` result list from [run_analysis_plan()].
#' @param palette One of `"web"` or `"print"`. See `sframe_brand()`.
#' @return A ggplot2 object, or `NULL` when the result carries no table.
#' @export
#' @seealso [run_analysis_plan()], [sframe_draw_likert_diverging()]
sframe_plot_sentiment <- function(result, palette = c("web", "print")) {
  rlang::check_installed("ggplot2", reason = "to plot sentiment.")
  palette <- match.arg(palette)
  tbl <- result$table
  if (!is.data.frame(tbl) || nrow(tbl) == 0 || !"sentiment" %in% names(tbl)) return(NULL)
  brand <- sframe_brand(palette)
  grouped <- "group" %in% names(tbl)

  if (isTRUE(result$options$wordcloud)) {
    ws <- result$word_sentiment
    if (!is.data.frame(ws) || nrow(ws) == 0) return(NULL)
    cloud_tbl <- .sframe_sentiment_cloud_layout(ws)
    if (is.null(cloud_tbl)) return(NULL)
    fill_map <- stats::setNames(c(brand$accent, brand$teal), c("negative", "positive"))
    # Tight coordinate limits from the actual placed extents (each word's
    # anchor point +/- its own measured half-width/half-height), the same
    # fix sframe_plot_term_frequency()'s single cloud needed: without it,
    # a long word at the outer edge (e.g. "comfortable") clips mid-word
    # rather than sitting fully inside the panel.
    x_data_range <- range(c(cloud_tbl$x - cloud_tbl$half_w, cloud_tbl$x + cloud_tbl$half_w))
    ylim <- range(c(cloud_tbl$y - cloud_tbl$half_h, cloud_tbl$y + cloud_tbl$half_h))
    # The 2 group labels sit past the outermost word on each side, at a
    # fixed offset from the data's own extent, the same "anchor beyond the
    # content" placement the tidytext/wordcloud comparison.cloud() example
    # uses its own boxed labels for.
    label_pad <- diff(x_data_range) * 0.08
    xlim <- x_data_range + c(-1, 1) * label_pad
    # Negative words are anchored at -offset (left), positive at +offset
    # (right) -- the same .sframe_sentiment_cloud_layout() call above --
    # matching the diverging bar's own left-negative/right-positive
    # convention, so the left label is always "negative" and the right
    # always "positive" regardless of the data's actual extent.
    labels_df <- data.frame(
      x = xlim, y = 0, label = c("negative", "positive"),
      stringsAsFactors = FALSE
    )
    return(
      ggplot2::ggplot(cloud_tbl, ggplot2::aes(x = .data$x, y = .data$y,
                                              label = .data$word, size = .data$size,
                                              colour = .data$sentiment, alpha = .data$n)) +
        # Colour is the categorical negative/positive hue; ALPHA is what
        # varies dark-to-light with frequency within each side (the same
        # mechanism the term cloud uses), floored at 0.5 so even the
        # least-frequent word on a side stays legible rather than fading
        # toward invisible.
        ggplot2::geom_text(fontface = "bold") +
        ggplot2::geom_label(data = labels_df,
                            ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
                            inherit.aes = FALSE,
                            fill = brand$grid, colour = brand$ink, fontface = "bold",
                            label.size = 0, size = 3.6) +
        ggplot2::scale_size_identity() +
        ggplot2::scale_colour_manual(values = fill_map, guide = "none") +
        ggplot2::scale_alpha_continuous(range = c(0.5, 1), guide = "none") +
        ggplot2::coord_fixed(xlim = xlim, ylim = ylim, expand = TRUE) +
        ggplot2::labs(title = paste("Sentiment terms for", result$variable %||% "")) +
        ggplot2::theme_void() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
    )
  }

  bar_tbl <- tbl[tbl$sentiment %in% c("positive", "negative"), , drop = FALSE]
  bar_tbl <- bar_tbl[!is.na(bar_tbl$n), , drop = FALSE]
  if (nrow(bar_tbl) == 0) return(NULL)
  # Diverging signed count: negative sentiment plotted on the negative side
  # of zero, positive sentiment on the positive side, so the bar's position
  # relative to the zero line is the primary signal (matching the Likert
  # diverging convention), with the dark/light pole colouring as a
  # secondary cue.
  bar_tbl$signed_n <- ifelse(bar_tbl$sentiment == "negative", -bar_tbl$n, bar_tbl$n)
  bar_tbl$sentiment <- factor(bar_tbl$sentiment, levels = c("negative", "positive"))
  fill_map <- stats::setNames(c(brand$accent, brand$teal), c("negative", "positive"))

  p <- ggplot2::ggplot(bar_tbl, ggplot2::aes(x = if (grouped) .data$group else "", y = .data$signed_n,
                                             fill = .data$sentiment)) +
    ggplot2::geom_col(colour = brand$ink, linewidth = 0.3, width = 0.6) +
    ggplot2::geom_hline(yintercept = 0, colour = brand$ink, linewidth = 0.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = fill_map, name = NULL) +
    ggplot2::labs(
      title = paste("Sentiment for", result$variable %||% ""),
      x = NULL, y = "Response count (negative | positive)"
    ) +
    theme_surveyframe(palette = palette)
  if (grouped) p <- p + ggplot2::facet_wrap(~ group, scales = "free_y")
  p
}

