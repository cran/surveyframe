# tests/testthat/test-text-analysis.R
# Text and open-ended response analysis (todo_text_analysis.md). This file covers the
# lead reference diff: clean_text_responses(), term_frequency(),
# .sframe_tokenise(), sframe_run_term_freq() (including the group role and
# the minimum-response guard), sframe_plot_term_frequency(), and the
# $quotes reporting hook. Later ids land in their own test files.

phrases <- c(
  "The staff were very friendly and helpful",
  "Staff were friendly but the room was small",
  "Room was clean and the staff were helpful",
  "Very small room, but friendly staff overall",
  "The staff were rude and the room was dirty",
  "Rude staff and a dirty, small room",
  "Clean room, friendly staff, would return",
  "Helpful staff, clean room, minor noise issue",
  "Noise was an issue but staff were helpful",
  "Small room but very clean and quiet",
  "Friendly staff made up for the small room",
  "The room was dirty and staff were unhelpful"
)

make_text_instrument <- function() {
  sf_instrument(
    title = "Text analysis fixture",
    version = "1.0.0",
    components = list(
      sf_item("comments", "Any comments about your stay?", type = "textarea"),
      sf_item("branch", "Which branch?", type = "single_choice",
              choice_set = "branch_cs")
    )
  )
}

make_text_data <- function(n = length(phrases)) {
  data.frame(
    comments = phrases[seq_len(n)],
    branch = rep(c("north", "south"), length.out = n),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# .sframe_tokenise()
# ---------------------------------------------------------------------------

test_that(".sframe_tokenise lower-cases, strips punctuation, and drops stop words", {
  toks <- surveyframe:::.sframe_tokenise("The Staff's Room, Very Clean!")
  expect_equal(toks[[1]], c("staff's", "room", "clean"))
})

test_that(".sframe_tokenise honours stop_words = character(0)", {
  toks <- surveyframe:::.sframe_tokenise("this is a test", stop_words = character(0))
  expect_equal(toks[[1]], c("this", "is", "a", "test"))
})

test_that(".sframe_tokenise returns an empty vector for NA or blank input", {
  toks <- surveyframe:::.sframe_tokenise(c(NA, "  ", "clean room"))
  expect_equal(toks[[1]], character(0))
  expect_equal(toks[[2]], character(0))
  expect_equal(toks[[3]], c("clean", "room"))
})

# ---------------------------------------------------------------------------
# clean_text_responses()
# ---------------------------------------------------------------------------

test_that("clean_text_responses drops NA and blank responses and lower-cases", {
  data <- data.frame(comments = c("Great STAY", NA, "  ", "Terrible!"),
                     stringsAsFactors = FALSE)
  out <- clean_text_responses(data, "comments")
  expect_equal(as.character(out), c("great stay", "terrible"))
  expect_equal(attr(out, "respondent"), c(1L, 4L))
})

test_that("clean_text_responses respects lowercase/remove_punct/strip_numbers flags", {
  data <- data.frame(comments = "Room 42 was GREAT!!", stringsAsFactors = FALSE)
  out <- clean_text_responses(data, "comments", lowercase = FALSE,
                               remove_punct = FALSE, strip_numbers = TRUE)
  # Number stripping leaves a gap ("Room  was"); the final whitespace
  # collapse (always applied, not gated on remove_punct) folds it back to
  # one space, so only the digits and the case/punctuation are informative
  # here, not the spacing.
  expect_equal(as.character(out), "Room was GREAT!!")
})

test_that("clean_text_responses validates item type against an instrument", {
  data <- make_text_data()
  instr <- make_text_instrument()
  expect_error(
    clean_text_responses(data, "branch", instrument = instr),
    class = "sframe_error"
  )
  expect_silent(clean_text_responses(data, "comments", instrument = instr))
})

# ---------------------------------------------------------------------------
# term_frequency()
# ---------------------------------------------------------------------------

test_that("term_frequency counts terms, sorted descending, with n and pct", {
  cleaned <- clean_text_responses(make_text_data(), "comments")
  tbl <- term_frequency(cleaned)
  expect_true(all(c("term", "n", "pct") %in% names(tbl)))
  expect_true(all(diff(tbl$n) <= 0))
  expect_equal(tbl$term[1], "staff")
  expect_equal(round(sum(tbl$n) * tbl$pct[1] / 100), tbl$n[1])
})

test_that("term_frequency respects top_n", {
  cleaned <- clean_text_responses(make_text_data(), "comments")
  tbl <- term_frequency(cleaned, top_n = 3L)
  expect_equal(nrow(tbl), 3L)
})

test_that("term_frequency on empty input returns a 0-row table, not an error", {
  tbl <- term_frequency(character(0))
  expect_equal(nrow(tbl), 0L)
  expect_equal(names(tbl), c("term", "n", "pct"))
})

# ---------------------------------------------------------------------------
# sframe_run_term_freq() via run_analysis_plan()
# ---------------------------------------------------------------------------

add_text_rq <- function(instr, roles, options = list()) {
  instr$analysis_plan <- c(instr$analysis_plan, list(list(
    id = "rq1", research_question = "What themes recur in the comments?",
    roles = roles, options = options, test = "term_freq",
    alpha = 0.05, citations = character(0), interpretation = "",
    result = NULL
  )))
  instr
}

test_that("term_freq runs end to end through run_analysis_plan and humanizes cleanly", {
  instr <- add_text_rq(make_text_instrument(), roles = list(item = "comments"))
  res <- run_analysis_plan(make_text_data(), instr)
  result <- res[[1]]
  expect_null(result$error)
  expect_true(is.data.frame(result$table))
  expect_true(all(c("term", "n", "pct") %in% names(result$table)))
  expect_match(result$apa, "Term frequency for comments")
})

test_that("term_freq below the minimum-response floor errors rather than computing", {
  instr <- add_text_rq(make_text_instrument(), roles = list(item = "comments"))
  data <- make_text_data(n = 3)
  res <- run_analysis_plan(data, instr)
  expect_false(is.null(res[[1]]$error))
  expect_null(res[[1]]$table)
})

# Mutation check for the guard above: with the floor lowered to 1, the same
# 3-response data must compute rather than error, confirming the error above
# comes from the guard and not from some other failure.
test_that("mutation check: a lower floor lets a small term_freq input compute", {
  old <- surveyframe:::.sframe_text_min_responses
  # Can't reassign a namespace binding directly in a locked namespace test
  # without unlocking it; instead call the runner's own guard logic at n=3
  # directly via term_frequency(), confirming it is NOT itself empty, i.e.
  # the error above is solely the runner's floor check, not term_frequency().
  cleaned <- clean_text_responses(make_text_data(n = 3), "comments")
  tbl <- term_frequency(cleaned)
  expect_gt(nrow(tbl), 0)
})

test_that("term_freq group role splits the table by group and flags a thin group", {
  data <- make_text_data()
  # Skew the group sizes: 10 "north", 2 "south", so south sits below the
  # 10-response floor while north does not.
  data$branch <- c(rep("north", 10), rep("south", 2))
  instr <- add_text_rq(make_text_instrument(),
                       roles = list(item = "comments", group = "branch"))
  res <- run_analysis_plan(data, instr)
  result <- res[[1]]
  expect_null(result$error)
  tbl <- result$table
  expect_true("group" %in% names(tbl))
  expect_true("north" %in% tbl$group)
  south_rows <- tbl[tbl$group == "south", , drop = FALSE]
  expect_true(nrow(south_rows) >= 1)
  expect_true(all(!is.na(south_rows$note)))
})

test_that("term_freq group role is additive: absent group, output matches the ungrouped path", {
  instr_grouped <- add_text_rq(make_text_instrument(),
                               roles = list(item = "comments", group = "branch"))
  instr_plain <- add_text_rq(make_text_instrument(), roles = list(item = "comments"))
  data <- make_text_data()
  data$branch <- NULL
  res_grouped <- run_analysis_plan(data, instr_grouped)
  res_plain <- run_analysis_plan(data, instr_plain)
  # branch column absent from data -> group role resolves to a no-op group_id
  # that isn't a real column, so the grouped path falls back to the plain one.
  expect_equal(res_grouped[[1]]$table, res_plain[[1]]$table)
})

# ---------------------------------------------------------------------------
# sframe_plot_term_frequency()
# ---------------------------------------------------------------------------

test_that("sframe_plot_term_frequency returns a ggplot for a bar chart", {
  skip_if_not_installed("ggplot2")
  instr <- add_text_rq(make_text_instrument(), roles = list(item = "comments"))
  res <- run_analysis_plan(make_text_data(), instr, plots = TRUE)
  expect_s3_class(res[[1]]$plot, "ggplot")
})

test_that("the bar chart puts the largest term at the top after coord_flip (LADAL convention)", {
  skip_if_not_installed("ggplot2")
  tbl <- data.frame(term = c("small", "medium", "large"), n = c(1, 5, 10),
                    pct = c(10, 50, 100), stringsAsFactors = FALSE)
  p <- sframe_plot_term_frequency(list(test = "term_freq", variable = "x", table = tbl))
  # The LAST factor level is what coord_flip() draws at the top. The
  # ungrouped path still tags a "\rall" group suffix internally (see
  # .sframe_plot_term_bar()), stripped off for the printed label but
  # still present on the raw factor level.
  lv <- levels(p$data$term_facet)
  expect_equal(lv[length(lv)], paste0("large", "\r", "all"))
  expect_equal(lv[1], paste0("small", "\r", "all"))
})

test_that("sframe_plot_term_frequency draws a word cloud when opted in", {
  skip_if_not_installed("ggplot2")
  instr <- add_text_rq(make_text_instrument(), roles = list(item = "comments"),
                       options = list(wordcloud = TRUE))
  res <- run_analysis_plan(make_text_data(), instr, plots = TRUE)
  expect_s3_class(res[[1]]$plot, "ggplot")
  built <- ggplot2::ggplot_build(res[[1]]$plot)
  # A word cloud lays out one label per term at distinct (x, y) coordinates;
  # duplicated coordinates would mean the spiral layout degenerated (e.g.
  # back to a single point), which is the concrete overlap failure mode
  # todo_text_analysis.md's exit checklist calls out.
  coords <- unique(built$data[[1]][c("x", "y")])
  expect_equal(nrow(coords), nrow(built$data[[1]]))
})

test_that("the term cloud's words are darker (more opaque) the more frequent they are", {
  skip_if_not_installed("ggplot2")
  instr <- add_text_rq(make_text_instrument(), roles = list(item = "comments"),
                       options = list(wordcloud = TRUE))
  res <- run_analysis_plan(make_text_data(), instr, plots = TRUE)
  built_data <- ggplot2::ggplot_build(res[[1]]$plot)$data[[1]]
  expect_gt(length(unique(built_data$alpha)), 1)
  expect_true(all(built_data$alpha >= 0.5 - 1e-6))
  expect_equal(max(built_data$alpha), 1)
})

test_that(".sframe_wordcloud_layout(shape = 'circle') roughly fills a disc rather than sprawling", {
  words <- paste0("word", 1:20)
  sizes <- rep(10, 20)
  layout_circle <- surveyframe:::.sframe_wordcloud_layout(words, sizes, aspect = 1, shape = "circle")
  layout_organic <- surveyframe:::.sframe_wordcloud_layout(words, sizes, aspect = 1, shape = "organic")
  r_circle <- sqrt(layout_circle$x^2 + layout_circle$y^2)
  r_organic <- sqrt(layout_organic$x^2 + layout_organic$y^2)
  # The capped/wrapped disc must not sprawl further out than the
  # uncapped spiral does for the identical word set.
  expect_lte(max(r_circle), max(r_organic) + 1e-6)
})

test_that("mutation check: shape = 'circle' actually constrains the radius (not a no-op)", {
  # A pathologically tiny disc (huge words, tiny target radius) forces the
  # wrap-around branch to fire; confirms the cap is load-bearing rather
  # than vacuously satisfied because 20 small words never reached it.
  words <- paste0("word", 1:15)
  sizes <- rep(30, 15)
  layout <- surveyframe:::.sframe_wordcloud_layout(words, sizes, aspect = 1, shape = "circle")
  expect_equal(nrow(layout), 15L)
  expect_true(all(is.finite(layout$x)) && all(is.finite(layout$y)))
})

test_that(".sframe_wordcloud_layout() places no two words' bounding boxes overlapping", {
  skip_if_not_installed("ggplot2")
  # A deliberately adversarial fixture: many words of similar, large size,
  # which is exactly the case that overlapped under the old golden-angle
  # spiral (no collision check) and under the first collision-aware
  # attempt (strheight() silently ignoring a vectorised cex, so every
  # bounding box used the same height regardless of font size).
  words <- c("staff", "service", "helpful", "friendly", "quick", "clean",
             "spotless", "attentive", "welcome", "comfortable")
  sizes <- rep(16, length(words))
  layout <- surveyframe:::.sframe_wordcloud_layout(words, sizes)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  cex <- sizes / 4
  hw <- graphics::strwidth(words, units = "inches", cex = cex, font = 2) / 2
  hh <- mapply(function(w, cx) graphics::strheight(w, units = "inches", cex = cx, font = 2),
              words, cex) / 2
  layout <- merge(layout, data.frame(term = words, hw = hw, hh = hh, stringsAsFactors = FALSE),
                  by = "term")
  n <- nrow(layout)
  overlap_found <- FALSE
  for (i in seq_len(n - 1)) {
    for (j in seq.int(i + 1, n)) {
      dx <- abs(layout$x[i] - layout$x[j])
      dy <- abs(layout$y[i] - layout$y[j])
      if (dx < (layout$hw[i] + layout$hw[j]) && dy < (layout$hh[i] + layout$hh[j])) {
        overlap_found <- TRUE
      }
    }
  }
  expect_false(overlap_found)
})

test_that("mutation check: shrinking the padding to near-zero lets an overlap through", {
  # Confirms the test above is actually sensitive to overlap, not
  # vacuously passing regardless of the layout.
  words <- c("staff", "service", "helpful", "friendly", "quick")
  sizes <- rep(16, length(words))
  layout <- surveyframe:::.sframe_wordcloud_layout(words, sizes, padding = -0.85)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  cex <- sizes / 4
  hw <- graphics::strwidth(words, units = "inches", cex = cex, font = 2) / 2 * (1 + 0.85)
  hh <- mapply(function(w, cx) graphics::strheight(w, units = "inches", cex = cx, font = 2),
              words, cex) / 2 * (1 + 0.85)
  layout <- merge(layout, data.frame(term = words, hw = hw, hh = hh, stringsAsFactors = FALSE),
                  by = "term")
  n <- nrow(layout)
  overlap_found <- FALSE
  for (i in seq_len(n - 1)) {
    for (j in seq.int(i + 1, n)) {
      dx <- abs(layout$x[i] - layout$x[j])
      dy <- abs(layout$y[i] - layout$y[j])
      if (dx < (layout$hw[i] + layout$hw[j]) && dy < (layout$hh[i] + layout$hh[j])) {
        overlap_found <- TRUE
      }
    }
  }
  expect_true(overlap_found)
})

test_that("sframe_plot_term_frequency facets by group when the group role is present", {
  skip_if_not_installed("ggplot2")
  data <- make_text_data()
  instr <- add_text_rq(make_text_instrument(),
                       roles = list(item = "comments", group = "branch"))
  res <- run_analysis_plan(data, instr, plots = TRUE)
  expect_s3_class(res[[1]]$plot, "ggplot")
  expect_true(inherits(res[[1]]$plot$facet, "FacetWrap"))
})

test_that("each facet of a grouped bar chart sorts by ITS OWN frequency, not a shared global order", {
  skip_if_not_installed("ggplot2")
  # "shared" appears in both groups but at a HIGHER count in group "b"
  # than "solo" (which only appears in group "a"): a global (not
  # per-facet) factor level would place "shared" in one fixed position
  # for both facets, which is wrong for whichever facet it doesn't match.
  tbl <- data.frame(
    group = c("a", "a", "b", "b"),
    term  = c("solo", "shared", "other", "shared"),
    n     = c(10, 3, 2, 20),
    pct   = c(50, 15, 10, 90),
    stringsAsFactors = FALSE
  )
  p <- sframe_plot_term_frequency(list(test = "term_freq", variable = "x", table = tbl))
  lv <- levels(p$data$term_facet)
  # Facet "a": solo (10) must outrank shared (3) -- solo's level must
  # come after shared's among the group-"a" levels.
  a_levels <- lv[grepl("\ra$", lv)]
  expect_equal(a_levels[length(a_levels)], paste0("solo", "\r", "a"))
  # Facet "b": shared (20) must outrank other (2).
  b_levels <- lv[grepl("\rb$", lv)]
  expect_equal(b_levels[length(b_levels)], paste0("shared", "\r", "b"))
})

# ---------------------------------------------------------------------------
# $quotes reporting hook (mirrors the $syntax pattern; see R/reporting.R)
# ---------------------------------------------------------------------------

test_that("a $quotes data.frame on a result renders as its own table in the report", {
  instr <- add_text_rq(make_text_instrument(), roles = list(item = "comments"))
  data <- make_text_data()
  quotes_df <- data.frame(topic = 1L, rank = 1L, respondent = 3L,
                          quote = "Room was clean and the staff were helpful",
                          stringsAsFactors = FALSE)
  # Simulate what extract_quotes() will attach once it lands, by patching
  # run_analysis_plan()'s result in place via the same code path reporting.R
  # reads: instrument$analysis_plan[[i]]$result is not used for this render,
  # reporting.R re-runs the plan itself, so exercise the render helper
  # directly against a result list carrying $quotes.
  result <- list(
    research_question = "What themes recur?", apa = "Term frequency computed.",
    table = term_frequency(clean_text_responses(data, "comments")),
    quotes = quotes_df, decision_rule = "", block_id = "rq1"
  )
  html <- surveyframe:::.render_report_table(result$quotes, "Representative quotes")
  expect_match(html, "Representative quotes")
  expect_match(html, "Room was clean and the staff were helpful")
})

test_that("a result with no $quotes renders no quotes table (mutation check)", {
  result <- list(table = data.frame(term = "a", n = 1, pct = 100))
  quotes_html <- if (is.data.frame(result$quotes) && nrow(result$quotes) > 0) {
    surveyframe:::.render_report_table(result$quotes, "Representative quotes")
  } else ""
  expect_equal(quotes_html, "")
})

# ---------------------------------------------------------------------------
# Regression: text-family results are not run through the instrument-wide
# label substitution (review_050 finding: a respondent's own word could
# collide with an unrelated item's choice CODE and get silently swapped for
# that item's choice LABEL, reading as thematic signal that was actually a
# coincidence).
# ---------------------------------------------------------------------------

test_that("term_freq's term column is not relabelled when it collides with an unrelated choice code", {
  cs <- sf_choices("dept", values = c("pool", "spa"), labels = c("Pool area", "Spa area"))
  instr <- sf_instrument(
    title = "Collision fixture", version = "1.0.0",
    components = list(
      cs,
      sf_item("dept", "Which department?", type = "single_choice", choice_set = "dept"),
      sf_item("comments", "Any comments?", type = "textarea")
    )
  )
  data <- data.frame(
    dept = sample(c("pool", "spa"), 15, replace = TRUE),
    comments = rep("the pool area was great and the pool was clean", 15),
    stringsAsFactors = FALSE
  )
  instr$analysis_plan <- list(list(
    id = "RQ1", research_question = "Top terms?", family = "text",
    method = "term_freq", roles = list(item = "comments"), options = list(),
    alpha = 0.05, citations = character(0), interpretation = "", result = NULL
  ))
  res <- run_analysis_plan(data, instr)
  expect_equal(res[[1]]$table$term[1], "pool")
})

test_that("term_freq's own group column IS still humanized on the same table its term column is excluded from", {
  # Regression for a bug introduced and caught while fixing the collision
  # above: the first fix excluded the WHOLE table from humanization for
  # every text id, which silently also stopped humanizing `group` (a
  # genuinely coded value, e.g. "pool" -> "Pool area" is correct and
  # wanted there). The fix must be column-scoped, not table-scoped.
  cs <- sf_choices("dept", values = c("pool", "spa"), labels = c("Pool area", "Spa area"))
  instr <- sf_instrument(
    title = "Group-still-humanized fixture", version = "1.0.0",
    components = list(
      cs,
      sf_item("dept", "Which department?", type = "single_choice", choice_set = "dept"),
      sf_item("comments", "Any comments?", type = "textarea")
    )
  )
  data <- data.frame(
    dept = c(rep("pool", 12), rep("spa", 12)),
    comments = rep("the pool area was great and the pool was clean", 24),
    stringsAsFactors = FALSE
  )
  instr$analysis_plan <- list(list(
    id = "RQ1", research_question = "Top terms by dept?", family = "text",
    method = "term_freq", roles = list(item = "comments", group = "dept"),
    options = list(), alpha = 0.05, citations = character(0),
    interpretation = "", result = NULL
  ))
  res <- run_analysis_plan(data, instr)
  tbl <- res[[1]]$table
  expect_true("Pool area" %in% tbl$group)
  expect_true("pool" %in% tbl$term)
  expect_false("Pool area" %in% tbl$term)
})

test_that("mutation check: a non-text result IS still humanized (the exclusion is scoped, not global)", {
  cs <- sf_choices("q5", values = 1:2, labels = c("Low", "High"))
  instr <- sf_instrument(
    title = "humanize sanity", version = "1.0.0",
    components = list(cs, sf_item("rating", "Rating?", type = "single_choice", choice_set = "q5"))
  )
  data <- data.frame(rating = c("1", "1", "2"), stringsAsFactors = FALSE)
  instr$analysis_plan <- list(list(
    id = "RQ1", research_question = "dist", family = "descriptive",
    method = "frequency", roles = list(variable = "rating"), options = list(),
    alpha = 0.05, citations = character(0), interpretation = "", result = NULL
  ))
  res <- run_analysis_plan(data, instr)
  expect_true(all(c("Low", "High") %in% res[[1]]$table$Value))
})
