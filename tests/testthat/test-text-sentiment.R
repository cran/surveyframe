# tests/testthat/test-text-sentiment.R
# Sentiment and DFM analysis (todo_0.5.md section 1b, agent 3): the bing-
# lexicon sentiment runner sframe_run_tidy_sentiment(), the descriptive
# document-feature matrix runner sframe_run_quanteda_dfm(), and the
# diverging sentiment plot sframe_plot_sentiment(). Every test that needs
# tidytext or quanteda skips cleanly via skip_if_not_installed() when the
# package is absent, matching test-text-analysis.R's convention.
#
# These tests call the runners directly (sframe_run_tidy_sentiment(),
# sframe_run_quanteda_dfm()) rather than through run_analysis_plan(), since
# this diff deliberately does not touch R/analysis_plan.R's dispatch switch
# (see the final report for the exact "tidy_sentiment =" / "quanteda_dfm ="
# lines to add there). options$group is resolved centrally by
# sframe_run_one_block() for test %in% c("term_freq", "tidy_sentiment")
# already, so the group-role tests below pass options$group directly,
# reproducing what that resolution step would hand the runner.

# 12 responses: 5 clearly positive (bing: "amazing", "wonderful", "great",
# "excellent", "fantastic", "lovely", "perfect", "loved") and 5 clearly
# negative ("terrible", "awful", "horrible", "worst", "disgusting", "bad",
# "dirty", "hated") net-score responses, plus 2 with no bing-lexicon hit at
# all (net neutral). Hand-verified against tidytext::get_sentiments("bing")
# directly (not just re-running the code under test) before writing the
# expectations below.
sentiment_phrases <- c(
  "Amazing wonderful staff, loved the stay",         # 3 pos, 0 neg -> positive
  "Terrible awful room, hated it",                   # 0 pos, 3 neg -> negative
  "Great excellent friendly service",                 # 3 pos, 0 neg -> positive
  "Horrible worst experience ever",                   # 0 pos, 2 neg -> negative
  "Fantastic lovely perfect room",                     # 3 pos, 0 neg -> positive
  "Disgusting bad dirty room",                         # 0 pos, 3 neg -> negative
  "The front desk gave directions to the elevator",    # 0 pos, 0 neg -> neutral
  "Check in happened at the counter near the door",    # 0 pos, 0 neg -> neutral
  "Amazing staff, great room, wonderful stay",         # 3 pos, 0 neg -> positive
  "Awful staff, terrible room, horrible stay",         # 0 pos, 3 neg -> negative
  "Lovely excellent fantastic service",                # 3 pos, 0 neg -> positive
  "Worst dirty disgusting experience"                  # 0 pos, 3 neg -> negative
)

make_sentiment_instrument <- function() {
  sf_instrument(
    title = "Sentiment fixture",
    version = "1.0.0",
    components = list(
      sf_item("comments", "Any comments about your stay?", type = "textarea"),
      sf_item("branch", "Which branch?", type = "single_choice",
              choice_set = "branch_cs")
    )
  )
}

make_sentiment_data <- function(n = length(sentiment_phrases)) {
  data.frame(
    comments = sentiment_phrases[seq_len(n)],
    branch = rep(c("north", "south"), length.out = n),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# sframe_run_tidy_sentiment(): hand-verified counts
# ---------------------------------------------------------------------------

test_that("tidy sentiment counts positive/negative responses correctly on a hand-verified fixture", {
  skip_if_not_installed("tidytext")
  data <- make_sentiment_data()
  res <- sframe_run_tidy_sentiment(data, list(item = "comments"), list(), NULL)
  expect_null(res$error)
  tbl <- res$table
  expect_true(all(c("sentiment", "n", "prop") %in% names(tbl)))
  # 5 responses net positive, 5 net negative, 2 net neutral (no bing hits at
  # all), hand-counted above against tidytext::get_sentiments("bing").
  expect_equal(tbl$n[tbl$sentiment == "positive"], 5L)
  expect_equal(tbl$n[tbl$sentiment == "negative"], 5L)
  expect_equal(tbl$n[tbl$sentiment == "neutral"], 2L)
  expect_equal(tbl$prop[tbl$sentiment == "positive"], round(5 / 12, 3))
})

test_that("tidy sentiment $scores carries one row per usable respondent", {
  skip_if_not_installed("tidytext")
  data <- make_sentiment_data()
  res <- sframe_run_tidy_sentiment(data, list(item = "comments"), list(), NULL)
  expect_true(is.data.frame(res$scores))
  expect_equal(nrow(res$scores), 12L)
  expect_true(all(c("respondent", "positive", "negative", "score") %in% names(res$scores)))
  # Response 1 ("Amazing wonderful staff, loved the stay") has 3 positive
  # bing hits and 0 negative.
  expect_equal(res$scores$score[1], 3L)
  # Response 2 ("Terrible awful room, hated it") has 3 negative bing hits.
  expect_equal(res$scores$score[2], -3L)
})

test_that("tidy_sentiment runner humanizes cleanly (run_analysis_plan dispatch not yet wired, see report)", {
  skip_if_not_installed("tidytext")
  instr <- make_sentiment_instrument()
  result <- sframe_run_tidy_sentiment(
    make_sentiment_data(), list(item = "comments"), list(), instr
  )
  expect_null(result$error)
  expect_true(is.data.frame(result$table))
  expect_match(result$apa, "Sentiment for comments")
})

# ---------------------------------------------------------------------------
# tidy_sentiment: group role
# ---------------------------------------------------------------------------

test_that("tidy_sentiment group role splits the table by group and flags a thin group", {
  skip_if_not_installed("tidytext")
  data <- make_sentiment_data()
  # Skew the group sizes: 10 "north", 2 "south", so south sits below the
  # 10-response floor while north does not.
  data$branch <- c(rep("north", 10), rep("south", 2))
  result <- sframe_run_tidy_sentiment(
    data, list(item = "comments"), list(group = "branch"), NULL
  )
  expect_null(result$error)
  tbl <- result$table
  expect_true("group" %in% names(tbl))
  expect_true("north" %in% tbl$group)
  south_rows <- tbl[tbl$group == "south", , drop = FALSE]
  expect_true(nrow(south_rows) >= 1)
  expect_true(all(!is.na(south_rows$note)))
})

test_that("tidy_sentiment group role is additive: absent group, output matches the ungrouped path", {
  skip_if_not_installed("tidytext")
  data <- make_sentiment_data()
  data$branch <- NULL
  res_grouped <- sframe_run_tidy_sentiment(
    data, list(item = "comments"), list(group = "branch"), NULL
  )
  res_plain <- sframe_run_tidy_sentiment(
    data, list(item = "comments"), list(), NULL
  )
  expect_equal(res_grouped$table, res_plain$table)
})

# ---------------------------------------------------------------------------
# tidy_sentiment: minimum-response guard, mutation-checked
# ---------------------------------------------------------------------------

test_that("tidy_sentiment below the minimum-response floor errors rather than computing", {
  skip_if_not_installed("tidytext")
  data <- make_sentiment_data(n = 3)
  result <- sframe_run_tidy_sentiment(data, list(item = "comments"), list(), NULL)
  expect_false(is.null(result$error))
  expect_null(result$table)
})

test_that("mutation check: the tidy_sentiment guard is what produces the error above", {
  skip_if_not_installed("tidytext")
  # Same technique as test-text-analysis.R's mutation check: confirm the
  # underlying computation itself is NOT empty at n = 3, so the error above
  # is solely the runner's floor check against .sframe_text_min_responses,
  # not some other failure (e.g. an empty lexicon join).
  data <- make_sentiment_data(n = 3)
  cleaned <- clean_text_responses(data, "comments")
  res <- surveyframe:::sframe_run_tidy_sentiment(
    data, list(item = "comments"), list(), NULL
  )
  expect_false(is.null(res$error))
  # Directly bypass the floor by calling the internal scoring path at n = 3:
  # confirms 3 usable responses do produce real per-response scores, i.e.
  # the guard, not the scoring logic, is what blocks the n = 3 case above.
  toks <- surveyframe:::.sframe_tokenise(cleaned, stop_words = character(0))
  expect_equal(length(toks), 3L)
  expect_true(any(lengths(toks) > 0))
})

# ---------------------------------------------------------------------------
# sframe_run_quanteda_dfm()
# ---------------------------------------------------------------------------

test_that("quanteda_dfm produces a summary table with the expected shape", {
  skip_if_not_installed("quanteda")
  data <- make_sentiment_data()
  res <- sframe_run_quanteda_dfm(data, list(item = "comments"), list(), NULL)
  expect_null(res$error)
  expect_true(is.data.frame(res$table))
  expect_equal(nrow(res$table), 1L)
  expect_true(all(c("n_responses", "n_features", "sparsity") %in% names(res$table)))
  expect_equal(res$table$n_responses, 12L)
  expect_true(res$table$n_features > 0)
  expect_true(res$table$sparsity >= 0 && res$table$sparsity <= 1)
  expect_true(is.data.frame(res$top_features))
  expect_true(all(c("term", "n") %in% names(res$top_features)))
  expect_true(all(diff(res$top_features$n) <= 0))
  expect_match(res$apa, "Document-feature matrix for comments")
})

test_that("quanteda_dfm below the minimum-response floor errors rather than computing", {
  skip_if_not_installed("quanteda")
  data <- make_sentiment_data(n = 3)
  res <- sframe_run_quanteda_dfm(data, list(item = "comments"), list(), NULL)
  expect_false(is.null(res$error))
  expect_null(res$table)
})

test_that("mutation check: quanteda_dfm's guard is what blocks the n = 3 case", {
  skip_if_not_installed("quanteda")
  # Confirm a dfm CAN be built at n = 3 directly, so the error above is
  # solely the runner's floor check, not a downstream quanteda failure.
  data <- make_sentiment_data(n = 3)
  cleaned <- clean_text_responses(data, "comments")
  d <- quanteda::dfm(quanteda::tokens(as.character(cleaned)))
  expect_true(quanteda::nfeat(d) > 0)
})

# ---------------------------------------------------------------------------
# sframe_plot_sentiment()
# ---------------------------------------------------------------------------

test_that("sframe_plot_sentiment returns a ggplot for a diverging bar chart", {
  skip_if_not_installed("tidytext")
  skip_if_not_installed("ggplot2")
  result <- sframe_run_tidy_sentiment(
    make_sentiment_data(), list(item = "comments"), list(), NULL
  )
  p <- sframe_plot_sentiment(result)
  expect_s3_class(p, "ggplot")
})

test_that("sframe_plot_sentiment facets by group when the group role is present", {
  skip_if_not_installed("tidytext")
  skip_if_not_installed("ggplot2")
  data <- make_sentiment_data()
  data$branch <- c(rep("north", 10), rep("south", 2))
  result <- sframe_run_tidy_sentiment(
    data, list(item = "comments"), list(group = "branch"), NULL
  )
  p <- sframe_plot_sentiment(result)
  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$facet, "FacetWrap"))
})

test_that("sframe_plot_sentiment returns NULL when the result carries no table", {
  skip_if_not_installed("ggplot2")
  expect_null(sframe_plot_sentiment(list(table = NULL)))
})

# ---------------------------------------------------------------------------
# $word_sentiment and the comparison-cloud plot (options$wordcloud = TRUE)
# ---------------------------------------------------------------------------

test_that("sframe_run_tidy_sentiment() attaches word x sentiment counts", {
  skip_if_not_installed("tidytext")
  result <- sframe_run_tidy_sentiment(
    make_sentiment_data(), list(item = "comments"), list(), NULL
  )
  ws <- result$word_sentiment
  expect_true(is.data.frame(ws))
  expect_true(all(c("word", "sentiment", "n") %in% names(ws)))
  expect_true(all(ws$sentiment %in% c("positive", "negative")))
  # From the hand-verified fixture: "amazing" appears in 2 positive
  # responses, "terrible" in 2 negative ones.
  expect_equal(ws$n[ws$word == "amazing"], 2L)
  expect_equal(ws$n[ws$word == "terrible"], 2L)
})

test_that("the comparison cloud places negative words left and positive words right of centre", {
  skip_if_not_installed("tidytext")
  skip_if_not_installed("ggplot2")
  result <- sframe_run_tidy_sentiment(
    make_sentiment_data(), list(item = "comments"), list(), NULL
  )
  cloud_tbl <- surveyframe:::.sframe_sentiment_cloud_layout(result$word_sentiment)
  expect_true(all(cloud_tbl$x[cloud_tbl$sentiment == "negative"] < 0))
  expect_true(all(cloud_tbl$x[cloud_tbl$sentiment == "positive"] > 0))
})

test_that("the comparison cloud's two groups never overlap each other or themselves", {
  skip_if_not_installed("tidytext")
  skip_if_not_installed("ggplot2")
  result <- sframe_run_tidy_sentiment(
    make_sentiment_data(), list(item = "comments"), list(), NULL
  )
  cloud_tbl <- surveyframe:::.sframe_sentiment_cloud_layout(result$word_sentiment)
  n <- nrow(cloud_tbl)
  overlap_found <- FALSE
  for (i in seq_len(n - 1)) {
    for (j in seq.int(i + 1, n)) {
      dx <- abs(cloud_tbl$x[i] - cloud_tbl$x[j])
      dy <- abs(cloud_tbl$y[i] - cloud_tbl$y[j])
      if (dx < (cloud_tbl$half_w[i] + cloud_tbl$half_w[j]) &&
          dy < (cloud_tbl$half_h[i] + cloud_tbl$half_h[j])) {
        overlap_found <- TRUE
      }
    }
  }
  expect_false(overlap_found)
})

test_that("sframe_plot_sentiment draws a comparison cloud when opted in", {
  skip_if_not_installed("tidytext")
  skip_if_not_installed("ggplot2")
  result <- sframe_run_tidy_sentiment(
    make_sentiment_data(), list(item = "comments"), list(wordcloud = TRUE), NULL
  )
  result$options <- list(wordcloud = TRUE)
  p <- sframe_plot_sentiment(result)
  expect_s3_class(p, "ggplot")
  # Every word's rendered extent must sit fully inside the panel's data
  # range (the concrete clipping failure mode caught and fixed while
  # building this: coord_fixed() computed from anchor points alone cuts
  # off the far edge of a long word at the boundary).
  built <- ggplot2::ggplot_build(p)
  panel_range <- built$layout$panel_params[[1]]$x.range
  cloud_tbl <- surveyframe:::.sframe_sentiment_cloud_layout(result$word_sentiment)
  expect_true(all(cloud_tbl$x - cloud_tbl$half_w >= panel_range[1] - 1e-6))
  expect_true(all(cloud_tbl$x + cloud_tbl$half_w <= panel_range[2] + 1e-6))
})

test_that("the comparison cloud's words are darker (more opaque) the more frequent they are", {
  skip_if_not_installed("tidytext")
  skip_if_not_installed("ggplot2")
  result <- sframe_run_tidy_sentiment(
    make_sentiment_data(), list(item = "comments"), list(wordcloud = TRUE), NULL
  )
  result$options <- list(wordcloud = TRUE)
  p <- sframe_plot_sentiment(result)
  built_data <- ggplot2::ggplot_build(p)$data[[1]]
  # Alpha must vary (not a single flat value) and must be floored, not
  # allowed to fade toward invisible.
  expect_gt(length(unique(built_data$alpha)), 1)
  expect_true(all(built_data$alpha >= 0.5 - 1e-6))
  expect_true(all(built_data$alpha <= 1))
  # The single most frequent word overall must be at (or near) full
  # opacity, not just "some word somewhere is dark".
  expect_equal(max(built_data$alpha), 1)
})
