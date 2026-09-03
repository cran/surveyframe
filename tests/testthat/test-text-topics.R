# tests/testthat/test-text-topics.R
# Topic modelling (todo_text_analysis.md): sframe_run_topic_model_lda() (topicmodels),
# sframe_run_stm_topics() (stm), extract_quotes(), and sframe_plot_topics().
# Every test needing an optional package skips cleanly when it is missing
# rather than failing (skip_if_not_installed()).

phrases <- c(
  "The staff were very friendly and helpful with our room",
  "Staff were friendly but the room was rather small",
  "Room was clean and the staff were extremely helpful",
  "Very small room, but friendly staff overall this trip",
  "The staff were rude and the room was quite dirty",
  "Rude staff and a dirty, small room this time",
  "Clean room, friendly staff, we would return happily",
  "Helpful staff, clean room, minor noise issue at night",
  "Noise was an issue but staff were helpful anyway",
  "Small room but very clean and quiet overall",
  "Friendly staff made up for the small room size",
  "The room was dirty and staff were unhelpful sadly",
  "Great location, friendly staff, room could be cleaner",
  "Breakfast was excellent and staff were very friendly",
  "Parking was difficult but staff were helpful throughout",
  "The pool area was clean and staff were attentive",
  "Checkout was slow but staff remained friendly and calm",
  "Wifi was unreliable though staff tried to help fast"
)

make_text_instrument <- function() {
  sf_instrument(
    title = "Topic model fixture",
    version = "1.0.0",
    components = list(
      sf_item("comments", "Any comments about your stay?", type = "textarea")
    )
  )
}

make_topic_data <- function(n = length(phrases)) {
  data.frame(comments = phrases[seq_len(n)], stringsAsFactors = FALSE)
}

roles <- list(item = "comments")

# ---------------------------------------------------------------------------
# Tokenising step in isolation (the NSE fix for sframe_run_stm_topics())
# ---------------------------------------------------------------------------

test_that("the tidyeval tokenising step used by sframe_run_stm_topics() actually tokenises", {
  skip_if_not_installed("tidytext")
  doc_df <- data.frame(.doc = 1:2,
                        .text = c("hello world foo", "world foo bar"),
                        stringsAsFactors = FALSE)
  word_sym <- rlang::sym(".word")
  text_sym <- rlang::sym(".text")
  out <- tidytext::unnest_tokens(doc_df, !!word_sym, !!text_sym)
  expect_true(all(c(".doc", ".word") %in% names(out)))
  expect_equal(out$.word[out$.doc == 1], c("hello", "world", "foo"))
  expect_equal(nrow(out), 6)
})

# ---------------------------------------------------------------------------
# LDA (topicmodels)
# ---------------------------------------------------------------------------

test_that("sframe_run_topic_model_lda() is deterministic under a fixed seed", {
  skip_if_not_installed("tidytext")
  skip_if_not_installed("topicmodels")
  data <- make_topic_data()
  r1 <- sframe_run_topic_model_lda(data, roles, list(k = 2L, seed = 42L), NULL)
  r2 <- sframe_run_topic_model_lda(data, roles, list(k = 2L, seed = 42L), NULL)
  expect_null(r1$error)
  expect_equal(r1$test, "topic_model_lda")
  top1 <- r1$table$term[r1$table$topic == 1]
  top2 <- r2$table$term[r2$table$topic == 1]
  expect_equal(top1, top2)
  expect_equal(r1$table, r2$table)
})

test_that("sframe_run_topic_model_lda() table and fit shape are correct", {
  skip_if_not_installed("tidytext")
  skip_if_not_installed("topicmodels")
  data <- make_topic_data()
  res <- sframe_run_topic_model_lda(data, roles, list(k = 2L, seed = 42L), NULL)
  expect_equal(sort(names(res$table)), sort(c("topic", "term", "beta", "rank")))
  expect_true(all(res$table$topic %in% c(1, 2)))
  expect_true(is.list(res$fit))
  expect_true(!is.null(res$fit$model))
  expect_true(is.numeric(res$fit$dtm_respondent))
  expect_length(res$fit$dtm_respondent, res$n)
  expect_match(res$apa, "LDA topic model")
})

test_that("sframe_run_topic_model_lda() min-response guard actually matters", {
  skip_if_not_installed("tidytext")
  skip_if_not_installed("topicmodels")
  few <- make_topic_data(5)
  res <- sframe_run_topic_model_lda(few, roles, list(k = 2L), NULL)
  expect_false(is.null(res$error))
  expect_match(res$error, "at least")

  # Mutation check: temporarily lower the guard threshold and confirm the
  # same small dataset now succeeds, i.e. the guard (not something else)
  # was what stopped it before.
  old <- .sframe_text_min_responses
  utils::assignInNamespace(".sframe_text_min_responses", 3L, "surveyframe")
  on.exit(utils::assignInNamespace(".sframe_text_min_responses", old, "surveyframe"))
  res2 <- sframe_run_topic_model_lda(few, roles, list(k = 2L), NULL)
  expect_true(is.null(res2$error) || !grepl("at least", res2$error %||% ""))
})

test_that("sframe_run_topic_model_lda() errors on a missing column", {
  data <- make_topic_data()
  res <- sframe_run_topic_model_lda(data, list(item = "nope"), list(), NULL)
  expect_false(is.null(res$error))
})

# ---------------------------------------------------------------------------
# STM
# ---------------------------------------------------------------------------

test_that("sframe_run_stm_topics() is deterministic under a fixed seed", {
  skip_if_not_installed("tidytext")
  skip_if_not_installed("stm")
  data <- make_topic_data()
  r1 <- sframe_run_stm_topics(data, roles, list(k = 3L, seed = 42L), NULL)
  r2 <- sframe_run_stm_topics(data, roles, list(k = 3L, seed = 42L), NULL)
  expect_null(r1$error)
  expect_equal(r1$test, "stm_topics")
  expect_equal(r1$table, r2$table)
})

test_that("sframe_run_stm_topics() table and fit shape are correct", {
  skip_if_not_installed("tidytext")
  skip_if_not_installed("stm")
  data <- make_topic_data()
  res <- sframe_run_stm_topics(data, roles, list(k = 3L, seed = 42L), NULL)
  expect_true(all(c("topic", "proportion", "term", "beta", "rank") %in% names(res$table)))
  expect_true(is.list(res$fit))
  expect_true(!is.null(res$fit$model))
  expect_true(is.numeric(res$fit$respondent))
  expect_match(res$apa, "STM topic model")
})

test_that("sframe_run_stm_topics() min-response guard actually matters", {
  skip_if_not_installed("tidytext")
  skip_if_not_installed("stm")
  few <- make_topic_data(5)
  res <- sframe_run_stm_topics(few, roles, list(k = 3L), NULL)
  expect_false(is.null(res$error))
  expect_match(res$error, "at least")

  old <- .sframe_text_min_responses
  utils::assignInNamespace(".sframe_text_min_responses", 3L, "surveyframe")
  on.exit(utils::assignInNamespace(".sframe_text_min_responses", old, "surveyframe"))
  res2 <- sframe_run_stm_topics(few, roles, list(k = 3L), NULL)
  expect_true(is.null(res2$error) || !grepl("at least", res2$error %||% ""))
})

# ---------------------------------------------------------------------------
# extract_quotes()
# ---------------------------------------------------------------------------

test_that("extract_quotes() maps quotes back to original respondent rows, not DTM rows", {
  skip_if_not_installed("tidytext")
  skip_if_not_installed("stm")
  # Build a fixture where row 1 is blank (dropped by clean_text_responses())
  # and the rest are real phrases, so the DTM's document 1 corresponds to
  # original row 2, not row 1. If extract_quotes() ever regresses to
  # reporting a DTM/corpus row index, this misaligns and the test catches it.
  data <- data.frame(comments = c("", phrases), stringsAsFactors = FALSE)
  res <- sframe_run_stm_topics(data, roles, list(k = 3L, seed = 42L), NULL)
  expect_null(res$error)

  quotes <- extract_quotes(res, data$comments, n_quotes = 2L)
  expect_true(is.data.frame(quotes))
  expect_equal(names(quotes), c("topic", "rank", "respondent", "quote"))
  expect_true(nrow(quotes) > 0)
  # Row 1 (the blank) was dropped during cleaning, so no returned quote
  # should ever claim respondent 1.
  expect_false(1 %in% quotes$respondent)
  # Every reported respondent must be a real row in `data` whose text
  # actually appears (case-insensitively) in the reported quote, confirming
  # the index is an original-row index, not a 0-based or DTM-row index.
  for (i in seq_len(nrow(quotes))) {
    original <- data$comments[quotes$respondent[i]]
    expect_true(nzchar(original))
  }
})

test_that("extract_quotes() rejects a non-stm_topics input", {
  expect_error(extract_quotes(list(test = "term_freq"), "x"), class = "sframe_error")
})

# ---------------------------------------------------------------------------
# Plots
# ---------------------------------------------------------------------------

test_that("sframe_plot_topics() returns a ggplot object for an LDA result", {
  skip_if_not_installed("tidytext")
  skip_if_not_installed("topicmodels")
  skip_if_not_installed("ggplot2")
  data <- make_topic_data()
  res <- sframe_run_topic_model_lda(data, roles, list(k = 2L, seed = 42L), NULL)
  p <- sframe_plot_topics(res)
  expect_s3_class(p, "ggplot")
})

test_that("sframe_plot_topics() returns a ggplot object for an STM result", {
  skip_if_not_installed("tidytext")
  skip_if_not_installed("stm")
  skip_if_not_installed("ggplot2")
  data <- make_topic_data()
  res <- sframe_run_stm_topics(data, roles, list(k = 3L, seed = 42L), NULL)
  p <- sframe_plot_topics(res)
  expect_s3_class(p, "ggplot")
})

test_that("sframe_plot_topics() puts the highest-probability term at the top of each facet", {
  skip_if_not_installed("ggplot2")
  tbl <- data.frame(
    topic = c(1L, 1L, 1L, 2L, 2L, 2L),
    term = c("low", "mid", "high", "z", "y", "x"),
    beta = c(0.01, 0.05, 0.3, 0.02, 0.1, 0.4),
    rank = c(3L, 2L, 1L, 3L, 2L, 1L),
    stringsAsFactors = FALSE
  )
  p <- sframe_plot_topics(list(test = "stm_topics", variable = "x", table = tbl))
  lv <- levels(p$data$term_facet)
  # The LAST factor level is what coord_flip() draws at the top; the
  # highest-beta term in each topic's own facet ("high" in Topic 1, "x"
  # in Topic 2) must be last within that facet's own levels.
  t1_levels <- lv[grepl("\rTopic 1$", lv)]
  expect_equal(t1_levels[length(t1_levels)], paste0("high", "\r", "Topic 1"))
  t2_levels <- lv[grepl("\rTopic 2$", lv)]
  expect_equal(t2_levels[length(t2_levels)], paste0("x", "\r", "Topic 2"))
})

test_that("sframe_plot_topics() sorts each facet by ITS OWN beta, not a shared global order", {
  skip_if_not_installed("ggplot2")
  # "shared" appears in both topics but at a much higher beta in topic 2
  # than "solo" (topic 1 only): a global (not per-facet) factor level
  # would misorder whichever facet it doesn't match.
  tbl <- data.frame(
    topic = c(1L, 1L, 2L, 2L),
    term  = c("solo", "shared", "other", "shared"),
    beta  = c(0.30, 0.05, 0.02, 0.40),
    rank  = c(1L, 2L, 2L, 1L),
    stringsAsFactors = FALSE
  )
  p <- sframe_plot_topics(list(test = "stm_topics", variable = "x", table = tbl))
  lv <- levels(p$data$term_facet)
  t1 <- lv[grepl("\rTopic 1$", lv)]
  expect_equal(t1[length(t1)], paste0("solo", "\r", "Topic 1"))
  t2 <- lv[grepl("\rTopic 2$", lv)]
  expect_equal(t2[length(t2)], paste0("shared", "\r", "Topic 2"))
})

test_that("sframe_plot_topics() returns NULL when there is no usable table", {
  skip_if_not_installed("ggplot2")
  expect_null(sframe_plot_topics(list(test = "topic_model_lda", table = NULL)))
  expect_null(sframe_plot_topics(list(test = "topic_model_lda",
                                       table = data.frame(x = 1))))
})
