# tests/testthat/test-text-ngram-context.R
# N-gram frequency and keyword-in-context (todo_text_analysis.md), built on the lead
# reference diff in test-text-analysis.R: ngram_frequency(), term_context(),
# sframe_run_ngram_freq(), sframe_run_term_context().

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

# sframe_run_ngram_freq() and sframe_run_term_context() are not yet wired
# into sframe_run_one_block()'s switch (that wiring is added centrally once
# all method ids for this release have landed), so these runners are called
# directly rather than through run_analysis_plan().
run_ngram_freq <- function(data, roles, options = list(), instrument = NULL) {
  surveyframe:::sframe_run_ngram_freq(data, roles, options, instrument)
}
run_term_context <- function(data, roles, options = list(), instrument = NULL) {
  surveyframe:::sframe_run_term_context(data, roles, options, instrument)
}

# ---------------------------------------------------------------------------
# ngram_frequency()
# ---------------------------------------------------------------------------

test_that("ngram_frequency counts bigrams correctly on a tiny hand-verified fixture", {
  txt <- c("clean room clean room", "clean room small room")
  # Tokens per response (no stop words in this fixture):
  #   "clean" "room" "clean" "room"  -> bigrams: clean-room, room-clean, clean-room
  #   "clean" "room" "small" "room"  -> bigrams: clean-room, room-small, small-room
  # Total bigrams: clean room x3, room clean x1, room small x1, small room x1
  tbl <- ngram_frequency(txt, n = 2L, stop_words = character(0))
  expect_equal(tbl$term[1], "clean room")
  expect_equal(tbl$n[1], 3L)
  expect_equal(sum(tbl$n), 6L)
})

test_that("ngram_frequency counts trigrams correctly on a tiny hand-verified fixture", {
  txt <- c("clean room every time", "clean room every visit")
  # Tokens: clean room every time / clean room every visit
  # Trigrams: "clean room every" x2, "room every time" x1, "room every visit" x1
  tbl <- ngram_frequency(txt, n = 3L, stop_words = character(0))
  expect_equal(tbl$term[1], "clean room every")
  expect_equal(tbl$n[1], 2L)
  expect_equal(sum(tbl$n), 4L)
})

test_that("ngram_frequency removes stop words before building n-grams by default", {
  # "the" is a stop word, so it never appears inside a returned bigram, and
  # the surviving tokens close the gap it leaves rather than skip a slot.
  tbl <- ngram_frequency("the clean room and the small room", n = 2L)
  expect_false(any(grepl("\\bthe\\b", tbl$term)))
  expect_true("clean room" %in% tbl$term)
})

test_that("ngram_frequency respects top_n", {
  cleaned <- clean_text_responses(make_text_data(), "comments")
  tbl <- ngram_frequency(cleaned, n = 2L, top_n = 3L)
  expect_lte(nrow(tbl), 3L)
})

test_that("ngram_frequency on empty input returns a 0-row table, not an error", {
  tbl <- ngram_frequency(character(0), n = 2L)
  expect_equal(nrow(tbl), 0L)
  expect_equal(names(tbl), c("term", "n", "pct"))
})

test_that("ngram_frequency on responses shorter than n returns a 0-row table", {
  tbl <- ngram_frequency(c("clean", "room"), n = 3L, stop_words = character(0))
  expect_equal(nrow(tbl), 0L)
})

# ---------------------------------------------------------------------------
# term_context()
# ---------------------------------------------------------------------------

test_that("term_context matches a whole word, case-insensitively", {
  tbl <- term_context(structure("the room was clean and the staff nice",
                                respondent = 7L), "ROOM")
  expect_equal(nrow(tbl), 1L)
  expect_equal(tbl$match[1], "room")
  expect_equal(tbl$respondent[1], 7L)
})

test_that("term_context does not match a substring: 'room' does not match 'roomy'", {
  tbl <- term_context("the roomy felt roomy", "room")
  expect_equal(nrow(tbl), 0L)
})

test_that("term_context truncates before/after to the window size", {
  txt <- "one two three four room five six seven eight"
  tbl <- term_context(txt, "room", window = 2L)
  expect_equal(tbl$before[1], "three four")
  expect_equal(tbl$after[1], "five six")
})

test_that("term_context handles a match at the start or end of a response", {
  tbl_start <- term_context("room was clean today", "room", window = 3L)
  expect_equal(tbl_start$before[1], "")
  expect_equal(tbl_start$after[1], "was clean today")

  tbl_end <- term_context("the staff loved the room", "room", window = 3L)
  expect_equal(tbl_end$after[1], "")
  expect_equal(tbl_end$before[1], "staff loved the")
})

test_that("term_context caps total matches at max_matches across responses", {
  txt <- rep("the room was a lovely room", 10)
  tbl <- term_context(txt, "room", max_matches = 5L)
  expect_equal(nrow(tbl), 5L)
})

test_that("term_context uses the respondent attribute when present, else seq_along", {
  cleaned <- clean_text_responses(make_text_data(), "comments")
  resp_attr <- attr(cleaned, "respondent")
  tbl <- term_context(cleaned, "room", max_matches = 100L)
  expect_true(all(tbl$respondent %in% resp_attr))

  no_attr <- c("the small room was clean", "no match here")
  tbl2 <- term_context(no_attr, "room")
  expect_equal(tbl2$respondent[1], 1L)
})

test_that("term_context errors clearly when term is missing or blank", {
  expect_error(term_context("the room was clean", ""), class = "sframe_error")
  expect_error(term_context("the room was clean", NA_character_), class = "sframe_error")
})

# ---------------------------------------------------------------------------
# sframe_run_ngram_freq() (called directly: not yet wired into
# sframe_run_one_block()'s switch, see the note above)
# ---------------------------------------------------------------------------

test_that("ngram_freq runs end to end and defaults to bigrams", {
  result <- run_ngram_freq(make_text_data(), roles = list(item = "comments"))
  expect_null(result$error)
  expect_true(is.data.frame(result$table))
  expect_true(all(c("term", "n", "pct") %in% names(result$table)))
  expect_true(all(grepl(" ", result$table$term)))
  expect_match(result$apa, "2-gram frequency for comments")
})

test_that("ngram_freq honours options$n for trigrams", {
  result <- run_ngram_freq(make_text_data(), roles = list(item = "comments"),
                           options = list(n = 3L))
  expect_null(result$error)
  # A trigram term has exactly 2 internal spaces.
  if (nrow(result$table) > 0) {
    spaces <- lengths(regmatches(result$table$term,
                                 gregexpr(" ", result$table$term)))
    expect_equal(spaces, rep(2L, nrow(result$table)))
  }
})

test_that("ngram_freq below the minimum-response floor errors rather than computing", {
  data <- make_text_data(n = 3)
  result <- run_ngram_freq(data, roles = list(item = "comments"))
  expect_false(is.null(result$error))
  expect_null(result$table)
})

test_that("mutation check: lowering the min-response floor lets ngram_freq compute below it", {
  ns <- asNamespace("surveyframe")
  old <- get(".sframe_text_min_responses", envir = ns)
  unlockBinding(".sframe_text_min_responses", ns)
  assign(".sframe_text_min_responses", 1L, envir = ns)
  lockBinding(".sframe_text_min_responses", ns)
  on.exit({
    unlockBinding(".sframe_text_min_responses", ns)
    assign(".sframe_text_min_responses", old, envir = ns)
    lockBinding(".sframe_text_min_responses", ns)
  }, add = TRUE)

  data <- make_text_data(n = 3)
  result <- run_ngram_freq(data, roles = list(item = "comments"))
  expect_null(result$error)
  expect_true(is.data.frame(result$table))
})

# ---------------------------------------------------------------------------
# sframe_run_term_context() (called directly, see the note above)
# ---------------------------------------------------------------------------

test_that("term_context runs end to end through the runner", {
  result <- run_term_context(make_text_data(),
                             roles = list(item = "comments", term = "room"))
  expect_null(result$error)
  expect_true(is.data.frame(result$table))
  expect_true(all(c("respondent", "before", "match", "after") %in% names(result$table)))
  expect_match(result$apa, "room")
})

test_that("term_context errors clearly when no keyword is supplied", {
  result <- run_term_context(make_text_data(), roles = list(item = "comments"))
  expect_false(is.null(result$error))
  expect_match(result$error, "keyword")
})

test_that("term_context accepts options$term as a fallback for roles$term", {
  result <- run_term_context(make_text_data(), roles = list(item = "comments"),
                             options = list(term = "staff"))
  expect_null(result$error)
  expect_true(nrow(result$table) > 0)
})

test_that("term_context below the minimum-response floor errors rather than computing", {
  data <- make_text_data(n = 3)
  result <- run_term_context(data, roles = list(item = "comments", term = "room"))
  expect_false(is.null(result$error))
  expect_null(result$table)
})

test_that("mutation check: lowering the min-response floor lets term_context compute below it", {
  ns <- asNamespace("surveyframe")
  old <- get(".sframe_text_min_responses", envir = ns)
  unlockBinding(".sframe_text_min_responses", ns)
  assign(".sframe_text_min_responses", 1L, envir = ns)
  lockBinding(".sframe_text_min_responses", ns)
  on.exit({
    unlockBinding(".sframe_text_min_responses", ns)
    assign(".sframe_text_min_responses", old, envir = ns)
    lockBinding(".sframe_text_min_responses", ns)
  }, add = TRUE)

  data <- make_text_data(n = 3)
  result <- run_term_context(data, roles = list(item = "comments", term = "room"))
  expect_null(result$error)
  expect_true(is.data.frame(result$table))
})
