# Batch 4's findings on the shared text tokeniser and its neighbours.
#
# #3. The tokeniser replaced everything outside ASCII letters, digits,
#     apostrophes and spaces, so "café" became "caf" and "naïve" became two
#     fragments. Text in another script could become empty. The public cleaner
#     keeps alphanumerics, so text surviving cleaning changed again inside
#     term frequency, n-grams, co-occurrence, LDA and sentiment.
# #4. Stop words were removed before the n-gram window slid over what was
#     left, so "clean but not comfortable" produced the bigram
#     "clean comfortable", a phrase no respondent wrote, while the help said
#     an n-gram never straddles a dropped word.
# #6. clean_text_responses() chose its rows before stripping punctuation, so a
#     response of punctuation alone survived as an empty string and counted
#     towards the usable total.
# #7. term_context() coerced `window` with no range check, so 0 made the index
#     sequences run backwards and copied the match into its own context.

test_that("3: a word with an accent survives tokenising", {
  toks <- surveyframe:::.sframe_tokenise("The café was naïve but sehr schön",
                                         stop_words = character(0))[[1]]
  expect_true("café" %in% toks)
  expect_true("naïve" %in% toks)
  expect_true("schön" %in% toks)
  # and no fragment is left behind
  expect_false("caf" %in% toks)
  expect_false("na" %in% toks)
})

test_that("3: text in another script survives", {
  toks <- surveyframe:::.sframe_tokenise("الخدمة جيدة",
                                         stop_words = character(0))[[1]]
  expect_length(toks, 2)
})

test_that("3: punctuation is still separated", {
  toks <- surveyframe:::.sframe_tokenise("Great service, really good!",
                                         stop_words = character(0))[[1]]
  expect_equal(toks, c("great", "service", "really", "good"))
})

test_that("4: an n-gram is built from words that were next to each other", {
  # "but" and "not" are stop words. The phrase respondents wrote is
  # "clean but not comfortable", so there is no adjacent pair of kept words
  # here at all, and the table is empty. It used to report
  # "clean comfortable", a phrase nobody wrote.
  ng <- ngram_frequency("clean but not comfortable", n = 2)
  expect_equal(nrow(ng), 0)
  expect_false("clean comfortable" %in% ng$term)
  # and nothing leaks out of the gap marker
  expect_false(any(grepl("NA", ng$term, fixed = TRUE)))
})

test_that("4: adjacency inside a run of kept words still counts", {
  ng <- ngram_frequency(rep("excellent friendly service", 2), n = 2)
  expect_true("excellent friendly" %in% ng$term)
  expect_true("friendly service" %in% ng$term)
})

test_that("6: a response that cleans away is dropped, with its mapping", {
  d <- data.frame(comments = c("Great service", "!!!", "...", "Very good"),
                  stringsAsFactors = FALSE)
  cleaned <- clean_text_responses(d, "comments")
  expect_length(as.character(cleaned), 2)
  expect_true(all(nzchar(as.character(cleaned))))
  # the respondent map still points at the rows the text came from
  expect_equal(attr(cleaned, "respondent"), c(1L, 4L))
})

test_that("6: a response of numbers alone is dropped when numbers are stripped", {
  d <- data.frame(comments = c("Great service", "12345"),
                  stringsAsFactors = FALSE)
  cleaned <- clean_text_responses(d, "comments", strip_numbers = TRUE)
  expect_length(as.character(cleaned), 1)
  expect_equal(attr(cleaned, "respondent"), 1L)
})

test_that("7: a zero window gives empty context, and a negative one is refused", {
  txt <- structure("the service was very good indeed", respondent = 1L)
  ctx <- term_context(txt, "service", window = 0)
  expect_equal(ctx$match, "service")
  expect_equal(ctx$before, "")
  expect_equal(ctx$after, "")

  expect_error(term_context(txt, "service", window = -1),
               class = "sframe_error")
})

test_that("7: an n-gram size below 2 is refused", {
  expect_error(ngram_frequency("clean and comfortable", n = 1),
               class = "sframe_error")
  expect_error(ngram_frequency("clean and comfortable", n = 0),
               class = "sframe_error")
})

# Batch 4 #5: the grouped text paths subset the original row indices with a
# comparison that returns NA for a respondent with no group value. Subsetting
# by NA yields an NA element, so the row map came out longer than the text it
# labels, and the two could no longer be paired.

# Enough responses per group to clear the minimum-corpus guard, plus one
# respondent with no group value, which is the case that broke alignment.
grouped_data <- function() {
  # The runners need 10 usable responses per group before they report terms.
  north <- c("great friendly service", "friendly helpful staff",
             "service was excellent", "friendly service again",
             "excellent friendly welcome", "helpful service throughout",
             "friendly welcome throughout", "excellent helpful service",
             "service friendly and quick", "quick friendly service",
             "welcome was excellent", "helpful friendly team")
  south <- c("slow and rude", "rude slow service", "waiting far too long",
             "slow service again", "rude staff throughout", "long slow wait",
             "rude and slow throughout", "slow rude service", "long rude wait",
             "waiting slow and rude", "rude service again", "slow long queue")
  data.frame(
    comments = c(north, south, "lovely staff"),
    branch   = c(rep("north", length(north)), rep("south", length(south)), NA),
    stringsAsFactors = FALSE)
}

grouped_roles <- list(item = "comments")
grouped_options <- list(group = "branch")

test_that("5: a respondent with no group value leaves the groups intact", {
  res <- expect_no_error(surveyframe:::sframe_run_term_freq(
    grouped_data(), grouped_roles, grouped_options, NULL))
  expect_setequal(unique(res$table$group), c("north", "south"))
  # real terms came back, so this is no degenerate result
  expect_true(all(!is.na(res$table$term)))
  expect_true("friendly" %in% res$table$term[res$table$group == "north"])
  expect_true("rude" %in% res$table$term[res$table$group == "south"])
  # the ungrouped respondent's words belong to neither group
  expect_false("lovely" %in% res$table$term)
})

test_that("5: grouped sentiment survives a missing group value", {
  skip_if_not_installed("tidytext")
  res <- expect_no_error(surveyframe:::sframe_run_tidy_sentiment(
    grouped_data(), grouped_roles, grouped_options, NULL))
  expect_true(is.data.frame(res$table))
  expect_setequal(unique(res$table$group), c("north", "south"))
  # each group holds 12 responses, and the ungrouped one belongs to neither.
  # It used to be counted into both, as an extra neutral observation.
  totals <- tapply(res$table$n, res$table$group, sum)
  expect_equal(unname(totals[["north"]]), 12)
  expect_equal(unname(totals[["south"]]), 12)
})

# Batch 4 #8: a grouped word cloud summed each group's term table, which the
# runner had already truncated to top_n. A term ranked just below the cutoff in
# every group disappeared even where it led the corpus, and a term that did
# survive lost its counts from the groups where it fell short.

test_that("8: the grouped runner carries overall counts", {
  res <- surveyframe:::sframe_run_term_freq(
    grouped_data(), grouped_roles, c(grouped_options, list(top_n = 1L)), NULL)

  # each group reports its single leading term: "friendly" and "rude"
  expect_equal(nrow(res$table), 2)
  expect_setequal(res$table$term, c("friendly", "rude"))

  # the corpus leader is "service", which appears in both groups and led
  # neither, so summing the truncated group tables would lose it entirely
  expect_true(is.data.frame(res$overall_table))
  expect_equal(res$overall_table$term[[1]], "service")
  expect_false("service" %in% res$table$term)
})

test_that("8: overall counts are the corpus total, not a sum of cutoffs", {
  res_all <- surveyframe:::sframe_run_term_freq(
    grouped_data(), grouped_roles, c(grouped_options, list(top_n = 1L)), NULL)
  # "service" appears in both groups' responses, so its corpus count exceeds
  # either group's leading term, which is the ranking a cloud has to show
  n_service <- res_all$overall_table$n[res_all$overall_table$term == "service"]
  expect_gt(n_service, max(res_all$table$n))
})
