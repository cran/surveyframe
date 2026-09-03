# tests/testthat/test-text-cooccurrence-network.R
# Co-occurrence network (todo_text_analysis.md, method id `co_occurrence_network`):
# .sframe_cooccurrence_edges() edge-list correctness, the minimum-content
# guard (5 distinct terms, 1 edge), Louvain/layout seed determinism, the
# modularity score, .sframe_cluster_palette()'s 8-cluster boundary, and
# sframe_plot_cooccurrence_network(). Written by the lead reviewing this
# id closely (the most expensive/highest-risk item in the release, per
# todo_text_analysis.md's delegation section) since the building agent stalled
# before writing its own test file; every claim here was verified against
# the actual runner output first (see the manual determinism check in the
# integration session), not written on trust.

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

make_network_data <- function(n = length(phrases)) {
  data.frame(comments = phrases[seq_len(n)], stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# .sframe_cooccurrence_edges()
# ---------------------------------------------------------------------------

test_that(".sframe_cooccurrence_edges counts pairs correctly on a hand-verified fixture", {
  # 3 short, controlled responses. Top terms (top_n = 10, no stop words to
  # strip beyond the built-in list): "cat", "dog", "bird" are the only
  # content words. Response 1: cat+dog co-occur once. Response 2: dog+bird
  # co-occur once. Response 3: cat+dog+bird all co-occur (3 pairs, 1 each).
  text <- c("cat and dog", "dog and bird", "cat dog and bird")
  built <- surveyframe:::.sframe_cooccurrence_edges(text, top_n = 10L)
  edges <- built$edges
  # Expected pairs, alphabetically ordered (term_a < term_b): bird-cat n=1
  # (response 3 only), bird-dog n=2 (responses 2 and 3), cat-dog n=2
  # (responses 1 and 3).
  edges <- edges[order(edges$term_a, edges$term_b), ]
  expect_equal(edges$term_a, c("bird", "bird", "cat"))
  expect_equal(edges$term_b, c("cat", "dog", "dog"))
  expect_equal(edges$n, c(1L, 2L, 2L))
})

test_that(".sframe_cooccurrence_edges never emits a reversed duplicate pair", {
  built <- surveyframe:::.sframe_cooccurrence_edges(phrases, top_n = 20L)
  edges <- built$edges
  expect_true(all(edges$term_a < edges$term_b))
  pair_keys <- paste(edges$term_a, edges$term_b)
  expect_equal(length(pair_keys), length(unique(pair_keys)))
})

test_that(".sframe_cooccurrence_edges returns 0 rows, not an error, for text with no co-occurring pairs", {
  built <- surveyframe:::.sframe_cooccurrence_edges(character(0), top_n = 10L)
  expect_equal(nrow(built$edges), 0L)
})

# ---------------------------------------------------------------------------
# sframe_run_cooccurrence_network(): guards
# ---------------------------------------------------------------------------

test_that("the minimum-response guard rejects data below the response floor", {
  data <- make_network_data(n = 3)
  r <- surveyframe:::sframe_run_cooccurrence_network(data, list(item = "comments"), list(), NULL)
  expect_false(is.null(r$error))
  expect_match(r$error, "usable responses")
})

test_that("mutation check: the response-floor guard actually gates (not vacuously true)", {
  ns <- asNamespace("surveyframe")
  old <- ns$.sframe_text_min_responses
  unlockBinding(".sframe_text_min_responses", ns)
  ns$.sframe_text_min_responses <- 1L
  on.exit({
    unlockBinding(".sframe_text_min_responses", ns)
    ns$.sframe_text_min_responses <- old
    lockBinding(".sframe_text_min_responses", ns)
  })
  data <- make_network_data(n = 3)
  r <- surveyframe:::sframe_run_cooccurrence_network(data, list(item = "comments"), list(), NULL)
  # With the floor lowered to 1, this now clears the response guard; it may
  # still fail the separate minimum-*content* guard below (fewer terms in
  # only 3 responses), but must not fail with the response-count message.
  error_msg <- r$error %||% ""
  expect_false(grepl("usable responses", error_msg))
})

test_that("the minimum-content guard rejects a graph with too few distinct terms", {
  # 12 identical single-word responses: only 1 distinct term, so it clears
  # the response floor but has no co-occurrence structure at all.
  data <- data.frame(comments = rep("hello", 12), stringsAsFactors = FALSE)
  r <- surveyframe:::sframe_run_cooccurrence_network(data, list(item = "comments"), list(), NULL)
  expect_false(is.null(r$error))
  expect_match(r$error, "distinct terms")
})

test_that("mutation check: the minimum-content guard actually gates", {
  ns <- asNamespace("surveyframe")
  old <- ns$.sframe_cooccurrence_min_terms
  unlockBinding(".sframe_cooccurrence_min_terms", ns)
  ns$.sframe_cooccurrence_min_terms <- 1L
  on.exit({
    unlockBinding(".sframe_cooccurrence_min_terms", ns)
    ns$.sframe_cooccurrence_min_terms <- old
    lockBinding(".sframe_cooccurrence_min_terms", ns)
  })
  data <- data.frame(comments = rep("hello world", 12), stringsAsFactors = FALSE)
  r <- surveyframe:::sframe_run_cooccurrence_network(data, list(item = "comments"), list(), NULL)
  expect_null(r$error)
})

# ---------------------------------------------------------------------------
# sframe_run_cooccurrence_network(): seed determinism (the critical property)
# ---------------------------------------------------------------------------

test_that("the same seed produces an identical table across repeated runs", {
  skip_if_not_installed("igraph")
  data <- make_network_data()
  r1 <- surveyframe:::sframe_run_cooccurrence_network(data, list(item = "comments"), list(seed = 42L), NULL)
  r2 <- surveyframe:::sframe_run_cooccurrence_network(data, list(item = "comments"), list(seed = 42L), NULL)
  expect_null(r1$error)
  expect_identical(r1$table, r2$table)
})

test_that("a different seed can produce a different result (the seed argument is actually used)", {
  skip_if_not_installed("igraph")
  data <- make_network_data()
  r1 <- surveyframe:::sframe_run_cooccurrence_network(data, list(item = "comments"), list(seed = 42L), NULL)
  r3 <- surveyframe:::sframe_run_cooccurrence_network(data, list(item = "comments"), list(seed = 7L), NULL)
  expect_null(r3$error)
  # Not asserting the partitions/layouts MUST differ (Louvain can
  # legitimately converge the same way from different seeds on an easy
  # graph) -- only that this particular fixture, empirically, does differ,
  # which confirms the seed is actually threaded through rather than
  # silently ignored.
  expect_false(isTRUE(all.equal(r1$table, r3$table)))
})

test_that("the resulting table and apa carry the expected node/edge/cluster/modularity shape", {
  skip_if_not_installed("igraph")
  data <- make_network_data()
  r <- surveyframe:::sframe_run_cooccurrence_network(data, list(item = "comments"), list(seed = 42L), NULL)
  expect_null(r$error)
  expect_true(all(c("term", "frequency", "cluster", "x", "y") %in% names(r$table)))
  expect_true(all(c("term_a", "term_b", "n") %in% names(r$edges)))
  expect_match(r$apa, "terms")
  expect_match(r$apa, "edges")
  expect_match(r$apa, "clusters")
  expect_match(r$apa, "modularity")
  # igraph::modularity() on a Louvain partition of a connected weighted
  # graph is bounded in [-1, 1] in general and typically positive for real
  # community structure; assert the documented general bound rather than a
  # narrower range this specific fixture happens to hit.
  mod <- as.numeric(sub(".*modularity = ([0-9.-]+)\\).*", "\\1", r$apa))
  expect_true(mod >= -1 && mod <= 1)
})

test_that("the igraph graph/community objects are not attached to the result", {
  skip_if_not_installed("igraph")
  data <- make_network_data()
  r <- surveyframe:::sframe_run_cooccurrence_network(data, list(item = "comments"), list(seed = 42L), NULL)
  expect_null(r$fit)
  expect_false(any(vapply(r, function(x) inherits(x, "igraph"), logical(1))))
})

# ---------------------------------------------------------------------------
# .sframe_cluster_palette()
# ---------------------------------------------------------------------------

test_that("the cluster palette has no 'Other' bucket at exactly 8 clusters", {
  pal <- surveyframe:::.sframe_cluster_palette(8, "web")
  expect_equal(length(pal), 8L)
  expect_false("Other" %in% names(pal))
})

test_that("the cluster palette adds an explicit 'Other' bucket, not a 9th hue, past 8 clusters", {
  pal8 <- surveyframe:::.sframe_cluster_palette(8, "web")
  pal9 <- surveyframe:::.sframe_cluster_palette(9, "web")
  expect_equal(length(pal9), 9L)
  expect_true("Other" %in% names(pal9))
  # The first 8 colours must be identical to the 8-cluster palette, not a
  # re-interpolated ramp across 9 slots (that would be the hue-cycling the
  # brief explicitly rules out).
  expect_equal(unname(pal9[1:8]), unname(pal8))
})

# ---------------------------------------------------------------------------
# sframe_plot_cooccurrence_network()
# ---------------------------------------------------------------------------

test_that("sframe_plot_cooccurrence_network returns a ggplot object", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggplot2")
  data <- make_network_data()
  r <- surveyframe:::sframe_run_cooccurrence_network(data, list(item = "comments"), list(seed = 42L), NULL)
  p <- sframe_plot_cooccurrence_network(r)
  expect_s3_class(p, "ggplot")
})

test_that("sframe_plot_cooccurrence_network does not error on a synthetic >8-cluster fixture", {
  skip_if_not_installed("ggplot2")
  synthetic <- list(
    test = "co_occurrence_network", variable = "comments",
    table = data.frame(
      term = paste0("t", 1:12),
      frequency = sample(3:20, 12),
      cluster = 1:12,
      x = rnorm(12), y = rnorm(12),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(term_a = paste0("t", 1:11), term_b = paste0("t", 2:12),
                       n = sample(1:5, 11, replace = TRUE),
                       stringsAsFactors = FALSE)
  )
  p <- sframe_plot_cooccurrence_network(synthetic)
  expect_s3_class(p, "ggplot")
})

# ---------------------------------------------------------------------------
# End to end through run_analysis_plan()
# ---------------------------------------------------------------------------

test_that("co_occurrence_network runs end to end through run_analysis_plan", {
  skip_if_not_installed("igraph")
  instr <- sf_instrument(
    title = "Network fixture", version = "1.0.0",
    components = list(sf_item("comments", "Any comments?", type = "textarea"))
  )
  instr$analysis_plan <- list(list(
    id = "rq1", research_question = "What themes cluster together?",
    roles = list(item = "comments"), options = list(seed = 42L),
    test = "co_occurrence_network", alpha = 0.05, citations = character(0),
    interpretation = "", result = NULL
  ))
  res <- run_analysis_plan(make_network_data(), instr)
  expect_null(res[[1]]$error)
  expect_true(is.data.frame(res[[1]]$table))
})
