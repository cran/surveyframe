# tests/testthat/test-text-cooccurrence.R
# Pairwise within-response term co-occurrence (todo_0.5.md). Covers
# .sframe_cooccurrence() pair-counting correctness, sframe_run_co_occurrence()'s
# 2 guards (minimum-response floor and zero-edges), and
# sframe_plot_cooccurrence(). This id is not yet wired into
# sframe_run_one_block()'s switch or the plot dispatcher, so the runner and
# plot are exercised directly rather than through run_analysis_plan().

make_cooc_instrument <- function() {
  sf_instrument(
    title = "Co-occurrence fixture",
    version = "1.0.0",
    components = list(
      sf_item("comments", "Any comments about your stay?", type = "textarea")
    )
  )
}

# ---------------------------------------------------------------------------
# .sframe_cooccurrence(): hand-verified fixture
# ---------------------------------------------------------------------------
# 3 short responses, tokens after stop-word removal:
#   r1: "clean room staff"   -> {clean, room, staff}
#   r2: "clean room noise"   -> {clean, room, noise}
#   r3: "staff room"         -> {staff, room}
# Top terms (all 4 appear, top_n large enough to include all): room appears
# in all 3 responses (n=3), clean/staff appear in 2 (n=2 each), noise in 1.
# Pairs by hand:
#   (clean, room): r1, r2                    -> 2
#   (clean, staff): r1                        -> 1
#   (room, staff): r1, r3                     -> 2
#   (clean, noise): r2                        -> 1
#   (room, noise): r2                         -> 1
#   (noise, staff): none                      -> 0 (not returned)
fixture_text <- c(
  "Clean room, staff were great",
  "Clean room, but some noise",
  "Staff and room were fine"
)

test_that(".sframe_cooccurrence pair-counts correctly on a hand-verified fixture", {
  edges <- surveyframe:::.sframe_cooccurrence(fixture_text, top_n = 10L)
  expect_true(all(c("term_a", "term_b", "n") %in% names(edges)))

  get_n <- function(a, b) {
    row <- edges[(edges$term_a == a & edges$term_b == b) |
                   (edges$term_a == b & edges$term_b == a), , drop = FALSE]
    if (nrow(row) == 0) 0L else row$n
  }
  expect_equal(get_n("clean", "room"), 2L)
  expect_equal(get_n("clean", "staff"), 1L)
  expect_equal(get_n("room", "staff"), 2L)
  expect_equal(get_n("clean", "noise"), 1L)
  expect_equal(get_n("room", "noise"), 1L)
  expect_equal(get_n("noise", "staff"), 0L)

  # noise-staff never co-occurs, so it must not appear as a row at all.
  expect_false(any((edges$term_a == "noise" & edges$term_b == "staff") |
                      (edges$term_a == "staff" & edges$term_b == "noise")))
})

test_that(".sframe_cooccurrence orders term_a < term_b alphabetically, no reversed duplicates", {
  edges <- surveyframe:::.sframe_cooccurrence(fixture_text, top_n = 10L)
  expect_true(all(edges$term_a < edges$term_b))
  # No pair should appear with both orderings.
  reversed_keys <- paste(edges$term_b, edges$term_a)
  forward_keys <- paste(edges$term_a, edges$term_b)
  expect_equal(length(intersect(forward_keys, reversed_keys)), 0L)
})

test_that(".sframe_cooccurrence sorts by n descending", {
  edges <- surveyframe:::.sframe_cooccurrence(fixture_text, top_n = 10L)
  expect_true(all(diff(edges$n) <= 0))
})

test_that(".sframe_cooccurrence counts a repeated token within one response only once", {
  # "room" repeats 3 times in one response alongside "staff"; the pair
  # (room, staff) must still count as 1 for this response, not 3.
  txt <- c("room room room staff", "room staff")
  edges <- surveyframe:::.sframe_cooccurrence(txt, top_n = 10L)
  row <- edges[edges$term_a == "room" & edges$term_b == "staff", ]
  expect_equal(row$n, 2L)
})

test_that(".sframe_cooccurrence returns a 0-row table when fewer than 2 top terms exist", {
  edges <- surveyframe:::.sframe_cooccurrence(character(0), top_n = 10L)
  expect_equal(nrow(edges), 0L)
  expect_equal(names(edges), c("term_a", "term_b", "n"))
})

# ---------------------------------------------------------------------------
# sframe_run_co_occurrence()
# ---------------------------------------------------------------------------

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

make_cooc_data <- function(n = length(phrases)) {
  data.frame(comments = phrases[seq_len(n)], stringsAsFactors = FALSE)
}

test_that("sframe_run_co_occurrence runs end to end and reports n_responses/n_pairs", {
  data <- make_cooc_data()
  instr <- make_cooc_instrument()
  res <- surveyframe:::sframe_run_co_occurrence(
    data, roles = list(item = "comments"), options = list(), instrument = instr
  )
  expect_null(res$error)
  expect_true(is.data.frame(res$table))
  expect_true(all(c("term_a", "term_b", "n") %in% names(res$table)))
  expect_match(res$apa, "12 responses")
  expect_match(res$apa, paste0(nrow(res$table), " co-occurring pairs"))
})

test_that("sframe_run_co_occurrence below the minimum-response floor errors rather than computing", {
  data <- make_cooc_data(n = 3)
  instr <- make_cooc_instrument()
  res <- surveyframe:::sframe_run_co_occurrence(
    data, roles = list(item = "comments"), options = list(), instrument = instr
  )
  expect_false(is.null(res$error))
  expect_null(res$table)
})

# Mutation check for the min-response guard: confirm the same 3-response
# input, taken past the runner's guard, genuinely has usable tokens to
# co-occur, i.e. the error above comes from the guard and not from some
# other empty-input failure the guard happens to mask.
test_that("mutation check: the same 3-response input computes once past the floor", {
  cleaned <- clean_text_responses(make_cooc_data(n = 3), "comments")
  edges <- surveyframe:::.sframe_cooccurrence(cleaned, top_n = 20L)
  expect_gt(nrow(edges), 0)
})

test_that("sframe_run_co_occurrence reports the zero-edges error on sparse data", {
  # Every response is a single distinct word: no response has 2 top terms in
  # common, so 0 pairs co-occur even though the response floor is met.
  words <- c("alpha", "bravo", "charlie", "delta", "echo",
             "foxtrot", "golf", "hotel", "india", "juliet")
  data <- data.frame(comments = words, stringsAsFactors = FALSE)
  instr <- make_cooc_instrument()
  res <- surveyframe:::sframe_run_co_occurrence(
    data, roles = list(item = "comments"), options = list(), instrument = instr
  )
  expect_equal(res$error, "No co-occurring term pairs found.")
  expect_null(res$table)
})

# Mutation check for the zero-edges guard: 2-word responses sharing terms
# genuinely produce edges, confirming the error above is the guard firing on
# real zero-pair input and not a bug that would fire regardless.
test_that("mutation check: 2-word overlapping responses do produce edges", {
  data <- data.frame(comments = c("alpha bravo", "alpha bravo"),
                     stringsAsFactors = FALSE)
  cleaned <- clean_text_responses(data, "comments")
  edges <- surveyframe:::.sframe_cooccurrence(cleaned, top_n = 20L)
  expect_gt(nrow(edges), 0)
})

test_that("sframe_run_co_occurrence errors when the item column is missing", {
  data <- make_cooc_data()
  instr <- make_cooc_instrument()
  res <- surveyframe:::sframe_run_co_occurrence(
    data, roles = list(item = "nonexistent"), options = list(), instrument = instr
  )
  expect_false(is.null(res$error))
})

test_that("sframe_run_co_occurrence respects options$top_n", {
  data <- make_cooc_data()
  instr <- make_cooc_instrument()
  res_small <- surveyframe:::sframe_run_co_occurrence(
    data, roles = list(item = "comments"), options = list(top_n = 3L), instrument = instr
  )
  res_big <- surveyframe:::sframe_run_co_occurrence(
    data, roles = list(item = "comments"), options = list(top_n = 20L), instrument = instr
  )
  expect_null(res_small$error)
  expect_null(res_big$error)
  small_terms <- unique(c(res_small$table$term_a, res_small$table$term_b))
  expect_lte(length(small_terms), 3L)
})

# ---------------------------------------------------------------------------
# sframe_plot_cooccurrence()
# ---------------------------------------------------------------------------

test_that("sframe_plot_cooccurrence returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  data <- make_cooc_data()
  instr <- make_cooc_instrument()
  res <- surveyframe:::sframe_run_co_occurrence(
    data, roles = list(item = "comments"), options = list(), instrument = instr
  )
  p <- sframe_plot_cooccurrence(res)
  expect_s3_class(p, "ggplot")
})

test_that("sframe_plot_cooccurrence returns NULL for a table-less result", {
  skip_if_not_installed("ggplot2")
  expect_null(sframe_plot_cooccurrence(list(table = NULL)))
  expect_null(sframe_plot_cooccurrence(list(table = data.frame())))
})

test_that("sframe_plot_cooccurrence's underlying matrix is symmetric before melting", {
  skip_if_not_installed("ggplot2")
  data <- make_cooc_data()
  instr <- make_cooc_instrument()
  res <- surveyframe:::sframe_run_co_occurrence(
    data, roles = list(item = "comments"), options = list(), instrument = instr
  )
  tbl <- res$table
  terms <- sort(unique(c(tbl$term_a, tbl$term_b)))
  mat <- matrix(0, nrow = length(terms), ncol = length(terms),
                dimnames = list(terms, terms))
  for (i in seq_len(nrow(tbl))) {
    mat[tbl$term_a[i], tbl$term_b[i]] <- tbl$n[i]
    mat[tbl$term_b[i], tbl$term_a[i]] <- tbl$n[i]
  }
  expect_equal(mat, t(mat))

  # And the plot itself: pick one pair and confirm both (a, b) and (b, a)
  # tiles exist in the built plot data with equal fill value.
  p <- sframe_plot_cooccurrence(res)
  built <- ggplot2::ggplot_build(p)
  tile_data <- built$data[[1]]
  pair <- tbl[1, ]
  a_idx <- which(terms == pair$term_a)
  b_idx <- which(terms == pair$term_b)
  # ggplot_build encodes x/y as the integer factor level positions; term_b
  # is plotted on a reversed y scale, so recover its level position that way.
  y_levels <- rev(terms)
  fill_ab <- tile_data$fill[tile_data$x == a_idx & tile_data$y == which(y_levels == pair$term_b)]
  fill_ba <- tile_data$fill[tile_data$x == b_idx & tile_data$y == which(y_levels == pair$term_a)]
  expect_equal(fill_ab, fill_ba)
})
