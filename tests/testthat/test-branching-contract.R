# Batch 4's findings on branching, where the static survey and the Shiny
# survey disagreed with each other and with the declaration.
#
# #19. The static survey stores a multi-select answer comma-joined and compared
#      the whole string against the allowed values, so selecting a and b failed
#      a rule allowing either.
# #20. The Shiny lookup held one rule per target item, so a second rule on the
#      same item replaced the first, and visibility read only that rule's own
#      answer, so a hidden controlling item still gated on what it held. The
#      static evaluator already combined rules with AND and cascaded.
# #21. Conversational Shiny navigation walked every answerable item with no
#      visibility check, so a participant could be required to answer a
#      question the branching excludes, which serialisation then blanked.

multi_instrument <- function() {
  sf_instrument("Branching", components = list(
    sf_choices("food", values = c("a", "b", "c"),
               labels = c("Apples", "Bread", "Cheese")),
    sf_item("bought", "What did you buy?", type = "multiple_choice",
            choice_set = "food"),
    sf_item("why", "Why those?", type = "text"),
    sf_branch("why", depends_on = "bought", operator = "%in%",
              value = c("a", "b"))
  ))
}

# ── #19: the static evaluator ────────────────────────────────────────────

test_that("19: a multi-select answer matches a rule on any of its values", {
  skip_if_not_installed("V8")
  ctx <- static_survey_context(multi_instrument())

  ctx$eval("responses['bought'] = 'a';")
  expect_true(ctx$get("isVisible('why')"))

  # 2 selected, either of which the rule allows. This is the defect: the
  # comma-joined string was compared whole, so "a,b" matched nothing.
  ctx$eval("responses['bought'] = 'a,b';")
  expect_true(ctx$get("isVisible('why')"))

  # a selection the rule allows, alongside one it does not
  ctx$eval("responses['bought'] = 'b,c';")
  expect_true(ctx$get("isVisible('why')"))

  # nothing the rule allows
  ctx$eval("responses['bought'] = 'c';")
  expect_false(ctx$get("isVisible('why')"))

  ctx$eval("responses['bought'] = '';")
  expect_false(ctx$get("isVisible('why')"))
})

# ── #20: one contract across both evaluators ─────────────────────────────

two_rule_instrument <- function() {
  sf_instrument("Compound", components = list(
    sf_item("age", "Age", type = "numeric"),
    sf_item("country", "Country", type = "text"),
    sf_item("detail", "Tell us more", type = "text"),
    sf_branch("detail", depends_on = "age", operator = ">=", value = 18),
    sf_branch("detail", depends_on = "country", operator = "==", value = "UK")
  ))
}

test_that("20: every rule on an item is kept, and they combine with AND", {
  instr <- two_rule_instrument()
  lookup <- sframe_branch_lookup(instr)
  # both rules survive, where the second used to replace the first
  expect_length(lookup[["detail"]], 2)

  visible <- function(vals) {
    sframe_item_visible(instr$items[[3]], vals, lookup)
  }
  expect_true(visible(list(age = 20, country = "UK")))
  expect_false(visible(list(age = 20, country = "FR")))
  expect_false(visible(list(age = 12, country = "UK")))
  expect_false(visible(list(age = NULL, country = "UK")))
})

test_that("20: a hidden controlling item counts as unanswered", {
  instr <- sf_instrument("Cascade", components = list(
    sf_item("own", "Do you own a car?", type = "text"),
    sf_item("make", "Which make?", type = "text"),
    sf_item("year", "Which year?", type = "text"),
    sf_branch("make", depends_on = "own", operator = "==", value = "yes"),
    sf_branch("year", depends_on = "make", operator = "==", value = "Ford")
  ))
  lookup <- sframe_branch_lookup(instr)
  by_id <- function(id) Filter(function(i) i$id == id, instr$items)[[1]]

  # own = yes: make shows, and year follows the answer to make
  vals <- list(own = "yes", make = "Ford")
  expect_true(sframe_item_visible(by_id("make"), vals, lookup))
  expect_true(sframe_item_visible(by_id("year"), vals, lookup))

  # own = no: make is hidden, so its stale answer cannot reveal year
  stale <- list(own = "no", make = "Ford")
  expect_false(sframe_item_visible(by_id("make"), stale, lookup))
  expect_false(sframe_item_visible(by_id("year"), stale, lookup))
})

test_that("20: a rule cycle terminates", {
  instr <- sf_instrument("Cycle", components = list(
    sf_item("a", "A", type = "text"),
    sf_item("b", "B", type = "text"),
    sf_branch("a", depends_on = "b", operator = "==", value = "x"),
    sf_branch("b", depends_on = "a", operator = "==", value = "x")
  ))
  lookup <- sframe_branch_lookup(instr)
  expect_no_error(sframe_item_visible(instr$items[[1]],
                                      list(a = "x", b = "x"), lookup))
})

# ── #21: conversational navigation respects visibility ───────────────────

test_that("21: the conversational sequence leaves out hidden questions", {
  instr <- multi_instrument()
  lookup <- sframe_branch_lookup(instr)

  # nothing bought yet, so the follow-up is out of the sequence
  seq_hidden <- sframe_visible_sequence(instr, list(bought = NULL), lookup)
  expect_equal(vapply(seq_hidden, function(i) i$id, character(1)), "bought")

  # an answer the rule allows brings it in
  seq_shown <- sframe_visible_sequence(instr, list(bought = c("a", "b")),
                                       lookup)
  expect_equal(vapply(seq_shown, function(i) i$id, character(1)),
               c("bought", "why"))
})
