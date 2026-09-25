# Batch 7 #16 and #17: SurveyBuilder's question list could be worked only with
# a pointer.
#
# #16. A question row was a non-focusable div with a click handler, so there was
#      no keyboard way to select a question for editing. Ordering existed only
#      as drag handlers, with no keyboard move action. Duplicate and delete were
#      reachable; selecting and reordering were not.
# #17. A modal opened and closed by adding and removing a class, with no focus
#      move in either direction, so a keyboard user opening a dialog was left
#      behind it and closing one lost their place.
#
# The builder is a standalone HTML file, so these assert the markup and the
# handlers it ships. The headless-browser pass in the release checklist
# exercises them at a real keyboard.

builder_source <- function() {
  sframe_installed_text("inst", "builder", "survey_builder.html")
}

test_that("16: a question row can be reached and chosen by keyboard", {
  src <- builder_source()
  row <- regmatches(src, regexpr("return pd\\+'<div class=\"ic'.*?';", src))[[1]]
  expect_true(nzchar(row))

  # focusable, and Enter or Space chooses it
  expect_match(row, 'tabindex="0"', fixed = TRUE)
  expect_match(row, "onkeydown=", fixed = TRUE)
  # the chosen row says so, rather than relying on colour alone
  expect_match(row, "aria-selected=", fixed = TRUE)
})

test_that("16: the key handler selects on Enter and Space and moves with arrows", {
  src <- builder_source()
  expect_match(src, "function itemRowKey(", fixed = TRUE)
  handler <- regmatches(src, regexpr("function itemRowKey\\((?s).*?\\n\\}", src,
                                     perl = TRUE))[[1]]
  expect_match(handler, "'Enter'", fixed = TRUE)
  expect_match(handler, "' '", fixed = TRUE)
  # the keyboard route to ordering the review asked for
  expect_match(handler, "moveItemBy", fixed = TRUE)
})

test_that("16: ordering has named buttons, not drag alone", {
  src <- builder_source()
  row <- regmatches(src, regexpr("return pd\\+'<div class=\"ic'.*?';", src))[[1]]
  expect_match(row, 'aria-label="Move question up"', fixed = TRUE)
  expect_match(row, 'aria-label="Move question down"', fixed = TRUE)
  expect_match(row, "moveItemBy(", fixed = TRUE)

  # and the move itself records an undo step, as the drag reorder does
  mover <- regmatches(src, regexpr("function moveItemBy\\((?s).*?\\n\\}", src,
                                   perl = TRUE))[[1]]
  expect_true(nzchar(mover))
  expect_match(mover, "snap()", fixed = TRUE)
  # a move off either end does nothing
  expect_match(mover, "return", fixed = TRUE)
})

test_that("17: a modal takes focus and gives it back", {
  src <- builder_source()
  opener <- regmatches(src, regexpr("function openModal\\((?s).*?\\n\\}", src,
                                    perl = TRUE))[[1]]
  closer <- regmatches(src, regexpr("function closeModal\\((?s).*?\\n\\}", src,
                                    perl = TRUE))[[1]]
  expect_match(opener, "focus()", fixed = TRUE)
  # what had focus is remembered, so closing returns the keyboard user there
  expect_match(opener, "__modalReturn", fixed = TRUE)
  expect_match(closer, "__modalReturn", fixed = TRUE)
  expect_match(closer, "focus()", fixed = TRUE)
})

test_that("16: the list and its rows are a matching pair of roles", {
  src <- builder_source()
  # a listbox of options, which is what aria-required-children wants and what
  # aria-selected belongs to. The file already guarded this for role="list".
  expect_match(src, 'id="itemList" role="listbox"', fixed = TRUE)
  expect_match(src, "el.setAttribute('role','listbox')", fixed = TRUE)
  expect_false(grepl('role="listitem"', src, fixed = TRUE))
  # and the empty state still drops the role, since it holds no options
  expect_match(src, "el.removeAttribute('role')", fixed = TRUE)
})

test_that("the builder's own script parses", {
  skip_if_not_installed("V8")
  # A hand-edited inline script that fails to parse takes the whole builder
  # with it, and no other test would notice. The builder's script is the last
  # plain <script> block: the earlier match is the inlined static survey
  # template, which carries a <script> of its own inside a text/template
  # block.
  src <- builder_source()
  blocks <- regmatches(src, gregexpr("(?s)<script>.*?</script>", src,
                                     perl = TRUE))[[1]]
  body <- sub("(?s)^<script>", "", blocks[[length(blocks)]], perl = TRUE)
  body <- sub("(?s)</script>$", "", body, perl = TRUE)
  expect_gt(nchar(body), 50000)

  ctx <- V8::v8()
  ctx$assign("__src", body)
  expect_true(ctx$get("(function(){ try { new Function(__src); return true; }
                        catch (e) { return false; } })()"))
})
