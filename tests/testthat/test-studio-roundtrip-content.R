# tests/testthat/test-studio-roundtrip-content.R
#
# SurveyStudio rebuilds the instrument from its editor state, and replaces the
# live instrument with that rebuild whenever the draft is valid.
#
# F1. The rebuild set every item's reverse flag to FALSE and rebuilt reversal
#     only from scale reverse_items, so an item declared reverse = TRUE was
#     silently scored as answered, and every composite and alpha built on it
#     changed.
# F2. The rebuild never carried conjoint designs, so a declared design vanished.
# F3. An item held as a plain list lost date limits, comparison items and the
#     comparison scale, which the item constructor supports.
#
# The existing round-trip test only asserted the rebuild was valid, which is how
# all 3 lived. These assert the content survives.

roundtrip_instrument <- function() {
  sf_instrument(title = "Round trip", components = list(
    sf_choices("ag5", 1:5, as.character(1:5)),
    sf_item("s1", "One", "likert", choice_set = "ag5", scale_id = "sat"),
    sf_item("s2", "Two (reversed at item level)", "likert", choice_set = "ag5",
            scale_id = "sat", reverse = TRUE),
    sf_item("s3", "Three (reversed at scale level)", "likert", choice_set = "ag5",
            scale_id = "sat"),
    sf_item("dob", "Date of birth", "date", date_min = "1920-01-01",
            date_max = "2010-12-31"),
    sf_item("pw", "Compare", "pairwise_comparison",
            comparison_items = c("price", "quality"), comparison_scale = "saaty"),
    sf_scale("sat", "Satisfaction", items = c("s1", "s2", "s3"),
             reverse_items = "s3"),
    sf_conjoint_design("dce", attributes = list(price = c("low", "high"),
                                                 speed = c("slow", "fast")),
                       method = "full", n_alternatives = 2L, seed = 7)
  ))
}

rebuilt_through_studio <- function(instrument) {
  path <- tempfile(fileext = ".sframe")
  write_sframe(instrument, path)
  loaded <- read_sframe(path)
  st <- sframe_builder_state_from_instrument(loaded)
  draft <- sframe_builder_validate_draft(
    meta = st$meta, choices = st$choices, items = st$items, scales = st$scales,
    branching = st$branching, checks = st$checks, analysis_plan = st$analysis_plan,
    models = st$models, render = st$render, amendments = st$amendments,
    designs = st$designs, origin = st$origin)
  list(loaded = loaded, draft = draft)
}

test_that("F1: item-level reverse coding survives a Studio rebuild", {
  rt <- rebuilt_through_studio(roundtrip_instrument())
  expect_true(rt$draft$valid)
  items <- rt$draft$instrument$items
  expect_true(isTRUE(items[[which(vapply(items, `[[`, "", "id") == "s2")]]$reverse))

  d <- data.frame(s1 = c(5, 1, 4), s2 = c(1, 5, 2), s3 = c(1, 5, 3))
  expect_equal(score_scales(d, rt$draft$instrument)$sat,
               score_scales(d, rt$loaded)$sat)
})

test_that("F2: a conjoint design survives a Studio rebuild", {
  rt <- rebuilt_through_studio(roundtrip_instrument())
  expect_length(rt$draft$instrument$designs, 1)
  expect_identical(rt$draft$instrument$designs[[1]]$profiles,
                   rt$loaded$designs[[1]]$profiles)
})

test_that("F1, F2, F3: an unedited Studio rebuild keeps the same content", {
  rt <- rebuilt_through_studio(roundtrip_instrument())
  expect_identical(sframe_content_hash(as_sframe(validate_sframe(rt$draft$instrument, strict = TRUE))),
                   sframe_content_hash(rt$loaded))
  expect_null(rt$draft$revision_problem)
})

test_that("F3: a plain-list item keeps date limits and comparison settings", {
  plain <- list(id = "dob", label = "Date of birth", type = "date",
                date_min = "1920-01-01", date_max = "2010-12-31")
  item <- sframe_builder_as_item(plain)
  expect_identical(item$date_min, "1920-01-01")
  expect_identical(item$date_max, "2010-12-31")

  pw <- sframe_builder_as_item(list(id = "pw", label = "Compare",
                                    type = "pairwise_comparison",
                                    comparison_items = c("a", "b"),
                                    comparison_scale = "influence"))
  expect_identical(pw$comparison_items, c("a", "b"))
  expect_identical(pw$comparison_scale, "influence")
})

test_that("F1 and F3 in the item editor: editing an item keeps what the form does not show", {
  # SurveyStudio's item form shows the label, type, required flag, choice set,
  # help and placeholder. Saving it replaced the whole item, so an edit to the
  # wording erased reverse coding, scale membership, page and type settings.
  existing <- sf_item("dob", "Date of birth", "date", required = TRUE,
                      scale_id = "age", reverse = TRUE, page = 2,
                      date_min = "1920-01-01", date_max = "2010-12-31")
  edits <- list(id = "dob", label = "Your date of birth", type = "date",
                required = TRUE, choice_set = NULL, help = "As on your passport",
                placeholder = NULL)
  updated <- sframe_builder_update_item(existing, edits)

  expect_identical(updated$label, "Your date of birth")
  expect_identical(updated$help, "As on your passport")
  expect_true(isTRUE(updated$reverse))
  expect_identical(updated$scale_id, "age")
  expect_identical(as.numeric(updated$page), 2)
  expect_identical(updated$date_min, "1920-01-01")
  expect_identical(updated$date_max, "2010-12-31")
})

test_that("F3 in the item editor: changing the type drops the old type's settings", {
  existing <- sf_item("mx", "Grid", "matrix", choice_set = "ag5",
                      matrix_items = c("a", "b"), scale_id = "s", page = 3)
  edits <- list(id = "mx", label = "Now free text", type = "text",
                required = FALSE, choice_set = NULL, help = NULL, placeholder = "Type here")
  updated <- sframe_builder_update_item(existing, edits)

  expect_identical(updated$type, "text")
  expect_null(updated$matrix_items)
  expect_identical(updated$scale_id, "s")
  expect_identical(as.numeric(updated$page), 3)
  expect_identical(updated$placeholder, "Type here")
})

# Found while chasing a 1-in-25 failure of the test above, and not in the
# review: the builder's projection dropped meta$created_at, so composing an
# instrument back stamped the current time. The two usually landed in the same
# second and the content hash matched; crossing a second boundary showed that
# a Studio rebuild had been changing when the instrument was created, and with
# it the content hash that the amendment boundary rests on.

test_that("a Studio rebuild keeps the instrument's creation time", {
  instr <- roundtrip_instrument()
  instr$meta$created_at <- "2020-01-01T00:00:00Z"

  st <- sframe_builder_state_from_instrument(instr)
  expect_identical(st$meta$created_at, "2020-01-01T00:00:00Z")

  rebuilt <- sframe_builder_compose_instrument(
    meta = st$meta, choices = st$choices, items = st$items, scales = st$scales,
    branching = st$branching, checks = st$checks,
    analysis_plan = st$analysis_plan, models = st$models, render = st$render,
    amendments = st$amendments, designs = st$designs)
  expect_identical(rebuilt$meta$created_at, "2020-01-01T00:00:00Z")
})

test_that("a rebuild's content hash holds across a second boundary", {
  instr <- roundtrip_instrument()
  path <- tempfile(fileext = ".sframe")
  write_sframe(instr, path)
  loaded <- read_sframe(path)

  st <- sframe_builder_state_from_instrument(loaded)
  # the clock moves between loading and composing, as it does in a real
  # session, where the old projection took a fresh timestamp
  Sys.sleep(1.1)
  draft <- sframe_builder_validate_draft(
    meta = st$meta, choices = st$choices, items = st$items, scales = st$scales,
    branching = st$branching, checks = st$checks,
    analysis_plan = st$analysis_plan, models = st$models, render = st$render,
    amendments = st$amendments, designs = st$designs, origin = st$origin)

  expect_identical(
    sframe_content_hash(as_sframe(validate_sframe(draft$instrument,
                                                  strict = TRUE))),
    sframe_content_hash(loaded))
  expect_null(draft$revision_problem)
})

test_that("an instrument built fresh still gets a creation time", {
  new <- sf_instrument("Fresh", components = list(
    sf_item("q1", "One", type = "numeric")))
  expect_match(new$meta$created_at, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T")
})
