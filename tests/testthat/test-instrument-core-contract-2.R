# Batch 1's remaining correctness findings on the instrument core.
#
# #6.  The 15-type enum and the per-type configuration were checked at
#      construction alone, so a mutated instrument revalidated clean.
# #7.  Ambiguous and empty choice sets validated.
# #10. One label dictionary held every choice code, so the same code in two
#      choice sets took the first set's label everywhere.
# #11. A branch value containing a comma split into two values, so a literal
#      choice code could not be matched.
# #12. Check declarations could flag everyone, or overwrite another check.
# #13. sf_id() on a branch returned an empty string, and branch lists were
#      named with empty strings.
# #14. The item and scale tables are summaries, presented as the full record.

base_item <- function(...) sf_item("q1", "How satisfied are you?",
                                   type = "numeric", ...)

instr_of <- function(...) sf_instrument("Core", components = list(...))

test_that("6: a mutated item type and its configuration are revalidated", {
  # the enum, which only the constructor enforced
  instr <- instr_of(base_item())
  instr$items[[1]]$type <- "checkbox"
  expect_false(validate_sframe(instr, strict = FALSE)$valid)

  # a logical flag stored as a string read as FALSE through isTRUE()
  instr2 <- instr_of(base_item())
  instr2$items[[1]]$required <- "TRUE"
  expect_false(validate_sframe(instr2, strict = FALSE)$valid)

  # slider bounds, stored with no order or step check
  bad_slider <- sf_item("s1", "Pick a number", type = "slider",
                        slider_min = 1, slider_max = 10, slider_step = 1)
  bad_slider$slider_min <- 10
  bad_slider$slider_max <- 1
  expect_false(validate_sframe(instr_of(bad_slider), strict = FALSE)$valid)

  zero_step <- sf_item("s2", "Pick a number", type = "slider",
                       slider_min = 1, slider_max = 10, slider_step = 1)
  zero_step$slider_step <- 0
  expect_false(validate_sframe(instr_of(zero_step), strict = FALSE)$valid)

  # a matrix with no rows
  mat <- sf_item("m1", "Rate each", type = "matrix",
                 matrix_items = c("A", "B"), choice_set = "cs")
  mat$matrix_items <- character(0)
  expect_false(validate_sframe(instr_of(
    sf_choices("cs", values = 1:2, labels = c("Low", "High")), mat),
    strict = FALSE)$valid)

  # a choice-type item with no choice set at all
  no_cs <- sf_item("c1", "Pick one", type = "single_choice", choice_set = "cs")
  no_cs$choice_set <- NULL
  expect_false(validate_sframe(instr_of(
    sf_choices("cs", values = 1:2, labels = c("Low", "High")), no_cs),
    strict = FALSE)$valid)

  # an ordinary instrument stays valid
  ok <- instr_of(sf_choices("cs", values = 1:2, labels = c("Low", "High")),
                 sf_item("c1", "Pick one", type = "single_choice",
                         choice_set = "cs"))
  expect_true(validate_sframe(ok, strict = FALSE)$valid)
})

test_that("7: a choice set has to carry usable, unique codes", {
  expect_error(sf_choices("amb", values = c("yes", "yes"),
                          labels = c("Agree", "Disagree")),
               class = "sframe_error")
  expect_error(sf_choices("empty", values = character(0),
                          labels = character(0)),
               class = "sframe_error")

  # and a referenced set mutated into an ambiguous one is a validation problem
  instr <- instr_of(sf_choices("cs", values = 1:2, labels = c("Low", "High")),
                    sf_item("c1", "Pick one", type = "single_choice",
                            choice_set = "cs"))
  instr$choices[[1]]$values <- c("1", "1")
  expect_false(validate_sframe(instr, strict = FALSE)$valid)
})

test_that("10: an ambiguous code keeps its code, and context gives its label", {
  instr <- instr_of(
    sf_choices("agree", values = 1:2, labels = c("Strongly disagree", "Agree")),
    sf_choices("freq",  values = 1:2, labels = c("Daily", "Weekly")),
    sf_item("a1", "Agreement", type = "likert", choice_set = "agree"),
    sf_item("f1", "Frequency", type = "likert", choice_set = "freq")
  )
  lookup <- sframe_label_lookup(instr)
  # "1" and "2" mean 2 different things here, so the shared dictionary leaves
  # them alone. It used to hand every table the first set it read.
  expect_false("1" %in% names(lookup))
  expect_false("2" %in% names(lookup))
  expect_equal(unname(lookup[["a1"]]), "Agreement")

  # with the item named, each code reads in its own wording
  expect_equal(unname(sframe_item_value_labels(instr, "f1")[["1"]]), "Daily")
  expect_equal(unname(sframe_item_value_labels(instr, "a1")[["1"]]),
               "Strongly disagree")

  # an unambiguous code still relabels through the shared dictionary
  plain <- instr_of(
    sf_choices("agree", values = 1:2, labels = c("Strongly disagree", "Agree")),
    sf_item("a1", "Agreement", type = "likert", choice_set = "agree")
  )
  expect_equal(unname(sframe_label_lookup(plain)[["1"]]), "Strongly disagree")
})

test_that("11: a literal comma in a branch value survives", {
  # I() marks the value as literal, so the legacy split leaves it alone
  expect_equal(sframe_branch_in_values(I("Food, drink")), "Food, drink")

  # the legacy comma-separated scalar still splits
  expect_equal(sframe_branch_in_values("a, b"), c("a", "b"))

  # and an explicit vector is untouched
  expect_equal(sframe_branch_in_values(c("Food, drink", "Travel")),
               c("Food, drink", "Travel"))

  # the marker survives the constructor, so a declared rule matches
  b <- sf_branch("q2", depends_on = "q1", operator = "%in%",
                 value = I("Food, drink"))
  expect_equal(sframe_branch_in_values(b$value), "Food, drink")
})

test_that("12: a check has to be usable and uniquely identified", {
  expect_error(sf_check("attn1", type = "attention", item_id = "q5"),
               class = "sframe_error")

  a <- sf_check("attn1", type = "attention", item_id = "q1", pass_values = "3")
  b <- sf_check("attn1", type = "attention", item_id = "q1", pass_values = "4")
  instr <- instr_of(base_item(), a, b)
  expect_false(validate_sframe(instr, strict = FALSE)$valid)
})

test_that("13: a branch has an identity, and lists are named by it", {
  b <- sf_branch("q2", depends_on = "q1", operator = "==", value = "yes")
  expect_true(nzchar(sf_id(b)))

  instr <- instr_of(base_item(), sf_item("q2", "Why?", type = "text"), b)
  branches <- sf_branches(instr)
  expect_true(all(nzchar(names(branches))))
  expect_s3_class(branches[[sf_id(b)]], "sf_branch")

  # one rule per target, since the target is what names the rule
  other <- sf_branch("q2", depends_on = "q1", operator = "==", value = "no")
  expect_false(validate_sframe(
    instr_of(base_item(), sf_item("q2", "Why?", type = "text"), b, other),
    strict = FALSE)$valid)
})

test_that("14: the tables say they are summaries and name the full record", {
  src <- sframe_source_text("R", "as_data_frame.R")
  expect_match(src, "summary", fixed = TRUE)
  expect_match(src, "sf_items()", fixed = TRUE)
})
