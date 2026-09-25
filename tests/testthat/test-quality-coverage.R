# Batch 2's remaining correctness findings on the quality report.
#
# #8. An expected column absent from the export left both the numerator and
#     the denominator, so a partial export read as more complete than the
#     instrument declares.
# #9. Straight-lining required only 2 answers from a respondent whatever
#     `straightline_min_items` said, so c(3, 3, NA, NA) was flagged in a
#     4-item scale on the strength of 2 answers.

quality_instrument <- function() {
  sf_instrument("Quality", components = list(
    sf_choices("ag5", values = 1:5,
               labels = c("Strongly disagree", "Disagree", "Neutral",
                          "Agree", "Strongly agree")),
    sf_item("sat_1", "Item 1", type = "likert", choice_set = "ag5",
            scale_id = "sat"),
    sf_item("sat_2", "Item 2", type = "likert", choice_set = "ag5",
            scale_id = "sat"),
    sf_item("sat_3", "Item 3", type = "likert", choice_set = "ag5",
            scale_id = "sat"),
    sf_item("sat_4", "Item 4", type = "likert", choice_set = "ag5",
            scale_id = "sat"),
    sf_scale("sat", "Satisfaction",
             items = c("sat_1", "sat_2", "sat_3", "sat_4"))
  ))
}

test_that("8: a column the export left out is reported, not ignored", {
  instr <- quality_instrument()
  # sat_4 never reached the export at all
  data <- data.frame(sat_1 = c(5, 4, 3), sat_2 = c(5, 4, 3),
                     sat_3 = c(5, 4, 3))
  qr <- quality_report(data, instr)

  expect_equal(sf_missing_columns(qr), "sat_4")
  # the respondent rate counts the absent column, so an export missing a
  # quarter of the declaration cannot read as complete
  expect_true(all(qr$missing$respondent_miss > 0))
  expect_equal(unname(qr$missing$item_miss_rate[["sat_4"]]), 1)
})

test_that("8: a complete export reports no absent column", {
  instr <- quality_instrument()
  data <- data.frame(sat_1 = c(5, 4), sat_2 = c(5, 4),
                     sat_3 = c(5, 4), sat_4 = c(5, NA))
  qr <- quality_report(data, instr)
  expect_length(sf_missing_columns(qr), 0)
  expect_equal(unname(qr$missing$respondent_miss), c(0, 0.25))
})

test_that("9: straight-lining needs the declared minimum of answers", {
  instr <- quality_instrument()
  data <- data.frame(
    sat_1 = c(3, 3, 5),
    sat_2 = c(3, 3, 4),
    sat_3 = c(NA, 3, 3),
    sat_4 = c(NA, 3, 2)
  )
  qr <- quality_report(data, instr, straightline_min_items = 4)
  sl <- qr$straightline$sat

  # row 1 answered 2 of 4, which is too little to call inattention
  expect_false(1 %in% sl$flagged_rows)
  # row 2 answered all 4 identically
  expect_true(2 %in% sl$flagged_rows)
  # the report says how many rows it could judge
  expect_equal(sl$n_eligible, 2)
  expect_equal(sl$flag_rate, 0.5)
})

test_that("9: a lower minimum admits the shorter response", {
  instr <- quality_instrument()
  data <- data.frame(sat_1 = c(3, 5), sat_2 = c(3, 4),
                     sat_3 = c(NA, 3), sat_4 = c(NA, 2))
  sl <- quality_report(data, instr, straightline_min_items = 2)$straightline$sat
  expect_true(1 %in% sl$flagged_rows)
  expect_equal(sl$n_eligible, 2)
})
