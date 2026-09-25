# Batch 4 #31: the APA sentences carry avoidable number-format defects.
#
# APA 7 asks for no leading zero on a statistic bounded by 1, and for an
# interval to say what confidence level it is. sframe_p_string() printed
# "= 0.032" with the leading zero APA forbids for p, and sframe_ci_string()
# emitted a bare bracketed pair with no CI label.
#
# The italics APA also asks for are manuscript styling, which a character
# vector cannot carry; that half is recorded as deferred and the accessor's
# help says what a researcher has to add.

test_that("31: p has no leading zero, and stays bounded", {
  expect_equal(sframe_p_string(0.032), "= .032")
  expect_equal(sframe_p_string(0.5), "= .500")
  expect_equal(sframe_p_string(0.916), "= .916")
  # the small-p guard is unchanged, and the review calls it sound
  expect_equal(sframe_p_string(0.0001), "< .001")
  expect_equal(sframe_p_string(NA_real_), "= NA")
  # 1 is a legal p, and has no leading zero to drop
  expect_equal(sframe_p_string(1), "= 1.000")
})

test_that("31: an interval says what level it is", {
  ci <- c(lower = 0.12, upper = 0.48)
  out <- sframe_ci_string(ci)
  expect_match(out, "95% CI", fixed = TRUE)
  expect_match(out, "[0.12, 0.48]", fixed = TRUE)
})

test_that("31: an interval that could not be computed still says why", {
  ci <- structure(c(lower = NA_real_, upper = NA_real_),
                  reason = "every resample was tied")
  expect_match(sframe_ci_string(ci), "every resample was tied", fixed = TRUE)
  expect_equal(sframe_ci_string(NULL), "")
})

test_that("31: the accessor's help says the sentence is plain text", {
  src <- sframe_source_text("R", "accessors.R")
  expect_match(src, "italic", fixed = TRUE)
})
