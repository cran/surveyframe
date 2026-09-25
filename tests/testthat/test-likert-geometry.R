# Batch 4 #9: the grouped diverging chart began its negative stack at zero and
# then drew the neutral segment across zero, so the two overlapped by half the
# neutral width. With 5 equally frequent categories the negatives occupied
# -40 to 0 and neutral occupied -10 to 10: 10 percentage points of the bar were
# drawn twice, and the bar was 10 points too short. The single-item chart had
# the geometry right, and both the matrix and scale charts use this builder.

# The segment frame the grouped builder computes, without drawing anything.
grouped_segments <- function(counts_by_row, labels) {
  skip_if_not_installed("ggplot2")
  p <- surveyframe:::.sframe_likert_grouped_plot(
    counts_by_row, scale_values = seq_along(labels), scale_labels = labels,
    title = "t", palette = "print")
  skip_if(is.null(p))
  p$data
}

five <- c("Strongly disagree", "Disagree", "Neutral", "Agree",
          "Strongly agree")

test_that("9: segments meet without overlapping, and span the full 100", {
  # 5 categories, 20 responses each: every segment is 20 percentage points
  segs <- grouped_segments(list(q1 = c(20, 20, 20, 20, 20)), five)
  segs <- segs[order(segs$xmin), ]

  # each segment starts where the last one ended
  expect_equal(segs$xmin[-1], segs$xmax[-nrow(segs)])
  # and the whole bar is 100 points wide
  expect_equal(sum(segs$xmax - segs$xmin), 100)
  # with the neutral segment straddling zero evenly
  neu <- segs[segs$category == "Neutral", ]
  expect_equal(neu$xmin, -10)
  expect_equal(neu$xmax, 10)
  # the negatives end where neutral begins, so the bar reaches half the
  # neutral width beyond the 40 points of negative opinion
  expect_equal(min(segs$xmin), -50)
  expect_equal(max(segs$xmax), 50)
})

test_that("9: a lopsided row keeps the same rule", {
  segs <- grouped_segments(list(q1 = c(50, 10, 20, 10, 10)), five)
  segs <- segs[order(segs$xmin), ]
  expect_equal(segs$xmin[-1], segs$xmax[-nrow(segs)])
  expect_equal(sum(segs$xmax - segs$xmin), 100)
  neu <- segs[segs$category == "Neutral", ]
  expect_equal(neu$xmin, -10)
  expect_equal(neu$xmax, 10)
})

test_that("9: an even-length scale has no neutral segment to straddle", {
  four <- c("Strongly disagree", "Disagree", "Agree", "Strongly agree")
  segs <- grouped_segments(list(q1 = c(25, 25, 25, 25)), four)
  segs <- segs[order(segs$xmin), ]
  expect_equal(segs$xmin[-1], segs$xmax[-nrow(segs)])
  expect_equal(sum(segs$xmax - segs$xmin), 100)
  # the split falls exactly on zero
  expect_true(0 %in% c(segs$xmin, segs$xmax))
})

test_that("9: every row shares the zero line", {
  segs <- grouped_segments(
    list(q1 = c(20, 20, 20, 20, 20), q2 = c(60, 20, 10, 5, 5)), five)
  for (r in unique(segs$row)) {
    one <- segs[segs$row == r, ]
    one <- one[order(one$xmin), ]
    expect_equal(one$xmin[-1], one$xmax[-nrow(one)])
    expect_equal(sum(one$xmax - one$xmin), 100)
  }
})
