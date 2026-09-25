# Batch 4's findings where a figure summarised a different population, or used a
# reference the data cannot be judged against.
#
# #11. The repeated-measures runner drops any respondent missing a measure, and
#      the plot dropped missing values within each measure separately, then
#      carried the test's own APA sentence as its subtitle. A respondent the
#      test excluded could move the plotted medians.
# #12. The raw-variable Q-Q plot drew sample values against standard-normal
#      quantiles and added an unparameterised y = x line, so a normal variable
#      with mean 100 and SD 10 looked wildly non-normal.
# #13. The network runner stores Louvain cluster IDs; the plot renumbered them
#      by size and titled the legend "Cluster", so cluster 2 in the table could
#      appear as Cluster 1 in the figure.

repeated_data <- function() {
  data.frame(
    t1 = c(1, 2, 3, 4, 5, 100),
    t2 = c(2, 3, 4, 5, 6, NA),
    t3 = c(3, 4, 5, 6, 7, 100)
  )
}

test_that("11: the figure uses the cases the test used", {
  skip_if_not_installed("ggplot2")
  d <- repeated_data()
  result <- list(test = "friedman", vars = c("t1", "t2", "t3"),
                 apa = "Friedman test")
  p <- sframe_plot_repeated_measures(result, d, palette = "print")
  skip_if(is.null(p))

  # respondent 6 is missing t2, so the test drops them. The plot used to keep
  # them in t1 and t3, where their value of 100 moves the median.
  complete <- d[stats::complete.cases(d[, c("t1", "t2", "t3")]), ]
  expected <- vapply(c("t1", "t2", "t3"),
                     function(v) stats::median(complete[[v]]), numeric(1))
  expect_equal(p$data$value, unname(expected))

  # and the figure says which respondents it describes
  expect_match(p$labels$caption %||% "", "complete", ignore.case = TRUE)
})

test_that("11: with nothing missing, the figure is unchanged", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(t1 = c(1, 2, 3), t2 = c(2, 3, 4), t3 = c(3, 4, 5))
  result <- list(test = "friedman", vars = c("t1", "t2", "t3"), apa = "x")
  p <- sframe_plot_repeated_measures(result, d, palette = "print")
  skip_if(is.null(p))
  expect_equal(p$data$value, c(2, 3, 4))
})

test_that("12: a raw-variable Q-Q plot uses a fitted reference line", {
  skip_if_not_installed("ggplot2")
  set.seed(42)
  d <- data.frame(score = stats::rnorm(200, mean = 100, sd = 10))
  panels <- sframe_plot_variable_distribution(d, "score", palette = "print")
  skip_if(is.null(panels))

  qq <- panels$qq
  expect_false(is.null(qq))

  # a line through y = x would sit far below data centred on 100, so an
  # ordinary normal variable read as a severe departure from normality. The
  # reference follows the sample's own location and scale.
  layer_classes <- vapply(qq$layers, function(l) class(l$geom)[1], character(1))
  ab <- qq$layers[[which(layer_classes == "GeomAbline")[1]]]
  # absolute differences: expect_equal()'s tolerance is relative, and a
  # relative tolerance wide enough to allow sampling noise here would also
  # accept the y = x line this replaced (intercept 0 against a mean of 100).
  expect_lt(abs(ab$data$intercept - mean(d$score)), 1)
  expect_lt(abs(ab$data$slope - stats::sd(d$score)), 1)
  # and the figure says what the line is
  expect_match(qq$labels$caption, "quartiles", fixed = TRUE)
})

test_that("12: a standardised variable still gets a line near y = x", {
  skip_if_not_installed("ggplot2")
  set.seed(7)
  d <- data.frame(z = stats::rnorm(300))
  qq <- sframe_plot_variable_distribution(d, "z", palette = "print")$qq
  skip_if(is.null(qq))
  ab <- qq$layers[[1]]
  expect_lt(abs(ab$data$intercept - mean(d$z)), 0.1)
  expect_lt(abs(ab$data$slope - stats::sd(d$z)), 0.1)
})

# Batch 4 #13: the network runner stores Louvain cluster IDs, and the plot
# renumbered them by size while titling the legend "Cluster", so the cluster a
# table called 2 could appear as Cluster 1 in the figure.

network_result <- function() {
  tbl <- data.frame(
    term = c("a", "b", "c", "d", "e", "f"),
    frequency = c(9, 8, 7, 3, 2, 1),
    # cluster 2 is the larger one, so a size ranking would call it 1
    cluster = c(2L, 2L, 2L, 1L, 1L, 3L),
    # the runner supplies a layout, which the plot needs
    x = c(0, 1, 0.5, 2, 2.5, 3),
    y = c(0, 0.5, 1, 2, 2.5, 3),
    stringsAsFactors = FALSE)
  list(test = "co_occurrence_network", table = tbl,
       edges = data.frame(term_a = c("a", "d"), term_b = c("b", "e"),
                          n = c(4, 2), stringsAsFactors = FALSE))
}

test_that("13: the figure calls a cluster what the table calls it", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("igraph")
  p <- sframe_plot_cooccurrence_network(network_result(), palette = "print")
  skip_if(is.null(p))

  # the mapping, not just the set of labels: a size ranking would produce the
  # same 3 numbers, so only which term carries which number tells them apart.
  point_data <- NULL
  for (l in p$layers) {
    if (is.data.frame(l$data) && "cluster_label" %in% names(l$data)) {
      point_data <- l$data
      break
    }
  }
  expect_false(is.null(point_data))
  label_of <- stats::setNames(as.character(point_data$cluster_label),
                              point_data$term)
  # a, b and c are cluster 2 in the table, and the largest cluster, so a size
  # ranking would call them 1
  expect_equal(unname(label_of[["a"]]), "2")
  expect_equal(unname(label_of[["d"]]), "1")
  expect_equal(unname(label_of[["f"]]), "3")
})
