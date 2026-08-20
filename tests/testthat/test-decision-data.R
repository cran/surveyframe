# tests/testthat/test-decision-data.R
# The MCDM data contract: column encoding, per-respondent assembly,
# aggregation, collected weights, the rated performance matrix, and the
# options normaliser. Each function is exercised directly against small
# fixtures with hand-computed expectations, not through the runner pipeline.

crits <- c("service", "location", "price", "delivery")
vendors <- c("Alpha", "Basilica", "Coral")

demo_instrument <- function() {
  sf_instrument(
    title = "Hotel supplier selection",
    version = "1.0.0",
    components = list(
      sf_item("crit_pairs", "Compare each pair of criteria",
              type = "pairwise_comparison",
              comparison_items = crits, comparison_scale = "saaty"),
      sf_item("crit_points", "Divide 100 points across the criteria",
              type = "criteria_weight", comparison_items = crits),
      sf_item("crit_influence", "How strongly does each factor influence?",
              type = "pairwise_comparison",
              comparison_items = c("service", "price"),
              comparison_scale = "influence"),
      sf_choices("q5", values = 1:5,
                 labels = c("Very poor", "Poor", "Fair", "Good", "Excellent")),
      sf_item("rate_service", "Rate each supplier: service", type = "matrix",
              matrix_items = vendors, choice_set = "q5"),
      sf_item("rate_price", "Rate each supplier: value", type = "matrix",
              matrix_items = vendors, choice_set = "q5")
    )
  )
}

# ---------------------------------------------------------------------------
# Column encoding
# ---------------------------------------------------------------------------

test_that("saaty items expand to one column per unordered pair", {
  item <- sf_item("p", "Compare", type = "pairwise_comparison",
                  comparison_items = c("a", "b", "c"))
  cols <- sframe_comparison_columns(item)
  expect_equal(length(cols), 3)  # n(n-1)/2 = 3
  expect_equal(cols, c("p__a__vs__b", "p__a__vs__c", "p__b__vs__c"))
})

test_that("influence items expand to one column per ordered pair", {
  item <- sf_item("p", "Influence", type = "pairwise_comparison",
                  comparison_items = c("a", "b", "c"),
                  comparison_scale = "influence")
  cols <- sframe_comparison_columns(item)
  expect_equal(length(cols), 6)  # n(n-1) = 6
  expect_true(all(c("p__a__to__b", "p__b__to__a") %in% cols))
})

test_that("criteria_weight items expand to one column per criterion", {
  item <- sf_item("w", "Allocate", type = "criteria_weight",
                  comparison_items = c("a", "b"))
  expect_equal(sframe_comparison_columns(item), c("w__a", "w__b"))
})

# ---------------------------------------------------------------------------
# Assembly
# ---------------------------------------------------------------------------

test_that("a saaty matrix is reciprocal with a unit diagonal", {
  study <- demo_instrument()
  # A single respondent: service 3x location, service 5x price, service equal
  # to delivery, location 1/2 of price (encoded -2), location 4x delivery,
  # price 2x delivery.
  responses <- data.frame(
    crit_pairs__service__vs__location  = 3,
    crit_pairs__service__vs__price     = 5,
    crit_pairs__service__vs__delivery  = 1,
    crit_pairs__location__vs__price    = -2,
    crit_pairs__location__vs__delivery = 4,
    crit_pairs__price__vs__delivery    = 2
  )
  out <- sframe_assemble_pairwise(responses, study, "crit_pairs")
  expect_equal(out$n_respondents, 1)
  expect_equal(out$n_dropped, 0)
  m <- out$matrices[[1]]
  expect_equal(dim(m), c(4L, 4L))
  expect_equal(dimnames(m), list(crits, crits))
  expect_equal(diag(m), stats::setNames(rep(1, 4), crits))
  expect_equal(m["service", "location"], 3)
  expect_equal(m["location", "service"], 1 / 3)
  # A negative code favours the second side.
  expect_equal(m["location", "price"], 1 / 2)
  expect_equal(m["price", "location"], 2)
  expect_true(all(abs(m * t(m) - 1) < 1e-12))
})

test_that("an influence matrix is directed with a zero diagonal", {
  study <- demo_instrument()
  responses <- data.frame(
    crit_influence__service__to__price = 3,
    crit_influence__price__to__service = 1
  )
  m <- sframe_assemble_pairwise(responses, study, "crit_influence")$matrices[[1]]
  expect_equal(diag(m), stats::setNames(c(0, 0), c("service", "price")))
  expect_equal(m["service", "price"], 3)
  expect_equal(m["price", "service"], 1)
  expect_false(isTRUE(all.equal(m["service", "price"], m["price", "service"])))
})

test_that("incomplete and out-of-range respondents are dropped and counted", {
  study <- demo_instrument()
  responses <- data.frame(
    crit_pairs__service__vs__location  = c(3, NA, 3, 3),
    crit_pairs__service__vs__price     = c(5, 5, 99, 5),
    crit_pairs__service__vs__delivery  = c(1, 1, 1, 1),
    crit_pairs__location__vs__price    = c(-2, -2, -2, -1),
    crit_pairs__location__vs__delivery = c(4, 4, 4, 4),
    crit_pairs__price__vs__delivery    = c(2, 2, 2, 2)
  )
  out <- sframe_assemble_pairwise(responses, study, "crit_pairs")
  expect_equal(out$n_respondents, 1)
  expect_equal(out$n_dropped, 3)
  expect_equal(out$dropped$row, c(2L, 3L, 4L))
  expect_equal(out$dropped$reason,
               c("incomplete", "out of range", "out of range"))
})

test_that("assembly aborts when a pair column is absent", {
  study <- demo_instrument()
  expect_error(
    sframe_assemble_pairwise(data.frame(crit_pairs__service__vs__location = 3),
                             study, "crit_pairs"),
    class = "sframe_import_error"
  )
})

test_that("assembly refuses an item of the wrong type", {
  study <- demo_instrument()
  expect_error(
    sframe_assemble_pairwise(data.frame(x = 1), study, "crit_points"),
    class = "sframe_validation_error"
  )
  expect_error(
    sframe_assemble_pairwise(data.frame(x = 1), study, "nope"),
    class = "sframe_validation_error"
  )
})

# ---------------------------------------------------------------------------
# Aggregation
# ---------------------------------------------------------------------------

test_that("geometric aggregation preserves reciprocity, arithmetic does not", {
  m1 <- matrix(c(1, 1 / 3, 3, 1), 2, 2, dimnames = list(c("a", "b"),
                                                        c("a", "b")))
  m2 <- matrix(c(1, 1 / 5, 5, 1), 2, 2, dimnames = list(c("a", "b"),
                                                        c("a", "b")))
  geo <- sframe_aggregate_judgements(list(m1, m2), "geometric")
  # sqrt(3 * 5) = 3.872983
  expect_equal(geo$matrix["a", "b"], sqrt(15))
  expect_equal(geo$matrix["b", "a"], 1 / sqrt(15))
  expect_true(all(abs(geo$matrix * t(geo$matrix) - 1) < 1e-12))
  expect_equal(geo$n_respondents, 2)

  arith <- sframe_aggregate_judgements(list(m1, m2), "arithmetic")
  expect_equal(arith$matrix["a", "b"], 4)          # (3 + 5) / 2
  expect_equal(arith$matrix["b", "a"], (1 / 3 + 1 / 5) / 2)
  # The arithmetic mean is not reciprocal: 4 * 0.2667 = 1.067, not 1.
  expect_false(abs(arith$matrix["a", "b"] * arith$matrix["b", "a"] - 1) < 1e-6)
})

test_that("aggregation accepts the assembly object and carries drop counts", {
  study <- demo_instrument()
  responses <- data.frame(
    crit_pairs__service__vs__location  = c(3, NA),
    crit_pairs__service__vs__price     = c(5, 5),
    crit_pairs__service__vs__delivery  = c(1, 1),
    crit_pairs__location__vs__price    = c(-2, -2),
    crit_pairs__location__vs__delivery = c(4, 4),
    crit_pairs__price__vs__delivery    = c(2, 2)
  )
  agg <- sframe_aggregate_judgements(
    sframe_assemble_pairwise(responses, study, "crit_pairs")
  )
  expect_equal(agg$n_respondents, 1)
  expect_equal(agg$n_dropped, 1)
  expect_equal(dim(agg$matrix), c(4L, 4L))
})

test_that("aggregation errors on an empty or mismatched set", {
  expect_error(sframe_aggregate_judgements(list()),
               class = "sframe_validation_error")
  expect_error(
    sframe_aggregate_judgements(list(diag(2), diag(3))),
    class = "sframe_validation_error"
  )
})

# ---------------------------------------------------------------------------
# Consistency
# ---------------------------------------------------------------------------

test_that("a perfectly consistent matrix has CR zero", {
  # Built from the priority vector (0.5, 0.3, 0.2): every ratio agrees.
  w <- c(0.5, 0.3, 0.2)
  m <- outer(w, w, "/")
  dimnames(m) <- list(letters[1:3], letters[1:3])
  expect_lt(abs(sframe_consistency_ratio(m)), 1e-8)
  expect_equal(unname(sframe_principal_eigen(m)$weights), w)
})

test_that("an inconsistent matrix exceeds the 0.10 threshold", {
  # The classic intransitive triple: a > b, b > c, but c > a.
  m <- matrix(c(1, 1 / 9, 9,
                9, 1, 1 / 9,
                1 / 9, 9, 1), 3, 3, byrow = TRUE,
              dimnames = list(letters[1:3], letters[1:3]))
  expect_gt(sframe_consistency_ratio(m), 0.10)
})

test_that("the CR distribution is reported and can filter respondents", {
  consistent <- outer(c(0.5, 0.3, 0.2), c(0.5, 0.3, 0.2), "/")
  dimnames(consistent) <- list(letters[1:3], letters[1:3])
  inconsistent <- matrix(c(1, 1 / 9, 9,
                           9, 1, 1 / 9,
                           1 / 9, 9, 1), 3, 3, byrow = TRUE,
                         dimnames = list(letters[1:3], letters[1:3]))

  unfiltered <- sframe_aggregate_judgements(list(consistent, inconsistent))
  expect_equal(length(unfiltered$consistency$cr), 2)
  expect_equal(unfiltered$consistency$share_above, 0.5)
  expect_equal(unfiltered$n_respondents, 2)
  expect_equal(unfiltered$n_dropped_consistency, 0)

  filtered <- sframe_aggregate_judgements(list(consistent, inconsistent),
                                          cr_filter = TRUE)
  expect_equal(filtered$n_respondents, 1)
  expect_equal(filtered$n_dropped_consistency, 1)
  expect_equal(filtered$matrix["a", "b"], consistent[1, 2])

  # Filtering everything out is an error, not a silent empty result.
  expect_error(
    sframe_aggregate_judgements(list(inconsistent), cr_filter = TRUE),
    class = "sframe_validation_error"
  )
})

test_that("no consistency ratio exists beyond the random index table", {
  big <- matrix(1, 11, 11)
  expect_true(is.na(sframe_consistency_ratio(big)))
})

# ---------------------------------------------------------------------------
# Collected weights
# ---------------------------------------------------------------------------

test_that("constant-sum weights are renormalised per respondent", {
  study <- demo_instrument()
  # Respondent 2 allocated only 50 points in total. Without per-respondent
  # renormalisation their judgement would count half as much as respondent 1.
  responses <- data.frame(
    crit_points__service  = c(40, 20),
    crit_points__location = c(30, 15),
    crit_points__price    = c(20, 10),
    crit_points__delivery = c(10, 5)
  )
  out <- sframe_collected_weights(responses, study, "crit_points")
  expect_equal(out$source, "criteria_weight")
  expect_equal(out$n_respondents, 2)
  expect_equal(sum(out$weights), 1)
  # Both respondents allocated in the same proportions, so the mean is those
  # proportions exactly.
  expect_equal(unname(out$weights), c(0.4, 0.3, 0.2, 0.1))
  expect_equal(names(out$weights), crits)
})

test_that("unusable point allocations are dropped and counted", {
  study <- demo_instrument()
  responses <- data.frame(
    crit_points__service  = c(40, NA, 0),
    crit_points__location = c(30, 15, 0),
    crit_points__price    = c(20, 10, 0),
    crit_points__delivery = c(10, 5, 0)
  )
  out <- sframe_collected_weights(responses, study, "crit_points")
  expect_equal(out$n_respondents, 1)
  expect_equal(out$n_dropped, 2)
})

test_that("pairwise weights are the principal eigenvector of the AIJ matrix", {
  study <- demo_instrument()
  # A perfectly consistent set of judgements over the four criteria, built
  # from priorities proportional to (8, 4, 2, 1) so every pairwise ratio is a
  # whole number on the 1-9 scale and the eigenvector is known in advance.
  w <- c(service = 8, location = 4, price = 2, delivery = 1) / 15
  responses <- data.frame(
    crit_pairs__service__vs__location  = w[["service"]] / w[["location"]],
    crit_pairs__service__vs__price     = w[["service"]] / w[["price"]],
    crit_pairs__service__vs__delivery  = w[["service"]] / w[["delivery"]],
    crit_pairs__location__vs__price    = w[["location"]] / w[["price"]],
    crit_pairs__location__vs__delivery = w[["location"]] / w[["delivery"]],
    crit_pairs__price__vs__delivery    = w[["price"]] / w[["delivery"]]
  )
  out <- sframe_collected_weights(responses, study, "crit_pairs")
  expect_equal(out$source, "pairwise")
  expect_equal(unname(out$weights), unname(w), tolerance = 1e-8)
  expect_equal(names(out$weights), crits)
  expect_equal(sum(out$weights), 1)
  expect_lt(out$consistency$max, 1e-8)
})

test_that("an influence item cannot supply criterion weights", {
  study <- demo_instrument()
  responses <- data.frame(
    crit_influence__service__to__price = 3,
    crit_influence__price__to__service = 1
  )
  expect_error(
    sframe_collected_weights(responses, study, "crit_influence"),
    class = "sframe_validation_error"
  )
})

# ---------------------------------------------------------------------------
# Rated performance matrix (path C)
# ---------------------------------------------------------------------------

test_that("the rated matrix aggregates the item__alternative columns", {
  study <- demo_instrument()
  responses <- data.frame(
    rate_service__Alpha    = c(4, 5, 3),
    rate_service__Basilica = c(2, 2, 2),
    rate_service__Coral    = c(5, 5, 5),
    rate_price__Alpha      = c(3, 3, 3),
    rate_price__Basilica   = c(4, 5, 3),
    rate_price__Coral      = c(1, 2, 3)
  )
  out <- sframe_rated_matrix(responses, study,
                             c("rate_service", "rate_price"))
  expect_equal(dim(out$matrix), c(3L, 2L))
  expect_equal(dimnames(out$matrix),
               list(vendors, c("rate_service", "rate_price")))
  expect_equal(out$matrix["Alpha", "rate_service"], 4)     # mean(4, 5, 3)
  expect_equal(out$matrix["Coral", "rate_price"], 2)       # mean(1, 2, 3)
  expect_true(all(out$n == 3))
  expect_equal(out$sd["Basilica", "rate_service"], 0)

  med <- sframe_rated_matrix(responses, study,
                             c("rate_service", "rate_price"),
                             statistic = "median")
  expect_equal(med$matrix["Alpha", "rate_service"], 4)
  expect_equal(med$statistic, "median")
})

test_that("rated criteria must list the same alternatives", {
  study <- sf_instrument(
    title = "Mismatch", components = list(
      sf_choices("q5", values = 1:2, labels = c("No", "Yes")),
      sf_item("a", "A", type = "matrix", matrix_items = c("x", "y"),
              choice_set = "q5"),
      sf_item("b", "B", type = "matrix", matrix_items = c("x", "z"),
              choice_set = "q5")
    )
  )
  expect_error(
    sframe_rated_matrix(data.frame(a__x = 1, a__y = 2, b__x = 1, b__z = 2),
                        study, c("a", "b")),
    class = "sframe_validation_error"
  )
})

test_that("a missing rating column is a named import error", {
  study <- demo_instrument()
  expect_error(
    sframe_rated_matrix(data.frame(rate_service__Alpha = 4), study,
                        "rate_service"),
    "rate_service__Basilica",
    class = "sframe_import_error"
  )
})

# ---------------------------------------------------------------------------
# Options normaliser
# ---------------------------------------------------------------------------

test_that("a list of rows is rebuilt as a labelled matrix", {
  opts <- sframe_decision_options(list(
    matrix = list(c(4.1, 3.0, 210, 36),
                  c(3.6, 4.5, 180, 48),
                  c(4.8, 2.5, 260, 24)),
    alternatives = c("Alpha", "Basilica", "Coral"),
    criteria = crits,
    criteria_types = c("benefit", "benefit", "cost", "cost")
  ))
  expect_true(is.matrix(opts$matrix))
  expect_equal(dim(opts$matrix), c(3L, 4L))
  expect_equal(dimnames(opts$matrix),
               list(c("Alpha", "Basilica", "Coral"), crits))
  expect_equal(opts$matrix["Coral", "price"], 260)
  expect_equal(opts$criteria_types,
               c("benefit", "benefit", "cost", "cost"))
})

test_that("labels are generated when none are declared", {
  opts <- sframe_decision_options(list(matrix = list(c(1, 2), c(3, 4))))
  expect_equal(rownames(opts$matrix), c("A1", "A2"))
  expect_equal(colnames(opts$matrix), c("C1", "C2"))
})

test_that("a data.frame `options$matrix` is not transposed", {
  # A data.frame is also is.list()==TRUE, but its list elements are columns,
  # not rows. Treating it as a list-of-row-vectors (the shape a real list
  # uses) silently transposes the matrix: caught in real use when 5
  # alternatives x 4 criteria came back as a 4 x 5 matrix.
  df <- data.frame(service = c(4.1, 3.6, 4.8), price = c(210, 180, 260))
  opts <- sframe_decision_options(list(
    matrix = df, alternatives = c("Alpha", "Basilica", "Coral"),
    criteria = c("service", "price")
  ))
  expect_equal(dim(opts$matrix), c(3, 2))
  expect_equal(opts$matrix["Coral", "price"], 260)
  expect_equal(opts$matrix["Alpha", "service"], 4.1)
})

test_that("every dimension mismatch is named exactly", {
  m <- list(c(1, 2), c(3, 4), c(5, 6))
  expect_error(
    sframe_decision_options(list(matrix = m, alternatives = c("a", "b"))),
    "names 2 alternative\\(s\\) but `options\\$matrix` has 3 row\\(s\\)",
    class = "sframe_validation_error"
  )
  expect_error(
    sframe_decision_options(list(matrix = m, criteria = c("x", "y", "z"))),
    "names 3 criterion/criteria but `options\\$matrix` has 2 column\\(s\\)",
    class = "sframe_validation_error"
  )
  expect_error(
    sframe_decision_options(list(matrix = m, weights = c(0.5, 0.3, 0.2))),
    "`options\\$weights` has 3 entry/entries but there are 2 criteria",
    class = "sframe_validation_error"
  )
  expect_error(
    sframe_decision_options(list(matrix = m,
                                 criteria_types = c("benefit", "cost",
                                                    "benefit"))),
    "`options\\$criteria_types` has 3 entry/entries but there are 2 criteria",
    class = "sframe_validation_error"
  )
  expect_error(
    sframe_decision_options(list(matrix = m,
                                 criteria_types = c("benefit", "maximise"))),
    "Found: maximise",
    class = "sframe_validation_error"
  )
  expect_error(
    sframe_decision_options(list(matrix = list(c(1, 2), c(3, 4, 5)))),
    "differing lengths",
    class = "sframe_validation_error"
  )
})

test_that("options without a matrix pass through untouched", {
  opts <- sframe_decision_options(list(cr_filter = TRUE))
  expect_true(opts$cr_filter)
  expect_null(opts$matrix)
  expect_equal(length(sframe_decision_options(NULL)), 0)
})
