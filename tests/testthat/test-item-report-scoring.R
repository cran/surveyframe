# tests/testthat/test-item-report-scoring.R
#
# item_report() diagnostics, batch 2 findings 17, 18 and 19.
#
# 17. Item-rest correlations used raw item columns, while reliability_report()
#     reversed declared items first. A reverse-keyed item in a coherent scale
#     came out strongly negative beside a high alpha.
# 18. The rest score was a row sum with na.rm = TRUE, so a missing rest item
#     counted as 0 and the respondent stayed in the correlation. Diagnostics
#     now use respondents who answered every item in the scale, the same rows
#     reliability_report() uses.
# 19. Floor and ceiling were the sample's own minimum and maximum. On a 1 to 7
#     item answered only 3, 4 and 5, answers of 3 and 5 were called floor and
#     ceiling. They now use the declared bounds, and are NA without them.

skip_if_not_installed("psych")

seven <- function() sf_choices("ag7", 1:7, as.character(1:7))

keyed_instrument <- function() {
  sf_instrument(title = "Keyed", components = list(
    seven(),
    sf_item("k1", "One", "likert", choice_set = "ag7", scale_id = "k"),
    sf_item("k2", "Two", "likert", choice_set = "ag7", scale_id = "k"),
    sf_item("k3", "Three (reversed)", "likert", choice_set = "ag7",
            scale_id = "k", reverse = TRUE),
    sf_scale("k", "Keyed", items = c("k1", "k2", "k3"))
  ))
}

keyed_data <- function(n = 200) {
  set.seed(7)
  latent <- stats::rnorm(n)
  band <- function(x) round(pmin(pmax(x + 4, 1), 7))
  data.frame(k1 = band(latent + stats::rnorm(n, sd = .5)),
             k2 = band(latent + stats::rnorm(n, sd = .5)),
             k3 = 8 - band(latent + stats::rnorm(n, sd = .5)))
}

test_that("17: a reverse-keyed item gets its item-rest correlation on the scored orientation", {
  d <- keyed_data()
  oriented <- transform(d, k3 = 8 - k3)
  oracle <- psych::alpha(oriented, check.keys = FALSE,
                         warnings = FALSE)$item.stats$r.drop

  got <- item_report(d, keyed_instrument())$k$diagnostics$item_rest_r
  expect_equal(got, oracle, tolerance = 1e-8)
  expect_true(all(got > 0.5))
})

test_that("18: a missing rest item removes the respondent from the diagnostics", {
  d <- keyed_data()
  d$k2[1:30] <- NA
  complete <- d[stats::complete.cases(d), ]
  oracle <- psych::alpha(transform(complete, k3 = 8 - k3), check.keys = FALSE,
                         warnings = FALSE)$item.stats$r.drop

  got <- item_report(d, keyed_instrument())$k$diagnostics$item_rest_r
  expect_equal(got, oracle, tolerance = 1e-8)
})

test_that("19: floor and ceiling use the declared bounds of the scale", {
  ins <- sf_instrument(title = "Bounds", components = list(
    seven(),
    sf_item("m1", "One", "likert", choice_set = "ag7", scale_id = "m"),
    sf_item("m2", "Two", "likert", choice_set = "ag7", scale_id = "m"),
    sf_scale("m", "Middle", items = c("m1", "m2"))
  ))
  d <- data.frame(m1 = c(3, 4, 5, 4), m2 = c(1, 7, 4, 4))
  diag <- item_report(d, ins)$m$diagnostics

  # m1 never reaches 1 or 7
  expect_equal(diag$floor_pct[diag$item_id == "m1"], 0)
  expect_equal(diag$ceiling_pct[diag$item_id == "m1"], 0)
  expect_equal(diag$floor_pct[diag$item_id == "m2"], 0.25)
  expect_equal(diag$ceiling_pct[diag$item_id == "m2"], 0.25)
})

test_that("19: floor and ceiling are NA for an item with no declared bounds", {
  ins <- sf_instrument(title = "Unbounded", components = list(
    sf_item("u1", "One", "numeric", scale_id = "u"),
    sf_item("u2", "Two", "numeric", scale_id = "u"),
    sf_scale("u", "Unbounded", items = c("u1", "u2"))
  ))
  d <- data.frame(u1 = c(3, 4, 5), u2 = c(2, 4, 6))
  diag <- item_report(d, ins)$u$diagnostics
  expect_true(all(is.na(diag$floor_pct)))
  expect_true(all(is.na(diag$ceiling_pct)))
})
