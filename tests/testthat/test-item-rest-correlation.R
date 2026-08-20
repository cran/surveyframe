# tests/testthat/test-item-rest-correlation.R
# The item-rest correlation in item_report() is the correlation between an item
# and the SUM of the other items in its scale. An earlier implementation
# subtracted the item from a rowMeans() total, which leaves roughly noise
# carrying the item negatively and so returned strong negative values on a
# highly reliable scale. psych::alpha()$item.stats$r.drop is the oracle.

make_scale_data <- function(n = 300, k = 6, seed = 42) {
  set.seed(seed)
  latent <- stats::rnorm(n)
  out <- as.data.frame(lapply(seq_len(k), function(i) {
    round(pmin(pmax(latent * 1.1 + stats::rnorm(n, sd = 0.55) + 4, 1), 7))
  }))
  names(out) <- paste0("q", seq_len(k))
  out
}

scale_instrument <- function(items) {
  sf_instrument(
    title   = "Item-rest correlation",
    version = "1.0.0",
    components = c(
      lapply(items, function(id) sf_item(id, paste("Item", id), type = "likert")),
      list(sf_scale("s1", "Test scale", items = items))
    )
  )
}

test_that("item-rest correlation matches psych::alpha()$item.stats$r.drop", {
  skip_if_not_installed("psych")

  dat  <- make_scale_data()
  inst <- scale_instrument(names(dat))

  oracle <- psych::alpha(dat, warnings = FALSE)$item.stats$r.drop

  ir   <- item_report(dat, inst)
  got  <- ir$s1$diagnostics$item_rest_r

  expect_equal(got, oracle, tolerance = 1e-8)
})

test_that("a highly reliable scale returns positive item-rest correlations", {
  skip_if_not_installed("psych")

  dat  <- make_scale_data()
  inst <- scale_instrument(names(dat))

  # alpha here is around 0.95, so every item must correlate positively with
  # the rest of its scale. The earlier implementation returned about -0.46.
  expect_gt(psych::alpha(dat, warnings = FALSE)$total$raw_alpha, 0.9)

  got <- item_report(dat, inst)$s1$diagnostics$item_rest_r
  expect_true(all(got > 0.5))
})

test_that("item-rest correlation is the sum of the other items, not a mean residual", {
  dat  <- make_scale_data()
  inst <- scale_instrument(names(dat))

  got <- item_report(dat, inst)$s1$diagnostics$item_rest_r

  expected <- vapply(names(dat), function(col) {
    rest <- rowSums(dat[, setdiff(names(dat), col), drop = FALSE], na.rm = TRUE)
    stats::cor(dat[[col]], rest, use = "complete.obs")
  }, numeric(1))

  expect_equal(got, unname(expected), tolerance = 1e-10)

  # the discredited formula, pinned so it cannot creep back in
  total_mean <- rowMeans(dat, na.rm = TRUE)
  discredited <- vapply(names(dat), function(col) {
    stats::cor(dat[[col]], total_mean - dat[[col]], use = "complete.obs")
  }, numeric(1))
  expect_true(all(discredited < 0))
  expect_false(isTRUE(all.equal(got, unname(discredited))))
})

test_that("a 2-item scale reduces to the correlation between the 2 items", {
  dat  <- make_scale_data()[, c("q1", "q2")]
  inst <- scale_instrument(names(dat))

  got <- item_report(dat, inst)$s1$diagnostics$item_rest_r
  r   <- stats::cor(dat$q1, dat$q2, use = "complete.obs")

  expect_equal(got, c(r, r), tolerance = 1e-10)
})

test_that("missing values do not inflate the rest score", {
  dat <- make_scale_data()
  dat$q3[1:20] <- NA_real_
  inst <- scale_instrument(names(dat))

  got <- item_report(dat, inst)$s1$diagnostics$item_rest_r

  expect_false(any(is.na(got)))
  expect_true(all(got > 0.5))
})
