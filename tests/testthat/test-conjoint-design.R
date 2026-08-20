# tests/testthat/test-conjoint-design.R
# sf_conjoint_design() declares a choice-experiment design as part of the
# instrument's contract. It is a generator, not an estimator. The properties
# that matter are that the design is reproducible from what is stored, that it
# round-trips with the instrument hash intact, and that adding the feature did
# not change the hash of instruments that do not use it.

dce_attributes <- function() {
  list(
    price    = c("50", "100", "150"),
    board    = c("room only", "breakfast"),
    distance = c("beachfront", "10 min walk")
  )
}

test_that("a full factorial enumerates every combination", {
  d <- sf_conjoint_design("d1", dce_attributes(), method = "full", seed = 1)

  expect_s3_class(d, "sf_conjoint_design")
  expect_equal(nrow(d$profiles), 3 * 2 * 2)
  expect_equal(d$balance$n_full_factorial, 12)
  expect_true(all(c("profile_id", "price", "board", "distance") %in%
                    names(d$profiles)))
  expect_equal(anyDuplicated(d$profiles$profile_id), 0)
})

test_that("the same seed reproduces the identical design", {
  a <- sf_conjoint_design("d1", dce_attributes(), method = "balanced",
                          n_profiles = 6, seed = 99)
  b <- sf_conjoint_design("d1", dce_attributes(), method = "balanced",
                          n_profiles = 6, seed = 99)

  expect_identical(a$profiles, b$profiles)
  expect_identical(a$tasks, b$tasks)
})

test_that("a different seed gives a different design", {
  a <- sf_conjoint_design("d1", dce_attributes(), method = "random",
                          n_profiles = 6, seed = 1)
  b <- sf_conjoint_design("d1", dce_attributes(), method = "random",
                          n_profiles = 6, seed = 2)
  expect_false(identical(a$profiles, b$profiles) && identical(a$tasks, b$tasks))
})

test_that("a seed is recorded even when the caller does not supply one", {
  # Without this the design could not be regenerated from the contract, which
  # is the whole point of declaring it.
  d <- sf_conjoint_design("d1", dce_attributes(), method = "random",
                          n_profiles = 6)
  expect_true(is.integer(d$seed))
  expect_false(is.na(d$seed))

  again <- sf_conjoint_design("d1", dce_attributes(), method = "random",
                              n_profiles = 6, seed = d$seed)
  expect_identical(again$profiles, d$profiles)
})

test_that("generating a design does not disturb the caller's RNG stream", {
  set.seed(123)
  before <- runif(3)

  set.seed(123)
  invisible(sf_conjoint_design("d1", dce_attributes(), method = "random",
                               n_profiles = 6, seed = 7))
  after <- runif(3)

  expect_equal(before, after)
})

test_that("balanced is at least as even as the average random draw", {
  # The search keeps the best subset it sees, so it cannot be worse than a
  # typical single draw. Compared against the median of 20 random designs
  # rather than one, which would be noise.
  bal <- sf_conjoint_design("d1", dce_attributes(), method = "balanced",
                            n_profiles = 6, seed = 5)$balance$imbalance
  rand <- vapply(1:20, function(s) {
    sf_conjoint_design("d1", dce_attributes(), method = "random",
                       n_profiles = 6, seed = s)$balance$imbalance
  }, numeric(1))

  expect_lte(bal, stats::median(rand))
})

test_that("the task schedule has the declared shape, no repeat per task", {
  d <- sf_conjoint_design("d1", dce_attributes(), method = "full",
                          n_alternatives = 2, n_tasks = 3, blocks = 2, seed = 3)

  expect_equal(nrow(d$tasks), 2 * 3 * 2)
  expect_setequal(unique(d$tasks$block), 1:2)
  expect_setequal(unique(d$tasks$alternative), 1:2)

  by_task <- split(d$tasks$profile_id,
                   paste(d$tasks$block, d$tasks$task, sep = "-"))
  for (tk in by_task) {
    expect_equal(anyDuplicated(tk), 0,
                 info = "a profile must not appear twice in one task")
  }
})

test_that("a supplied design is declared as given", {
  profiles <- data.frame(
    price    = c("50", "150"),
    board    = c("room only", "breakfast"),
    distance = c("beachfront", "10 min walk"),
    stringsAsFactors = FALSE
  )
  d <- sf_conjoint_design("d1", dce_attributes(), profiles = profiles, seed = 1)

  expect_identical(d$method, "supplied")
  expect_equal(nrow(d$profiles), 2)
  expect_equal(d$profiles$price, c("50", "150"))
})

test_that("a supplied design using undeclared levels is refused", {
  profiles <- data.frame(
    price    = c("50", "999"),
    board    = c("room only", "breakfast"),
    distance = c("beachfront", "10 min walk"),
    stringsAsFactors = FALSE
  )
  expect_error(
    sf_conjoint_design("d1", dce_attributes(), profiles = profiles),
    class = "sframe_validation_error"
  )
})

test_that("degenerate declarations are refused", {
  expect_error(sf_conjoint_design("d1", list(price = c("50", "100"))),
               class = "sframe_validation_error")
  expect_error(
    sf_conjoint_design("d1", list(a = c("1", "2"), b = "only")),
    class = "sframe_validation_error"
  )
  expect_error(
    sf_conjoint_design("d1", list(a = c("1", "1"), b = c("x", "y"))),
    class = "sframe_validation_error"
  )
  expect_error(
    sf_conjoint_design("d1", dce_attributes(), method = "random"),
    class = "sframe_validation_error"
  )
  expect_error(
    sf_conjoint_design("d1", dce_attributes(), method = "random",
                       n_profiles = 999),
    class = "sframe_validation_error"
  )
  expect_error(
    sf_conjoint_design("d1", dce_attributes(), n_alternatives = 1),
    class = "sframe_validation_error"
  )
})

test_that("asking for more tasks than the profiles support is refused", {
  expect_error(
    sf_conjoint_design("d1", dce_attributes(), method = "full",
                       n_alternatives = 2, n_tasks = 99),
    class = "sframe_validation_error"
  )
})

test_that("a design attaches to an instrument and survives a round trip", {
  d <- sf_conjoint_design("hotel_dce", dce_attributes(), method = "balanced",
                          n_profiles = 6, seed = 42)
  inst <- sf_instrument(
    title = "DCE", version = "1.0.0",
    components = list(sf_item("q1", "Anything", type = "text"), d)
  )

  expect_length(inst$designs, 1)

  tmp <- tempfile(fileext = ".sframe")
  write_sframe(inst, tmp)
  back <- read_sframe(tmp, validate = FALSE)

  expect_length(back$designs, 1)
  expect_s3_class(back$designs[[1]], "sf_conjoint_design")
  expect_identical(back$designs[[1]]$profiles, d$profiles)
  expect_identical(back$designs[[1]]$tasks, d$tasks)
  expect_identical(back$designs[[1]]$seed, d$seed)
  expect_identical(back$designs[[1]]$attributes, d$attributes)
})

test_that("an instrument with no design keeps the key out of the payload", {
  inst <- sf_instrument(
    title = "No design", version = "1.0.0",
    components = list(sf_item("q1", "Anything", type = "text"))
  )
  expect_false("designs" %in% names(sframe_serialization_payload(inst)))
})

test_that("an existing instrument keeps its hash across a read and rewrite", {
  # This is the property the conditional key actually protects, and it is
  # not the one an earlier version of this test claimed. Writing an empty
  # designs array does NOT break reading an old file: read_sframe() hashes
  # the parsed payload, which has no designs key either, so it stays
  # self-consistent. What breaks is identity. An instrument read from an
  # existing file and written back would come out under a different hash,
  # so the same content would silently change identity, and R would desync
  # from the builder's JS sframeCanon(). Measured while checking this:
  # c3df10ec before, febd07c5 after.
  for (f in c("tourism_services_demo.sframe",
              "surveyframe_input_types_demo.sframe")) {
    p <- system.file("extdata", f, package = "surveyframe")
    skip_if(!nzchar(p) || !file.exists(p), paste("demo not available:", f))

    stored <- jsonlite::fromJSON(p, simplifyVector = FALSE)$hash$value
    inst   <- read_sframe(p, validate = FALSE)

    expect_identical(sframe_hash_value(inst), stored,
                     info = paste(f, "must hash to the same value it stores"))
  }
})

test_that("an instrument declaring a design does carry the key", {
  d <- sf_conjoint_design("d1", dce_attributes(), method = "full", seed = 1)
  inst <- sf_instrument(
    title = "With design", version = "1.0.0",
    components = list(sf_item("q1", "Anything", type = "text"), d)
  )
  expect_true("designs" %in% names(sframe_serialization_payload(inst)))
})

test_that("print states that this declares a design and does not fit models", {
  d <- sf_conjoint_design("d1", dce_attributes(), method = "full", seed = 1)
  out <- paste(utils::capture.output(print(d)), collapse = "\n")
  expect_match(out, "Conjoint design")
  expect_match(out, "does not fit choice models")
})
