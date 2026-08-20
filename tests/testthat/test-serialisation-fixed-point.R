# tests/testthat/test-serialisation-fixed-point.R
# Writing an instrument, reading it, and writing it again must produce the
# same hash. It did not: a component built by an sf_* constructor carries
# every optional field as NULL and a plan block carries only what the caller
# supplied, while the reader drops those NULLs and fills 8 plan defaults. So
# identical content could carry 2 different hashes depending on whether it had
# been through a read, which is a poor property for the value that is meant to
# be the instrument's identity. Measured before the fix: ec822fff written,
# 3fa36502 after a read.

fresh_instrument <- function() {
  sf_instrument(
    title = "Fixed point", version = "1.0.0",
    components = list(
      sf_choices("ag5", 1:5, c("a", "b", "c", "d", "e")),
      sf_item("q1", "Q1", type = "likert", choice_set = "ag5", scale_id = "sc"),
      sf_item("q2", "Q2", type = "matrix", matrix_items = c("r1", "r2"),
              choice_set = "ag5"),
      sf_scale("sc", "S", items = "q1")
    ),
    analysis_plan = list(list(
      id = "RQ1", research_question = "R?", family = "descriptive",
      method = "frequencies", roles = list(variables = "q1")
    ))
  )
}

stored_hash <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)$hash$value
}

test_that("write, read, write leaves the hash unchanged", {
  p1 <- tempfile(fileext = ".sframe")
  write_sframe(fresh_instrument(), p1)
  h1 <- stored_hash(p1)

  back <- read_sframe(p1)
  expect_identical(sframe_hash_value(back), h1)

  p2 <- tempfile(fileext = ".sframe")
  write_sframe(back, p2)
  expect_identical(stored_hash(p2), h1)
})

test_that("a fresh instrument and its round trip serialise to the same payload", {
  inst <- fresh_instrument()
  p <- tempfile(fileext = ".sframe")
  write_sframe(inst, p)

  a <- sframe_serialization_payload(read_sframe(p))
  b <- sframe_serialization_payload(read_sframe(p))
  expect_identical(a, b)

  # the parts that used to diverge
  fresh <- sframe_serialization_payload(as_sframe(validate_sframe(inst, strict = TRUE)))
  roundtripped <- sframe_serialization_payload(read_sframe(p))
  expect_identical(fresh$items, roundtripped$items)
  expect_identical(fresh$analysis_plan, roundtripped$analysis_plan)
  expect_identical(fresh$scales, roundtripped$scales)
})

test_that("the bundled instruments still hash to the value they store", {
  # The fix normalises on write, so it must not disturb files that were
  # already written in the settled form. If any of these moves, every stored
  # .sframe in the wild moved with it.
  for (f in c("tourism_services_demo.sframe",
              "surveyframe_input_types_demo.sframe",
              "hotel_supplier_mcdm.sframe")) {
    p <- system.file("extdata", f, package = "surveyframe")
    skip_if(!nzchar(p) || !file.exists(p), paste("not available:", f))
    expect_identical(sframe_hash_value(read_sframe(p)), stored_hash(p),
                     info = f)
  }
})

test_that("the restore functions are idempotent on content", {
  # This is what makes normalising at write time safe. If any of these stopped
  # being idempotent, writing a settled file would change its hash.
  inst <- read_sframe(system.file("extdata", "tourism_services_demo.sframe",
                                  package = "surveyframe"))
  strip <- function(x) { class(x) <- NULL; x }

  expect_identical(strip(inst$items[[1]]),
                   strip(sframe_restore_item(inst$items[[1]])))
  expect_identical(strip(inst$choices[[1]]),
                   strip(sframe_restore_choices(inst$choices[[1]])))
  expect_identical(inst$analysis_plan[[1]],
                   sframe_restore_analysis_block(inst$analysis_plan[[1]]))
})

test_that("a non-round decimal survives write_sframe()/read_sframe() without a false integrity failure", {
  # write_sframe()'s hash is computed with digits = NA (full precision,
  # sframe_hash_payload() -> sframe_hash_json()), but the JSON actually
  # written to disk used to go through jsonlite::toJSON() with its default
  # digits = 4, silently rounding any non-round decimal (a scale weight of
  # 1/3, say) before it reached disk. The file's own embedded hash was
  # computed over the un-rounded value, so read_sframe() (or any other
  # reader) recomputing the hash from the actually-written, rounded text
  # got a different hash and reported "modified" on a file nobody had
  # touched. Caught via a standalone browser-based .sframe verifier, which
  # reproduced the same false failure independently of this package.
  # Fixed by adding digits = NA to write_sframe()'s own toJSON() call, so
  # the same full-precision value that was hashed is the one written.
  path <- tempfile(fileext = ".sframe")
  instr <- sf_instrument(
    title = "Non-round weight", version = "1.0.0",
    components = list(
      sf_choices("ag5", 1:5, c("a", "b", "c", "d", "e")),
      sf_item("q1", "Q1", type = "likert", choice_set = "ag5", scale_id = "sc"),
      sf_scale("sc", "S", items = "q1", weights = 1 / 3)
    )
  )
  write_sframe(instr, path, overwrite = TRUE)
  written <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_equal(as.numeric(written$scales[[1]]$weights[[1]]), 1 / 3)
  back <- expect_no_error(read_sframe(path))
  expect_equal(back$scales[[1]]$weights, 1 / 3)
})
