# tests/testthat/test-sframe-schema.R
# inst/schema/sframe_schema.json documents the .sframe format so a reviewer
# or a second tool can validate a file without installing surveyframe. No
# JSON Schema validation package is added as a dependency for this (none was
# already present) -- these checks confirm the schema's own required-key
# list stays in sync with what write_sframe() actually produces, by hand,
# which is what a schema is for either way.

schema_path <- function() {
  system.file("schema", "sframe_schema.json", package = "surveyframe")
}

test_that("the schema file is installed and parses as JSON", {
  path <- schema_path()
  skip_if(!nzchar(path), "schema not found via system.file(); run devtools::load_all() first")
  schema <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_equal(schema$type, "object")
  expect_true(all(c("hash", "version", "meta", "items", "choices", "scales")
                  %in% schema$required))
})

test_that("a freshly written .sframe file's top-level keys match the schema's properties", {
  path <- schema_path()
  skip_if(!nzchar(path), "schema not found via system.file(); run devtools::load_all() first")
  schema <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  item <- sf_item("q1", "How satisfied are you?", type = "text")
  instr <- sf_instrument("Demo", components = list(item))
  out <- tempfile(fileext = ".sframe")
  write_sframe(instr, out, overwrite = TRUE)
  written <- jsonlite::fromJSON(out, simplifyVector = FALSE)

  extra_keys <- setdiff(names(written), names(schema$properties))
  expect_equal(extra_keys, character(0))
  expect_true(all(schema$required %in% names(written)))
})

test_that("a .sframe file with an amendment round-trips its amendments key against the schema", {
  path <- schema_path()
  skip_if(!nzchar(path), "schema not found via system.file(); run devtools::load_all() first")
  schema <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_true("amendments" %in% names(schema$properties))

  item <- sf_item("q1", "How satisfied are you?", type = "text")
  instr <- sf_instrument("Demo", components = list(item))
  item2 <- sf_item("q1", "How satisfied are you overall?", type = "text")
  revised <- sf_instrument("Demo", components = list(item2))
  amended <- amend_sframe(
    instr, revised,
    reason_code = "instrument_revision",
    reason_text = "wording",
    deviation_report = "wording only"
  )
  out <- tempfile(fileext = ".sframe")
  write_sframe(amended, out, overwrite = TRUE)
  written <- jsonlite::fromJSON(out, simplifyVector = FALSE)

  expect_true("amendments" %in% names(written))
  entry <- written$amendments[[1]]
  required_entry_keys <- schema$properties$amendments$items$required
  expect_true(all(required_entry_keys %in% names(entry)))
})
