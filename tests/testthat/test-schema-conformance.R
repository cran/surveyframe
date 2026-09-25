# tests/testthat/test-schema-conformance.R
#
# E1. write_sframe() used jsonlite's auto_unbox, so a one-item scale was written
#     as "items": "sat_1" where the published instrument profile requires an
#     array. Singletons are legal instruments. The builder already wrote true
#     arrays, so the 2 routes also hashed the same instrument differently.
# Batch 5, finding 10. The reader, the bundled schema and the published profile
#     disagreed on required fields, the reader never checked sframe_format, and
#     the bundled schema said render must be an object while the writer can
#     emit an empty array.
#
# No JSON Schema validator is a dependency, so the profile's typed constraints
# are checked here directly, with a drift guard against the profile file when
# the sframe-schema repository sits beside this one.

profile_problems <- function(p) {
  out <- character(0)
  bad <- function(cond, msg) if (!isTRUE(cond)) out <<- c(out, msg)
  is_array <- function(x) is.list(x) && is.null(names(x))
  bad(is.character(p$sframe_format) && grepl("^[0-9]+\\.[0-9]+$", p$sframe_format),
      "sframe_format")
  bad(is.list(p$hash) && identical(p$hash$algo, "sha256"), "hash")
  bad(is.list(p$meta) && is.character(p$meta$title), "meta.title")
  bad(is_array(p$items), "items array")
  types <- c("likert", "single_choice", "multiple_choice", "numeric", "text",
             "textarea", "date", "matrix", "slider", "ranking", "rating",
             "pairwise_comparison", "criteria_weight", "section_break", "text_block")
  for (it in p$items) bad(is.character(it$id) && it$type %in% types, paste("item", it$id))
  for (cs in p$choices %||% list()) {
    bad(is_array(cs$values), paste0("choices[", cs$id, "].values array"))
    bad(is.null(cs$labels) || is_array(cs$labels), paste0("choices[", cs$id, "].labels array"))
  }
  for (sc in p$scales %||% list()) {
    bad(is_array(sc$items) && all(vapply(sc$items, is.character, logical(1))),
        paste0("scales[", sc$id, "].items array of strings"))
    bad(is.null(sc$min_valid) || (is.numeric(sc$min_valid) && sc$min_valid == round(sc$min_valid)),
        paste0("scales[", sc$id, "].min_valid integer"))
  }
  bad(is.null(p$render) || is.list(p$render), "render")
  out
}

written <- function(instrument) {
  path <- tempfile(fileext = ".sframe")
  write_sframe(instrument, path)
  list(path = path,
       parsed = jsonlite::fromJSON(paste(readLines(path, warn = FALSE), collapse = "\n"),
                                   simplifyVector = FALSE))
}

singleton_instrument <- function() {
  sf_instrument(title = "Singletons", components = list(
    sf_choices("one", values = 1, labels = "Only option"),
    sf_item("s1", "Single scale item", "likert", choice_set = "one", scale_id = "s"),
    sf_item("mx", "One-row matrix", "matrix", choice_set = "one", matrix_items = "row"),
    sf_scale("s", "One-item scale", items = "s1", reverse_items = "s1")
  ))
}

test_that("E1: singleton components are written as JSON arrays", {
  p <- written(singleton_instrument())$parsed
  expect_true(is.list(p$choices[[1]]$values))
  expect_true(is.list(p$choices[[1]]$labels))
  expect_true(is.list(p$scales[[1]]$items))
  expect_true(is.list(p$scales[[1]]$reverse_items))
  expect_true(is.list(p$items[[2]]$matrix_items))
})

test_that("E1: written output meets the instrument profile for 0, 1 and many members", {
  empty <- sf_instrument(title = "Empty", components = list(sf_item("t", "Text", "text")))
  many <- sf_instrument(title = "Many", components = list(
    sf_choices("ag", 1:5, as.character(1:5)),
    sf_item("a", "A", "likert", choice_set = "ag", scale_id = "s"),
    sf_item("b", "B", "likert", choice_set = "ag", scale_id = "s"),
    sf_scale("s", "S", items = c("a", "b"))))
  for (ins in list(empty, singleton_instrument(), many)) {
    expect_identical(profile_problems(written(ins)$parsed), character(0))
  }
})

test_that("E1: a singleton instrument is a serialisation fixed point", {
  first <- written(singleton_instrument())
  again <- written(read_sframe(first$path))
  expect_identical(again$parsed$hash$value, first$parsed$hash$value)
})

test_that("10: the reader accepts a minimal profile file with only the core fields", {
  minimal <- list(sframe_format = "1.0", hash = list(algo = "sha256", value = ""),
                  meta = list(title = "Minimal"),
                  items = list(list(id = "q1", label = "Q1", type = "text")))
  minimal$hash$value <- sframe_hash_payload(minimal)
  path <- tempfile(fileext = ".sframe")
  writeLines(jsonlite::toJSON(minimal, auto_unbox = TRUE, null = "null", digits = NA), path)
  ins <- read_sframe(path)
  expect_s3_class(ins, "sframe")
  expect_length(ins$items, 1)
})

test_that("10: a file from a newer major format version is refused clearly", {
  p <- written(singleton_instrument())
  parsed <- p$parsed
  parsed$sframe_format <- "2.0"
  parsed$hash$value <- ""
  parsed$hash$value <- sframe_hash_payload(parsed)
  writeLines(jsonlite::toJSON(parsed, auto_unbox = TRUE, null = "null", digits = NA), p$path)
  expect_error(read_sframe(p$path), "2.0", class = "sframe_error")
})

test_that("10: the bundled schema agrees with the reader and the writer", {
  path <- system.file("schema", "sframe_schema.json", package = "surveyframe")
  skip_if(!nzchar(path), "bundled schema not found")
  schema <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_setequal(unlist(schema$required), c("hash", "meta", "items"))
  expect_true("array" %in% unlist(schema$properties$render$type))
  expect_false(grepl("byte-identical", schema$properties$hash$description, fixed = TRUE))
})

test_that("10: the profile checker here matches the published profile's required fields", {
  profile <- test_path("..", "..", "..", "sframe-schema", "schema",
                       "sframe-instrument.schema.json")
  skip_if(!file.exists(profile), "sframe-schema repository not beside this one")
  schema <- jsonlite::fromJSON(profile, simplifyVector = FALSE)
  expect_setequal(unlist(schema$required), c("sframe_format", "hash", "meta", "items"))
})
