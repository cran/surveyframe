# tests/testthat/test-doc-contracts-2.R
#
# Batch 8 correctness findings, and the codebook and CFA reverse-coding item
# carried over from the scoring phase.
#
# #10. One shared page documented 15 as.data.frame() methods with materially
#      different schemas, and said row.names is forwarded where several methods
#      return their table directly and ignore it.
# #11. sf_apa() was documented for analysis results only, though it answers for
#      4 standalone reports, and sf_flagged() returns a union of row positions.
# #14. The EFA help sent readers to another package for a solution
#      efa_solution() already fits.
# #16. The package page said every function operates on an instrument, where
#      the text and bootstrap helpers take plain vectors.
# #19. The conjoint help said a design becomes part of the instrument contract
#      without showing how, and sf_instrument() omitted designs from its
#      components.
# Carried over: the codebook's reverse column and the CFA syntax comment read
#      item-level reverse only, so an item a scale reverses looked unreversed.

reverse_instrument <- function() {
  sf_instrument(title = "Reverse", components = list(
    sf_choices("ag5", 1:5, as.character(1:5)),
    sf_item("a1", "A1", "likert", choice_set = "ag5", scale_id = "s"),
    sf_item("a2", "A2", "likert", choice_set = "ag5", scale_id = "s"),
    sf_scale("s", "S", items = c("a1", "a2"), reverse_items = "a2")))
}

test_that("carried over: the codebook shows an item its scale reverses", {
  book <- as.data.frame(reverse_instrument())
  expect_identical(book$reverse[book$id == "a2"], TRUE)
  expect_identical(book$reverse[book$id == "a1"], FALSE)
})

test_that("carried over: generated CFA syntax names scale-level reversed items", {
  ins <- reverse_instrument()
  model <- sf_model(id = "m", type = "cb_sem", engine = "lavaan",
                    constructs = list(sf_construct("s", items = c("a1", "a2"))))
  syn <- cfa_lavaan_syntax(model = model, instrument = ins)
  expect_match(syn, "Reverse-coded indicators: a2", fixed = TRUE)
})

source_of <- function(file) {
  p <- sframe_source_path("R", file)
  skip_if(is.na(p), paste("no source tree for", file))
  skip_if(!file.exists(p), "package source not available")
  paste(readLines(p, warn = FALSE), collapse = "\n")
}

test_that("10: the coercion help describes the schemas and the row.names limit", {
  src <- source_of("as_data_frame.R")
  expect_match(src, "row.names", fixed = TRUE)
  expect_match(src, "`sframe_validation`: one row per problem", fixed = TRUE)
  # the claim under test: several methods return a stored table directly
  book <- codebook_report(reverse_instrument())
  expect_identical(rownames(as.data.frame(book, row.names = c("x", "y"))),
                   rownames(as.data.frame(book)))
})

test_that("11: sf_apa() answers for standalone reports too, and the help says so", {
  # 4 standalone reports carry an APA sentence, alongside analysis results
  d <- data.frame(a1 = c(1, 5, 3, 4, 2, 5), a2 = c(5, 1, 3, 2, 4, 1))
  ar <- assumption_report(d, reverse_instrument())
  expect_type(sf_apa(ar), "character")
  src <- source_of("accessors.R")
  expect_match(src, "missing-data report", fixed = TRUE)
  expect_match(src, "row positions", fixed = TRUE)
})

test_that("14: the EFA help names efa_solution() as the way to fit one", {
  src <- source_of("psychometrics.R")
  expect_match(src, "efa_solution()", fixed = TRUE)
})

test_that("16: the package page says which functions take plain vectors", {
  src <- source_of("surveyframe-package.R")
  expect_false(grepl("Every function operates on", src, fixed = TRUE))
  expect_match(src, "sframe_demos()", fixed = TRUE)
})

test_that("19: a conjoint design is documented as an instrument component", {
  design <- sf_conjoint_design("d", attributes = list(a = c("x", "y"), b = c("p", "q")),
                               method = "full", seed = 3)
  ins <- sf_instrument(title = "With design", components = list(
    sf_item("q1", "Q", "text"), design))
  expect_length(ins$designs, 1)
  # the help, not the code: the conjoint page shows the design going into an
  # instrument, and sf_instrument() lists it among the components it accepts
  conj <- source_of("conjoint_design.R")
  expect_match(sub("^.*?sf_conjoint_design <- function", "", conj), "", fixed = TRUE)
  roxygen <- function(src) paste(grep("^#'", strsplit(src, "\n")[[1]], value = TRUE), collapse = "\n")
  expect_match(roxygen(conj), "sf_instrument(", fixed = TRUE)
  expect_match(roxygen(source_of("sf_instrument.R")), "sf_conjoint_design()", fixed = TRUE)
})
