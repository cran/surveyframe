# tests/testthat/test-citation.R
#
# The author's given names are "Mohammed Ali" and the family name is
# "Sharafuddin". Passed to person() as one string, R abbreviates the pair to a
# single initial and citation() printed "Sharafuddin M". The years were fixed
# strings that would go stale every January.

citation_meta <- function(date_publication = NULL) {
  path <- sframe_installed_path("DESCRIPTION")
  skip_if(is.na(path), "DESCRIPTION not found")
  meta <- as.list(read.dcf(path)[1, ])
  meta[["Date/Publication"]] <- date_publication
  meta
}

read_citation <- function(meta) {
  path <- sframe_installed_path("inst", "CITATION")
  skip_if(is.na(path), "CITATION not found")
  utils::readCitationFile(path, meta = meta)
}

test_that("the text citation carries both given-name initials", {
  ci <- read_citation(citation_meta())
  expect_match(format(ci, style = "text"), "Sharafuddin MA", fixed = TRUE)
  expect_identical(ci$author[[1]]$family, "Sharafuddin")
  expect_identical(ci$author[[1]]$given, c("Mohammed", "Ali"))
})

test_that("the BibTeX entry spells out the full name", {
  bib <- paste(toBibtex(read_citation(citation_meta())), collapse = "\n")
  expect_match(bib, "author = {Mohammed Ali Sharafuddin}", fixed = TRUE)
})

test_that("CITATION takes its year from the release, not today", {
  ci <- read_citation(citation_meta(date_publication = "2031-03-04 10:00:00 UTC"))
  expect_identical(ci$year, "2031")
})

test_that("DESCRIPTION's author matches CITATION's", {
  path <- sframe_installed_path("DESCRIPTION")
  skip_if(is.na(path), "DESCRIPTION not found")
  authors <- eval(parse(text = read.dcf(path, fields = "Authors@R")[1, 1]))
  expect_identical(authors[[1]]$given, c("Mohammed", "Ali"))
  expect_identical(authors[[1]]$family, "Sharafuddin")
})

test_that("the APA self-citation has its version and years filled in", {
  cits <- unlist(sframe_citations_for_test("t_test"))
  self <- grep("Sharafuddin, M. A.", cits, fixed = TRUE, value = TRUE)
  r_core <- grep("R Core Team", cits, fixed = TRUE, value = TRUE)
  expect_length(self, 1L)
  expect_length(r_core, 1L)
  expect_false(any(grepl("{", cits, fixed = TRUE)))
  expect_match(self, paste0("(", .sframe_release_year(), ")"), fixed = TRUE)
  expect_match(self, as.character(utils::packageVersion("surveyframe")),
               fixed = TRUE)
  expect_match(r_core, paste0("(", R.version$year, ")"), fixed = TRUE)
  expect_match(.sframe_release_year(), "^[0-9]{4}$")
})

test_that("_pkgdown.yml's JSON-LD version matches DESCRIPTION", {
  yml <- sframe_source_text("_pkgdown.yml")
  declared <- regmatches(yml, regexpr('"softwareVersion": "[^"]+"', yml))
  expect_identical(
    declared,
    sprintf('"softwareVersion": "%s"',
            read.dcf(sframe_source_path("DESCRIPTION"), fields = "Version")[1, 1]))
})
