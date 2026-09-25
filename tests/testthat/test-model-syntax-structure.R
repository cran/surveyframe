# tests/testthat/test-model-syntax-structure.R
#
# C2. seminr_syntax() compressed indicator names into a numeric range after
# checking only that each name was a prefix plus a number. c("Q1", "Q3") became
# multi_items("Q", 1:3), which adds Q2 to the fitted model whenever Q2 is in the
# data.
# Batch 2 finding 20. An indirect effect was accepted when a path along its
# route was never declared, and the syntax multiplied a label no path carries.
# Batch 2 finding 21. A total effect was written once per indirect effect, so
# 2 parallel mediators gave total_A_C twice, each holding 1 indirect effect.

test_that("C2: non-contiguous indicators are written out exactly", {
  m <- sf_model(
    id = "p1", type = "pls_sem", engine = "seminr",
    constructs = list(
      sf_construct("A", items = c("Q1", "Q3"), mode = "composite"),
      sf_construct("B", items = c("R1", "R2", "R3"), mode = "composite")
    ),
    paths = list(sf_path("A", "B"))
  )
  code <- seminr_syntax(m)

  expect_false(grepl('multi_items("Q", 1:3)', code, fixed = TRUE))
  expect_match(code, 'composite("A", c("Q1", "Q3"))', fixed = TRUE)
  # a sequence that is exactly contiguous may still be compressed
  expect_match(code, 'multi_items("R", 1:3)', fixed = TRUE)
  expect_no_error(parse(text = code))
})

test_that("C2: out-of-order or repeated-width names are never compressed", {
  m <- sf_model(
    id = "p2", type = "pls_sem", engine = "seminr",
    constructs = list(
      sf_construct("A", items = c("Q3", "Q1", "Q2"), mode = "composite"),
      sf_construct("B", items = c("Q10", "Q11", "Q12"), mode = "composite")
    ),
    paths = list(sf_path("A", "B"))
  )
  code <- seminr_syntax(m)
  expect_match(code, 'composite("A", c("Q3", "Q1", "Q2"))', fixed = TRUE)
})

three_constructs <- function(ids) {
  lapply(ids, function(id) sf_construct(id, items = paste0(tolower(id), 1:3)))
}

test_that("20: an indirect effect along an undeclared path is rejected", {
  m <- sf_model(
    id = "m1", type = "cb_sem", engine = "lavaan",
    constructs = three_constructs(c("SAT", "TRU", "LOY")),
    paths = list(sf_path("SAT", "TRU"), sf_path("SAT", "LOY")),
    indirect = list(sf_indirect(from = "SAT", through = "TRU", to = "LOY"))
  )
  v <- validate_model(m, strict = FALSE)
  expect_false(v$valid)
  expect_match(paste(v$problems, collapse = " "), "TRU")
  expect_match(paste(v$problems, collapse = " "), "LOY")
  expect_error(sem_lavaan_syntax(m), class = "sframe_error")
})

test_that("21: parallel mediators give one total effect holding every route", {
  m <- sf_model(
    id = "m2", type = "cb_sem", engine = "lavaan",
    constructs = three_constructs(c("SAT", "TRU", "VAL", "LOY")),
    paths = list(sf_path("SAT", "TRU", label = "a1"),
                 sf_path("TRU", "LOY", label = "b1"),
                 sf_path("SAT", "VAL", label = "a2"),
                 sf_path("VAL", "LOY", label = "b2"),
                 sf_path("SAT", "LOY", label = "c")),
    indirect = list(sf_indirect(from = "SAT", through = "TRU", to = "LOY",
                                label = "via_tru"),
                    sf_indirect(from = "SAT", through = "VAL", to = "LOY",
                                label = "via_val"))
  )
  syn <- sem_lavaan_syntax(m)
  totals <- regmatches(syn, gregexpr("(?m)^total_SAT_LOY :=.*$", syn, perl = TRUE))[[1]]

  expect_length(totals, 1)
  rhs <- trimws(strsplit(sub("^total_SAT_LOY :=", "", totals), "+", fixed = TRUE)[[1]])
  expect_setequal(rhs, c("c", "via_tru", "via_val"))

  skip_if_not_installed("lavaan")
  expect_no_error(lavaan::lavaanify(syn))
})
