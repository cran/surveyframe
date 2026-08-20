# tests/testthat/test-chrome-detritus.R
# pagedown::chrome_print() leaves Chrome's own scratch directories in
# tempdir(), which R CMD check reports as "detritus in the temp directory".
# render_report(format = "pdf") clears them, and must clear only those.

test_that("the cleaner removes Chrome's scratch directories and nothing else", {
  before <- list.files(tempdir(), all.files = TRUE, no.. = TRUE)

  # Stand-ins for what Chrome leaves behind, plus one file that must survive.
  chrome_dir <- file.path(tempdir(), "com.google.Chrome.testXY")
  scoped_dir <- file.path(tempdir(), "scoped_dirTESTXY")
  keeper     <- file.path(tempdir(), "sframe_keeper_testXY.txt")
  dir.create(chrome_dir, showWarnings = FALSE)
  dir.create(scoped_dir, showWarnings = FALSE)
  writeLines("keep me", keeper)
  on.exit(unlink(c(chrome_dir, scoped_dir, keeper), recursive = TRUE,
                 force = TRUE), add = TRUE)

  removed <- sframe_clean_chrome_detritus(before)

  expect_false(dir.exists(chrome_dir))
  expect_false(dir.exists(scoped_dir))
  expect_true(file.exists(keeper))
  expect_setequal(removed, c("com.google.Chrome.testXY", "scoped_dirTESTXY"))
})

test_that("the cleaner leaves entries that already existed before the call", {
  # A Chrome-named directory that predates the call is somebody else's, so
  # passing a `before` listing that already contains it must spare it.
  pre <- file.path(tempdir(), "com.google.Chrome.preexisting")
  dir.create(pre, showWarnings = FALSE)
  on.exit(unlink(pre, recursive = TRUE, force = TRUE), add = TRUE)

  before <- list.files(tempdir(), all.files = TRUE, no.. = TRUE)
  removed <- sframe_clean_chrome_detritus(before)

  expect_true(dir.exists(pre))
  expect_false("com.google.Chrome.preexisting" %in% removed)
})

# End-to-end check. Note this one only has teeth where Chrome actually
# leaves detritus, which varies by Chrome build and platform. It passed on a
# local Linux machine with the cleaner disabled, because that Chrome left
# nothing behind, and fails on the GitHub Actions image, which does. The 2
# tests above are the real guard on the cleaner and fail under mutation.
test_that("render_report(format = 'pdf') leaves no Chrome detritus behind", {
  skip_on_cran()
  skip_if_not_installed("pagedown")
  chrome <- tryCatch(pagedown::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome), "No Chrome available for chrome_print()")

  instr <- sf_instrument("Detritus check", components = list(
    sf_choices("ag5", 1:5, c("SD", "D", "N", "A", "SA")),
    sf_item("q1", "Item 1", type = "likert", choice_set = "ag5")
  ))

  before <- list.files(tempdir(), all.files = TRUE, no.. = TRUE)
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out, force = TRUE), add = TRUE)
  old <- options(surveyframe.use_quarto = FALSE)
  on.exit(options(old), add = TRUE)

  render_report(instr, output_file = out, format = "pdf")

  leftover <- grep("^(com\\.google\\.Chrome\\.|scoped_dir)",
                   setdiff(list.files(tempdir(), all.files = TRUE, no.. = TRUE),
                           before),
                   value = TRUE)
  expect_identical(leftover, character(0))
})
