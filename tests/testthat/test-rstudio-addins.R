# tests/testthat/test-rstudio-addins.R
# The add-in layer is 3 launchers and one text insert. What can be tested
# without RStudio is that the bindings the menu declares actually exist, that
# they fail soft when rstudioapi is absent, and that the inserted skeleton
# really constructs a valid instrument.
#
# That last one is the point of the file. The implementation guide's original
# skeleton was written against an API that no longer exists: it passed id =
# to sf_instrument(), called the component list items =, and handed sf_item()
# an inline choices = argument. A skeleton that does not build a valid
# instrument is worse than no skeleton, because it teaches the wrong shape to
# whoever reaches for it first.

test_that("every binding named in addins.dcf exists as a function", {
  dcf <- system.file("rstudio", "addins.dcf", package = "surveyframe")
  skip_if(!nzchar(dcf) || !file.exists(dcf), "addins.dcf not installed")

  entries <- read.dcf(dcf)
  bindings <- as.character(entries[, "Binding"])

  expect_length(bindings, 4)
  expect_setequal(bindings, c("addin_launch_builder", "addin_launch_studio",
                              "addin_launch_dashboard",
                              "addin_insert_skeleton"))
  for (b in bindings) {
    expect_true(is.function(get(b, envir = asNamespace("surveyframe"))),
                info = b)
  }
})

test_that("addins.dcf declares a name and description for each binding", {
  dcf <- system.file("rstudio", "addins.dcf", package = "surveyframe")
  skip_if(!nzchar(dcf) || !file.exists(dcf), "addins.dcf not installed")

  entries <- read.dcf(dcf)
  expect_true(all(c("Name", "Description", "Binding", "Interactive") %in%
                    colnames(entries)))
  expect_true(all(nzchar(as.character(entries[, "Name"]))))
  expect_true(all(nzchar(as.character(entries[, "Description"]))))
})

test_that("the inserted skeleton parses, runs, and validates", {
  sk <- surveyframe:::sframe_addin_skeleton()
  expect_type(sk, "character")

  ex <- parse(text = sk)
  env <- new.env(parent = asNamespace("surveyframe"))
  expect_no_error(eval(ex, envir = env))

  inst <- get("instrument", envir = env)
  expect_s3_class(inst, "sframe")

  v <- validate_sframe(inst, strict = FALSE)
  expect_true(v$valid)
  expect_length(v$problems, 0)
})

test_that("the skeleton uses the current constructor API, not the stale one", {
  sk <- surveyframe:::sframe_addin_skeleton()

  # the shapes the guide's version got wrong
  expect_no_match(sk, "id\\s*=\\s*\"my_study\"")
  expect_no_match(sk, "items\\s*=\\s*list\\(\\s*sf_item")
  expect_no_match(sk, "choices\\s*=\\s*sf_choices")

  # and the shapes it should use
  expect_match(sk, "components  = list(", fixed = TRUE)
  expect_match(sk, "choice_set = \"agree5\"", fixed = TRUE)
})

test_that("the skeleton round-trips through write and read", {
  # A skeleton that builds an instrument which cannot be saved would send a
  # first-time user straight into a failure on their next step.
  sk <- surveyframe:::sframe_addin_skeleton()
  env <- new.env(parent = asNamespace("surveyframe"))
  eval(parse(text = sk), envir = env)

  path <- tempfile(fileext = ".sframe")
  expect_no_error(write_sframe(get("instrument", envir = env), path))
  expect_no_error(read_sframe(path))
})

test_that("the add-ins fail soft when rstudioapi is unavailable", {
  # These are interactive conveniences, so a missing suggested package must
  # produce a message and NULL rather than an error inside the IDE.
  testthat::local_mocked_bindings(
    sframe_addin_ready = function() FALSE,
    .package = "surveyframe"
  )
  for (fn in c("addin_launch_builder", "addin_launch_studio",
               "addin_launch_dashboard", "addin_insert_skeleton")) {
    f <- get(fn, envir = asNamespace("surveyframe"))
    expect_null(f(), info = fn)
  }
})

test_that("no file outside R/rstudio_addins.R calls rstudioapi", {
  # The package must behave identically outside RStudio.
  r_dir <- system.file("R", package = "surveyframe")
  skip_if(!nzchar(r_dir), "installed package has no source R/ to scan")
  src <- list.files("../../R", pattern = "[.]R$", full.names = TRUE)
  skip_if(length(src) == 0, "source R/ not reachable from the test dir")

  offenders <- Filter(function(f) {
    !identical(basename(f), "rstudio_addins.R") &&
      any(grepl("rstudioapi::", readLines(f, warn = FALSE), fixed = TRUE))
  }, src)
  expect_length(offenders, 0)
})
