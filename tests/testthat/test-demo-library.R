# tests/testthat/test-demo-library.R
# The demo library exists to serve twice over: as fixtures that point at one
# method when something breaks, and as worked examples somebody follows for
# their own survey. Both jobs need every demo to load, validate, read back and
# run, so this loops over the index rather than naming demos one at a time. A
# demo added later is covered without touching this file.
#
# The coverage assertions at the end are the point of the file. A hand-written
# table claiming full coverage is worth nothing, since it shares the blind spot
# of the table it checks, so they are derived from the package's own registries
# at run time.

demo_names <- function() sframe_demos()$name

test_that("every demo in the index is on disk and loads", {
  for (nm in demo_names()) {
    d <- sframe_demo(nm)
    expect_s3_class(d$instrument, "sframe")
    expect_true(file.exists(d$instrument_path))
    expect_true(file.exists(d$responses_path))
    expect_true(file.exists(d$codebook_path))
    expect_true(nrow(d$responses) > 0)
  }
})

test_that("every demo validates, reads back, and runs its plan without an error block", {
  for (nm in demo_names()) {
    d <- sframe_demo(nm)
    v <- validate_sframe(d$instrument, strict = FALSE)
    expect_true(v$valid)

    res <- run_analysis_plan(d$responses, d$instrument)
    errs <- Filter(function(b) !is.null(b$error), res)
    # A method whose optional package is absent on this machine is a
    # legitimate skip. The package guards those deliberately, and the demo
    # still teaches the method to anyone who has it installed.
    errs <- Filter(function(b) !grepl(
      "install|not installed|requires the|cannot be loaded|is required",
      b$error, ignore.case = TRUE), errs)
    # A block with an unavailable method still counts as a block and still
    # renders, so a demo can look complete while a section is dead. This is
    # what caught "frequencies" where the method is "frequency".
    expect_length(errs, 0)
    if (length(errs)) {
      fail(paste(nm, "block errored:", errs[[1]]$error))
    }
  }
})

test_that("the codebook carries value labels, which is what a plain CSV loses", {
  # codebook_report() gives the variable label and names the choice set, and
  # stops there, so a student importing dm_1 into SPSS gets bare integers. The
  # shipped codebook carries both.
  for (nm in demo_names()) {
    cb <- utils::read.csv(sframe_demo(nm)$codebook_path, stringsAsFactors = FALSE)
    expect_true(all(c("item_id", "item_label", "value", "value_label") %in%
                      names(cb)), label = nm)
    instr <- sframe_demo(nm)$instrument
    has_choices <- vapply(instr$items,
                          function(it) !is.null(it$choice_set), logical(1))
    if (any(has_choices)) {
      expect_true(any(!is.na(cb$value_label)))
    }
  }
})

test_that("branded = TRUE applies the branding and leaves the file alone", {
  nm <- "two_group"
  path <- sframe_demo(nm)$instrument_path
  before <- tools::md5sum(path)

  plain <- sframe_demo(nm)
  branded <- sframe_demo(nm, branded = TRUE)
  expect_identical(branded$instrument$render, sframe_demo_branding())
  expect_false(identical(plain$instrument$render, branded$instrument$render))
  # One branding definition serves every demo, and nothing on disk moves.
  expect_identical(tools::md5sum(path), before)
})

test_that("sframe_demo() rejects an unknown name rather than guessing", {
  expect_error(sframe_demo("no_such_demo"), "Unknown demo")
})

test_that("sframe_demo() and sframe_demo_qmd() point to sframe_demos() when name is omitted", {
  # Calling either with no argument previously surfaced R's generic
  # "argument \"name\" is missing, with no default", which names neither
  # the fix (sframe_demos()) nor an example.
  expect_error(sframe_demo(), "sframe_demos\\(\\)")
  expect_error(sframe_demo(), "name.*required")
  expect_error(sframe_demo_qmd(), "sframe_demos\\(\\)")
  expect_error(sframe_demo_qmd(), "name.*required")
})

test_that("sframe_demo_qmd() writes a runnable notebook with no placeholders left", {
  # Base R rather than withr, which this package deliberately does not
  # depend on: an undeclared withr import was removed once before.
  dir <- tempfile("demo-qmd-"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  p <- sframe_demo_qmd("two_group", dir = dir)
  expect_true(file.exists(p))
  txt <- readLines(p, warn = FALSE)
  expect_false(any(grepl("{{", txt, fixed = TRUE)))
  expect_true(any(grepl('sframe_demo("two_group")', txt, fixed = TRUE)))
  # and it refuses to clobber by default
  expect_error(sframe_demo_qmd("two_group", dir = dir), "already exists")
  expect_no_error(sframe_demo_qmd("two_group", dir = dir, overwrite = TRUE))
})

test_that("sframe_demo_qmd() and sframe_analysis_qmd() report where they wrote to", {
  # Both returned their path only invisibly, with no message, so a user
  # calling them for a side effect (as the help page's own example does,
  # sframe_demo_qmd("two_group", dir = tempdir())) saw no confirmation at
  # all that anything happened or where it landed.
  dir <- tempfile("qmd-msg-"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  expect_message(sframe_demo_qmd("two_group", dir = dir), "written to.*two_group\\.qmd")

  demo <- sframe_demo("likert_scale")
  expect_message(
    sframe_analysis_qmd(demo$instrument, demo$responses, dir = dir),
    "written to.*\\.qmd"
  )
})

test_that("sframe_analysis_qmd() writes a runnable notebook for any instrument, not just a bundled demo", {
  dir <- tempfile("analysis-qmd-"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  demo <- sframe_demo("two_group")
  out <- sframe_analysis_qmd(demo$instrument, demo$responses, dir = dir)

  expect_true(file.exists(out$qmd))
  expect_true(file.exists(out$sframe))
  expect_true(file.exists(out$csv))

  txt <- readLines(out$qmd, warn = FALSE)
  expect_false(any(grepl("{{", txt, fixed = TRUE)))
  expect_true(any(grepl("read_sframe(", txt, fixed = TRUE)))
  expect_true(any(grepl("read_responses(", txt, fixed = TRUE)))

  # The notebook's own load/run chunks actually work against the files
  # sframe_analysis_qmd() wrote, not just parse: this is the exact defect
  # class the demo library's own build generator was built to catch, a
  # notebook that looks right but fails the moment somebody runs it.
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(dir)
  instr <- read_sframe(basename(out$sframe))
  resp  <- read_responses(basename(out$csv), instr, strict = FALSE)
  results <- run_analysis_plan(resp, instr)
  expect_gt(length(results), 0)

  # refuses to clobber by default, same convention as sframe_demo_qmd()
  expect_error(sframe_analysis_qmd(demo$instrument, demo$responses, dir = dir), "already exists")
  expect_no_error(sframe_analysis_qmd(demo$instrument, demo$responses, dir = dir, overwrite = TRUE))
})

test_that("sframe_export_labelled() attaches variable and value labels", {
  skip_if_not_installed("haven")
  d <- sframe_demo("likert_scale")
  tmp <- tempfile("labelled-"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  out <- file.path(tmp, "x.sav")
  sframe_export_labelled(d$responses, d$instrument, out)
  back <- haven::read_sav(out)

  expect_identical(attr(back$org_1, "label"), "The event ran to time.")
  labs <- attr(back$org_1, "labels")
  expect_true(!is.null(labs))
  expect_true("Strongly disagree" %in% names(labs))

  expect_error(sframe_export_labelled(d$responses, d$instrument,
                                      file.path(tempdir(), "x.csv")),
               "must end in")
})

test_that("the verification demo tells a clean file from a tampered one", {
  # The most persuasive thing in the library for a reader who wants to know
  # what the hash is actually for.
  clean <- sframe_demo("verification")$instrument_path
  expect_no_error(read_sframe(clean))

  tampered <- file.path(dirname(clean), "verification_tampered.sframe")
  skip_if(!file.exists(tampered), "tampered fixture not bundled")
  expect_error(read_sframe(tampered), "[Ii]ntegrity")
})

test_that("the instrument_revision demo records a disclosed amendment", {
  d <- sframe_demo("instrument_revision")
  log <- as.data.frame(amendment_log(d$instrument))
  expect_equal(nrow(log), 1)
  expect_identical(log$reason_code[1], "instrument_revision")
  expect_true(nzchar(log$reason_text[1]))
})

test_that("COVERAGE: every analysis method the package dispatches has a demo", {
  # Derived from the dispatch itself, so a method added later shows up here as
  # uncovered instead of quietly going untaught.
  covered <- unique(unlist(lapply(demo_names(), function(nm) {
    vapply(sf_plan(sframe_demo(nm)$instrument),
           function(b) b$method %||% "", character(1))
  })))
  covered <- covered[nzchar(covered)]

  dispatched <- surveyframe:::sframe_dispatch_methods()
  missing <- setdiff(dispatched, covered)
  expect_identical(missing, character(0),
                   info = paste("methods with no demo:",
                                paste(missing, collapse = ", ")))
})

test_that("COVERAGE: every item type the package offers appears in a demo", {
  types <- eval(formals(sf_item)$type)
  used <- unique(unlist(lapply(demo_names(), function(nm) {
    vapply(sframe_demo(nm)$instrument$items,
           function(it) it$type, character(1))
  })))
  missing <- setdiff(types, used)
  expect_identical(missing, character(0),
                   info = paste("item types with no demo:",
                                paste(missing, collapse = ", ")))
})
