# tests/testthat/test-rstudio-addins.R
# The add-in layer is 3 launchers and one text insert. With rstudioapi mocked,
# these tests check that the bindings the menu declares exist, that each one
# opens the right tool, that every failure inside the IDE becomes one message
# and NULL, and that the inserted skeleton constructs a valid instrument.
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

# --- The interactive contract, with RStudio stood in for -------------------

# Mocks rstudioapi as if RStudio were running, with the given answers.
local_rstudio <- function(select = function(...) NULL,
                          context = list(id = "doc1"),
                          insert = function(...) invisible(NULL),
                          available = TRUE,
                          env = parent.frame()) {
  skip_if_not_installed("rstudioapi")
  testthat::local_mocked_bindings(
    isAvailable = function(...) available,
    selectFile = select,
    getSourceEditorContext = function(...) context,
    insertText = insert,
    .package = "rstudioapi", .env = env
  )
}

# Replaces the 3 launchers with recorders. Returns the environment the calls
# land in.
local_launchers <- function(env = parent.frame()) {
  calls <- new.env()
  record <- function(name) function(...) {
    calls[[name]] <- list(...)
    invisible(name)
  }
  testthat::local_mocked_bindings(
    launch_builder = record("builder"),
    launch_studio = record("studio"),
    launch_dashboard = record("dashboard"),
    .package = "surveyframe", .env = env
  )
  calls
}

skeleton_file <- function() {
  env <- new.env(parent = asNamespace("surveyframe"))
  eval(parse(text = surveyframe:::sframe_addin_skeleton()), envir = env)
  path <- tempfile(fileext = ".sframe")
  write_sframe(get("instrument", envir = env), path)
  path
}

test_that("with rstudioapi installed but RStudio absent, each add-in stops with one message", {
  local_rstudio(available = FALSE)
  calls <- local_launchers()
  for (fn in c("addin_launch_builder", "addin_launch_studio",
               "addin_launch_dashboard", "addin_insert_skeleton")) {
    f <- get(fn, envir = asNamespace("surveyframe"))
    msgs <- testthat::capture_messages(res <- f())
    expect_null(res, info = fn)
    expect_length(msgs, 1)
    expect_match(msgs, "run inside RStudio", info = fn)
  }
  expect_length(ls(calls), 0)
})

test_that("the builder and workspace add-ins open the right tool", {
  local_rstudio()
  calls <- local_launchers()
  addin_launch_builder()
  addin_launch_studio()
  expect_setequal(ls(calls), c("builder", "studio"))
  expect_length(calls$studio, 0)
})

test_that("Analyse an existing instrument opens Studio on the responses screen", {
  path <- skeleton_file()
  local_rstudio(select = function(...) path)
  calls <- local_launchers()
  addin_launch_dashboard()
  expect_identical(ls(calls), "studio")
  expect_identical(calls$studio$screen, "responses")
  expect_s3_class(calls$studio$instrument, "sframe")
  expect_identical(calls$studio$instrument$meta$title, "My study")
})

test_that("cancelling the file dialog does nothing and says nothing", {
  local_rstudio(select = function(...) NULL)
  calls <- local_launchers()
  expect_silent(res <- addin_launch_dashboard())
  expect_null(res)
  expect_length(ls(calls), 0)
})

test_that("an error from the file dialog becomes one message", {
  local_rstudio(select = function(...) stop("dialog unavailable"))
  calls <- local_launchers()
  msgs <- testthat::capture_messages(res <- addin_launch_dashboard())
  expect_null(res)
  expect_identical(msgs, "surveyframe add-in: dialog unavailable\n")
  expect_length(ls(calls), 0)
})

test_that("an .sframe that will not load becomes one message, before any launch", {
  bad <- tempfile(fileext = ".sframe")
  writeLines("{ not an instrument", bad)
  local_rstudio(select = function(...) bad)
  calls <- local_launchers()
  msgs <- testthat::capture_messages(res <- addin_launch_dashboard())
  expect_null(res)
  expect_length(msgs, 1)
  expect_match(msgs, "^surveyframe add-in: ")
  expect_length(ls(calls), 0)
})

test_that("a launcher's error becomes one message and NULL", {
  local_rstudio()
  testthat::local_mocked_bindings(
    launch_builder = function(...) stop("port in use"),
    launch_studio = function(...) stop("shiny missing"),
    .package = "surveyframe"
  )
  for (fn in c("addin_launch_builder", "addin_launch_studio")) {
    f <- get(fn, envir = asNamespace("surveyframe"))
    msgs <- testthat::capture_messages(res <- f())
    expect_null(res, info = fn)
    expect_length(msgs, 1)
    expect_match(msgs, "^surveyframe add-in: (port in use|shiny missing)")
  }
})

test_that("the exported launchers still raise their own errors", {
  # sframe_run_addin() is for the menu only. Programmatic calls keep erroring.
  expect_error(launch_studio(screen = "no-such-screen"))
})

test_that("inserting with no source document open explains itself", {
  inserted <- FALSE
  local_rstudio(context = NULL, insert = function(...) inserted <<- TRUE)
  msgs <- testthat::capture_messages(res <- addin_insert_skeleton())
  expect_null(res)
  expect_length(msgs, 1)
  expect_match(msgs, "open an R script")
  expect_false(inserted)
})

test_that("the starter instrument is inserted into the active document", {
  got <- NULL
  local_rstudio(context = list(id = "doc42"),
                insert = function(...) got <<- list(...))
  expect_null(addin_insert_skeleton())
  expect_identical(got$id, "doc42")
  expect_identical(got$text, surveyframe:::sframe_addin_skeleton())
})

test_that("an insertion error becomes one message", {
  local_rstudio(insert = function(...) stop("document is read-only"))
  msgs <- testthat::capture_messages(res <- addin_insert_skeleton())
  expect_null(res)
  expect_identical(msgs, "surveyframe add-in: document is read-only\n")
})

test_that("the menu copy names what each add-in does", {
  dcf <- system.file("rstudio", "addins.dcf", package = "surveyframe")
  skip_if(!nzchar(dcf) || !file.exists(dcf), "addins.dcf not installed")
  entries <- read.dcf(dcf)
  copy <- stats::setNames(as.list(as.data.frame(t(entries[, c("Name", "Description")]),
                                                stringsAsFactors = FALSE)),
                          entries[, "Binding"])
  expect_identical(copy$addin_launch_builder, c(
    "surveyframe: Design an instrument",
    "Open the client-side SurveyBuilder to design a questionnaire and analysis plan."))
  expect_identical(copy$addin_launch_studio, c(
    "surveyframe: Open analysis workspace",
    "Open SurveyStudio to preview an instrument, load responses, run analyses, and export reports."))
  expect_identical(copy$addin_launch_dashboard, c(
    "surveyframe: Analyse an existing instrument",
    "Choose an .sframe file and open SurveyStudio at the response-loading step."))
  expect_identical(copy$addin_insert_skeleton, c(
    "surveyframe: Insert starter instrument",
    "Insert a valid questionnaire, scale, and reliability-plan template in the active R script."))
  # SurveyBuilder is a client-side HTML page, not a Shiny app.
  expect_false(any(grepl("Shiny HTML", entries[, "Description"], fixed = TRUE)))
  expect_true(all(startsWith(entries[, "Name"], "surveyframe: ")))
})

test_that("the starter instrument shows the next steps without taking them", {
  sk <- surveyframe:::sframe_addin_skeleton()
  expect_match(sk, "validation <- validate_sframe(instrument)", fixed = TRUE)
  expect_match(sk, "# write_sframe(instrument, \"my-study.sframe\")", fixed = TRUE)
  # Commented out, so running the inserted script writes no file.
  expect_no_match(sk, "^write_sframe", perl = TRUE)
  expect_no_match(sk, "\nwrite_sframe")
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
