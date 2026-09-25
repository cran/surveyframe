# tests/testthat/test-amendment-write-boundary.R
#
# E2. The amendment log was checked by nothing at the write boundary. Reading a
#     file, editing it in memory and writing it produced a self-consistent file
#     with no disclosure, and a log with removed or reordered entries wrote just
#     as happily. The help promised read_sframe() rejected any edit that did not
#     go through amend_sframe(), which only held for edits made to the file on
#     disk.
# Batch 5, finding 8. amend_sframe() fingerprinted the after-state before
#     validation set meta$validated, so new_hash described a state that was
#     never written.
# Batch 5, finding 9. The caller chose the tier, so a plan or model change could
#     be recorded as "pipeline" with no deviation report.
# Batch 5, finding 4. link_git_commit() returned HEAD with linked = TRUE without
#     comparing the instrument to anything in the repository.
# G5 and G3. The provenance, read and write help promised more than the code does.

base_instrument <- function(label = "How satisfied are you?") {
  sf_instrument(title = "Revisions", components = list(
    sf_item("q1", label, "text"),
    sf_item("q2", "Anything else?", "text")))
}

written <- function(instrument) {
  path <- tempfile(fileext = ".sframe")
  write_sframe(instrument, path)
  path
}

test_that("E2: an undisclosed in-memory edit of a loaded file is refused on write", {
  loaded <- read_sframe(written(base_instrument()))
  loaded$items[[1]]$label <- "Quietly changed"
  expect_error(write_sframe(loaded, tempfile(fileext = ".sframe")),
               "amend_sframe", class = "sframe_error")
})

test_that("E2: the same edit disclosed through amend_sframe() writes and reads back", {
  loaded <- read_sframe(written(base_instrument()))
  edited <- loaded
  edited$items[[1]]$label <- "Openly changed"
  amended <- amend_sframe(loaded, edited, reason_code = "instrument_revision",
                          reason_text = "Clarified wording.",
                          deviation_report = "Wording only.")
  path <- tempfile(fileext = ".sframe")
  expect_no_error(write_sframe(amended, path))
  back <- read_sframe(path)
  expect_identical(nrow(amendment_log(back)), 1L)
  expect_identical(back$items[[1]]$label, "Openly changed")
})

test_that("E2: a change made after the last amendment is refused", {
  loaded <- read_sframe(written(base_instrument()))
  edited <- loaded
  edited$items[[1]]$label <- "Disclosed"
  amended <- amend_sframe(loaded, edited, reason_code = "instrument_revision",
                          reason_text = "Clarified.", deviation_report = "Wording.")
  amended$items[[2]]$label <- "Not disclosed"
  expect_error(write_sframe(amended, tempfile(fileext = ".sframe")),
               class = "sframe_error")
})

two_amendments <- function() {
  loaded <- read_sframe(written(base_instrument()))
  e1 <- loaded; e1$items[[1]]$label <- "First revision"
  a1 <- amend_sframe(loaded, e1, reason_code = "instrument_revision",
                     reason_text = "One.", deviation_report = "Wording.")
  e2 <- a1; e2$items[[2]]$label <- "Second revision"
  a2 <- amend_sframe(a1, e2, reason_code = "instrument_revision",
                     reason_text = "Two.", deviation_report = "Wording.")
  read_sframe(written(a2))
}

test_that("E2: removing an amendment from a loaded log is refused", {
  loaded <- two_amendments()
  loaded$amendments <- loaded$amendments[2]
  expect_error(write_sframe(loaded, tempfile(fileext = ".sframe")),
               class = "sframe_error")
})

test_that("E2: reordering amendments is refused", {
  loaded <- two_amendments()
  fresh <- base_instrument("Second revision")
  fresh$items[[2]]$label <- "Second revision"
  fresh$amendments <- rev(loaded$amendments)
  expect_error(write_sframe(fresh, tempfile(fileext = ".sframe")),
               class = "sframe_error")
})

test_that("E2: new_instrument = TRUE declares a changed file a new instrument", {
  loaded <- read_sframe(written(base_instrument()))
  loaded$items[[1]]$label <- "A new study"
  path <- tempfile(fileext = ".sframe")
  expect_no_error(write_sframe(loaded, path, new_instrument = TRUE))
  expect_identical(read_sframe(path)$items[[1]]$label, "A new study")
})

test_that("E2: a freshly built instrument writes with no amendment needed", {
  expect_no_error(written(base_instrument()))
})

test_that("8: an amendment's new_hash is the content that is written", {
  previous <- base_instrument()
  after <- base_instrument("Built fresh")
  amended <- amend_sframe(previous, after, reason_code = "instrument_revision",
                          reason_text = "Fresh.", deviation_report = "Wording.")
  back <- read_sframe(written(amended))
  expect_identical(amendment_log(back)$new_hash, sframe_content_hash(back))
})

test_that("9: a plan change cannot be recorded as a pipeline amendment", {
  previous <- base_instrument()
  after <- base_instrument()
  after$analysis_plan <- list(list(id = "b1", research_question = "Q",
                                   method = "frequency", roles = list(variable = "q1")))
  expect_error(
    amend_sframe(previous, after, reason_code = "data_correction",
                 reason_text = "Cleaned.", tier = "pipeline"),
    "design", class = "sframe_error")
  expect_error(
    amend_sframe(previous, after, reason_code = "data_correction",
                 reason_text = "Cleaned."),
    "deviation_report", class = "sframe_error")
})

test_that("4: link_git_commit() verifies the instrument only against a committed file", {
  skip_if(!nzchar(Sys.which("git")), "git not available")
  repo <- tempfile("repo"); dir.create(repo)
  git <- function(...) system2("git", c("-C", shQuote(repo), ...), stdout = TRUE, stderr = TRUE)
  git("init", "-q"); git("config", "user.email", "t@example.org"); git("config", "user.name", "T")
  ins <- base_instrument()
  path <- file.path(repo, "study.sframe")
  write_sframe(ins, path)
  git("add", "study.sframe"); git("commit", "-q", "-m", shQuote("add instrument"))

  same <- link_git_commit(read_sframe(path), repo_path = repo, path = "study.sframe")
  expect_true(same$linked)
  expect_true(same$verified)

  changed <- base_instrument("Different content")
  other <- link_git_commit(changed, repo_path = repo, path = "study.sframe")
  expect_false(other$verified)

  pointer <- link_git_commit(ins, repo_path = repo)
  expect_false(pointer$verified)
  expect_match(pointer$reason, "path")
})

test_that("G5 and G3: provenance, read and write help promise only what the code does", {
  src <- function(f) {
    p <- sframe_source_path("R", f)
    skip_if(is.na(p), "no source tree here")
    skip_if(!file.exists(p), "package source not available")
    paste(readLines(p, warn = FALSE), collapse = "\n")
  }
  amend <- src("amendments.R")
  expect_false(grepl("byte-identical", amend, fixed = TRUE))
  expect_false(grepl("hard-aborts on any edit that never went through", amend, fixed = TRUE))
  git <- src("git_link.R")
  expect_false(grepl("confirms the file on disk matches what this specific", git, fixed = TRUE))
  rw <- src("read_write_sframe.R")
  expect_false(grepl("verified on load unless `validate = FALSE`", rw, fixed = TRUE))
  expect_false(grepl("unless\n#' the object already carries a valid status", rw, fixed = TRUE))
})

test_that("E2 in SurveyStudio: an edited loaded draft reports its revision problem", {
  path <- written(base_instrument())
  loaded <- read_sframe(path)
  state <- sframe_builder_state_from_instrument(loaded)
  expect_false(is.null(state$origin))

  draft_args <- function(st) {
    list(meta = st$meta, choices = st$choices, items = st$items, scales = st$scales,
         branching = st$branching, checks = st$checks,
         analysis_plan = st$analysis_plan, models = st$models, render = st$render,
         amendments = st$amendments, origin = st$origin)
  }
  unchanged <- do.call(sframe_builder_validate_draft, draft_args(state))
  expect_null(unchanged$revision_problem)

  state$items[[1]]$label <- "Edited in Studio"
  edited <- do.call(sframe_builder_validate_draft, draft_args(state))
  expect_true(edited$valid)
  expect_match(edited$revision_problem, "amend_sframe")
  expect_error(write_sframe(edited$instrument, tempfile(fileext = ".sframe")),
               class = "sframe_error")
})

test_that("E2: a broken link inside the amendment log is refused on its own", {
  # The last entry still matches the content and no load record exists, so only
  # the check that each entry follows the one before it can catch this.
  loaded <- two_amendments()
  fresh <- loaded
  attr(fresh, "sframe_origin") <- NULL
  fresh$amendments[[2]]$previous_hash <- strrep("0", 64)
  expect_error(write_sframe(fresh, tempfile(fileext = ".sframe")),
               "out of sequence", class = "sframe_error")
})
