# tests/testthat/test-git-link.R
# link_git_commit() shells out to `git` and must degrade gracefully -- git is
# never a hard or Suggests dependency, so both "no git repository here" and
# "git found this repository" are tested, and both must return an ordinary
# list rather than erroring.

git_link_instr <- function() {
  item <- sf_item("q1", "How satisfied are you?", type = "text")
  sf_instrument("Demo", components = list(item))
}

test_that("link_git_commit() reports not linked outside a git repository", {
  skip_if_not(nzchar(Sys.which("git")), "git not available")
  instr <- git_link_instr()
  outside <- tempfile()
  dir.create(outside)
  on.exit(unlink(outside, recursive = TRUE), add = TRUE)
  result <- link_git_commit(instr, repo_path = outside)
  expect_false(result$linked)
  expect_equal(result$reason, "not a git repository")
})

test_that("link_git_commit() links inside a fresh git repository with a commit", {
  skip_if_not(nzchar(Sys.which("git")), "git not available")
  instr <- git_link_instr()
  repo <- tempfile()
  dir.create(repo)
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  system2("git", shQuote(c("-C", repo, "init", "-q")))
  system2("git", shQuote(c("-C", repo, "config", "user.email", "test@example.com")))
  system2("git", shQuote(c("-C", repo, "config", "user.name", "Test")))
  writeLines("placeholder", file.path(repo, "file.txt"))
  system2("git", shQuote(c("-C", repo, "add", "file.txt")))
  system2("git", shQuote(c("-C", repo, "commit", "-q", "-m", "Initial commit")))

  result <- link_git_commit(instr, repo_path = repo)
  expect_true(result$linked)
  expect_match(result$commit, "^[0-9a-f]{40}$")
  expect_equal(result$message, "Initial commit")
})

test_that("link_git_commit() reports not linked in a directory with no commits", {
  skip_if_not(nzchar(Sys.which("git")), "git not available")
  instr <- git_link_instr()
  repo <- tempfile()
  dir.create(repo)
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)
  system2("git", c("-C", repo, "init", "-q"))

  result <- link_git_commit(instr, repo_path = repo)
  expect_false(result$linked)
})

test_that("link_git_commit() requires an sframe instrument", {
  expect_error(link_git_commit(list(not = "an sframe")), class = "sframe_error")
})
