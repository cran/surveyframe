# git_link.R
#
# A bare SHA-256 hash proves a file is byte-identical to what produced it; it
# gives no diff, author, timestamp, or reason for a change. Git already
# solves that -- commit messages, authorship, and history -- so rather than
# have surveyframe compete with git, link_git_commit() has the manifest point
# at it. The SHA-256 hash still confirms the file on disk matches what a
# named, already-explained commit produced; it is not a substitute for the
# commit history, only a check that nothing was edited outside version
# control after that commit.
#
# Git is optional. No package here depends on it existing, and it is not a
# hard or Suggests dependency -- link_git_commit() shells out with system2()
# and degrades to an informative, non-error return value when git is
# unavailable or the path isn't a repository, so the rest of the package
# works identically whether or not the researcher uses git at all.

sframe_git_available <- function() {
  nzchar(Sys.which("git"))
}

sframe_git_run <- function(args, repo_path) {
  # system2() joins its `args` into a single shell command string without
  # quoting each element, so any argument containing a space (a commit
  # subject line, most obviously) silently splits into two shell words
  # instead of one. shQuote() every element to prevent that -- harmless for
  # flags like "-C" that never contain a space, required for the ones that
  # might.
  out <- tryCatch(
    suppressWarnings(system2(
      "git", shQuote(c("-C", repo_path, args)),
      stdout = TRUE, stderr = TRUE
    )),
    error = function(e) character(0)
  )
  status <- attr(out, "status") %||% 0L
  list(status = status, output = paste(out, collapse = "\n"))
}

#' Link an instrument to its current Git commit
#'
#' Records the current Git commit SHA and subject line for `repo_path`
#' alongside the instrument. This does not replace Git history -- it is a
#' pointer into it. The SHA-256 hash [write_sframe()] embeds in the
#' `.sframe` file confirms the file on disk matches what this specific,
#' already-explained commit produced; a reviewer reads the commit itself
#' for the "what changed and why."
#'
#' Git is entirely optional. When `repo_path` is not inside a Git
#' repository, or the `git` executable is not on the `PATH`, this returns a
#' clear, non-error result with `linked = FALSE` rather than aborting --
#' the rest of surveyframe never requires Git.
#'
#' @param instrument An `sframe` object.
#' @param repo_path Character. Path to check for a Git repository. Defaults
#'   to the current working directory.
#'
#' @return A list with `linked` (logical), and when `linked` is `TRUE`,
#'   `commit` (the full commit SHA) and `message` (the commit's subject
#'   line); when `linked` is `FALSE`, `reason` (a human-readable explanation:
#'   `"git not found"` or `"not a git repository"`).
#' @export
#' @seealso [amend_sframe()], [write_sframe()]
#'
#' @examples
#' item  <- sf_item("q1", "How satisfied are you?", type = "text")
#' instr <- sf_instrument("Demo", components = list(item))
#' link_git_commit(instr, repo_path = tempdir())
link_git_commit <- function(instrument, repo_path = ".") {
  sframe_check_instrument(instrument)

  if (!sframe_git_available()) {
    return(list(linked = FALSE, reason = "git not found"))
  }

  inside <- sframe_git_run(c("rev-parse", "--is-inside-work-tree"), repo_path)
  if (!identical(inside$status, 0L) || !identical(trimws(inside$output), "true")) {
    return(list(linked = FALSE, reason = "not a git repository"))
  }

  sha <- sframe_git_run(c("rev-parse", "HEAD"), repo_path)
  if (!identical(sha$status, 0L) || !nzchar(trimws(sha$output))) {
    return(list(linked = FALSE, reason = "not a git repository"))
  }

  msg <- sframe_git_run(c("log", "-1", "--format=%s"), repo_path)

  list(
    linked = TRUE,
    commit = trimws(sha$output),
    message = trimws(msg$output)
  )
}
