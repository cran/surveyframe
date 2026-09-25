# git_link.R
#
# A content hash identifies an instrument's content. It gives no diff, author,
# timestamp or reason for a change. Git records those, so link_git_commit()
# points at Git. Given the tracked file's path, it also compares the instrument
# with the file as committed, and reports that comparison as `verified`.
# Without a path it is a pointer to HEAD and nothing more.
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
#' Records the current Git commit SHA and subject line for `repo_path`. It is
#' a pointer into Git history, where a reviewer reads what changed and why.
#'
#' Given `path`, the tracked `.sframe` file, it also compares the instrument
#' with that file as committed at HEAD, and sets `verified = TRUE` only when
#' their content hashes match. Without `path`, nothing is compared and
#' `verified` is `FALSE`. A verified link shows the instrument matches a
#' committed file. It does not show who wrote the instrument or when, beyond
#' what the commit itself records.
#'
#' Git is entirely optional. When `repo_path` is not inside a Git
#' repository, or the `git` executable is not on the `PATH`, this returns a
#' clear, non-error result with `linked = FALSE` rather than aborting --
#' the rest of surveyframe never requires Git.
#'
#' @param instrument An `sframe` object.
#' @param repo_path Character. Path to check for a Git repository. Defaults
#'   to the current working directory.
#' @param path Character or `NULL`. The instrument's `.sframe` file, relative
#'   to `repo_path`. When given, the instrument is compared with the file as
#'   committed at HEAD.
#'
#' @return A list with `linked` (logical), and when `linked` is `TRUE`,
#'   `commit` (the full commit SHA), `message` (the commit's subject line),
#'   `path`, and `verified` (logical). `reason` explains an unlinked result
#'   (`"git not found"` or `"not a git repository"`) or an unverified one.
#' @export
#' @seealso [amend_sframe()], [write_sframe()]
#'
#' @examples
#' item  <- sf_item("q1", "How satisfied are you?", type = "text")
#' instr <- sf_instrument("Demo", components = list(item))
#' link_git_commit(instr, repo_path = tempdir())
link_git_commit <- function(instrument, repo_path = ".", path = NULL) {
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

  out <- list(
    linked = TRUE,
    commit = trimws(sha$output),
    message = trimws(msg$output),
    path = path,
    verified = FALSE
  )
  if (is.null(path)) {
    out$reason <- "no path given, so the instrument was not compared with the repository"
    return(out)
  }
  committed <- sframe_git_run(c("show", paste0("HEAD:./", path)), repo_path)
  if (!identical(committed$status, 0L)) {
    out$reason <- sprintf("'%s' is not committed at HEAD", path)
    return(out)
  }
  stored <- tryCatch(
    jsonlite::fromJSON(committed$output, simplifyVector = FALSE)$hash$value,
    error = function(e) NULL)
  current <- sframe_hash_value(as_sframe(validate_sframe(instrument, strict = TRUE)))
  if (identical(stored, current)) {
    out$verified <- TRUE
  } else {
    out$reason <- sprintf("the instrument differs from '%s' as committed at HEAD", path)
  }
  out
}
