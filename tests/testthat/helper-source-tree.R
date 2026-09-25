# tests/testthat/helper-source-tree.R
#
# Several tests assert on the package's own source: a roxygen block's wording, a
# template's markup, the NAMESPACE. Those files exist in a source tree and not
# in an installed package, so reading them through file.path("..", "..", "R")
# works under devtools::test() and fails under R CMD check, where the tests run
# beside an installed copy. That cost a check ERROR with 30 failures.
#
# sframe_source_path() returns the path when a source tree is at hand, and NA
# otherwise. Files that ship inside the package are read through
# sframe_installed_path(), which works in both places, so those tests keep
# running under check rather than skipping.

# A file that ships with the package: inst/ contents, man/, NAMESPACE, NEWS.md.
# system.file() finds it in an installed package, and the source tree is the
# fallback for the inst/ prefix that installation strips.
sframe_installed_path <- function(...) {
  parts <- c(...)
  # inst/x installs as x, and pkgload's system.file() refuses a path beginning
  # with "inst" outright, so the prefix is dropped before asking.
  lookup <- if (identical(parts[1], "inst") && length(parts) > 1) {
    parts[-1]
  } else {
    parts
  }
  found <- tryCatch(
    system.file(paste(lookup, collapse = "/"), package = "surveyframe"),
    error = function(e) "")
  if (nzchar(found) && file.exists(found)) return(found)

  src <- do.call(file.path, as.list(c("..", "..", parts)))
  if (file.exists(src)) return(src)
  NA_character_
}

# A file that exists only in a source tree, R/*.R above all.
sframe_source_path <- function(...) {
  src <- do.call(file.path, as.list(c("..", "..", c(...))))
  if (file.exists(src)) return(src)
  NA_character_
}

# The text of a source file, or a skip where no source tree is at hand.
sframe_source_text <- function(...) {
  path <- sframe_source_path(...)
  testthat::skip_if(is.na(path),
                    paste0("no source tree here: ", paste(c(...), collapse = "/")))
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# The text of a file that ships with the package, from wherever it is.
sframe_installed_text <- function(...) {
  path <- sframe_installed_path(...)
  testthat::skip_if(is.na(path),
                    paste0("file not found: ", paste(c(...), collapse = "/")))
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# The package's exports, from the installed namespace where there is one and the
# source NAMESPACE otherwise.
sframe_exports <- function() {
  if ("surveyframe" %in% loadedNamespaces()) {
    return(getNamespaceExports("surveyframe"))
  }
  path <- sframe_installed_path("NAMESPACE")
  testthat::skip_if(is.na(path), "no NAMESPACE found")
  lines <- readLines(path, warn = FALSE)
  gsub('"', "", sub("^export\\((.*)\\)$", "\\1",
                    grep("^export\\(", lines, value = TRUE)))
}
