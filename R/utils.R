# utils.R
# Shared internal helpers.

# Minimal HTML escaping for internal report rendering.
htmltools_escape <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return("")
  }

  x <- paste(as.character(x), collapse = " ")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

sframe_as_data_frame <- function(x) {
  data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
}
