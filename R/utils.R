# utils.R
# Shared internal helpers.

# Minimal HTML escaping for internal report rendering. Collapses a vector
# to one space-joined string first, so this is for a single scalar of
# text (a caption, a title, a message), never for a vector whose elements
# must stay in separate table cells: use htmltools_escape_each() for that.
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

# Same escaping, element by element, with no collapsing: for building one
# table header or one table row, where each vector element must become
# its own <th>/<td> rather than being merged into the neighbouring cell.
htmltools_escape_each <- function(x) {
  vapply(x, function(el) htmltools_escape(el), character(1))
}

sframe_as_data_frame <- function(x) {
  data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
}

# A single id/value -> label lookup for an instrument, covering item ids,
# scale ids, and every choice set's coded values, built once per instrument
# and reused everywhere a result needs to show what a respondent saw
# instead of the id or code it was stored under. First writer wins on a
# collision (an item/scale id takes priority over a choice value that
# happens to share the same string, since ids and choice codes are drawn
# from different, non-overlapping namespaces in practice).
sframe_label_lookup <- function(instrument) {
  if (is.null(instrument) || !inherits(instrument, "sframe")) {
    return(character(0))
  }
  lookup <- character(0)
  add <- function(id, label) {
    if (is.null(id) || !nzchar(id) || is.null(label) || is.na(label) || !nzchar(label)) return()
    if (!id %in% names(lookup)) lookup[[id]] <<- label
  }
  for (i in instrument$items %||% list()) add(i$id, i$label)
  for (s in instrument$scales %||% list()) add(s$id, s$label)
  for (cs in instrument$choices %||% list()) {
    vals <- as.character(cs$values %||% character(0))
    labs <- cs$labels %||% character(0)
    if (length(vals) == length(labs)) {
      for (k in seq_along(vals)) add(vals[k], labs[k])
    }
  }
  lookup
}

# Replace ids and coded values with their labels wherever they appear in a
# result table: row names, column names, and character-column cell values.
# A blanket, single-pass substitution rather than a per-test-type change,
# so every table shape (frequency, crosstab, group comparison, regression
# coefficients, and so on) reads in respondent-facing language without
# touching each runner's own table-building code.
sframe_humanize_table <- function(tbl, lookup) {
  if (!is.data.frame(tbl) || nrow(tbl) == 0 || length(lookup) == 0) {
    return(tbl)
  }
  relabel <- function(x) {
    hit <- lookup[x]
    ifelse(is.na(hit), x, unname(hit))
  }
  rn <- rownames(tbl)
  if (!is.null(rn) && !identical(rn, as.character(seq_len(nrow(tbl))))) {
    rownames(tbl) <- relabel(rn)
  }
  names(tbl) <- relabel(names(tbl))
  char_cols <- vapply(tbl, is.character, logical(1))
  tbl[char_cols] <- lapply(tbl[char_cols], relabel)
  tbl
}
