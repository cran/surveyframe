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

  # A choice code is scoped to its choice set, and this dictionary has no
  # column context to scope by. A code meaning 2 different things in 2 sets
  # used to take whichever label was read first, so "1" from a frequency item
  # printed as "Strongly disagree". A code carrying one meaning across the
  # whole instrument still relabels; an ambiguous one stays as its code, and
  # sframe_item_value_labels() gives the labels where the item is known.
  code_labels <- list()
  for (cs in instrument$choices %||% list()) {
    vals <- as.character(cs$values %||% character(0))
    labs <- as.character(cs$labels %||% character(0))
    if (length(vals) != length(labs)) next
    for (k in seq_along(vals)) {
      code_labels[[vals[k]]] <- unique(c(code_labels[[vals[k]]], labs[k]))
    }
  }
  for (code in names(code_labels)) {
    if (length(code_labels[[code]]) == 1) add(code, code_labels[[code]])
  }
  lookup
}

# The value labels for one item, read through the choice set it names. This is
# the context-aware counterpart to sframe_label_lookup(), for a caller that
# knows which item a column holds.
sframe_item_value_labels <- function(instrument, item_id) {
  if (is.null(instrument) || !inherits(instrument, "sframe")) {
    return(character(0))
  }
  ids <- vapply(instrument$items %||% list(),
                function(i) as.character(i$id %||% "")[1], character(1))
  hit <- match(as.character(item_id)[1], ids)
  if (is.na(hit)) return(character(0))
  set_id <- as.character(instrument$items[[hit]]$choice_set %||% "")[1]
  if (!nzchar(set_id)) return(character(0))
  for (cs in instrument$choices %||% list()) {
    if (!identical(as.character(cs$id %||% "")[1], set_id)) next
    vals <- as.character(cs$values %||% character(0))
    labs <- as.character(cs$labels %||% character(0))
    if (length(vals) != length(labs)) return(character(0))
    return(stats::setNames(labs, vals))
  }
  character(0)
}

# Replace ids and coded values with their labels wherever they appear in a
# result table: row names, column names, and character-column cell values.
# A blanket, single-pass substitution rather than a per-test-type change,
# so every table shape (frequency, crosstab, group comparison, regression
# coefficients, and so on) reads in respondent-facing language without
# touching each runner's own table-building code.
#
# `exclude_cols` opts specific columns (by their pre-relabel name) out of
# cell-value substitution, for a column whose values are free text rather
# than coded values (a text-family result's `term`/`term_a`/`match`
# column): a respondent's own word can otherwise collide with an unrelated
# item's choice CODE anywhere in the instrument and get silently swapped
# for that item's choice LABEL (found in review_050; a comment containing
# "pool" read back as "Pool area" because some other item happened to code
# a choice "pool"). Column names and row names are still relabelled
# normally; only cell values in an excluded column are left alone, so a
# genuinely coded column on the same table (a `group` column, say) keeps
# humanising as before.
sframe_humanize_table <- function(tbl, lookup, exclude_cols = character(0)) {
  if (!is.data.frame(tbl) || nrow(tbl) == 0 || length(lookup) == 0) {
    return(tbl)
  }
  relabel <- function(x) {
    hit <- lookup[x]
    ifelse(is.na(hit), x, unname(hit))
  }
  orig_names <- names(tbl)
  rn <- rownames(tbl)
  if (!is.null(rn) && !identical(rn, as.character(seq_len(nrow(tbl))))) {
    rownames(tbl) <- relabel(rn)
  }
  names(tbl) <- relabel(names(tbl))
  char_cols <- vapply(tbl, is.character, logical(1))
  char_cols[orig_names %in% exclude_cols] <- FALSE
  tbl[char_cols] <- lapply(tbl[char_cols], relabel)
  tbl
}

# One seam for optional-package availability checks. Besides keeping runners
# consistent, this lets their missing-dependency branches be tested on a
# machine where the optional package happens to be installed.
sframe_has_package <- function(package) {
  requireNamespace(package, quietly = TRUE)
}
