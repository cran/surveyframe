# render_survey.R
# Full Shiny survey renderer with welcome page, thank-you page,
# conversational mode, and all item types.

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

sframe_choices_lookup <- function(instrument) {
  stats::setNames(
    lapply(instrument$choices, function(cs) {
      stats::setNames(as.character(cs$values), cs$labels)
    }),
    vapply(instrument$choices, function(cs) cs$id, character(1))
  )
}

# Every rule that controls an item, keyed by that item. Holding a single rule
# per item meant a second rule on the same item replaced the first, so a
# 2-condition gate ran on one condition. The static survey already kept them
# all and combined them with AND; this is the same contract on the R side.
sframe_branch_lookup <- function(instrument) {
  bl <- list()
  for (rule in instrument$branching %||% list()) {
    id <- as.character(rule$item_id %||% "")[1]
    if (!nzchar(id)) next
    bl[[id]] <- c(bl[[id]] %||% list(), list(rule))
  }
  bl
}

sframe_theme_colour <- function(instrument, theme = NULL) {
  theme %||% instrument$render$theme %||% "#5b8dee"
}

.evaluate_branch <- function(rule, dep_val) {
  if (is.null(dep_val) || length(dep_val) == 0L || all(is.na(dep_val))) {
    return(rule$action == "hide")
  }
  dep_chr <- as.character(dep_val[!is.na(dep_val)])
  rule_chr <- as.character(rule$value)
  dep_num <- suppressWarnings(as.numeric(dep_chr))
  rule_num <- suppressWarnings(as.numeric(rule_chr[1]))
  result <- switch(rule$operator,
    "=="   = any(dep_chr %in% rule_chr),
    "!="   = all(!dep_chr %in% rule_chr),
    # A multi-select answer is itself a set, so any selected value the rule
    # allows satisfies it.
    "%in%" = any(trimws(dep_chr) %in% sframe_branch_in_values(rule$value)),
    ">"    = any(!is.na(dep_num) & !is.na(rule_num) & dep_num > rule_num),
    ">="   = any(!is.na(dep_num) & !is.na(rule_num) & dep_num >= rule_num),
    "<"    = any(!is.na(dep_num) & !is.na(rule_num) & dep_num < rule_num),
    "<="   = any(!is.na(dep_num) & !is.na(rule_num) & dep_num <= rule_num),
    FALSE
  )
  if (rule$action == "show") result else !result
}

# Whether an item is on screen. Rules on one item combine with AND, and a
# controlling item that is itself hidden counts as unanswered, so visibility
# cascades down a chain and a stale answer behind a closed branch cannot
# reveal anything. `seen` stops a declared cycle recursing, matching the
# static evaluator, which treats a repeat as visible so the walk terminates.
sframe_item_visible <- function(item, input_values, branch_lookup,
                                seen = character(0)) {
  id <- as.character(item$id %||% "")[1]
  rules <- branch_lookup[[id]]
  if (is.null(rules) || length(rules) == 0) return(TRUE)
  if (id %in% seen) return(TRUE)
  seen <- c(seen, id)

  for (rule in rules) {
    dep <- as.character(rule$depends_on %||% "")[1]
    dep_visible <- TRUE
    if (nzchar(dep) && !is.null(branch_lookup[[dep]])) {
      dep_visible <- sframe_item_visible(list(id = dep), input_values,
                                         branch_lookup, seen)
    }
    dep_val <- if (dep_visible) input_values[[dep]] else NULL
    if (!.evaluate_branch(rule, dep_val)) return(FALSE)
  }
  TRUE
}

# The items a conversational survey walks, in order, leaving out the ones
# branching excludes. Navigation used to iterate every answerable item, so a
# participant could be required to answer a question the contract excludes,
# which serialisation then blanked.
sframe_visible_sequence <- function(instrument, input_values, branch_lookup,
                                    items = NULL) {
  items <- items %||% (instrument$items %||% list())
  Filter(function(i) sframe_item_visible(i, input_values, branch_lookup), items)
}

sframe_missing_value <- function(item, value) {
  if (item$type %in% c("section_break", "text_block")) return(FALSE)
  if (item$type %in% sframe_expanded_comparison_types) {
    if (is.null(value) || length(value) == 0) return(TRUE)
    unanswered <- vapply(value, function(cell) !sframe_has_answer(cell),
                         logical(1))
    if (any(unanswered)) return(TRUE)
    # A constant-sum allocation that does not total 100 is not a complete
    # answer, the same rule validatePage() enforces on the static survey.
    if (identical(item$type, "criteria_weight")) {
      total <- sum(suppressWarnings(as.numeric(unlist(value))), na.rm = TRUE)
      if (!isTRUE(all.equal(total, 100))) return(TRUE)
    }
    return(FALSE)
  }
  if (item$type == "matrix") {
    if (is.null(value) || length(value) == 0) return(TRUE)
    return(any(vapply(value, function(cell) {
      is.null(cell) || length(cell) == 0 || all(is.na(cell))
    }, logical(1))))
  }
  if (is.null(value) || length(value) == 0) return(TRUE)
  if (all(is.na(value))) return(TRUE)
  if (item$type %in% c("text", "textarea", "ranking"))
    return(!any(nzchar(trimws(as.character(value)))))
  if (item$type == "numeric")
    return(all(is.na(suppressWarnings(as.numeric(value)))))
  FALSE
}

sframe_item_input_value <- function(item, input_values) {
  # A slider always shows a position, so its value is an answer only once the
  # participant has moved it.
  if (identical(item$type, "slider") &&
      !isTRUE(input_values[[paste0(item$id, "__touched")]])) {
    return(NULL)
  }
  if (item$type == "matrix" && !is.null(item$matrix_items)) {
    return(lapply(seq_along(item$matrix_items), function(r) {
      input_values[[paste0(item$id, "__", r)]]
    }))
  }
  if (item$type %in% sframe_expanded_comparison_types) {
    cols <- sframe_comparison_columns(item)
    if (length(cols) == 0) return(NULL)
    return(lapply(cols, function(col) input_values[[col]]))
  }

  input_values[[item$id]]
}

sframe_missing_required_items <- function(instrument, input_values, branch_lookup) {
  visible <- Filter(function(i) sframe_item_visible(i, input_values, branch_lookup),
                    instrument$items)
  vapply(
    Filter(function(i) isTRUE(i$required) &&
           sframe_missing_value(i, sframe_item_input_value(i, input_values)), visible),
    function(i) i$id, character(1)
  )
}

sframe_serialise_response_value <- function(value) {
  if (is.null(value) || length(value) == 0 || all(is.na(value)))
    return(NA_character_)
  if (inherits(value, "Date")) return(as.character(value))
  paste(as.character(value), collapse = "|")
}

# The expansion values a multi-column item contributes to a response row,
# named by their export columns. Matrix cells carry the chosen value, ranking
# options carry their rank position, and multi-select options carry 0 or 1,
# which is the contract the static template and the Google Sheets collector
# already emit and read_responses() already expects.
sframe_expansion_values <- function(item, instrument, input_values) {
  cols <- sframe_item_expansion_columns(instrument, list(item))
  if (length(cols) == 0) return(NULL)

  if (identical(item$type, "matrix")) {
    # The rendered inputs are positional (item__1, item__2), while the export
    # columns carry the sub-item label, so the 2 are matched by position.
    vals <- lapply(seq_along(item$matrix_items), function(r) {
      sframe_serialise_response_value(input_values[[paste0(item$id, "__", r)]])
    })
    return(stats::setNames(vals[seq_along(cols)], cols))
  }

  if (item$type %in% sframe_expanded_comparison_types) {
    return(stats::setNames(
      lapply(cols, function(col) {
        sframe_serialise_response_value(input_values[[col]])
      }),
      cols
    ))
  }

  # ranking and multiple_choice both post a single input holding every
  # selection, so the options are derived from the column names.
  opts <- sub(paste0("^", item$id, "__"), "", cols)
  chosen <- as.character(input_values[[item$id]] %||% character(0))

  if (identical(item$type, "ranking")) {
    # The ranking input holds the order as a pipe-joined string.
    order <- unlist(strsplit(paste(chosen, collapse = "|"), "|", fixed = TRUE))
    order <- order[nzchar(order)]
    return(stats::setNames(lapply(opts, function(o) {
      pos <- match(o, order)
      if (is.na(pos)) NA_character_ else as.character(pos)
    }), cols))
  }

  stats::setNames(lapply(opts, function(o) {
    if (o %in% chosen) "1" else "0"
  }), cols)
}

# A per-response identifier, in the shape the static survey already writes:
# "R" and 8 upper-case base36 characters. The static template generated one from
# the first release and the Shiny path generated none, so the 2 collection
# routes produced different column sets for the same instrument, and neither a
# duplicate check nor an idempotent retry had a key to work from.
sframe_new_response_id <- function() {
  alphabet <- c(0:9, LETTERS)
  paste0("R", paste(sample(alphabet, 8, replace = TRUE), collapse = ""))
}

sframe_response_row <- function(instrument, input_values, branch_lookup,
                                 started_at, submitted_at = Sys.time(),
                                 response_id = sframe_new_response_id()) {
  # Multi-column items expand to one column per sub-item, option, pair, or
  # criterion rather than to a single joined column, matching the static
  # template and the Google Sheets collector. Before 0.4.0 the Shiny path
  # pipe-joined matrix cells into one column, so a matrix question answered
  # here arrived as mx = "4|5" where read_responses() and the whole analysis
  # layer expect mx__r1 and mx__r2. That data could not be read back by the
  # package at all. Ranking and multi-select had the same shape problem.
  expanded_types <- c("matrix", "ranking", "multiple_choice",
                      sframe_expanded_comparison_types)
  expanded_items <- Filter(function(i) i$type %in% expanded_types,
                           instrument$items)
  plain_items <- Filter(function(i) !i$type %in% expanded_types,
                        instrument$items)

  item_values <- lapply(plain_items, function(item) {
    if (!sframe_item_visible(item, input_values, branch_lookup))
      return(NA_character_)
    sframe_serialise_response_value(sframe_item_input_value(item, input_values))
  })
  names(item_values) <- vapply(plain_items, function(i) i$id, character(1))

  for (item in expanded_items) {
    visible <- sframe_item_visible(item, input_values, branch_lookup)
    vals <- sframe_expansion_values(item, instrument, input_values)
    # An item with nothing to expand (no matrix rows, no choice set) still
    # needs its own column rather than vanishing from the row.
    if (is.null(vals)) {
      item_values[[item$id]] <- if (!visible) {
        NA_character_
      } else {
        sframe_serialise_response_value(input_values[[item$id]])
      }
      next
    }
    for (col in names(vals)) {
      item_values[[col]] <- if (!visible) NA_character_ else vals[[col]]
    }
  }

  sframe_as_data_frame(as.data.frame(c(
    list(
      respondent_id = response_id,
      started_at   = format(as.POSIXct(started_at,   tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
      submitted_at = format(as.POSIXct(submitted_at, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
    ),
    item_values
  ), stringsAsFactors = FALSE, check.names = FALSE))
}

# Submission has 2 steps that fail independently: writing the response, and
# handing it to on_submit. They shared one error handler, so a callback failure
# after a successful write told the participant nothing was saved and left them
# on the form. Submitting again appended the same answers a second time.
#
# The state records what has already succeeded for this submission, so a retry
# repeats only what is left. It is per session and per submission, which is
# what makes a retry idempotent without changing the collected columns.
sframe_new_submission_state <- function() {
  list(saved = FALSE, notified = FALSE)
}

# Runs the outstanding steps and returns the state, whether the submission is
# complete, and a message where a step failed.
sframe_persist_response <- function(row, output_path, on_submit, state) {
  state <- state %||% sframe_new_submission_state()

  if (!isTRUE(state$saved)) {
    if (is.null(output_path)) {
      state$saved <- TRUE
    } else {
      err <- tryCatch({
        sframe_append_response_csv(output_path, row)
        NULL
      }, error = function(e) e)
      if (!is.null(err)) {
        return(list(
          state = state, saved = FALSE, notified = FALSE,
          message = paste0("Your answers could not be saved: ",
                           conditionMessage(err),
                           " Your answers are still on this page, so you can ",
                           "try again.")))
      }
      state$saved <- TRUE
    }
  }

  if (!isTRUE(state$notified) && is.function(on_submit)) {
    err <- tryCatch({
      on_submit(row)
      NULL
    }, error = function(e) e)
    if (!is.null(err)) {
      return(list(
        state = state, saved = TRUE, notified = FALSE,
        message = paste0("Your answers are saved. A follow-up step failed: ",
                         conditionMessage(err),
                         " Trying again repeats that step alone, and will not ",
                         "record your answers twice.")))
    }
  }
  state$notified <- TRUE
  list(state = state, saved = TRUE, notified = TRUE, message = NULL)
}

# The header of an existing response file, or NULL when there is none yet.
sframe_response_file_header <- function(path) {
  if (!file.exists(path) || file.size(path) == 0) return(NULL)
  names(utils::read.csv(path, nrows = 0, check.names = FALSE))
}

# Refuses a response file whose columns differ from the ones about to be
# written. Same columns in another order are compatible, and are aligned by
# name when appending.
sframe_check_response_header <- function(path, header, columns) {
  if (is.null(header) || setequal(header, columns)) return(invisible(TRUE))
  missing <- setdiff(header, columns)
  extra <- setdiff(columns, header)
  # A file written before this version had no respondent_id column. Every
  # question column still matches, so the file is compatible and a study
  # collecting today keeps collecting: the row is aligned to the header it
  # finds and the id is left out of that file, rather than refusing the append
  # or writing answers under the wrong headings.
  if (identical(extra, "respondent_id") && length(missing) == 0) {
    rlang::inform(
      paste0("'", path, "' was written before responses carried a ",
             "respondent_id, so this response is appended without one. Collect ",
             "into a new file to record an identifier for every response."),
      class = "sframe_response_id_absent"
    )
    return(invisible(TRUE))
  }
  rlang::abort(
    c(
      paste0("The response file '", path, "' was written for a different ",
             "set of questions, so appending would put answers under the ",
             "wrong headings."),
      if (length(missing))
        c(i = paste0("In the file only: ", paste(missing, collapse = ", "))),
      if (length(extra))
        c(i = paste0("In this instrument only: ", paste(extra, collapse = ", "))),
      i = "Collect this version of the instrument into a new output_path."
    ),
    class = "sframe_error"
  )
}

# Appends one response row. The file header is read first: a row is written
# in the file's own column order, and refused when its columns differ. A
# positional append wrote a changed instrument's values under the previous
# instrument's headings, and nothing reported it.
sframe_append_response_csv <- function(path, row) {
  dir_path <- dirname(path)
  if (!dir.exists(dir_path))
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  header <- sframe_response_file_header(path)
  sframe_check_response_header(path, header, names(row))
  if (!is.null(header)) row <- row[, header, drop = FALSE]
  utils::write.table(row, file = path, sep = ",", row.names = FALSE,
                     col.names = is.null(header), append = !is.null(header),
                     qmethod = "double", na = "")
  invisible(path)
}

# ---------------------------------------------------------------------------
# Item renderer
# ---------------------------------------------------------------------------

sframe_label_tag <- function(item) {
  tags <- shiny::tags
  tags$div(
    class = "sf-label-block",
    tags$div(
      class = "sf-label-row",
      tags$span(class = "sf-label-text", item$label),
      if (isTRUE(item$required)) tags$span(class = "sf-required", "*")
    ),
    if (!is.null(item$help) && nzchar(trimws(item$help)))
      tags$p(class = "sf-help-text", item$help)
  )
}

# A value the participant has given, as opposed to a control's empty state.
sframe_has_answer <- function(value) {
  !is.null(value) && length(value) > 0 && !all(is.na(value)) &&
    any(nzchar(trimws(as.character(value))))
}

# A JavaScript string literal, for embedding an id in an inline handler.
sframe_js_string <- function(x) {
  as.character(jsonlite::toJSON(as.character(x), auto_unbox = TRUE))
}

# A Shiny date input that starts empty. Shiny's date binding fills a missing
# initial date with today, so a question such as a date of birth arrived
# pre-answered. An empty initial-date attribute leaves the field blank.
sframe_blank_date_input <- function(tag) {
  for (k in seq_along(tag$children)) {
    child <- tag$children[[k]]
    if (inherits(child, "shiny.tag") && identical(child$name, "input")) {
      tag$children[[k]]$attribs[["data-initial-date"]] <- ""
    }
  }
  tag
}

# Renders one item's control. `values` holds the answers given so far, and
# every control is drawn with its answer restored. A control drawn at its
# default reports that default to the server when it binds, so drawing a
# page again without restoring would erase what the participant entered.
sframe_render_input <- function(item, choices_lookup, values = list()) {
  tags <- shiny::tags
  radioButtons <- shiny::radioButtons
  checkboxGroupInput <- shiny::checkboxGroupInput
  numericInput <- shiny::numericInput
  textInput <- shiny::textInput
  textAreaInput <- shiny::textAreaInput
  dateInput <- shiny::dateInput
  sliderInput <- shiny::sliderInput
  actionButton <- shiny::actionButton
  selectInput <- shiny::selectInput
  # choices_lookup maps each choice set to codes named by their labels
  cs  <- choices_lookup[[item$choice_set %||% ""]]
  codes  <- unname(cs %||% character(0))
  labels <- names(cs %||% character(0))
  lbl <- sframe_label_tag(item)
  current <- values[[item$id]]
  answered <- sframe_has_answer(current)

  switch(item$type,

    section_break = tags$div(
      class = "sf-section-break",
      tags$h2(class = "sf-section-title", item$label),
      if (!is.null(item$section_intro))
        tags$p(class = "sf-section-intro", item$section_intro)
    ),

    text_block = tags$div(
      class = "sf-text-block",
      tags$p(item$label)
    ),

    likert = radioButtons(item$id, lbl,
      choices = cs %||% character(0), inline = TRUE,
      selected = if (answered) as.character(current)[1] else character(0)),

    single_choice = radioButtons(item$id, lbl,
      choices = cs %||% character(0),
      selected = if (answered) as.character(current)[1] else character(0)),

    multiple_choice = checkboxGroupInput(item$id, lbl,
      choices = cs %||% character(0),
      selected = if (answered) as.character(current) else NULL),

    numeric = numericInput(item$id, lbl,
      value = if (answered) current else NA),

    text = textInput(item$id, lbl,
      value = if (answered) as.character(current) else "",
      placeholder = item$placeholder %||% ""),

    textarea = textAreaInput(item$id, lbl,
      value = if (answered) as.character(current) else "",
      placeholder = item$placeholder %||% "", rows = 4),

    date = {
      date_tag <- dateInput(item$id, lbl,
        value = if (answered) as.character(current)[1] else NULL,
        min = item$date_min %||% NULL,
        max = item$date_max %||% NULL)
      if (answered) date_tag else sframe_blank_date_input(date_tag)
    },

    # A slider always shows a position, so its position alone is not an
    # answer. It counts once the participant moves it, which the page reports
    # as <id>__touched.
    slider = {
      touched <- isTRUE(values[[paste0(item$id, "__touched")]])
      tags$div(
        class = "sf-slider-block",
        `data-sf-slider` = item$id,
        lbl,
        sliderInput(item$id, label = NULL,
          min   = item$slider_min  %||% 0,
          max   = item$slider_max  %||% 100,
          value = if (answered) current else item$slider_min %||% 0,
          step  = item$slider_step %||% 1),
        tags$p(class = "sf-slider-hint", role = "status",
               if (touched) "Your answer is recorded."
               else "Move the slider to give your answer.")
      )
    },

    rating = {
      stars <- item$rating_max %||% 5
      given <- if (answered) suppressWarnings(as.numeric(current)[1]) else 0
      tags$div(
        class = "sf-rating-block",
        lbl,
        tags$div(
          class = "sf-stars",
          lapply(seq_len(stars), function(i) {
            actionButton(
              inputId = paste0(item$id, "_star_", i),
              label   = if ((item$rating_icon %||% "star") == "heart") "\u2665" else "\u2605",
              class   = paste("sf-star-btn", if (isTRUE(i <= given)) "active"),
              `aria-label` = paste(i, "of", stars),
              onclick = sprintf(
                "sfSetRating(%s, %d, %d); return false;",
                sframe_js_string(item$id), i, stars
              )
            )
          }),
          # Hidden numeric input carries the actual value
          numericInput(item$id, label = NULL,
                       value = if (answered) current else NA,
                       min = 1, max = stars)
        )
      )
    },

    # The list holds codes in data-value and shows labels. Order is changed by
    # dragging, by the move buttons from the keyboard, or confirmed as shown,
    # and every route records the order through sfRankRecord(). An untouched
    # ranking records nothing.
    ranking = {
      order <- codes
      if (answered) {
        given <- unlist(strsplit(paste(as.character(current), collapse = "|"),
                                 "|", fixed = TRUE))
        given <- given[given %in% codes]
        order <- c(unique(given), setdiff(codes, given))
      }
      list_id <- paste0("rank_", item$id)
      tags$div(
        class = "sf-ranking-block",
        lbl,
        tags$div(
          class = "sf-rank-list",
          id    = list_id,
          `data-input` = item$id,
          lapply(order, function(code) {
            label <- labels[match(code, codes)]
            tags$div(
              class        = "sf-rank-item",
              `data-value` = code,
              tags$span(class = "sf-rank-handle", `aria-hidden` = "true", "\u283f"),
              tags$span(class = "sf-rank-label", label),
              tags$span(
                class = "sf-rank-moves",
                tags$button(type = "button", class = "sf-rank-move",
                            `aria-label` = paste("Move", label, "up"),
                            onclick = "sfRankMove(this, -1)", "\u25b2"),
                tags$button(type = "button", class = "sf-rank-move",
                            `aria-label` = paste("Move", label, "down"),
                            onclick = "sfRankMove(this, 1)", "\u25bc")
              )
            )
          })
        ),
        tags$div(
          class = "sf-rank-confirm",
          tags$button(type = "button", class = "btn-secondary sf-rank-keep",
                      onclick = "sfRankConfirm(this)", "Keep this order"),
          tags$span(class = "sf-rank-status", role = "status",
                    `aria-live` = "polite",
                    if (answered) "Your order is recorded."
                    else "Drag the options or use the arrows, or keep the order shown.")
        ),
        tags$div(
          class = "sf-rank-input",
          textInput(item$id, label = NULL,
                    value = if (answered) paste(order, collapse = "|") else "")
        )
      )
    },

    matrix = {
      rows <- item$matrix_items %||% character(0)
      if (length(rows) == 0 || is.null(cs)) {
        tags$div(lbl, tags$p(class = "sf-help-text", "Matrix not fully configured."))
      } else {
        tags$div(
          class = "sf-matrix-block",
          lbl,
          tags$div(
            class = "sf-matrix-scroll",
            tags$table(
              class = "sf-matrix",
              tags$thead(
                tags$tr(
                  tags$th(""),
                  lapply(labels, function(l) tags$th(l))
                )
              ),
              tags$tbody(
                lapply(seq_along(rows), function(r) {
                  input_id <- paste0(item$id, "__", r)
                  given <- values[[input_id]]
                  tags$tr(
                    tags$td(class = "sf-matrix-row-label", rows[r]),
                    lapply(seq_along(codes), function(k) {
                      tags$td(
                        class = "sf-matrix-cell",
                        tags$input(
                          type  = "radio",
                          name  = input_id,
                          value = codes[k],
                          id    = paste0(input_id, "_", k),
                          `aria-label` = paste0(rows[r], ": ", labels[k]),
                          checked = if (sframe_has_answer(given) &&
                                        identical(as.character(given)[1], codes[k])) NA,
                          onclick = sprintf(
                            "Shiny.setInputValue(%s, this.value, {priority:'event'})",
                            sframe_js_string(input_id)
                          )
                        )
                      )
                    })
                  )
                })
              )
            )
          )
        )
      }
    },

    # Decision-family items render one input per expansion column, using the
    # ids the static template and the export contract use (item__a__vs__b,
    # item__a__to__b, item__crit). Each starts unanswered: a judgement of
    # "Equally important" or "No influence" is a substantive answer, and
    # pre-selecting it recorded that answer for anyone who moved past.
    pairwise_comparison = {
      cmp_items <- item$comparison_items %||% character(0)
      cmp_scale <- item$comparison_scale %||% "saaty"
      if (length(cmp_items) < 2) {
        tags$div(lbl, tags$p(class = "sf-help-text",
                             "Comparison not fully configured."))
      } else {
        pairs <- sframe_comparison_pairs(cmp_items, cmp_scale)
        cols  <- sframe_comparison_columns(item)
        tags$div(
          class = "sf-decision-block",
          lbl,
          tags$div(
            class = "sf-decision-rows",
            lapply(seq_len(nrow(pairs)), function(k) {
              opts <- if (identical(cmp_scale, "influence")) {
                stats::setNames(
                  as.character(0:4),
                  c("No influence", "Low", "Medium", "High", "Very high")
                )
              } else {
                stats::setNames(
                  as.character(c(-9:-2, 1, 2:9)),
                  c(paste(pairs$b[k], "extremely more important"),
                    paste(pairs$b[k], "very strongly more important"),
                    paste(pairs$b[k], "strongly more important"),
                    paste(pairs$b[k], "moderately more important"),
                    paste(pairs$b[k], "slightly more important"),
                    paste(pairs$b[k], "marginally more important"),
                    paste(pairs$b[k], "a little more important"),
                    paste(pairs$b[k], "very slightly more important"),
                    "Equally important",
                    paste(pairs$a[k], "very slightly more important"),
                    paste(pairs$a[k], "a little more important"),
                    paste(pairs$a[k], "marginally more important"),
                    paste(pairs$a[k], "slightly more important"),
                    paste(pairs$a[k], "moderately more important"),
                    paste(pairs$a[k], "strongly more important"),
                    paste(pairs$a[k], "very strongly more important"),
                    paste(pairs$a[k], "extremely more important"))
                )
              }
              prompt <- if (identical(cmp_scale, "influence")) {
                sprintf("How strongly does %s influence %s?",
                        pairs$a[k], pairs$b[k])
              } else {
                sprintf("%s compared with %s", pairs$a[k], pairs$b[k])
              }
              given <- values[[cols[k]]]
              tags$div(
                class = "sf-decision-row",
                tags$label(`for` = cols[k], class = "sf-decision-label",
                           prompt),
                selectInput(
                  cols[k], label = NULL,
                  choices = c(stats::setNames("", "Choose a judgement"), opts),
                  selected = if (sframe_has_answer(given)) as.character(given)[1] else "",
                  selectize = FALSE,
                  width = "100%"
                )
              )
            })
          )
        )
      }
    },

    criteria_weight = {
      cmp_items <- item$comparison_items %||% character(0)
      if (length(cmp_items) < 2) {
        tags$div(lbl, tags$p(class = "sf-help-text",
                             "Criteria not fully configured."))
      } else {
        cols <- sframe_comparison_columns(item)
        tags$div(
          class = "sf-decision-block",
          lbl,
          tags$p(class = "sf-help-text",
                 "Divide 100 points across the criteria. The total must be 100."),
          tags$div(
            class = "sf-decision-rows",
            lapply(seq_along(cmp_items), function(k) {
              given <- values[[cols[k]]]
              tags$div(
                class = "sf-decision-row",
                tags$label(`for` = cols[k], class = "sf-decision-label",
                           cmp_items[k]),
                numericInput(cols[k], label = NULL,
                             value = if (sframe_has_answer(given)) given else NA,
                             min = 0, max = 100, step = 1, width = "120px")
              )
            })
          )
        )
      }
    },

    textInput(item$id, lbl,
              value = if (answered) as.character(current) else "")  # fallback
  )
}

# The respondent-side JavaScript for render_survey(): ranking by drag, by
# keyboard and by confirmation, slider touch tracking, star ratings, and
# branching visibility sent from the server.
sframe_survey_js <- function() {
  "
  function sfRankRecord(list, announce) {
    var inputId = list.getAttribute('data-input');
    var items = Array.prototype.slice.call(list.querySelectorAll('.sf-rank-item'));
    var codes = items.map(function(i) { return i.getAttribute('data-value'); });
    var field = document.getElementById(inputId);
    if (field) field.value = codes.join('|');
    Shiny.setInputValue(inputId, codes.join('|'), {priority: 'event'});
    var block = list.closest('.sf-ranking-block');
    var status = block && block.querySelector('.sf-rank-status');
    if (status) status.textContent = (announce ? announce + ' ' : '') + 'Your order is recorded.';
  }
  function sfRankMove(btn, dir) {
    var item = btn.closest('.sf-rank-item');
    var list = item.parentNode;
    var target = dir < 0 ? item.previousElementSibling : item.nextElementSibling;
    if (!target) return;
    if (dir < 0) list.insertBefore(item, target); else list.insertBefore(target, item);
    var items = Array.prototype.slice.call(list.querySelectorAll('.sf-rank-item'));
    var label = item.querySelector('.sf-rank-label').textContent;
    sfRankRecord(list, label + ' moved to position ' + (items.indexOf(item) + 1) +
                 ' of ' + items.length + '.');
    btn.focus();
  }
  function sfRankConfirm(btn) {
    sfRankRecord(btn.closest('.sf-ranking-block').querySelector('.sf-rank-list'), '');
  }
  function initRanking(listId) {
    var list = document.getElementById(listId);
    if (!list || list.getAttribute('data-sf-init')) return;
    list.setAttribute('data-sf-init', '1');
    var dragging = null;
    list.querySelectorAll('.sf-rank-item').forEach(function(item) {
      item.setAttribute('draggable', 'true');
      item.addEventListener('dragstart', function() { dragging = item; item.style.opacity = '.5'; });
      item.addEventListener('dragend', function() {
        item.style.opacity = '1';
        if (dragging) sfRankRecord(list, '');
        dragging = null;
      });
      item.addEventListener('dragover', function(e) {
        e.preventDefault();
        if (!dragging || dragging === item) return;
        var r = item.getBoundingClientRect();
        if (e.clientY < r.top + r.height / 2) list.insertBefore(dragging, item);
        else list.insertBefore(dragging, item.nextSibling);
      });
    });
  }
  var sfSliderKeys = ['ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown',
                      'Home', 'End', 'PageUp', 'PageDown'];
  function sfSliderTouched(e) {
    if (e.type === 'keydown' && sfSliderKeys.indexOf(e.key) < 0) return;
    var wrap = e.target.closest && e.target.closest('[data-sf-slider]');
    if (!wrap) return;
    Shiny.setInputValue(wrap.getAttribute('data-sf-slider') + '__touched', true);
    var hint = wrap.querySelector('.sf-slider-hint');
    if (hint) hint.textContent = 'Your answer is recorded.';
  }
  // Listeners and the message handler are registered once per page, even when
  // the script is included by more than one survey module.
  if (!window.sfSurveyListeners) {
    window.sfSurveyListeners = true;
    document.addEventListener('pointerdown', sfSliderTouched, true);
    document.addEventListener('keydown', sfSliderTouched, true);
  }
  function sfSetRating(id, val, max) {
    var btns = document.querySelectorAll('[id^=\"' + id + '_star_\"]');
    btns.forEach(function(b, i) { b.classList.toggle('active', i < val); });
    Shiny.setInputValue(id, val, {priority: 'event'});
  }
  if (!window.sfVisibilityHandler) $(function() {
    if (window.sfVisibilityHandler) return;
    window.sfVisibilityHandler = true;
    Shiny.addCustomMessageHandler('sf-visibility', function(msg) {
      (msg.show || []).forEach(function(id) {
        var el = document.getElementById('sf_item_' + id); if (el) el.style.display = '';
      });
      (msg.hide || []).forEach(function(id) {
        var el = document.getElementById('sf_item_' + id); if (el) el.style.display = 'none';
      });
    });
  });
  "
}

# ---------------------------------------------------------------------------
# Progress bar UI
# ---------------------------------------------------------------------------

sframe_progress_ui <- function(answered, total, colour) {
  tags <- shiny::tags
  pct <- if (total > 0) round(answered / total * 100) else 0
  tags$div(
    class = "sf-progress",
    tags$div(class = "sf-progress-bar",
      tags$div(class = "sf-progress-fill",
        style = sprintf("width:%d%%;background:%s", pct, colour))),
    tags$div(class = "sf-progress-text",
      sprintf("%d of %d questions answered", answered, total))
  )
}

# ---------------------------------------------------------------------------
# Main exported function
# ---------------------------------------------------------------------------

#' Render a survey from an instrument object
#'
#' Launches a Shiny survey with a welcome page, configurable header, all item
#' types, branching logic, required-field enforcement, progress tracking,
#' standard and conversational (one-question-at-a-time) display modes, and a
#' customisable thank-you page. Responses can be persisted to CSV or passed to
#' a callback.
#'
#' @param instrument An `sframe` object.
#' @param mode Character. Deployment mode. Currently `"shiny"`.
#' @param title Character or NULL. Override for the survey title.
#' @param theme Character or NULL. Hex colour for the survey theme.
#' @param save_responses Character. `"none"` (default) or `"csv"`.
#' @param output_path Character or NULL. CSV path when `save_responses = "csv"`.
#' @param on_submit Function or NULL. Callback receiving the submitted row.
#'
#' @return A `shiny.appobj`.
#' @export
#' @seealso [launch_studio()], [read_responses()]
#'
#' @examples
#' \donttest{
#' cs    <- sf_choices("ag5", 1:5,
#'            c("Strongly disagree", "Disagree", "Neutral",
#'              "Agree", "Strongly agree"))
#' item  <- sf_item("sat_1", "How satisfied are you?",
#'                  type = "likert", choice_set = "ag5")
#' instr <- sf_instrument("My Survey", components = list(cs, item))
#' app <- render_survey(instr)
#' app <- render_survey(instr, save_responses = "csv",
#'                       output_path = tempfile(fileext = ".csv"))
#' }
render_survey <- function(
    instrument,
    mode           = c("shiny"),
    title          = NULL,
    theme          = NULL,
    save_responses = c("none", "csv"),
    output_path    = NULL,
    on_submit      = NULL
) {
  sframe_require_shiny("to deploy surveys with render_survey()")
  sframe_check_instrument(instrument)
  mode           <- rlang::arg_match(mode)
  save_responses <- rlang::arg_match(save_responses)

  if (!is.null(on_submit) && !is.function(on_submit))
    rlang::abort("`on_submit` must be NULL or a function.", class = "sframe_error")
  if (identical(save_responses, "csv") && is.null(output_path))
    rlang::abort("`output_path` must be supplied when save_responses = 'csv'.",
                 class = "sframe_error")

  # Render settings
  rnd           <- instrument$render %||% list()
  colour        <- sframe_theme_colour(instrument, theme)
  display_title <- title %||% instrument$meta$title
  conv_mode     <- identical(rnd$mode, "conversational")
  show_progress <- !isFALSE(rnd$header$show_progress)

  # Welcome page settings
  wlc <- rnd$welcome %||% list()
  welcome_title    <- wlc$title        %||% display_title
  welcome_intro    <- wlc$intro_text   %||% ""
  welcome_consent  <- wlc$consent_text %||% ""
  need_consent     <- isTRUE(wlc$consent_required)
  start_label      <- wlc$start_label  %||% "Start Survey"
  has_welcome      <- nzchar(trimws(welcome_intro)) || nzchar(trimws(welcome_consent))

  # Thank-you page settings
  tku <- rnd$thankyou %||% list()
  thankyou_msg     <- tku$message      %||% "Thank you for completing this survey."
  thankyou_redirect <- tku$redirect_url %||% ""
  thankyou_download <- isTRUE(tku$show_download)

  # Header settings
  hdr            <- rnd$header %||% list()
  institution    <- hdr$institution %||% ""
  logo_b64       <- hdr$logo_base64 %||% ""
  # Fix E/F: use stored media type so JPEG and GIF logos display correctly
  logo_media_type <- hdr$logo_media_type %||% "image/png"

  submit_label    <- rnd$submit_label %||% "Submit"
  choices_lookup  <- sframe_choices_lookup(instrument)
  branch_lookup   <- sframe_branch_lookup(instrument)

  # Check an existing response file before any participant arrives, so an
  # incompatible file stops the researcher at launch.
  if (identical(save_responses, "csv")) {
    expected <- names(sframe_response_row(instrument, list(), branch_lookup,
                                          started_at = Sys.time()))
    sframe_check_response_header(output_path,
                                 sframe_response_file_header(output_path),
                                 expected)
  }

  # Collect answerable items (not section_break or text_block)
  answerable_types <- c("likert","single_choice","multiple_choice","numeric",
                        "text","textarea","date","slider","rating","ranking","matrix",
                        sframe_expanded_comparison_types)

  css <- sprintf("
    body{font-family:'Helvetica Neue',Arial,sans-serif;background:#f4f5f8;
         color:#1a1a2e;margin:0;padding:0}
    .sf-wrap{max-width:680px;margin:0 auto;padding:28px 20px}
    .sf-card{background:#fff;border-radius:12px;
             box-shadow:0 2px 12px rgba(0,0,0,.08);padding:32px;margin-bottom:20px}
    .sf-header{display:flex;align-items:center;gap:16px;margin-bottom:24px;
               padding-bottom:16px;border-bottom:2px solid #f0f0f0}
    .sf-logo{max-height:48px;max-width:120px}
    .sf-institution{font-size:13px;color:#666}
    .sf-survey-title{font-size:24px;font-weight:700;margin-bottom:8px}
    .sf-survey-desc{color:#666;margin-bottom:20px}
    .sf-welcome-intro{line-height:1.7;margin-bottom:16px}
    .sf-consent-box{background:#f8f9fc;border:1.5px solid #dde;
                    border-radius:8px;padding:14px;margin-bottom:16px;font-size:13px}
    .sf-progress{margin-bottom:20px}
    .sf-progress-bar{width:100%%;height:8px;background:#e9ecef;border-radius:99px;overflow:hidden}
    .sf-progress-fill{height:100%%;border-radius:99px;transition:width .3s}
    .sf-progress-text{margin-top:5px;font-size:12px;color:#777}
    .sf-label-block{display:block;margin-bottom:4px}
    .sf-label-row{display:flex;align-items:center;gap:6px;font-weight:600;font-size:15px}
    .sf-required{color:#ef5350;font-size:12px}
    .sf-help-text{font-size:12px;color:#888;margin:3px 0 0;font-weight:400}
    .sf-section-break{border-top:2px solid %s;margin:28px 0 20px;padding-top:16px}
    .sf-section-title{font-size:18px;font-weight:700;color:#1a1a2e;margin:0 0 6px}
    .sf-section-intro{color:#666;font-size:14px;margin:0}
    .sf-text-block{background:#f8f9fc;border-left:4px solid %s;
                   padding:12px 16px;border-radius:0 8px 8px 0;margin-bottom:16px;
                   font-size:14px;line-height:1.6}
    .sf-matrix-scroll{overflow-x:auto}
    .sf-matrix{border-collapse:collapse;width:100%%;min-width:400px}
    .sf-matrix th,.sf-matrix td{padding:10px 14px;border:1px solid #eee;
                                  text-align:center;font-size:13px}
    .sf-matrix th{background:#f7f8fa;font-weight:600}
    .sf-matrix-row-label{text-align:left!important;font-weight:500}
    .sf-matrix-cell input[type=radio]{accent-color:%s;transform:scale(1.2)}
    .sf-rating-block .sf-stars{display:flex;align-items:center;gap:6px;margin-top:8px}
    .sf-star-btn{background:none;border:none;font-size:28px;cursor:pointer;
                  color:#ddd;padding:0;line-height:1;transition:color .15s}
    .sf-star-btn.active{color:#f59e0b}
    .sf-ranking-block .sf-rank-list{border:1.5px solid #dde;border-radius:8px;
                                     padding:8px;margin-top:8px}
    .sf-rank-item{display:flex;align-items:center;gap:10px;padding:8px 10px;
                   background:#f8f9fc;border-radius:6px;margin-bottom:5px;
                   cursor:grab;font-size:14px;border:1px solid #eee}
    .sf-rank-handle{color:#bbb;font-size:18px}
    .sf-rank-input{display:none}
    .sf-rank-label{flex:1}
    .sf-rank-moves{display:flex;gap:4px}
    .sf-rank-move{width:32px;height:32px;border:1px solid #dde;border-radius:6px;
                  background:#fff;cursor:pointer;font-size:12px}
    .sf-rank-move:focus-visible,.sf-rank-keep:focus-visible{outline:3px solid %s;outline-offset:2px}
    .sf-rank-confirm{display:flex;flex-wrap:wrap;align-items:center;gap:10px;margin-top:8px}
    .sf-rank-status,.sf-slider-hint{font-size:13px;color:#555;margin:4px 0 0}
    .sf-item-wrap{margin-bottom:24px;animation:fadeIn .2s ease}
    @keyframes fadeIn{from{opacity:0;transform:translateY(8px)}to{opacity:1;transform:none}}
    .sf-conv-nav{display:flex;gap:10px;margin-top:20px}
    .sf-conv-hint{font-size:12px;color:#aaa;margin-top:8px}
    .sf-error-msg{color:#ef5350;font-size:13px;margin-top:6px}
    .btn-primary{background:%s;color:#fff;border:none;border-radius:8px;
                  padding:12px 28px;font-size:15px;font-weight:600;
                  cursor:pointer;width:100%%;transition:opacity .15s}
    .btn-primary:hover{opacity:.9}
    .btn-secondary{background:#fff;color:%s;border:2px solid %s;
                    border-radius:8px;padding:11px 24px;font-size:14px;
                    font-weight:600;cursor:pointer;transition:opacity .15s}
    .btn-secondary:hover{opacity:.8}
    .sf-thankyou{text-align:center;padding:40px 20px}
    .sf-thankyou-icon{font-size:56px;margin-bottom:16px}
    .sf-thankyou-title{font-size:24px;font-weight:700;margin-bottom:10px}
    .sf-thankyou-msg{color:#555;font-size:15px;line-height:1.6}
  ", colour, colour, colour, colour, colour, colour, colour)
  tags <- shiny::tags

  # -------------------------------------------------------------------------
  # UI
  # -------------------------------------------------------------------------
  ui <- shiny::fluidPage(
    tags$head(tags$style(css),
      if (conv_mode) tags$script(shiny::HTML("
        // Conversational mode JS
        function sfConvGo(dir) {
          Shiny.setInputValue('conv_nav', dir + '_' + Date.now(), {priority:'event'});
        }
        document.addEventListener('keydown', function(e) {
          if (e.key === 'Enter' && !e.shiftKey) {
            var btn = document.getElementById('conv_next');
            if (btn) { e.preventDefault(); sfConvGo('next'); }
          }
        });
      ")) else NULL,
      tags$script(shiny::HTML(sframe_survey_js()))
    ),
    shiny::uiOutput("survey_ui")
  )

  # -------------------------------------------------------------------------
  # Server
  # -------------------------------------------------------------------------
  server <- function(input, output, session) {
    started_at  <- Sys.time()
    page_state  <- shiny::reactiveVal("welcome")   # welcome | survey | thankyou
    conv_idx    <- shiny::reactiveVal(1L)
    submitted_row <- shiny::reactiveVal(NULL)
    # What this submission has already done, so a retry after a failure
    # repeats only the step that failed.
    submission_state <- shiny::reactiveVal(sframe_new_submission_state())

    input_values <- shiny::reactive({ shiny::reactiveValuesToList(input) })

    # Switch welcome -> survey
    shiny::observeEvent(input$start_btn, {
      if (need_consent && !isTRUE(input$consent_check)) {
        shiny::showNotification("Please read and accept the consent statement.",
                                type = "warning")
        return()
      }
      page_state("survey")
    })

    # Conversational navigation
    shiny::observeEvent(input$conv_nav, {
      parts  <- strsplit(input$conv_nav, "_")[[1]]
      dir    <- parts[1]
      # Branching decides the sequence, so a question the contract excludes
      # is never navigated to. Walking every answerable item could require an
      # answer to a hidden question, which serialisation then blanked.
      items  <- sframe_visible_sequence(
        instrument, input_values(), branch_lookup,
        Filter(function(i) i$type %in% answerable_types, instrument$items))
      n      <- length(items)
      cur    <- conv_idx()
      item   <- items[[cur]]
      iv     <- input_values()

      if (dir == "next") {
        item_value <- sframe_item_input_value(item, iv)
        if (isTRUE(item$required) && sframe_missing_value(item, item_value)) {
          shiny::showNotification(
            paste("Please answer:", item$label), type = "warning", duration = 3)
          return()
        }
        if (cur < n) conv_idx(cur + 1L)
        else shiny::updateActionButton(session, "submit_btn",
                                       label = submit_label)
      } else {
        if (cur > 1L) conv_idx(cur - 1L)
      }
    })

    # Submit
    shiny::observeEvent(input$submit_btn, {
      iv <- input_values()
      bl <- branch_lookup
      missing_req <- sframe_missing_required_items(instrument, iv, bl)
      if (length(missing_req) > 0) {
        missing_labels <- vapply(
          Filter(function(i) i$id %in% missing_req, instrument$items),
          function(i) i$label, character(1))
        shiny::showNotification(
          paste("Please answer:", paste(missing_labels, collapse = "; ")),
          type = "error", duration = 6)
        return()
      }
      row <- submitted_row() %||% sframe_response_row(instrument, iv, bl,
                                                      started_at)
      res <- sframe_persist_response(
        row,
        if (identical(save_responses, "csv")) output_path else NULL,
        on_submit,
        submission_state())
      submission_state(res$state)
      # The row is held so a retry writes the same answers, rather than
      # rebuilding them from inputs that may have moved.
      submitted_row(row)
      if (!is.null(res$message)) {
        shiny::showNotification(res$message, type = "error", duration = 10)
        return()
      }
      page_state("thankyou")
    })

    # -----------------------------------------------------------------------
    # UI render
    # -----------------------------------------------------------------------
    output$survey_ui <- shiny::renderUI({
      state <- page_state()

      # Header (shown on survey page)
      header_ui <- if (state == "survey" && (nzchar(institution) || nzchar(logo_b64))) {
        tags$div(class = "sf-header",
          if (nzchar(logo_b64))
            tags$img(class = "sf-logo",
              src = paste0("data:", logo_media_type, ";base64,", logo_b64)),
          if (nzchar(institution))
            tags$div(class = "sf-institution", institution)
        )
      }

      # ---- WELCOME PAGE ----
      if (state == "welcome") {
        if (!has_welcome) {
          # Skip welcome, go straight to survey
          shiny::isolate(page_state("survey"))
          return(shiny::uiOutput("survey_ui"))
        }
        return(tags$div(class = "sf-wrap",
          tags$div(class = "sf-card",
            tags$div(class = "sf-survey-title", welcome_title),
            if (nzchar(welcome_intro))
              tags$div(class = "sf-welcome-intro",
                shiny::HTML(gsub("\n", "<br>", welcome_intro))),
            if (nzchar(welcome_consent))
              tags$div(class = "sf-consent-box",
                if (need_consent)
                  shiny::checkboxInput("consent_check", welcome_consent, value = FALSE)
                else
                  tags$p(welcome_consent)
              ),
            tags$br(),
            shiny::actionButton("start_btn", start_label,
                                class = "btn-primary",
                                style = paste0("background:", colour, ";border-color:", colour))
          )
        ))
      }

      # ---- THANK-YOU PAGE ----
      if (state == "thankyou") {
        return(tags$div(class = "sf-wrap",
          tags$div(class = "sf-card sf-thankyou",
            tags$div(class = "sf-thankyou-icon", "\u2705"),
            tags$div(class = "sf-thankyou-title", "Submitted successfully"),
            tags$div(class = "sf-thankyou-msg", thankyou_msg),
            if (nzchar(thankyou_redirect)) {
              tags$script(sprintf(
                "setTimeout(function(){window.location.href='%s';}, 3000);",
                thankyou_redirect))
            },
            if (thankyou_download && !is.null(submitted_row())) {
              shiny::downloadButton("dl_response", "Download my response",
                style = "margin-top:20px")
            }
          )
        ))
      }

      # ---- SURVEY PAGE ----
      # The page is drawn from the answers held so far, read without taking a
      # dependency on them. Reading them reactively re-drew every control on
      # each answer, and a re-drawn control reports its default to the server,
      # which erased the answer just given. Progress and branching visibility
      # move with answers through their own output and observer below.
      iv         <- shiny::isolate(input_values())
      all_items  <- instrument$items
      ans_items  <- Filter(function(i) i$type %in% answerable_types, all_items)
      # Conversational mode shows one of these at a time, and it has to be the
      # same sequence navigation walks or the index points at another question.
      conv_items <- sframe_visible_sequence(instrument, iv, branch_lookup,
                                            ans_items)

      progress_ui <- if (show_progress) shiny::uiOutput("sf_progress")

      # ---- CONVERSATIONAL MODE ----
      if (conv_mode) {
        if (length(conv_items) == 0) return(NULL)
        cur  <- conv_idx()
        cur  <- min(max(cur, 1), length(conv_items))
        item <- conv_items[[cur]]
        n    <- length(conv_items)

        item_ui <- tags$div(
          class = "sf-item-wrap",
          sframe_render_input(item, choices_lookup, iv),
          tags$div(class = "sf-conv-hint",
            if (n > 1)
              sprintf("Question %d of %d", cur, n)
          )
        )

        nav <- tags$div(class = "sf-conv-nav",
          if (cur > 1)
            tags$button("<- Back", class = "btn-secondary",
              onclick = "sfConvGo('back')"),
          if (cur < n)
            tags$button(id = "conv_next",
              paste0("Next ->"),
              class = "btn-primary",
              style = paste0("background:", colour),
              onclick = "sfConvGo('next')")
          else
            shiny::actionButton("submit_btn", submit_label,
              class = "btn-primary",
              style = paste0("background:", colour, ";border:none"))
        )

        return(tags$div(class = "sf-wrap",
          header_ui,
          tags$div(class = "sf-card",
            tags$div(class = "sf-survey-title", display_title),
            progress_ui,
            item_ui,
            nav
          )
        ))
      }

      # ---- STANDARD MODE ----
      # Every item is drawn once, and hidden items are hidden in place, so
      # showing an item never re-draws the others.
      items_ui <- lapply(all_items, function(item) {
        tags$div(class = "sf-item-wrap",
          id = paste0("sf_item_", item$id),
          style = if (!sframe_item_visible(item, iv, branch_lookup)) "display:none",
          sframe_render_input(item, choices_lookup, iv))
      })

      # Initialise ranking JS after render
      rank_init <- lapply(
        Filter(function(i) i$type == "ranking", all_items),
        function(item) {
          tags$script(sprintf(
            "setTimeout(function(){initRanking(%s);},200);",
            sframe_js_string(paste0("rank_", item$id))))
        }
      )

      tags$div(class = "sf-wrap",
        header_ui,
        tags$div(class = "sf-card",
          tags$div(class = "sf-survey-title", display_title),
          if (!is.null(instrument$meta$description) &&
              nzchar(instrument$meta$description))
            tags$div(class = "sf-survey-desc", instrument$meta$description),
          progress_ui,
          do.call(shiny::tagList, items_ui),
          do.call(shiny::tagList, rank_init),
          tags$br(),
          shiny::actionButton("submit_btn", submit_label,
            class = "btn-primary",
            style = paste0("background:", colour, ";border:none;display:block"))
        )
      )
    })

    output$sf_progress <- shiny::renderUI({
      iv <- input_values()
      ans_items <- Filter(function(i) i$type %in% answerable_types,
                          instrument$items)
      visible <- Filter(function(i) sframe_item_visible(i, iv, branch_lookup),
                        ans_items)
      if (length(visible) == 0) return(NULL)
      answered <- sum(vapply(visible, function(i) {
        !sframe_missing_value(i, sframe_item_input_value(i, iv))
      }, logical(1)))
      sframe_progress_ui(answered, length(visible), colour)
    })

    # Branching shows and hides items in place, without drawing the page again.
    shiny::observe({
      if (!identical(page_state(), "survey") || conv_mode) return()
      iv <- input_values()
      ids <- vapply(instrument$items, function(i) i$id, character(1))
      shown <- vapply(instrument$items, function(i) {
        sframe_item_visible(i, iv, branch_lookup)
      }, logical(1))
      session$sendCustomMessage("sf-visibility",
        list(show = as.list(ids[shown]), hide = as.list(ids[!shown])))
    })

    # Download handler for thank-you page response download
    output$dl_response <- shiny::downloadHandler(
      filename = function() paste0("my_response_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content  = function(file) {
        row <- submitted_row()
        if (!is.null(row)) utils::write.csv(row, file, row.names = FALSE)
      }
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}
