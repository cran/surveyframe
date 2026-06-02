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

sframe_branch_lookup <- function(instrument) {
  bl <- list()
  for (rule in instrument$branching) bl[[rule$item_id]] <- rule
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
    "%in%" = any(dep_chr %in% rule_chr),
    ">"    = any(!is.na(dep_num) & !is.na(rule_num) & dep_num > rule_num),
    ">="   = any(!is.na(dep_num) & !is.na(rule_num) & dep_num >= rule_num),
    "<"    = any(!is.na(dep_num) & !is.na(rule_num) & dep_num < rule_num),
    "<="   = any(!is.na(dep_num) & !is.na(rule_num) & dep_num <= rule_num),
    FALSE
  )
  if (rule$action == "show") result else !result
}

sframe_item_visible <- function(item, input_values, branch_lookup) {
  rule <- branch_lookup[[item$id]]
  if (is.null(rule)) return(TRUE)
  .evaluate_branch(rule, input_values[[rule$depends_on]])
}

sframe_missing_value <- function(item, value) {
  if (item$type %in% c("section_break", "text_block")) return(FALSE)
  if (item$type == "matrix") {
    if (is.null(value) || length(value) == 0) return(TRUE)
    return(any(vapply(value, function(cell) {
      is.null(cell) || length(cell) == 0 || all(is.na(cell))
    }, logical(1))))
  }
  if (is.null(value) || length(value) == 0) return(TRUE)
  if (all(is.na(value))) return(TRUE)
  if (item$type %in% c("text", "textarea"))
    return(!any(nzchar(trimws(as.character(value)))))
  if (item$type == "numeric")
    return(all(is.na(suppressWarnings(as.numeric(value)))))
  FALSE
}

sframe_item_input_value <- function(item, input_values) {
  if (item$type == "matrix" && !is.null(item$matrix_items)) {
    return(lapply(seq_along(item$matrix_items), function(r) {
      input_values[[paste0(item$id, "__", r)]]
    }))
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

sframe_response_row <- function(instrument, input_values, branch_lookup,
                                 started_at, submitted_at = Sys.time()) {
  item_values <- lapply(instrument$items, function(item) {
    if (!sframe_item_visible(item, input_values, branch_lookup))
      return(NA_character_)
    if (item$type == "matrix" && !is.null(item$matrix_items)) {
      vals <- lapply(seq_along(item$matrix_items), function(r) {
        sframe_serialise_response_value(input_values[[paste0(item$id, "__", r)]])
      })
      return(paste(vals, collapse = "|"))
    }
    sframe_serialise_response_value(input_values[[item$id]])
  })
  names(item_values) <- vapply(instrument$items, function(i) i$id, character(1))
  sframe_as_data_frame(as.data.frame(c(
    list(
      started_at   = format(as.POSIXct(started_at,   tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
      submitted_at = format(as.POSIXct(submitted_at, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
    ),
    item_values
  ), stringsAsFactors = FALSE, check.names = FALSE))
}

sframe_append_response_csv <- function(path, row) {
  dir_path <- dirname(path)
  if (!dir.exists(dir_path))
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  exists <- file.exists(path)
  utils::write.table(row, file = path, sep = ",", row.names = FALSE,
                     col.names = !exists, append = exists,
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

sframe_render_input <- function(item, choices_lookup) {
  tags <- shiny::tags
  radioButtons <- shiny::radioButtons
  checkboxGroupInput <- shiny::checkboxGroupInput
  numericInput <- shiny::numericInput
  textInput <- shiny::textInput
  textAreaInput <- shiny::textAreaInput
  dateInput <- shiny::dateInput
  sliderInput <- shiny::sliderInput
  actionButton <- shiny::actionButton
  cs  <- choices_lookup[[item$choice_set %||% ""]]
  lbl <- sframe_label_tag(item)

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
      choices = cs %||% character(0), inline = TRUE, selected = character(0)),

    single_choice = radioButtons(item$id, lbl,
      choices = cs %||% character(0), selected = character(0)),

    multiple_choice = checkboxGroupInput(item$id, lbl,
      choices = cs %||% character(0)),

    numeric = numericInput(item$id, lbl, value = NA),

    text = textInput(item$id, lbl,
      placeholder = item$placeholder %||% ""),

    textarea = textAreaInput(item$id, lbl,
      placeholder = item$placeholder %||% "", rows = 4),

    date = dateInput(item$id, lbl, value = NULL),

    slider = tags$div(
      lbl,
      sliderInput(item$id, label = NULL,
        min   = item$slider_min  %||% 0,
        max   = item$slider_max  %||% 100,
        value = item$slider_min  %||% 0,
        step  = item$slider_step %||% 1)
    ),

    rating = tags$div(
      class = "sf-rating-block",
      lbl,
      tags$div(
        class = "sf-stars",
        lapply(seq_len(item$rating_max %||% 5), function(i) {
          actionButton(
            inputId = paste0(item$id, "_star_", i),
            label   = if ((item$rating_icon %||% "star") == "heart") "\u2665" else "\u2605",
            class   = "sf-star-btn",
            onclick = sprintf(
              "sfSetRating('%s', %d, %d); return false;",
              item$id, i, item$rating_max %||% 5
            )
          )
        }),
        # Hidden numeric input carries the actual value
        numericInput(item$id, label = NULL, value = NA,
                     min = 1, max = item$rating_max %||% 5)
      )
    ),

    ranking = tags$div(
      class = "sf-ranking-block",
      lbl,
      tags$div(
        class = "sf-rank-list",
        id    = paste0("rank_", item$id),
        lapply(seq_along(cs), function(i) {
          tags$div(
            class         = "sf-rank-item",
            `data-value`  = names(cs)[i],
            tags$span(class = "sf-rank-handle", "\u283f"),
            tags$span(unname(cs)[i])
          )
        })
      ),
      tags$div(
        class = "sf-rank-input",
        textInput(item$id, label = NULL, value = paste(names(cs), collapse = "|"))
      )
    ),

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
                  lapply(unname(cs), function(lbl) tags$th(lbl))
                )
              ),
              tags$tbody(
                lapply(seq_along(rows), function(r) {
                  input_id <- paste0(item$id, "__", r)
                  tags$tr(
                    tags$td(class = "sf-matrix-row-label", rows[r]),
                    lapply(names(cs), function(v) {
                      tags$td(
                        class = "sf-matrix-cell",
                        tags$input(
                          type  = "radio",
                          name  = input_id,
                          value = v,
                          id    = paste0(input_id, "_", v),
                          onclick = sprintf(
                            "Shiny.setInputValue('%s', '%s', {priority:'event'})",
                            input_id, v
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

    textInput(item$id, lbl)  # fallback
  )
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
#' render_survey(instr)
#' render_survey(instr, save_responses = "csv", output_path = "responses.csv")
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

  # Collect answerable items (not section_break or text_block)
  answerable_types <- c("likert","single_choice","multiple_choice","numeric",
                        "text","textarea","date","slider","rating","ranking","matrix")

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
  ", colour, colour, colour, colour, colour, colour)
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
      # Drag-to-rank JS for ranking items
      tags$script(shiny::HTML("
        function initRanking(listId, inputId) {
          var list = document.getElementById(listId);
          if (!list) return;
          var dragging = null;
          list.querySelectorAll('.sf-rank-item').forEach(function(item) {
            item.setAttribute('draggable', 'true');
            item.addEventListener('dragstart', function() { dragging = item; item.style.opacity='.5'; });
            item.addEventListener('dragend',   function() { item.style.opacity='1'; updateRankInput(listId, inputId); });
            item.addEventListener('dragover',  function(e) { e.preventDefault(); var r=item.getBoundingClientRect(); var mid=r.top+r.height/2; if(e.clientY<mid) list.insertBefore(dragging,item); else list.insertBefore(dragging,item.nextSibling); });
          });
          function updateRankInput(listId, inputId) {
            var items = document.querySelectorAll('#'+listId+' .sf-rank-item');
            var vals = Array.from(items).map(function(i){return i.getAttribute('data-value');});
            Shiny.setInputValue(inputId, vals.join('|'), {priority:'event'});
          }
        }
        // Rating star handler
        function sfSetRating(id, val, max) {
          var btns = document.querySelectorAll('[id^=\"'+id+'_star_\"]');
          btns.forEach(function(b, i) { b.classList.toggle('active', i < val); });
          Shiny.setInputValue(id, val, {priority:'event'});
        }
      "))
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
      items  <- Filter(function(i) i$type %in% answerable_types, instrument$items)
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
      row <- sframe_response_row(instrument, iv, bl, started_at)
      err <- tryCatch({
        if (identical(save_responses, "csv"))
          sframe_append_response_csv(output_path, row)
        if (is.function(on_submit)) on_submit(row)
        NULL
      }, error = function(e) e)
      if (!is.null(err)) {
        shiny::showNotification(
          paste("Could not save response:", conditionMessage(err)),
          type = "error", duration = 8)
        return()
      }
      submitted_row(row)
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
      iv         <- input_values()
      all_items  <- instrument$items
      ans_items  <- Filter(function(i) i$type %in% answerable_types, all_items)
      answered   <- sum(vapply(ans_items, function(i) {
        sframe_item_visible(i, iv, branch_lookup) &&
          !sframe_missing_value(i, sframe_item_input_value(i, iv))
      }, logical(1)))
      n_visible  <- sum(vapply(ans_items, function(i) {
        sframe_item_visible(i, iv, branch_lookup)
      }, logical(1)))

      progress_ui <- if (show_progress && n_visible > 0)
        sframe_progress_ui(answered, n_visible, colour)

      # ---- CONVERSATIONAL MODE ----
      if (conv_mode) {
        cur  <- conv_idx()
        cur  <- min(cur, length(ans_items))
        item <- ans_items[[cur]]
        n    <- length(ans_items)

        item_ui <- tags$div(
          class = "sf-item-wrap",
          sframe_render_input(item, choices_lookup),
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
      items_ui <- lapply(all_items, function(item) {
        if (!sframe_item_visible(item, iv, branch_lookup)) return(NULL)
        tags$div(class = "sf-item-wrap",
          sframe_render_input(item, choices_lookup))
      })

      # Initialise ranking JS after render
      rank_init <- lapply(
        Filter(function(i) i$type == "ranking", all_items),
        function(item) {
          tags$script(sprintf(
            "setTimeout(function(){initRanking('rank_%s','%s');},200);",
            item$id, item$id))
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
