# survey_module.R

#' Shiny module UI for an embedded survey
#'
#' Places a survey inside a larger Shiny application. Pair with
#' [survey_module_server()] in the server function. The module shows a
#' welcome screen, the instrument's pages with branching and required-item
#' checks, and a thank-you screen.
#'
#' Every item type is drawn with the same controls [render_survey()] uses, so
#' a response collected through the module has the same columns as one
#' collected there. [survey_module_server()] describes what is returned.
#'
#' @param id A character string. The module namespace ID, passed identically
#'   to [survey_module_server()].
#' @param width Character. CSS width for the survey card. Defaults to
#'   `"100%"`.
#'
#' @return A `shiny.tag` object.
#' @export
#' @seealso [survey_module_server()], [render_survey()],
#'   [export_static_survey()]
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(surveyframe)
#'
#' cs    <- sf_choices("ag5", 1:5, c("SD", "D", "N", "A", "SA"))
#' item  <- sf_item("q1", "Rate your experience.", type = "likert",
#'                  choice_set = "ag5", required = TRUE)
#' instr <- sf_instrument("Quick Survey", components = list(cs, item))
#' store <- file.path(tempdir(), "responses.csv")
#'
#' ui <- fluidPage(
#'   survey_module_ui("demo"),
#'   verbatimTextOutput("result")
#' )
#'
#' server <- function(input, output, session) {
#'   resp <- survey_module_server(
#'     "demo", instrument = instr,
#'     # Called before the survey is marked complete. An error here keeps the
#'     # respondent on the last page with a message, so nothing is lost.
#'     on_submit = function(response) {
#'       row <- as.data.frame(response, check.names = FALSE)
#'       utils::write.table(row, store, sep = ",", row.names = FALSE,
#'                          col.names = !file.exists(store),
#'                          append = file.exists(store))
#'     }
#'   )
#'   output$result <- renderPrint({
#'     req(resp())
#'     resp()
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
survey_module_ui <- function(id, width = "100%") {
  rlang::check_installed("shiny", reason = "to embed a survey module.")
  ns <- shiny::NS(id)

  shiny::div(
    id = ns("sf_module"),
    style = paste0("max-width:", width, ";margin:0 auto;font-family:",
                   "system-ui,-apple-system,'Segoe UI',sans-serif;"),
    shiny::tags$script(shiny::HTML(sframe_survey_js())),
    shiny::uiOutput(ns("survey_ui")),
    # Page changes bring the module itself into view. Scrolling the window
    # moved the whole host application.
    shiny::tags$script(
      shiny::HTML(
        sprintf(
          "Shiny.addCustomMessageHandler(%s, function(msg) {
             if (msg.action === 'scrollTop') {
               var el = document.getElementById(%s);
               if (el) el.scrollIntoView({block: 'start', behavior: 'smooth'});
             }
           });",
          sframe_js_string(ns("sfControl")), sframe_js_string(ns("sf_module"))
        )
      )
    )
  )
}

#' Shiny module server for an embedded survey
#'
#' Draws the survey and collects the respondent's answers. Returns a
#' `reactive` holding `NULL` until the survey has been submitted and saved.
#'
#' # Supported item types
#'
#' Every item type is supported, with the same controls [render_survey()]
#' uses: likert, single choice, multiple choice, numeric, text, text area,
#' date, slider, rating, ranking, matrix, pairwise comparison and criteria
#' weight, plus section breaks and text blocks.
#'
#' # What is submitted
#'
#' The response is a named list. It starts with `response_id`, `started_at`
#' and `submitted_at`, followed by one element per response column, named as
#' [read_responses()] expects. A multi-column item contributes one element per
#' column: `item__row` for a matrix, `item__option` for ranking and multiple
#' choice, and one element per pair or criterion for decision items. Values
#' are character.
#'
#' An item hidden by branching is `NA`, so an answer given before branching
#' hid its item is never submitted. An unanswered item is `NA` too. A slider
#' counts as answered once the respondent moves it, and a ranking once the
#' respondent reorders it or chooses "Keep this order". Date questions start
#' empty.
#'
#' # Saving, and a failed save
#'
#' `on_submit` is called with the response before the survey is marked
#' complete. If it raises an error, the respondent sees a message and stays on
#' the last page, can submit again, and the returned reactive stays `NULL`.
#' The thank-you screen appears only after `on_submit` returns.
#'
#' # Changing the instrument
#'
#' When `instrument` is a reactive and its value changes, the survey returns
#' to the welcome screen, the returned reactive goes back to `NULL`, and no
#' answer given to the previous instrument carries into the new one.
#'
#' @param id A character string matching the `id` passed to
#'   [survey_module_ui()].
#' @param instrument An `sframe` object, or a `reactive` that returns one.
#' @param on_submit Optional function of one argument, called with the
#'   response list before the survey is marked complete. Use it to store the
#'   response. An error it raises is shown to the respondent, and the survey
#'   stays open for another attempt.
#'
#' @return A `reactive` that returns `NULL` until a response is submitted and
#'   `on_submit`, when supplied, has returned. After that it returns the
#'   response list.
#' @export
#' @seealso [survey_module_ui()], [render_survey()], [read_responses()]
#'
#' @examples
#' \donttest{
#' # survey_module_ui() has a complete example, including on_submit.
#' }
survey_module_server <- function(id, instrument, on_submit = NULL) {
  rlang::check_installed("shiny", reason = "to use the survey module.")
  rlang::check_installed("digest", reason = "to generate survey response IDs.")
  if (!is.null(on_submit) && !is.function(on_submit))
    rlang::abort("`on_submit` must be NULL or a function.", class = "sframe_error")

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    instr_rx <- if (shiny::is.reactive(instrument)) instrument
                else shiny::reactive(instrument)

    state <- shiny::reactiveValues(
      screen     = "welcome",   # "welcome" | "survey" | "thankyou"
      page       = 1L,
      submitted  = NULL,
      started_at = NULL,
      # Each instrument gets fresh input ids, so an answer given to a previous
      # instrument can never be read as an answer to the current one.
      generation = 1L
    )

    prefix_rx <- shiny::reactive(paste0("sf", state$generation, "_"))

    # The answers to the current instrument, keyed by item and expansion id.
    values_rx <- shiny::reactive({
      prefix <- prefix_rx()
      all <- shiny::reactiveValuesToList(input)
      keep <- startsWith(names(all), prefix)
      stats::setNames(all[keep], substring(names(all)[keep], nchar(prefix) + 1L))
    })

    scroll_to_top <- function() {
      session$sendCustomMessage(ns("sfControl"), list(action = "scrollTop"))
    }

    shiny::observeEvent(instr_rx(), {
      state$generation <- state$generation + 1L
      state$screen     <- "welcome"
      state$page       <- 1L
      state$submitted  <- NULL
      state$started_at <- NULL
    }, ignoreInit = TRUE)

    # The page is drawn from the answers held so far, read through isolate(),
    # so an answer never draws the page again. Branching visibility moves with
    # answers through the observer below.
    output$survey_ui <- shiny::renderUI({
      instr  <- instr_rx()
      prefix <- prefix_rx()
      values <- shiny::isolate(values_rx())
      content <- switch(state$screen,
        welcome  = sf_mod_welcome(instr, ns, prefix),
        survey   = sf_mod_survey(instr, state$page, values, ns, prefix),
        thankyou = sf_mod_thankyou(instr$render$thankyou %||% list())
      )
      shiny::tagList(sf_mod_css(instr), shiny::div(class = "sf-mod", content))
    })

    shiny::observe({
      if (!identical(state$screen, "survey")) return()
      instr  <- instr_rx()
      values <- values_rx()
      full   <- ns(prefix_rx())
      bl     <- sframe_branch_lookup(instr)
      ids    <- vapply(instr$items, function(i) i$id, character(1))
      shown  <- vapply(instr$items, function(i) {
        sframe_item_visible(i, values, bl)
      }, logical(1))
      session$sendCustomMessage("sf-visibility",
        list(show = as.list(paste0(full, ids[shown])),
             hide = as.list(paste0(full, ids[!shown]))))
    })

    shiny::observeEvent(input$sf_start, {
      consent <- input[[paste0(prefix_rx(), "consent")]]
      wl <- instr_rx()$render$welcome %||% list()
      if (isTRUE(wl$consent_required) && !isTRUE(consent)) {
        shiny::showNotification("Please confirm your consent before continuing.",
                                type = "warning")
        return()
      }
      state$screen     <- "survey"
      state$page       <- 1L
      state$started_at <- Sys.time()
      scroll_to_top()
    })

    shiny::observeEvent(input$sf_next, {
      instr  <- instr_rx()
      values <- values_rx()
      bl     <- sframe_branch_lookup(instr)
      pages  <- sframe_module_build_pages(instr)
      page_items <- pages[[state$page]]

      missing <- Filter(function(i) {
        isTRUE(i$required) && sframe_item_visible(i, values, bl) &&
          sframe_missing_value(i, sframe_item_input_value(i, values))
      }, page_items)
      if (length(missing) > 0) {
        shiny::showNotification(
          paste("Please answer:",
                paste(vapply(missing, function(i) i$label, character(1)),
                      collapse = "; ")),
          type = "warning", duration = 6
        )
        return()
      }

      if (state$page >= length(pages)) {
        sframe_module_do_submit(state, instr, values, bl, on_submit)
      } else {
        state$page <- state$page + 1L
        scroll_to_top()
      }
    })

    shiny::observeEvent(input$sf_back, {
      if (state$page > 1L) {
        state$page <- state$page - 1L
        scroll_to_top()
      }
    })

    shiny::reactive(state$submitted)
  })
}

# ---------------------------------------------------------------------------
# Internal module helpers
# ---------------------------------------------------------------------------

sframe_module_build_pages <- function(instr) {
  mode <- instr$render$mode %||% "standard"
  items <- instr$items

  if (mode == "conversational") {
    return(lapply(items, list))
  }

  page_nums <- vapply(items,
                      function(i) as.integer(i$page %||% 1L),
                      integer(1))
  uq <- sort(unique(page_nums))
  lapply(uq, function(p) items[page_nums == p])
}

# Builds the response with the same row builder render_survey() uses, so both
# Shiny routes submit the same columns. on_submit runs first, and the survey
# is marked complete only once it returns.
sframe_module_do_submit <- function(state, instr, values, branch_lookup,
                                    on_submit) {
  row <- sframe_response_row(instr, values, branch_lookup,
                             started_at = state$started_at %||% Sys.time())
  resp <- c(
    list(response_id = paste0("R", toupper(substr(
      digest::digest(Sys.time()), 1, 8)))),
    as.list(row)
  )
  if (is.function(on_submit)) {
    err <- tryCatch({ on_submit(resp); NULL }, error = function(e) e)
    if (!is.null(err)) {
      shiny::showNotification(
        paste("Your response could not be saved:", conditionMessage(err),
              "Please try submitting again."),
        type = "error", duration = 8)
      return(invisible(FALSE))
    }
  }
  state$submitted <- resp
  state$screen    <- "thankyou"
  invisible(TRUE)
}

sf_mod_css <- function(instr) {
  theme <- instr$render$theme %||% "#2563eb"
  shiny::tags$style(shiny::HTML(sprintf(
    ".sf-mod{--cp:%s;font-family:system-ui,-apple-system,'Segoe UI',sans-serif;font-size:14px;color:#0f172a}
     .sf-mod .card{background:#fff;border-radius:10px;box-shadow:0 2px 8px rgba(0,0,0,.07);padding:28px;margin-bottom:12px}
     .sf-mod .sf-item-wrap{margin-bottom:22px}
     .sf-mod .sf-label-row{display:flex;align-items:center;gap:6px;font-weight:600;font-size:14px}
     .sf-mod .sf-required{color:#dc2626;font-size:11px}
     .sf-mod .sf-help-text{font-size:12px;color:#64748b;margin:3px 0 0;font-weight:400}
     .sf-mod .btn-p{background:var(--cp);color:#fff;border:none;border-radius:8px;padding:10px 24px;font-size:14px;font-weight:600;cursor:pointer;width:100%%}
     .sf-mod .btn-p:hover{filter:brightness(.92)}
     .sf-mod .btn-s,.sf-mod .btn-secondary{background:#f1f5f9;color:#0f172a;border:none;border-radius:8px;padding:10px 24px;font-size:14px;cursor:pointer}
     .sf-mod .nav{display:flex;gap:10px;margin-top:20px}
     .sf-mod .nav .sp{flex:1}
     .sf-mod .sf-section-break{border-top:3px solid var(--cp);padding-top:16px;margin-bottom:16px}
     .sf-mod .sf-section-title{font-size:17px;font-weight:700}
     .sf-mod .sf-text-block{background:#eff6ff;border-left:4px solid var(--cp);padding:12px;border-radius:0 8px 8px 0;font-size:13px;color:#1e40af;margin-bottom:14px}
     .sf-mod .sf-matrix-scroll{overflow-x:auto}
     .sf-mod .sf-matrix{border-collapse:collapse;width:100%%}
     .sf-mod .sf-matrix th,.sf-mod .sf-matrix td{padding:8px 10px;border:1px solid #e2e8f0;text-align:center;font-size:13px}
     .sf-mod .sf-matrix-row-label{text-align:left!important}
     .sf-mod .sf-stars{display:flex;gap:6px;margin-top:8px}
     .sf-mod .sf-star-btn{background:none;border:none;font-size:26px;cursor:pointer;color:#cbd5e1;padding:0}
     .sf-mod .sf-star-btn.active{color:#f59e0b}
     .sf-mod .sf-rank-list{border:1.5px solid #e2e8f0;border-radius:8px;padding:8px;margin-top:8px}
     .sf-mod .sf-rank-item{display:flex;align-items:center;gap:10px;padding:8px 10px;background:#f8fafc;border-radius:6px;margin-bottom:5px;border:1px solid #e2e8f0;cursor:grab}
     .sf-mod .sf-rank-label{flex:1}
     .sf-mod .sf-rank-moves{display:flex;gap:4px}
     .sf-mod .sf-rank-move{width:32px;height:32px;border:1px solid #cbd5e1;border-radius:6px;background:#fff;cursor:pointer}
     .sf-mod .sf-rank-move:focus-visible,.sf-mod .sf-rank-keep:focus-visible{outline:3px solid var(--cp);outline-offset:2px}
     .sf-mod .sf-rank-input{display:none}
     .sf-mod .sf-rank-confirm{display:flex;flex-wrap:wrap;align-items:center;gap:10px;margin-top:8px}
     .sf-mod .sf-rank-status,.sf-mod .sf-slider-hint{font-size:13px;color:#475569;margin:4px 0 0}
     .sf-mod .sf-decision-row{margin-bottom:10px}
     .sf-mod .pg-info{font-size:11px;color:#64748b;margin-bottom:14px}
     .sf-mod .ty-ic{font-size:48px;text-align:center;margin-bottom:12px}
     .sf-mod .ty-t{font-size:20px;font-weight:700;text-align:center;margin-bottom:8px}
     .sf-mod .ty-m{color:#475569;text-align:center;font-size:13px}",
    theme
  )))
}

sf_mod_welcome <- function(instr, ns, prefix) {
  wl <- instr$render$welcome %||% list()
  shiny::div(class = "card",
    shiny::tags$h2(style = "margin-bottom:8px", wl$title %||% instr$meta$title),
    if (!is.null(wl$intro_text)) shiny::p(style = "white-space:pre-wrap", wl$intro_text),
    if (!is.null(wl$consent_text))
      shiny::div(style = "background:#eff6ff;border:1.5px solid #bfdbfe;border-radius:8px;padding:14px;font-size:13px;color:#1e40af;margin-bottom:14px;white-space:pre-wrap",
                 wl$consent_text),
    if (isTRUE(wl$consent_required))
      shiny::checkboxInput(ns(paste0(prefix, "consent")),
                           "I have read and I consent to participate."),
    shiny::actionButton(ns("sf_start"),
                        wl$start_label %||% "Start Survey",
                        class = "btn-p",
                        style = "margin-top:16px;width:100%")
  )
}

sf_mod_survey <- function(instr, page, values, ns, prefix) {
  pages <- sframe_module_build_pages(instr)
  max_page <- length(pages)
  page_items <- pages[[page]]
  choices_lookup <- sframe_choices_lookup(instr)
  bl <- sframe_branch_lookup(instr)

  # The shared renderer uses an item's id as its input id, so each item is
  # drawn under its namespaced id, with the answers keyed the same way.
  full <- ns(prefix)
  drawn_values <- if (length(values)) {
    stats::setNames(values, paste0(full, names(values)))
  } else {
    list()
  }

  item_uis <- lapply(page_items, function(item) {
    drawn <- item
    drawn$id <- paste0(full, item$id)
    shiny::div(class = "sf-item-wrap",
      id = paste0("sf_item_", drawn$id),
      style = if (!sframe_item_visible(item, values, bl)) "display:none",
      sframe_render_input(drawn, choices_lookup, drawn_values))
  })

  rank_init <- lapply(
    Filter(function(i) identical(i$type, "ranking"), page_items),
    function(i) {
      shiny::tags$script(sprintf(
        "setTimeout(function(){initRanking(%s);},200);",
        sframe_js_string(paste0("rank_", full, i$id))))
    }
  )

  nav <- shiny::div(class = "nav",
    if (page > 1L)
      shiny::actionButton(ns("sf_back"), "\u2190 Back", class = "btn-s"),
    shiny::div(class = "sp"),
    shiny::actionButton(
      ns("sf_next"),
      if (page < max_page) "Next \u2192"
      else instr$render$submit_label %||% "Submit",
      class = "btn-p"
    )
  )

  shiny::div(class = "card",
    if (page == 1L) {
      shiny::tagList(
        shiny::tags$h2(style = "margin-bottom:6px", instr$meta$title),
        if (!is.null(instr$meta$description))
          shiny::p(style = "color:#475569;font-size:13px;margin-bottom:18px",
                   instr$meta$description)
      )
    },
    if (max_page > 1L)
      shiny::div(class = "pg-info",
                 paste0("Page ", page, " of ", max_page)),
    shiny::tagList(item_uis),
    shiny::tagList(rank_init),
    nav
  )
}

sf_mod_thankyou <- function(ty) {
  shiny::div(class = "card",
    shiny::div(class = "ty-ic", "\u2714"),
    shiny::div(class = "ty-t", "Thank You"),
    shiny::div(class = "ty-m",
               ty$message %||% "Your response has been recorded.")
  )
}
