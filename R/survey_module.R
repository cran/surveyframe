# survey_module.R

#' Shiny module UI for an embedded survey
#'
#' Places a survey rendered by surveyframe inside a larger Shiny application.
#' Pair with [survey_module_server()] in the server function. The module
#' renders the full instrument including welcome page, all item types,
#' branching logic, required-field validation, and a thank-you screen.
#'
#' @param id A character string. The module namespace ID, passed identically
#'   to [survey_module_server()].
#' @param width Character. CSS width for the survey card. Defaults to
#'   `"100%"`.
#'
#' @return A `shiny.tag` object.
#' @export
#' @seealso [survey_module_server()], [launch_studio()],
#'   [export_static_survey()]
#'
#' @examples
#' \donttest{
#' # Minimal embedding example:
#' library(shiny)
#' library(surveyframe)
#'
#' cs    <- sf_choices("ag5", 1:5, c("SD", "D", "N", "A", "SA"))
#' item  <- sf_item("q1", "Rate your experience.", type = "likert",
#'                  choice_set = "ag5", required = TRUE)
#' instr <- sf_instrument("Quick Survey", components = list(cs, item))
#'
#' ui <- fluidPage(
#'   survey_module_ui("demo"),
#'   verbatimTextOutput("result")
#' )
#'
#' server <- function(input, output, session) {
#'   resp <- survey_module_server("demo", instrument = instr)
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
    style = paste0("max-width:", width, ";margin:0 auto;font-family:",
                   "system-ui,-apple-system,'Segoe UI',sans-serif;"),
    shiny::uiOutput(ns("survey_ui")),
    shiny::tags$script(
      shiny::HTML(
        sprintf(
          "Shiny.addCustomMessageHandler('%s', function(msg) {
             if (msg.action === 'scrollTop') {
               window.scrollTo({top: 0, behavior: 'smooth'});
             }
           });",
          ns("sfControl")
        )
      )
    )
  )
}

#' Shiny module server for an embedded survey
#'
#' Renders the survey instrument and collects the respondent's answers.
#' Returns a `reactive` that holds `NULL` until the form is submitted, then
#' returns the response as a named list (one element per visible item).
#'
#' @param id A character string matching the `id` passed to
#'   [survey_module_ui()].
#' @param instrument An `sframe` object, or a `reactive` that returns one.
#'   Changing the reactive value resets the survey.
#' @param on_submit Optional function of one argument. Called immediately
#'   after submission with the response list. Useful for writing to a
#'   database or sending an email without waiting for an
#'   [shiny::observeEvent()] elsewhere in the app.
#'
#' @return A `reactive` that returns `NULL` before submission and the
#'   response list after.
#' @export
#' @seealso [survey_module_ui()]
#'
#' @examples
#' \donttest{
#' # See survey_module_ui() for a complete example.
#' }
survey_module_server <- function(id, instrument, on_submit = NULL) {
  rlang::check_installed("shiny", reason = "to use the survey module.")
  rlang::check_installed("digest", reason = "to generate survey response IDs.")

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Resolve instrument (reactive or plain object)
    instr_rx <- if (shiny::is.reactive(instrument)) instrument
                else shiny::reactive(instrument)

    # Module state
    state <- shiny::reactiveValues(
      screen      = "welcome",   # "welcome" | "survey" | "thankyou"
      page        = 1L,
      responses   = list(),
      submitted   = NULL,
      started_at  = NULL
    )

    # Pages: a list of item-lists grouped by item$page
    pages_rx <- shiny::reactive({
      instr <- instr_rx()
      sframe_module_build_pages(instr)
    })

    max_page_rx <- shiny::reactive(length(pages_rx()))

    # ---- Welcome ----------------------------------------------------------
    output$survey_ui <- shiny::renderUI(render_module_ui(
      state, instr_rx(), pages_rx(), max_page_rx(), ns
    ))

    # ---- Navigation observers --------------------------------------------
    shiny::observeEvent(input$sf_start, {
      if (!is.null(input$sf_consent) && isFALSE(input$sf_consent)) {
        shiny::showNotification("Please confirm your consent before continuing.",
                                type = "warning")
        return()
      }
      state$screen     <- "survey"
      state$page       <- 1L
      state$started_at <- Sys.time()
      session$sendCustomMessage(ns("sfControl"), list(action = "scrollTop"))
    })

    shiny::observeEvent(input$sf_next, {
      instr  <- instr_rx()
      items  <- pages_rx()[[state$page]]
      errors <- sframe_module_validate(items, state$responses, instr)

      if (length(errors) > 0) {
        shiny::showNotification(
          paste0(length(errors), " required question(s) need an answer."),
          type = "warning", duration = 4
        )
        return()
      }

      if (state$page >= max_page_rx()) {
        sframe_module_do_submit(state, instr_rx(), on_submit)
      } else {
        state$page <- state$page + 1L
        session$sendCustomMessage(ns("sfControl"), list(action = "scrollTop"))
      }
    })

    shiny::observeEvent(input$sf_back, {
      if (state$page > 1L) {
        state$page <- state$page - 1L
        session$sendCustomMessage(ns("sfControl"), list(action = "scrollTop"))
      }
    })

    # ---- Collect input values as they change ------------------------------
    shiny::observe({
      instr <- instr_rx()
      responses <- shiny::isolate(state$responses)
      changed <- FALSE
      lapply(instr$items, function(item) {
        inp_id <- paste0("sf_", item$id)
        val    <- input[[inp_id]]
        if (!is.null(val)) {
          responses[[item$id]] <- val
          changed <<- TRUE
        }
      })
      if (changed) {
        state$responses <- responses
      }
    })

    # ---- Return submitted responses ---------------------------------------
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

sframe_module_is_visible <- function(item_id, responses, branching) {
  rule <- Filter(function(r) r$item_id == item_id, branching)
  if (!length(rule)) return(TRUE)
  rule <- rule[[1]]
  actual <- responses[[rule$depends_on]]
  if (is.null(actual)) return(rule$action != "show")
  cond <- sframe_module_eval_op(rule$operator,
                                 as.character(actual),
                                 as.character(rule$value))
  if (rule$action == "show") cond else !cond
}

sframe_module_eval_op <- function(op, actual, value) {
  switch(op,
    "==" = actual == value,
    "!=" = actual != value,
    "%in%" = actual %in% strsplit(value, ",\\s*")[[1]],
    ">"   = suppressWarnings(!is.na(as.numeric(actual)) &&
               as.numeric(actual) > as.numeric(value)),
    ">="  = suppressWarnings(!is.na(as.numeric(actual)) &&
               as.numeric(actual) >= as.numeric(value)),
    "<"   = suppressWarnings(!is.na(as.numeric(actual)) &&
               as.numeric(actual) < as.numeric(value)),
    "<="  = suppressWarnings(!is.na(as.numeric(actual)) &&
               as.numeric(actual) <= as.numeric(value)),
    FALSE
  )
}

sframe_module_validate <- function(items, responses, instr) {
  errors <- character(0)
  for (item in items) {
    if (!item$required) next
    if (!sframe_module_is_visible(item$id, responses, instr$branching)) next
    v <- responses[[item$id]]
    if (is.null(v) || identical(v, "") || identical(v, character(0))) {
      errors <- c(errors, item$id)
    }
  }
  errors
}

sframe_module_do_submit <- function(state, instr, on_submit) {
  resp <- c(
    list(
      response_id  = paste0("R", toupper(substr(
        digest::digest(Sys.time()), 1, 8))),
      started_at   = format(state$started_at, "%Y-%m-%dT%H:%M:%SZ",
                            tz = "UTC"),
      submitted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    state$responses
  )
  state$submitted <- resp
  state$screen    <- "thankyou"
  if (is.function(on_submit)) on_submit(resp)
}

render_module_ui <- function(state, instr, pages, max_page, ns) {
  theme <- instr$render$theme %||% "#2563eb"
  wl    <- instr$render$welcome   %||% list()
  ty    <- instr$render$thankyou  %||% list()

  css <- shiny::tags$style(shiny::HTML(sprintf(
    ".sf-mod{--cp:%s;font-family:system-ui,-apple-system,'Segoe UI',sans-serif;font-size:14px;color:#0f172a}
     .sf-mod .card{background:#fff;border-radius:10px;box-shadow:0 2px 8px rgba(0,0,0,.07);padding:28px;margin-bottom:12px}
     .sf-mod .item{margin-bottom:22px}
     .sf-mod .q-lbl{font-weight:600;margin-bottom:8px;font-size:14px}
     .sf-mod .help-t{font-size:12px;color:#94a3b8;margin-bottom:6px;margin-top:-4px}
     .sf-mod .btn-p{background:var(--cp);color:#fff;border:none;border-radius:8px;padding:10px 24px;font-size:14px;font-weight:600;cursor:pointer;width:100%%}
     .sf-mod .btn-p:hover{filter:brightness(.92)}
     .sf-mod .btn-s{background:#f1f5f9;color:#0f172a;border:none;border-radius:8px;padding:10px 24px;font-size:14px;cursor:pointer}
     .sf-mod .inp{width:100%%;border:1.5px solid #e2e8f0;border-radius:8px;padding:9px 12px;font-size:14px}
     .sf-mod .inp:focus{outline:none;border-color:var(--cp);box-shadow:0 0 0 3px rgba(37,99,235,.1)}
     .sf-mod .opt{display:flex;align-items:flex-start;gap:8px;padding:7px 10px;border-radius:6px;cursor:default;font-size:13px}
     .sf-mod .opt:hover{background:#f1f5f9}
     .sf-mod .opt input{accent-color:var(--cp);margin-top:2px;flex-shrink:0}
     .sf-mod .nav{display:flex;gap:10px;margin-top:20px}
     .sf-mod .nav .sp{flex:1}
     .sf-mod .sec{border-top:3px solid var(--cp);padding-top:16px;margin-bottom:16px}
     .sf-mod .sec-t{font-size:17px;font-weight:700}
     .sf-mod .pg-info{font-size:11px;color:#94a3b8;margin-bottom:14px}
     .sf-mod .ty-ic{font-size:48px;text-align:center;margin-bottom:12px}
     .sf-mod .ty-t{font-size:20px;font-weight:700;text-align:center;margin-bottom:8px}
     .sf-mod .ty-m{color:#475569;text-align:center;font-size:13px}",
    theme, theme
  )))

  content <- switch(state$screen,
    welcome  = sf_mod_welcome(instr, wl, ns),
    survey   = sf_mod_survey(state, instr, pages, max_page, ns),
    thankyou = sf_mod_thankyou(ty)
  )

  shiny::tagList(css, shiny::div(class = "sf-mod", content))
}

sf_mod_welcome <- function(instr, wl, ns) {
  shiny::div(class = "card",
    shiny::tags$h2(style = "margin-bottom:8px", wl$title %||% instr$meta$title),
    if (!is.null(wl$intro_text)) shiny::p(style = "white-space:pre-wrap", wl$intro_text),
    if (!is.null(wl$consent_text))
      shiny::div(style = "background:#eff6ff;border:1.5px solid #bfdbfe;border-radius:8px;padding:14px;font-size:13px;color:#1e40af;margin-bottom:14px;white-space:pre-wrap",
                 wl$consent_text),
    if (isTRUE(wl$consent_required))
      shiny::checkboxInput(ns("sf_consent"), "I have read and I consent to participate."),
    shiny::actionButton(ns("sf_start"),
                        wl$start_label %||% "Start Survey",
                        class = "btn-p",
                        style = "margin-top:16px;width:100%")
  )
}

sf_mod_survey <- function(state, instr, pages, max_page, ns) {
  items   <- pages[[state$page]]
  choiceM <- stats::setNames(instr$choices,
                              vapply(instr$choices, `[[`, character(1), "id"))
  resp    <- state$responses
  brch    <- instr$branching

  item_uis <- lapply(items, function(item) {
    visible <- sframe_module_is_visible(item$id, resp, brch)
    if (!visible) return(NULL)
    sf_mod_render_item(item, choiceM, ns, resp)
  })

  nav <- shiny::div(class = "nav",
    if (state$page > 1L)
      shiny::actionButton(ns("sf_back"), "\u2190 Back", class = "btn-s"),
    shiny::div(class = "sp"),
    shiny::actionButton(
      ns("sf_next"),
      if (state$page < max_page) "Next \u2192"
      else instr$render$submit_label %||% "Submit",
      class = "btn-p"
    )
  )

  shiny::div(class = "card",
    if (state$page == 1L) {
      shiny::tagList(
        shiny::tags$h2(style = "margin-bottom:6px", instr$meta$title),
        if (!is.null(instr$meta$description))
          shiny::p(style = "color:#475569;font-size:13px;margin-bottom:18px",
                   instr$meta$description)
      )
    },
    if (max_page > 1L)
      shiny::div(class = "pg-info",
                 paste0("Page ", state$page, " of ", max_page)),
    shiny::tagList(item_uis),
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

sf_mod_render_item <- function(item, choiceM, ns, resp) {
  t  <- item$type
  cs <- choiceM[[item$choice_set %||% ""]]

  if (t == "section_break") {
    return(shiny::div(class = "sec",
      shiny::div(class = "sec-t", item$label),
      if (!is.null(item$section_intro)) shiny::p(item$section_intro)
    ))
  }
  if (t == "text_block") {
    return(shiny::div(
      style = "background:#eff6ff;border-left:4px solid var(--cp);padding:12px;border-radius:0 8px 8px 0;font-size:13px;color:#1e40af;margin-bottom:14px",
      item$label
    ))
  }

  lbl_tag <- shiny::div(class = "q-lbl",
    item$label,
    if (isTRUE(item$required))
      shiny::span(style = "color:#dc2626;font-size:11px;margin-left:3px", "*")
  )
  help_tag <- if (!is.null(item$help))
    shiny::div(class = "help-t", item$help)

  ctrl <- switch(t,
    likert         = sf_mod_likert(item, cs, ns, resp),
    single_choice  = sf_mod_single(item, cs, ns, resp),
    multiple_choice= sf_mod_multi(item, cs, ns, resp),
    numeric = shiny::numericInput(ns(paste0("sf_", item$id)), NULL,
                                   value = resp[[item$id]] %||% NA),
    text    = shiny::textInput(ns(paste0("sf_", item$id)), NULL,
                                value    = resp[[item$id]] %||% "",
                                placeholder = item$placeholder %||% ""),
    textarea= shiny::textAreaInput(ns(paste0("sf_", item$id)), NULL,
                                    value       = resp[[item$id]] %||% "",
                                    placeholder = item$placeholder %||% "",
                                    rows        = 4),
    date    = shiny::dateInput(ns(paste0("sf_", item$id)), NULL,
                                value = resp[[item$id]] %||% Sys.Date()),
    slider  = shiny::sliderInput(ns(paste0("sf_", item$id)), NULL,
                                  min   = item$slider_min %||% 0,
                                  max   = item$slider_max %||% 100,
                                  step  = item$slider_step %||% 1,
                                  value = resp[[item$id]] %||%
                                    round((item$slider_min %||% 0 +
                                             item$slider_max %||% 100) / 2)),
    shiny::p(style = "color:#94a3b8;font-size:12px",
             paste0("Item type '", t, "' renders in the full studio."))
  )

  shiny::div(class = "item",
    lbl_tag, help_tag, ctrl
  )
}

sf_mod_likert <- function(item, cs, ns, resp) {
  if (is.null(cs)) return(shiny::p("Choice set not found.", style = "color:#dc2626"))
  saved <- resp[[item$id]] %||% character(0)
  choices <- stats::setNames(as.character(cs$values), cs$labels)
  shiny::radioButtons(ns(paste0("sf_", item$id)), NULL,
                       choices = choices, selected = saved,
                       inline  = TRUE)
}

sf_mod_single <- function(item, cs, ns, resp) {
  if (is.null(cs)) return(shiny::p("Choice set not found.", style = "color:#dc2626"))
  saved <- resp[[item$id]] %||% character(0)
  choices <- stats::setNames(as.character(cs$values), cs$labels)
  shiny::radioButtons(ns(paste0("sf_", item$id)), NULL,
                       choices = choices, selected = saved)
}

sf_mod_multi <- function(item, cs, ns, resp) {
  if (is.null(cs)) return(shiny::p("Choice set not found.", style = "color:#dc2626"))
  saved_raw <- resp[[item$id]] %||% ""
  saved     <- strsplit(as.character(saved_raw), ",")[[1]]
  choices   <- stats::setNames(as.character(cs$values), cs$labels)
  shiny::checkboxGroupInput(ns(paste0("sf_", item$id)), NULL,
                             choices = choices, selected = saved)
}
