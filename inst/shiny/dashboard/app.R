# inst/shiny/dashboard/app.R
# Launched by launch_dashboard(). Reads SFRAME_INSTRUMENT and
# SFRAME_RESPONSES from the calling environment via app.R-level globals.

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Package 'shiny' is required. Install it with: install.packages('shiny')")
}

# shiny aliases — avoids library(shiny) which is not CRAN-safe in inst/ files
fluidPage      <- shiny::fluidPage
tagList        <- shiny::tagList
tags           <- shiny::tags
HTML           <- shiny::HTML
actionButton   <- shiny::actionButton
selectInput    <- shiny::selectInput
dateRangeInput <- shiny::dateRangeInput
uiOutput       <- shiny::uiOutput
plotOutput     <- shiny::plotOutput
tableOutput    <- shiny::tableOutput
downloadButton <- shiny::downloadButton
reactiveVal    <- shiny::reactiveVal
reactive       <- shiny::reactive
observeEvent   <- shiny::observeEvent
renderUI        <- shiny::renderUI
renderPlot      <- shiny::renderPlot
renderTable     <- shiny::renderTable
downloadHandler <- shiny::downloadHandler
req             <- shiny::req
outputOptions   <- shiny::outputOptions
shinyApp        <- shiny::shinyApp
div            <- shiny::tags$div
p              <- shiny::tags$p

`%||%` <- function(x, y) if (is.null(x)) y else x

instr     <- get("SFRAME_INSTRUMENT", inherits = TRUE)
responses <- get("SFRAME_RESPONSES",  inherits = TRUE)

# ── Helpers ─────────────────────────────────────────────────────────────────
THEME  <- instr$render$theme %||% "#2563eb"
TITLE  <- instr$meta$title %||% "Survey Dashboard"
n_resp <- if (is.null(responses)) 0L else nrow(responses)

item_by_id <- function(id) {
  x <- Filter(function(i) i$id == id, instr$items)
  if (length(x)) x[[1]] else NULL
}
scale_by_id <- function(id) {
  x <- Filter(function(s) s$id == id, instr$scales)
  if (length(x)) x[[1]] else NULL
}
choice_by_id <- function(id) {
  x <- Filter(function(c) c$id == id, instr$choices)
  if (length(x)) x[[1]] else NULL
}
q_items <- Filter(function(i) {
  !i$type %in% c("section_break", "text_block")
}, instr$items)

# ── Palette helper ──────────────────────────────────────────────────────────
hex_to_rgb <- function(h) {
  h <- sub("^#", "", h)
  if (nchar(h) == 3) h <- paste0(rep(strsplit(h,"")[[1]], each=2), collapse="")
  r <- strtoi(substr(h,1,2),16)
  g <- strtoi(substr(h,3,4),16)
  b <- strtoi(substr(h,5,6),16)
  c(r,g,b)
}
rgb_a <- function(h, a = 1) {
  v <- hex_to_rgb(h)
  sprintf("rgba(%d,%d,%d,%.2f)", v[1], v[2], v[3], a)
}

# ── CSS ─────────────────────────────────────────────────────────────────────
dash_css <- sprintf("
body{background:#f1f5f9;font-family:system-ui,-apple-system,'Segoe UI',sans-serif;margin:0}
.db-hdr{background:%s;color:#fff;padding:14px 24px;display:flex;align-items:center;gap:14px}
.db-hdr-t{font-size:18px;font-weight:700;flex:1}
.db-hdr-sub{font-size:12px;opacity:.75}
.db-nav{background:#1e293b;padding:8px 0;display:flex;gap:2px;padding-left:12px;flex-wrap:wrap}
.db-nav .btn{padding:7px 16px;border-radius:6px;font-size:12px;font-weight:500;color:rgba(255,255,255,.6);cursor:pointer;background:none;border:none;transition:all .15s}
.db-nav .btn.active{background:rgba(255,255,255,.12);color:#fff}
.db-nav .btn:hover:not(.active){background:rgba(255,255,255,.06);color:rgba(255,255,255,.85)}
.db-body{padding:22px 24px;max-width:1100px;margin:0 auto}
.db-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(180px,1fr));gap:14px;margin-bottom:22px}
.kpi{background:#fff;border-radius:10px;padding:18px;box-shadow:0 1px 4px rgba(0,0,0,.07)}
.kpi-v{font-size:28px;font-weight:700;color:%s;line-height:1}
.kpi-l{font-size:12px;color:#64748b;margin-top:5px}
.db-card{background:#fff;border-radius:10px;padding:20px;box-shadow:0 1px 4px rgba(0,0,0,.07);margin-bottom:16px}
.db-card-t{font-size:14px;font-weight:700;color:#0f172a;margin-bottom:14px;display:flex;align-items:center;gap:8px}
.db-card-t .badge{font-size:10px;font-weight:500;background:#f1f5f9;color:#64748b;padding:2px 7px;border-radius:9999px}
.item-grid{display:grid;grid-template-columns:1fr 1fr;gap:16px}
@media(max-width:720px){.item-grid{grid-template-columns:1fr}}
.tbl{width:100%%;border-collapse:collapse;font-size:12px}
.tbl th{background:#f8fafc;padding:7px 10px;text-align:left;border-bottom:2px solid #e2e8f0;font-weight:600;color:#475569}
.tbl td{padding:7px 10px;border-bottom:1px solid #f1f5f9;color:#0f172a}
.tbl tr:last-child td{border-bottom:none}
.tbl tr:hover td{background:#fafbfd}
.bar-row{display:flex;align-items:center;gap:8px;margin-bottom:5px}
.bar-lbl{font-size:11px;color:#475569;min-width:120px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.bar-bg{flex:1;background:#f1f5f9;border-radius:9999px;height:10px;overflow:hidden}
.bar-fill{height:10px;border-radius:9999px;background:%s;transition:width .3s}
.bar-ct{font-size:11px;color:#94a3b8;min-width:30px;text-align:right}
.miss-cell{display:inline-block;width:12px;height:12px;border-radius:2px;margin:1px}
.quality-flag{display:flex;align-items:center;gap:10px;padding:8px 12px;border-radius:6px;background:#fef2f2;border:1px solid #fecaca;margin-bottom:6px;font-size:12px;color:#991b1b}
.quality-ok{background:#f0fdf4;border-color:#bbf7d0;color:#166534}
.sel-wrap{margin-bottom:14px;display:flex;align-items:center;gap:10px}
.sel-wrap select{padding:7px 10px;border:1.5px solid #e2e8f0;border-radius:7px;font-size:13px;color:#0f172a;background:#fff}
.sel-wrap label{font-size:12px;font-weight:600;color:#475569}
.filter-bar{background:#fff;border-radius:10px;padding:14px 16px;box-shadow:0 1px 4px rgba(0,0,0,.07);margin-bottom:16px}
.filter-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(210px,1fr));gap:12px;align-items:end}
.filter-grid label{font-size:12px;font-weight:600;color:#475569}
.filter-note{font-size:11px;color:#94a3b8;margin-top:8px}
", THEME, THEME, THEME)

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$title(paste(TITLE, "Dashboard", sep = ": ")),
    tags$style(HTML(dash_css)),
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('sfDashNav', function(msg){
         document.querySelectorAll('.db-nav .btn').forEach(function(b){
           b.classList.remove('active');
         });
         var t=document.getElementById('nav_' + msg.active);
         if(t)t.classList.add('active');
       });"
    ))
  ),
  div(class = "db-hdr",
    div(class = "db-hdr-t", TITLE),
    div(class = "db-hdr-sub",
        paste0(n_resp, " response", if (n_resp != 1) "s" else ""))
  ),
  div(class = "db-nav",
    actionButton("nav_overview", "Overview",  class = "btn active"),
    actionButton("nav_items",    "Items",     class = "btn"),
    actionButton("nav_scales",   "Scales",    class = "btn"),
    actionButton("nav_quality",  "Quality",   class = "btn"),
    actionButton("nav_data",     "Raw Data",  class = "btn")
  ),
  div(class = "db-body", uiOutput("filter_bar"), uiOutput("db_content"))
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  active_tab <- reactiveVal("overview")
  date_filter_column <- dashboard_date_column(responses)
  categorical_filters <- dashboard_categorical_columns(responses)

  output$filter_bar <- renderUI({
    if (is.null(responses) || nrow(responses) == 0 ||
        (!length(categorical_filters) && is.null(date_filter_column))) {
      return(NULL)
    }

    cats <- lapply(categorical_filters, function(nm) {
      vals <- sort(unique(as.character(responses[[nm]][!is.na(responses[[nm]])])))
      if (!length(vals)) return(NULL)
      div(
        tags$label(nm),
        selectInput(
          paste0("filter_", nm),
          NULL,
          choices = vals,
          selected = character(0),
          multiple = TRUE,
          width = "100%"
        )
      )
    })
    cats <- Filter(Negate(is.null), cats)

    date_ui <- NULL
    if (!is.null(date_filter_column)) {
      d <- dashboard_parse_date(responses[[date_filter_column]])
      d <- d[!is.na(d)]
      if (length(d)) {
        date_ui <- div(
          tags$label(date_filter_column),
          dateRangeInput(
            "filter_date_range",
            NULL,
            start = min(d),
            end = max(d),
            min = min(d),
            max = max(d),
            width = "100%"
          )
        )
      }
    }

    div(
      class = "filter-bar",
      div(class = "db-card-t", "Filters"),
      div(class = "filter-grid", c(cats, list(date_ui))),
      div(class = "filter-note", "Leave a filter blank to include all responses.")
    )
  })

  filtered_responses <- reactive({
    if (is.null(responses)) return(NULL)
    out <- responses
    for (nm in categorical_filters) {
      selected <- input[[paste0("filter_", nm)]]
      if (length(selected)) {
        out <- out[as.character(out[[nm]]) %in% selected, , drop = FALSE]
      }
    }
    if (!is.null(date_filter_column) && !is.null(input$filter_date_range)) {
      d <- dashboard_parse_date(out[[date_filter_column]])
      rng <- as.Date(input$filter_date_range)
      keep <- is.na(d) | (d >= rng[1] & d <= rng[2])
      out <- out[keep, , drop = FALSE]
    }
    out
  })

  lapply(c("overview","items","scales","quality","data"), function(tab) {
    observeEvent(input[[paste0("nav_", tab)]], {
      active_tab(tab)
      session$sendCustomMessage("sfDashNav", list(active = tab))
    }, ignoreInit = TRUE)
  })

  output$db_content <- renderUI({
    resp <- filtered_responses()
    switch(active_tab(),
      overview = db_overview_ui(resp),
      items    = db_items_ui(input, resp),
      scales   = db_scales_ui(input, resp),
      quality  = db_quality_ui(resp),
      data     = db_data_ui(resp)
    )
  })

  # Item detail chart
  output$item_chart <- renderPlot({
    resp <- filtered_responses()
    req(input$item_sel, resp)
    item <- item_by_id(input$item_sel)
    if (is.null(item) || !input$item_sel %in% names(resp)) {
      plot.new(); text(.5,.5,"No data for this item.",col="#94a3b8"); return()
    }
    col_data <- resp[[input$item_sel]]
    t <- item$type
    old_par <- par(mar = c(4,5,2,1), bg = "white")
    on.exit(par(old_par))
    if (t %in% c("likert","single_choice","multiple_choice")) {
      cs <- choice_by_id(item$choice_set %||% "")
      if (!is.null(cs)) {
        freq <- table(factor(col_data, levels = as.character(cs$values)))
        names(freq) <- cs$labels
      } else {
        freq <- table(col_data)
      }
      barplot(freq, horiz = TRUE, las = 1,
              col = THEME, border = NA,
              main = NULL, xlab = "Frequency",
              cex.names = .8, cex.axis = .8)
    } else if (t %in% c("numeric","slider","rating")) {
      num <- suppressWarnings(as.numeric(col_data))
      num <- num[!is.na(num)]
      if (!length(num)) { plot.new(); text(.5,.5,"No numeric data."); return() }
      hist(num, col = THEME, border = "white",
           main = NULL, xlab = item$label,
           ylab = "Count", cex.axis = .8, cex.lab = .85, las = 1)
    } else {
      plot.new()
      text(.5,.5, paste0("Chart unavailable for type '", t, "'."),
           col = "#94a3b8", cex = .9)
    }
  }, bg = "white")
  # plotOutput("item_chart") lives inside renderUI, so Shiny would suspend this
  # output by default. suspendWhenHidden=FALSE keeps it evaluated and sends
  # the PNG as soon as the client-side plotOutput element appears in the DOM.
  outputOptions(output, "item_chart", suspendWhenHidden = FALSE)

  # Scale distribution chart
  output$scale_chart <- renderPlot({
    resp <- filtered_responses()
    req(input$scale_sel, resp)
    sc <- scale_by_id(input$scale_sel)
    if (is.null(sc)) return()
    score_cols <- intersect(sc$items, names(resp))
    if (!length(score_cols)) {
      plot.new(); text(.5,.5,"Scale items not found in responses.",col="#94a3b8"); return()
    }
    nums <- lapply(resp[score_cols], function(x) suppressWarnings(as.numeric(x)))
    scores <- rowMeans(do.call(cbind, nums), na.rm = TRUE)
    scores <- scores[!is.na(scores)]
    if (!length(scores)) {
      plot.new(); text(.5,.5,"No valid scale scores.",col="#94a3b8"); return()
    }
    old_par <- par(mar = c(4,4,2,1), bg = "white")
    on.exit(par(old_par))
    hist(scores, col = THEME, border = "white",
         main = NULL, xlab = paste0(sc$label, " score"),
         ylab = "Count", cex.axis = .8, cex.lab = .85, las = 1,
         breaks = min(nclass.Sturges(scores), 20))
    abline(v = mean(scores, na.rm = TRUE), col = "#dc2626", lwd = 2, lty = 2)
    legend("topright", legend = sprintf("M = %.2f", mean(scores, na.rm=TRUE)),
           col = "#dc2626", lty = 2, lwd = 2, bty = "n", cex = .8)
  }, bg = "white")
  outputOptions(output, "scale_chart", suspendWhenHidden = FALSE)

  # Raw data table
  output$raw_table <- renderTable({
    resp <- filtered_responses()
    if (is.null(resp)) return(data.frame(Note = "No responses loaded."))
    head(resp, 200)
  }, striped = TRUE, hover = TRUE, bordered = FALSE, width = "100%",
     na = "Not available")

  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0(gsub("[^a-zA-Z0-9]", "_", TITLE), "_responses.csv")
    },
    content = function(file) {
      if (!is.null(responses)) {
        utils::write.csv(filtered_responses(), file, row.names = FALSE, na = "")
      }
    }
  )
}

# ── Panel constructors ────────────────────────────────────────────────────────
db_overview_ui <- function(resp = responses) {
  n_current <- if (is.null(resp)) 0L else nrow(resp)
  date_range <- if (!is.null(resp) && "submitted_at" %in% names(resp)) {
    dts <- dashboard_parse_date(resp$submitted_at)
    dts <- dts[!is.na(dts)]
    if (length(dts) > 1) {
      paste0(format(min(dts), "%d %b"), " to ", format(max(dts), "%d %b %Y"))
    } else "Not available"
  } else "Not available"

  n_items  <- length(q_items)
  n_scales <- length(instr$scales)
  n_checks <- length(instr$checks)

  kpi <- function(val, lbl) {
    div(class = "kpi",
      div(class = "kpi-v", val),
      div(class = "kpi-l", lbl)
    )
  }

  tagList(
    div(class = "db-grid",
      kpi(n_current, "Filtered responses"),
      kpi(n_items,  "Survey items"),
      kpi(n_scales, "Scales defined"),
      kpi(n_checks, "Attention checks"),
      kpi(date_range, "Date range")
    ),
    div(class = "db-card",
      div(class = "db-card-t", "Instrument summary"),
      tags$table(class = "tbl",
        tags$thead(tags$tr(
          tags$th("Field"), tags$th("Value")
        )),
        tags$tbody(
          tags$tr(tags$td("Title"),   tags$td(TITLE)),
          tags$tr(tags$td("Version"), tags$td(instr$meta$version %||% "Not available")),
          tags$tr(tags$td("Authors"), tags$td(instr$meta$authors  %||% "Not available")),
          tags$tr(tags$td("Mode"),    tags$td(instr$render$mode   %||% "standard")),
          tags$tr(tags$td("Validated"), tags$td(
            if (isTRUE(instr$meta$validated)) "Yes" else "No"
          ))
        )
      )
    )
  )
}

db_items_ui <- function(input, resp = responses) {
  if (!length(q_items)) return(div(class="db-card","No question items defined."))
  choices_v <- stats::setNames(
    vapply(q_items, `[[`, character(1), "id"),
    vapply(q_items, function(i) paste0(i$id, ": ", substr(i$label %||% "", 1, 40)), character(1))
  )
  tagList(
    div(class = "sel-wrap",
      tags$label("Select item:"),
      selectInput("item_sel", NULL, choices = choices_v, width = "400px")
    ),
    div(class = "db-card",
      div(class = "db-card-t", "Response distribution"),
      if (is.null(resp) || nrow(resp) == 0) {
        p(style = "color:#94a3b8;font-size:13px",
          "Load response data with launch_dashboard(instrument, responses) to see charts.")
      } else {
        plotOutput("item_chart", height = "280px")
      }
    ),
    db_freq_table_ui(input, resp)
  )
}

db_freq_table_ui <- function(input, resp = responses) {
  if (is.null(resp) || nrow(resp) == 0 || is.null(input$item_sel)) return(NULL)
  item <- item_by_id(input$item_sel)
  if (is.null(item) || !input$item_sel %in% names(resp)) return(NULL)
  col_data <- resp[[input$item_sel]]

  if (!item$type %in% c("likert","single_choice","multiple_choice")) return(NULL)

  cs <- choice_by_id(item$choice_set %||% "")
  freq <- if (!is.null(cs)) {
    table(factor(col_data, levels = as.character(cs$values)))
  } else {
    table(col_data)
  }
  pct <- round(prop.table(freq) * 100, 1)

  rows <- lapply(names(freq), function(nm) {
    lbl <- if (!is.null(cs)) {
      idx <- match(nm, as.character(cs$values))
      if (!is.na(idx)) cs$labels[[idx]] else nm
    } else nm
    tags$tr(
      tags$td(lbl),
      tags$td(freq[[nm]]),
      tags$td(paste0(pct[[nm]], "%"))
    )
  })

  div(class = "db-card",
    div(class = "db-card-t", "Frequency table"),
    tags$table(class = "tbl",
      tags$thead(tags$tr(tags$th("Response"), tags$th("n"), tags$th("%"))),
      tags$tbody(rows)
    )
  )
}

db_scales_ui <- function(input, resp = responses) {
  if (!length(instr$scales)) {
    return(div(class="db-card","No scales defined in this instrument."))
  }
  sc_choices <- stats::setNames(
    vapply(instr$scales, `[[`, character(1), "id"),
    vapply(instr$scales, function(s) paste0(s$id, ": ", s$label), character(1))
  )
  tagList(
    div(class = "sel-wrap",
      tags$label("Select scale:"),
      selectInput("scale_sel", NULL, choices = sc_choices, width = "360px")
    ),
    div(class = "db-card",
      div(class = "db-card-t", "Scale score distribution"),
      if (is.null(resp) || nrow(resp) == 0) {
        p(style="color:#94a3b8;font-size:13px",
          "Load response data to see scale score distributions.")
      } else {
        plotOutput("scale_chart", height = "260px")
      }
    ),
    div(class = "db-card",
      div(class = "db-card-t", "Scale definitions"),
      tags$table(class = "tbl",
        tags$thead(tags$tr(
          tags$th("ID"), tags$th("Label"), tags$th("Method"),
          tags$th("Items"), tags$th("Reverse items")
        )),
        tags$tbody(lapply(instr$scales, function(sc) {
          tags$tr(
            tags$td(tags$code(sc$id)),
            tags$td(sc$label),
            tags$td(sc$method %||% "mean"),
            tags$td(paste(sc$items, collapse = ", ")),
            tags$td(paste(sc$reverse_items %||% "none", collapse = ", "))
          )
        }))
      )
    )
  )
}

db_quality_ui <- function(resp = responses) {
  n_current <- if (is.null(resp)) 0L else nrow(resp)
  if (!length(instr$checks)) {
    return(div(class="db-card","No attention checks defined in this instrument."))
  }
  rows <- lapply(instr$checks, function(ck) {
    n_pass <- if (!is.null(resp) && ck$item_id %in% names(resp)) {
      col_data <- as.character(resp[[ck$item_id]])
      pass_vals <- as.character(ck$pass_values %||% character(0))
      sum(col_data %in% pass_vals, na.rm = TRUE)
    } else NA_integer_

    pct_pass <- if (!is.null(resp) && !is.na(n_pass) && n_current > 0) {
      round(n_pass / n_current * 100, 1)
    } else NA_real_

    flag_class <- if (is.na(pct_pass)) ""
                  else if (pct_pass >= 80) "quality-ok"
                  else "quality-flag"

    tags$tr(class = flag_class,
      tags$td(tags$code(ck$id)),
      tags$td(ck$type %||% "attention"),
      tags$td(tags$code(ck$item_id)),
      tags$td(paste(ck$pass_values %||% "Not available", collapse = ", ")),
      tags$td(ck$fail_action %||% "flag"),
      tags$td(if (is.na(n_pass)) "Not available" else n_pass),
      tags$td(if (is.na(pct_pass)) "Not available" else paste0(pct_pass, "%"))
    )
  })

  div(class = "db-card",
    div(class = "db-card-t", "Attention check results"),
    tags$table(class = "tbl",
      tags$thead(tags$tr(
        tags$th("Check ID"), tags$th("Type"), tags$th("Item"),
        tags$th("Correct answer(s)"), tags$th("Fail action"),
        tags$th("n pass"), tags$th("% pass")
      )),
      tags$tbody(rows)
    )
  )
}

db_data_ui <- function(resp = responses) {
  n_current <- if (is.null(resp)) 0L else nrow(resp)
  tagList(
    div(class = "db-card",
      div(class = "db-card-t", "Raw responses",
        downloadButton("dl_csv", "Download CSV",
          style = sprintf(
            "margin-left:auto;padding:5px 12px;font-size:12px;background:%s;color:#fff;border:none;border-radius:6px;cursor:pointer",
            THEME
          ))
      ),
      if (is.null(resp) || n_current == 0) {
        p(style="color:#94a3b8;font-size:13px",
            "No response data loaded. Pass a data frame to launch_dashboard().")
      } else {
        tagList(
          p(style="font-size:11px;color:#94a3b8;margin-bottom:10px",
            paste0("Showing first 200 of ", n_current, " filtered rows.")),
          div(style="overflow-x:auto", tableOutput("raw_table"))
        )
      }
    )
  )
}

dashboard_categorical_columns <- function(data) {
  if (is.null(data) || !is.data.frame(data)) return(character(0))
  cols <- names(data)[vapply(data, function(x) {
    if (!(is.character(x) || is.factor(x) || is.logical(x))) return(FALSE)
    vals <- unique(x[!is.na(x)])
    length(vals) > 1 && length(vals) <= 12
  }, logical(1))]
  setdiff(cols, grep("(^|_)id$", cols, value = TRUE))
}

dashboard_date_column <- function(data) {
  if (is.null(data) || !is.data.frame(data)) return(NULL)
  candidates <- names(data)[vapply(data, function(x) {
    inherits(x, c("Date", "POSIXct", "POSIXlt")) ||
      (is.character(x) && any(!is.na(dashboard_parse_date(x))))
  }, logical(1))]
  candidates <- intersect(c("submitted_at", "started_at", candidates), candidates)
  if (length(candidates)) candidates[[1]] else NULL
}

dashboard_parse_date <- function(x) {
  if (inherits(x, "Date"))                   return(as.Date(x))
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.Date(x))
  if (!is.character(x))                      return(rep(as.Date(NA), length(x)))

  # Blanks and whitespace-only become NA before any format attempt.
  x_clean <- trimws(x)
  x_clean[!nzchar(x_clean)] <- NA_character_

  out  <- rep(as.Date(NA_character_), length(x))
  todo <- which(!is.na(x_clean))

  # Explicit formats: when format= is given, as.POSIXct returns NA for
  # non-matching strings instead of throwing an error. Try in priority order.
  formats <- c(
    "%Y-%m-%dT%H:%M:%SZ",  # ISO 8601 Z suffix  2024-01-15T10:30:00Z
    "%Y-%m-%dT%H:%M:%S",   # ISO 8601 no tz     2024-01-15T10:30:00
    "%Y-%m-%d %H:%M:%S",   # space datetime     2024-01-15 10:30:00
    "%Y-%m-%d",            # date only          2024-01-15
    "%d/%m/%Y",            # UK dd/mm/yyyy      15/01/2024
    "%m/%d/%Y"             # US mm/dd/yyyy      01/15/2024
  )

  for (fmt in formats) {
    if (!length(todo)) break
    parsed <- tryCatch(
      as.Date(as.POSIXct(x_clean[todo], format = fmt, tz = "UTC")),
      error = function(e) rep(as.Date(NA), length(todo))
    )
    good <- !is.na(parsed)
    if (any(good)) {
      out[todo[good]] <- parsed[good]
      todo <- todo[!good]
    }
  }

  # Last-resort auto-detection for any remaining unparsed strings, wrapped in
  # tryCatch so it can never crash the dashboard.
  if (length(todo)) {
    fallback <- tryCatch(
      as.Date(suppressWarnings(as.POSIXct(x_clean[todo], tz = "UTC"))),
      error = function(e) rep(as.Date(NA), length(todo))
    )
    good <- !is.na(fallback)
    if (any(good)) out[todo[good]] <- fallback[good]
  }

  out
}

# ── Launch ────────────────────────────────────────────────────────────────────
shinyApp(ui, server)
