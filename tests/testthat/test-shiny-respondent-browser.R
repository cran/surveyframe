# tests/testthat/test-shiny-respondent-browser.R
#
# Drives render_survey() in a real headless Chrome. A10 only happens in a
# browser: the server received each answer, then the page re-drew its controls
# at their defaults and they reported those defaults back, erasing the answer.
# testServer() cannot see that, so the proof has to be a browser.
#
# Skipped on CRAN and wherever chromote or Chrome is unavailable.

skip_on_cran()
skip_if_not_installed("shiny")
skip_if_not_installed("chromote")
skip_if_not_installed("callr")
chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
skip_if(length(chrome) != 1L || is.na(chrome) || !nzchar(chrome),
        "Chrome not available")

# covr instruments every expression in the child process, so loading the
# package and starting Shiny can take substantially longer than in a normal
# R CMD check. This is a startup deadline, not a fixed delay once the app is
# ready.
.browser_start_attempts <- 180L

browser_instrument <- function() {
  sf_instrument(title = "Browser", components = list(
    sf_choices("yn", values = c(0, 1), labels = c("No", "Yes")),
    sf_choices("agree", values = 1:3,
               labels = c("Disagree", "Neutral", "Agree")),
    sf_item("q1", "Do you smoke?", "single_choice", choice_set = "yn"),
    sf_item("t1", "Comment", "text"),
    sf_item("dob", "Date of birth", "date"),
    sf_item("rk", "Rank these", "ranking", choice_set = "agree"),
    sf_item("mx", "Rate each", "matrix", choice_set = "agree",
            matrix_items = c("price", "service")),
    sf_item("sl", "Satisfaction", "slider", slider_min = 0, slider_max = 10)
  ))
}

# An installed package (as under covr) also has a DESCRIPTION, but no R
# sources, so load_all() only applies where .R files exist.
has_package_source <- function(dir) {
  file.exists(file.path(dir, "DESCRIPTION")) &&
    length(list.files(file.path(dir, "R"), pattern = "\\.[Rr]$")) > 0L
}

# Kills the child and builds an error that carries what it wrote to stderr,
# so a failed start reports its cause instead of only a timeout.
start_failure <- function(proc, what) {
  alive <- proc$is_alive()
  err <- tryCatch(paste(utils::tail(proc$read_error_lines(), 15L),
                        collapse = "\n"), error = function(e) "")
  proc$kill()
  paste0("the ", what, " app did not start within 90 seconds (child ",
         if (alive) "still running" else "exited", ")\n", err)
}

# Serves the app in a background R process and returns a live session.
serve_survey <- function(instrument, csv) {
  src <- test_path("..", "..")
  from_source <- has_package_source(src)
  port <- httpuv::randomPort()
  proc <- callr::r_bg(function(src, from_source, instrument, csv, port) {
    if (from_source) pkgload::load_all(src, quiet = TRUE)
    else library(surveyframe)
    app <- render_survey(instrument, save_responses = "csv", output_path = csv)
    shiny::runApp(app, port = port, launch.browser = FALSE)
  }, args = list(normalizePath(src), from_source, instrument, csv, port))
  url <- paste0("http://127.0.0.1:", port)
  for (i in seq_len(.browser_start_attempts)) {
    up <- tryCatch({ suppressWarnings(readLines(url, n = 1)); TRUE },
                   error = function(e) FALSE)
    if (up) break
    Sys.sleep(0.5)
  }
  if (!up) {
    stop(start_failure(proc, "survey"))
  }
  b <- chromote::ChromoteSession$new(width = 1000, height = 2000)
  b$Page$navigate(url)
  # Wait for the survey page itself to be drawn and bound, not a fixed time.
  ready <- "!!(document.getElementById('submit_btn') && window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())"
  for (i in seq_len(.browser_start_attempts)) {
    if (isTRUE(b$Runtime$evaluate(ready, returnByValue = TRUE)$result$value)) break
    Sys.sleep(0.5)
  }
  Sys.sleep(1)
  list(b = b, proc = proc)
}

page_js <- function(s, code) {
  s$b$Runtime$evaluate(code, returnByValue = TRUE)$result$value
}

click_at <- function(s, selector) {
  xy <- jsonlite::fromJSON(page_js(s, sprintf(
    "var e=document.querySelector(%s); e.scrollIntoView({block:'center'});
     var r=e.getBoundingClientRect();
     JSON.stringify({x:r.left+r.width/2, y:r.top+r.height/2})",
    jsonlite::toJSON(selector, auto_unbox = TRUE))))
  for (t in c("mousePressed", "mouseReleased"))
    s$b$Input$dispatchMouseEvent(type = t, x = xy$x, y = xy$y,
                                 button = "left", clickCount = 1)
}

# Chrome activates a focused button on Enter only when the key event carries
# its text, as a real key press does.
press_enter <- function(s) {
  s$b$Input$dispatchKeyEvent(type = "keyDown", key = "Enter", code = "Enter",
                             windowsVirtualKeyCode = 13L, text = "\r")
  s$b$Input$dispatchKeyEvent(type = "keyUp", key = "Enter", code = "Enter",
                             windowsVirtualKeyCode = 13L)
}

stop_survey <- function(s) {
  try(s$b$close(), silent = TRUE)
  s$proc$kill()
}

test_that("A10: answers survive in the browser and reach the stored row", {
  csv <- tempfile(fileext = ".csv")
  s <- serve_survey(browser_instrument(), csv)
  on.exit(stop_survey(s))

  click_at(s, "input[name=q1][value='1']")
  Sys.sleep(2)
  expect_identical(page_js(s, "(document.querySelector('input[name=q1]:checked')||{}).value||'NONE'"), "1")

  # Typed one character at a time, slower than the text input debounce, so
  # each character reaches the server. A page that re-draws on an answer
  # moves focus out of the field after the first character.
  page_js(s, "var t=document.getElementById('t1'); t.focus(); 'ok'")
  for (ch in strsplit("hello", "")[[1]]) {
    s$b$Input$dispatchKeyEvent(type = "keyDown", key = ch, text = ch)
    s$b$Input$dispatchKeyEvent(type = "keyUp", key = ch)
    Sys.sleep(0.6)
  }
  Sys.sleep(1)
  expect_identical(page_js(s, "document.getElementById('t1').value"), "hello")
  # the earlier answer is still there after a later one
  expect_identical(page_js(s, "(document.querySelector('input[name=q1]:checked')||{}).value||'NONE'"), "1")

  click_at(s, "input[name=mx__1][value='3']")
  Sys.sleep(2)

  click_at(s, "#submit_btn")
  Sys.sleep(3)
  row <- utils::read.csv(csv, colClasses = "character", check.names = FALSE)
  expect_identical(row$q1, "1")
  expect_identical(row$t1, "hello")
  expect_identical(row$mx__price, "3")
})

test_that("A6: date and slider start unanswered and submit nothing untouched", {
  csv <- tempfile(fileext = ".csv")
  s <- serve_survey(browser_instrument(), csv)
  on.exit(stop_survey(s))

  expect_identical(page_js(s, "document.querySelector('#dob input').value"), "")

  click_at(s, "input[name=q1][value='0']")
  Sys.sleep(1)
  click_at(s, "#submit_btn")
  Sys.sleep(3)
  row <- utils::read.csv(csv, colClasses = "character", check.names = FALSE,
                         na.strings = character())
  expect_identical(row$q1, "0")
  expect_identical(row$dob, "")
  expect_identical(row$sl, "")
  expect_identical(c(row$rk__1, row$rk__2, row$rk__3), c("", "", ""))
})

test_that("A5: a ranking can be ordered from the keyboard alone", {
  csv <- tempfile(fileext = ".csv")
  s <- serve_survey(browser_instrument(), csv)
  on.exit(stop_survey(s))

  # Focus "Move Agree up" and press Enter twice, from the keyboard only.
  page_js(s, "document.querySelector('button[aria-label=\"Move Agree up\"]').focus(); 'ok'")
  press_enter(s)
  Sys.sleep(0.5)
  press_enter(s)
  Sys.sleep(1)

  order <- page_js(s, "Array.from(document.querySelectorAll('#rank_rk .sf-rank-item')).map(function(e){return e.getAttribute('data-value')}).join('|')")
  expect_identical(order, "3|1|2")
  # focus stays on the control the participant is using
  expect_identical(page_js(s, "document.activeElement.getAttribute('aria-label')"),
                   "Move Agree up")
  expect_match(page_js(s, "document.querySelector('#rank_rk').closest('.sf-ranking-block').querySelector('.sf-rank-status').textContent"),
               "position 1 of 3")

  click_at(s, "#submit_btn")
  Sys.sleep(3)
  row <- utils::read.csv(csv, colClasses = "character", check.names = FALSE)
  expect_identical(c(row$rk__1, row$rk__2, row$rk__3), c("2", "3", "1"))
})

# ── the reusable module, embedded in a host application ───────────────────

serve_module <- function(instrument, csv) {
  src <- test_path("..", "..")
  from_source <- has_package_source(src)
  port <- httpuv::randomPort()
  proc <- callr::r_bg(function(src, from_source, instrument, csv, port) {
    if (from_source) pkgload::load_all(src, quiet = TRUE)
    else library(surveyframe)
    ui <- shiny::fluidPage(shiny::tags$h1("Host application"),
                           survey_module_ui("embedded"))
    server <- function(input, output, session) {
      survey_module_server("embedded", instrument, on_submit = function(r) {
        utils::write.csv(as.data.frame(r, check.names = FALSE), csv,
                         row.names = FALSE)
      })
    }
    shiny::runApp(shiny::shinyApp(ui, server), port = port,
                  launch.browser = FALSE)
  }, args = list(normalizePath(src), from_source, instrument, csv, port))
  url <- paste0("http://127.0.0.1:", port)
  up <- FALSE
  for (i in seq_len(.browser_start_attempts)) {
    up <- tryCatch({ suppressWarnings(readLines(url, n = 1)); TRUE },
                   error = function(e) FALSE)
    if (up) break
    Sys.sleep(0.5)
  }
  if (!up) stop(start_failure(proc, "module"))
  b <- chromote::ChromoteSession$new(width = 1000, height = 2400)
  b$Page$navigate(url)
  ready <- "!!(document.getElementById('embedded-sf_start') && window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())"
  for (i in seq_len(.browser_start_attempts)) {
    if (isTRUE(b$Runtime$evaluate(ready, returnByValue = TRUE)$result$value)) break
    Sys.sleep(0.5)
  }
  Sys.sleep(1)
  list(b = b, proc = proc)
}

test_that("A7: the embedded module collects advanced items in a real browser", {
  ins <- sf_instrument(title = "Embedded", components = list(
    sf_choices("agree", values = 1:3,
               labels = c("Disagree", "Neutral", "Agree")),
    sf_item("mx", "Rate each", "matrix", choice_set = "agree",
            matrix_items = c("price", "service"), required = TRUE),
    sf_item("rt", "Stars", "rating", rating_max = 5, required = TRUE),
    sf_item("rk", "Rank", "ranking", choice_set = "agree", required = TRUE),
    sf_item("pw", "Compare", "pairwise_comparison",
            comparison_items = c("price", "quality"), required = TRUE),
    sf_item("sl", "Satisfaction", "slider", slider_min = 0, slider_max = 10,
            required = TRUE)
  ))
  csv <- tempfile(fileext = ".csv")
  s <- serve_module(ins, csv)
  on.exit(stop_survey(s))

  click_at(s, "#embedded-sf_start")
  Sys.sleep(2)
  click_at(s, "input[name='embedded-sf1_mx__1'][value='3']")
  click_at(s, "input[name='embedded-sf1_mx__2'][value='1']")
  click_at(s, "#embedded-sf1_rt_star_4")
  page_js(s, "document.querySelector('button[aria-label=\"Move Agree up\"]').focus(); 'ok'")
  press_enter(s)
  Sys.sleep(0.5)
  page_js(s, "var e=document.getElementById('embedded-sf1_pw__price__vs__quality'); e.value='5'; e.dispatchEvent(new Event('change',{bubbles:true})); 'ok'")
  # move the slider handle, which is what marks a slider answered
  click_at(s, "[data-sf-slider='embedded-sf1_sl'] .irs-line")
  Sys.sleep(1)
  # the host page has not been scrolled away by the module
  expect_match(page_js(s, "document.body.innerText"), "Host application")

  click_at(s, "#embedded-sf_next")
  Sys.sleep(3)
  expect_true(file.exists(csv))
  row <- utils::read.csv(csv, colClasses = "character", check.names = FALSE,
                         na.strings = "NA")
  expect_identical(row$mx__price, "3")
  expect_identical(row$mx__service, "1")
  expect_identical(row$rt, "4")
  expect_identical(c(row$rk__1, row$rk__2, row$rk__3), c("1", "3", "2"))
  expect_identical(row$pw__price__vs__quality, "5")
  expect_false(is.na(row$sl))
  expect_match(page_js(s, "document.body.innerText"), "Thank You")
})
