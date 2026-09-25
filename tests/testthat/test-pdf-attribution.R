# Batch 4 #14: a PDF report recursively rendered the HTML and threw that return
# value away, then returned a bare path. The engine attribute the HTML path
# attaches was lost, so a reader could not tell which engine built the content,
# and the message announced the intermediate HTML path rather than the PDF that
# was asked for.

pdf_instrument <- function() {
  sf_instrument("Attribution", components = list(
    sf_item("q1", "A number", type = "numeric")))
}

test_that("14: an HTML report records its engine", {
  skip_on_cran()  # renders a report or runs a full plan: slow on CRAN's machines
  out <- tempfile(fileext = ".html")
  res <- suppressMessages(render_report(pdf_instrument(), output_file = out,
                                        include_analysis = FALSE))
  expect_true(attr(res, "engine") %in% c("quarto", "html"))
})

test_that("14: a PDF records the engine that built its content, and the print step", {
  skip_on_cran()
  skip_if_not_installed("pagedown")
  skip_if(!nzchar(Sys.which("google-chrome")) &&
          !nzchar(Sys.which("chromium")) &&
          !nzchar(Sys.which("chromium-browser")),
          "no local Chrome")

  out <- tempfile(fileext = ".pdf")
  res <- suppressMessages(render_report(pdf_instrument(), output_file = out,
                                        format = "pdf",
                                        include_analysis = FALSE))
  expect_true(file.exists(res))
  # the engine that produced the content, kept through the PDF step
  expect_true(attr(res, "engine") %in% c("quarto", "html"))
  # and the conversion that produced the file asked for
  expect_equal(attr(res, "converter"), "pagedown")
})

test_that("14: the announced path is the one that was asked for", {
  skip_on_cran()
  skip_if_not_installed("pagedown")
  skip_if(!nzchar(Sys.which("google-chrome")) &&
          !nzchar(Sys.which("chromium")) &&
          !nzchar(Sys.which("chromium-browser")),
          "no local Chrome")

  out <- tempfile(fileext = ".pdf")
  msgs <- character(0)
  withCallingHandlers(
    render_report(pdf_instrument(), output_file = out, format = "pdf",
                  include_analysis = FALSE),
    sframe_report_engine = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  # the PDF is named, where the intermediate HTML used to be announced
  expect_true(any(grepl(basename(out), msgs, fixed = TRUE)))
  expect_false(any(grepl("[.]html", utils::tail(msgs, 1))))
})
