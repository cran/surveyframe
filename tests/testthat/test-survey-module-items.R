# tests/testthat/test-survey-module-items.R
#
# The reusable module, survey_module_ui() and survey_module_server().
#
# A7.  Matrix, rating, ranking, pairwise comparison and criteria weight drew a
#      placeholder paragraph with no control, while validation still demanded
#      an answer. A survey with any of them required could not be completed.
#      The module now draws every item with the same renderer as
#      render_survey(), so the 2 Shiny routes collect identically.
# F4.  Changing a reactive instrument did not reset the survey.
# F5.  A cleared answer, and an answer to an item branching later hid, were
#      still submitted.
# F6.  Restoring a multiple-choice answer kept only the first selection.
# F7.  The survey was marked complete before on_submit() ran, so a failed save
#      still showed the thank-you screen.
# F15. The module scrolled the host application's whole window.
# A6 in the module. A date question started on today, and a slider on its
#      midpoint, and both were submitted untouched.

skip_if_not_installed("shiny")
skip_if_not_installed("digest")

# The module's input id for an item or expansion column, in the first
# instrument it is given.
mid <- function(id, generation = 1) paste0("sf", generation, "_", id)


advanced_instrument <- function() {
  sf_instrument(title = "Module", components = list(
    sf_choices("agree", values = 1:3,
               labels = c("Disagree", "Neutral", "Agree")),
    sf_item("mx", "Rate each", "matrix", choice_set = "agree",
            matrix_items = c("price", "service"), required = TRUE),
    sf_item("rt", "Stars", "rating", rating_max = 5, required = TRUE),
    sf_item("rk", "Rank", "ranking", choice_set = "agree", required = TRUE),
    sf_item("pw", "Compare", "pairwise_comparison",
            comparison_items = c("price", "quality"), required = TRUE),
    sf_item("cw", "Allocate", "criteria_weight",
            comparison_items = c("price", "quality"), required = TRUE)
  ))
}

test_that("A7: every advanced item type draws a control", {
  shiny::testServer(survey_module_server,
                    args = list(instrument = advanced_instrument()), {
    session$setInputs(sf_start = 1)
    html <- as.character(output$survey_ui$html)

    expect_false(grepl("renders in the full studio", html, fixed = TRUE))
    expect_match(html, sprintf('name="%s"', session$ns(mid("mx__1"))), fixed = TRUE)
    expect_match(html, session$ns(mid("rt_star_1")), fixed = TRUE)
    expect_match(html, 'class="sf-rank-list"', fixed = TRUE)
    expect_match(html, session$ns(mid("pw__price__vs__quality")), fixed = TRUE)
    expect_match(html, session$ns(mid("cw__price")), fixed = TRUE)
  })
})

test_that("A7: a survey of required advanced items can be completed", {
  shiny::testServer(survey_module_server,
                    args = list(instrument = advanced_instrument()), {
    session$setInputs(sf_start = 1)
    inputs <- list(3, 2, 4, "3|1|2", "5", 60, 40)
    names(inputs) <- mid(c("mx__1", "mx__2", "rt", "rk",
                           "pw__price__vs__quality", "cw__price", "cw__quality"))
    do.call(session$setInputs, inputs)
    session$setInputs(sf_next = 1)

    res <- session$returned()
    expect_false(is.null(res))
    expect_identical(res$mx__price, "3")
    expect_identical(res$rt, "4")
    expect_identical(c(res$rk__1, res$rk__2, res$rk__3), c("2", "3", "1"))
    expect_identical(res$pw__price__vs__quality, "5")
    expect_identical(res$cw__quality, "40")
  })
})

test_that("A7: an unanswered required advanced item still blocks submission", {
  shiny::testServer(survey_module_server,
                    args = list(instrument = advanced_instrument()), {
    session$setInputs(sf_start = 1)
    session$setInputs(sf_next = 1)
    expect_null(session$returned())
  })
})

basic_instrument <- function() {
  sf_instrument(title = "Basic", components = list(
    sf_choices("yn", values = c(0, 1), labels = c("No", "Yes")),
    sf_choices("fruit", values = c("a", "b", "c"),
               labels = c("Apple", "Banana", "Cherry")),
    sf_item("gate", "Do you eat fruit?", "single_choice", choice_set = "yn"),
    sf_item("mc", "Which fruit?", "multiple_choice", choice_set = "fruit"),
    sf_item("t1", "Comment", "text"),
    sf_item("dob", "Date of birth", "date"),
    sf_item("sl", "Satisfaction", "slider", slider_min = 0, slider_max = 10),
    sf_branch("mc", depends_on = "gate", operator = "==", value = "1",
              action = "show")
  ))
}

test_that("F5: an answer to an item branching has hidden is not submitted", {
  shiny::testServer(survey_module_server,
                    args = list(instrument = basic_instrument()), {
    session$setInputs(sf_start = 1)
    inputs <- list("1", c("a", "b"))
    names(inputs) <- mid(c("gate", "mc"))
    do.call(session$setInputs, inputs)
    hide <- list("0"); names(hide) <- mid("gate")
    do.call(session$setInputs, hide)
    session$setInputs(sf_next = 1)

    res <- session$returned()
    expect_identical(res$gate, "0")
    expect_true(all(is.na(c(res$mc__a, res$mc__b, res$mc__c))))
  })
})

test_that("F5: a cleared answer is not submitted", {
  shiny::testServer(survey_module_server,
                    args = list(instrument = basic_instrument()), {
    session$setInputs(sf_start = 1)
    set <- list("1", c("a", "b"), "first thoughts")
    names(set) <- mid(c("gate", "mc", "t1"))
    do.call(session$setInputs, set)
    cleared <- list(NULL, "")
    names(cleared) <- mid(c("mc", "t1"))
    do.call(session$setInputs, cleared)
    session$setInputs(sf_next = 1)

    res <- session$returned()
    expect_identical(c(res$mc__a, res$mc__b, res$mc__c), c("0", "0", "0"))
    expect_true(is.na(res$t1) || identical(res$t1, ""))
  })
})

two_page_instrument <- function() {
  sf_instrument(title = "Two pages", components = list(
    sf_choices("fruit", values = c("a", "b", "c"),
               labels = c("Apple", "Banana", "Cherry")),
    sf_item("gate", "Anything?", "text", page = 1),
    sf_item("mc", "Which fruit?", "multiple_choice", choice_set = "fruit",
            page = 1),
    sf_item("t2", "Anything else?", "text", page = 2)
  ))
}

test_that("F6: every selected option is drawn checked again", {
  shiny::testServer(survey_module_server,
                    args = list(instrument = two_page_instrument()), {
    session$setInputs(sf_start = 1)
    set <- list("1", c("a", "c"))
    names(set) <- mid(c("gate", "mc"))
    do.call(session$setInputs, set)
    # leaving the page and coming back draws it again from the answers
    session$setInputs(sf_next = 1)
    session$setInputs(sf_back = 1)
    html <- as.character(output$survey_ui$html)
    checked <- regmatches(html, gregexpr(
      sprintf('<input[^>]*name="%s"[^>]*value="[^"]*"[^>]*checked', session$ns(mid("mc"))),
      html))[[1]]
    expect_length(checked, 2)
  })
})

test_that("F7: a failed save does not mark the survey complete", {
  shiny::testServer(survey_module_server,
                    args = list(instrument = basic_instrument(),
                                on_submit = function(r) stop("database down")), {
    session$setInputs(sf_start = 1)
    set <- list("0"); names(set) <- mid("gate")
    do.call(session$setInputs, set)
    session$setInputs(sf_next = 1)

    expect_null(session$returned())
    expect_false(grepl("Thank You", as.character(output$survey_ui$html), fixed = TRUE))
  })
})

test_that("F4: a new instrument resets the survey and carries no answers over", {
  current <- shiny::reactiveVal(basic_instrument())
  second <- sf_instrument(title = "Second", components = list(
    sf_item("t1", "Required comment", "text", required = TRUE)
  ))
  shiny::testServer(survey_module_server,
                    args = list(instrument = current), {
    session$setInputs(sf_start = 1)
    set <- list("carried?"); names(set) <- mid("t1")
    do.call(session$setInputs, set)

    current(second)
    session$flushReact()
    expect_match(as.character(output$survey_ui$html), "Start Survey", fixed = TRUE)

    session$setInputs(sf_start = 2)
    session$setInputs(sf_next = 2)
    # t1 was answered under the first instrument only, so it is unanswered
    expect_null(session$returned())
  })
})

test_that("A6: in the module, date and slider start unanswered", {
  shiny::testServer(survey_module_server,
                    args = list(instrument = basic_instrument()), {
    session$setInputs(sf_start = 1)
    html <- as.character(output$survey_ui$html)
    expect_match(html, 'data-initial-date=""', fixed = TRUE)

    set <- list("0", 5); names(set) <- mid(c("gate", "sl"))
    do.call(session$setInputs, set)
    session$setInputs(sf_next = 1)
    res <- session$returned()
    expect_true(is.na(res$sl))
    expect_true(is.na(res$dob))
  })
})

test_that("F15: the module scrolls itself into view, not the host window", {
  html <- as.character(survey_module_ui("demo"))
  expect_false(grepl("window.scrollTo", html, fixed = TRUE))
  expect_match(html, "scrollIntoView", fixed = TRUE)
})
