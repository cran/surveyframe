## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(surveyframe)

## ----load---------------------------------------------------------------------
demo <- sframe_demo_data()
instr <- demo$instrument
responses <- demo$responses

## ----scales-------------------------------------------------------------------
lapply(instr$scales, function(scale) {
  list(
    id = scale$id,
    label = scale$label,
    items = scale$items,
    method = scale$method,
    min_valid = scale$min_valid,
    reverse_items = scale$reverse_items
  )
})

## ----scoring------------------------------------------------------------------
scored <- score_scales(
  responses,
  instr,
  keep_items = TRUE,
  keep_meta = TRUE
)

scale_ids <- vapply(instr$scales, function(scale) scale$id, character(1))
head(scored[, intersect(scale_ids, names(scored)), drop = FALSE])

## ----reliability, eval = requireNamespace("psych", quietly = TRUE)------------
alpha_only <- reliability_report(
  responses,
  instr,
  omega = FALSE
)

alpha_only

## ----omega, eval = requireNamespace("psych", quietly = TRUE)------------------
omega_report <- reliability_report(
  responses,
  instr,
  alpha = FALSE,
  omega = TRUE
)

omega_report

## ----item-report--------------------------------------------------------------
items <- item_report(responses, instr)

names(items)
items[[1]]

## ----efa, eval = requireNamespace("psych", quietly = TRUE)--------------------
efa_report(responses, instr)

## ----validity-----------------------------------------------------------------
loadings <- list(
  digital_marketing = c(dm_1 = .72, dm_2 = .78, dm_3 = .81),
  service_quality = c(sq_1 = .75, sq_2 = .80, sq_3 = .77),
  satisfaction = c(sat_1 = .76, sat_2 = .82)
)

validity <- validity_report(loadings)

validity

