## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(surveyframe)

## ----reliability, eval = requireNamespace("psych", quietly = TRUE)------------
demo      <- sframe_demo_data()
instr     <- demo$instrument
responses <- demo$responses

reliability_report(responses, instr, omega = TRUE)

## ----item-report--------------------------------------------------------------
demo      <- sframe_demo_data()
instr     <- demo$instrument
responses <- demo$responses

items <- item_report(responses, instr)
names(items)
items[[1]]

## ----efa, eval = requireNamespace("psych", quietly = TRUE)--------------------
efa_report(responses, instr)

## ----validity-----------------------------------------------------------------
published_loadings <- list(
  DMRE = c(dmre_1 = .815, dmre_2 = .899, dmre_3 = .668, dmre_4 = .838),
  DMAU = c(dmau_1 = .843, dmau_2 = .915, dmau_3 = .920),
  DMEU = c(dmeu_1 = .897, dmeu_2 = .929, dmeu_3 = .932),
  DMPV = c(dmpv_1 = .818, dmpv_2 = .916, dmpv_3 = .900, dmpv_4 = .863),
  DSQA = c(dsqa_1 = .904, dsqa_2 = .920, dsqa_3 = .869),
  DSQT = c(dsqt_1 = .778, dsqt_2 = .883, dsqt_3 = .879, dsqt_4 = .811, dsqt_5 = .713),
  DSUQ = c(dsuq_1 = .780, dsuq_2 = .855, dsuq_3 = .845, dsuq_4 = .551, dsuq_5 = .529),
  TS   = c(ts_1 = .912, ts_2 = .950, ts_3 = .869),
  BI   = c(bi_1 = .937, bi_2 = .911, bi_3 = .940)
)

validity <- validity_report(published_loadings)
validity$reliability

## ----validity-scores, eval = FALSE--------------------------------------------
# validity_report(published_loadings, construct_scores = scored_constructs)

