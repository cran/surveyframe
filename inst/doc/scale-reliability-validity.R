## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(surveyframe)
library(knitr)

## ----reliability, eval = requireNamespace("psych", quietly = TRUE)------------
demo      <- sframe_demo_data()
instr     <- demo$instrument
responses <- demo$responses

rr <- reliability_report(responses, instr, omega = TRUE)
# as.data.frame() gives one row per scale, so no reaching into the object.
rr_df <- as.data.frame(rr)
rel_df <- data.frame(
  Scale   = paste0(rr_df$label, " (", rr_df$scale_id, ")"),
  Items   = rr_df$n_items,
  N       = rr_df$n,
  Alpha   = ifelse(is.na(rr_df$alpha),   "n/a", sprintf("%.2f", rr_df$alpha)),
  Omega_h = ifelse(is.na(rr_df$omega_h), "n/a", sprintf("%.2f", rr_df$omega_h)),
  Omega_t = ifelse(is.na(rr_df$omega_t), "n/a", sprintf("%.2f", rr_df$omega_t)),
  stringsAsFactors = FALSE)
kable(rel_df, row.names = FALSE,
      col.names = c("Scale", "Items", "N", "Alpha", "Omega h", "Omega total"),
      align = c("l", "c", "c", "r", "r", "r"),
      caption = "Scale reliability statistics")

## ----item-report--------------------------------------------------------------
demo      <- sframe_demo_data()
instr     <- demo$instrument
responses <- demo$responses

items <- item_report(responses, instr)
first  <- items[[1]]
kable(first$diagnostics, digits = 2,
      caption = paste0("Item diagnostics: ", first$label, " (", first$scale_id, ")"))

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
kable(as.data.frame(validity), digits = 2,
      caption = "Composite reliability and average variance extracted")

## ----validity-scores----------------------------------------------------------
demo <- sframe_demo_data()
scored <- score_scales(demo$responses, demo$instrument)
scale_ids <- vapply(sf_scales(demo$instrument), sf_id, character(1))
construct_scores <- scored[, scale_ids, drop = FALSE]

loadings <- lapply(sf_scales(demo$instrument), function(s) {
  stats::setNames(rep(0.8, length(s$items)), s$items)
})
names(loadings) <- scale_ids

from_scores <- validity_report(loadings, construct_scores = construct_scores)
from_scores$htmt_method

## ----validity-htmt------------------------------------------------------------
items_by_construct <- lapply(sf_scales(demo$instrument), function(s) {
  scored[, s$items, drop = FALSE]
})
names(items_by_construct) <- scale_ids

real_htmt <- validity_report(loadings, construct_scores = construct_scores,
                             items_by_construct = items_by_construct)
real_htmt$htmt_method

