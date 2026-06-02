## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(surveyframe)

## ----item-ids-----------------------------------------------------------------
dmre <- paste0("dmre_", 1:4); dmau <- paste0("dmau_", 1:3)
dmeu <- paste0("dmeu_", 1:3); dmpv <- paste0("dmpv_", 1:4)
dsqa <- paste0("dsqa_", 1:3); dsqt <- paste0("dsqt_", 1:5)
dsuq <- paste0("dsuq_", 1:5); ts <- paste0("ts_", 1:3); bi <- paste0("bi_", 1:3)

## ----efa-syntax---------------------------------------------------------------
cat(efa_syntax(items = c(dmre, dmau, dmeu, dmpv), nfactors = 4,
               extraction = "minres", rotation = "oblimin"))

## ----cfa----------------------------------------------------------------------
construct_def <- list(
  DMRE = dmre, DMAU = dmau, DMEU = dmeu, DMPV = dmpv,
  DSQA = dsqa, DSQT = dsqt, DSUQ = dsuq, TS = ts, BI = bi
)

cfa_model <- sf_model(
  id    = "lower_order_cfa",
  label = "Lower-order measurement model",
  type  = "cfa",
  constructs = Map(function(id, items) sf_construct(id, id, items),
                   names(construct_def), construct_def)
)

cat(cfa_lavaan_syntax(model = cfa_model))

## ----cbsem--------------------------------------------------------------------
sem_model <- sf_model(
  id    = "tourism_structural",
  label = "Digital marketing structural model",
  type  = "cb_sem",
  constructs = list(
    sf_construct("DME",  "Digital marketing effectiveness", c(dmre, dmau, dmeu, dmpv)),
    sf_construct("DSQ",  "Destination service quality",     c(dsqa, dsqt)),
    sf_construct("DSUQ", "Destination sustainability quality", dsuq),
    sf_construct("TS",   "Tourist satisfaction", ts),
    sf_construct("BI",   "Behavioural intention", bi)
  ),
  paths = list(
    sf_path("DME", "DSQ", label = "h1"),  sf_path("DME", "DSUQ", label = "h2"),
    sf_path("DME", "TS",  label = "h3"),  sf_path("DME", "BI",   label = "h4"),
    sf_path("DSQ", "TS",  label = "h5"),  sf_path("DSQ", "BI",   label = "h6"),
    sf_path("DSUQ", "TS", label = "h7"),  sf_path("DSUQ", "BI",  label = "h8"),
    sf_path("TS", "BI",   label = "h9")
  ),
  indirect = list(
    sf_indirect("DME", "DSQ", "TS",  label = "h10"),
    sf_indirect("DME", "DSUQ", "TS", label = "h11"),
    sf_indirect("DME", c("DSQ", "TS"), "BI",  label = "h12"),
    sf_indirect("DME", c("DSUQ", "TS"), "BI", label = "h13")
  ),
  options = list(estimator = "MLR", missing = "fiml", standardised = TRUE)
)

validate_model(sem_model)
cat(sem_lavaan_syntax(sem_model))

## ----pls----------------------------------------------------------------------
pls_model <- sf_model(
  id    = "tourism_pls",
  label = "Two-stage higher-order PLS model",
  type  = "pls_sem",
  constructs = list(
    sf_construct("DME",  "Digital marketing effectiveness",
                 c("DMRE", "DMAU", "DMEU", "DMPV"), mode = "composite"),
    sf_construct("DSQ",  "Destination service quality",
                 c("DSQA", "DSQT"), mode = "composite"),
    sf_construct("DSUQ", "Destination sustainability quality", dsuq, mode = "composite"),
    sf_construct("TS",   "Tourist satisfaction", ts, mode = "composite"),
    sf_construct("BI",   "Behavioural intention", bi, mode = "composite")
  ),
  paths = list(
    sf_path("DME", "DSQ"), sf_path("DME", "DSUQ"),
    sf_path("DME", "TS"),  sf_path("DME", "BI"),
    sf_path("DSQ", "TS"),  sf_path("DSQ", "BI"),
    sf_path("DSUQ", "TS"), sf_path("DSUQ", "BI"),
    sf_path("TS", "BI")
  ),
  options = list(bootstrap = 1000)
)

cat(seminr_syntax(pls_model))

## ----json---------------------------------------------------------------------
cat(model_report_template(sem_model, include_json = FALSE))

