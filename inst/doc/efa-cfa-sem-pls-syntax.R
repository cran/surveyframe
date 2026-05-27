## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(surveyframe)

## ----load---------------------------------------------------------------------
demo <- sframe_demo_data()
instr <- demo$instrument

## ----efa-syntax---------------------------------------------------------------
cat(efa_syntax(
  items = c("dm_1", "dm_2", "dm_3", "sq_1", "sq_2", "sq_3"),
  nfactors = 2,
  extraction = "minres",
  rotation = "oblimin"
))

## ----cfa----------------------------------------------------------------------
cat(cfa_lavaan_syntax(instr, ordered = TRUE))

## ----cfa-wrapper--------------------------------------------------------------
cat(cfa_syntax(instr))

## ----constructs---------------------------------------------------------------
dm <- sf_construct(
  id = "DM",
  label = "Digital marketing",
  items = c("dm_1", "dm_2", "dm_3"),
  mode = "reflective"
)

sq <- sf_construct(
  id = "SQ",
  label = "Service quality",
  items = c("sq_1", "sq_2", "sq_3"),
  mode = "reflective"
)

sat <- sf_construct(
  id = "SAT",
  label = "Satisfaction",
  items = c("sat_1", "sat_2"),
  mode = "reflective"
)

## ----cb-sem-------------------------------------------------------------------
sem_model <- sf_model(
  id = "tourism_sem",
  label = "Tourism structural model",
  type = "cb_sem",
  constructs = list(dm, sq, sat),
  paths = list(
    sf_path("DM", "SQ", label = "a"),
    sf_path("SQ", "SAT", label = "b"),
    sf_path("DM", "SAT", label = "c_prime")
  ),
  indirect = list(
    sf_indirect("DM", "SQ", "SAT", label = "indirect_DM_SQ_SAT")
  ),
  options = list(estimator = "MLR", missing = "fiml", standardised = TRUE)
)

validate_model(sem_model, instr)

cat(sem_lavaan_syntax(sem_model, instr))

## ----pls----------------------------------------------------------------------
pls_model <- sf_model(
  id = "tourism_pls",
  label = "Tourism PLS model",
  type = "pls_sem",
  constructs = list(
    sf_construct("DM", "Digital marketing", c("dm_1", "dm_2", "dm_3"), mode = "composite"),
    sf_construct("SQ", "Service quality", c("sq_1", "sq_2", "sq_3"), mode = "composite"),
    sf_construct("SAT", "Satisfaction", c("sat_1", "sat_2"), mode = "composite")
  ),
  paths = list(
    sf_path("DM", "SQ"),
    sf_path("SQ", "SAT"),
    sf_path("DM", "SAT")
  ),
  options = list(bootstrap = 5000)
)

cat(seminr_syntax(pls_model))

## ----model-json---------------------------------------------------------------
model_json(sem_model)

