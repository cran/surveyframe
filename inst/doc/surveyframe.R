## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(surveyframe)

## ----load-demo----------------------------------------------------------------
demo <- sframe_demo_data()
instr <- demo$instrument
responses <- demo$responses

instr

## ----inspect------------------------------------------------------------------
names(instr)

length(instr$items)
length(instr$scales)

vapply(instr$items[1:5], function(x) x$id, character(1))
vapply(instr$items[1:5], function(x) x$type, character(1))

## ----validate-----------------------------------------------------------------
validation <- validate_sframe(instr, strict = FALSE)

validation$valid
length(validation$problems)

## ----validate-strict, eval = FALSE--------------------------------------------
# instr <- validate_sframe(instr)

## ----read-responses-----------------------------------------------------------
responses <- read_responses(
  x = demo$responses_path,
  instrument = instr,
  respondent_id = "respondent_id",
  submitted_at = "submitted_at",
  meta_cols = "started_at"
)

dim(responses)
head(responses[, 1:6])

## ----missing------------------------------------------------------------------
missing <- missing_data_report(responses, instr)

missing

## ----quality------------------------------------------------------------------
quality <- quality_report(
  data = responses,
  instrument = instr,
  respondent_id = "respondent_id",
  submitted_at = "submitted_at",
  started_at = "started_at"
)

quality

## ----score--------------------------------------------------------------------
scored <- score_scales(
  data = responses,
  instrument = instr,
  keep_items = TRUE,
  keep_meta = TRUE
)

scale_names <- vapply(instr$scales, function(x) x$id, character(1))
scale_names

head(scored[, intersect(scale_names, names(scored)), drop = FALSE])

## ----descriptives-------------------------------------------------------------
descriptives_report(
  scored,
  variables = intersect(scale_names, names(scored))
)

## ----reliability, eval = requireNamespace("psych", quietly = TRUE)------------
reliability <- reliability_report(
  data = responses,
  instrument = instr,
  omega = FALSE
)

reliability

## ----item-report--------------------------------------------------------------
items <- item_report(responses, instr)

names(items)
items[[1]]

## ----validity-----------------------------------------------------------------
example_loadings <- list(
  digital_marketing = c(dm_1 = .72, dm_2 = .78, dm_3 = .81),
  service_quality = c(sq_1 = .75, sq_2 = .80, sq_3 = .77)
)

validity_report(example_loadings)

## ----efa, eval = requireNamespace("psych", quietly = TRUE)--------------------
efa <- efa_report(responses, instr)

efa

## ----cfa-syntax---------------------------------------------------------------
cat(cfa_syntax(instr))

## ----cfa-lavaan---------------------------------------------------------------
cat(cfa_lavaan_syntax(instr, ordered = TRUE))

## ----sem-model----------------------------------------------------------------
model <- sf_model(
  id = "demo_model",
  label = "Demo structural model",
  type = "cb_sem",
  constructs = list(
    sf_construct("DM", "Digital marketing", c("dm_1", "dm_2", "dm_3")),
    sf_construct("SQ", "Service quality", c("sq_1", "sq_2", "sq_3"))
  ),
  paths = list(
    sf_path("DM", "SQ")
  )
)

model_json(model)

## ----sem-syntax---------------------------------------------------------------
cat(sem_lavaan_syntax(model, instr))

## ----pls-syntax---------------------------------------------------------------
pls_model <- sf_model(
  id = "demo_pls",
  label = "Demo PLS model",
  type = "pls_sem",
  constructs = list(
    sf_construct("DM", "Digital marketing", c("dm_1", "dm_2", "dm_3"), mode = "composite"),
    sf_construct("SQ", "Service quality", c("sq_1", "sq_2", "sq_3"), mode = "composite")
  ),
  paths = list(
    sf_path("DM", "SQ")
  )
)

cat(seminr_syntax(pls_model))

## ----codebook-----------------------------------------------------------------
codebook <- codebook_report(instr)

codebook

## ----render-report, eval = FALSE----------------------------------------------
# render_report(
#   instrument = instr,
#   data = responses,
#   output_file = "surveyframe-demo-report.html",
#   include_codebook = TRUE,
#   include_quality = TRUE,
#   include_missing = TRUE,
#   include_descriptives = TRUE,
#   include_reliability = TRUE,
#   include_models = TRUE
# )

## ----gui-path, eval = FALSE---------------------------------------------------
# launch_builder()
# launch_studio()
# launch_dashboard()

## ----gui-demo, eval = FALSE---------------------------------------------------
# launch_builder_demo()
# launch_studio_demo()
# launch_dashboard_demo()

## ----compact-workflow, eval = FALSE-------------------------------------------
# library(surveyframe)
# 
# instr <- read_sframe("my_survey.sframe")
# instr <- validate_sframe(instr)
# 
# responses <- read_responses(
#   "responses.csv",
#   instr,
#   respondent_id = "respondent_id",
#   submitted_at = "submitted_at",
#   meta_cols = "started_at"
# )
# 
# quality <- quality_report(responses, instr, respondent_id = "respondent_id")
# scored <- score_scales(responses, instr)
# reliability <- reliability_report(responses, instr, omega = FALSE)
# codebook <- codebook_report(instr)
# 
# render_report(
#   instrument = instr,
#   data = responses,
#   output_file = "survey-report.html",
#   include_codebook = TRUE,
#   include_quality = TRUE,
#   include_missing = TRUE,
#   include_descriptives = TRUE,
#   include_reliability = TRUE
# )

## ----citation, eval = FALSE---------------------------------------------------
# citation("surveyframe")

