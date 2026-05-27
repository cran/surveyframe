## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(surveyframe)

## ----load---------------------------------------------------------------------
demo <- sframe_demo_data()
instr <- demo$instrument
responses <- demo$responses

dim(responses)

## ----expected-----------------------------------------------------------------
item_ids <- vapply(instr$items, function(x) x$id, character(1))
head(item_ids)

names(responses)[1:8]

## ----import-------------------------------------------------------------------
strict_responses <- read_responses(
  demo$responses_path,
  instr,
  respondent_id = "respondent_id",
  submitted_at = "submitted_at",
  meta_cols = "started_at",
  strict = TRUE
)

relaxed_responses <- read_responses(
  demo$responses_path,
  instr,
  respondent_id = "respondent_id",
  submitted_at = "submitted_at",
  meta_cols = "started_at",
  strict = FALSE
)

identical(names(strict_responses), names(relaxed_responses))

## ----missing------------------------------------------------------------------
missing <- missing_data_report(responses, instr)

missing$item_missing
head(missing$respondent_missing)

## ----quality------------------------------------------------------------------
quality <- quality_report(
  responses,
  instr,
  respondent_id = "respondent_id",
  submitted_at = "submitted_at",
  started_at = "started_at"
)

quality

## ----score--------------------------------------------------------------------
scored <- score_scales(responses, instr, keep_items = TRUE, keep_meta = TRUE)

scale_ids <- vapply(instr$scales, function(x) x$id, character(1))
head(scored[, intersect(scale_ids, names(scored)), drop = FALSE])

## ----descriptives-------------------------------------------------------------
descriptives_report(
  scored,
  variables = intersect(scale_ids, names(scored))
)

## ----analysis-plan------------------------------------------------------------
instr$analysis_plan <- list(
  list(
    id = "RQ1",
    research_question = "Is digital marketing associated with satisfaction?",
    family = "association",
    method = "correlation_spearman",
    roles = list(x = "digital_marketing", y = "satisfaction"),
    options = list(alpha = 0.05),
    status = "valid_plan",
    requires_data = TRUE
  ),
  list(
    id = "RQ2",
    research_question = "Do visit types differ in satisfaction?",
    family = "group_comparison",
    method = "mann_whitney",
    roles = list(group = "visit_type", outcome = "satisfaction"),
    options = list(alpha = 0.05),
    status = "valid_plan",
    requires_data = TRUE
  )
)

results <- run_analysis_plan(responses, instr)
results

## ----gui, eval = FALSE--------------------------------------------------------
# launch_studio(
#   instrument = instr,
#   responses = responses,
#   screen = "analysis",
#   launch.browser = FALSE
# )

