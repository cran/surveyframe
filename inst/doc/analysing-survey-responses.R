## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(surveyframe)

## ----load---------------------------------------------------------------------
demo      <- sframe_demo_data()
instr     <- demo$instrument
responses <- demo$responses

dim(responses)

## ----import-------------------------------------------------------------------
responses <- read_responses(
  demo$responses_path,
  instr,
  respondent_id = "respondent_id",
  submitted_at  = "submitted_at",
  meta_cols     = "started_at",
  strict        = TRUE
)

dim(responses)

## ----screening----------------------------------------------------------------
missing_data_report(responses, instr)

quality_report(
  responses, instr,
  respondent_id = "respondent_id",
  submitted_at  = "submitted_at",
  started_at    = "started_at"
)

## ----score--------------------------------------------------------------------
scored    <- score_scales(responses, instr, keep_items = TRUE, keep_meta = TRUE)
scale_ids <- vapply(instr$scales, function(x) x$id, character(1))

head(scored[, intersect(scale_ids, names(scored)), drop = FALSE])

## ----assumptions--------------------------------------------------------------
assumption_report(
  scored,
  predictors = c("digital_marketing", "service_quality", "sustainability"),
  outcome    = "satisfaction"
)

## ----plan---------------------------------------------------------------------
instr$analysis_plan <- list(
  list(id = "RQ1",
       research_question = "Is digital marketing perception associated with satisfaction?",
       family = "association", method = "correlation_pearson",
       roles = list(x = "digital_marketing", y = "satisfaction"),
       options = list(alpha = 0.05)),
  list(id = "RQ2",
       research_question = "Do the three perception scales predict satisfaction?",
       family = "regression", method = "regression_linear",
       roles = list(predictors = c("digital_marketing", "service_quality", "sustainability"),
                    dependent = "satisfaction"),
       options = list(alpha = 0.05)),
  list(id = "RQ3",
       research_question = "Do first-time and repeat visitors differ in behavioural intention?",
       family = "group_comparison", method = "mann_whitney",
       roles = list(group = "visit_type", outcome = "behavioural_intention"),
       options = list(alpha = 0.05))
)

## ----run----------------------------------------------------------------------
results <- run_analysis_plan(responses, instr)
results

## ----single-result------------------------------------------------------------
rq1 <- results[[1]]

rq1$apa
rq1$effect_label
rq1$prompt
unlist(rq1$citations)

## ----render, eval = FALSE-----------------------------------------------------
# render_results(results, instr, output_file = "results.html", citation_format = "apa")

## ----gui, eval = FALSE--------------------------------------------------------
# launch_studio(
#   instrument     = instr,
#   responses      = responses,
#   screen         = "analysis",
#   launch.browser = FALSE
# )

