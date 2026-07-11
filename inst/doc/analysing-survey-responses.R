## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(surveyframe)
library(knitr)

# Tabulate analysis-plan results the same way the report template does.
results_table <- function(results) {
  g <- function(r, f) { v <- r[[f]]; if (is.null(v) || !length(v)) "" else as.character(v)[1] }
  df <- data.frame(
    RQ       = vapply(results, g, "", "block_id"),
    Question = vapply(results, g, "", "research_question"),
    Method   = vapply(results, g, "", "method"),
    Result   = vapply(results, g, "", "apa"),
    Effect   = vapply(results, g, "", "effect_label"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  kable(df, row.names = FALSE,
        col.names = c("RQ", "Research question", "Method", "Result (APA)", "Effect"),
        align = c("l", "l", "l", "r", "l"))
}

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
mr <- missing_data_report(responses, instr)
kable(mr$item_missing, digits = 2,
      col.names = c("Variable", "Missing (n)", "Missing (%)", "Valid (n)"),
      caption = "Item-level missingness")

qr <- quality_report(
  responses, instr,
  respondent_id = "respondent_id",
  submitted_at  = "submitted_at",
  started_at    = "started_at"
)
quality_summary <- data.frame(
  Metric = c("Respondents", "Items", "Flagged for review", "Flag rate"),
  Value  = c(qr$summary$n_respondents, qr$summary$n_items, qr$summary$n_flagged,
             sprintf("%.1f%%", 100 * qr$summary$flag_rate)),
  stringsAsFactors = FALSE
)
kable(quality_summary, align = c("l", "r"), caption = "Quality screening summary")

## ----score--------------------------------------------------------------------
scored    <- score_scales(responses, instr, keep_items = TRUE, keep_meta = TRUE)
scale_ids <- vapply(instr$scales, function(x) x$id, character(1))
score_cols <- intersect(scale_ids, names(scored))

kable(head(scored[, score_cols, drop = FALSE]), digits = 2,
      caption = "Scale scores, first respondents")

## ----score-distributions, fig.width = 7, fig.height = 3, fig.align = "left"----
op <- par(mfrow = c(1, length(score_cols)), mar = c(4, 3, 2, 1))
for (s in score_cols) {
  v <- scored[[s]]; v <- v[is.finite(v)]
  hist(v, col = "#16B3B1", border = "white", main = s,
       xlab = "Score", ylab = "")
}
par(op)

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
results_table(results)

## ----run-plots, eval = requireNamespace("ggplot2", quietly = TRUE)------------
results_plots <- run_analysis_plan(responses, instr, plots = TRUE)
results_plots[[1]]$plot

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

