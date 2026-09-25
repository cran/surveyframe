## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(surveyframe)
library(knitr)
set.seed(2026)

has_ggplot <- requireNamespace("ggplot2", quietly = TRUE)
knitr::opts_chunk$set(fig.width = 7, fig.height = 4.2, dpi = 96)

## ----load---------------------------------------------------------------------
demo <- read_sframe(system.file("extdata", "hotel_supplier_mcdm.sframe",
                                package = "surveyframe"))
responses <- utils::read.csv(
  system.file("extdata", "hotel_supplier_mcdm_responses.csv",
              package = "surveyframe"),
  stringsAsFactors = FALSE
)

c(respondents = nrow(responses), criteria = 4, suppliers = 5)

## ----sources, echo = FALSE----------------------------------------------------
kable(
  data.frame(
    Source = c("Pairwise comparison", "Constant sum", "Rated matrix",
               "Researcher supplied"),
    Item = c("crit_pairs", "crit_points",
             "rate_service and 3 more", "declared in the plan block"),
    Provides = c("Criterion weights", "Criterion weights",
                 "Performance matrix", "Performance matrix"),
    How = c("Respondents judge each pair on the Saaty 1 to 9 scale",
            "Respondents divide 100 points across the criteria",
            "Respondents rate every supplier on every criterion",
            "Audited figures the researcher enters directly"),
    check.names = FALSE, stringsAsFactors = FALSE
  ),
  row.names = FALSE, align = c("l", "l", "l", "l"),
  caption = "The 4 declared input sources in the worked example."
)

## ----ahp----------------------------------------------------------------------
results <- run_analysis_plan(responses, demo, plots = has_ggplot)
ahp <- results[["RQ1"]]

kable(ahp$table, row.names = FALSE,
      caption = "Criterion weights derived from pairwise judgements.")

## ----consistency--------------------------------------------------------------
round(ahp$cr, 4)

## ----ahp-plot, fig.alt = "Bar chart of the four criterion weights derived from pairwise comparisons, with service carrying the largest weight and delivery the smallest.", eval = has_ggplot----
ahp$plot

## ----topsis-supplied----------------------------------------------------------
audited <- results[["RQ2"]]

kable(audited$table, row.names = FALSE,
      caption = paste("TOPSIS ranking on audited figures, weighted by",
                      "collected judgements."))

c(weights = audited$weights_source, matrix = audited$matrix_source)

## ----topsis-rated-------------------------------------------------------------
rated <- results[["RQ3"]]

kable(rated$table, row.names = FALSE,
      caption = paste("TOPSIS ranking on staff ratings, weighted by the",
                      "constant-sum question."))

## ----compare, echo = FALSE----------------------------------------------------
cmp <- merge(
  audited$table[, c("Alternative", "Rank")],
  rated$table[, c("Alternative", "Rank")],
  by = "Alternative", suffixes = c("_audited", "_rated")
)
cmp <- cmp[order(cmp$Rank_audited), ]
kable(cmp, row.names = FALSE,
      col.names = c("Supplier", "Rank on audited figures",
                    "Rank on staff ratings"),
      caption = "The same method, two declared input sources, two answers.")

## ----sensitivity--------------------------------------------------------------
sens <- sensitivity_analysis(
  x = matrix(c(4.1, 3.0, 210, 36,
               3.6, 4.5, 180, 48,
               4.8, 2.5, 260, 24,
               3.9, 4.0, 150, 72,
               4.4, 3.8, 230, 30),
             nrow = 5, byrow = TRUE),
  weights        = audited$weights,
  criteria_types = c("benefit", "benefit", "cost", "cost"),
  method         = "topsis",
  alternatives   = c("Alpha", "Basilica", "Coral", "Dhoni", "Equator"),
  criteria       = c("service", "location", "price", "delivery")
)

sens

## ----sensitivity-table--------------------------------------------------------
kable(as.data.frame(sens), row.names = FALSE,
      caption = "Ranking stability under a 5 percent change in each weight.")

## ----sensitivity-plot, fig.alt = "Bar chart of rank correlation for each criterion perturbed up and down, against a dashed reference line at one marking an unchanged ranking.", eval = has_ggplot----
plot(sens)

## ----dematel------------------------------------------------------------------
dematel <- results[["RQ4"]]

kable(dematel$table, row.names = FALSE,
      caption = "DEMATEL cause and effect classification.")

## ----dematel-plot, fig.alt = "Influence map plotting prominence against relation for the four criteria, separating causal drivers above the axis from affected criteria below it.", eval = has_ggplot----
dematel$plot

## ----summary, echo = FALSE----------------------------------------------------
# as.data.frame() flattens the results to one row per block, so the whole
# summary is a column selection, with no loop over internals.
results_df <- as.data.frame(results)
summary_df <- data.frame(
  RQ = results_df$block_id,
  Question = results_df$research_question,
  Method = toupper(results_df$test),
  Result = results_df$apa,
  check.names = FALSE, stringsAsFactors = FALSE
)
kable(summary_df, row.names = FALSE,
      col.names = c("RQ", "Research question", "Method", "Result"),
      caption = "The declared analysis plan and what each block returned.")

