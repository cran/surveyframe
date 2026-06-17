## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(surveyframe)

## ----collector, eval = FALSE--------------------------------------------------
# study <- read_sframe("my_study.sframe")
# 
# export_google_sheet(
#   study,
#   sheet_url  = "https://docs.google.com/spreadsheets/d/YOUR_SHEET_ID",
#   output_dir = "."
# )

## ----endpoint-export, eval = FALSE--------------------------------------------
# study$render$google_sheets_endpoint <-
#   "https://script.google.com/macros/s/YOUR_SCRIPT_ID/exec"
# 
# export_static_survey(
#   study,
#   output_path = "index.html",
#   open        = FALSE
# )

## ----read, eval = FALSE-------------------------------------------------------
# responses <- read_sheet_responses(
#   sheet_id   = "https://docs.google.com/spreadsheets/d/YOUR_SHEET_ID",
#   instrument = study
# )
# 
# qr <- quality_report(responses, study, respondent_id = "respondent_id")

