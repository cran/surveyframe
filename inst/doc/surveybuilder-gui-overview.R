## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(surveyframe)

## ----input-types--------------------------------------------------------------
demo      <- sframe_input_types_demo_data()
instr     <- demo$instrument
responses <- demo$responses

table(vapply(instr$items, function(x) x$type, character(1)))
dim(responses)

## ----gui-demo, eval = FALSE---------------------------------------------------
# launch_builder_demo()
# launch_studio_demo()
# launch_dashboard_demo()

## ----builder-path, eval = FALSE-----------------------------------------------
# builder_file <- launch_builder(open = FALSE)
# builder_file

## ----studio, eval = FALSE-----------------------------------------------------
# launch_studio(
#   instrument     = instr,
#   responses      = responses,
#   screen         = "analysis",
#   launch.browser = FALSE
# )

## ----dashboard, eval = FALSE--------------------------------------------------
# launch_dashboard(
#   instrument     = instr,
#   responses      = responses,
#   launch.browser = FALSE
# )

