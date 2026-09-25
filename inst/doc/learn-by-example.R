## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.path = "figures/")
library(surveyframe)

## ----index--------------------------------------------------------------------
head(sframe_demos()[, c("name", "teaches")], 5)

## ----five-minute-start, eval = FALSE------------------------------------------
# demo <- sframe_demo("first_survey")
# results <- run_analysis_plan(demo$responses, demo$instrument)
# sframe_demo_qmd("first_survey") # write an editable Quarto notebook

## ----first-survey-------------------------------------------------------------
demo <- sframe_demo("first_survey")
demo$instrument

## ----first-survey-shot, echo = FALSE, out.width = "100%"----------------------
knitr::include_graphics("figures/survey-first-survey.png")

## ----first-survey-plan--------------------------------------------------------
do.call(rbind, lapply(sf_plan(demo$instrument), function(b) {
  data.frame(id = b$id, question = b$research_question, method = b$method)
}))

## ----first-survey-run---------------------------------------------------------
results <- run_analysis_plan(demo$responses, demo$instrument)
results[[1]]$apa

## ----two-group-shot, echo = FALSE, out.width = "100%"-------------------------
knitr::include_graphics("figures/survey-two-group.png")

## ----two-group----------------------------------------------------------------
tg <- sframe_demo("two_group")
res <- run_analysis_plan(tg$responses, tg$instrument)
for (b in res) cat(b$test, ": ", b$apa, "\n", sep = "")

## ----paired-shot, echo = FALSE, out.width = "100%"----------------------------
knitr::include_graphics("figures/survey-paired.png")

## ----paired-------------------------------------------------------------------
pr <- sframe_demo("paired")
for (b in run_analysis_plan(pr$responses, pr$instrument)) {
  cat(b$test, ": ", b$apa, "\n", sep = "")
}

## ----multi-group-shot, echo = FALSE, out.width = "100%"-----------------------
knitr::include_graphics("figures/survey-multi-group.png")

## ----multi-group--------------------------------------------------------------
mg <- sframe_demo("multi_group")
for (b in run_analysis_plan(mg$responses, mg$instrument)[1:2]) {
  cat(b$test, ": ", b$apa, "\n", sep = "")
}

## ----likert-shot, echo = FALSE, out.width = "100%"----------------------------
knitr::include_graphics("figures/survey-likert-scale.png")

## ----likert-------------------------------------------------------------------
ls_demo <- sframe_demo("likert_scale")
rel <- reliability_report(ls_demo$responses, ls_demo$instrument)
as.data.frame(rel)[, c("scale_id", "n_items", "alpha")]

## ----sem----------------------------------------------------------------------
sem <- sframe_demo("sem_pls")
vapply(sf_models(sem$instrument), function(m) m$type, character(1))

## ----matrix-shot, echo = FALSE, out.width = "100%"----------------------------
knitr::include_graphics("figures/survey-matrix-likert.png")

## ----matrix-------------------------------------------------------------------
ml <- sframe_demo("matrix_likert")
grep("^session__", names(ml$responses), value = TRUE)

## ----branching-shot, echo = FALSE, out.width = "100%"-------------------------
knitr::include_graphics("figures/survey-branching.png")

## ----branching----------------------------------------------------------------
br <- sframe_demo("branching")
sf_branches(br$instrument)[[1]]

## ----open-text----------------------------------------------------------------
ot <- sframe_demo("open_text")
tf <- run_analysis_plan(ot$responses, ot$instrument)[[1]]
head(tf$table, 5)

## ----branding-----------------------------------------------------------------
str(sframe_demo_branding(), max.level = 1)

## ----branded, eval = FALSE----------------------------------------------------
# sframe_demo("two_group", branded = TRUE)

## ----plain-shot, echo = FALSE, out.width = "100%"-----------------------------
knitr::include_graphics("figures/presentation-plain.png")

## ----welcome-shot, echo = FALSE, out.width = "100%"---------------------------
knitr::include_graphics("figures/presentation-welcome.png")

## ----branded-shot, echo = FALSE, out.width = "100%"---------------------------
knitr::include_graphics("figures/presentation-branded.png")

## ----conversational-shot, echo = FALSE, out.width = "100%"--------------------
knitr::include_graphics("figures/presentation-conversational.png")

## ----conv-branch-shot, echo = FALSE, out.width = "100%"-----------------------
knitr::include_graphics("figures/presentation-conversational-branching.png")

## ----routes, eval = FALSE-----------------------------------------------------
# # 1. One self-contained HTML file you can host or email
# export_static_survey(demo$instrument, "survey.html")
# 
# # 2. A Shiny app
# render_survey(demo$instrument, mode = "shiny")
# 
# # 3. A Google Sheets collector, generated as an Apps Script
# export_google_sheet(demo$instrument, sheet_url = "https://...")

## ----report, eval = FALSE-----------------------------------------------------
# render_report(demo$instrument, demo$responses, output_path = "report.html")

## ----amend--------------------------------------------------------------------
rev <- sframe_demo("instrument_revision")
log <- as.data.frame(amendment_log(rev$instrument))
log[, c("reason_code", "tier", "reason_text")]

## ----verify, error = TRUE-----------------------------------------------------
try({
v <- sframe_demo("verification")
tampered <- file.path(dirname(v$instrument_path), "verification_tampered.sframe")
read_sframe(tampered)
})

## ----artefacts----------------------------------------------------------------
d <- sframe_demo("two_group")
basename(unlist(d[c("instrument_path", "responses_path",
                    "codebook_path", "results_path")]))

## ----labelled, eval = FALSE---------------------------------------------------
# sframe_export_labelled(d$responses, d$instrument, "two_group.sav")

## ----qmd, eval = FALSE--------------------------------------------------------
# sframe_demo_qmd("two_group")

