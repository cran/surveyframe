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

## ----choices------------------------------------------------------------------
likert5 <- sf_choices(
  "likert5",
  values = 1:5,
  labels = c("Strongly disagree", "Disagree",
             "Neither agree nor disagree", "Agree", "Strongly agree")
)

## ----helpers------------------------------------------------------------------
stem_items <- function(ids, stem, completions, scale_id) {
  Map(
    function(id, comp)
      sf_item(id, paste(stem, comp),
              type = "likert", required = TRUE,
              choice_set = "likert5", scale_id = scale_id),
    ids, completions
  )
}

solo_items <- function(ids, labels, scale_id) {
  Map(
    function(id, lab)
      sf_item(id, lab,
              type = "likert", required = TRUE,
              choice_set = "likert5", scale_id = scale_id),
    ids, labels
  )
}

## ----dmre---------------------------------------------------------------------
dmre_stem <- paste(
  "The digital marketing contents I encountered during my",
  "trip planning and booking phases were"
)
dmre_completions <- c(
  "relevant to my interests.",
  "engaging.",
  "customisable.",
  "flexible and I was able to make real-time adjustments for my demand.",
  "able to cater to my specific needs and preferences."
)
dmre_items <- stem_items(
  paste0("dmre_", 1:5), dmre_stem, dmre_completions, "DMRE"
)

## ----dmau---------------------------------------------------------------------
dmau_stem <- "The contents, communication, and booking services were"
dmau_completions <- c(
  "easy to find so I could contact service providers and book my trip directly.",
  "fast.",
  "efficient.",
  "of good quality.",
  "user friendly."
)
dmau_items <- stem_items(
  paste0("dmau_", 1:5), dmau_stem, dmau_completions, "DMAU"
)

## ----dmeu---------------------------------------------------------------------
dmeu_stem <- paste(
  "The digital marketing contents and procedures regarding",
  "trip planning and booking were"
)
dmeu_completions <- c(
  "easy to learn.",
  "understandable and required little mental effort.",
  "neat and simple.",
  "easy to follow.",
  "mobile friendly."
)
dmeu_items <- stem_items(
  paste0("dmeu_", 1:5), dmeu_stem, dmeu_completions, "DMEU"
)

## ----dmpv---------------------------------------------------------------------
dmpv_stem <- paste(
  "In terms of value, both the commercial and",
  "user-generated contents are"
)
dmpv_completions <- c(
  "sufficient to support eco-friendly practices.",
  "appropriate.",
  "trustworthy and credible.",
  "consistent across all digital platforms.",
  "value for money."
)
dmpv_items <- stem_items(
  paste0("dmpv_", 1:5), dmpv_stem, dmpv_completions, "DMPV"
)

## ----dsqa---------------------------------------------------------------------
dsqa_stem <- "The accommodation related services met my expectations for"
dsqa_completions <- c(
  "check-in and check-out services.",
  "room cleanliness.",
  "staff attitude.",
  "safety and security."
)
dsqa_items <- stem_items(
  paste0("dsqa_", 1:4), dsqa_stem, dsqa_completions, "DSQA"
)

## ----dsqt---------------------------------------------------------------------
dsqt_stem <- "The local transport related services met my expectations for"
dsqt_completions <- c(
  "frequency.",
  "connectivity.",
  "comfort.",
  "staff attitude.",
  "ride safety."
)
dsqt_items <- stem_items(
  paste0("dsqt_", 1:5), dsqt_stem, dsqt_completions, "DSQT"
)

## ----dsuq---------------------------------------------------------------------
dsuq_labels <- c(
  "I can easily find and purchase locally-made handicrafts and souvenir products.",
  "The livelihoods of local vendors and artisans are respected, and fair prices are paid for their products.",
  "The size of food portions sold is adequate, reducing waste and leftovers.",
  "Awareness programmes are adequate in encouraging me to reduce water consumption.",
  "There are enough local guides with in-depth knowledge to enhance my travel experience.",
  "Sustainable transport options such as bikes, walking routes, and public transport are adequate.",
  "Reusable bags are adequately available for purchase.",
  "Digital infrastructure is adequate so that I can avoid printing and use digital copies.",
  "There are adequate choices of sustainable seafood in the destination.",
  "There are enough litter bins throughout the destination.",
  "There are adequate awareness signs about endangered marine species, plants, and animals."
)
dsuq_items <- solo_items(paste0("dsuq_", 1:11), dsuq_labels, "DSUQ")

## ----ts-bi--------------------------------------------------------------------
ts_items <- solo_items(
  paste0("ts_", 1:3),
  c(
    "The destination met or exceeded my expectations.",
    "Overall, my travel experience with the destination was good.",
    "Overall, I felt comfortable in the destination."
  ),
  "TS"
)

bi_items <- solo_items(
  paste0("bi_", 1:3),
  c(
    "I will recommend others to use online platforms for planning and booking their trips.",
    "I will share my experience online.",
    "I intend to revisit the destination."
  ),
  "BI"
)

## ----demographics-------------------------------------------------------------
gender_cs    <- sf_choices("gender",
  c("male", "female", "transgender"), c("Male", "Female", "Transgender"))

age_cs       <- sf_choices("age",
  c("18_25", "26_40", "41_50", "51_60", "60_plus"),
  c("18-25", "26-40", "41-50", "51-60", "60 and above"))

visitor_cs   <- sf_choices("visitor",
  c("first_time", "repeat"),
  c("First-time visitor", "Repeated visitor"))

freq_cs      <- sf_choices("freq_visit",
  c("lt_1", "once", "2_3", "gt_3"),
  c("Less than 1 time in a year", "Once in a year",
    "2-3 times in a year", "More than 3 times in a year"))

nationality_cs <- sf_choices("nationality",
  c("thai", "chinese", "japanese", "korean", "indian", "australian",
    "british", "german", "american", "other"),
  c("Thai", "Chinese", "Japanese", "Korean", "Indian", "Australian",
    "British", "German", "American", "Other"))

education_cs <- sf_choices("education",
  c("high_school", "diploma", "undergraduate", "post_graduate"),
  c("High school", "Diploma", "Undergraduate", "Post graduate"))

profession_cs <- sf_choices("profession",
  c("student", "business", "salaried", "freelancer", "not_working"),
  c("Student", "Business", "Salaried and working", "Freelancer",
    "Not working (housewife or retired)"))

companion_cs <- sf_choices("companion",
  c("friends", "family", "other"),
  c("Friends", "Family members", "Other"))

group_cs     <- sf_choices("group_size",
  c("lt_3", "3_5", "gt_5"),
  c("Less than 3", "3-5", "More than 5"))

demo_items <- list(
  sf_item("gender",        "Gender",                type = "single_choice",
          required = TRUE, choice_set = "gender"),
  sf_item("age_band",      "Age",                   type = "single_choice",
          required = TRUE, choice_set = "age"),
  sf_item("visitor_type",  "I am",                  type = "single_choice",
          required = TRUE, choice_set = "visitor"),
  sf_item("freq_visit",    "Frequency of visit",    type = "single_choice",
          required = TRUE, choice_set = "freq_visit"),
  sf_item("nationality",   "Nationality",           type = "single_choice",
          required = TRUE, choice_set = "nationality"),
  sf_item("education",     "Education level",       type = "single_choice",
          required = TRUE, choice_set = "education"),
  sf_item("profession",    "Profession",            type = "single_choice",
          required = TRUE, choice_set = "profession"),
  sf_item("companion",     "I visit with my",       type = "single_choice",
          required = TRUE, choice_set = "companion"),
  sf_item("group_size",    "My travel group size is", type = "single_choice",
          required = TRUE, choice_set = "group_size")
)

## ----scales-------------------------------------------------------------------
make_scale <- function(id, label, ids) {
  sf_scale(id, label, items = ids, method = "mean")
}

scales <- list(
  make_scale("DMRE", "Digital marketing: relevance and engagement",  paste0("dmre_", 1:5)),
  make_scale("DMAU", "Digital marketing: accessibility and usefulness", paste0("dmau_", 1:5)),
  make_scale("DMEU", "Digital marketing: ease of use",               paste0("dmeu_", 1:5)),
  make_scale("DMPV", "Digital marketing: perceived value",           paste0("dmpv_", 1:5)),
  make_scale("DSQA", "Destination service quality: accommodation",   paste0("dsqa_", 1:4)),
  make_scale("DSQT", "Destination service quality: transport",       paste0("dsqt_", 1:5)),
  make_scale("DSUQ", "Destination sustainability quality",           paste0("dsuq_", 1:11)),
  make_scale("TS",   "Tourist satisfaction",                         paste0("ts_",   1:3)),
  make_scale("BI",   "Behavioural intention",                        paste0("bi_",   1:3))
)

## ----assemble-----------------------------------------------------------------
choice_sets <- list(
  likert5, gender_cs, age_cs, visitor_cs, freq_cs, nationality_cs,
  education_cs, profession_cs, companion_cs, group_cs
)

rated_items <- c(dmre_items, dmau_items, dmeu_items, dmpv_items,
                 dsqa_items, dsqt_items, dsuq_items, ts_items, bi_items)

study <- sf_instrument(
  title       = "Digital Marketing Effectiveness of Tourism Services",
  version     = "1.0.0",
  description = paste(
    "Questionnaire covering digital marketing effectiveness,",
    "destination service quality, sustainability quality,",
    "tourist satisfaction, and behavioural intention.",
    "Adopted from Sharafuddin, Madhavan & Wangtueai (2024),",
    "Administrative Sciences, 14(11), 273.",
    "https://doi.org/10.3390/admsci14110273"
  ),
  authors   = "Mohammed Ali Sharafuddin",
  languages = "en",
  components = c(choice_sets, rated_items, demo_items, scales)
)

study

## ----validate-----------------------------------------------------------------
v <- validate_sframe(study, strict = FALSE)
v$valid
length(v$problems)

## ----write--------------------------------------------------------------------
# Save the instrument. Keep this file alongside your analysis script.
sframe_path <- write_sframe(study, file.path(tempdir(), "tourism_services_v1.sframe"),
                             overwrite = TRUE)

# Reload the instrument from disk at any time with:
study2 <- read_sframe(sframe_path)
identical(study$meta$title, study2$meta$title)

## ----endpoint, eval = FALSE---------------------------------------------------
# # Replace with your deployed Apps Script URL after the setup steps below.
# study$render$google_sheets_endpoint <-
#   "https://script.google.com/macros/s/YOUR_SCRIPT_ID/exec"

## ----static-export------------------------------------------------------------
html_path <- export_static_survey(
  study,
  output_path = file.path(tempdir(), "tourism_services_survey.html"),
  open        = FALSE
)
file.exists(html_path)

## ----theme--------------------------------------------------------------------
study$render$theme <- "#0f766e"

## ----gs-export----------------------------------------------------------------
script_path <- export_google_sheet(
  study,
  sheet_url  = "https://docs.google.com/spreadsheets/d/YOUR_SHEET_ID",
  output_dir = tempdir()
)
file.exists(script_path)

## ----live-read, eval = FALSE--------------------------------------------------
# responses <- read_sheet_responses(
#   sheet_id   = "YOUR_SHEET_ID",
#   instrument = study
# )

## ----simulate-----------------------------------------------------------------
set.seed(2024)
n <- 60

# Each construct is driven by a latent score plus item-level noise.
# Correlations between constructs are introduced by sharing variance.
lat <- function(mu, sigma) pmax(1, pmin(5, round(rnorm(n, mu, sigma))))
add_noise <- function(x, sigma = 0.45) {
  pmax(1L, pmin(5L, as.integer(round(x + rnorm(n, 0, sigma)))))
}

# Higher service quality and sustainability lift satisfaction.
lat_dsqa <- lat(3.7, 0.6)
lat_dsqt <- lat(3.6, 0.6)
lat_dsuq <- lat(3.5, 0.6)
lat_ts   <- pmax(1, pmin(5, round(
  0.4 * lat_dsqa + 0.3 * lat_dsqt + 0.2 * lat_dsuq + rnorm(n, 0.6, 0.3)
)))
lat_bi   <- pmax(1, pmin(5, round(0.7 * lat_ts + rnorm(n, 0.5, 0.4))))

# Digital marketing constructs are loosely correlated with each other.
lat_dmre <- lat(3.8, 0.6)
lat_dmau <- pmax(1, pmin(5, round(0.5 * lat_dmre + rnorm(n, 1.9, 0.4))))
lat_dmeu <- pmax(1, pmin(5, round(0.4 * lat_dmre + rnorm(n, 2.2, 0.4))))
lat_dmpv <- pmax(1, pmin(5, round(0.3 * lat_dmre + rnorm(n, 2.5, 0.4))))

# Repeat visitors score slightly higher on satisfaction and intention.
visitor_type <- sample(c("first_time", "repeat"), n,
                       replace = TRUE, prob = c(0.45, 0.55))
lat_ts[visitor_type == "repeat"] <- pmin(5L, lat_ts[visitor_type == "repeat"] + 1L)
lat_bi[visitor_type == "repeat"] <- pmin(5L, lat_bi[visitor_type == "repeat"] + 1L)

# Build item columns from latent scores.
make_cols <- function(lat, k, prefix) {
  setNames(
    as.data.frame(
      vapply(seq_len(k), function(i) add_noise(lat), integer(n))
    ),
    paste0(prefix, seq_len(k))
  )
}

sim_df <- cbind(
  data.frame(
    respondent_id = sprintf("R%03d", seq_len(n)),
    submitted_at  = format(
      seq(as.POSIXct("2025-01-10 09:00", tz = "UTC"),
          by = "1 hour", length.out = n),
      "%Y-%m-%dT%H:%M:%SZ"
    ),
    started_at = format(
      seq(as.POSIXct("2025-01-10 08:50", tz = "UTC"),
          by = "1 hour", length.out = n),
      "%Y-%m-%dT%H:%M:%SZ"
    ),
    gender = sample(c("male", "female", "transgender"),
                    n, TRUE, c(0.44, 0.55, 0.01)),
    age_band = sample(c("18_25", "26_40", "41_50", "51_60", "60_plus"),
                      n, TRUE, c(0.15, 0.40, 0.25, 0.15, 0.05)),
    visitor_type = visitor_type,
    freq_visit = sample(c("lt_1", "once", "2_3", "gt_3"),
                        n, TRUE, c(0.15, 0.25, 0.35, 0.25)),
    nationality = sample(
      c("thai", "chinese", "japanese", "korean", "indian",
        "australian", "british", "german", "american", "other"),
      n, TRUE),
    education = sample(c("high_school", "diploma", "undergraduate", "post_graduate"),
                       n, TRUE, c(0.05, 0.10, 0.48, 0.37)),
    profession = sample(c("student", "business", "salaried", "freelancer", "not_working"),
                        n, TRUE, c(0.15, 0.20, 0.45, 0.12, 0.08)),
    companion = sample(c("friends", "family", "other"),
                       n, TRUE, c(0.35, 0.55, 0.10)),
    group_size = sample(c("lt_3", "3_5", "gt_5"),
                        n, TRUE, c(0.30, 0.50, 0.20)),
    stringsAsFactors = FALSE
  ),
  make_cols(lat_dmre, 5, "dmre_"),
  make_cols(lat_dmau, 5, "dmau_"),
  make_cols(lat_dmeu, 5, "dmeu_"),
  make_cols(lat_dmpv, 5, "dmpv_"),
  make_cols(lat_dsqa, 4, "dsqa_"),
  make_cols(lat_dsqt, 5, "dsqt_"),
  make_cols(lat_dsuq, 11, "dsuq_"),
  make_cols(lat_ts,   3, "ts_"),
  make_cols(lat_bi,   3, "bi_")
)

# Align the data frame to the instrument.
responses <- read_responses(sim_df, study,
                             respondent_id = "respondent_id",
                             submitted_at  = "submitted_at",
                             meta_cols     = "started_at",
                             strict        = FALSE)

cat("Respondents:", nrow(responses), "\n")
cat("Columns:    ", ncol(responses), "\n")

## ----quality------------------------------------------------------------------
qr <- quality_report(responses, study, respondent_id = "respondent_id")
quality_summary <- data.frame(
  Metric = c("Respondents", "Items", "Flagged for review", "Flag rate"),
  Value  = c(qr$summary$n_respondents, qr$summary$n_items, qr$summary$n_flagged,
             sprintf("%.1f%%", 100 * qr$summary$flag_rate)),
  stringsAsFactors = FALSE
)
kable(quality_summary, align = c("l", "r"), caption = "Quality screening summary")

## ----missing------------------------------------------------------------------
mr <- missing_data_report(responses, study)
# mr holds $item_missing, $respondent_missing, $patterns, $mcar, and $apa.
kable(mr$item_missing, digits = 2,
      col.names = c("Variable", "Missing (n)", "Missing (%)", "Valid (n)"),
      caption = "Item-level missingness")
# $apa provides a plain-language summary suitable for a methods section.
cat(mr$apa, "\n")

## ----score--------------------------------------------------------------------
scored <- score_scales(responses, study)

scale_cols <- c("DMRE", "DMAU", "DMEU", "DMPV",
                "DSQA", "DSQT", "DSUQ", "TS", "BI")

# Display scale means and standard deviations as a table
scale_summary <- data.frame(
  Scale = scale_cols,
  Mean  = round(colMeans(scored[, scale_cols], na.rm = TRUE), 2),
  SD    = round(apply(scored[, scale_cols], 2, sd, na.rm = TRUE), 2),
  row.names = NULL
)
kable(scale_summary, digits = 2, caption = "Scale score summary")

## ----score-distributions, fig.width = 7, fig.height = 4, fig.align = "left", fig.alt = "Histograms of the scored scale distributions, one panel per scale"----
op <- par(mfrow = c(3, 3), mar = c(4, 3, 2, 1))
for (s in scale_cols) {
  v <- scored[[s]]; v <- v[is.finite(v)]
  hist(v, col = "#16B3B1", border = "white", main = s, xlab = "Score", ylab = "")
}
par(op)

## ----reliability--------------------------------------------------------------
if (requireNamespace("psych", quietly = TRUE)) {
  rr <- reliability_report(scored, study, omega = FALSE)
  rel_df <- do.call(rbind, lapply(rr, function(s) data.frame(
    Scale   = paste0(s$label, " (", s$scale_id, ")"),
    Items   = s$n_items,
    N       = s$n,
    Alpha   = if (!is.null(s$alpha)) sprintf("%.2f", s$alpha) else "n/a",
    stringsAsFactors = FALSE)))
  kable(rel_df, row.names = FALSE, align = c("l", "c", "c", "r"),
        caption = "Scale reliability")
}

## ----efa, eval=FALSE----------------------------------------------------------
# if (requireNamespace("psych", quietly = TRUE)) {
#   er <- efa_report(scored, study)
#   print(er)
# }

## ----validity, eval=FALSE-----------------------------------------------------
# # Supply a named list of loadings (construct -> item loadings vector)
# loadings_list <- list(
#   DMRE = c(dm_1 = 0.78, dm_2 = 0.82, dm_3 = 0.75),
#   TS   = c(ts_1 = 0.84, ts_2 = 0.80)
# )
# vr <- validity_report(loadings_list)
# print(vr$reliability)

## ----plan---------------------------------------------------------------------
study$analysis_plan <- list(
  list(
    id               = "RQ1",
    research_question = "Are digital marketing perceptions associated with tourist satisfaction?",
    family           = "association",
    method           = "correlation_pearson",
    roles            = list(x = "DMRE", y = "TS"),
    options          = list(alpha = 0.05)
  ),
  list(
    id               = "RQ2",
    research_question = "Is service quality associated with tourist satisfaction?",
    family           = "association",
    method           = "correlation_pearson",
    roles            = list(x = "DSQT", y = "TS"),
    options          = list(alpha = 0.05)
  ),
  list(
    id               = "RQ3",
    research_question = "Do service quality and sustainability quality predict satisfaction?",
    family           = "regression",
    method           = "regression_linear",
    roles            = list(predictors = c("DSQA", "DSQT", "DSUQ"),
                            dependent  = "TS"),
    options          = list(alpha = 0.05)
  ),
  list(
    id               = "RQ4",
    research_question = "Do first-time and repeat visitors differ in satisfaction?",
    family           = "group_comparison",
    method           = "mann_whitney",
    roles            = list(group = "visitor_type", outcome = "TS"),
    options          = list(alpha = 0.05)
  ),
  list(
    id               = "RQ5",
    research_question = "Does satisfaction predict behavioural intention?",
    family           = "regression",
    method           = "regression_linear",
    roles            = list(predictors = "TS", dependent = "BI"),
    options          = list(alpha = 0.05)
  )
)

length(study$analysis_plan)

## ----assumptions--------------------------------------------------------------
if (requireNamespace("psych", quietly = TRUE)) {
  ar <- assumption_report(scored, study)
  print(ar)
}

## ----run-plan-----------------------------------------------------------------
results <- run_analysis_plan(scored, study)

## ----show-results-------------------------------------------------------------
results_table(results)

## ----show-prompt--------------------------------------------------------------
cat(results[[1]]$prompt)

## ----run-plan-plots, eval=requireNamespace("ggplot2", quietly = TRUE), fig.width=7, fig.height=4.5, fig.alt="Chart attached to the first analysis-plan result by run_analysis_plan with plots enabled"----
results_p <- run_analysis_plan(scored, study, plots = TRUE)
first_plot <- Filter(function(r) !is.null(r$plot), results_p)[[1]]
first_plot$plot

## ----render-results-----------------------------------------------------------
results_path <- render_results(
  results,
  study,
  output_file = file.path(tempdir(), "tourism_results.html")
)
cat("Results report written:", results_path, "\n")
cat("Size:", round(file.size(results_path) / 1024, 1), "KB\n")

## ----render-report------------------------------------------------------------
render_report(
  study,
  data             = scored,
  output_file      = file.path(tempdir(), "tourism_report.html"),
  include_codebook = TRUE,
  include_quality  = TRUE,
  include_missing  = TRUE,
  include_descriptives = TRUE,
  include_reliability  = TRUE,
  include_analysis = TRUE,
  include_models   = FALSE
)

## ----live-workflow, eval = FALSE----------------------------------------------
# # 1. Pull the latest responses from the Google Sheet.
# responses <- read_sheet_responses(
#   sheet_id   = "YOUR_SHEET_ID",
#   instrument = study
# )
# 
# # 2. Run quality checks on the new data.
# quality_report(responses, study, respondent_id = "respondent_id")
# 
# # 3. Score the scales.
# scored <- score_scales(responses, study)
# 
# # 4. Run the pre-declared analysis plan.
# results <- run_analysis_plan(scored, study)
# 
# # 5. Render the updated report.
# render_report(
#   study,
#   data             = scored,
#   output_file      = "tourism_report_latest.html",
#   include_codebook = TRUE,
#   include_quality  = TRUE,
#   include_missing  = TRUE,
#   include_descriptives = TRUE,
#   include_reliability  = TRUE,
#   include_analysis = TRUE,
#   include_models   = FALSE
# )

## ----citation, eval = FALSE---------------------------------------------------
# citation("surveyframe")

