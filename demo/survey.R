## surveyframe guided end-to-end demo
##
## This walkthrough uses the bundled tourism-services questionnaire and
## simulated response dataset. It is safe to run offline.
## Eight sections - approximately 10-15 minutes to complete.

library(surveyframe)

`%||%` <- function(x, y) if (is.null(x)) y else x

## pause_demo(): contextual prompt. hint describes what is about to happen.
pause_demo <- function(hint = "Press <Return> to continue...") {
  if (interactive()) {
    invisible(readline(hint))
  } else {
    invisible(NULL)
  }
}

## section(): prints a numbered section header.
section <- function(title, n, total = 8L) {
  cat("\n", strrep("=", 72), "\n", sep = "")
  cat(sprintf("Section %d of %d  |  %s\n", n, total, title))
  cat(strrep("=", 72), "\n", sep = "")
}

browse_local <- function(path) {
  if (interactive() && file.exists(path)) {
    utils::browseURL(paste0("file://", normalizePath(path)))
  }
}

## ask_yn(): loops until the user types y, n, or presses Ctrl-C.
ask_yn <- function(prompt) {
  if (!interactive()) return(FALSE)
  repeat {
    ans <- trimws(readline(paste0(prompt, " [y/N] ")))
    if (tolower(ans) %in% c("y",  "yes")) return(TRUE)
    if (tolower(ans) %in% c("n",  "no", "")) return(FALSE)
    cat("Please type y or n and press <Return>.\n")
  }
}

demo_dir <- file.path(tempdir(), "surveyframe-demo")
dir.create(demo_dir, recursive = TRUE, showWarnings = FALSE)

## - Section 1 -

section("Walk through the SurveyBuilder GUI (demo preloaded)", 1)

cat("The builder opens in Build mode with the input-types demo already loaded.\n")
cat("You do NOT need to manually load a .sframe file.\n\n")
cat("Suggested workflow:\n")
cat("  1. The item list on the left is populated with demo questions.\n")
cat("     Click any item to inspect its settings in the right panel.\n")
cat("  2. Click the gear icon (top-right) to open Settings, then explore:\n")
cat("       General   - change the Theme colour or switch Presentation mode.\n")
cat("       Welcome   - edit the welcome title and consent checkbox.\n")
cat("       Thank You - edit the post-submit message.\n")
cat("       Header    - upload an institution logo.\n")
cat("  3. Click + Add question, choose Single choice, and edit the labels.\n")
cat("  4. Click Preview (top bar) to see the full respondent-facing survey.\n")
cat("     The Welcome / Survey / Thank You tabs step through each page.\n")
cat("  5. Go back to Settings > General, switch mode to Conversational, then\n")
cat("     click Preview again. The survey now steps one question at a time.\n")
cat("  6. Click Analyse (top bar) to see the planned analyses and model builder.\n")
cat("  7. Click Save .sframe (top bar) to save a modified version.\n\n")

if (interactive()) {
  pause_demo("Ready to open the SurveyBuilder? Press <Return>...")
  launch_builder_demo(open = TRUE)
  pause_demo("Done exploring the builder? Press <Return> to continue...")
} else {
  cat("Non-interactive session: launch_builder_demo(open = TRUE) skipped.\n")
}

## - Section 2 -

section("Load the tourism-services instrument and simulated responses", 2)

cat("Sections 2-8 use the tourism-services instrument (120 simulated respondents,\n")
cat("pre-planned analyses, five Likert scales, and one attention check).\n\n")

instrument_path <- system.file(
  "extdata", "tourism_services_demo.sframe",
  package = "surveyframe"
)
responses_path <- system.file(
  "extdata", "tourism_services_responses.csv",
  package = "surveyframe"
)

instr <- read_sframe(instrument_path)
responses <- read_responses(
  responses_path,
  instr,
  respondent_id = "respondent_id",
  submitted_at  = "submitted_at",
  meta_cols     = "started_at"
)

print(instr)
cat("\nResponses loaded:", nrow(responses), "rows x", ncol(responses), "columns\n")
cat("Demo output folder:", demo_dir, "\n\n")
pause_demo("Press <Return> to continue to Section 3: Static survey export...")

## - Section 3 -

section("Export and test a respondent-facing static survey", 3)

# Export a conversational-mode copy so users can compare both presentation styles.
instr_conv        <- instr
instr_conv$render$mode <- "conversational"

static_path <- export_static_survey(
  instr_conv,
  output_path = file.path(demo_dir, "tourism_services_static_survey.html"),
  open        = FALSE,
  overwrite   = TRUE
)

cat("A self-contained offline HTML survey has been written to:\n")
cat(static_path, "\n\n")
cat("This export uses Conversational mode (one question at a time).\n")
cat("Standard (page-by-page) mode is what you saw in the builder Preview.\n\n")
cat("In the browser:\n")
cat("  -> Read the welcome page and click Start.\n")
cat("  -> The survey steps forward one question at a time automatically.\n")
cat("  -> Skip a required question; the survey should block progression.\n")
cat("  -> Complete all questions and click Submit; a CSV response file downloads.\n")
cat("  -> Come back here after submitting.\n\n")

browse_local(static_path)
pause_demo("Finished testing the static survey? Press <Return> to continue...")

## - Section 4 -

section("Score multi-item constructs (scale scoring)", 4)

scored     <- score_scales(responses, instr)
scale_names <- vapply(instr$scales, function(s) s$id, character(1))

scale_summary <- data.frame(
  scale   = scale_names,
  label   = vapply(instr$scales, function(s) s$label,        character(1)),
  n_items = vapply(instr$scales, function(s) length(s$items), integer(1)),
  mean    = round(vapply(scored[scale_names], mean,       numeric(1), na.rm = TRUE), 2),
  sd      = round(vapply(scored[scale_names], stats::sd,  numeric(1), na.rm = TRUE), 2),
  stringsAsFactors = FALSE,
  check.names      = FALSE
)

print(scale_summary, row.names = FALSE)
utils::write.csv(
  scale_summary,
  file.path(demo_dir, "table_scale_summary.csv"),
  row.names = FALSE
)

png(file.path(demo_dir, "plot_scale_distributions.png"), width = 1100, height = 750)
op <- par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
for (nm in scale_names) {
  hist(
    scored[[nm]],
    breaks = seq(1, 5, by = 0.5),
    col    = "#1f6f78",
    border = "white",
    main   = gsub("_", " ", nm),
    xlab   = "Scale score",
    xlim   = c(1, 5)
  )
  abline(v = mean(scored[[nm]], na.rm = TRUE), col = "#d97706", lwd = 2)
}
par(op)
dev.off()

cat("\nSaved:\n")
cat("  table_scale_summary.csv\n")
cat("  plot_scale_distributions.png\n\n")
cat("In the browser:\n")
cat("  -> Each histogram shows the distribution of one scale (1-5 range).\n")
cat("  -> The orange line marks the sample mean for that scale.\n\n")

browse_local(file.path(demo_dir, "plot_scale_distributions.png"))
pause_demo("Press <Return> to continue to Section 5: Analysis plan...")

## - Section 5 -

section("Run the pre-planned analysis", 5)

results <- run_analysis_plan(responses, instr)

analysis_table <- data.frame(
  rq                = seq_along(results),
  research_question = vapply(results, function(x) x$research_question, character(1)),
  test              = vapply(results, function(x) x$test,              character(1)),
  apa_result        = vapply(results, function(x) x$apa %||% x$error %||% "", character(1)),
  stringsAsFactors  = FALSE,
  check.names       = FALSE
)

cat("Analysis results:\n\n")
print(analysis_table[, c("rq", "test", "apa_result")], row.names = FALSE)
utils::write.csv(
  analysis_table,
  file.path(demo_dir, "table_analysis_results.csv"),
  row.names = FALSE
)

png(file.path(demo_dir, "plot_digital_marketing_satisfaction.png"),
    width = 900, height = 650)
plot(
  scored$digital_marketing,
  scored$satisfaction,
  pch  = 19,
  col  = "#1f6f7899",
  xlab = "Digital marketing effectiveness",
  ylab = "Tourist satisfaction",
  main = "Digital marketing and satisfaction"
)
abline(stats::lm(satisfaction ~ digital_marketing, data = scored),
       col = "#d97706", lwd = 2)
grid()
dev.off()

cat("\nSaved:\n")
cat("  table_analysis_results.csv\n")
cat("  plot_digital_marketing_satisfaction.png\n\n")
cat("In the browser:\n")
cat("  -> The scatter plot shows the relationship between digital marketing\n")
cat("     effectiveness and tourist satisfaction.\n")
cat("  -> The orange line is the OLS regression fit.\n\n")

browse_local(file.path(demo_dir, "plot_digital_marketing_satisfaction.png"))
pause_demo("Press <Return> to continue to Section 6: Quality report...")

## - Section 6 -

section("Check response quality", 6)

cat("quality_report() flags respondents who:\n")
cat("  - failed one or more attention checks,\n")
cat("  - completed the survey unusually quickly,\n")
cat("  - have excessive missing values, or\n")
cat("  - gave identical answers across all items of a scale (straight-lining).\n\n")

quality <- quality_report(
  responses,
  instr,
  respondent_id      = "respondent_id",
  submitted_at       = "submitted_at",
  started_at         = "started_at",
  straightline_scales = FALSE
)

quality_table <- data.frame(
  Metric = c(
    "Respondents",
    "Flagged respondents",
    "Flag rate",
    "Attention-check pass rate",
    "Median completion time (seconds)"
  ),
  Value = c(
    quality$summary$n_respondents,
    quality$summary$n_flagged,
    sprintf("%.1f%%", quality$summary$flag_rate * 100),
    sprintf("%.1f%%", quality$attention$attention_agree$pass_rate * 100),
    round(quality$timing$median_sec, 1)
  ),
  stringsAsFactors = FALSE,
  check.names      = FALSE
)

print(quality_table, row.names = FALSE)
utils::write.csv(
  quality_table,
  file.path(demo_dir, "table_quality_summary.csv"),
  row.names = FALSE
)

png(file.path(demo_dir, "plot_attention_check.png"), width = 780, height = 520)
barplot(
  c(
    Pass = quality$attention$attention_agree$n_pass,
    Fail = quality$attention$attention_agree$n_fail
  ),
  col    = c("#1f6f78", "#dc2626"),
  border = NA,
  main   = "Attention-check outcome",
  ylab   = "Respondents"
)
dev.off()

cat("\nSaved:\n")
cat("  table_quality_summary.csv\n")
cat("  plot_attention_check.png\n\n")
cat("In the browser:\n")
cat("  -> Green = respondents who passed the attention check.\n")
cat("  -> Red   = respondents who failed.\n")
cat("  -> In real research, exclude or re-weight flagged respondents before\n")
cat("     running the analysis in Section 5.\n\n")

browse_local(file.path(demo_dir, "plot_attention_check.png"))
pause_demo("Press <Return> to continue to Section 7: HTML reports...")

## - Section 7 -

section("Render publication-ready HTML reports", 7)

cat("Generating two reports:\n")
cat("  analysis_results.html  - APA-formatted analysis results, effect sizes,\n")
cat("                           interpretation prompts, and references.\n")
cat("  survey_report.html     - instrument codebook, data quality summary,\n")
cat("                           descriptive statistics, and reproducibility block.\n\n")
cat("Tables in both reports use APA style: horizontal rules only, no cell\n")
cat("borders, no row shading.\n\n")

results_report <- render_results(
  results,
  instr,
  output_file = file.path(demo_dir, "analysis_results.html")
)

old <- options(surveyframe.use_quarto = FALSE)
full_report <- render_report(
  instr,
  data                = responses,
  output_file         = file.path(demo_dir, "survey_report.html"),
  include_reliability = FALSE,
  include_analysis    = FALSE
)
options(old)

cat("Reports written:\n")
cat("  ", results_report, "\n", sep = "")
cat("  ", full_report,    "\n\n", sep = "")
cat("In the browser:\n")
cat("  -> analysis_results.html: one card per research question, with the\n")
cat("     APA result string, effect size badge, and writing prompt.\n")
cat("  -> survey_report.html: codebook, quality summary, descriptives, and\n")
cat("     a reproducibility hash for the instrument.\n\n")

browse_local(results_report)
browse_local(full_report)

cat("All files created so far:\n")
print(list.files(demo_dir, full.names = FALSE))
pause_demo("\nPress <Return> to continue to Section 8: Dashboard...")

## - Section 8 -

section("Explore responses in the interactive dashboard", 8)

cat("The dashboard opens with the tourism-services instrument and the 120\n")
cat("simulated responses loaded in this session.\n\n")
cat("Use the five tabs:\n")
cat("  Overview  - response count, date range, instrument summary.\n")
cat("  Items     - per-item frequency charts and frequency tables.\n")
cat("  Scales    - scale score histograms with mean overlay.\n")
cat("  Quality   - attention-check pass rates with colour-coded rows.\n")
cat("  Raw Data  - scrollable response table; CSV download button.\n\n")

if (ask_yn("Open the response dashboard now?")) {
  launch_dashboard(instr, responses)
}

cat("\n", strrep("-", 72), "\n", sep = "")
cat("Demo complete. Repeatable commands for your own work:\n\n")
cat("  launch_builder()         # empty builder for a new survey\n")
cat("  launch_builder_demo()    # builder with the input-types demo preloaded\n")
cat("  launch_studio_demo()     # studio with the input-types demo preloaded\n")
cat("  launch_dashboard_demo()  # dashboard with 120 input-types demo responses\n")
cat("\n")
