## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(surveyframe)
library(knitr)
set.seed(2026)

results_table <- function(results) {
  g <- function(r, f) { v <- r[[f]]; if (is.null(v) || !length(v)) "" else as.character(v)[1] }
  df <- data.frame(
    RQ       = vapply(results, g, "", "block_id"),
    Question = vapply(results, g, "", "research_question"),
    Method   = vapply(results, g, "", "method"),
    Result   = vapply(results, g, "", "apa"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  kable(df, row.names = FALSE,
        col.names = c("RQ", "Research question", "Method", "Result (APA)"),
        align = c("l", "l", "l", "r"))
}

## ----plan-n-------------------------------------------------------------------
sample_size_plan(type = "t_test", groups = 2)

## ----instrument---------------------------------------------------------------
group_cs     <- sf_choices(id = "grp_cs", values = c("control", "treatment"),
                            labels = c("Control", "Treatment"))
converted_cs <- sf_choices(id = "conv_cs", values = c("no", "yes"),
                            labels = c("No", "Yes"))

items <- list(
  sf_item(id = "group", label = "Study arm", type = "single_choice", choice_set = "grp_cs"),
  sf_item(id = "outcome", label = "Outcome score", type = "numeric"),
  sf_item(id = "converted", label = "Converted (yes/no)", type = "single_choice",
          choice_set = "conv_cs")
)

study <- sf_instrument(
  title      = "Small-sample pilot",
  version    = "0.1.0",
  authors    = "surveyframe",
  components = c(list(group_cs, converted_cs), items)
)

study

## ----assumption-advisory------------------------------------------------------
n20 <- data.frame(score = rnorm(20, mean = 50, sd = 10))
n50 <- data.frame(score = rnorm(50, mean = 50, sd = 10))

ar_small <- assumption_report(n20, variables = "score")
ar_small$advisory

ar_large <- assumption_report(n50, variables = "score")
is.null(ar_large$advisory)

## ----mann-whitney-------------------------------------------------------------
pilot <- data.frame(
  group   = rep(c("control", "treatment"), each = 10),
  outcome = c(rnorm(10, 48, 8), rnorm(10, 55, 8)),
  converted = sample(c("no", "yes"), 20, replace = TRUE, prob = c(0.6, 0.4)),
  covariate = rnorm(20, 0, 1)
)

sf_plan(study) <- list(
  list(id = "RQ1",
       research_question = "Does the treatment arm score higher than control?",
       family = "group_comparison", method = "mann_whitney",
       roles = list(group = "group", outcome = "outcome"),
       options = list(alpha = 0.05))
)

mw_results <- run_analysis_plan(pilot, study)
results_table(mw_results)
mw_results[[1]]$hl_shift
mw_results[[1]]$hl_conf_int

## ----wilcoxon-pair------------------------------------------------------------
before <- rnorm(8, 50, 6)
after  <- before + rnorm(8, 4, 5)

sf_plan(study) <- list(
  list(id = "RQ2",
       research_question = "Did scores change from before to after?",
       family = "group_comparison", method = "wilcoxon_pair",
       roles = list(before = "before", after = "after"),
       options = list(alpha = 0.05))
)

wp_results <- run_analysis_plan(data.frame(before = before, after = after), study)
results_table(wp_results)
wp_results[[1]]$pseudomedian
wp_results[[1]]$pseudomedian_conf_int

## ----fisher-------------------------------------------------------------------
sf_plan(study) <- list(
  list(id = "RQ3",
       research_question = "Is conversion associated with study arm?",
       family = "association", method = "fisher_exact",
       roles = list(row = "group", column = "converted"),
       options = list(alpha = 0.05))
)

fe_results <- run_analysis_plan(pilot, study)
results_table(fe_results)
fe_results[[1]]$odds_ratio
fe_results[[1]]$odds_ratio_conf_int

## ----bootstrap-median---------------------------------------------------------
bootstrap_ci(pilot$outcome, FUN = stats::median, R = 999)

## ----firth, eval = requireNamespace("logistf", quietly = TRUE)----------------
sf_plan(study) <- list(
  list(id = "RQ4",
       research_question = "Does the covariate predict conversion?",
       family = "regression", method = "firth_logistic",
       roles = list(dependent = "converted", predictors = "covariate"),
       options = list(conf.level = 0.95))
)

firth_results <- run_analysis_plan(pilot, study)
results_table(firth_results)
firth_results[[1]]$coefficients

## ----firth-note, eval = !requireNamespace("logistf", quietly = TRUE), echo = FALSE----
# cat("logistf is not installed in this build environment, so the Firth",
#     "example above is skipped. Install logistf to run it.\n")

## ----cohens-d-----------------------------------------------------------------
cohens_d_ci(pilot$outcome[pilot$group == "treatment"],
            pilot$outcome[pilot$group == "control"], R = 999)

## ----citation, eval = FALSE---------------------------------------------------
# citation("surveyframe")

