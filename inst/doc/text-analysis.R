## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(surveyframe)
library(knitr)
set.seed(2026)

has_ggplot     <- requireNamespace("ggplot2", quietly = TRUE)
has_tidytext   <- requireNamespace("tidytext", quietly = TRUE)
has_quanteda   <- requireNamespace("quanteda", quietly = TRUE)
has_stm        <- requireNamespace("stm", quietly = TRUE)
has_igraph     <- requireNamespace("igraph", quietly = TRUE)
knitr::opts_chunk$set(fig.width = 7, fig.height = 4.2, dpi = 96)

## ----instrument---------------------------------------------------------------
satisfaction_cs <- sf_choices(
  "agree5", values = 1:5,
  labels = c("Strongly disagree", "Disagree", "Neutral",
             "Agree", "Strongly agree")
)
branch_cs <- sf_choices(
  "branch", values = c("north", "south"),
  labels = c("North branch", "South branch")
)

instr <- sf_instrument(
  title = "Hospitality feedback",
  version = "1.0.0",
  components = list(
    satisfaction_cs, branch_cs,
    sf_item("satisfaction", "Overall, I was satisfied with my visit.",
            type = "likert", choice_set = "agree5"),
    sf_item("branch", "Which branch did you visit?",
            type = "single_choice", choice_set = "branch_cs"),
    sf_item("comments", "What stood out about your visit, good or bad?",
            type = "textarea")
  )
)

## ----simulate-----------------------------------------------------------------
positive_pool <- c(
  "The staff were incredibly friendly and helpful.",
  "Check-in was quick and the room was spotless.",
  "Our server was attentive and the food arrived fast.",
  "The team went out of their way to help us.",
  "Friendly staff made the whole visit pleasant.",
  "The room was clean and comfortable throughout our stay.",
  "Quick service and a warm welcome from everyone.",
  "The staff were helpful whenever we needed anything."
)
negative_pool <- c(
  "We had to wait a long time for someone to help us.",
  "The staff seemed rude and unhelpful the whole time.",
  "The wait for a table was far too long.",
  "Our room was dirty and the service was slow.",
  "Staff were slow to respond and not very friendly.",
  "The long wait ruined an otherwise average visit.",
  "Service was poor and the staff seemed uninterested.",
  "We waited a long time and nobody apologised."
)

sample_comment <- function(p_positive) {
  n_sentences <- sample(1:2, 1)
  pools <- sample(c("pos", "neg"), n_sentences, replace = TRUE,
                  prob = c(p_positive, 1 - p_positive))
  sentences <- vapply(pools, function(p) {
    if (p == "pos") sample(positive_pool, 1) else sample(negative_pool, 1)
  }, character(1))
  paste(sentences, collapse = " ")
}

n <- 60
branch <- sample(c("north", "south"), n, replace = TRUE)
comments <- vapply(branch, function(b) {
  sample_comment(if (b == "north") 0.8 else 0.35)
}, character(1))

responses <- data.frame(
  satisfaction = sample(3:5, n, replace = TRUE, prob = c(0.2, 0.35, 0.45)),
  branch = branch,
  comments = comments,
  stringsAsFactors = FALSE
)

kable(head(responses, 4), row.names = FALSE,
      caption = "The first 4 simulated responses.")

## ----clean--------------------------------------------------------------------
cleaned <- clean_text_responses(responses, "comments", instrument = instr)
length(cleaned)
head(attr(cleaned, "respondent"))

## ----term-freq----------------------------------------------------------------
terms <- term_frequency(cleaned, top_n = 10)
kable(terms, row.names = FALSE, caption = "The 10 most frequent terms.")

## ----ngram-freq---------------------------------------------------------------
bigrams <- ngram_frequency(cleaned, n = 2, top_n = 8)
kable(bigrams, row.names = FALSE, caption = "The 8 most frequent bigrams.")

## ----plan-term----------------------------------------------------------------
sf_plan(instr) <- list(
  list(id = "RQ1",
       research_question = "What themes recur in the open-ended comments?",
       family = "text", method = "term_freq",
       roles = list(item = "comments"),
       options = list()),
  list(id = "RQ2",
       research_question = "Do the leading themes differ by branch?",
       family = "text", method = "term_freq",
       roles = list(item = "comments", group = "branch"),
       options = list())
)

results <- run_analysis_plan(responses, instr, plots = has_ggplot)
kable(results[["RQ1"]]$table, row.names = FALSE,
      caption = "Term frequency across all branches.")

## ----plan-term-plot, fig.alt = "Horizontal bar chart of the most frequent terms across all responses.", eval = has_ggplot----
results[["RQ1"]]$plot

## ----wordcloud, fig.alt = "A circular word cloud of the most frequent terms, sized and shaded (dark for frequent, light for rare) by frequency.", eval = has_ggplot----
sframe_plot_term_frequency(
  list(test = "term_freq", variable = "comments", table = terms,
       options = list(wordcloud = TRUE))
)

## ----group-table--------------------------------------------------------------
kable(results[["RQ2"]]$table, row.names = FALSE,
      caption = "Term frequency split by branch.")

## ----group-plot, fig.alt = "Two side-by-side bar charts of the most frequent terms, one for the north branch and one for the south branch.", eval = has_ggplot----
results[["RQ2"]]$plot

## ----context------------------------------------------------------------------
kwic <- term_context(cleaned, term = "wait", window = 5)
kable(kwic, row.names = FALSE, caption = 'Every occurrence of "wait" in context.')

## ----cooccurrence-------------------------------------------------------------
sf_plan(instr) <- c(sf_plan(instr), list(list(
  id = "RQ3",
  research_question = "Which terms tend to appear together in the same comment?",
  family = "text", method = "co_occurrence",
  roles = list(item = "comments"),
  options = list()
)))
results <- run_analysis_plan(responses, instr, plots = has_ggplot)

kable(head(results[["RQ3"]]$table, 8), row.names = FALSE,
      caption = "The strongest co-occurring term pairs.")

## ----cooccurrence-plot, fig.alt = "Heatmap of pairwise term co-occurrence counts.", eval = has_ggplot----
results[["RQ3"]]$plot

## ----network, eval = has_igraph-----------------------------------------------
sf_plan(instr) <- c(sf_plan(instr), list(list(
  id = "RQ4",
  research_question = "Do the frequent terms form distinct thematic clusters?",
  family = "text", method = "co_occurrence_network",
  roles = list(item = "comments"),
  options = list(seed = 42)
)))
results <- run_analysis_plan(responses, instr, plots = has_ggplot)

kable(results[["RQ4"]]$table, row.names = FALSE,
      caption = "Term co-occurrence network: one row per node.")
results[["RQ4"]]$apa

## ----network-plot, fig.alt = "Force-directed network of co-occurring terms, coloured by Louvain cluster and sized by frequency.", eval = has_igraph && has_ggplot----
results[["RQ4"]]$plot

## ----network-note, eval = !has_igraph, echo = FALSE, results = "asis"---------
# cat("_igraph is not installed in this environment, so this section did not run.",
#     "Install igraph to see the clustered network._")

## ----sentiment, eval = has_tidytext-------------------------------------------
sf_plan(instr) <- c(sf_plan(instr), list(list(
  id = "RQ5",
  research_question = "Is sentiment in the comments more positive or negative, and does it differ by branch?",
  family = "text", method = "tidy_sentiment",
  roles = list(item = "comments", group = "branch"),
  options = list()
)))
results <- run_analysis_plan(responses, instr, plots = has_ggplot)

kable(results[["RQ5"]]$table, row.names = FALSE,
      caption = "Sentiment counts, split by branch.")
results[["RQ5"]]$apa

## ----sentiment-plot, fig.alt = "Diverging bar chart of positive and negative sentiment counts, faceted by branch.", eval = has_tidytext && has_ggplot----
results[["RQ5"]]$plot

## ----sentiment-cloud, fig.alt = "Comparison word cloud: negative-sentiment terms to the left of centre in red, positive-sentiment terms to the right in teal, each sized and shaded by frequency.", eval = has_tidytext && has_ggplot----
instr_cloud <- instr
plan <- sf_plan(instr_cloud)
plan[[which(vapply(plan, `[[`, "", "id") == "RQ5")]]$options <- list(wordcloud = TRUE)
sf_plan(instr_cloud) <- plan

result_cloud <- run_analysis_plan(responses, instr_cloud, plots = TRUE)
result_cloud[["RQ5"]]$plot

## ----sentiment-note, eval = !has_tidytext, echo = FALSE, results = "asis"-----
# cat("_tidytext is not installed in this environment, so this section did not run.",
#     "Install tidytext to see the sentiment breakdown._")

## ----dfm, eval = has_quanteda-------------------------------------------------
sf_plan(instr) <- c(sf_plan(instr), list(list(
  id = "RQ6",
  research_question = "What does the document-feature matrix of the comments look like?",
  family = "text", method = "quanteda_dfm",
  roles = list(item = "comments"),
  options = list()
)))
results <- run_analysis_plan(responses, instr)

kable(results[["RQ6"]]$table, row.names = FALSE,
      caption = "Document-feature matrix summary.")
kable(head(results[["RQ6"]]$top_features, 8), row.names = FALSE,
      caption = "The leading features.")

## ----dfm-note, eval = !has_quanteda, echo = FALSE, results = "asis"-----------
# cat("_quanteda is not installed in this environment, so this section did not run.",
#     "Install quanteda to see the document-feature matrix._")

## ----stm, eval = has_stm && has_tidytext--------------------------------------
sf_plan(instr) <- c(sf_plan(instr), list(list(
  id = "RQ7",
  research_question = "What topics organise the open-ended comments?",
  family = "text", method = "stm_topics",
  roles = list(item = "comments"),
  options = list(k = 3, seed = 42)
)))
results <- run_analysis_plan(responses, instr, plots = has_ggplot)

kable(results[["RQ7"]]$table, row.names = FALSE,
      caption = "Top terms per topic.")
results[["RQ7"]]$apa

## ----stm-plot, fig.alt = "Faceted bar chart of the top terms for each of the 3 topics.", eval = has_stm && has_tidytext && has_ggplot----
results[["RQ7"]]$plot

## ----quotes, eval = has_stm && has_tidytext-----------------------------------
quotes <- extract_quotes(results[["RQ7"]], text = cleaned, n_quotes = 2)
kable(quotes, row.names = FALSE,
      caption = "The 2 most representative comments per topic.")

## ----stm-note, eval = !(has_stm && has_tidytext), echo = FALSE, results = "asis"----
# cat("_stm and/or tidytext are not installed in this environment, so this",
#     "section did not run. Install both to see the topic model and its",
#     "representative quotes._")

## ----report, eval = FALSE-----------------------------------------------------
# render_report(instr, responses, output_file = "hospitality-feedback.html")

