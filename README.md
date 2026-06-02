<img src="man/figures/readme-logo.png" align="right" width="220" alt="surveyframe" />

# surveyframe

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/surveyframe)](https://CRAN.R-project.org/package=surveyframe)
[![R-CMD-check](https://github.com/MohammedAliSharafuddin/surveyframe/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MohammedAliSharafuddin/surveyframe/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

`surveyframe` is a research-design-first survey package for R. Most survey tools
collect answers and return counts. `surveyframe` begins at the research design
and carries it through to a written results report.

The unit of work is the instrument, a typed `sframe` object that stores three
things together:

1. **The questions.** Items, choice sets, scales, branching rules, and attention
   checks.
2. **The analysis plan.** A list of research questions, where each question is
   bound to a named statistical technique and to the variables that fill each
   role in that technique. The plan is written during design, before any data
   arrive.
3. **The measurement or structural model.** Constructs, indicators, and paths
   for EFA, CFA, CB-SEM, and PLS-SEM.

Because the plan and the model live inside the instrument, the link between a
question, the variable it produces, and the test that variable feeds is fixed at
design time. When responses come back, the plan runs in one pass and returns
results already formatted for reporting, with effect sizes, a writing prompt for
each finding, and the reference that supports each test.

The package works offline during examples, tests, vignettes, and checks. Browser
and Shiny entry points use `open = FALSE` or explicit launch functions, so
automated checks do not open a browser.

## Installation

Install from CRAN:

```r
install.packages("surveyframe")
```

To get unreleased changes from the development version:

```r
remotes::install_github("MohammedAliSharafuddin/surveyframe")
```

Optional packages are only needed for selected features:

```r
install.packages(c("shiny", "psych", "googlesheets4", "digest", "MASS", "nnet"))
```

Syntax generation works without installing `lavaan` or `seminr`. Install those
packages when you want to fit the generated CFA, CB-SEM, or PLS-SEM models.

## Already have data?

If you have collected responses in a CSV or Google Sheet and want to start
from the analysis step, build a minimal instrument that matches your column
names and load the data directly:

```r
library(surveyframe)

# 1. Describe the items you already collected
cs  <- sf_choices("agree5", 1:5,
        c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
i1  <- sf_item("q1", "Item 1", type = "likert", choice_set = "agree5", scale_id = "S")
i2  <- sf_item("q2", "Item 2", type = "likert", choice_set = "agree5", scale_id = "S")
sc  <- sf_scale("S", "My scale", items = c("q1", "q2"))
instr <- sf_instrument("My study", components = list(cs, i1, i2, sc))

# 2. Load your CSV
responses <- read_responses("my_data.csv", instr, strict = FALSE)

# 3. Score and analyse
scored  <- score_scales(responses, instr)
results <- run_analysis_plan(scored, instr)
```

## Documentation workflow

Start with:

1. A worked study: digital marketing and tourism services
2. Building a survey instrument: questions, plan, and model
3. Analysing survey responses: running the plan
4. Scale reliability and validity
5. EFA, CFA, CB-SEM, and PLS-SEM syntax generation
6. The visual workflow: SurveyBuilder, SurveyStudio, and the dashboard

Read all six vignettes inside R with:

```r
browseVignettes("surveyframe")
```

## An instrument is the research design

```r
library(surveyframe)

agree5 <- sf_choices(
  "agree5",
  values = 1:5,
  labels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")
)

visitor_type_choices <- sf_choices(
  "visitor_type",
  values = c("first_time", "repeat"),
  labels = c("First-time visitor", "Repeat visitor")
)

sat_1 <- sf_item("sat_1", "The service was reliable.",
  type = "likert", choice_set = "agree5", scale_id = "sat")
sat_2 <- sf_item("sat_2", "The service was responsive.",
  type = "likert", choice_set = "agree5", scale_id = "sat")
sat_3 <- sf_item("sat_3", "I would recommend the service.",
  type = "likert", choice_set = "agree5", scale_id = "sat")

visitor_type <- sf_item("visitor_type", "Visitor type", type = "single_choice",
  choice_set = "visitor_type")
sat <- sf_scale("sat", "Satisfaction", items = c("sat_1", "sat_2", "sat_3"))

instr <- sf_instrument(
  "Service Survey",
  components = list(
    agree5, visitor_type_choices, sat_1, sat_2, sat_3, visitor_type, sat
  ),
  analysis_plan = list(
    list(
      id                = "RQ1",
      research_question = "Do first-time and repeat visitors differ in satisfaction?",
      family            = "group_comparison",
      method            = "mann_whitney",
      roles             = list(group = "visitor_type", outcome = "sat"),
      options           = list(alpha = 0.05)
    )
  )
)

instr <- validate_sframe(instr)
write_sframe(instr, tempfile(fileext = ".sframe"))
```

`write_sframe()` validates the instrument and writes the validated object,
including the validation flag, the analysis plan, and any saved model
specifications.

## Import and score

```r
responses <- data.frame(
  respondent_id = paste0("R", 1:5),
  sat_1 = c(4, 5, 3, 4, NA),
  sat_2 = c(5, 4, 3, 4, 5),
  sat_3 = c(4, 5, 2, 4, 4),
  visitor_type = c("first_time", "repeat", "first_time", "repeat", "first_time")
)

resp <- read_responses(responses, instr, respondent_id = "respondent_id", strict = FALSE)

score_scales(resp, instr)
missing_data_report(resp, instr)
```

## Run the analysis plan

Each block binds a research question to a technique and to the variables that
fill each role. `run_analysis_plan()` runs every block and returns one result
per question. Earlier `.sframe` files using `variables` and `test` fields remain
compatible.

```r
results <- run_analysis_plan(resp, instr)
results
```

Supported method IDs include descriptives, missing data, quality checks,
reliability, EFA readiness and solutions, CFA, CB-SEM, and PLS-SEM syntax,
chi-square, Fisher's exact test, McNemar, Cochran's Q, t-tests, Mann-Whitney,
Wilcoxon, one- and two-way ANOVA, ANCOVA, repeated-measures ANOVA,
Kruskal-Wallis, Friedman, Pearson, Spearman, and Kendall correlations, partial
correlations, linear and logistic regression, mediation, and moderation. Each
technique reports an APA statistic, an effect size where it applies, a writing
prompt, and the reference that supports it.

## Render the results report

```r
render_results(results, instr, output_file = tempfile(fileext = ".html"))
```

The report holds one section per research question, with the APA result, the
writing prompt, a space for the interpretation, and a reference list compiled
from the techniques used.

## Reliability, EFA, and CFA

```r
if (requireNamespace("psych", quietly = TRUE)) {
  reliability_report(resp, instr, omega = FALSE)
  efa_report(resp, instr)
}

cfa_syntax(instr)
cfa_lavaan_syntax(instr, ordered = TRUE)
```

## Model layer

```r
model <- sf_model(
  "model_1",
  "Satisfaction model",
  type = "cb_sem",
  constructs = list(
    sf_construct("SAT", "Satisfaction", c("sat_1", "sat_2", "sat_3"))
  )
)

instr <- add_model(instr, model)
model_json(model)
sem_lavaan_syntax(model, instr)
```

## PLS-SEM syntax

```r
pls_model <- sf_model(
  "pls_1",
  "Satisfaction and loyalty PLS model",
  type = "pls_sem",
  constructs = list(
    sf_construct("SAT", "Satisfaction", c("sat_1", "sat_2"), mode = "composite"),
    sf_construct("LOY", "Loyalty", "sat_3", mode = "single_item")
  ),
  paths = list(sf_path("SAT", "LOY")),
  options = list(bootstrap = 5000)
)

seminr_syntax(pls_model)
```

## A full study report

```r
render_report(
  instr,
  data = resp,
  output_file = tempfile(fileext = ".html"),
  include_codebook = TRUE,
  include_quality = TRUE,
  include_missing = TRUE,
  include_descriptives = TRUE,
  include_analysis = TRUE,
  include_models = TRUE
)
```

The built-in HTML fallback does not require Quarto. If the Quarto CLI is
available locally, `render_report()` can use the bundled template.

## Visual tools

```r
launch_builder(open = FALSE)
export_static_survey(instr, open = FALSE)
```

Use `launch_builder()` to author the questionnaire, the plan, and the model and
to export the `.sframe` file and model syntax; it runs no statistics.
`launch_studio()` uploads responses, runs the plan on its Analysis Plan screen,
and renders the report on its Export screen. `launch_dashboard()` is a read-only
response explorer. Demo launchers are available for training:

```r
launch_builder_demo(open = FALSE)
# launch_studio_demo()
# launch_dashboard_demo()
```

Interactive functions such as `launch_builder(open = TRUE)`, `launch_studio()`,
`render_survey()`, and `launch_dashboard()` are available for manual use. Tests
and examples avoid opening browsers.

## v0.4 scope

MCDM and DEMATEL fall outside v0.3 scope. These methods are scheduled for v0.4.

## Citation

```r
citation("surveyframe")
```

## Related resources

- Quantitative Analysis with Small Samples, a companion textbook on statistical
  inference when sample sizes are small:
  <https://flairmi.com/textbooks/smallsamplelab.html>
- Small-sample inference helpers, validated by a simulation study of survey
  methods, are planned for a future surveyframe version. The textbook above
  describes the methods that surveyframe will add and when to prefer each one.

## License

MIT. See `LICENSE`.
