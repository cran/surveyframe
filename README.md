# surveyframe

<!-- badges: start -->
[![R-CMD-check](https://github.com/MohammedAliSharafuddin/surveyframe/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MohammedAliSharafuddin/surveyframe/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/MohammedAliSharafuddin/surveyframe/branch/main/graph/badge.svg)](https://app.codecov.io/gh/MohammedAliSharafuddin/surveyframe?branch=main)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

`surveyframe` supports survey research workflows through a typed instrument
object, the `sframe`. It stores item definitions, choice sets, scales,
branching, checks, analysis plans, model specifications, rendering hints, and
reproducibility metadata.

The package works offline during examples, tests, vignettes, and checks.
Browser and Shiny entry points use `open = FALSE` or explicit launch functions
so automated checks do not open a browser.

## Installation

Install the development version from GitHub:

```r
remotes::install_github("MohammedAliSharafuddin/surveyframe")
```

Optional packages are only needed for selected features:

```r
install.packages(c("shiny", "psych", "googlesheets4", "digest", "MASS", "nnet"))
```

Syntax generation works without installing `lavaan` or `seminr`. Install those
packages when you want to fit the generated CFA, CB-SEM, or PLS-SEM models.

## Documentation workflow

Start with:

1. A complete surveyframe workflow
2. Building a survey instrument
3. Analysing survey responses
4. Scale reliability and validity
5. EFA, CFA, CB-SEM, and PLS-SEM syntax generation
6. SurveyBuilder GUI overview

## Basic instrument

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
  )
)

instr <- validate_sframe(instr)
write_sframe(instr, tempfile(fileext = ".sframe"))
```

`write_sframe()` validates the instrument and writes the validated object,
including the validation flag and any saved model specifications.

## Response import and descriptives

```r
responses <- data.frame(
  respondent_id = paste0("R", 1:5),
  sat_1 = c(4, 5, 3, 4, NA),
  sat_2 = c(5, 4, 3, 4, 5),
  sat_3 = c(4, 5, 2, 4, 4),
  visitor_type = c("first_time", "repeat", "first_time", "repeat", "first_time")
)

resp <- read_responses(
  responses,
  instr,
  respondent_id = "respondent_id",
  strict = FALSE
)

score_scales(resp, instr)
descriptives_report(resp, variables = c("sat_1", "sat_2", "sat_3"))
missing_data_report(resp, instr)
```

## Role-based analysis plans

Analysis plans use role-based variable assignment. Earlier `.sframe` files
using `variables` and `test` fields remain compatible.

```r
instr$analysis_plan <- list(
  list(
    id = "RQ1",
    research_question = "Are the satisfaction items associated?",
    family = "association",
    method = "correlation_spearman",
    roles = list(x = "sat_1", y = "sat_2"),
    options = list(alpha = 0.05),
    status = "valid_plan",
    requires_data = TRUE
  )
)

run_analysis_plan(resp, instr)
```

Supported method IDs include descriptives, missing data, quality checks,
reliability, EFA readiness and solutions, CFA/CB-SEM/PLS-SEM syntax,
chi-square, Fisher's exact test, McNemar, Cochran's Q, t-tests, Mann-Whitney,
Wilcoxon, one- and two-way ANOVA, ANCOVA, repeated-measures ANOVA, Kruskal-
Wallis, Friedman, Pearson/Spearman/Kendall correlations, partial correlations,
linear and logistic regression, mediation, and moderation.

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
    sf_construct(
      "SAT",
      "Satisfaction",
      c("sat_1", "sat_2"),
      mode = "composite"
    ),
    sf_construct(
      "LOY",
      "Loyalty",
      "sat_3",
      mode = "single_item"
    )
  ),
  paths = list(
    sf_path("SAT", "LOY")
  ),
  options = list(bootstrap = 5000)
)

seminr_syntax(pls_model)
```

## Reporting

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

Use `launch_builder()` as the standalone questionnaire builder,
`launch_studio()` as the workflow hub, and `launch_dashboard()` as the
read-only response explorer. Demo launchers are available for training:

```r
launch_builder_demo(open = FALSE)
# launch_studio_demo()
# launch_dashboard_demo()
```

Interactive functions such as `launch_builder(open = TRUE)`,
`launch_studio()`, `render_survey()`, and `launch_dashboard()` are available
for manual use. Tests and examples avoid opening browsers.

## v0.4 scope

MCDM and DEMATEL fall outside v0.3 scope. These methods are scheduled for
v0.4.

## Citation

```r
citation("surveyframe")
```

## License

MIT. See `LICENSE`.
