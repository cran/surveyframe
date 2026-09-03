<img src="man/figures/readme-logo.png" align="right" width="220" alt="surveyframe" style="margin-top: 2rem;" />

# surveyframe

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/surveyframe)](https://CRAN.R-project.org/package=surveyframe)
[![R-CMD-check](https://github.com/MohammedAliSharafuddin/surveyframe/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MohammedAliSharafuddin/surveyframe/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

> **Just here to verify a `.sframe` file's integrity hash or read its
> amendment log?**
> **[Verify a file now](https://mohammedalisharafuddin.github.io/surveyframe/verify/)**.
> No R, no install. It runs entirely in your browser and nothing is
> uploaded.

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

Because the plan and the model live inside the instrument, you never have to go
back and match up questions, variables, and tests by hand. Say you're comparing
satisfaction between first-time and repeat visitors: write that comparison into
the plan once, alongside the questions, and it stays linked to the right
variables from then on. When responses arrive, running the plan is a single
step: each result comes back ready to write up, with a plot, a table, an APA
statistic, an effect size where it applies, a writing prompt, and the reference
that supports it. Plots switch between a colour palette for on-screen use and a
black-and-white palette for print, both checked against WCAG contrast rules, so
the same run produces a journal-ready figure with no separate step.

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

`surveyframe` is not a replacement for whatever collection tool your
institution already has approved: Qualtrics, REDCap, Google Forms, or a
paper form typed up afterward. It reads response data as a plain CSV or
`data.frame` from any of them: export from your collection tool, rename
columns to match your instrument's item IDs (or build the instrument to
match the export), and load it. If you have collected responses in a CSV
or Google Sheet and want to start from the analysis step, build a minimal
instrument that matches your column names and load the data directly:

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
3. Deploying a survey and collecting responses on free hosting
4. Analysing survey responses: running the plan
5. Scale reliability and validity
6. EFA, CFA, CB-SEM, and PLS-SEM syntax generation
7. The visual workflow: SurveyBuilder, SurveyStudio, and the dashboard
8. Learn by example: 22 small surveys, the bundled demo library
9. Multi-criteria decision analysis: AHP, TOPSIS, and 8 other methods
10. Small-sample inference
11. Text and open-ended response analysis

Read all eleven vignettes inside R with:

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

write_sframe(instr, tempfile(fileext = ".sframe"))

# See the instrument as a survey a respondent would fill in:
export_static_survey(instr, open = FALSE)
```

`write_sframe()` validates the instrument and writes the validated object,
including the validation flag, the analysis plan, and any saved model
specifications. `export_static_survey()` renders it as a self-contained
HTML survey, the same function covered in "Visual tools" below.

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
to export the `.sframe` file and model syntax. It runs no statistics.
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

## Roadmap

0.4.0 (CRAN, 2026-08-20) added three capability themes:

- **Small-sample survey helpers**, validated by a simulation study:
  Hodges-Lehmann and paired-Wilcoxon pseudomedian estimators, exact
  Fisher odds-ratio intervals, and Firth logistic regression.
- **10 multi-criteria decision methods** (TOPSIS, AHP, ANP, DEMATEL,
  VIKOR, MOORA, SMART, WASPAS, PROMETHEE, ELECTRE), with 2 new
  question types for collecting judgements, weight-sensitivity
  analysis, and declared conjoint designs.
- **Text and open-ended response analysis**, 9 methods: term and
  n-gram frequency, keyword in context, co-occurrence networks,
  sentiment, document-feature matrices, and topic modelling via LDA
  or a structural topic model.

A disclosed-amendment and Git-linked provenance mechanism shipped
alongside, on top of the existing `.sframe` integrity hash. Full
detail in `NEWS.md`.

0.4.1 focuses on stability: fixes found by using surveyframe on real
instruments, no new capability theme. 0.4.2 continues in the same
direction.

## When to use surveyframe, and when not to

surveyframe is a fit when a study's analysis has to be decided before
data collection: a scale to validate, a pre-registered hypothesis test,
a measurement model to fit, or an audit trail showing the plan wasn't
changed after seeing results. Its core strength is a pre-declared,
integrity-checked analysis plan bound to the instrument itself. It also
interoperates with [survey](https://cran.r-project.org/package=survey)
and [srvyr](https://cran.r-project.org/package=srvyr), the standard
tools for weighting and variance estimation on data from a complex
probability sample.

Haven't decided the analysis yet? Collect first with any web-form tool,
Google Forms, Qualtrics, REDCap, or the R package
[surveydown](https://github.com/surveydown-dev/surveydown), and add the plan when you're
ready: surveyframe reads exported CSV data from any of them (see
"Already have data?" above).

**If this package is ever archived by CRAN**, the GitHub repository
remains the canonical source: `remotes::install_github("MohammedAliSharafuddin/surveyframe")`.
Each CRAN release is also deposited to Zenodo with its own DOI, so a
specific version stays citable and retrievable independently of both
CRAN's and GitHub's continued availability.

## FAQ

**Is surveyframe a replacement for Qualtrics, Google Forms, or REDCap?**
No. Those collect responses with no plan required up front. surveyframe
declares the analysis plan before collection, and reads exported CSV
data from any of them once the plan is added (see "Already have data?"
above).

**Does surveyframe do survey weighting or complex-sample variance
estimation?** No. [survey](https://cran.r-project.org/package=survey)
and [srvyr](https://cran.r-project.org/package=srvyr) are the standard R
tools for stratified, clustered, or weighted samples. surveyframe's
instruments interoperate with that workflow instead of duplicating it.

**Can I use surveyframe with data I've already collected?** Yes. Build
a minimal instrument that matches your column names, then load your CSV
or `data.frame` directly with `read_responses()`. See "Already have
data?" above for a worked example.

**What is a pre-declared analysis plan, and why does it matter?** A
list of research questions written into the instrument at design time,
each bound to a technique and to the variables that fill its roles,
before any response arrives. It removes matching questions, variables,
and tests up by hand, and it's the basis for the audit trail showing the
plan wasn't changed after seeing results.

**What happens if surveyframe is ever archived by CRAN?** The GitHub
repository stays the canonical source
(`remotes::install_github("MohammedAliSharafuddin/surveyframe")`), and
every CRAN release is separately deposited to Zenodo with its own DOI.

## Citation

```r
citation("surveyframe")
```

## Related resources

- Sharafuddin, M. A., Jaleel, A. A., and Madhavan, M. (2026).
  *Quantitative Analysis with Small Samples: A Practical Guide for Students
  and Early-Career Researchers* (Version 0.1.0) [Book]. Zenodo.
  <https://doi.org/10.5281/zenodo.20221929>. A companion textbook on
  statistical inference when sample sizes are small, also available at
  <https://flairmi.com/textbooks/smallsamplelab.html>. It describes the
  small-sample methods that surveyframe added in 0.4.0 and when to prefer
  each one.
- Sharafuddin, M. A. (2026). *surveyframe: A Pre-Declared, Reproducible
  Framework for Multi-Criteria Decision Analysis in Survey Research*.
  Manuscript in preparation for Computo. Describes the 10 MCDM methods
  surveyframe added in 0.4.0.

## License

MIT. See `LICENSE`.
