# surveyframe 0.3.0

## New features

### Analysis planning, survey statistics, and model syntax

* Added a role-based analysis-plan structure while preserving old
  `variables`/`test` analysis blocks. New plans can store `family`, `method`,
  `roles`, `options`, `hypotheses`, `decision_rule`,
  `reporting_references`, `status`, and `requires_data`.
* Added common survey analysis helpers: `descriptives_report()`,
  `missing_data_report()`, `assumption_report()`, `posthoc_report()`,
  `validity_report()`, and `sample_size_plan()`.
* Expanded `run_analysis_plan()` to dispatch the v0.3 method registry,
  including descriptives, missing data, sparse-table tests, related-sample
  tests, Kendall and partial correlations, two-way ANOVA, ANCOVA, repeated
  ANOVA, ordinal and multinomial logistic regression, mediation, moderation,
  and model-syntax output.
* Added a model specification layer: `sf_construct()`, `sf_path()`,
  `sf_covariance()`, `sf_indirect()`, `sf_model()`, `validate_model()`,
  `model_json()`, `add_model()`, `efa_solution()`, `efa_syntax()`,
  `cfa_lavaan_syntax()`, `sem_lavaan_syntax()`, `seminr_syntax()`, and
  `model_report_template()`. Syntax generation does not require `lavaan` or
  `seminr`.
* `cfa_syntax()` remains available as a backward-compatible wrapper around
  `cfa_lavaan_syntax()`.
* SurveyBuilder Analyse mode now uses a three-panel Plan/Run/Report workspace
  with variable metadata badges, role-based variable assignment, method
  options, output preview, reporting references, and a table-based model
  builder. Significance level is shown only for inferential methods.

### Static HTML survey export

* Added `export_static_survey()`. This produces a single, self-contained
  HTML file that runs the survey in any modern browser without a Shiny
  server or an internet connection. All thirteen item types are fully
  rendered (Likert, single choice, multiple choice, matrix, numeric, text,
  long text, date, slider, rating, ranking, section break, text block).
  Branching logic, required-field validation, a progress bar, welcome and
  thank-you pages are all handled in client-side JavaScript. On submission
  the browser downloads a per-respondent CSV file. An optional
  `endpoint_url` argument adds a parallel JSON POST to any serverless
  endpoint (Google Apps Script, Netlify function, etc.).

  The exported file is suitable for hosting on GitHub Pages, Netlify, or
  any static file server, and can also be shared directly as an e-mail
  attachment and opened from disk.

  The SHA-256 hash written into `.sframe` files by `write_sframe()` and by
  the SurveyBuilder HTML is computed using the same canonicalisation
  algorithm, so instruments round-trip correctly between the browser and R.

### Interactive response dashboard

* Added `launch_dashboard()`. Opens a five-panel Shiny dashboard for
  exploring collected response data alongside the instrument definition,
  without modifying either. The panels are:

  | Panel | Content |
  |---|---|
  | Overview | Response count, date range, instrument metadata |
  | Items | Per-item bar charts, histograms, and frequency tables |
  | Scales | Scale score distributions with mean overlay |
  | Quality | Attention-check pass rates |
  | Raw data | Scrollable response table with CSV download |

  When called without arguments the dashboard loads the bundled tourism
  services demo. When called with a user-supplied instrument and no
  `responses` argument, it opens in metadata-only mode showing instrument
  structure.

* Added `sframe_demo_data()`, `sframe_input_types_demo_data()`,
  `launch_builder_demo()`, `launch_studio_demo()`, and
  `launch_dashboard_demo()` for CRAN-safe examples, training, and local GUI
  testing.

* Added a bundled input-types demo instrument and simulated response dataset
  for testing SurveyBuilder, SurveyStudio, the dashboard, and all supported
  item controls.

* `launch_studio()` now accepts preloaded instruments, response data frames,
  CSV response paths, initial screen selection, host, port, and browser
  control. SurveyStudio reads these preloaded values during startup.

### Shiny survey module

* Added `survey_module_ui()` and `survey_module_server()`. These allow a
  survey to be embedded inside a larger Shiny application as a first-class
  module. `survey_module_server()` returns a `reactive` that holds `NULL`
  until the form is submitted, then returns the response as a named list
  keyed by item ID.

  ```r
  ui <- fluidPage(survey_module_ui("s1"))
  server <- function(input, output, session) {
    resp <- survey_module_server("s1", instrument = instr)
    observeEvent(resp(), { saveRDS(resp(), "response.rds") })
  }
  ```

  An optional `on_submit` callback fires immediately on submission, before
  any `observeEvent()` elsewhere in the app.

### Extended analysis plan tests

`run_analysis_plan()` now implements four additional tests used by the
SurveyBuilder's test dropdown:

* `anova_one`: One-way ANOVA with eta-squared effect size. When the
  result is significant and there are more than two groups, Tukey HSD
  post-hoc output is included in the result object.
* `t_test_pair`: Paired-samples t-test with Cohen's d_z.
* `wilcoxon_pair`: Wilcoxon signed-rank test with r effect size.
* `regression_logistic_binary`: Binary logistic regression with
  McFadden R-squared and an overall model chi-square test. The full
  coefficient table is returned for interpretation.

All four runners produce an APA-formatted summary string and an
interpretation `prompt` field to guide write-up.

## Bug fixes

* `write_sframe()` validates the instrument and writes the validated object,
  preserving `meta$validated = TRUE` in the saved `.sframe` file.
* `.sframe` serialisation now includes a `models` field and continues to read
  older `.sframe` files where `models` is absent.
* `read_responses()` no longer requires display-only items such as
  `section_break` and `text_block` to appear as response columns.
* `validate_sframe()` now checks model references, analysis-plan roles,
  invalid model IDs, duplicate model IDs, and model indicator/path integrity.
* The package title and description were tightened for CRAN submission and
  avoid overclaiming.

* `launch_builder(open = TRUE)` opens the SurveyBuilder HTML in the system's
  default browser via `utils::browseURL()`.
* `R/studio_builder.R` contains three fully implemented internal
  functions (`sframe_builder_empty_state`, `sframe_builder_state_from_instrument`,
  `sframe_builder_validate_draft`) used by SurveyStudio startup and draft
  validation.
* SHA-256 hashing in the SurveyBuilder HTML includes a pure-JavaScript
  fallback for environments where `crypto.subtle` is unavailable on
  `file://` origins, including common Firefox `file://` configurations.
  Saving a `.sframe` file from the builder now always succeeds.
* The SurveyBuilder's `rqSuggest` box now appears with an icon and a
  plain-language recommendation when two or more variables are selected in
  the RQ modal.
* The undo and redo buttons in the SurveyBuilder topbar are now correctly
  disabled when their respective history stacks are empty.

## Security hardening

* `export_google_sheet()` now writes Google Apps Script using JSON-encoded
  JavaScript literals instead of interpolating instrument metadata directly
  into executable code. The generated endpoint also rejects missing,
  over-large, and non-object JSON POST bodies.
* SurveyStudio upload handlers now validate uploaded `.sframe` and `.csv`
  files by extension, size, and text-content checks before passing them to
  import functions.
* `read_sframe()` now validates the top-level `.sframe` payload structure
  before hash verification and object reconstruction.
* Internal HTML report generation now applies escaping consistently to
  report titles, instrument metadata, citations, and effect labels.
* Quarto report rendering now cleans temporary render directories and RDS
  files with `on.exit()` even when rendering fails.

## Documentation

* `validate_sframe()`, `score_scales()`, `codebook_report()`, `cfa_syntax()`,
  and `launch_builder(open = FALSE)` have fully runnable examples.
* Reworked the vignette set into a coherent workflow covering instrument
  building, response analysis, reliability and validity, EFA/CFA/SEM/PLS
  syntax generation, and GUI usage.
* Added `cran-comments.md` with platform list, dependency justification,
  bundled-asset description, and a `\dontrun{}` use justification table.

* `launch_builder_demo()` now injects the demo instrument JSON directly into
  a temporary copy of `survey_builder.html`. The demo questions, scales, and
  analysis plan are visible immediately on launch — no manual Load .sframe
  step is needed.
* `launch_studio_demo()` and `launch_dashboard_demo()` now pass
  `launch.browser = TRUE` so the browser always opens automatically.
* `inst/shiny/dashboard/app.R`: replaced `library(shiny)` with a
  `requireNamespace` check and explicit `shiny::` namespace prefixes for CRAN
  policy compliance.
* `db_quality_ui()`: `class = flag_class` added to `tags$tr()` so quality
  rows are colour-coded by flag status.
* `db_data_ui()`: download-button background colour now uses
  `sprintf("...%s...", THEME)` instead of `background:var(--cp,#2563eb)` so
  the button respects the active dashboard theme.
* `db_overview_ui()`: date parsing is now delegated to `dashboard_parse_date()`
  for robust, error-free date display.
* `render_report()` and `render_results()`: HTML report tables now use APA
  formatting — horizontal rules only, no vertical borders, no row shading.
  A significance footnote is appended automatically when a p-value column is
  detected.
* `demo/survey.R`: complete UX rewrite with section headers, contextual pause
  prompts, "In the browser: ->" guidance, `ask_yn()` yes/no prompts, and a
  conversational mode for static survey export. All output is ASCII-safe.
* `dashboard_parse_date()` completely rewritten. No longer throws
  `Error in as.POSIXlt.character: character string is not in a standard
  unambiguous format`. Now tries six explicit format templates in priority
  order (ISO 8601 Z-suffix, ISO 8601 no-tz, space datetime, date-only,
  UK dd/mm/yyyy, US mm/dd/yyyy) with `tryCatch` on each, plus a wrapped
  fallback. Non-matching strings return `NA` instead of an error.
* Dashboard Items and Scales tabs: `outputOptions(output, "item_chart",
  suspendWhenHidden = FALSE)` and `outputOptions(output, "scale_chart",
  suspendWhenHidden = FALSE)` added. Response distribution and Scale score
  distribution charts now render as soon as the user switches to those tabs
  instead of appearing blank.

## Future direction: v0.4

* MCDM and DEMATEL fall outside v0.3 scope. Planned v0.4 work includes
  MCDM input fields, AHP pairwise-comparison matrices, DEMATEL direct
  influence matrices, TOPSIS/VIKOR/PROMETHEE/ELECTRE planning, MCDM data
  validation and consistency checks, DEMATEL thresholding and causal diagram
  export, advanced SEM and PLS-SEM execution, higher-order constructs,
  multi-group SEM planning, a diagram-based model builder, complex survey
  design weighting, JASP/jamovi-friendly exports, and report-writer
  integration after the data and model schemas stabilise.

---

# surveyframe 0.1.0

* Initial release.
* Core S3 object system: `sf_instrument()`, `sf_item()`, `sf_choices()`,
  `sf_scale()`.
* Serialisation: `write_sframe()`, `read_sframe()` with SHA-256 integrity
  checking.
* Shiny survey renderer: `render_survey()`.
* Static SurveyBuilder HTML: `launch_builder()`.
* Response reader: `read_responses()`.
