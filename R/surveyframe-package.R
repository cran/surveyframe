# The \if{latex} line in @description is for the PDF manual. Inline code is
# set in a typewriter font that cannot be hyphenated, so a long function name
# near the end of a line ran past the right margin, 83 times in 0.4.2's
# manual. This page is typeset first, which is why it is no longer marked
# internal, and \global carries the looser line breaking to every page after
# it. HTML help and pkgdown ignore the line.
#' surveyframe: Survey Instrument Workflows for R
#'
#' @description
#' \if{latex}{\out{\global\emergencystretch=3em\global\tolerance=3000\global\hbadness=10000}}
#' surveyframe defines a survey instrument as a first-class R object and
#' supports a complete workflow from questionnaire design through data
#' collection, quality checking, scoring, psychometric diagnostics, and
#' reproducible reporting. The package covers static HTML survey export,
#' an embeddable Shiny survey module, an interactive response dashboard,
#' a role-based analysis planner with pre-declared research questions, common
#' survey statistics with small-sample alternatives (Hodges-Lehmann,
#' pseudomedian, exact odds-ratio, and Firth logistic regression), multi-
#' criteria decision analysis (AHP, ANP, DEMATEL, VIKOR, MOORA, SMART,
#' WASPAS, PROMETHEE, ELECTRE, and TOPSIS), and model syntax planning for
#' EFA, CFA, CB-SEM, and PLS-SEM.
#'
#' ## Core workflow
#'
#' 1. **Design** an instrument with [launch_builder()] or [sf_instrument()] and
#'    its component constructors: [sf_item()], [sf_choices()], [sf_scale()],
#'    [sf_branch()], [sf_check()].
#' 2. **Validate and save** with [validate_sframe()] and [write_sframe()].
#'    `validate_sframe()` returns an `sframe_validation` diagnostic rather
#'    than the instrument itself. Recover a validated instrument with
#'    [as_sframe()].
#' 3. **Deploy** a Shiny survey with [render_survey()].
#' 4. **Load responses** with [read_responses()] or [read_sheet_responses()].
#' 5. **Check quality** with [quality_report()].
#' 6. **Score and analyse** with [score_scales()], [descriptives_report()],
#'    [missing_data_report()], [reliability_report()], [item_report()],
#'    [efa_report()], [cfa_syntax()], and [run_analysis_plan()], which also
#'    runs any decision-analysis blocks in the plan.
#' 7. **Report** with [codebook_report()], [render_report()], and
#'    [render_results()].
#'
#' ## The instrument object
#'
#' The workflow runs on an `sframe` object. It is the single source of truth
#' for item definitions, scale structure, reverse-coding keys, branching
#' rules, check specifications, analysis plans, and optional model
#' specifications. Accessors such as [sf_meta()], [sf_items()], [sf_scales()],
#' [sf_plan()], and [sf_models()] read its parts without reaching into the
#' object directly.
#'
#' Some helpers work on plain vectors, for use beside that workflow or on
#' their own: the text helpers such as [term_frequency()], and the interval
#' helpers [bootstrap_ci()], [cohens_d_ci()], [cramers_v_ci()] and
#' [eta_sq_ci()].
#'
#' ## A first session
#'
#' [sframe_demos()] lists 22 worked demos, each one instrument, its responses
#' and the results surveyframe produced. `sframe_demo("two_group")` loads one,
#' and `sframe_demo_qmd("two_group")` writes a notebook to edit.
#' `vignette("learn-by-example")` teaches from the same library.
#'
#' ## How functions are named
#'
#' Three families, which the prefix tells apart.
#'
#' * `sf_` builds or reads the instrument object model: the constructors
#'   [sf_item()] and [sf_scale()], the accessors [sf_items()] and [sf_plan()],
#'   and the replacement forms such as `sf_plan<-`.
#' * `sframe_` covers everything the package adds around that object: the
#'   plots such as [sframe_plot_reliability()], the demo library through
#'   [sframe_demos()], the decision helpers, and the builder's own state.
#' * The workflow verbs carry no prefix, because they name the step a
#'   researcher is taking: [validate_sframe()], [score_scales()],
#'   [run_analysis_plan()], [render_report()], and the `_report()` family.
#'
#' The prefixes group functions; they do not pair them. No name stem appears
#' under both, so there is no `sframe_` twin of an `sf_` function to look for.
#'
#' ## File format
#'
#' Instruments are stored as UTF-8 JSON files with the `.sframe` extension.
#' Each file includes a SHA-256 integrity hash for reproducibility auditing.
#'
"_PACKAGE"

## usethis namespace: start
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom rlang abort warn arg_match check_installed %||%
#' @importFrom openssl sha256
#' @importFrom stats cor sd var complete.cases density
#' @importFrom utils capture.output
## usethis namespace: end
NULL
