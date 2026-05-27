# sf_instrument.R

#' Create a survey instrument object
#'
#' Assembles a survey instrument from its component objects. This is
#' the top-level constructor for the `sframe` class. All other constructors
#' ([sf_item()], [sf_choices()], [sf_scale()], [sf_branch()], [sf_check()])
#' produce components that are passed into this function via `components`.
#'
#' @param title Character. The title of the survey instrument.
#' @param version Character. A semantic version string. Defaults to `"0.1.0"`.
#' @param description Character or NULL. A brief description of the instrument
#'   and its intended population or purpose.
#' @param authors Character vector or NULL. Author names, used in codebooks
#'   and reports.
#' @param languages Character vector. Language codes for the instrument.
#'   Defaults to `"en"`. Multi-language support is planned for a later release.
#' @param components List. A list of component objects created by the
#'   constructor family: [sf_item()], [sf_choices()], [sf_scale()],
#'   [sf_branch()], and [sf_check()]. Components are sorted by class
#'   automatically. Supply components created by the surveyframe constructors.
#' @param render List or NULL. Optional rendering hints passed to
#'   [render_survey()], such as theme colour or progress bar visibility.
#' @param analysis_plan List. Optional pre-planned analysis blocks created in
#'   the HTML SurveyBuilder Analyse mode.
#' @param models List. Optional model specifications created with [sf_model()]
#'   or imported from a `.sframe` file.
#'
#' @return An object of class `sframe` with slots `meta`, `items`, `choices`,
#'   `scales`, `branching`, `checks`, `analysis_plan`, `models`, and `render`.
#' @export
#' @seealso [sf_item()], [sf_choices()], [sf_scale()], [sf_branch()],
#'   [sf_check()], [validate_sframe()], [write_sframe()]
#'
#' @examples
#' choices <- sf_choices("agree5", 1:5,
#'   c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
#'
#' item1 <- sf_item("sat_1", "The service met my expectations.",
#'                  type = "likert", choice_set = "agree5",
#'                  scale_id = "sat", required = TRUE)
#' item2 <- sf_item("sat_2", "I would recommend this service.",
#'                  type = "likert", choice_set = "agree5",
#'                  scale_id = "sat", required = TRUE)
#'
#' scale <- sf_scale("sat", "Satisfaction", items = c("sat_1", "sat_2"))
#'
#' instr <- sf_instrument(
#'   title      = "Service Quality Survey",
#'   version    = "1.0.0",
#'   components = list(choices, item1, item2, scale)
#' )
#' print(instr)
sf_instrument <- function(
    title,
    version     = "0.1.0",
    description = NULL,
    authors     = NULL,
    languages   = "en",
    components  = list(),
    render      = NULL,
    analysis_plan = list(),
    models = list()
) {
  # Sort components by class into named slots
  items     <- Filter(function(x) inherits(x, "sf_item"),    components)
  choices   <- Filter(function(x) inherits(x, "sf_choices"), components)
  scales    <- Filter(function(x) inherits(x, "sf_scale"),   components)
  branching <- Filter(function(x) inherits(x, "sf_branch"),  components)
  checks    <- Filter(function(x) inherits(x, "sf_check"),   components)

  # Keep the component list limited to known surveyframe classes.
  known_classes <- c("sf_item", "sf_choices", "sf_scale", "sf_branch", "sf_check")
  unknown <- Filter(
    function(x) !any(vapply(known_classes, function(cls) inherits(x, cls), logical(1))),
    components
  )
  if (length(unknown) > 0) {
    sframe_abort_validation(
      paste0(
        "All components must be created by the sf_ constructor family. ",
        length(unknown),
        " unrecognised component(s) found."
      ),
      instrument_title = title
    )
  }

  instrument <- structure(
    list(
      meta = list(
        title       = title,
        version     = version,
        description = description,
        authors     = authors,
        languages   = languages,
        validated   = FALSE,
        created_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      ),
      items     = items,
      choices   = choices,
      scales    = scales,
      branching = branching,
      checks    = checks,
      analysis_plan = analysis_plan %||% list(),
      models    = models %||% list(),
      render    = render %||% list()
    ),
    class = "sframe"
  )

  instrument
}
