# sf_item.R

#' Define a survey item
#'
#' Creates a single survey item object for inclusion in an `sframe` instrument.
#' Items are the atomic units of a survey instrument. Every item must have a
#' unique `id` within the instrument it is added to.
#'
#' @param id Character. A unique identifier for this item. Used as the column
#'   name in response data. Must contain only letters, numbers, and `_` characters.
#' @param label Character. The question text or content displayed to the
#'   respondent.
#' @param type Character. The response type. One of `"likert"`,
#'   `"single_choice"`, `"multiple_choice"`, `"numeric"`, `"text"`,
#'   `"textarea"`, `"date"`, `"matrix"`, `"slider"`, `"ranking"`, `"rating"`,
#'   `"section_break"`, or `"text_block"`.
#' @param required Logical. Whether the respondent must answer this item.
#' @param choice_set Character or NULL. The `id` of a choice set defined with
#'   [sf_choices()].
#' @param scale_id Character or NULL. The `id` of the scale this item belongs to.
#' @param reverse Logical. Whether this item is reverse-coded within its scale.
#' @param help Character or NULL. Help text displayed beneath the question.
#' @param placeholder Character or NULL. Placeholder text for text inputs.
#' @param matrix_items Character vector or NULL. Row labels for `"matrix"` type.
#' @param slider_min Numeric or NULL. Minimum value for `"slider"` type.
#' @param slider_max Numeric or NULL. Maximum value for `"slider"` type.
#' @param slider_step Numeric or NULL. Step size for `"slider"` type.
#' @param rating_max Integer or NULL. Maximum rating for `"rating"` type.
#' @param rating_icon Character or NULL. Icon type: `"star"` or `"heart"`.
#' @param section_intro Character or NULL. Intro text for `"section_break"` type.
#' @param page Integer or NULL. Page number for multi-page surveys.
#'
#' @return An object of class `sf_item` (a named list).
#' @export
#' @seealso [sf_instrument()], [sf_choices()], [sf_scale()]
#'
#' @examples
#' item <- sf_item(
#'   id = "sat_overall", label = "Overall, how satisfied are you?",
#'   type = "likert", required = TRUE, choice_set = "agree5",
#'   scale_id = "satisfaction"
#' )
#'
#' sec <- sf_item("sec_1", "Demographic Information", type = "section_break",
#'                section_intro = "Please answer the following questions.")
sf_item <- function(
    id,
    label,
    type          = c("likert", "single_choice", "multiple_choice",
                      "numeric", "text", "textarea", "date",
                      "matrix", "slider", "ranking", "rating",
                      "section_break", "text_block"),
    required      = FALSE,
    choice_set    = NULL,
    scale_id      = NULL,
    reverse       = FALSE,
    help          = NULL,
    placeholder   = NULL,
    matrix_items  = NULL,
    slider_min    = NULL,
    slider_max    = NULL,
    slider_step   = NULL,
    rating_max    = NULL,
    rating_icon   = NULL,
    section_intro = NULL,
    page          = NULL
) {
  type <- rlang::arg_match(type)

  structure(
    list(
      id = id,
      label = label,
      type = type,
      required = required,
      choice_set = choice_set,
      scale_id = scale_id,
      reverse = reverse,
      help = help,
      placeholder = placeholder,
      matrix_items = matrix_items,
      slider_min = slider_min,
      slider_max = slider_max,
      slider_step = slider_step,
      rating_max = rating_max,
      rating_icon = rating_icon,
      section_intro = section_intro,
      page = page
    ),
    class = "sf_item"
  )
}
