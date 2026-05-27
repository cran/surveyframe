## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(surveyframe)

## ----choices------------------------------------------------------------------
agree5 <- sf_choices(
  id = "agree5",
  values = 1:5,
  labels = c(
    "Strongly disagree",
    "Disagree",
    "Neither agree nor disagree",
    "Agree",
    "Strongly agree"
  )
)

yes_no <- sf_choices(
  id = "yes_no",
  values = c("yes", "no"),
  labels = c("Yes", "No")
)

## ----items--------------------------------------------------------------------
intro <- sf_item(
  "intro",
  "Study introduction",
  type = "section_break",
  section_intro = "Please answer the following questions about the service."
)

visit_again <- sf_item(
  "visit_again",
  "Would you use this service again?",
  type = "single_choice",
  required = TRUE,
  choice_set = "yes_no"
)

sat_1 <- sf_item(
  "sat_1",
  "The service met my expectations.",
  type = "likert",
  required = TRUE,
  choice_set = "agree5",
  scale_id = "satisfaction"
)

sat_2 <- sf_item(
  "sat_2",
  "The service was easy to use.",
  type = "likert",
  required = TRUE,
  choice_set = "agree5",
  scale_id = "satisfaction"
)

sat_3 <- sf_item(
  "sat_3",
  "The service was frustrating to use.",
  type = "likert",
  required = TRUE,
  choice_set = "agree5",
  scale_id = "satisfaction",
  reverse = TRUE
)

age <- sf_item(
  "age",
  "Age in years",
  type = "numeric",
  required = FALSE
)

comments <- sf_item(
  "comments",
  "Any other comments?",
  type = "textarea",
  required = FALSE
)

attention <- sf_item(
  "attention",
  "For quality control, please select Agree.",
  type = "single_choice",
  required = TRUE,
  choice_set = "agree5"
)

## ----scale--------------------------------------------------------------------
satisfaction <- sf_scale(
  id = "satisfaction",
  label = "Satisfaction",
  items = c("sat_1", "sat_2", "sat_3"),
  method = "mean",
  min_valid = 2,
  reverse_items = "sat_3"
)

## ----checks-------------------------------------------------------------------
attention_check <- sf_check(
  id = "attention_agree",
  item_id = "attention",
  type = "attention",
  pass_values = 4,
  fail_action = "flag",
  label = "Instructional attention check"
)

## ----assemble-----------------------------------------------------------------
instr <- sf_instrument(
  title = "Service Feedback Survey",
  version = "0.3.0",
  description = "Short example instrument for a service feedback study.",
  authors = "Research team",
  languages = "en",
  components = list(
    agree5, yes_no,
    intro, visit_again, sat_1, sat_2, sat_3, age, comments, attention,
    satisfaction, attention_check
  )
)

validation <- validate_sframe(instr, strict = FALSE)
validation$valid
validation$problems

## ----strict-------------------------------------------------------------------
instr <- validate_sframe(instr)
instr$meta$validated

## ----roundtrip----------------------------------------------------------------
path <- tempfile(fileext = ".sframe")
write_sframe(instr, path, overwrite = TRUE)

loaded <- read_sframe(path)
inherits(loaded, "sframe")
loaded$meta$title

## ----gui, eval = FALSE--------------------------------------------------------
# launch_builder()
# launch_builder_demo(open = FALSE)

