## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(surveyframe)

## ----choices------------------------------------------------------------------
likert5 <- sf_choices(
  "likert5",
  values = 1:5,
  labels = c("Strongly disagree", "Disagree",
             "Neither agree nor disagree", "Agree", "Strongly agree")
)

visitor <- sf_choices("visitor", c("first_time", "repeat"),
                      c("First-time visitor", "Repeat visitor"))

## ----items--------------------------------------------------------------------
intro <- sf_item(
  "intro", "About your trip",
  type          = "section_break",
  section_intro = "Please rate your experience of digital booking and your visit."
)

dmpv_1 <- sf_item("dmpv_1", "The destination's online content was trustworthy.",
  type = "likert", required = TRUE, choice_set = "likert5", scale_id = "DMPV")
dmpv_2 <- sf_item("dmpv_2", "The online information was consistent across platforms.",
  type = "likert", required = TRUE, choice_set = "likert5", scale_id = "DMPV")
dmpv_3 <- sf_item("dmpv_3", "The digital channels offered good value for money.",
  type = "likert", required = TRUE, choice_set = "likert5", scale_id = "DMPV")
dmpv_4 <- sf_item("dmpv_4", "I was aware of the destination through digital channels.",
  type = "likert", required = TRUE, choice_set = "likert5", scale_id = "DMPV")

ts_1 <- sf_item("ts_1", "The trip met my expectations.",
  type = "likert", required = TRUE, choice_set = "likert5", scale_id = "TS")
ts_2 <- sf_item("ts_2", "My overall travel experience was satisfying.",
  type = "likert", required = TRUE, choice_set = "likert5", scale_id = "TS")
ts_3 <- sf_item("ts_3", "I felt comfortable at the destination.",
  type = "likert", required = TRUE, choice_set = "likert5", scale_id = "TS")

visitor_type <- sf_item("visitor_type", "I am a",
  type = "single_choice", required = TRUE, choice_set = "visitor")

attention <- sf_item("attention", "For quality control, please select Agree.",
  type = "single_choice", required = TRUE, choice_set = "likert5")

## ----scales-------------------------------------------------------------------
dmpv <- sf_scale("DMPV", "Digital marketing perceived value",
  items = c("dmpv_1", "dmpv_2", "dmpv_3", "dmpv_4"), method = "mean", min_valid = 3)

ts <- sf_scale("TS", "Tourist satisfaction",
  items = c("ts_1", "ts_2", "ts_3"), method = "mean", min_valid = 2)

## ----checks-------------------------------------------------------------------
attention_check <- sf_check(
  id = "attention_agree", item_id = "attention", type = "attention",
  pass_values = 4, fail_action = "flag", label = "Instructional attention check"
)

## ----branch-------------------------------------------------------------------
repeat_branch <- sf_branch(
  item_id    = "ts_3",
  depends_on = "visitor_type",
  operator   = "==",
  value      = "repeat",
  action     = "show"
)

## ----plan---------------------------------------------------------------------
analysis_plan <- list(
  list(id = "M1", research_question = "Is perceived value internally consistent?",
       family = "measurement", method = "reliability_alpha",
       roles = list(items = c("dmpv_1", "dmpv_2", "dmpv_3", "dmpv_4"))),
  list(id = "RQ1", research_question = "Do repeat visitors report higher satisfaction?",
       family = "group_comparison", method = "mann_whitney",
       roles = list(group = "visitor_type", outcome = "TS"),
       options = list(alpha = 0.05))
)

## ----assemble-----------------------------------------------------------------
instr <- sf_instrument(
  title       = "Digital marketing study (teaching slice)",
  version     = "1.0.0",
  description = "A slice of the published Thailand study for teaching the constructors.",
  authors     = "Research team",
  languages   = "en",
  components  = list(
    likert5, visitor,
    intro, dmpv_1, dmpv_2, dmpv_3, dmpv_4, ts_1, ts_2, ts_3,
    visitor_type, attention, dmpv, ts, attention_check, repeat_branch
  ),
  analysis_plan = analysis_plan
)

ts_model <- sf_model(
  id    = "ts_cfa",
  label = "Satisfaction measurement model",
  type  = "cfa",
  constructs = list(
    sf_construct("DMPV", "Perceived value", c("dmpv_1", "dmpv_2", "dmpv_3", "dmpv_4")),
    sf_construct("TS",   "Tourist satisfaction", c("ts_1", "ts_2", "ts_3"))
  )
)

instr <- add_model(instr, ts_model)

## ----validate-----------------------------------------------------------------
validate_sframe(instr, strict = FALSE)

## ----validate-explore---------------------------------------------------------
validation <- validate_sframe(instr, strict = FALSE)
sf_is_valid(validation)
sf_problems(validation)

## ----strict-------------------------------------------------------------------
instr <- as_sframe(validate_sframe(instr))
sf_meta(instr)$validated

## ----roundtrip----------------------------------------------------------------
path <- tempfile(fileext = ".sframe")
write_sframe(instr, path, overwrite = TRUE)

loaded <- read_sframe(path)
inherits(loaded, "sframe")
sf_meta(loaded)$title
length(sf_plan(loaded))
length(loaded$models)

## ----gui, eval = FALSE--------------------------------------------------------
# launch_builder()
# launch_builder_demo(open = FALSE)

