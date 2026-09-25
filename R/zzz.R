# R/zzz.R
# The message library(surveyframe) prints: how to try a demo, how to start,
# how to cite, and where to get help. It prints in interactive sessions only,
# so scripts, Quarto renders and R CMD check stay quiet, and it goes through
# packageStartupMessage() so suppressPackageStartupMessages() silences it.

.onAttach <- function(libname, pkgname) {
  if (!sframe_is_interactive()) return(invisible())
  packageStartupMessage(sframe_startup_message())
}

# interactive() behind a helper, so a test can drive both answers.
sframe_is_interactive <- function() interactive()

# The demo lines are commands a new user can copy and run as they stand.
# preview = TRUE strips the collector, so trying the demo sends nothing.
sframe_startup_message <- function() {
  paste0(
    "surveyframe ", utils::packageVersion("surveyframe"),
    ": survey instruments, from design to report.\n",
    "Try a demo (sframe_demos() lists all of them):\n",
    "  demo <- sframe_demo(\"two_group\", branded = TRUE)\n",
    "  export_static_survey(demo$instrument, preview = TRUE)  # respondent view\n",
    "  launch_studio(demo$instrument, screen = \"preview\")     # in SurveyStudio\n",
    "Start:  launch_builder() to design, launch_studio() to analyse,\n",
    "        or vignette(\"surveyframe\") for the tour.\n",
    "Cite:   citation(\"surveyframe\")\n",
    "Help:   https://mohammedalisharafuddin.github.io/surveyframe/"
  )
}
