# inst/shiny/app.R
# SurveyStudio - build, edit, preview, and analyse surveyframe instruments.

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

tags <- shiny::tags
HTML <- shiny::HTML
fluidPage <- shiny::fluidPage
fluidRow <- shiny::fluidRow
column <- shiny::column
uiOutput <- shiny::uiOutput
renderUI <- shiny::renderUI
actionButton <- shiny::actionButton
checkboxInput <- shiny::checkboxInput
checkboxGroupInput <- shiny::checkboxGroupInput
dateInput <- shiny::dateInput
downloadButton <- shiny::downloadButton
downloadHandler <- shiny::downloadHandler
fileInput <- shiny::fileInput
numericInput <- shiny::numericInput
observe <- shiny::observe
observeEvent <- shiny::observeEvent
radioButtons <- shiny::radioButtons
reactive <- shiny::reactive
reactiveValues <- shiny::reactiveValues
req <- shiny::req
selectInput <- shiny::selectInput
showNotification <- shiny::showNotification
shinyApp <- shiny::shinyApp
tagList <- shiny::tagList
textAreaInput <- shiny::textAreaInput
textInput <- shiny::textInput
updateCheckboxGroupInput <- shiny::updateCheckboxGroupInput
updateSelectInput <- shiny::updateSelectInput
updateTextAreaInput <- shiny::updateTextAreaInput
updateTextInput <- shiny::updateTextInput

builder_empty_state <- surveyframe::sframe_builder_empty_state
builder_state_from_instrument <- surveyframe::sframe_builder_state_from_instrument
builder_validate_draft <- surveyframe::sframe_builder_validate_draft
sf_branch <- surveyframe::sf_branch
sf_check <- surveyframe::sf_check
sf_choices <- surveyframe::sf_choices
sf_item <- surveyframe::sf_item
sf_scale <- surveyframe::sf_scale
write_sframe <- surveyframe::write_sframe

status_badge <- function(ok, label_ok = "Ready", label_no = "Pending") {
  if (ok) {
    tags$span(class = "badge badge-ok", label_ok)
  } else {
    tags$span(class = "badge badge-wait", label_no)
  }
}

trim_or_null <- function(x) {
  x <- trimws(x %||% "")
  if (nzchar(x)) x else NULL
}

parse_lines <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(character(0))
  }
  values <- trimws(unlist(strsplit(text, "\n", fixed = TRUE), use.names = FALSE))
  values[nzchar(values)]
}

parse_csv <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(character(0))
  }
  values <- trimws(unlist(strsplit(text, ",", fixed = TRUE), use.names = FALSE))
  values[nzchar(values)]
}

parse_choice_values <- function(text) {
  values <- parse_lines(text)
  if (length(values) == 0) {
    return(values)
  }
  utils::type.convert(values, as.is = TRUE)
}

parse_branch_value <- function(text, operator) {
  raw <- trimws(text %||% "")
  if (!nzchar(raw)) {
    return(NULL)
  }
  if (identical(operator, "%in%")) {
    values <- parse_csv(raw)
    if (length(values) == 0) {
      return(NULL)
    }
    return(utils::type.convert(values, as.is = TRUE))
  }
  utils::type.convert(raw, as.is = TRUE)
}

parse_check_values <- function(text) {
  values <- parse_csv(text %||% "")
  if (length(values) == 0) {
    return(NULL)
  }
  utils::type.convert(values, as.is = TRUE)
}

sframe_upload_ext <- function(upload) {
  tolower(tools::file_ext(upload$name %||% ""))
}

sframe_validate_upload <- function(upload, extensions, max_size = 10 * 1024^2) {
  if (is.null(upload) || is.null(upload$datapath) || !file.exists(upload$datapath)) {
    stop("No file was uploaded. Select a .sframe file and try again.", call. = FALSE)
  }

  ext <- sframe_upload_ext(upload)
  if (!ext %in% extensions) {
    stop(
      paste0(
        "Unsupported file type '.", ext, "'. Expected: .",
        paste(extensions, collapse = ", ."), "."
      ),
      call. = FALSE
    )
  }

  size <- file.info(upload$datapath)$size
  if (is.na(size) || size <= 0 || size > max_size) {
    stop("Uploaded file is empty or too large.", call. = FALSE)
  }

  con <- file(upload$datapath, open = "rb")
  on.exit(close(con), add = TRUE)
  bytes <- readBin(con, what = "raw", n = min(size, 512))
  if (length(bytes) == 0 || any(bytes == as.raw(0))) {
    stop("Upload a .sframe or JSON text file.", call. = FALSE)
  }

  text_start <- sub("^\ufeff", "", rawToChar(bytes))
  if (identical(ext, "sframe") && !startsWith(trimws(text_start), "{")) {
    stop("Uploaded .sframe file must contain JSON.", call. = FALSE)
  }

  normalizePath(upload$datapath, mustWork = TRUE)
}

format_value <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("")
  }
  paste(as.character(x), collapse = ", ")
}

upsert_component <- function(components, component) {
  if (length(components) == 0) {
    return(list(component))
  }

  ids <- vapply(components, function(x) x$id, character(1))
  idx <- match(component$id, ids)
  if (is.na(idx)) {
    c(components, list(component))
  } else {
    components[[idx]] <- component
    components
  }
}

remove_component <- function(components, id) {
  if (length(components) == 0) {
    return(components)
  }
  Filter(function(component) !identical(component$id, id), components)
}

drop_item_from_builder <- function(builder, item_id) {
  builder$items <- remove_component(builder$items, item_id)

  if (length(builder$scales) > 0) {
    builder$scales <- Filter(function(scale) {
      scale$items <- setdiff(scale$items %||% character(0), item_id)
      scale$reverse_items <- intersect(scale$reverse_items %||% character(0), scale$items)
      length(scale$items) > 0
    }, builder$scales)
  }

  if (length(builder$branching) > 0) {
    builder$branching <- Filter(function(rule) {
      !identical(rule$item_id, item_id) && !identical(rule$depends_on, item_id)
    }, builder$branching)
  }

  if (length(builder$checks) > 0) {
    builder$checks <- Filter(function(check) !identical(check$item_id, item_id), builder$checks)
  }

  builder
}

optional_choices <- function(ids, none_label = "(None)") {
  choices <- stats::setNames(ids, ids)
  c(stats::setNames("", none_label), choices)
}

table_card <- function(title, headers, rows, empty_label) {
  tags$div(class = "card",
    tags$div(class = "card-title", title),
    if (length(rows) == 0) {
      tags$p(class = "hint", empty_label)
    } else {
      tags$table(class = "sf-table",
        tags$thead(tags$tr(do.call(tagList, lapply(headers, tags$th)))),
        tags$tbody(rows)
      )
    }
  )
}

analysis_role <- function(id, label, min = 1, max = 1, levels = "any") {
  list(id = id, label = label, min = min, max = max, levels = levels)
}

analysis_registry <- local({
  role <- analysis_role
  list(
    frequency = list(
      family = "descriptive", label = "Frequency table",
      roles = list(role("variable", "Variable", levels = c("nominal", "ordinal", "likert"))),
      show_alpha = FALSE, show_hypotheses = FALSE, show_effect_size = FALSE,
      assumptions = character(0),
      output = "Counts, percentages, valid percentages, and missing values.",
      refs = character(0)
    ),
    descriptives = list(
      family = "descriptive", label = "Descriptives",
      roles = list(
        role("variables", "Variables", max = 99, levels = c("continuous", "scale", "ordinal", "likert")),
        role("split_by", "Split by", min = 0, max = 1, levels = c("nominal", "ordinal"))
      ),
      show_alpha = FALSE, show_hypotheses = FALSE, show_effect_size = FALSE,
      assumptions = character(0),
      output = "N, valid N, missing N, mean, SD, median, IQR, range, skewness, kurtosis, SE, and CI.",
      refs = character(0)
    ),
    missing_data = list(
      family = "data_quality", label = "Missing values",
      roles = list(role("variables", "Variables", min = 0, max = 99, levels = "any")),
      show_alpha = FALSE, show_hypotheses = FALSE, show_effect_size = FALSE,
      assumptions = character(0),
      output = "Item missingness, respondent missingness, missing patterns, and deletion summary.",
      refs = character(0)
    ),
    quality = list(
      family = "data_quality", label = "Quality report",
      roles = list(role("variables", "Variables", min = 0, max = 99, levels = "any")),
      show_alpha = FALSE, show_hypotheses = FALSE, show_effect_size = FALSE,
      assumptions = character(0),
      output = "Attention checks, response quality flags, and quality summary.",
      refs = character(0)
    ),
    scale_descriptives = list(
      family = "descriptive", label = "Scale descriptives",
      roles = list(role("scales", "Scale scores", min = 0, max = 99, levels = "scale")),
      show_alpha = FALSE, show_hypotheses = FALSE, show_effect_size = FALSE,
      assumptions = character(0),
      output = "Scale means, variability, missingness, and confidence intervals.",
      refs = character(0)
    ),
    crosstab = list(
      family = "categorical", label = "Cross-tab",
      roles = list(
        role("row", "Row variable", levels = c("nominal", "ordinal")),
        role("column", "Column variable", levels = c("nominal", "ordinal"))
      ),
      show_alpha = FALSE, show_hypotheses = FALSE, show_effect_size = TRUE,
      assumptions = "Expected cell counts",
      output = "Cross-tabulation with association measures when run.",
      refs = "field_2018"
    ),
    chi_square = list(
      family = "categorical", label = "Chi-square",
      roles = list(
        role("row", "Variable X", levels = c("nominal", "ordinal")),
        role("column", "Variable Y", levels = c("nominal", "ordinal"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = "Expected-count check",
      output = "Chi-square, p value, Cramer's V or phi.",
      refs = c("field_2018", "cohen_1988")
    ),
    fisher_exact = list(
      family = "categorical", label = "Fisher's exact",
      roles = list(
        role("row", "Variable X", levels = c("nominal", "ordinal")),
        role("column", "Variable Y", levels = c("nominal", "ordinal"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = "Sparse-table handling",
      output = "Fisher's exact p value and odds ratio for 2 x 2 tables.",
      refs = "field_2018"
    ),
    mcnemar = list(
      family = "related_samples", label = "McNemar",
      roles = list(
        role("before", "Before or condition 1", levels = "nominal"),
        role("after", "After or condition 2", levels = "nominal")
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = FALSE,
      assumptions = "Paired binary categories",
      output = "McNemar chi-square and p value.",
      refs = "field_2018"
    ),
    cochran_q = list(
      family = "related_samples", label = "Cochran's Q",
      roles = list(role("measures", "Binary repeated measures", min = 2, max = 99, levels = "nominal")),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = FALSE,
      assumptions = "Related binary measures",
      output = "Cochran's Q and pairwise McNemar plan.",
      refs = "field_2018"
    ),
    mann_whitney = list(
      family = "group_comparison", label = "Mann-Whitney U",
      roles = list(
        role("group", "Grouping variable", levels = c("nominal", "ordinal")),
        role("outcome", "Outcome variable", levels = c("ordinal", "continuous", "scale", "likert"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = "Exactly two groups",
      output = "U statistic, p value, and rank-biserial effect size.",
      refs = c("mann_1947", "cohen_1988")
    ),
    t_test_ind = list(
      family = "group_comparison", label = "Independent t-test",
      roles = list(
        role("group", "Grouping variable", levels = "nominal"),
        role("outcome", "Outcome variable", levels = c("continuous", "scale"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = c("Normality", "Equal variance"),
      output = "t, df, p value, and Cohen's d.",
      refs = c("field_2018", "cohen_1988")
    ),
    t_test_pair = list(
      family = "related_samples", label = "Paired t-test",
      roles = list(
        role("before", "Measure 1", levels = c("continuous", "scale")),
        role("after", "Measure 2", levels = c("continuous", "scale"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = "Normality of differences",
      output = "Paired t, df, p value, and dz.",
      refs = "field_2018"
    ),
    wilcoxon_pair = list(
      family = "related_samples", label = "Wilcoxon signed-rank",
      roles = list(
        role("before", "Measure 1", levels = c("ordinal", "continuous", "scale", "likert")),
        role("after", "Measure 2", levels = c("ordinal", "continuous", "scale", "likert"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = "Paired ordinal or continuous measures",
      output = "V, p value, and r effect size.",
      refs = "cohen_1988"
    ),
    kruskal_wallis = list(
      family = "group_comparison", label = "Kruskal-Wallis",
      roles = list(
        role("group", "Grouping variable", levels = c("nominal", "ordinal")),
        role("outcome", "Outcome variable", levels = c("ordinal", "continuous", "scale", "likert"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = "Independent groups",
      output = "H, p value, eta-squared, and pairwise plan.",
      refs = c("kruskal_1952", "cohen_1988")
    ),
    anova_one = list(
      family = "group_comparison", label = "One-way ANOVA",
      roles = list(
        role("group", "Grouping variable", levels = "nominal"),
        role("outcome", "Outcome variable", levels = c("continuous", "scale"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = c("Normality", "Homogeneity of variance"),
      output = "F, p value, eta-squared, and post-hoc plan.",
      refs = c("field_2018", "cohen_1988")
    ),
    anova_two = list(
      family = "group_comparison", label = "Two-way ANOVA",
      roles = list(
        role("factor1", "Factor 1", levels = "nominal"),
        role("factor2", "Factor 2", levels = "nominal"),
        role("outcome", "Outcome", levels = c("continuous", "scale"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = c("Interaction interpretation", "Homogeneity of variance"),
      output = "Main effects, interaction, and partial eta-squared.",
      refs = c("field_2018", "cohen_1988")
    ),
    ancova = list(
      family = "group_comparison", label = "ANCOVA",
      roles = list(
        role("group", "Group", levels = "nominal"),
        role("covariates", "Covariates", min = 1, max = 99, levels = c("continuous", "scale")),
        role("outcome", "Outcome", levels = c("continuous", "scale"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = "Homogeneity of regression slopes",
      output = "Adjusted group effects and slope warning.",
      refs = "field_2018"
    ),
    repeated_anova = list(
      family = "related_samples", label = "Repeated-measures ANOVA",
      roles = list(role("measures", "Repeated measures", min = 2, max = 99, levels = c("continuous", "scale"))),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = "Within-subject design",
      output = "Within-subject ANOVA summary.",
      refs = "field_2018"
    ),
    friedman = list(
      family = "related_samples", label = "Friedman",
      roles = list(role("measures", "Repeated ordinal measures", min = 2, max = 99, levels = c("ordinal", "continuous", "scale", "likert"))),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = FALSE,
      assumptions = "Related measures",
      output = "Friedman chi-square and p value.",
      refs = "field_2018"
    ),
    correlation_pearson = list(
      family = "association", label = "Pearson correlation",
      roles = list(
        role("x", "Variable X", levels = c("continuous", "scale")),
        role("y", "Variable Y", levels = c("continuous", "scale"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = c("Linearity", "Bivariate normality"),
      output = "r, p value, and confidence interval where feasible.",
      refs = c("field_2018", "cohen_1988")
    ),
    correlation_spearman = list(
      family = "association", label = "Spearman correlation",
      roles = list(
        role("x", "Variable X", levels = c("ordinal", "continuous", "scale", "likert")),
        role("y", "Variable Y", levels = c("ordinal", "continuous", "scale", "likert"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = "Monotonic association",
      output = "rho and p value.",
      refs = c("spearman_1904", "cohen_1988")
    ),
    correlation_kendall = list(
      family = "association", label = "Kendall tau",
      roles = list(
        role("x", "Variable X", levels = c("ordinal", "likert")),
        role("y", "Variable Y", levels = c("ordinal", "likert"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = "Ordinal association",
      output = "tau and p value.",
      refs = "kendall_1938"
    ),
    partial_correlation = list(
      family = "association", label = "Partial correlation",
      roles = list(
        role("x", "Variable X", levels = c("continuous", "scale")),
        role("y", "Variable Y", levels = c("continuous", "scale")),
        role("controls", "Control variables", min = 1, max = 99, levels = c("continuous", "scale"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = "Linear control model",
      output = "Partial r and p value.",
      refs = "field_2018"
    ),
    regression_linear = list(
      family = "regression", label = "Linear regression",
      roles = list(
        role("dependent", "Dependent variable", levels = c("continuous", "scale")),
        role("predictors", "Predictors", min = 1, max = 99, levels = c("nominal", "ordinal", "continuous", "scale", "likert"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = c("Residual normality", "VIF", "Cook distance"),
      output = "Model fit, coefficients, and assumptions.",
      refs = "field_2018"
    ),
    regression_logistic_binary = list(
      family = "regression", label = "Binary logistic",
      roles = list(
        role("dependent", "Binary outcome", levels = "nominal"),
        role("predictors", "Predictors", min = 1, max = 99, levels = c("nominal", "ordinal", "continuous", "scale"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = c("Sparse cells", "Separation warning"),
      output = "Odds ratios, pseudo R-squared, and classification table.",
      refs = "hosmer_2013"
    ),
    regression_logistic_ordinal = list(
      family = "regression", label = "Ordinal logistic",
      roles = list(
        role("dependent", "Ordinal outcome", levels = c("ordinal", "likert")),
        role("predictors", "Predictors", min = 1, max = 99, levels = c("nominal", "ordinal", "continuous", "scale"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = "Ordered outcome",
      output = "MASS::polr model plan, odds ratios, and fit table.",
      refs = "hosmer_2013"
    ),
    regression_logistic_multinomial = list(
      family = "regression", label = "Multinomial logistic",
      roles = list(
        role("dependent", "Nominal outcome", levels = "nominal"),
        role("predictors", "Predictors", min = 1, max = 99, levels = c("nominal", "ordinal", "continuous", "scale"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = "Reference category",
      output = "nnet::multinom plan, odds ratios, and classification table.",
      refs = "hosmer_2013"
    ),
    mediation = list(
      family = "regression", label = "Mediation",
      roles = list(
        role("predictor", "Predictor", levels = c("continuous", "scale", "nominal")),
        role("mediator", "Mediator", levels = c("continuous", "scale")),
        role("outcome", "Outcome", levels = c("continuous", "scale"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = c("Regression assumptions", "Bootstrap CI"),
      output = "Direct, indirect, total effects, and bootstrap CI.",
      refs = "mackinnon_2008"
    ),
    moderation = list(
      family = "regression", label = "Moderation",
      roles = list(
        role("predictor", "Predictor", levels = c("continuous", "scale")),
        role("moderator", "Moderator", levels = c("continuous", "scale")),
        role("outcome", "Outcome", levels = c("continuous", "scale"))
      ),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = "Interaction model",
      output = "Interaction coefficient and conditional effects.",
      refs = "aiken_1991"
    ),
    reliability_alpha = list(
      family = "measurement", label = "Reliability alpha",
      roles = list(role("items", "Scale items", min = 2, max = 99, levels = c("ordinal", "likert", "continuous"))),
      show_alpha = FALSE, show_hypotheses = FALSE, show_effect_size = FALSE,
      assumptions = character(0),
      output = "Alpha, item-total correlations, and alpha if deleted.",
      refs = "cronbach_1951"
    ),
    reliability_omega = list(
      family = "measurement", label = "Reliability omega",
      roles = list(role("items", "Scale items", min = 2, max = 99, levels = c("ordinal", "likert", "continuous"))),
      show_alpha = FALSE, show_hypotheses = FALSE, show_effect_size = FALSE,
      assumptions = character(0),
      output = "Omega estimate when the optional psych package is available.",
      refs = "cronbach_1951"
    ),
    item_diagnostics = list(
      family = "measurement", label = "Item diagnostics",
      roles = list(role("items", "Items", min = 1, max = 99, levels = c("ordinal", "likert", "continuous"))),
      show_alpha = FALSE, show_hypotheses = FALSE, show_effect_size = FALSE,
      assumptions = character(0),
      output = "Item distributions, item-total correlations, and floor or ceiling checks.",
      refs = "field_2018"
    ),
    efa_readiness = list(
      family = "measurement", label = "EFA readiness",
      roles = list(role("items", "Items", min = 3, max = 99, levels = c("ordinal", "likert", "continuous"))),
      show_alpha = FALSE, show_hypotheses = FALSE, show_effect_size = FALSE,
      assumptions = c("KMO", "Bartlett test"),
      output = "KMO, Bartlett, and parallel-analysis planning.",
      refs = "field_2018"
    ),
    efa_solution = list(
      family = "measurement", label = "EFA solution",
      roles = list(role("items", "Items", min = 3, max = 99, levels = c("ordinal", "likert", "continuous"))),
      show_alpha = FALSE, show_hypotheses = FALSE, show_effect_size = FALSE,
      assumptions = c("Factor retention", "Cross-loadings"),
      output = "Loadings, communalities, uniqueness, and cross-loading flags.",
      refs = "field_2018"
    ),
    cfa_lavaan_syntax = list(
      family = "model", label = "CFA lavaan syntax",
      roles = list(role("model", "Saved model", min = 0, max = 1, levels = "model")),
      show_alpha = FALSE, show_hypotheses = FALSE, show_effect_size = FALSE,
      assumptions = "Construct indicators",
      output = "lavaan measurement model syntax.",
      refs = "field_2018"
    ),
    sem_lavaan_syntax = list(
      family = "model", label = "CB-SEM lavaan syntax",
      roles = list(role("model", "Saved model", levels = "model")),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = "Measurement and structural paths",
      output = "lavaan measurement, structural, indirect, and total-effect syntax.",
      refs = "field_2018"
    ),
    seminr_syntax = list(
      family = "model", label = "PLS-SEM seminr syntax",
      roles = list(role("model", "Saved model", levels = "model")),
      show_alpha = TRUE, show_hypotheses = TRUE, show_effect_size = TRUE,
      assumptions = "Construct modes and bootstrapping",
      output = "seminr measurement, structural, bootstrap, reliability, AVE, and HTMT syntax.",
      refs = "field_2018"
    )
  )
})

analysis_method_choices <- function() {
  stats::setNames(names(analysis_registry), vapply(analysis_registry, function(x) x$label, character(1)))
}

studio_level_meta <- function(item = NULL, scale = NULL) {
  if (!is.null(scale)) {
    return(list(level = "scale", code = "SCL", type = "scale score"))
  }
  type <- item$type %||% "text"
  if (identical(type, "likert")) {
    return(list(level = "likert", code = "LIK", type = type))
  }
  if (type %in% c("single_choice", "multiple_choice")) {
    return(list(level = "nominal", code = "NOM", type = type))
  }
  if (identical(type, "ranking")) {
    return(list(level = "ordinal", code = "ORD", type = type))
  }
  if (type %in% c("numeric", "slider", "rating")) {
    return(list(level = "continuous", code = "CON", type = type))
  }
  if (type %in% c("text", "textarea")) {
    return(list(level = "text", code = "TXT", type = type))
  }
  list(level = "identifier", code = "ID", type = type)
}

studio_variable_catalog <- function(instrument) {
  if (is.null(instrument)) {
    return(list())
  }
  # Display-only items are not response variables and must not appear as
  # analysis variables or role choices.
  question_items <- Filter(
    function(item) !(item$type %in% c("section_break", "text_block")),
    instrument$items %||% list()
  )
  item_rows <- lapply(question_items, function(item) {
    meta <- studio_level_meta(item = item)
    scale_membership <- item$scale_id %||% ""
    if (!nzchar(scale_membership) && length(instrument$scales %||% list()) > 0) {
      hits <- vapply(instrument$scales, function(scale) item$id %in% (scale$items %||% character(0)), logical(1))
      scale_membership <- paste(vapply(instrument$scales[hits], function(scale) scale$id, character(1)), collapse = ", ")
    }
    list(
      id = item$id,
      label = item$label %||% item$id,
      kind = "item",
      level = meta$level,
      code = meta$code,
      type = meta$type,
      choice_set = item$choice_set %||% "",
      scale = scale_membership,
      required = isTRUE(item$required)
    )
  })
  scale_rows <- lapply(instrument$scales %||% list(), function(scale) {
    meta <- studio_level_meta(scale = scale)
    list(
      id = scale$id,
      label = scale$label %||% scale$id,
      kind = "scale",
      level = meta$level,
      code = meta$code,
      type = meta$type,
      choice_set = "",
      scale = scale$id,
      required = FALSE
    )
  })
  c(item_rows, scale_rows)
}

studio_role_choices <- function(role, catalog, models) {
  if ("model" %in% (role$levels %||% character(0))) {
    ids <- vapply(models %||% list(), function(model) model$id %||% "", character(1))
    labels <- vapply(models %||% list(), function(model) {
      paste0(model$id %||% "", " (", model$type %||% "model", ")")
    }, character(1))
    return(stats::setNames(ids[nzchar(ids)], labels[nzchar(ids)]))
  }
  keep <- vapply(catalog, function(v) {
    "any" %in% role$levels || v$level %in% role$levels || v$kind %in% role$levels
  }, logical(1))
  vars <- catalog[keep]
  ids <- vapply(vars, function(v) v$id, character(1))
  labels <- vapply(vars, function(v) paste0(v$id, " - ", substr(v$label, 1, 48)), character(1))
  stats::setNames(ids, labels)
}

studio_flatten_roles <- function(roles) {
  vals <- unlist(roles, use.names = FALSE)
  unique(vals[nzchar(vals)])
}

studio_validate_plan_roles <- function(method, roles) {
  reg <- analysis_registry[[method]] %||% list(roles = list())
  messages <- character(0)
  for (role in reg$roles %||% list()) {
    vals <- roles[[role$id]] %||% character(0)
    vals <- vals[nzchar(vals)]
    n <- length(vals)
    if (n < role$min) {
      messages <- c(messages, paste0("Select ", role$min, " ", tolower(role$label), "."))
    }
    if (!is.null(role$max) && n > role$max) {
      messages <- c(messages, paste0(role$label, " allows at most ", role$max, " selection(s)."))
    }
  }
  guidance <- switch(
    method,
    frequency = "Select one variable for the frequency table.",
    mann_whitney = "Mann-Whitney U requires one grouping variable and one ordinal or continuous outcome.",
    chi_square = "Chi-square requires categorical variables.",
    fisher_exact = "Fisher's exact test is intended for sparse categorical tables.",
    correlation_pearson = "Pearson correlation requires continuous or scale-score variables.",
    correlation_spearman = "Spearman is safer for ordinal or Likert variables.",
    correlation_kendall = "Kendall tau is safer for ordinal or Likert variables.",
    regression_linear = "Regression requires one dependent variable and at least one predictor.",
    reliability_alpha = "Reliability requires at least two items in a scale.",
    efa_readiness = "EFA readiness usually requires at least three indicators.",
    efa_solution = "EFA solution usually requires at least three indicators.",
    cfa_lavaan_syntax = "CFA requires a saved model or construct plan; constructs with fewer than three indicators should be justified.",
    sem_lavaan_syntax = "CB-SEM requires a saved model with measurement and structural paths.",
    seminr_syntax = "PLS-SEM requires at least one construct and one structural path.",
    "Review compatibility before saving."
  )
  list(valid = length(messages) == 0, messages = messages, guidance = guidance)
}

studio_next_plan_id <- function(plan) {
  paste0("RQ", length(plan %||% list()) + 1L)
}

studio_safe_id <- function(x, prefix = "M") {
  x <- gsub("[^A-Za-z0-9_]", "_", trimws(x %||% ""))
  if (!nzchar(x)) {
    x <- prefix
  }
  if (!grepl("^[A-Za-z]", x)) {
    x <- paste0(prefix, "_", x)
  }
  x
}

studio_parse_paths <- function(text) {
  lines <- parse_lines(text %||% "")
  Filter(Negate(is.null), lapply(lines, function(line) {
    parts <- trimws(strsplit(line, "->", fixed = TRUE)[[1]])
    if (length(parts) != 2 || any(!nzchar(parts))) {
      return(NULL)
    }
    surveyframe::sf_path(parts[1], parts[2])
  }))
}

studio_parse_covariances <- function(text) {
  lines <- parse_lines(text %||% "")
  Filter(Negate(is.null), lapply(lines, function(line) {
    parts <- trimws(strsplit(line, "~~", fixed = TRUE)[[1]])
    if (length(parts) != 2 || any(!nzchar(parts))) {
      return(NULL)
    }
    surveyframe::sf_covariance(parts[1], parts[2])
  }))
}

studio_parse_indirect <- function(text) {
  lines <- parse_lines(text %||% "")
  Filter(Negate(is.null), lapply(lines, function(line) {
    parts <- trimws(strsplit(line, "->", fixed = TRUE)[[1]])
    if (length(parts) < 3 || any(!nzchar(parts))) {
      return(NULL)
    }
    surveyframe::sf_indirect(parts[1], parts[2:(length(parts) - 1)], parts[length(parts)])
  }))
}

INITIAL_INSTRUMENT <- shiny::getShinyOption("surveyframe_instrument", NULL)
INITIAL_RESPONSES <- shiny::getShinyOption("surveyframe_responses", NULL)
INITIAL_SCREEN <- shiny::getShinyOption("surveyframe_initial_screen", "auto")

if (is.null(INITIAL_SCREEN) || !nzchar(INITIAL_SCREEN)) {
  INITIAL_SCREEN <- "auto"
}

initial_builder <- if (inherits(INITIAL_INSTRUMENT, "sframe")) {
  builder_state_from_instrument(INITIAL_INSTRUMENT)
} else {
  builder_empty_state()
}

initial_tab <- INITIAL_SCREEN
if (identical(initial_tab, "data")) {
  initial_tab <- "responses"
}
if (identical(initial_tab, "auto")) {
  initial_tab <- if (!is.null(INITIAL_RESPONSES)) {
    "dashboard"
  } else if (inherits(INITIAL_INSTRUMENT, "sframe")) {
    "preview"
  } else {
    "open"
  }
}
if (!initial_tab %in% c("open", "preview", "responses", "quality",
                       "reliability", "analysis", "dashboard", "export")) {
  initial_tab <- if (inherits(INITIAL_INSTRUMENT, "sframe")) {
    "preview"
  } else {
    "open"
  }
}

tab_link_class <- function(tab) {
  if (identical(initial_tab, tab)) "active" else NULL
}

screen_class <- function(tab) {
  paste("screen", if (identical(initial_tab, tab)) "active")
}

ui <- fluidPage(
  tags$head(
    tags$title("SurveyStudio"),
    tags$style(HTML("
      body { font-family: 'Helvetica Neue', Arial, sans-serif;
             background: #f7f8fa; color: #1a1a2e; margin: 0; }
      .studio-shell { display: flex; min-height: 100vh; }
      .studio-sidebar {
        width: 240px; min-width: 240px; background: #1a1a2e;
        color: #c8ccd8; padding: 0; display: flex; flex-direction: column;
        position: fixed; top: 0; bottom: 0; left: 0; overflow-y: auto; z-index: 100;
      }
      .studio-logo {
        padding: 24px 20px 16px; font-size: 18px; font-weight: 700;
        color: #ffffff; letter-spacing: 0.02em; border-bottom: 1px solid #2e3250;
      }
      .studio-logo span { color: #16B3B1; }
      .studio-logo img { height: 22px; margin-right: 8px; vertical-align: middle; }
      .studio-nav { list-style: none; margin: 0; padding: 12px 0; flex: 1; }
      .studio-nav-item a {
        display: flex; align-items: center; gap: 10px;
        padding: 10px 20px; color: #9aa0b8; text-decoration: none;
        font-size: 14px; border-left: 3px solid transparent;
        transition: background 0.15s, color 0.15s;
      }
      .studio-nav-item a:hover,
      .studio-nav-item a.active {
        background: #22274a; color: #ffffff; border-left-color: #16B3B1;
      }
      .studio-status {
        padding: 16px 20px; font-size: 12px;
        border-top: 1px solid #2e3250; color: #9aa0b8;
      }
      .studio-main {
        margin-left: 240px; flex: 1; padding: 32px;
        min-height: 100vh; box-sizing: border-box;
        min-width: 0;
      }
      .screen { display: none; }
      .screen.active { display: block; }
      .card {
        background: #ffffff; border-radius: 8px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.08);
        padding: 24px; margin-bottom: 20px;
      }
      .card-title {
        font-size: 16px; font-weight: 600; margin: 0 0 16px;
        color: #1a1a2e;
      }
      .card-actions {
        display: flex; gap: 10px; flex-wrap: wrap; margin-top: 12px;
      }
      .badge {
        display: inline-block; padding: 3px 10px; border-radius: 12px;
        font-size: 12px; font-weight: 600;
      }
      .badge-ok { background: #e6f4ea; color: #2e7d32; }
      .badge-wait { background: #fef3cd; color: #856404; }
      .badge-warn { background: #fde8e8; color: #b91c1c; }
      .stat-row { display: flex; gap: 16px; flex-wrap: wrap; margin-bottom: 16px; }
      .stat-box {
        flex: 1; min-width: 120px; background: #f7f8fa; border-radius: 8px;
        padding: 16px; text-align: center;
      }
      .stat-box .stat-val { font-size: 28px; font-weight: 700; color: #1a1a2e; }
      .stat-box .stat-lbl { font-size: 12px; color: #6b718e; margin-top: 4px; }
      table.sf-table { width: 100%; border-collapse: collapse; font-size: 14px;
        display: block; overflow-x: auto; max-width: 100%; }
      /* Academic (APA) tables: horizontal rules only, no shading, no vertical lines */
      .sf-table th {
        background: none; text-align: left; padding: 8px 12px;
        font-weight: 700; color: #1a1a2e;
        border-top: 2px solid #1a1a2e; border-bottom: 1px solid #1a1a2e;
      }
      .sf-table td { padding: 8px 12px; border: none; vertical-align: top; overflow-wrap: anywhere; }
      .sf-table tbody tr:last-child td { border-bottom: 2px solid #1a1a2e; }
      .btn-primary {
        background: #16B3B1; color: #fff; border: none; border-radius: 6px;
        padding: 9px 20px; font-size: 14px; cursor: pointer; font-weight: 600;
      }
      .btn-primary:hover { background: #129a98; }
      .btn-outline {
        background: transparent; color: #16B3B1; border: 1.5px solid #16B3B1;
        border-radius: 6px; padding: 8px 18px; font-size: 14px;
        cursor: pointer; font-weight: 600;
      }
      .btn-outline:hover { background: #e6f7f7; }
      /* Brand pill tabs (match the dashboard nav), used for the studio sub-tabs */
      .studio-tabs .shiny-options-group {
        display: flex; gap: 2px; flex-wrap: wrap; background: #1e293b;
        padding: 6px; border-radius: 8px; margin: 4px 0 18px;
      }
      .studio-tabs .control-label { display: none; }
      .studio-tabs .radio-inline { margin: 0; padding: 0; }
      .studio-tabs .radio-inline input[type=radio] {
        position: absolute; opacity: 0; pointer-events: none;
      }
      .studio-tabs .radio-inline span {
        display: inline-block; padding: 7px 16px; border-radius: 6px;
        font-size: 12px; font-weight: 600; color: rgba(255,255,255,.6);
        cursor: pointer; transition: all .15s;
      }
      .studio-tabs .radio-inline:hover span {
        background: rgba(255,255,255,.06); color: rgba(255,255,255,.85);
      }
      .studio-tabs .radio-inline input[type=radio]:checked + span {
        background: #16B3B1; color: #fff;
      }
      h2.screen-title {
        font-size: 22px; font-weight: 700; margin: 0 0 24px; color: #1a1a2e;
      }
      .hint { font-size: 13px; color: #6b718e; margin-top: 6px; }
      .problem-list { margin: 0; padding-left: 18px; }
      .problem-list li { margin-bottom: 6px; }
      .sf-code {
        white-space: pre-wrap; background: #f7f8fa; border: 1px solid #e0e3ea;
        border-radius: 6px; padding: 12px; overflow-x: auto;
      }
      .role-card { border: 1px solid #e0e3ea; border-radius: 8px; padding: 12px; margin-bottom: 12px; }
      .role-card label { font-size: 13px; font-weight: 600; color: #1a1a2e; }
      .vmeta { border-bottom: 1px solid #f0f1f4; padding: 10px 0; }
      .vmeta:last-child { border-bottom: none; }
      .vmeta-id { font-size: 13px; font-weight: 700; color: #1a1a2e; }
      .vmeta-lbl { font-size: 12px; color: #4b5563; margin: 2px 0 5px; }
      .vbadge {
        display: inline-block; border: 1px solid #d9e2ec; border-radius: 999px;
        padding: 2px 7px; margin: 2px 3px 2px 0; font-size: 11px;
        color: #334155; background: #f8fafc;
      }
      .plan-msg { border-radius: 6px; padding: 10px; background: #fef3cd; color: #856404; }
      .plan-msg.ok { background: #e6f4ea; color: #2e7d32; }
      #survey_preview_frame {
        border: 1px solid #e0e3ea; border-radius: 8px; padding: 24px; background: #fff;
      }
    "))
  ),
  tags$div(class = "studio-shell",
    tags$div(class = "studio-sidebar",
      tags$div(class = "studio-logo",
        tags$img(src = "surveyframe-shiny-square-icon.png", alt = "surveyframe"),
        "Survey", tags$span("Studio")
      ),
      tags$ul(class = "studio-nav",
        tags$li(class = "studio-nav-item",
          tags$a(href = "#", `data-tab` = "open",
                 class = tab_link_class("open"), "Open Instrument")),
        tags$li(class = "studio-nav-item",
          tags$a(href = "#", `data-tab` = "preview",
                 class = tab_link_class("preview"), "Preview Survey")),
        tags$li(class = "studio-nav-item",
          tags$a(href = "#", `data-tab` = "responses",
                 class = tab_link_class("responses"), "Upload Responses")),
        tags$li(class = "studio-nav-item",
          tags$a(href = "#", `data-tab` = "quality",
                 class = tab_link_class("quality"), "Quality Dashboard")),
        tags$li(class = "studio-nav-item",
          tags$a(href = "#", `data-tab` = "reliability",
                 class = tab_link_class("reliability"), "Reliability")),
        tags$li(class = "studio-nav-item",
          tags$a(href = "#", `data-tab` = "analysis",
                 class = tab_link_class("analysis"), "Analysis Plan")),
        tags$li(class = "studio-nav-item",
          tags$a(href = "#", `data-tab` = "dashboard",
                 class = tab_link_class("dashboard"), "Dashboard")),
        tags$li(class = "studio-nav-item",
          tags$a(href = "#", `data-tab` = "export",
                 class = tab_link_class("export"), "Export"))
      ),
      tags$div(class = "studio-status", uiOutput("sidebar_status"))
    ),
    tags$div(class = "studio-main",
      tags$script(HTML("
        $(document).on('click', '[data-tab]', function(e) {
          e.preventDefault();
          var tab = $(this).data('tab');
          $('.studio-nav-item a').removeClass('active');
          $(this).addClass('active');
          $('.screen').removeClass('active');
          $('#screen-' + tab).addClass('active');
          Shiny.setInputValue('current_tab', tab, {priority: 'event'});
        });

        Shiny.addCustomMessageHandler('surveyframe-switch-tab', function(tab) {
          var link = $('[data-tab=\"' + tab + '\"]');
          if (link.length === 0) return;
          $('.studio-nav-item a').removeClass('active');
          link.addClass('active');
          $('.screen').removeClass('active');
          $('#screen-' + tab).addClass('active');
          Shiny.setInputValue('current_tab', tab, {priority: 'event'});
        });
      ")),

      tags$div(id = "screen-open", class = screen_class("open"),
        tags$h2(class = "screen-title", "Open Instrument"),
        tags$div(class = "card",
          tags$div(class = "card-title", "Load an existing .sframe"),
          fileInput("instr_file", NULL,
                    accept = ".sframe",
                    buttonLabel = "Browse .sframe file",
                    placeholder = "No file selected"),
          tags$p(class = "hint",
            "Design questions, welcome screen, logo, and thank-you page in the SurveyBuilder, then load the .sframe here to preview, analyse, and re-export."),
          uiOutput("open_status")
        ),
        uiOutput("instrument_summary_card")
      ),

      tags$div(id = "screen-preview", class = screen_class("preview"),
        tags$h2(class = "screen-title", "Preview Survey"),
        uiOutput("preview_gate"),
        tags$div(id = "survey_preview_frame", uiOutput("survey_preview_items"))
      ),

      tags$div(id = "screen-responses", class = screen_class("responses"),
        tags$h2(class = "screen-title", "Upload Responses"),
        uiOutput("responses_gate"),
        tags$div(class = "card",
          tags$div(class = "card-title", "Response data"),
          fileInput("resp_file", NULL,
                    accept = ".csv",
                    buttonLabel = "Browse CSV file",
                    placeholder = "No file selected"),
          fluidRow(
            column(4, textInput("resp_id_col", "Respondent ID column", placeholder = "e.g. id")),
            column(4, textInput("resp_time_col", "Submitted-at column", placeholder = "e.g. submitted_at")),
            column(4, checkboxInput("resp_strict", "Strict column check", value = TRUE))
          ),
          actionButton("load_responses_btn", "Load responses", class = "btn-primary")
        ),
        tags$div(class = "card",
          tags$div(class = "card-title", "Loading responses collected elsewhere"),
          tags$p(class = "hint",
            "Responses can come from the exported surveyframe survey or from another tool (Microsoft Forms, Qualtrics, Google Forms, and similar). Export them to CSV, then:"),
          tags$ol(class = "hint", style = "margin: 0 0 12px; padding-left: 18px; line-height: 1.7",
            tags$li("Open the matching instrument on the Open Instrument screen. The .sframe carries the analysis plan, so it is still required."),
            tags$li("Make sure the CSV column names match the item IDs in that instrument (download the sample CSV below for the expected format)."),
            tags$li("Browse the CSV above, set the respondent-ID and submitted-at columns if present, then click Load responses.")
          ),
          downloadButton("download_sample_csv", "Download sample CSV format", class = "btn-outline"),
          actionButton("load_sample_btn", "Or load the bundled demo", class = "btn-outline")
        ),
        tags$div(class = "card",
          tags$div(class = "card-title", "Read responses from a deployed Google Sheet"),
          tags$p(class = "hint",
            "If the survey was deployed with the surveyframe Google Sheets collector, read the live responses straight from the sheet. This needs the googlesheets4 package and access to the sheet."),
          textInput("sheet_url", "Google Sheet ID or URL",
                    placeholder = "https://docs.google.com/spreadsheets/d/YOUR_SHEET_ID"),
          fluidRow(
            column(4, textInput("sheet_tab", "Sheet tab", value = "Responses")),
            column(4, textInput("sheet_id_col", "Respondent ID column", value = "respondent_id")),
            column(4, textInput("sheet_time_col", "Submitted-at column", value = "submitted_at"))
          ),
          actionButton("load_sheet_btn", "Read from Google Sheet", class = "btn-primary")
        ),
        uiOutput("responses_summary_card")
      ),

      tags$div(id = "screen-quality", class = screen_class("quality"),
        tags$h2(class = "screen-title", "Quality Dashboard"),
        uiOutput("quality_gate"),
        uiOutput("quality_output")
      ),

      tags$div(id = "screen-reliability", class = screen_class("reliability"),
        tags$h2(class = "screen-title", "Reliability"),
        uiOutput("reliability_gate"),
        uiOutput("reliability_output")
      ),

      tags$div(id = "screen-analysis", class = screen_class("analysis"),
        tags$h2(class = "screen-title", "Analysis Plan"),
        uiOutput("analysis_gate"),
        tags$div(class = "studio-tabs",
          radioButtons(
            "analysis_stage", NULL,
            choices = c("Plan" = "Plan", "Run preview" = "Run", "Report outline" = "Report"),
            selected = "Plan",
            inline = TRUE
          )
        ),
        fluidRow(
          column(4, uiOutput("analysis_left_panel")),
          column(4, uiOutput("analysis_middle_panel")),
          column(4, uiOutput("analysis_right_panel"))
        )
      ),

      tags$div(id = "screen-dashboard", class = screen_class("dashboard"),
        tags$h2(class = "screen-title", "Dashboard"),
        uiOutput("dashboard_gate"),
        tags$div(class = "studio-tabs",
          radioButtons(
            "dashboard_view", NULL,
            choices = c("Overview", "Items", "Scales", "Data"),
            selected = "Overview",
            inline = TRUE
          )
        ),
        uiOutput("studio_dashboard_content")
      ),

      tags$div(id = "screen-export", class = screen_class("export"),
        tags$h2(class = "screen-title", "Export"),
        uiOutput("export_gate"),
        tags$div(class = "card",
          tags$div(class = "card-title", "Instrument file"),
          tags$p(class = "hint",
            "Download the current validated draft as a .sframe file."),
          uiOutput("export_sframe_ui")
        ),
        tags$div(class = "card",
          tags$div(class = "card-title", "Report contents"),
          checkboxInput("rpt_codebook", "Include codebook", value = TRUE),
          checkboxInput("rpt_quality", "Include quality report", value = TRUE),
          checkboxInput("rpt_missing", "Include missing-data report", value = TRUE),
          checkboxInput("rpt_descriptives", "Include descriptives", value = TRUE),
          checkboxInput("rpt_reliability", "Include reliability", value = TRUE),
          checkboxInput("rpt_analysis", "Include analysis-plan results", value = TRUE),
          checkboxInput("rpt_models", "Include saved models and syntax", value = TRUE),
          tags$br(),
          uiOutput("export_report_ui"),
          tags$p(class = "hint",
            "Generates a self-contained HTML report. Quarto is optional; an internal HTML fallback is available."),
          tags$p(class = "hint",
            "To export the deployable survey HTML or the Google Sheets collector, use the SurveyBuilder, which owns survey design and deployment.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    builder = initial_builder,
    instrument = INITIAL_INSTRUMENT,
    responses = INITIAL_RESPONSES,
    active_screen = initial_tab
  )

  set_builder_state <- function(state) {
    rv$builder <- state
    invisible(state)
  }

  if (!is.null(INITIAL_RESPONSES) && !is.data.frame(INITIAL_RESPONSES) &&
      !is.null(rv$instrument)) {
    tryCatch({
      rv$responses <- surveyframe::read_responses(INITIAL_RESPONSES, rv$instrument)
    }, error = function(e) NULL)
  }

  switch_tab <- function(tab) {
    session$sendCustomMessage("surveyframe-switch-tab", tab)
  }

  observeEvent(input$current_tab, {
    rv$active_screen <- input$current_tab
  }, ignoreInit = TRUE)

  sync_builder_inputs <- function(state) {
    updateTextInput(session, "draft_title", value = state$meta$title %||% "Untitled Survey")
    updateTextInput(session, "draft_version", value = state$meta$version %||% "0.1.0")
    updateTextAreaInput(session, "draft_description", value = state$meta$description %||% "")
    updateTextInput(session, "draft_authors", value = format_value(state$meta$authors))
    updateTextInput(session, "draft_languages", value = format_value(state$meta$languages))
  }

  builder_meta <- reactive({
    # Design now happens in the SurveyBuilder; the studio Build screen was
    # removed. Meta comes from the loaded instrument (rv$builder$meta); the
    # draft_* inputs no longer exist, so each field falls back to it.
    m <- rv$builder$meta %||% list()
    languages <- parse_csv(input$draft_languages %||% "")
    list(
      title = trimws(input$draft_title %||% m$title %||% "Untitled Survey"),
      version = trimws(input$draft_version %||% m$version %||% "0.1.0"),
      description = trim_or_null(input$draft_description) %||% (m$description %||% NULL),
      authors = {
        values <- parse_csv(input$draft_authors %||% "")
        if (length(values) == 0) (m$authors %||% NULL) else values
      },
      languages = if (length(languages) == 0) (m$languages %||% "en") else languages
    )
  })

  draft_result <- reactive({
    builder_validate_draft(
      meta = builder_meta(),
      choices = rv$builder$choices,
      items = rv$builder$items,
      scales = rv$builder$scales,
      branching = rv$builder$branching,
      checks = rv$builder$checks,
      analysis_plan = rv$builder$analysis_plan %||% list(),
      models = rv$builder$models %||% list(),
      render = rv$builder$render %||% list()
    )
  })

  preview_instrument <- reactive({
    draft_result()$instrument
  })

  observe({
    if (draft_result()$valid) {
      rv$instrument <- draft_result()$instrument
    }
  })

  observe({
    choice_ids <- vapply(rv$builder$choices, function(choice) choice$id, character(1))
    updateSelectInput(
      session,
      "item_choice_set",
      choices = optional_choices(choice_ids),
      selected = if ((input$item_choice_set %||% "") %in% c("", choice_ids)) input$item_choice_set else ""
    )
    updateSelectInput(session, "remove_choice_id", choices = optional_choices(choice_ids, "(Select one)"))
  })

  observe({
    item_ids <- vapply(rv$builder$items, function(item) item$id, character(1))
    updateSelectInput(session, "remove_item_id", choices = optional_choices(item_ids, "(Select one)"))
    updateCheckboxGroupInput(
      session,
      "scale_items",
      choices = stats::setNames(item_ids, item_ids),
      selected = intersect(input$scale_items %||% character(0), item_ids)
    )
    current_scale_items <- intersect(input$scale_items %||% character(0), item_ids)
    updateCheckboxGroupInput(
      session,
      "scale_reverse_items",
      choices = stats::setNames(current_scale_items, current_scale_items),
      selected = intersect(input$scale_reverse_items %||% character(0), current_scale_items)
    )
    updateSelectInput(session, "branch_item_id", choices = optional_choices(item_ids, "(Select one)"))
    updateSelectInput(session, "branch_depends_on", choices = optional_choices(item_ids, "(Select one)"))
    branch_labels <- if (length(rv$builder$branching) == 0) {
      character(0)
    } else {
      vapply(
        rv$builder$branching,
        function(rule) paste(rule$item_id, rule$action, "when", rule$depends_on, rule$operator, format_value(rule$value)),
        character(1)
      )
    }
    branch_keys <- if (length(rv$builder$branching) == 0) character(0) else seq_along(rv$builder$branching)
    updateSelectInput(session, "remove_branch_key",
                      choices = c(stats::setNames("", "(Select one)"), stats::setNames(as.character(branch_keys), branch_labels)))
    updateSelectInput(session, "check_item_id", choices = optional_choices(item_ids, "(Select one)"))
  })

  observe({
    scale_ids <- vapply(rv$builder$scales, function(scale) scale$id, character(1))
    updateSelectInput(session, "remove_scale_id", choices = optional_choices(scale_ids, "(Select one)"))
  })

  observe({
    check_ids <- vapply(rv$builder$checks, function(check) check$id, character(1))
    updateSelectInput(session, "remove_check_id", choices = optional_choices(check_ids, "(Select one)"))
  })

  session$onFlushed(function() {
    switch_tab(shiny::isolate(rv$active_screen %||% initial_tab))
  }, once = TRUE)

  observeEvent(input$new_survey_btn, {
    rv$builder <- builder_empty_state()
    rv$instrument <- NULL
    rv$responses <- NULL
    sync_builder_inputs(rv$builder)
    showNotification("Cleared the loaded instrument.", type = "message")
    switch_tab("open")
  })

  observeEvent(input$go_preview_btn, {
    switch_tab("preview")
  })

  observeEvent(input$add_choice_btn, {
    choice_id <- trim_or_null(input$choice_id)
    values <- parse_choice_values(input$choice_values)
    labels <- parse_lines(input$choice_labels)

    if (is.null(choice_id)) {
      showNotification("Choice set ID is required.", type = "error")
      return()
    }
    if (length(values) == 0 || length(labels) == 0) {
      showNotification("Enter at least one value and label.", type = "error")
      return()
    }
    if (length(values) != length(labels)) {
      showNotification("Choice set values and labels must have the same length.", type = "error")
      return()
    }

    choice <- sf_choices(
      id = choice_id,
      values = values,
      labels = labels,
      allow_other = isTRUE(input$choice_allow_other),
      randomise = isTRUE(input$choice_randomise)
    )
    existing <- choice_id %in% vapply(rv$builder$choices, function(x) x$id, character(1))
    state <- rv$builder
    state$choices <- upsert_component(state$choices, choice)
    set_builder_state(state)
    showNotification(
      if (existing) "Choice set updated." else "Choice set added.",
      type = "message"
    )
  })

  observeEvent(input$remove_choice_btn, {
    choice_id <- trim_or_null(input$remove_choice_id)
    if (is.null(choice_id)) {
      return()
    }
    in_use <- vapply(rv$builder$items, function(item) identical(item$choice_set, choice_id), logical(1))
    if (any(in_use)) {
      showNotification("That choice set is still used by one or more items.", type = "error")
      return()
    }
    state <- rv$builder
    state$choices <- remove_component(state$choices, choice_id)
    set_builder_state(state)
    showNotification("Choice set deleted.", type = "message")
  })

  observeEvent(input$add_item_btn, {
    item_id <- trim_or_null(input$item_id)
    item_label <- trim_or_null(input$item_label)
    item_type <- input$item_type %||% "text"
    choice_set <- trim_or_null(input$item_choice_set)

    if (is.null(item_id) || is.null(item_label)) {
      showNotification("Item ID and question text are required.", type = "error")
      return()
    }
    if (item_type %in% c("single_choice", "multiple_choice", "likert") && is.null(choice_set)) {
      showNotification("Choice items need a choice set.", type = "error")
      return()
    }

    item <- sf_item(
      id = item_id,
      label = item_label,
      type = item_type,
      required = isTRUE(input$item_required),
      choice_set = if (item_type %in% c("single_choice", "multiple_choice", "likert")) choice_set else NULL,
      help = trim_or_null(input$item_help),
      placeholder = if (item_type %in% c("text", "textarea")) trim_or_null(input$item_placeholder) else NULL
    )

    existing <- item_id %in% vapply(rv$builder$items, function(x) x$id, character(1))
    state <- rv$builder
    state$items <- upsert_component(state$items, item)
    set_builder_state(state)
    showNotification(if (existing) "Item updated." else "Item added.", type = "message")
  })

  observeEvent(input$remove_item_btn, {
    item_id <- trim_or_null(input$remove_item_id)
    if (is.null(item_id)) {
      return()
    }
    rv$builder <- drop_item_from_builder(rv$builder, item_id)
    showNotification("Item deleted. Related scale membership, branching, and checks were updated.", type = "message")
  })

  observeEvent(input$add_scale_btn, {
    scale_id <- trim_or_null(input$scale_id)
    scale_label <- trim_or_null(input$scale_label)
    scale_items <- input$scale_items %||% character(0)
    min_valid <- suppressWarnings(as.integer(input$scale_min_valid))

    if (is.null(scale_id) || is.null(scale_label)) {
      showNotification("Scale ID and label are required.", type = "error")
      return()
    }
    if (length(scale_items) == 0) {
      showNotification("Select at least one item for the scale.", type = "error")
      return()
    }
    if (is.na(min_valid) || min_valid < 1) {
      min_valid <- NULL
    }
    if (!is.null(min_valid) && min_valid > length(scale_items)) {
      showNotification("Minimum valid items must be less than or equal to the number of scale items.", type = "error")
      return()
    }

    scale <- sf_scale(
      id = scale_id,
      label = scale_label,
      items = scale_items,
      method = input$scale_method %||% "mean",
      min_valid = min_valid,
      reverse_items = input$scale_reverse_items %||% NULL
    )
    existing <- scale_id %in% vapply(rv$builder$scales, function(x) x$id, character(1))
    state <- rv$builder
    state$scales <- upsert_component(state$scales, scale)
    set_builder_state(state)
    showNotification(if (existing) "Scale updated." else "Scale added.", type = "message")
  })

  observeEvent(input$remove_scale_btn, {
    scale_id <- trim_or_null(input$remove_scale_id)
    if (is.null(scale_id)) {
      return()
    }
    state <- rv$builder
    state$scales <- remove_component(state$scales, scale_id)
    set_builder_state(state)
    showNotification("Scale deleted.", type = "message")
  })

  observeEvent(input$add_branch_btn, {
    target <- trim_or_null(input$branch_item_id)
    depends_on <- trim_or_null(input$branch_depends_on)
    operator <- input$branch_operator %||% "=="
    value <- parse_branch_value(input$branch_value, operator)

    if (is.null(target) || is.null(depends_on)) {
      showNotification("Select both a target item and a controlling item.", type = "error")
      return()
    }
    if (identical(target, depends_on)) {
      showNotification("Choose a different controlling item for this branching rule.", type = "error")
      return()
    }
    if (is.null(value)) {
      showNotification("Enter a match value for the branching rule.", type = "error")
      return()
    }

    rule <- sf_branch(
      item_id = target,
      depends_on = depends_on,
      operator = operator,
      value = value,
      action = input$branch_action %||% "show"
    )

    branch_key <- paste(target, depends_on, operator, input$branch_action %||% "show", sep = "|")
    existing_keys <- if (length(rv$builder$branching) == 0) {
      character(0)
    } else {
      vapply(rv$builder$branching, function(x) {
        paste(x$item_id, x$depends_on, x$operator, x$action, sep = "|")
      }, character(1))
    }

    if (branch_key %in% existing_keys) {
      idx <- match(branch_key, existing_keys)
      state <- rv$builder
      state$branching[[idx]] <- rule
      set_builder_state(state)
      showNotification("Branching rule updated.", type = "message")
    } else {
      state <- rv$builder
      state$branching <- c(state$branching, list(rule))
      set_builder_state(state)
      showNotification("Branching rule added.", type = "message")
    }
  })

  observeEvent(input$remove_branch_btn, {
    branch_key <- suppressWarnings(as.integer(input$remove_branch_key %||% NA))
    if (is.na(branch_key) || branch_key < 1 || branch_key > length(rv$builder$branching)) {
      return()
    }
    state <- rv$builder
    state$branching <- state$branching[-branch_key]
    set_builder_state(state)
    showNotification("Branching rule deleted.", type = "message")
  })

  observeEvent(input$add_check_btn, {
    check_id <- trim_or_null(input$check_id)
    item_id <- trim_or_null(input$check_item_id)
    check_type <- input$check_type %||% "attention"
    pass_values <- parse_check_values(input$check_pass_values)

    if (is.null(check_id) || is.null(item_id)) {
      showNotification("Check ID and check item are required.", type = "error")
      return()
    }
    if (check_type %in% c("attention", "instructional") && is.null(pass_values)) {
      showNotification("Enter at least one pass value for this check.", type = "error")
      return()
    }

    check <- sf_check(
      id = check_id,
      item_id = item_id,
      type = check_type,
      pass_values = pass_values,
      fail_action = input$check_fail_action %||% "flag",
      label = trim_or_null(input$check_label),
      notes = trim_or_null(input$check_notes)
    )

    existing <- check_id %in% vapply(rv$builder$checks, function(x) x$id, character(1))
    state <- rv$builder
    state$checks <- upsert_component(state$checks, check)
    set_builder_state(state)
    showNotification(if (existing) "Check updated." else "Check added.", type = "message")
  })

  observeEvent(input$remove_check_btn, {
    check_id <- trim_or_null(input$remove_check_id)
    if (is.null(check_id)) {
      return()
    }
    state <- rv$builder
    state$checks <- remove_component(state$checks, check_id)
    set_builder_state(state)
    showNotification("Check deleted.", type = "message")
  })

  observeEvent(input$instr_file, {
    req(input$instr_file)
    tryCatch({
      path <- sframe_validate_upload(input$instr_file, "sframe")
      loaded <- surveyframe::read_sframe(path)
      rv$builder <- builder_state_from_instrument(loaded)
      rv$instrument <- loaded
      rv$responses <- NULL
      sync_builder_inputs(rv$builder)
      showNotification("Instrument loaded. Showing the survey preview.", type = "message")
      switch_tab("preview")
    }, error = function(e) {
      showNotification(paste("Error:", conditionMessage(e)), type = "error")
    })
  })

  observeEvent(input$load_responses_btn, {
    req(input$resp_file, rv$instrument)
    id_col <- trim_or_null(input$resp_id_col)
    time_col <- trim_or_null(input$resp_time_col)
    tryCatch({
      path <- sframe_validate_upload(input$resp_file, "csv")
      rv$responses <- surveyframe::read_responses(
        x = path,
        instrument = rv$instrument,
        respondent_id = id_col,
        submitted_at = time_col,
        strict = isTRUE(input$resp_strict)
      )
      showNotification(paste(nrow(rv$responses), "responses loaded."), type = "message")
      switch_tab("quality")
    }, error = function(e) {
      showNotification(paste("Error:", conditionMessage(e)), type = "error")
    })
  })

  observeEvent(input$load_sheet_btn, {
    req(rv$instrument)
    sheet <- trim_or_null(input$sheet_url)
    if (is.null(sheet)) {
      showNotification("Enter the Google Sheet ID or URL first.", type = "warning")
      return(invisible(NULL))
    }
    if (!requireNamespace("googlesheets4", quietly = TRUE)) {
      showNotification(
        "Reading from Google Sheets needs the googlesheets4 package. Install it, then try again.",
        type = "error", duration = NULL)
      return(invisible(NULL))
    }
    tryCatch({
      rv$responses <- surveyframe::read_sheet_responses(
        sheet_id      = sheet,
        instrument    = rv$instrument,
        sheet_name    = trim_or_null(input$sheet_tab) %||% "Responses",
        respondent_id = trim_or_null(input$sheet_id_col),
        submitted_at  = trim_or_null(input$sheet_time_col)
      )
      showNotification(paste(nrow(rv$responses), "responses read from the Google Sheet."),
                       type = "message")
      switch_tab("quality")
    }, error = function(e) {
      showNotification(paste("Google Sheet error:", conditionMessage(e)), type = "error")
    })
  })

  observeEvent(input$load_sample_btn, {
    tryCatch({
      demo <- surveyframe::sframe_input_types_demo_data()
      rv$builder <- builder_state_from_instrument(demo$instrument)
      rv$instrument <- demo$instrument
      rv$responses <- demo$responses
      sync_builder_inputs(rv$builder)
      showNotification(
        paste0("Sample loaded: ", nrow(demo$responses),
               " responses. Explore Quality, Reliability, Analysis Plan, and Dashboard."),
        type = "message"
      )
      switch_tab("dashboard")
    }, error = function(e) {
      showNotification(paste("Could not load sample:", conditionMessage(e)), type = "error")
    })
  })

  output$download_sample_csv <- downloadHandler(
    filename = function() "surveyframe_input_types_responses.csv",
    content = function(file) {
      src <- system.file("extdata", "surveyframe_input_types_responses.csv",
                         package = "surveyframe")
      file.copy(src, file, overwrite = TRUE)
    }
  )

  output$sidebar_status <- renderUI({
    draft <- draft_result()
    tagList(
      tags$div(if (draft$valid) "Draft ready" else "Draft needs fixes"),
      tags$div(if (!is.null(rv$instrument)) "Instrument ready" else "No valid instrument"),
      tags$div(if (!is.null(rv$responses)) "Responses loaded" else "No responses")
    )
  })

  output$builder_summary_card <- renderUI({
    draft <- draft_result()
    tags$div(class = "card",
      tags$div(class = "card-title", draft$instrument$meta$title %||% "Survey draft"),
      tags$div(class = "stat-row",
        tags$div(class = "stat-box",
          tags$div(class = "stat-val", length(rv$builder$choices)),
          tags$div(class = "stat-lbl", "Choice sets")),
        tags$div(class = "stat-box",
          tags$div(class = "stat-val", length(rv$builder$items)),
          tags$div(class = "stat-lbl", "Items")),
        tags$div(class = "stat-box",
          tags$div(class = "stat-val", length(rv$builder$scales)),
          tags$div(class = "stat-lbl", "Scales")),
        tags$div(class = "stat-box",
          tags$div(class = "stat-val", length(rv$builder$analysis_plan %||% list())),
          tags$div(class = "stat-lbl", "Plans")),
        tags$div(class = "stat-box",
          tags$div(class = "stat-val", length(rv$builder$models %||% list())),
          tags$div(class = "stat-lbl", "Models")),
        tags$div(class = "stat-box",
          tags$div(class = "stat-val", if (draft$valid) "Ready" else "Fix"),
          tags$div(class = "stat-lbl", "Status"))
      )
    )
  })

  output$builder_validation_card <- renderUI({
    draft <- draft_result()
    tags$div(class = "card",
      tags$div(class = "card-title", "Draft validation"),
      if (draft$valid) {
        tagList(
          tags$p(status_badge(TRUE, "Validated", "Needs fixes")),
          tags$p(class = "hint",
            "This draft can be previewed, saved as a .sframe, and used for downstream analysis.")
        )
      } else {
        tagList(
          tags$p(status_badge(FALSE, "Validated", "Needs fixes")),
          tags$ul(class = "problem-list",
            do.call(tagList, lapply(draft$problems, tags$li))
          )
        )
      }
    )
  })

  output$choices_table <- renderUI({
    rows <- lapply(rv$builder$choices, function(choice) {
      tags$tr(
        tags$td(choice$id),
        tags$td(length(choice$values)),
        tags$td(format_value(choice$labels)),
        tags$td(if (isTRUE(choice$allow_other)) "Yes" else "No"),
        tags$td(if (isTRUE(choice$randomise)) "Yes" else "No")
      )
    })
    table_card("Current choice sets",
      headers = c("ID", "Options", "Labels", "Other", "Randomise"),
      rows = rows,
      empty_label = "No choice sets yet.")
  })

  output$items_table <- renderUI({
    rows <- lapply(rv$builder$items, function(item) {
      tags$tr(
        tags$td(item$id),
        tags$td(item$type),
        tags$td(item$choice_set %||% ""),
        tags$td(if (isTRUE(item$required)) "Yes" else "No"),
        tags$td(item$label)
      )
    })
    table_card("Current items",
      headers = c("ID", "Type", "Choice set", "Required", "Label"),
      rows = rows,
      empty_label = "No items yet.")
  })

  output$scales_table <- renderUI({
    rows <- lapply(rv$builder$scales, function(scale) {
      tags$tr(
        tags$td(scale$id),
        tags$td(scale$label),
        tags$td(format_value(scale$items)),
        tags$td(scale$method),
        tags$td(scale$min_valid %||% ""),
        tags$td(format_value(scale$reverse_items))
      )
    })
    table_card("Current scales",
      headers = c("ID", "Label", "Items", "Method", "Min valid", "Reverse"),
      rows = rows,
      empty_label = "No scales yet.")
  })

  output$branching_table <- renderUI({
    rows <- lapply(rv$builder$branching, function(rule) {
      tags$tr(
        tags$td(rule$item_id),
        tags$td(rule$depends_on),
        tags$td(rule$operator),
        tags$td(format_value(rule$value)),
        tags$td(rule$action)
      )
    })
    table_card("Current branching rules",
      headers = c("Target", "Depends on", "Operator", "Value", "Action"),
      rows = rows,
      empty_label = "No branching rules yet.")
  })

  output$checks_table <- renderUI({
    rows <- lapply(rv$builder$checks, function(check) {
      tags$tr(
        tags$td(check$id),
        tags$td(check$item_id),
        tags$td(check$type),
        tags$td(format_value(check$pass_values)),
        tags$td(check$fail_action)
      )
    })
    table_card("Current checks",
      headers = c("ID", "Item", "Type", "Pass values", "Fail action"),
      rows = rows,
      empty_label = "No checks yet.")
  })

  output$open_status <- renderUI({
    if (!is.null(rv$instrument)) {
      status_badge(TRUE, "Loaded", "No instrument loaded")
    } else {
      status_badge(FALSE, "Loaded", "No instrument loaded")
    }
  })

  output$instrument_summary_card <- renderUI({
    req(rv$instrument)
    instr <- rv$instrument
    tags$div(class = "card",
      tags$div(class = "card-title", instr$meta$title),
      tags$div(class = "stat-row",
        tags$div(class = "stat-box",
          tags$div(class = "stat-val", length(instr$items)),
          tags$div(class = "stat-lbl", "Items")),
        tags$div(class = "stat-box",
          tags$div(class = "stat-val", length(instr$scales)),
          tags$div(class = "stat-lbl", "Scales")),
        tags$div(class = "stat-box",
          tags$div(class = "stat-val", length(instr$analysis_plan %||% list())),
          tags$div(class = "stat-lbl", "Plans")),
        tags$div(class = "stat-box",
          tags$div(class = "stat-val", length(instr$models %||% list())),
          tags$div(class = "stat-lbl", "Models")),
        tags$div(class = "stat-box",
          tags$div(class = "stat-val", instr$meta$version),
          tags$div(class = "stat-lbl", "Version")),
        tags$div(class = "stat-box",
          tags$div(class = "stat-val",
                   if (isTRUE(instr$meta$validated)) "Valid" else "Draft"),
          tags$div(class = "stat-lbl", "Status"))
      )
    )
  })

  output$preview_gate <- renderUI({
    draft <- draft_result()
    if (length(draft$instrument$items) == 0) {
      return(tags$div(class = "card",
        "Add at least one item in Build Survey before previewing."))
    }
    if (!draft$valid) {
      return(tags$div(class = "card",
        tags$p("The current draft has validation issues. Preview is still shown so you can inspect the question flow."),
        tags$ul(class = "problem-list", do.call(tagList, lapply(draft$problems, tags$li)))
      ))
    }
    NULL
  })

  output$survey_preview_items <- renderUI({
    instr <- preview_instrument()
    req(instr)
    # Render the exact deployable survey so the preview is identical to what
    # respondents see. Reuses the single static-survey template through
    # export_static_survey() instead of re-creating the layout with widgets.
    tmp <- tempfile(fileext = ".html")
    ok <- tryCatch({
      suppressMessages(surveyframe::export_static_survey(
        instr, output_path = tmp, open = FALSE, overwrite = TRUE
      ))
      TRUE
    }, error = function(e) FALSE)
    if (!isTRUE(ok)) {
      return(tags$p(class = "hint",
        "Add at least one question and pass validation to preview the survey."))
    }
    html <- paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    tagList(
      tags$p(class = "hint", style = "margin-bottom: 12px;",
        "This is the exact deployable survey. Anything entered here stays in this preview."),
      tags$iframe(
        srcdoc = html,
        style = "width:100%;height:78vh;border:1px solid var(--cb);border-radius:10px;background:#fff;",
        title = "Survey preview"
      )
    )
  })

  output$responses_gate <- renderUI({
    if (is.null(rv$instrument)) {
      tags$div(class = "card",
        "Build or open a valid instrument before uploading responses.")
    }
  })

  output$responses_summary_card <- renderUI({
    req(rv$responses)
    tags$div(class = "card",
      tags$div(class = "card-title", "Loaded response data"),
      tags$div(class = "stat-row",
        tags$div(class = "stat-box",
          tags$div(class = "stat-val", nrow(rv$responses)),
          tags$div(class = "stat-lbl", "Respondents")),
        tags$div(class = "stat-box",
          tags$div(class = "stat-val", ncol(rv$responses)),
          tags$div(class = "stat-lbl", "Columns"))
      )
    )
  })

  output$quality_gate <- renderUI({
    if (is.null(rv$instrument) || is.null(rv$responses)) {
      tags$div(class = "card",
        "Build or open a valid instrument and load responses before running quality checks.")
    }
  })

  quality_result <- reactive({
    req(rv$instrument, rv$responses)
    id_col <- trim_or_null(input$resp_id_col)
    surveyframe::quality_report(rv$responses, rv$instrument, respondent_id = id_col)
  })

  output$quality_output <- renderUI({
    req(rv$instrument, rv$responses)
    qr <- quality_result()
    n <- qr$summary$n_respondents
    n_flag <- qr$summary$n_flagged %||% 0L
    stat_box <- function(v, l) tags$div(class = "stat-box",
      tags$div(class = "stat-val", v), tags$div(class = "stat-lbl", l))

    tagList(
      tags$div(class = "card",
        tags$div(class = "card-title", "Summary"),
        tags$div(class = "stat-row",
          stat_box(n, "Respondents"),
          stat_box(qr$summary$n_items, "Items"),
          stat_box(n - n_flag, "Clean"),
          stat_box(sprintf("%d (%.1f%%)", n_flag, qr$summary$flag_rate * 100), "Flagged")
        ),
        tags$p(class = "hint",
          "A respondent is flagged if caught by any check below: a failed attention check, straight-lining a scale, completing too fast, missing more than the threshold, or being a duplicate row.")
      ),

      if (length(qr$attention) > 0) {
        rows <- lapply(qr$attention, function(chk) {
          tags$tr(
            tags$td(chk$check_id),
            tags$td(chk$type),
            tags$td(chk$n_pass %||% NA),
            tags$td(chk$n_fail),
            tags$td(sprintf("%.1f%%", chk$pass_rate * 100))
          )
        })
        table_card("Attention checks",
          headers = c("Check ID", "Type", "n pass", "n fail", "Pass rate"),
          rows = rows,
          empty_label = "No checks.")
      },

      if (length(qr$straightline %||% list()) > 0) {
        rows <- lapply(qr$straightline, function(s) {
          tags$tr(
            tags$td(s$scale_id),
            tags$td(s$n_items),
            tags$td(length(s$flagged_rows %||% integer(0))),
            tags$td(sprintf("%.1f%%", (s$flag_rate %||% 0) * 100))
          )
        })
        table_card("Straight-lining by scale",
          headers = c("Scale", "Items", "Flagged", "Rate"),
          rows = rows,
          empty_label = "No multi-item scales.")
      },

      tags$div(class = "card",
        tags$div(class = "card-title", "Completion time"),
        if (isTRUE(qr$timing$available)) {
          tags$p(sprintf(
            "Median completion %.0f seconds. %d respondent(s) (%.1f%%) flagged as too fast.",
            qr$timing$median_sec %||% NA_real_,
            qr$timing$n_flagged %||% 0L,
            (qr$timing$flag_rate %||% 0) * 100))
        } else {
          tags$p(class = "hint",
            qr$timing$reason %||% "Timing analysis needs started_at and submitted_at columns.")
        }
      ),

      tags$div(class = "card",
        tags$div(class = "card-title", "Duplicates and missingness"),
        tags$p(sprintf("%d duplicate response row(s) detected.",
                       qr$duplicates$n_duplicates %||% 0L)),
        tags$p(sprintf(
          "%.1f%% of respondents exceed the %.0f%% missing threshold.",
          mean(qr$missing$respondent_miss > qr$missing$flagged_threshold, na.rm = TRUE) * 100,
          qr$missing$flagged_threshold * 100
        ))
      )
    )
  })

  output$reliability_gate <- renderUI({
    if (is.null(rv$instrument) || is.null(rv$responses)) {
      tags$div(class = "card",
        "Build or open a valid instrument and load responses to compute reliability.")
    }
  })

  reliability_result <- reactive({
    req(rv$instrument, rv$responses)
    tryCatch(
      surveyframe::reliability_report(rv$responses, rv$instrument),
      error = function(e) NULL
    )
  })

  output$reliability_output <- renderUI({
    req(rv$instrument, rv$responses)
    rr <- reliability_result()
    if (is.null(rr) || length(rr) == 0) {
      return(tags$div(class = "card",
        "No scales with two or more items found in response data."))
    }

    cards <- lapply(rr, function(scale) {
      rows <- list(
        tags$tr(tags$td("Items"), tags$td(scale$n_items)),
        tags$tr(tags$td("N"), tags$td(scale$n))
      )
      if (!is.null(scale$alpha)) {
        rows <- c(rows, list(tags$tr(tags$td("Cronbach alpha"),
                                     tags$td(sprintf("%.3f", scale$alpha)))))
      }
      if (!is.null(scale$omega_h)) {
        rows <- c(rows, list(tags$tr(tags$td("Omega h"),
                                     tags$td(sprintf("%.3f", scale$omega_h)))))
      }
      if (!is.null(scale$omega_t)) {
        rows <- c(rows, list(tags$tr(tags$td("Omega total"),
                                     tags$td(sprintf("%.3f", scale$omega_t)))))
      }

      tags$div(class = "card",
        tags$div(class = "card-title", paste0(scale$label, " (", scale$scale_id, ")")),
        tags$table(class = "sf-table", tags$tbody(rows))
      )
    })

    do.call(tagList, cards)
  })

  output$analysis_gate <- renderUI({
    if (is.null(rv$instrument)) {
      return(tags$div(class = "card",
        "Build or open a valid instrument before planning analyses."))
    }
    NULL
  })

  analysis_catalog <- reactive({
    req(rv$instrument)
    studio_variable_catalog(rv$instrument)
  })

  current_analysis_roles <- reactive({
    method <- input$analysis_method %||% "descriptives"
    reg <- analysis_registry[[method]] %||% analysis_registry$descriptives
    roles <- list()
    for (role in reg$roles %||% list()) {
      id <- paste0("analysis_role_", role$id)
      vals <- input[[id]] %||% character(0)
      vals <- as.character(vals)
      vals <- vals[nzchar(vals)]
      roles[[role$id]] <- if ((role$max %||% 1) > 1) vals else vals[1] %||% ""
    }
    roles
  })

  plan_table_ui <- function(plan) {
    plan <- rv$instrument$analysis_plan %||% list()
    if (length(plan) == 0) {
      return(tags$div(class = "card",
        tags$div(class = "card-title", "Saved analysis plans"),
        tags$p(class = "hint", "No saved analysis plans yet.")))
    }

    role_text <- function(block) {
      roles <- block$roles %||% NULL
      if (!is.null(roles) && length(roles) > 0) {
        return(paste(
          sprintf("%s: %s", names(roles), vapply(roles, format_value, character(1))),
          collapse = "; "
        ))
      }
      format_value(block$variables %||% character(0))
    }

    rows <- lapply(plan, function(block) {
      method <- block$method %||% block$test %||% ""
      reg <- analysis_registry[[method]] %||% list(label = method)
      tags$tr(
        tags$td(block$id %||% ""),
        tags$td(block$family %||% ""),
        tags$td(reg$label %||% method),
        tags$td(role_text(block)),
        tags$td(block$status %||% ""),
        tags$td(if (isTRUE(block$requires_data %||% TRUE)) "Yes" else "No")
      )
    })
    table_card("Saved analysis plans",
      headers = c("ID", "Family", "Method", "Roles", "Status", "Needs data"),
      rows = rows,
      empty_label = "No saved analysis plans.")
  }

  output$analysis_left_panel <- renderUI({
    req(rv$instrument)
    catalog <- analysis_catalog()
    variable_rows <- lapply(catalog, function(v) {
      tags$div(class = "vmeta",
        tags$div(class = "vmeta-id", v$id),
        tags$div(class = "vmeta-lbl", v$label),
        tags$div(
          tags$span(class = "vbadge", v$code),
          tags$span(class = "vbadge", v$type),
          tags$span(class = "vbadge", v$kind),
          if (nzchar(v$choice_set)) tags$span(class = "vbadge", paste("choice:", v$choice_set)),
          if (nzchar(v$scale)) tags$span(class = "vbadge", paste("scale:", v$scale)),
          tags$span(class = "vbadge", if (isTRUE(v$required)) "required" else "missing allowed")
        )
      )
    })
    models <- rv$instrument$models %||% list()
    model_rows <- lapply(models, function(model) {
      constructs <- model$measurement$constructs %||% list()
      paths <- model$structural$paths %||% list()
      tags$div(class = "vmeta",
        tags$div(class = "vmeta-id", model$id %||% ""),
        tags$div(class = "vmeta-lbl", model$label %||% ""),
        tags$div(
          tags$span(class = "vbadge", model$type %||% "model"),
          tags$span(class = "vbadge", model$engine %||% ""),
          tags$span(class = "vbadge", paste(length(constructs), "construct(s)")),
          tags$span(class = "vbadge", paste(length(paths), "path(s)"))
        )
      )
    })
    tagList(
      tags$div(class = "card",
        tags$div(class = "card-title", "Variables and constructs"),
        if (length(variable_rows) == 0) {
          tags$p(class = "hint", "Define items and scales before assigning variable roles.")
        } else {
          do.call(tagList, variable_rows)
        }
      ),
      tags$div(class = "card",
        tags$div(class = "card-title", "Saved models"),
        if (length(model_rows) == 0) {
          tags$p(class = "hint", "No saved models yet.")
        } else {
          do.call(tagList, model_rows)
        }
      )
    )
  })

  output$analysis_middle_panel <- renderUI({
    req(rv$instrument)
    stage <- input$analysis_stage %||% "Plan"
    if (identical(stage, "Run")) {
      full_plan  <- rv$instrument$analysis_plan %||% list()
      run_plan   <- Filter(function(b) !isFALSE(b$requires_data), full_plan)
      syntax_n   <- length(full_plan) - length(run_plan)
      return(tagList(
        plan_table_ui(run_plan),
        if (syntax_n > 0) tags$p(class = "hint", paste0(
          syntax_n, " syntax-only method(s) excluded from the run queue ",
          "(generate lavaan or seminr strings without response data).")),
        uiOutput("analysis_results_output")
      ))
    }
    if (identical(stage, "Report")) {
      return(tags$div(class = "card",
        tags$div(class = "card-title", "Report outline"),
        tags$p(class = "hint",
          "Use Export to generate the HTML report with codebook, quality, missing data, descriptives, reliability, saved analysis results, and model appendices."),
        tags$div(class = "stat-row",
          tags$div(class = "stat-box",
            tags$div(class = "stat-val", length(rv$instrument$analysis_plan %||% list())),
            tags$div(class = "stat-lbl", "Plans")),
          tags$div(class = "stat-box",
            tags$div(class = "stat-val", length(rv$instrument$models %||% list())),
            tags$div(class = "stat-lbl", "Models"))
        )
      ))
    }

    plan <- rv$instrument$analysis_plan %||% list()
    tagList(
      tags$div(class = "card",
        tags$div(class = "card-title", "Analysis options"),
        textInput(
          "analysis_plan_id",
          "Plan ID",
          value = shiny::isolate(input$analysis_plan_id %||% studio_next_plan_id(plan))
        ),
        textAreaInput(
          "analysis_question",
          "Research question",
          value = shiny::isolate(input$analysis_question %||% ""),
          rows = 3
        ),
        selectInput(
          "analysis_method",
          "Analysis method",
          choices = analysis_method_choices(),
          selected = shiny::isolate(input$analysis_method %||% "descriptives")
        ),
        tags$div(class = "hint", "Variables are assigned below by methodological role."),
        uiOutput("analysis_role_fields"),
        uiOutput("analysis_alpha_field"),
        textAreaInput(
          "analysis_decision_rule",
          "Planned decision rule",
          value = shiny::isolate(input$analysis_decision_rule %||% ""),
          rows = 3
        ),
        uiOutput("analysis_plan_validation"),
        uiOutput("save_analysis_plan_control"),
        tags$hr(),
        selectInput(
          "delete_analysis_plan_id",
          "Delete saved plan",
          choices = optional_choices(vapply(plan, function(block) block$id %||% "", character(1)), "(Select one)")
        ),
        actionButton("delete_analysis_plan_btn", "Delete plan", class = "btn-outline")
      ),
      uiOutput("model_builder_card")
    )
  })

  output$analysis_right_panel <- renderUI({
    req(rv$instrument)
    stage <- input$analysis_stage %||% "Plan"
    method <- input$analysis_method %||% "descriptives"
    reg <- analysis_registry[[method]] %||% analysis_registry$descriptives
    if (identical(stage, "Run")) {
      return(tags$div(class = "card",
        tags$div(class = "card-title", "Run preview"),
        tags$p(class = "hint",
          if (is.null(rv$responses)) {
            "Upload response data before running saved plans."
          } else {
            "Saved plans are run against the uploaded response data."
          })
      ))
    }
    if (identical(stage, "Report")) {
      return(tagList(
        uiOutput("models_table"),
        uiOutput("model_syntax_output")
      ))
    }
    tagList(
      tags$div(class = "card",
        tags$div(class = "card-title", "Output preview and reporting card"),
        tags$p(tags$strong(reg$label %||% method)),
        tags$p(reg$output %||% "Structured result."),
        if (length(reg$assumptions %||% character(0)) > 0) {
          tags$p(tags$strong("Assumptions: "), paste(reg$assumptions, collapse = ", "))
        },
        if (isTRUE(reg$show_alpha)) {
          tags$p(tags$strong("Significance level: "), input$analysis_alpha %||% 0.05)
        },
        if (length(reg$refs %||% character(0)) > 0) {
          tags$p(tags$strong("Reporting references: "), paste(reg$refs, collapse = ", "))
        }
      ),
      plan_table_ui(rv$instrument$analysis_plan %||% list())
    )
  })

  output$analysis_role_fields <- renderUI({
    req(rv$instrument)
    method <- input$analysis_method %||% "descriptives"
    reg <- analysis_registry[[method]] %||% analysis_registry$descriptives
    catalog <- analysis_catalog()
    models <- rv$instrument$models %||% list()
    fields <- lapply(reg$roles %||% list(), function(role) {
      choices <- studio_role_choices(role, catalog, models)
      selected <- input[[paste0("analysis_role_", role$id)]] %||% character(0)
      selected <- intersect(as.character(selected), unname(choices))
      tags$div(class = "role-card",
        selectInput(
          paste0("analysis_role_", role$id),
          paste0(role$label, if (role$min > 0) " *" else ""),
          choices = choices,
          selected = selected,
          multiple = (role$max %||% 1) > 1
        ),
        tags$div(class = "hint",
          paste0("Accepts: ", paste(role$levels, collapse = ", "),
                 ". Required: ", role$min, "; max: ", role$max, "."))
      )
    })
    do.call(tagList, fields)
  })

  output$analysis_alpha_field <- renderUI({
    method <- input$analysis_method %||% "descriptives"
    reg <- analysis_registry[[method]] %||% analysis_registry$descriptives
    if (!isTRUE(reg$show_alpha)) {
      return(NULL)
    }
    numericInput(
      "analysis_alpha",
      "Significance level",
      value = shiny::isolate(input$analysis_alpha %||% 0.05),
      min = 0.001,
      max = 0.2,
      step = 0.001
    )
  })

  output$analysis_plan_validation <- renderUI({
    method <- input$analysis_method %||% "descriptives"
    status <- studio_validate_plan_roles(method, current_analysis_roles())
    tags$div(class = paste("plan-msg", if (status$valid) "ok" else ""),
      if (status$valid) {
        paste("Required roles are complete.", status$guidance)
      } else {
        paste(status$messages, collapse = " ")
      }
    )
  })

  output$save_analysis_plan_control <- renderUI({
    method <- input$analysis_method %||% "descriptives"
    status <- studio_validate_plan_roles(method, current_analysis_roles())
    if (isTRUE(status$valid)) {
      actionButton("save_analysis_plan_btn", "Save analysis plan", class = "btn-primary")
    } else {
      tags$button(type = "button", class = "btn-primary", disabled = "disabled", "Save analysis plan")
    }
  })

  observeEvent(input$save_analysis_plan_btn, {
    req(rv$instrument)
    method <- input$analysis_method %||% "descriptives"
    reg <- analysis_registry[[method]] %||% analysis_registry$descriptives
    roles <- current_analysis_roles()
    status <- studio_validate_plan_roles(method, roles)
    if (!isTRUE(status$valid)) {
      showNotification(paste(status$messages, collapse = " "), type = "error")
      return()
    }
    plan_id <- studio_safe_id(trim_or_null(input$analysis_plan_id) %||%
                                studio_next_plan_id(rv$builder$analysis_plan), prefix = "RQ")
    question <- trim_or_null(input$analysis_question) %||% paste(reg$label, "analysis")
    options <- list()
    if (isTRUE(reg$show_alpha)) {
      options$alpha <- input$analysis_alpha %||% 0.05
    }
    block <- list(
      id = plan_id,
      research_question = question,
      family = reg$family %||% "",
      method = method,
      test = method,
      roles = roles,
      variables = studio_flatten_roles(roles),
      options = options,
      hypotheses = if (isTRUE(reg$show_hypotheses)) {
        list(null = "No effect or association is present.",
             alternative = "An effect or association is present.")
      } else {
        NULL
      },
      decision_rule = trim_or_null(input$analysis_decision_rule) %||% "",
      interpretation = trim_or_null(input$analysis_decision_rule) %||% "",
      reporting_references = reg$refs %||% character(0),
      citations = reg$refs %||% character(0),
      status = "valid_plan",
      requires_data = !method %in% c("cfa_lavaan_syntax", "sem_lavaan_syntax", "seminr_syntax")
    )
    state <- rv$builder
    existing <- vapply(state$analysis_plan %||% list(), function(x) x$id %||% "", character(1))
    idx <- match(plan_id, existing)
    if (is.na(idx)) {
      state$analysis_plan <- c(state$analysis_plan %||% list(), list(block))
    } else {
      state$analysis_plan[[idx]] <- block
    }
    set_builder_state(state)
    draft <- draft_result()
    if (draft$valid) {
      rv$instrument <- draft$instrument
    }
    showNotification("Analysis plan saved.", type = "message")
  })

  output$analysis_results_output <- renderUI({
    req(rv$instrument)
    if (length(rv$instrument$analysis_plan %||% list()) == 0) {
      return(NULL)
    }
    if (is.null(rv$responses)) {
      return(tags$div(class = "card",
        tags$div(class = "card-title", "Run results"),
        tags$p(class = "hint", "Upload response data to run saved analysis plans.")))
    }

    results <- tryCatch(
      surveyframe::run_analysis_plan(rv$responses, rv$instrument),
      error = function(e) e
    )
    if (inherits(results, "error")) {
      return(tags$div(class = "card",
        tags$div(class = "card-title", "Run results"),
        tags$p(conditionMessage(results))))
    }

    rows <- lapply(results, function(result) {
      tags$tr(
        tags$td(result$id %||% ""),
        tags$td(result$test %||% result$method %||% ""),
        tags$td(result$apa %||% result$error %||% "")
      )
    })
    table_card("Run results",
      headers = c("ID", "Method", "APA result"),
      rows = rows,
      empty_label = "No results were returned.")
  })

  observeEvent(input$delete_analysis_plan_btn, {
    plan_id <- trim_or_null(input$delete_analysis_plan_id)
    if (is.null(plan_id)) {
      return()
    }
    state <- rv$builder
    state$analysis_plan <- Filter(function(block) !identical(block$id, plan_id), state$analysis_plan %||% list())
    set_builder_state(state)
    draft <- draft_result()
    if (draft$valid) {
      rv$instrument <- draft$instrument
    }
    showNotification("Analysis plan deleted.", type = "message")
  })

  output$model_builder_card <- renderUI({
    req(rv$instrument)
    scale_ids <- vapply(rv$instrument$scales %||% list(), function(scale) scale$id, character(1))
    model_count <- length(rv$instrument$models %||% list()) + 1L
    tags$div(class = "card",
      tags$div(class = "card-title", "Model Builder"),
      textInput(
        "model_id",
        "Model ID",
        value = shiny::isolate(input$model_id %||% paste0("model_", model_count))
      ),
      textInput(
        "model_label",
        "Model label",
        value = shiny::isolate(input$model_label %||% paste(rv$instrument$meta$title %||% "Survey", "model"))
      ),
      selectInput(
        "model_type",
        "Output engine",
        choices = c("lavaan CFA" = "cfa", "lavaan CB-SEM" = "cb_sem", "seminr PLS-SEM" = "pls_sem"),
        selected = shiny::isolate(input$model_type %||% "cfa")
      ),
      radioButtons(
        "model_mode",
        "Measurement mode",
        choices = c("Reflective" = "reflective", "Composite" = "composite", "Single item" = "single_item"),
        selected = shiny::isolate(input$model_mode %||% "reflective"),
        inline = TRUE
      ),
      checkboxGroupInput(
        "model_scales",
        "Create constructs from scales",
        choices = stats::setNames(scale_ids, scale_ids),
        selected = intersect(shiny::isolate(input$model_scales %||% character(0)), scale_ids)
      ),
      textAreaInput("model_paths", "Structural paths", value = shiny::isolate(input$model_paths %||% ""),
                    rows = 3, placeholder = "SAT -> LOY"),
      textAreaInput("model_covariances", "Covariances", value = shiny::isolate(input$model_covariances %||% ""),
                    rows = 2, placeholder = "SAT ~~ LOY"),
      textAreaInput("model_indirect", "Indirect effects", value = shiny::isolate(input$model_indirect %||% ""),
                    rows = 2, placeholder = "SAT -> TRUST -> LOY"),
      numericInput("model_bootstrap", "Bootstrap samples", value = shiny::isolate(input$model_bootstrap %||% 5000),
                   min = 0, step = 100),
      tags$div(class = "card-actions",
        actionButton("save_model_btn", "Save model", class = "btn-primary")
      ),
      if (length(scale_ids) == 0) {
        tags$p(class = "hint", "Create scales before saving CFA, CB-SEM, or PLS-SEM models.")
      }
    )
  })

  observeEvent(input$save_model_btn, {
    req(rv$instrument)
    scale_ids <- input$model_scales %||% character(0)
    if (length(scale_ids) == 0) {
      showNotification("Select at least one scale to create model constructs.", type = "error")
      return()
    }
    scales <- Filter(function(scale) scale$id %in% scale_ids, rv$instrument$scales %||% list())
    type <- input$model_type %||% "cfa"
    mode <- if (identical(type, "pls_sem")) {
      input$model_mode %||% "composite"
    } else {
      "reflective"
    }
    constructs <- lapply(scales, function(scale) {
      surveyframe::sf_construct(
        id = studio_safe_id(scale$id, prefix = "C"),
        label = scale$label %||% scale$id,
        items = scale$items %||% character(0),
        mode = mode
      )
    })
    paths <- studio_parse_paths(input$model_paths)
    if (type %in% c("cb_sem", "pls_sem") && length(paths) == 0) {
      showNotification("CB-SEM and PLS-SEM models require at least one structural path.", type = "error")
      return()
    }
    model <- surveyframe::sf_model(
      id = studio_safe_id(trim_or_null(input$model_id) %||% paste0("model_", length(rv$instrument$models %||% list()) + 1L), prefix = "model"),
      label = trim_or_null(input$model_label),
      type = type,
      engine = if (identical(type, "pls_sem")) "seminr" else "lavaan",
      constructs = constructs,
      paths = paths,
      covariances = studio_parse_covariances(input$model_covariances),
      indirect = studio_parse_indirect(input$model_indirect),
      options = list(
        estimator = "MLR",
        standardised = TRUE,
        bootstrap = input$model_bootstrap %||% 5000,
        missing = "fiml"
      )
    )
    tryCatch({
      surveyframe::validate_model(model, instrument = rv$instrument, strict = TRUE)
      state <- rv$builder
      existing <- vapply(state$models %||% list(), function(x) x$id %||% "", character(1))
      idx <- match(model$id, existing)
      if (is.na(idx)) {
        state$models <- c(state$models %||% list(), list(model))
      } else {
        state$models[[idx]] <- model
      }
      set_builder_state(state)
      draft <- draft_result()
      if (draft$valid) {
        rv$instrument <- draft$instrument
      }
      showNotification("Model saved into the .sframe draft.", type = "message")
    }, error = function(e) {
      showNotification(paste("Model error:", conditionMessage(e)), type = "error")
    })
  })

  output$models_table <- renderUI({
    req(rv$instrument)
    models <- rv$instrument$models %||% list()
    if (length(models) == 0) {
      return(NULL)
    }

    rows <- lapply(models, function(model) {
      constructs <- model$measurement$constructs %||% model$constructs %||% list()
      paths <- model$structural$paths %||% model$paths %||% list()
      tags$tr(
        tags$td(model$id %||% ""),
        tags$td(model$label %||% ""),
        tags$td(model$type %||% ""),
        tags$td(model$engine %||% ""),
        tags$td(length(constructs)),
        tags$td(length(paths))
      )
    })
    table_card("Saved models",
      headers = c("ID", "Label", "Type", "Engine", "Constructs", "Paths"),
      rows = rows,
      empty_label = "No saved models.")
  })

  output$model_syntax_output <- renderUI({
    req(rv$instrument)
    models <- rv$instrument$models %||% list()
    if (length(models) == 0) {
      return(NULL)
    }

    cards <- lapply(models, function(model) {
      syntax <- tryCatch(
        switch(model$type %||% "",
          cfa = surveyframe::cfa_lavaan_syntax(instrument = rv$instrument, model = model),
          cb_sem = surveyframe::sem_lavaan_syntax(model, instrument = rv$instrument),
          sem = surveyframe::sem_lavaan_syntax(model, instrument = rv$instrument),
          pls_sem = surveyframe::seminr_syntax(model),
          "Syntax preview is not available for this model type."
        ),
        error = function(e) paste("Syntax preview could not be generated:", conditionMessage(e))
      )
      json <- tryCatch(
        surveyframe::model_json(model, pretty = TRUE),
        error = function(e) paste("Model JSON could not be generated:", conditionMessage(e))
      )
      tags$div(class = "card",
        tags$div(class = "card-title", model$label %||% model$id %||% "Saved model"),
        tags$p(class = "hint", paste("Type:", model$type %||% "", "| Engine:", model$engine %||% "")),
        tags$h4("Syntax"),
        tags$pre(class = "sf-code", syntax),
        tags$h4("Model JSON"),
        tags$pre(class = "sf-code", json)
      )
    })
    do.call(tagList, cards)
  })

  output$dashboard_gate <- renderUI({
    if (is.null(rv$instrument)) {
      return(tags$div(class = "card",
        "Load or build an instrument before opening the dashboard."))
    }
    if (is.null(rv$responses)) {
      return(tags$div(class = "card",
        "Load responses before opening a response dashboard."))
    }
    NULL
  })

  # Inline response dashboard, ported from launch_dashboard() and wired to the
  # studio's reactive instrument and responses (base-R charts, no new imports).
  dash_theme <- "#16B3B1"
  dash_q_items <- reactive({
    Filter(function(i) !(i$type %in% c("section_break", "text_block")),
           rv$instrument$items %||% list())
  })
  dash_find <- function(components, id) {
    for (cp in components %||% list()) if (identical(cp$id, id)) return(cp)
    NULL
  }

  output$studio_dashboard_content <- renderUI({
    req(rv$instrument)
    if (is.null(rv$responses)) return(NULL)
    view <- input$dashboard_view %||% "Overview"
    resp <- rv$responses
    instr <- rv$instrument

    if (identical(view, "Overview")) {
      date_range <- "Not available"
      if ("submitted_at" %in% names(resp)) {
        dts <- suppressWarnings(as.Date(substr(as.character(resp$submitted_at), 1, 10)))
        dts <- dts[!is.na(dts)]
        if (length(dts) > 1) {
          date_range <- paste0(format(min(dts), "%d %b"), " to ",
                               format(max(dts), "%d %b %Y"))
        }
      }
      kpi <- function(v, l) tags$div(class = "stat-box",
        tags$div(class = "stat-val", v), tags$div(class = "stat-lbl", l))
      return(tagList(
        tags$div(class = "stat-row",
          kpi(nrow(resp), "Responses"),
          kpi(length(dash_q_items()), "Items"),
          kpi(length(instr$scales %||% list()), "Scales"),
          kpi(length(instr$checks %||% list()), "Checks"),
          kpi(date_range, "Date range")
        ),
        tags$div(class = "card",
          tags$div(class = "card-title", "Instrument summary"),
          tags$p(class = "hint", paste0(
            "Title: ", instr$meta$title %||% "Untitled",
            "  |  Version: ", instr$meta$version %||% "Not available",
            "  |  Mode: ", instr$render$mode %||% "standard"))
        )
      ))
    }

    if (identical(view, "Items")) {
      qi <- dash_q_items()
      if (!length(qi)) return(tags$div(class = "card", "No question items."))
      ch <- stats::setNames(
        vapply(qi, `[[`, character(1), "id"),
        vapply(qi, function(i) paste0(i$id, ": ", substr(i$label %||% "", 1, 40)),
               character(1)))
      return(tagList(
        selectInput("dash_item_sel", "Select item", choices = ch, width = "420px"),
        tags$div(class = "card",
          tags$div(class = "card-title", "Response distribution"),
          shiny::plotOutput("studio_item_chart", height = "300px")),
        uiOutput("studio_item_table")
      ))
    }

    if (identical(view, "Scales")) {
      scs <- instr$scales %||% list()
      if (!length(scs)) return(tags$div(class = "card", "No scales defined."))
      ch <- stats::setNames(
        vapply(scs, `[[`, character(1), "id"),
        vapply(scs, function(s) paste0(s$id, ": ", s$label), character(1)))
      def_rows <- lapply(scs, function(s) {
        tags$tr(
          tags$td(s$id),
          tags$td(s$label %||% ""),
          tags$td(s$method %||% s$scoring %||% "mean"),
          tags$td(paste(s$items, collapse = ", ")),
          tags$td(paste(s$reverse %||% character(0), collapse = ", "))
        )
      })
      return(tagList(
        selectInput("dash_scale_sel", "Select scale", choices = ch, width = "380px"),
        tags$div(class = "card",
          tags$div(class = "card-title", "Scale score distribution"),
          shiny::plotOutput("studio_scale_chart", height = "280px")),
        table_card("Scale definitions",
          headers = c("ID", "Label", "Method", "Items", "Reverse items"),
          rows = def_rows, empty_label = "No scales defined.")
      ))
    }

    tags$div(class = "card",
      tags$div(class = "card-title", "Raw responses"),
      downloadButton("dash_dl_csv", "Download CSV", class = "btn-outline"),
      tags$p(class = "hint", paste0("Showing the first 200 of ", nrow(resp), " rows.")),
      tags$div(style = "overflow-x:auto", shiny::tableOutput("studio_raw_table"))
    )
  })

  output$studio_item_chart <- shiny::renderPlot({
    req(rv$instrument, rv$responses, input$dash_item_sel)
    resp <- rv$responses
    item <- dash_find(rv$instrument$items, input$dash_item_sel)
    if (is.null(item) || !input$dash_item_sel %in% names(resp)) {
      plot.new(); text(.5, .5, "No data for this item.", col = "#94a3b8"); return()
    }
    col_data <- resp[[input$dash_item_sel]]
    t <- item$type
    op <- par(mar = c(4, 9, 2, 1), bg = "white"); on.exit(par(op))
    if (t %in% c("likert", "single_choice", "multiple_choice")) {
      cs <- dash_find(rv$instrument$choices, item$choice_set %||% "")
      if (!is.null(cs)) {
        freq <- table(factor(col_data, levels = as.character(cs$values)))
        names(freq) <- cs$labels
      } else {
        freq <- table(col_data)
      }
      barplot(freq, horiz = TRUE, las = 1, col = dash_theme, border = NA,
              xlab = "Frequency", cex.names = .8, cex.axis = .8)
    } else if (t %in% c("numeric", "slider", "rating")) {
      num <- suppressWarnings(as.numeric(col_data)); num <- num[!is.na(num)]
      if (!length(num)) { plot.new(); text(.5, .5, "No numeric data."); return() }
      hist(num, col = dash_theme, border = "white", main = NULL,
           xlab = item$label, ylab = "Count", las = 1, cex.axis = .8)
    } else {
      plot.new()
      text(.5, .5, paste0("Chart unavailable for type '", t, "'."), col = "#94a3b8")
    }
  }, bg = "white")
  shiny::outputOptions(output, "studio_item_chart", suspendWhenHidden = FALSE)

  output$studio_item_table <- renderUI({
    req(rv$instrument, rv$responses, input$dash_item_sel)
    resp <- rv$responses
    item <- dash_find(rv$instrument$items, input$dash_item_sel)
    if (is.null(item) || !input$dash_item_sel %in% names(resp)) return(NULL)
    col_data <- resp[[input$dash_item_sel]]
    if (item$type %in% c("likert", "single_choice", "multiple_choice")) {
      cs <- dash_find(rv$instrument$choices, item$choice_set %||% "")
      if (!is.null(cs)) {
        freq <- table(factor(col_data, levels = as.character(cs$values)))
        labs <- cs$labels
        vals <- as.character(cs$values)
      } else {
        freq <- table(col_data); labs <- names(freq); vals <- names(freq)
      }
      tot <- sum(freq)
      rows <- lapply(seq_along(freq), function(i) tags$tr(
        tags$td(vals[i]), tags$td(labs[i]), tags$td(as.integer(freq[i])),
        tags$td(if (tot > 0) sprintf("%.1f%%", 100 * freq[i] / tot) else "0%")
      ))
      table_card("Frequency counts",
        headers = c("Value", "Label", "Count", "Percent"),
        rows = rows, empty_label = "No responses.")
    } else if (item$type %in% c("numeric", "slider", "rating")) {
      num <- suppressWarnings(as.numeric(col_data)); num <- num[!is.na(num)]
      if (!length(num)) return(NULL)
      rows <- list(tags$tr(
        tags$td(length(num)), tags$td(round(mean(num), 2)), tags$td(round(stats::sd(num), 2)),
        tags$td(round(min(num), 2)), tags$td(round(stats::median(num), 2)), tags$td(round(max(num), 2))
      ))
      table_card("Summary statistics",
        headers = c("N", "Mean", "SD", "Min", "Median", "Max"),
        rows = rows, empty_label = "No numeric data.")
    } else {
      NULL
    }
  })
  shiny::outputOptions(output, "studio_item_table", suspendWhenHidden = FALSE)

  output$studio_scale_chart <- shiny::renderPlot({
    req(rv$instrument, rv$responses, input$dash_scale_sel)
    resp <- rv$responses
    sc <- dash_find(rv$instrument$scales, input$dash_scale_sel)
    if (is.null(sc)) return()
    cols <- intersect(sc$items, names(resp))
    if (!length(cols)) {
      plot.new(); text(.5, .5, "Scale items not in responses.", col = "#94a3b8"); return()
    }
    nums <- lapply(resp[cols], function(x) suppressWarnings(as.numeric(x)))
    scores <- rowMeans(do.call(cbind, nums), na.rm = TRUE); scores <- scores[!is.na(scores)]
    if (!length(scores)) {
      plot.new(); text(.5, .5, "No valid scale scores.", col = "#94a3b8"); return()
    }
    op <- par(mar = c(4, 4, 2, 1), bg = "white"); on.exit(par(op))
    hist(scores, col = dash_theme, border = "white", main = NULL,
         xlab = paste0(sc$label, " score"), ylab = "Count", las = 1, cex.axis = .8)
    abline(v = mean(scores), col = "#dc2626", lwd = 2, lty = 2)
  }, bg = "white")
  shiny::outputOptions(output, "studio_scale_chart", suspendWhenHidden = FALSE)

  output$studio_raw_table <- shiny::renderTable({
    req(rv$responses)
    head(rv$responses, 200)
  }, striped = TRUE, hover = TRUE, bordered = FALSE, width = "100%", na = "")

  output$dash_dl_csv <- downloadHandler(
    filename = function() {
      paste0(gsub("[^a-zA-Z0-9]", "_", rv$instrument$meta$title %||% "survey"),
             "_responses.csv")
    },
    content = function(file) {
      utils::write.csv(rv$responses, file, row.names = FALSE, na = "")
    }
  )

  output$export_gate <- renderUI({
    if (is.null(rv$instrument)) {
      tags$div(class = "card",
        "Build or open a valid instrument before exporting files.")
    }
  })

  # A non-clickable placeholder shown when an export is not yet possible, so the
  # user cannot trigger a download that would error.
  export_disabled_btn <- function(label, hint) {
    tagList(
      tags$button(type = "button", class = "btn btn-primary", disabled = NA,
        style = "opacity: .5; cursor: not-allowed;",
        shiny::icon("download"), " ", label),
      tags$p(class = "hint", hint)
    )
  }

  output$export_sframe_ui <- renderUI({
    if (isTRUE(draft_result()$valid)) {
      downloadButton("download_sframe_btn", "Download .sframe", class = "btn-primary")
    } else {
      export_disabled_btn(
        "Download .sframe",
        "Open or build a valid instrument before downloading the .sframe file.")
    }
  })

  output$export_report_ui <- renderUI({
    if (!is.null(rv$instrument)) {
      downloadButton("download_report_btn", "Generate HTML report", class = "btn-primary")
    } else {
      export_disabled_btn(
        "Generate HTML report",
        "Open or build a valid instrument before generating the report.")
    }
  })

  output$download_sframe_btn <- downloadHandler(
    filename = function() {
      title <- draft_result()$instrument$meta$title %||% "survey"
      paste0(gsub("[^a-zA-Z0-9]", "_", title), ".sframe")
    },
    content = function(file) {
      draft <- draft_result()
      if (!draft$valid) {
        stop("Draft validation must pass before exporting a .sframe file.")
      }
      tmp <- tempfile(fileext = ".sframe")
      surveyframe::write_sframe(draft$instrument, tmp, overwrite = TRUE)
      file.copy(tmp, file, overwrite = TRUE)
    }
  )

  output$download_report_btn <- downloadHandler(
    filename = function() {
      paste0(
        gsub("[^a-zA-Z0-9]", "_", (rv$instrument$meta$title %||% "survey")),
        "_report.html"
      )
    },
    content = function(file) {
      tryCatch({
        surveyframe::render_report(
          instrument = rv$instrument,
          data = rv$responses,
          output_file = file,
          include_codebook = isTRUE(input$rpt_codebook),
          include_quality = isTRUE(input$rpt_quality),
          include_missing = isTRUE(input$rpt_missing),
          include_descriptives = isTRUE(input$rpt_descriptives),
          include_reliability = isTRUE(input$rpt_reliability),
          include_analysis = isTRUE(input$rpt_analysis),
          include_models = isTRUE(input$rpt_models)
        )
      }, error = function(e) {
        showNotification(paste("Report error:", conditionMessage(e)), type = "error")
      })
    }
  )

  # Screens are shown and hidden with custom CSS classes, so Shiny would suspend
  # the content outputs inside hidden screens and not always wake them on switch.
  # Keep the per-screen content outputs evaluated so every tab renders reliably.
  for (.oid in c(
    "instrument_summary_card", "survey_preview_items", "responses_summary_card",
    "quality_output", "reliability_output",
    "analysis_left_panel", "analysis_middle_panel", "analysis_right_panel",
    "analysis_results_output", "studio_dashboard_content",
    "export_sframe_ui", "export_report_ui",
    "download_sframe_btn", "download_report_btn"
  )) {
    shiny::outputOptions(output, .oid, suspendWhenHidden = FALSE)
  }
}

shinyApp(ui = ui, server = server)
