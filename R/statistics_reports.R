# statistics_reports.R
# Core survey statistics and structured reporting helpers.

# A column as numbers. A factor is read through its labels: as.numeric() on a
# factor gives level positions, so a factor holding 10 and 20 became 1 and 2.
# A factor whose labels are not numbers is an error. Character values that are
# not numbers become NA, as before.
sframe_num <- function(x) {
  if (is.factor(x)) return(sframe_as_measure(x, "a factor column"))
  suppressWarnings(as.numeric(x))
}

# Regression predictors: numeric columns, and text or factor columns whose
# values all read as numbers, enter as numbers. Any other text or factor column
# enters as a categorical predictor, where it had been forced to numbers.
sframe_predictor_frame <- function(data, predictors) {
  out <- lapply(predictors, function(col) {
    x <- data[[col]]
    if (is.numeric(x)) return(x)
    labels <- as.character(x)
    as_num <- suppressWarnings(as.numeric(labels))
    if (all(is.na(labels) | !is.na(as_num))) return(as_num)
    factor(labels)
  })
  stats::setNames(as.data.frame(out, stringsAsFactors = FALSE), predictors)
}

# The scales a psychometric block selects: scale ids named in its roles, and
# scales holding any item it names. NULL, meaning every scale, when it names
# neither.
sframe_block_scales <- function(roles, instrument) {
  named <- unique(as.character(unlist(
    roles[intersect(names(roles), c("scales", "scale", "items", "variables"))])))
  if (length(named) == 0) return(NULL)
  ids <- vapply(instrument$scales, function(s) s$id, character(1))
  holding <- vapply(instrument$scales, function(s) any(s$items %in% named),
                    logical(1))
  selected <- unique(c(intersect(named, ids), ids[holding]))
  if (length(selected) == 0) NULL else selected
}

# The quality block, with its configuration. The respondent id and timestamp
# columns come from the block, or the standard collected column names when
# present, so the duplicate and timing checks run on collected data.
sframe_run_quality <- function(data, instrument, roles, options) {
  pick <- function(role, default) {
    given <- sframe_role_values(roles, role)
    given <- as.character(options[[role]] %||% given)
    if (length(given) > 0 && nzchar(given[1])) return(given[1])
    if (default %in% colnames(data)) default else NULL
  }
  args <- list(
    data = data, instrument = instrument,
    respondent_id = pick("respondent_id", "respondent_id"),
    submitted_at = pick("submitted_at", "submitted_at"),
    started_at = pick("started_at", "started_at")
  )
  for (key in c("time_min", "missing_threshold", "straightline_min_items")) {
    if (!is.null(options[[key]])) args[[key]] <- options[[key]]
  }
  do.call(quality_report, args)
}

# Option keys each method reads. A key outside its method's list, and outside
# the keys every block may carry, is reported as ignored, so a setting that
# changed nothing does not look as if it applied. Methods absent from this map
# are not checked.
sframe_method_option_keys <- list(
  frequency = character(0), descriptives = "conf_level",
  missing_data = character(0),
  quality = c("respondent_id", "submitted_at", "started_at", "time_min",
              "missing_threshold", "straightline_min_items"),
  reliability_alpha = character(0), reliability_omega = character(0),
  item_diagnostics = character(0), efa_readiness = "nfactors",
  efa_solution = "nfactors", cfa_lavaan_syntax = "ordered",
  crosstab = "simulate_p_value", chi_square = "simulate_p_value",
  fisher_exact = "simulate_p_value", mcnemar = "correct",
  cochran_q = character(0), mann_whitney = character(0),
  t_test_ind = "var_equal", t_test_pair = character(0),
  kruskal_wallis = character(0), anova_one = character(0),
  anova_two = character(0), ancova = character(0),
  repeated_anova = character(0), friedman = character(0),
  wilcoxon_pair = character(0), correlation_pearson = character(0),
  correlation_spearman = character(0), correlation_kendall = character(0),
  partial_correlation = "method", regression_linear = character(0),
  regression_logistic_binary = character(0),
  regression_logistic_ordinal = character(0),
  regression_logistic_multinomial = character(0),
  firth_logistic = c("conf.level", "conf_level"), moderation = character(0),
  mediation = "bootstrap"
)

sframe_ignored_options <- function(method, options) {
  known <- sframe_method_option_keys[[method]]
  if (is.null(known) || length(options) == 0) return(NULL)
  ignored <- setdiff(names(options), c(known, "alpha", "weights", "seed"))
  if (length(ignored) == 0) NULL else ignored
}

sframe_nonmissing <- function(x) {
  x[!is.na(x)]
}

sframe_skewness <- function(x) {
  x <- sframe_nonmissing(sframe_num(x))
  if (length(x) < 3) return(NA_real_)
  s <- stats::sd(x)
  if (is.na(s) || s == 0) return(NA_real_)
  mean((x - mean(x))^3) / s^3
}

sframe_kurtosis <- function(x) {
  x <- sframe_nonmissing(sframe_num(x))
  if (length(x) < 4) return(NA_real_)
  s <- stats::sd(x)
  if (is.na(s) || s == 0) return(NA_real_)
  mean((x - mean(x))^4) / s^4 - 3
}

sframe_ci_mean <- function(x, conf_level = 0.95) {
  x <- sframe_nonmissing(sframe_num(x))
  n <- length(x)
  if (n < 2) return(c(NA_real_, NA_real_))
  se <- stats::sd(x) / sqrt(n)
  crit <- stats::qt((1 + conf_level) / 2, df = n - 1)
  mean(x) + c(-1, 1) * crit * se
}

sframe_weighted_mean <- function(x, w) {
  x <- sframe_num(x)
  w <- sframe_num(w)
  ok <- !is.na(x) & !is.na(w) & w >= 0
  if (!any(ok) || sum(w[ok]) == 0) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

sframe_role_values <- function(roles, names, default = character(0)) {
  if (is.null(roles) || !is.list(roles)) return(default)
  hits <- roles[intersect(names(roles), names)]
  vals <- unlist(hits, recursive = TRUE, use.names = FALSE)
  vals <- as.character(vals[!is.na(vals)])
  if (length(vals) == 0) default else vals
}

sframe_analysis_method <- function(block) {
  as.character(block$method %||% block$test %||% "")
}

sframe_analysis_roles <- function(block) {
  roles <- block$roles %||% list()
  vars <- as.character(block$variables %||% character(0))
  method <- sframe_analysis_method(block)
  if (length(roles) > 0) {
    return(roles)
  }
  switch(
    method,
    frequency = list(variable = vars[1]),
    descriptives = list(variables = vars),
    missing_data = list(variables = vars),
    quality = list(variables = vars),
    scale_descriptives = list(variables = vars),
    reliability_alpha = list(items = vars),
    reliability_omega = list(items = vars),
    item_diagnostics = list(items = vars),
    efa_readiness = list(items = vars),
    efa_solution = list(items = vars),
    cfa_lavaan_syntax = list(model = vars[1]),
    sem_lavaan_syntax = list(model = vars[1]),
    seminr_syntax = list(model = vars[1]),
    chi_square = list(row = vars[1], column = vars[2]),
    fisher_exact = list(row = vars[1], column = vars[2]),
    crosstab = list(row = vars[1], column = vars[2]),
    mcnemar = list(before = vars[1], after = vars[2]),
    cochran_q = list(measures = vars),
    t_test_ind = list(group = vars[1], outcome = vars[2]),
    t_test_pair = list(before = vars[1], after = vars[2]),
    mann_whitney = list(group = vars[1], outcome = vars[2]),
    wilcoxon_pair = list(before = vars[1], after = vars[2]),
    anova_one = list(group = vars[1], outcome = vars[2]),
    anova_two = list(factor1 = vars[1], factor2 = vars[2], outcome = vars[3]),
    ancova = list(group = vars[1], covariates = vars[2], outcome = vars[3]),
    repeated_anova = list(measures = vars),
    kruskal_wallis = list(group = vars[1], outcome = vars[2]),
    friedman = list(measures = vars),
    correlation_pearson = list(x = vars[1], y = vars[2]),
    correlation_spearman = list(x = vars[1], y = vars[2]),
    correlation_kendall = list(x = vars[1], y = vars[2]),
    partial_correlation = list(x = vars[1], y = vars[2], controls = vars[-c(1, 2)]),
    regression_linear = list(predictors = utils::head(vars, -1), dependent = utils::tail(vars, 1)),
    regression_logistic_binary = list(predictors = utils::head(vars, -1), dependent = utils::tail(vars, 1)),
    regression_logistic_ordinal = list(predictors = utils::head(vars, -1), dependent = utils::tail(vars, 1)),
    regression_logistic_multinomial = list(predictors = utils::head(vars, -1), dependent = utils::tail(vars, 1)),
    firth_logistic = list(predictors = utils::head(vars, -1), dependent = utils::tail(vars, 1)),
    moderation = list(predictor = vars[1], moderator = vars[2], outcome = vars[3]),
    mediation = list(predictor = vars[1], mediator = vars[2], outcome = vars[3]),
    # Decision family. A block declared with plain `variables` names the
    # matrix items that carry the rated performance matrix; weights then have
    # to come from options, since nothing in a bare variable list says which
    # item holds them.
    topsis = list(performance_items = vars),
    vikor = list(performance_items = vars),
    moora = list(performance_items = vars),
    smart = list(performance_items = vars),
    waspas = list(performance_items = vars),
    promethee = list(performance_items = vars),
    electre = list(performance_items = vars),
    ahp = list(pairwise = vars[1]),
    anp = list(pairwise = vars[1]),
    dematel = list(pairwise = vars[1]),
    # Text family (todo_text_analysis.md). `group` (section 1a) is optional so a bare
    # 2-element variable list still resolves without one.
    term_freq = list(item = vars[1], group = vars[2]),
    co_occurrence = list(item = vars[1]),
    topic_model_lda = list(item = vars[1]),
    stm_topics = list(item = vars[1]),
    ngram_freq = list(item = vars[1]),
    term_context = list(item = vars[1]),
    co_occurrence_network = list(item = vars[1]),
    tidy_sentiment = list(item = vars[1], group = vars[2]),
    quanteda_dfm = list(item = vars[1]),
    list(variables = vars)
  )
}

sframe_analysis_vars <- function(block) {
  unique(as.character(unlist(sframe_analysis_roles(block), recursive = TRUE, use.names = FALSE)))
}

sframe_inferential_methods <- c(
  "chi_square", "fisher_exact", "mcnemar", "cochran_q",
  "t_test_ind", "t_test_pair", "mann_whitney", "wilcoxon_pair",
  "anova_one", "anova_two", "ancova", "repeated_anova", "kruskal_wallis",
  "friedman", "correlation_pearson", "correlation_spearman",
  "correlation_kendall", "partial_correlation", "regression_linear",
  "regression_logistic_binary", "regression_logistic_ordinal",
  "regression_logistic_multinomial", "firth_logistic", "moderation", "mediation",
  "sem_lavaan_syntax"
)

sframe_method_uses_alpha <- function(method) {
  method %in% sframe_inferential_methods
}

sframe_require_columns <- function(data, cols, method) {
  cols <- as.character(cols)
  cols <- cols[!is.na(cols) & nzchar(cols)]
  missing <- setdiff(cols, colnames(data))
  if (length(missing) > 0) {
    return(paste0(method, " requires missing column(s): ",
                  paste(missing, collapse = ", "), "."))
  }
  NULL
}

#' Descriptive statistics report
#'
#' Computes survey descriptives for numeric, Likert, and scale-score columns,
#' including missingness, mean, standard deviation, median, IQR, range,
#' skewness, kurtosis, standard error, and confidence intervals.
#'
#' @param data A data.frame of responses.
#' @param variables Character vector of variables. When `NULL`, numeric-like
#'   columns are used.
#' @param split_by Optional grouping variable.
#' @param conf_level Confidence level for the mean interval.
#' @param weights Optional case-weight column.
#'
#' @return An object of class `sframe_descriptives_report`.
#' @export
#' @examples
#' demo <- sframe_demo_data()
#' dr <- descriptives_report(demo$responses, variables = c("sat_1", "sat_2"),
#'                            split_by = "visit_type")
#' dr$table
descriptives_report <- function(
    data,
    variables = NULL,
    split_by = NULL,
    conf_level = 0.95,
    weights = NULL
) {
  stopifnot(is.data.frame(data))
  if (is.null(variables)) {
    variables <- names(data)[vapply(data, function(x) any(!is.na(sframe_num(x))), logical(1))]
  }
  variables <- intersect(as.character(variables), colnames(data))
  if (length(variables) == 0) {
    rlang::abort("No descriptive variables were available.",
                 class = c("sframe_validation_error", "sframe_error"))
  }
  groups <- if (!is.null(split_by) && split_by %in% colnames(data)) {
    as.character(data[[split_by]])
  } else {
    rep("All", nrow(data))
  }
  group_levels <- unique(groups)
  if (length(group_levels) == 0) group_levels <- "All"
  w <- if (!is.null(weights) && weights %in% colnames(data)) data[[weights]] else NULL

  rows <- list()
  for (group in group_levels) {
    idx <- which(groups == group | (is.na(groups) & is.na(group)))
    if (length(idx) == 0) next
    for (var in variables) {
      x <- sframe_num(data[[var]][idx])
      valid <- !is.na(x)
      ci <- sframe_ci_mean(x, conf_level)
      rows[[length(rows) + 1L]] <- data.frame(
        variable = var,
        group = group,
        n = length(x),
        valid_n = sum(valid),
        missing_n = sum(!valid),
        mean = if (any(valid)) mean(x, na.rm = TRUE) else NA_real_,
        sd = if (sum(valid) > 1) stats::sd(x, na.rm = TRUE) else NA_real_,
        median = if (any(valid)) stats::median(x, na.rm = TRUE) else NA_real_,
        iqr = if (any(valid)) stats::IQR(x, na.rm = TRUE) else NA_real_,
        min = if (any(valid)) min(x, na.rm = TRUE) else NA_real_,
        max = if (any(valid)) max(x, na.rm = TRUE) else NA_real_,
        skewness = sframe_skewness(x),
        kurtosis = sframe_kurtosis(x),
        se = if (sum(valid) > 1) stats::sd(x, na.rm = TRUE) / sqrt(sum(valid)) else NA_real_,
        ci_low = ci[[1]],
        ci_high = ci[[2]],
        weighted_mean = if (!is.null(w)) sframe_weighted_mean(x, w[idx]) else NA_real_,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  table <- do.call(rbind, rows)
  structure(
    list(
      method = "descriptives",
      variables = variables,
      split_by = split_by,
      weights = weights,
      table = table,
      apa = sprintf("Descriptive statistics were computed for %d variable(s).", length(variables)),
      prompt = "Report central tendency, variability, missingness, and any skewed distributions before inferential tests."
    ),
    class = "sframe_descriptives_report"
  )
}

#' Missing-data report
#'
#' Reports item-wise missingness, respondent-wise missingness, missing-data
#' patterns, listwise and pairwise deletion counts, and scale scoring missing
#' rules. No imputation is performed.
#'
#' @param data A data.frame of responses.
#' @param instrument Optional `sframe` object.
#' @param variables Optional response columns. Defaults to instrument item IDs
#'   when an instrument is supplied, otherwise all columns.
#'
#' @return An object of class `sframe_missing_data_report`.
#' @export
#' @examples
#' demo <- sframe_demo_data()
#' mr <- missing_data_report(demo$responses, demo$instrument)
#' mr$item_missing
missing_data_report <- function(data, instrument = NULL, variables = NULL) {
  stopifnot(is.data.frame(data))
  if (is.null(variables)) {
    variables <- if (!is.null(instrument) && inherits(instrument, "sframe")) {
      vapply(instrument$items, function(i) i$id, character(1))
    } else {
      colnames(data)
    }
  }
  variables <- intersect(variables, colnames(data))
  item_data <- data[, variables, drop = FALSE]
  n <- nrow(item_data)
  item_table <- data.frame(
    variable = variables,
    missing_n = vapply(item_data, function(x) sum(is.na(x)), integer(1)),
    missing_pct = vapply(item_data, function(x) mean(is.na(x)), numeric(1)),
    valid_n = vapply(item_data, function(x) sum(!is.na(x)), integer(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  respondent_table <- data.frame(
    row = seq_len(n),
    missing_n = rowSums(is.na(item_data)),
    missing_pct = if (length(variables) == 0) 0 else rowMeans(is.na(item_data)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  pattern_key <- if (length(variables) == 0) {
    rep("", n)
  } else {
    apply(is.na(item_data), 1, function(row) paste(ifelse(row, "1", "0"), collapse = ""))
  }
  pattern_table <- as.data.frame(table(pattern = pattern_key), stringsAsFactors = FALSE)
  pattern_table$percent <- if (n == 0) numeric(0) else pattern_table$Freq / n
  names(pattern_table)[names(pattern_table) == "Freq"] <- "n"
  # The bit string itself ("000000000000000") is meaningless to read, so
  # decode each pattern into which variables are missing, in reader
  # language: "Complete case" for no missing items, otherwise the missing
  # variables' labels (falling back to their id when no instrument is
  # available), capped so one respondent missing everything does not turn
  # into an unreadable wall of text.
  if (nrow(pattern_table) > 0 && length(variables) > 0) {
    lookup <- sframe_label_lookup(instrument)
    label_for <- function(id) if (id %in% names(lookup)) lookup[[id]] else id
    pattern_table$description <- vapply(pattern_table$pattern, function(p) {
      missing_idx <- which(strsplit(p, "")[[1]] == "1")
      if (length(missing_idx) == 0) return("Complete case (no missing items)")
      labels <- vapply(variables[missing_idx], label_for, character(1))
      if (length(labels) > 5) {
        labels <- c(labels[1:5], sprintf("and %d more", length(labels) - 5))
      }
      paste0("Missing: ", paste(labels, collapse = ", "))
    }, character(1))
  } else {
    pattern_table$description <- character(0)
  }

  pairwise <- if (length(variables) > 0) {
    stats::setNames(
      lapply(variables, function(x) {
        stats::setNames(
          vapply(variables, function(y) sum(!is.na(data[[x]]) & !is.na(data[[y]])), integer(1)),
          variables
        )
      }),
      variables
    )
  } else {
    list()
  }

  scale_rules <- data.frame(
    scale_id = character(0),
    n_items = integer(0),
    min_valid = integer(0),
    missing_rule = character(0),
    stringsAsFactors = FALSE
  )
  if (!is.null(instrument) && inherits(instrument, "sframe") && length(instrument$scales) > 0) {
    scale_rules <- do.call(rbind, lapply(instrument$scales, function(scale) {
      min_valid <- scale$min_valid %||% length(scale$items)
      data.frame(
        scale_id = scale$id,
        n_items = length(scale$items),
        min_valid = min_valid,
        missing_rule = paste0("Score when at least ", min_valid, " item(s) are valid."),
        stringsAsFactors = FALSE
      )
    }))
  }

  structure(
    list(
      method = "missing_data",
      item_missing = item_table,
      respondent_missing = respondent_table,
      patterns = pattern_table,
      deletion = list(
        listwise_n = sum(stats::complete.cases(item_data)),
        pairwise_n = pairwise
      ),
      scale_missing_rules = scale_rules,
      mcar = sframe_mcar_test(item_data),
      apa = sprintf("Missing-data diagnostics were computed for %d variable(s).", length(variables)),
      prompt = "Report item and respondent missingness, the missing-data pattern, and the deletion rule used for each analysis."
    ),
    class = "sframe_missing_data_report"
  )
}

#' Flag univariate and multivariate outliers
#'
#' Uses transparent screening rules for numeric survey response variables. The
#' report supports data review before modelling, not automatic deletion.
#'
#' @param data A data.frame.
#' @param variables Character vector of numeric variables to screen. When
#'   `NULL`, all numeric columns are used.
#' @param method Outlier rule. `"zscore"` flags absolute z scores above
#'   `z_cut`, `"iqr"` flags values outside Tukey fences, and `"mahalanobis"`
#'   flags rows above the chi-square cutoff for the selected variables.
#' @param z_cut Numeric cutoff for `"zscore"`. Defaults to `3`.
#' @param iqr_multiplier Numeric multiplier for `"iqr"` fences. Defaults to
#'   `1.5`.
#' @param p_cut Probability cutoff for `"mahalanobis"`. Defaults to `0.975`.
#'
#' @return An object of class `sframe_outlier_report` with the method, screened
#'   variables, a result table, flagged row numbers, and a reporting prompt.
#' @export
#'
#' @examples
#' demo <- sframe_demo_data()
#' outliers <- outlier_report(
#'   demo$responses,
#'   variables = c("dm_1", "dm_2", "sat_1"),
#'   method = "zscore"
#' )
#' outliers$flagged_rows
outlier_report <- function(
    data,
    variables = NULL,
    method = c("zscore", "iqr", "mahalanobis"),
    z_cut = 3,
    iqr_multiplier = 1.5,
    p_cut = 0.975
) {
  stopifnot(is.data.frame(data))
  method <- rlang::arg_match(method)
  variables <- variables %||% names(data)[vapply(data, is.numeric, logical(1))]
  variables <- intersect(variables, names(data))
  if (length(variables) == 0L) {
    rlang::abort(
      "`variables` must include at least one column in `data`.",
      class = c("sframe_validation_error", "sframe_error")
    )
  }

  numeric_data <- as.data.frame(lapply(data[variables], function(x) {
    suppressWarnings(as.numeric(x))
  }))

  if (method == "zscore") {
    rows <- do.call(rbind, lapply(names(numeric_data), function(var) {
      x <- numeric_data[[var]]
      sx <- stats::sd(x, na.rm = TRUE)
      z <- if (is.finite(sx) && sx > 0) {
        (x - mean(x, na.rm = TRUE)) / sx
      } else {
        rep(NA_real_, length(x))
      }
      data.frame(
        row = seq_along(x),
        variable = var,
        value = x,
        statistic = z,
        threshold = z_cut,
        flagged = abs(z) > z_cut,
        stringsAsFactors = FALSE
      )
    }))
  } else if (method == "iqr") {
    rows <- do.call(rbind, lapply(names(numeric_data), function(var) {
      x <- numeric_data[[var]]
      qs <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE, names = FALSE)
      iqr <- qs[[2]] - qs[[1]]
      low <- qs[[1]] - iqr_multiplier * iqr
      high <- qs[[2]] + iqr_multiplier * iqr
      data.frame(
        row = seq_along(x),
        variable = var,
        value = x,
        statistic = x,
        threshold = paste0("[", signif(low, 4), ", ", signif(high, 4), "]"),
        flagged = x < low | x > high,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    complete <- stats::complete.cases(numeric_data)
    x <- numeric_data[complete, , drop = FALSE]
    if (nrow(x) <= ncol(x)) {
      rlang::abort(
        "Mahalanobis screening needs more complete rows than variables.",
        class = c("sframe_validation_error", "sframe_error")
      )
    }
    cov_mat <- stats::cov(x)
    inv <- tryCatch(solve(cov_mat), error = function(e) NULL)
    if (is.null(inv)) {
      rlang::abort(
        "Mahalanobis screening needs a non-singular covariance matrix.",
        class = c("sframe_validation_error", "sframe_error")
      )
    }
    distances <- rep(NA_real_, nrow(data))
    distances[complete] <- stats::mahalanobis(x, colMeans(x), cov_mat)
    cutoff <- stats::qchisq(p_cut, df = ncol(x))
    rows <- data.frame(
      row = seq_len(nrow(data)),
      variable = paste(variables, collapse = ", "),
      value = NA_real_,
      statistic = distances,
      threshold = cutoff,
      flagged = distances > cutoff,
      stringsAsFactors = FALSE
    )
  }

  flagged_rows <- sort(unique(rows$row[rows$flagged %in% TRUE]))
  structure(
    list(
      method = method,
      variables = variables,
      table = rows,
      flagged_rows = flagged_rows,
      prompt = "Review flagged observations before modelling and document any exclusion rule."
    ),
    class = "sframe_outlier_report"
  )
}

#' Small-sample advisory text
#'
#' Builds a short advisory note when a sample falls below the conventional
#' n = 30 threshold at which asymptotic approximations become unreliable.
#'
#' @param n Integer. The sample size.
#' @param test Character. A short description of the analysis the advisory
#'   applies to, inserted into the message.
#'
#' @return A single character string, or NULL when `n` is 30 or more.
#' @keywords internal
sframe_small_sample_advisory <- function(n, test) {
  if (length(n) != 1 || is.na(n)) return(NULL)
  if (n < 30) {
    sprintf(
      paste0(
        "Small sample detected (n = %d). For %s, consider non-parametric ",
        "alternatives. Asymptotic p-values may be unreliable. ",
        "Bootstrap or exact confidence intervals are provided where available."
      ),
      as.integer(n), test
    )
  } else {
    NULL
  }
}

#' Assumption-check report
#'
#' Performs common assumption checks for survey analyses using base R where
#' possible: Shapiro-Wilk tests, skewness/kurtosis screening, Levene and
#' Brown-Forsythe tests, regression residual checks, VIF, Cook's distance,
#' expected-count checks, and sparse-cell warnings.
#'
#' @param data A data.frame.
#' @param variables Numeric variables for normality screening.
#' @param group Optional grouping variable for Levene/Brown-Forsythe tests.
#' @param outcome Optional regression outcome.
#' @param predictors Optional regression predictors.
#' @param table_vars Optional two categorical variables for expected-count
#'   checks.
#'
#' @return An object of class `sframe_assumption_report`.
#' @export
#' @examples
#' demo <- sframe_demo_data()
#' ar <- assumption_report(demo$responses, variables = c("sat_1", "sat_2"),
#'                          group = "visit_type")
#' print(ar)
assumption_report <- function(
    data,
    variables = NULL,
    group = NULL,
    outcome = NULL,
    predictors = NULL,
    table_vars = NULL
) {
  stopifnot(is.data.frame(data))
  variables <- variables %||% character(0)
  variables <- intersect(variables, colnames(data))
  normality <- lapply(variables, function(var) {
    x <- sframe_nonmissing(sframe_num(data[[var]]))
    sh <- if (length(x) >= 3 && length(x) <= 5000) {
      tryCatch(stats::shapiro.test(x), error = function(e) NULL)
    } else {
      NULL
    }
    data.frame(
      variable = var,
      n = length(x),
      shapiro_w = if (!is.null(sh)) unname(sh$statistic) else NA_real_,
      shapiro_p = if (!is.null(sh)) sh$p.value else NA_real_,
      skewness = sframe_skewness(x),
      kurtosis = sframe_kurtosis(x),
      stringsAsFactors = FALSE
    )
  })
  normality <- if (length(normality)) do.call(rbind, normality) else data.frame()

  homogeneity <- data.frame()
  if (!is.null(group) && group %in% colnames(data) && length(variables) > 0) {
    homogeneity <- do.call(rbind, lapply(variables, function(var) {
      x <- sframe_num(data[[var]])
      g <- as.factor(data[[group]])
      ok <- !is.na(x) & !is.na(g)
      if (sum(ok) < 3 || nlevels(droplevels(g[ok])) < 2) {
        return(data.frame(variable = var, test = c("Levene", "Brown-Forsythe"),
                          F = NA_real_, p = NA_real_, stringsAsFactors = FALSE))
      }
      centred_mean <- abs(x[ok] - stats::ave(x[ok], g[ok], FUN = mean))
      centred_median <- abs(x[ok] - stats::ave(x[ok], g[ok], FUN = stats::median))
      lev <- summary(stats::aov(centred_mean ~ g[ok]))[[1]]
      bf <- summary(stats::aov(centred_median ~ g[ok]))[[1]]
      data.frame(
        variable = var,
        test = c("Levene", "Brown-Forsythe"),
        F = c(lev[["F value"]][1], bf[["F value"]][1]),
        p = c(lev[["Pr(>F)"]][1], bf[["Pr(>F)"]][1]),
        stringsAsFactors = FALSE
      )
    }))
  }

  regression <- NULL
  if (!is.null(outcome) && outcome %in% colnames(data) &&
      length(predictors %||% character(0)) > 0) {
    predictors <- intersect(predictors, colnames(data))
    reg_data <- data[, c(outcome, predictors), drop = FALSE]
    reg_data[[outcome]] <- sframe_num(reg_data[[outcome]])
    for (p in predictors) reg_data[[p]] <- sframe_num(reg_data[[p]])
    reg_data <- reg_data[stats::complete.cases(reg_data), , drop = FALSE]
    if (nrow(reg_data) > length(predictors) + 2) {
      fit <- tryCatch(
        stats::lm(stats::as.formula(paste(outcome, "~", paste(predictors, collapse = " + "))),
                  data = reg_data),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        residuals <- stats::residuals(fit)
        sh <- if (length(residuals) >= 3 && length(residuals) <= 5000) {
          tryCatch(stats::shapiro.test(residuals), error = function(e) NULL)
        } else NULL
        vif <- stats::setNames(rep(NA_real_, length(predictors)), predictors)
        for (p in predictors) {
          others <- setdiff(predictors, p)
          if (length(others) > 0) {
            vfit <- tryCatch(
              stats::lm(stats::as.formula(paste(p, "~", paste(others, collapse = " + "))),
                        data = reg_data),
              error = function(e) NULL
            )
            if (!is.null(vfit)) {
              r2 <- summary(vfit)$r.squared
              vif[[p]] <- if (r2 < 1) 1 / (1 - r2) else Inf
            }
          } else {
            vif[[p]] <- 1
          }
        }
        regression <- list(
          n = nrow(reg_data),
          residual_shapiro_w = if (!is.null(sh)) unname(sh$statistic) else NA_real_,
          residual_shapiro_p = if (!is.null(sh)) sh$p.value else NA_real_,
          vif = vif,
          cooks_distance = stats::cooks.distance(fit),
          standardised_residuals = stats::rstandard(fit)
        )
      }
    }
  }

  expected_counts <- NULL
  if (length(table_vars %||% character(0)) >= 2 &&
      all(table_vars[1:2] %in% colnames(data))) {
    tbl <- table(data[[table_vars[1]]], data[[table_vars[2]]])
    ct <- suppressWarnings(tryCatch(stats::chisq.test(tbl), error = function(e) NULL))
    expected_counts <- list(
      table = tbl,
      expected = if (!is.null(ct)) ct$expected else NULL,
      sparse_warning = if (!is.null(ct)) any(ct$expected < 5) else TRUE
    )
  }

  advisory_n <- if (!is.null(regression)) {
    regression$n
  } else if (nrow(normality) > 0) {
    max(normality$n, na.rm = TRUE)
  } else {
    nrow(data)
  }
  advisory <- sframe_small_sample_advisory(advisory_n, "these assumption checks")

  structure(
    list(
      method = "assumptions",
      normality = normality,
      homogeneity = homogeneity,
      regression = regression,
      expected_counts = expected_counts,
      advisory = advisory,
      apa = "Assumption checks were computed.",
      prompt = "Report assumption checks before interpreting inferential models, especially sparse cells, non-normal residuals, and high VIF values."
    ),
    class = "sframe_assumption_report"
  )
}

#' @exportS3Method print sframe_assumption_report
print.sframe_assumption_report <- function(x, ...) {
  cat("Assumption Report\n")
  if (nrow(x$normality) > 0) {
    cat("\nNormality:\n")
    print(x$normality, row.names = FALSE)
  }
  if (nrow(x$homogeneity) > 0) {
    cat("\nHomogeneity of variance:\n")
    print(x$homogeneity, row.names = FALSE)
  }
  if (!is.null(x$regression)) {
    cat(sprintf("\nRegression residuals: n = %d, Shapiro-Wilk p = %.3f\n",
                x$regression$n, x$regression$residual_shapiro_p))
  }
  if (isTRUE(x$expected_counts$sparse_warning)) {
    cat("\nWarning: sparse expected cell counts detected.\n")
  }
  if (!is.null(x$advisory)) cat(sprintf("\nNote: %s\n", x$advisory))
  invisible(x)
}

#' Post-hoc and pairwise comparison report
#'
#' @param data A data.frame.
#' @param method Comparison family. Supports `"anova"`, `"kruskal_wallis"`,
#'   `"chi_square"`, and `"cochran_q"`.
#' @param outcome Outcome variable for group comparisons.
#' @param group Grouping variable for group comparisons.
#' @param table_vars Two categorical variables for chi-square residuals and
#'   pairwise proportion tests.
#' @param measures Repeated binary measures for pairwise McNemar tests.
#' @param correction Multiple-comparison correction.
#'
#' @return An object of class `sframe_posthoc_report`.
#' @export
#' @examples
#' demo <- sframe_demo_data()
#' pr <- posthoc_report(demo$responses, method = "kruskal_wallis",
#'                       outcome = "sat_1", group = "visit_type")
#' pr$tables$pairwise_wilcox
posthoc_report <- function(
    data,
    method = c("anova", "kruskal_wallis", "chi_square", "cochran_q"),
    outcome = NULL,
    group = NULL,
    table_vars = NULL,
    measures = NULL,
    correction = c("holm", "bonferroni", "BH")
) {
  stopifnot(is.data.frame(data))
  method <- rlang::arg_match(method)
  correction <- rlang::arg_match(correction)
  tables <- list()
  warnings <- character(0)

  if (method == "anova" && !is.null(outcome) && !is.null(group)) {
    x <- sframe_num(data[[outcome]])
    g <- as.factor(data[[group]])
    ok <- !is.na(x) & !is.na(g)
    fit <- tryCatch(stats::aov(x[ok] ~ g[ok]), error = function(e) NULL)
    if (!is.null(fit)) {
      tables$tukey <- tryCatch(as.data.frame(stats::TukeyHSD(fit)[[1]]),
                               error = function(e) NULL)
      tables$pairwise_t <- stats::pairwise.t.test(x[ok], g[ok],
                                                  p.adjust.method = correction)
    }
  }
  if (method == "kruskal_wallis" && !is.null(outcome) && !is.null(group)) {
    x <- sframe_num(data[[outcome]])
    g <- as.factor(data[[group]])
    ok <- !is.na(x) & !is.na(g)
    tables$pairwise_wilcox <- stats::pairwise.wilcox.test(x[ok], g[ok],
                                                          p.adjust.method = correction)
    warnings <- c(warnings,
      "Dunn post-hoc tests require an optional package; surveyframe skips them by default.")
  }
  if (method == "chi_square" && length(table_vars %||% character(0)) >= 2) {
    tbl <- table(data[[table_vars[1]]], data[[table_vars[2]]])
    ct <- suppressWarnings(stats::chisq.test(tbl, correct = FALSE))
    expected <- ct$expected
    row_prop <- rowSums(tbl) / sum(tbl)
    col_prop <- colSums(tbl) / sum(tbl)
    adjusted <- (tbl - expected) / sqrt(expected * (1 - row_prop) %o% (1 - col_prop))
    tables$adjusted_residuals <- as.data.frame.matrix(adjusted)
    tables$pairwise_proportions <- tryCatch(
      stats::pairwise.prop.test(tbl[, 1], rowSums(tbl), p.adjust.method = correction),
      error = function(e) NULL
    )
  }
  if (method == "cochran_q" && length(measures %||% character(0)) >= 2) {
    pairs <- utils::combn(measures, 2, simplify = FALSE)
    tables$pairwise_mcnemar <- do.call(rbind, lapply(pairs, function(pair) {
      tbl <- table(data[[pair[1]]], data[[pair[2]]])
      mt <- tryCatch(stats::mcnemar.test(tbl), error = function(e) NULL)
      data.frame(
        measure_1 = pair[1],
        measure_2 = pair[2],
        statistic = if (!is.null(mt)) unname(mt$statistic) else NA_real_,
        p = if (!is.null(mt)) mt$p.value else NA_real_,
        stringsAsFactors = FALSE
      )
    }))
    tables$pairwise_mcnemar$p_adjusted <- stats::p.adjust(
      tables$pairwise_mcnemar$p,
      method = correction
    )
  }

  structure(
    list(
      method = paste0("posthoc_", method),
      correction = correction,
      tables = tables,
      warnings = warnings,
      apa = sprintf("Post-hoc comparisons used %s correction where applicable.", correction),
      prompt = "Report post-hoc tests only after the omnibus test and include the multiple-comparison correction."
    ),
    class = "sframe_posthoc_report"
  )
}

# Reads 1/0, TRUE/FALSE and yes/no, in any case, as 1 and 0, keeping missing
# values missing. Anything else is reported. Testing membership of a list of
# positive codes turned NA into 0 before complete-case filtering, so a missing
# response counted as an observed failure, and so did a code such as 2.
sframe_binary_columns <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  out <- list()
  for (col in names(df)) {
    raw <- trimws(tolower(as.character(df[[col]])))
    raw[!is.na(raw) & raw == ""] <- NA
    vals <- rep(NA_integer_, length(raw))
    vals[raw %in% c("1", "true", "yes")] <- 1L
    vals[raw %in% c("0", "false", "no")] <- 0L
    bad <- unique(as.character(df[[col]])[!is.na(raw) & is.na(vals)])
    if (length(bad) > 0) {
      return(list(error = paste0(
        "Cochran's Q needs binary responses coded 1/0, TRUE/FALSE or yes/no. ",
        "Column '", col, "' holds ", paste(utils::head(bad, 5), collapse = ", "),
        ".")))
    }
    out[[col]] <- vals
  }
  list(matrix = do.call(cbind, out))
}

sframe_cochran_q <- function(mat) {
  coded <- sframe_binary_columns(mat)
  if (!is.null(coded$error)) return(list(error = coded$error))
  mat <- coded$matrix
  mat <- mat[stats::complete.cases(mat), , drop = FALSE]
  n <- nrow(mat)
  k <- ncol(mat)
  if (n < 2 || k < 2) {
    return(list(error = "Cochran's Q requires at least two complete rows and two measures."))
  }
  col_sum <- colSums(mat)
  row_sum <- rowSums(mat)
  numerator <- (k - 1) * (k * sum(col_sum^2) - sum(col_sum)^2)
  denominator <- k * sum(row_sum) - sum(row_sum^2)
  if (denominator == 0) {
    return(list(error = paste0(
      "Cochran's Q is undefined for these data: every complete respondent ",
      "gave the same answer to all measures, so there is no within-respondent ",
      "variation to test.")))
  }
  q <- numerator / denominator
  p <- stats::pchisq(q, df = k - 1, lower.tail = FALSE)
  list(statistic = q, df = k - 1, p = p, n = n)
}

sframe_cramers_v <- function(tbl, chi_sq = NULL) {
  if (is.null(chi_sq)) {
    chi_sq <- suppressWarnings(stats::chisq.test(tbl, correct = FALSE)$statistic)
  }
  n <- sum(tbl)
  denom <- n * min(nrow(tbl) - 1, ncol(tbl) - 1)
  if (denom <= 0) return(NA_real_)
  sqrt(unname(chi_sq) / denom)
}

sframe_phi <- function(tbl, chi_sq = NULL) {
  if (!all(dim(tbl) == c(2, 2))) return(NA_real_)
  if (is.null(chi_sq)) {
    chi_sq <- suppressWarnings(stats::chisq.test(tbl, correct = FALSE)$statistic)
  }
  sqrt(unname(chi_sq) / sum(tbl))
}

sframe_run_descriptives_result <- function(data, roles, options = list()) {
  vars <- sframe_role_values(roles, c("variables", "items", "scales"))
  split_by <- sframe_role_values(roles, c("split_by", "group"), NULL)[1]
  weights <- sframe_role_values(roles, c("weights", "weight"), NULL)[1]
  out <- descriptives_report(
    data,
    variables = if (length(vars)) vars else NULL,
    split_by = split_by,
    conf_level = options$conf_level %||% 0.95,
    weights = weights
  )
  # report_obj keeps the classed object for sframe_plot_for_result() to
  # dispatch plot() on, same pattern as sframe_result_from_report() elsewhere:
  # the unclassed, merged result list is not safe to treat as report-shaped.
  c(unclass(out), list(test = "descriptives", report_obj = out))
}

sframe_run_missing_result <- function(data, instrument, roles) {
  vars <- sframe_role_values(roles, c("variables", "items"))
  out <- missing_data_report(data, instrument = instrument, variables = if (length(vars)) vars else NULL)
  # report_obj keeps the original classed object so sframe_plot_for_result()
  # can dispatch plot.sframe_missing_data_report() on it, the same pattern
  # sframe_result_from_report() uses for quality/reliability/EFA.
  c(unclass(out), list(test = "missing_data", table = out$item_missing, report_obj = out))
}

sframe_run_fisher <- function(data, roles, options = list()) {
  vars <- sframe_role_values(roles, c("row", "column", "variables"))
  err <- sframe_require_columns(data, vars[1:2], "Fisher's exact test")
  if (!is.null(err)) return(list(test = "fisher_exact", error = err))
  tbl <- table(data[[vars[1]]], data[[vars[2]]])
  # conf.int = TRUE errors when combined with simulate.p.value = TRUE in base
  # R, so the exact odds-ratio CI is only requested when simulation is off.
  # ft$conf.int is NULL for tables larger than 2x2 regardless of want_ci,
  # since Fisher's exact CI is only defined for a 2x2 table; this degrades
  # gracefully to odds_ratio_conf_int = NULL with no extra handling needed.
  want_ci <- !isTRUE(options$simulate_p_value)
  ft <- tryCatch(
    stats::fisher.test(tbl, simulate.p.value = isTRUE(options$simulate_p_value),
                        conf.int = want_ci),
    error = function(e) NULL
  )
  if (is.null(ft)) return(list(test = "fisher_exact", error = "Fisher's exact test failed."))
  effect <- if (all(dim(tbl) == c(2, 2))) sframe_phi(tbl) else sframe_cramers_v(tbl)
  effect_name <- if (all(dim(tbl) == c(2, 2))) "phi" else "Cramer's V"
  list(
    test = "fisher_exact",
    vars = vars[1:2],
    n = sum(tbl),
    table = as.data.frame.matrix(tbl),
    p = ft$p.value,
    odds_ratio = unname(ft$estimate %||% NA_real_),
    odds_ratio_conf_int = if (want_ci) as.numeric(ft$conf.int) else NULL,
    effect = effect,
    effect_name = effect_name,
    apa = sprintf("Fisher's exact test, p %s, %s = %.2f",
                  sframe_p_string(ft$p.value), effect_name, effect),
    prompt = "Use Fisher's exact test when expected counts are sparse, especially for 2 x 2 tables."
  )
}

sframe_run_mcnemar <- function(data, roles, options = list()) {
  vars <- sframe_role_values(roles, c("before", "after", "variables"))
  err <- sframe_require_columns(data, vars[1:2], "McNemar test")
  if (!is.null(err)) return(list(test = "mcnemar", error = err))
  tbl <- table(data[[vars[1]]], data[[vars[2]]])
  mt <- tryCatch(stats::mcnemar.test(tbl, correct = isTRUE(options$correct %||% TRUE)),
                 error = function(e) NULL)
  if (is.null(mt)) return(list(test = "mcnemar", error = "McNemar test failed."))
  list(
    test = "mcnemar",
    vars = vars[1:2],
    table = as.data.frame.matrix(tbl),
    statistic = unname(mt$statistic),
    df = unname(mt$parameter),
    p = mt$p.value,
    apa = sprintf("McNemar's chi-square(%d) = %.2f, p %s",
                  unname(mt$parameter), unname(mt$statistic), sframe_p_string(mt$p.value)),
    prompt = "Interpret McNemar's test as a related-samples change in paired categorical responses."
  )
}

sframe_run_cochran_q <- function(data, roles) {
  vars <- sframe_role_values(roles, c("measures", "variables"))
  err <- sframe_require_columns(data, vars, "Cochran's Q")
  if (!is.null(err)) return(list(test = "cochran_q", error = err))
  cq <- sframe_cochran_q(data[, vars, drop = FALSE])
  if (!is.null(cq$error)) return(c(list(test = "cochran_q"), cq))
  list(
    test = "cochran_q",
    vars = vars,
    n = cq$n,
    Q = cq$statistic,
    df = cq$df,
    p = cq$p,
    apa = sprintf("Cochran's Q(%d, N = %d) = %.2f, p %s",
                  cq$df, cq$n, cq$statistic, sframe_p_string(cq$p)),
    prompt = "If Cochran's Q is significant, report pairwise McNemar comparisons with correction."
  )
}

sframe_run_friedman <- function(data, roles) {
  vars <- sframe_role_values(roles, c("measures", "variables"))
  err <- sframe_require_columns(data, vars, "Friedman test")
  if (!is.null(err)) return(list(test = "friedman", error = err))
  mat <- as.matrix(as.data.frame(lapply(data[, vars, drop = FALSE], sframe_num)))
  mat <- mat[stats::complete.cases(mat), , drop = FALSE]
  ft <- tryCatch(stats::friedman.test(mat), error = function(e) NULL)
  if (is.null(ft)) return(list(test = "friedman", error = "Friedman test failed."))
  list(
    test = "friedman",
    vars = vars,
    n = nrow(mat),
    statistic = unname(ft$statistic),
    df = unname(ft$parameter),
    p = ft$p.value,
    apa = sprintf("Friedman chi-square(%d, N = %d) = %.2f, p %s",
                  unname(ft$parameter), nrow(mat), unname(ft$statistic),
                  sframe_p_string(ft$p.value)),
    prompt = "Interpret Friedman as a related-samples rank test across repeated survey measures."
  )
}

sframe_run_repeated_anova <- function(data, roles) {
  vars <- sframe_role_values(roles, c("measures", "variables"))
  err <- sframe_require_columns(data, vars, "Repeated-measures ANOVA")
  if (!is.null(err)) return(list(test = "repeated_anova", error = err))
  mat <- as.data.frame(lapply(data[, vars, drop = FALSE], sframe_num))
  complete_subject <- stats::complete.cases(mat[, vars, drop = FALSE])
  n_excluded_incomplete <- sum(!complete_subject)
  mat <- mat[complete_subject, , drop = FALSE]
  if (nrow(mat) < 2L) {
    return(list(
      test = "repeated_anova",
      error = paste0("Repeated-measures ANOVA needs at least 2 respondents ",
                     "with complete measurements."),
      vars = vars, n = nrow(mat),
      n_excluded_incomplete = n_excluded_incomplete
    ))
  }
  # .subject must be a factor. Left as an integer, aov() treats it as a
  # continuous covariate, the Error(.subject / condition) stratification
  # collapses to a single residual df, and the condition effect is tested
  # against the wrong error term (F came out around 1.45 where
  # jmv::anovaRM() gives 86.93 on the same data).
  mat$.subject <- factor(seq_len(nrow(mat)))
  long <- stats::reshape(
    mat,
    varying = vars,
    v.names = "value",
    timevar = "condition",
    times = vars,
    direction = "long"
  )
  long <- long[!is.na(long$value), , drop = FALSE]
  fit <- tryCatch(stats::aov(value ~ condition + Error(.subject / condition), data = long),
                  error = function(e) NULL)
  if (is.null(fit)) return(list(test = "repeated_anova", error = "Repeated-measures ANOVA failed."))
  fit_summary <- utils::capture.output(summary(fit))
  # Error(.subject / condition) always expands to exactly 2 strata, in this
  # fixed order: "Error: .subject" (a between-subjects leftover, degenerate
  # for a purely within-subject design) and "Error: .subject:condition" (the
  # real within-subject test). Both strata carry a row literally named
  # "condition" with a non-NA F, so taking the *first* match, as an earlier
  # version of this function did, silently returns the wrong one: on the
  # bundled demo data it reported F(1, 118) = 0.01, p = 0.916 (not
  # significant) from the ".subject" stratum, where the correct
  # ".subject:condition" stratum gives F(2, 234) = 30.66, p < .001 (highly
  # significant). Do not `break` on the first match; keep searching so the
  # last (innermost) stratum wins, which is always the within-subject one
  # for this fixed model structure.
  smry <- summary(fit)
  stat_row <- NULL
  within   <- NULL
  for (stratum in smry) {
    tab <- stratum[[1]]
    if (is.null(tab) || !"F value" %in% colnames(tab)) next
    cond_row <- which(trimws(rownames(tab)) == "condition")
    if (length(cond_row) && !is.na(tab[cond_row, "F value"])) {
      within   <- tab
      stat_row <- tab[cond_row, , drop = FALSE]
    }
  }
  if (is.null(stat_row)) {
    return(list(
      test = "repeated_anova", vars = vars, n = length(unique(long$.subject)),
      n_excluded_incomplete = n_excluded_incomplete,
      fit_summary = fit_summary,
      apa = "Repeated-measures ANOVA was estimated; inspect `fit_summary` for the within-subject effect.",
      prompt = "Report the within-subject effect, degrees of freedom, p value, effect size where available, and sphericity limitations."
    ))
  }
  ss_condition <- stat_row[["Sum Sq"]]
  ss_resid <- within[["Sum Sq"]][nrow(within)]
  df1 <- stat_row[["Df"]]
  df2 <- within[["Df"]][nrow(within)]
  F_stat <- stat_row[["F value"]]
  p <- stat_row[["Pr(>F)"]]
  partial_eta2 <- ss_condition / (ss_condition + ss_resid)
  list(
    test = "repeated_anova",
    vars = vars,
    n = length(unique(long$.subject)),
    n_excluded_incomplete = n_excluded_incomplete,
    df1 = df1, df2 = df2, F_stat = F_stat, p = p, eta2 = partial_eta2,
    table = data.frame(
      Statistic = "F", df1 = df1, df2 = df2,
      Estimate = sprintf("%.2f", F_stat), p = sframe_p_string(p),
      `Partial eta squared` = sprintf("%.3f", partial_eta2),
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    fit_summary = fit_summary,
    apa = sprintf("F(%d, %d) = %.2f, p %s, partial \u03B7\u00B2 = %.3f",
                  df1, df2, F_stat, sframe_p_string(p), partial_eta2),
    prompt = "Report the within-subject F, degrees of freedom, p value, and partial eta-squared."
  )
}

sframe_run_kendall <- function(data, roles) {
  vars <- sframe_role_values(roles, c("x", "y", "variables"))
  sframe_run_correlation(data, vars[1:2], "kendall")
}

sframe_run_partial_correlation <- function(data, roles, options = list()) {
  x <- sframe_role_values(roles, c("x", "predictor"))[1]
  y <- sframe_role_values(roles, c("y", "outcome", "dependent"))[1]
  controls <- sframe_role_values(roles, c("controls", "covariates", "control"))
  method <- options$method %||% "pearson"
  if (!method %in% c("pearson", "spearman")) {
    return(list(test = "partial_correlation",
                error = paste0("Partial correlation supports method 'pearson' or ",
                               "'spearman', not '", method, "'.")))
  }
  vars <- c(x, y, controls)
  err <- sframe_require_columns(data, vars, "Partial correlation")
  if (!is.null(err)) return(list(test = "partial_correlation", error = err))
  df <- as.data.frame(lapply(data[, vars, drop = FALSE], sframe_num))
  df <- df[stats::complete.cases(df), , drop = FALSE]
  if (nrow(df) < length(controls) + 4) {
    return(list(test = "partial_correlation",
                error = "Partial correlation requires more complete rows."))
  }
  # Spearman ranks every variable first, then partials the ranks. Ranking the
  # residuals, as before, is a different statistic.
  if (method == "spearman") df <- as.data.frame(lapply(df, rank))
  if (length(controls) > 0) {
    z <- as.matrix(df[controls])
    if (qr(cbind(1, z))$rank < ncol(z) + 1) {
      return(list(test = "partial_correlation",
                  error = "The control variables are collinear, so they cannot all be partialled out."))
    }
    rx <- stats::residuals(stats::lm(df[[x]] ~ z))
    ry <- stats::residuals(stats::lm(df[[y]] ~ z))
  } else {
    rx <- df[[x]]
    ry <- df[[y]]
  }
  r <- stats::cor(rx, ry)
  if (!is.finite(r)) return(list(test = "partial_correlation", error = "Partial correlation failed."))
  # Inference on n - 2 - k degrees of freedom, k being the number of controls.
  # The p value was taken from an ordinary correlation test on n - 2 while the
  # printed degrees of freedom said n - 2 - k.
  df_resid <- nrow(df) - 2L - length(controls)
  t_stat <- r * sqrt(df_resid / (1 - r^2))
  p_val <- 2 * stats::pt(-abs(t_stat), df_resid)
  list(
    test = "partial_correlation",
    vars = vars,
    method = method,
    n = nrow(df),
    r = r,
    df = df_resid,
    t = t_stat,
    p = p_val,
    controls = controls,
    apa = sprintf("partial r(%d) = %.2f, p %s",
                  df_resid, r, sframe_p_string(p_val)),
    prompt = "Interpret the partial correlation after accounting for the specified control variables."
  )
}

sframe_run_correlation_matrix <- function(data, vars, method = "pearson", use = "pairwise.complete.obs") {
  err <- sframe_require_columns(data, vars, "Correlation matrix")
  if (!is.null(err)) return(list(test = paste0("correlation_", method), error = err))
  mat <- as.data.frame(lapply(data[, vars, drop = FALSE], sframe_num))
  r <- stats::cor(mat, method = method, use = use)
  p <- matrix(NA_real_, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))
  for (i in seq_along(vars)) {
    for (j in seq_along(vars)) {
      if (i < j) {
        ct <- tryCatch(stats::cor.test(mat[[i]], mat[[j]], method = method),
                       error = function(e) NULL)
        if (!is.null(ct)) p[i, j] <- p[j, i] <- ct$p.value
      }
    }
  }
  list(
    test = paste0("correlation_", method),
    vars = vars,
    method = method,
    correlation_matrix = r,
    p_matrix = p,
    apa = sprintf("%s correlation matrix computed for %d variables.", method, length(vars)),
    prompt = "Report the correlation matrix with pairwise/listwise missing-data handling clearly stated."
  )
}

sframe_run_anova_two <- function(data, roles) {
  f1 <- sframe_role_values(roles, c("factor1", "factor_a"))[1]
  f2 <- sframe_role_values(roles, c("factor2", "factor_b"))[1]
  outcome <- sframe_role_values(roles, c("outcome", "dependent"))[1]
  vars <- c(f1, f2, outcome)
  err <- sframe_require_columns(data, vars, "Two-way ANOVA")
  if (!is.null(err)) return(list(test = "anova_two", error = err))
  df <- data[, vars, drop = FALSE]
  df[[outcome]] <- sframe_num(df[[outcome]])
  df[[f1]] <- as.factor(df[[f1]])
  df[[f2]] <- as.factor(df[[f2]])
  df <- df[stats::complete.cases(df), , drop = FALSE]
  fit <- tryCatch(stats::aov(stats::as.formula(paste(outcome, "~", f1, "*", f2)), data = df),
                  error = function(e) NULL)
  if (is.null(fit)) return(list(test = "anova_two", error = "Two-way ANOVA failed."))
  tab <- summary(fit)[[1]]
  ss_error <- tab[["Sum Sq"]][nrow(tab)]
  partial_eta <- tab[["Sum Sq"]] / (tab[["Sum Sq"]] + ss_error)
  # The residual row is not a tested effect, and its formula gave 0.5.
  partial_eta[nrow(tab)] <- NA_real_
  table <- data.frame(
    effect = trimws(rownames(tab)),
    df = tab[["Df"]],
    sum_sq = tab[["Sum Sq"]],
    mean_sq = tab[["Mean Sq"]],
    F = tab[["F value"]],
    p = tab[["Pr(>F)"]],
    partial_eta_sq = partial_eta,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  list(
    test = "anova_two",
    vars = vars,
    table = table,
    ss_type = "sequential (Type I)",
    apa = paste0("Two-way ANOVA estimated main effects and the interaction term, ",
                 "with sequential (Type I) sums of squares, so in an unbalanced ",
                 "design the first factor is tested before the second."),
    prompt = "Report both main effects and the interaction before interpreting simple effects."
  )
}

sframe_run_ancova <- function(data, roles) {
  group <- sframe_role_values(roles, c("group", "factor"))[1]
  outcome <- sframe_role_values(roles, c("outcome", "dependent"))[1]
  covariates <- sframe_role_values(roles, c("covariates", "covariate"))
  vars <- c(group, outcome, covariates)
  err <- sframe_require_columns(data, vars, "ANCOVA")
  if (!is.null(err)) return(list(test = "ancova", error = err))
  if (length(covariates) == 0) {
    return(list(test = "ancova", error = "ANCOVA requires at least one covariate."))
  }
  df <- data[, vars, drop = FALSE]
  df[[outcome]] <- sframe_num(df[[outcome]])
  df[[group]] <- as.factor(df[[group]])
  for (cov in covariates) df[[cov]] <- sframe_num(df[[cov]])
  df <- df[stats::complete.cases(df), , drop = FALSE]
  formula <- stats::as.formula(paste(outcome, "~", group, "+", paste(covariates, collapse = " + ")))
  fit <- tryCatch(stats::lm(formula, data = df), error = function(e) NULL)
  if (is.null(fit)) return(list(test = "ancova", error = "ANCOVA failed."))
  # Each term is tested after every other term, by comparing the full model
  # with the model dropping that term. A sequential aov() table with group
  # first tested the group difference before the covariates, which is an
  # unadjusted test labelled as adjusted.
  adj <- tryCatch(stats::drop1(fit, test = "F"), error = function(e) NULL)
  if (is.null(adj)) return(list(test = "ancova", error = "ANCOVA failed."))
  terms <- trimws(rownames(adj))[-1]
  res_df <- stats::df.residual(fit)
  res_ss <- sum(stats::residuals(fit)^2)
  table <- data.frame(
    effect  = c(terms, "Residuals"),
    df      = c(adj$Df[-1], res_df),
    sum_sq  = c(adj$`Sum of Sq`[-1], res_ss),
    mean_sq = c(adj$`Sum of Sq`[-1] / adj$Df[-1], res_ss / res_df),
    F       = c(adj$`F value`[-1], NA_real_),
    p       = c(adj$`Pr(>F)`[-1], NA_real_),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  slope_warning <- character(0)
  slope_fit <- tryCatch(
    stats::aov(stats::as.formula(paste0(outcome, " ~ ", group, " * (",
                                        paste(covariates, collapse = " + "), ")")),
               data = df),
    error = function(e) NULL
  )
  if (!is.null(slope_fit)) {
    slope_warning <- "Check homogeneity of regression slopes using the interaction model stored in `slope_model_summary`."
  }
  g <- table[table$effect == group, ]
  list(
    test = "ancova",
    vars = vars,
    n = nrow(df),
    table = table,
    ss_type = "adjusted (each term tested after all others)",
    slope_warning = slope_warning,
    slope_model_summary = if (!is.null(slope_fit)) utils::capture.output(summary(slope_fit)) else NULL,
    apa = sprintf("Adjusted for %s, the group effect was F(%d, %d) = %.2f, p %s.",
                  paste(covariates, collapse = " and "), as.integer(g$df), as.integer(res_df),
                  g$F, sframe_p_string(g$p)),
    prompt = "Report adjusted group effects and state whether homogeneity of regression slopes was checked."
  )
}

sframe_logistic_fit_table <- function(fit) {
  co <- as.data.frame(summary(fit)$coefficients)
  ci <- tryCatch(stats::confint(fit), error = function(e) NULL)
  co$odds_ratio <- exp(co[[1]])
  if (!is.null(ci) && nrow(ci) == nrow(co)) {
    co$or_ci_low <- exp(ci[, 1])
    co$or_ci_high <- exp(ci[, 2])
  }
  co
}

sframe_run_ordinal_logistic <- function(data, roles, options = list()) {
  outcome <- sframe_role_values(roles, c("dependent", "outcome"))[1]
  predictors <- sframe_role_values(roles, c("predictors", "covariates"))
  vars <- c(outcome, predictors)
  err <- sframe_require_columns(data, vars, "Ordinal logistic regression")
  if (!is.null(err)) return(list(test = "regression_logistic_ordinal", error = err))
  if (length(predictors) == 0) {
    return(list(test = "regression_logistic_ordinal",
                error = "Ordinal logistic regression requires at least one predictor."))
  }
  sframe_require_MASS("to fit ordinal logistic regression with MASS::polr()")
  df <- data[, vars, drop = FALSE]
  df[[outcome]] <- ordered(df[[outcome]])
  for (p in predictors) {
    if (is.numeric(df[[p]])) df[[p]] <- sframe_num(df[[p]])
  }
  df <- df[stats::complete.cases(df), , drop = FALSE]
  if (nlevels(df[[outcome]]) < 3) {
    return(list(test = "regression_logistic_ordinal",
                error = "Ordinal logistic regression requires an ordered outcome with at least three levels."))
  }
  fit <- tryCatch(MASS::polr(stats::as.formula(paste(outcome, "~", paste(predictors, collapse = " + "))),
                             data = df, Hess = TRUE),
                  error = function(e) NULL)
  if (is.null(fit)) return(list(test = "regression_logistic_ordinal", error = "Ordinal logistic regression failed."))
  co <- as.data.frame(summary(fit)$coefficients)
  co$odds_ratio <- exp(co[[1]])
  if ("Std. Error" %in% names(co)) {
    co$or_ci_low <- exp(co[[1]] - 1.96 * co[["Std. Error"]])
    co$or_ci_high <- exp(co[[1]] + 1.96 * co[["Std. Error"]])
  }
  null_fit <- tryCatch(MASS::polr(stats::as.formula(paste(outcome, "~ 1")),
                                  data = df, Hess = FALSE),
                       error = function(e) NULL)
  pseudo_r2 <- if (!is.null(null_fit)) {
    1 - as.numeric(stats::logLik(fit)) / as.numeric(stats::logLik(null_fit))
  } else {
    NA_real_
  }
  list(
    test = "regression_logistic_ordinal",
    vars = vars,
    n = nrow(df),
    coefficients = co,
    AIC = stats::AIC(fit),
    pseudo_r2 = pseudo_r2,
    apa = sprintf("Ordinal logistic regression was estimated with %d complete cases.", nrow(df)),
    prompt = "Report odds ratios, confidence intervals where available, model fit, and the reference ordering of the outcome."
  )
}

# Firth-penalised binary logistic regression. Penalised maximum likelihood
# removes the small-sample bias of ordinary logistic regression and returns
# finite estimates under complete or quasi-complete separation, both of which
# are common in the small samples surveyframe targets from v0.4 onwards.
sframe_run_firth_logistic <- function(data, roles, options = list()) {
  outcome <- sframe_role_values(roles, c("dependent", "outcome"))[1]
  predictors <- sframe_role_values(roles, c("predictors", "covariates"))
  vars <- c(outcome, predictors)
  err <- sframe_require_columns(data, vars, "Firth logistic regression")
  if (!is.null(err)) return(list(test = "firth_logistic", error = err))
  if (length(predictors) == 0) {
    return(list(test = "firth_logistic",
                error = "Firth logistic regression requires at least one predictor."))
  }
  if (!sframe_has_package("logistf")) {
    return(list(
      test = "firth_logistic",
      error = "Package 'logistf' needed. Install with: install.packages('logistf')"
    ))
  }

  df <- data[, vars, drop = FALSE]
  outcome_fac <- droplevels(as.factor(df[[outcome]]))
  if (nlevels(outcome_fac) != 2L) {
    return(list(test = "firth_logistic",
                error = paste0("'", outcome,
                               "' must have exactly two non-missing levels.")))
  }
  df[[outcome]] <- as.integer(outcome_fac) - 1L
  for (p in predictors) df[[p]] <- sframe_num(df[[p]])
  df <- df[stats::complete.cases(df), , drop = FALSE]
  if (nrow(df) < 3) {
    return(list(test = "firth_logistic",
                error = "Fewer than 3 complete observations available."))
  }

  conf_level <- options$conf.level %||% 0.95
  form <- stats::as.formula(
    paste(outcome, "~", paste(predictors, collapse = " + "))
  )
  fit <- tryCatch(
    logistf::logistf(form, data = df, alpha = 1 - conf_level),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(list(test = "firth_logistic", error = "Firth regression failed."))
  }

  est <- as.numeric(fit$coefficients)
  terms <- names(fit$coefficients)
  coefficients <- data.frame(
    Estimate = est,
    ci_low = as.numeric(fit$ci.lower),
    ci_high = as.numeric(fit$ci.upper),
    p = as.numeric(fit$prob),
    odds_ratio = exp(est),
    or_ci_low = exp(as.numeric(fit$ci.lower)),
    or_ci_high = exp(as.numeric(fit$ci.upper)),
    row.names = terms,
    stringsAsFactors = FALSE
  )
  # The likelihood-ratio statistic is twice the log-likelihood gain of the
  # full model over the null model, read by name from $loglik. The difference
  # alone was half the statistic.
  loglik <- fit$loglik
  lr <- if (all(c("full", "null") %in% names(loglik))) {
    unname(2 * (loglik[["full"]] - loglik[["null"]]))
  } else {
    NA_real_
  }

  list(
    test = "firth_logistic",
    vars = vars,
    n = nrow(df),
    conf_level = conf_level,
    coefficients = coefficients,
    conf_int = cbind(lower = as.numeric(fit$ci.lower),
                     upper = as.numeric(fit$ci.upper)),
    p_values = stats::setNames(as.numeric(fit$prob), terms),
    likelihood_ratio = lr,
    apa = sprintf(
      "Firth logistic regression (n = %d), likelihood ratio = %.2f",
      nrow(df), lr
    ),
    prompt = "Report penalised odds ratios with their profile-likelihood confidence intervals, and state that Firth penalisation was used because of the small sample or separation."
  )
}

sframe_run_multinomial_logistic <- function(data, roles, options = list()) {
  outcome <- sframe_role_values(roles, c("dependent", "outcome"))[1]
  predictors <- sframe_role_values(roles, c("predictors", "covariates"))
  vars <- c(outcome, predictors)
  err <- sframe_require_columns(data, vars, "Multinomial logistic regression")
  if (!is.null(err)) return(list(test = "regression_logistic_multinomial", error = err))
  if (length(predictors) == 0) {
    return(list(test = "regression_logistic_multinomial",
                error = "Multinomial logistic regression requires at least one predictor."))
  }
  sframe_require_nnet("to fit multinomial logistic regression with nnet::multinom()")
  df <- data[, vars, drop = FALSE]
  df[[outcome]] <- as.factor(df[[outcome]])
  df <- df[stats::complete.cases(df), , drop = FALSE]
  if (nlevels(df[[outcome]]) < 3) {
    return(list(test = "regression_logistic_multinomial",
                error = "Multinomial logistic regression requires an outcome with at least three categories."))
  }
  fit <- tryCatch(nnet::multinom(stats::as.formula(paste(outcome, "~", paste(predictors, collapse = " + "))),
                                 data = df, trace = FALSE),
                  error = function(e) NULL)
  if (is.null(fit)) return(list(test = "regression_logistic_multinomial", error = "Multinomial logistic regression failed."))
  pred <- stats::predict(fit, type = "class")
  class_table <- table(observed = df[[outcome]], predicted = pred)
  null_fit <- tryCatch(nnet::multinom(stats::as.formula(paste(outcome, "~ 1")),
                                      data = df, trace = FALSE),
                       error = function(e) NULL)
  pseudo_r2 <- if (!is.null(null_fit)) {
    1 - as.numeric(stats::logLik(fit)) / as.numeric(stats::logLik(null_fit))
  } else {
    NA_real_
  }
  list(
    test = "regression_logistic_multinomial",
    vars = vars,
    n = nrow(df),
    coefficients = summary(fit)$coefficients,
    odds_ratios = exp(summary(fit)$coefficients),
    AIC = stats::AIC(fit),
    pseudo_r2 = pseudo_r2,
    classification_table = as.data.frame.matrix(class_table),
    apa = sprintf("Multinomial logistic regression was estimated with %d complete cases.", nrow(df)),
    prompt = "Report the reference category, odds ratios, model fit, and classification table where meaningful."
  )
}

sframe_run_moderation <- function(data, roles) {
  predictor <- sframe_role_values(roles, "predictor")[1]
  moderator <- sframe_role_values(roles, "moderator")[1]
  outcome <- sframe_role_values(roles, c("outcome", "dependent"))[1]
  vars <- c(outcome, predictor, moderator)
  err <- sframe_require_columns(data, vars, "Moderation")
  if (!is.null(err)) return(list(test = "moderation", error = err))
  df <- as.data.frame(lapply(data[, vars, drop = FALSE], sframe_num))
  df <- df[stats::complete.cases(df), , drop = FALSE]
  formula <- stats::as.formula(paste(outcome, "~", predictor, "*", moderator))
  fit <- tryCatch(stats::lm(formula, data = df), error = function(e) NULL)
  if (is.null(fit)) return(list(test = "moderation", error = "Moderation model failed."))
  mod_mean <- mean(df[[moderator]], na.rm = TRUE)
  mod_sd <- stats::sd(df[[moderator]], na.rm = TRUE)
  grid <- data.frame(
    moderator_level = c("low", "mean", "high"),
    moderator_value = c(mod_mean - mod_sd, mod_mean, mod_mean + mod_sd),
    stringsAsFactors = FALSE
  )
  co <- stats::coef(fit)
  int_name <- paste0(predictor, ":", moderator)
  if (!int_name %in% names(co)) int_name <- paste0(moderator, ":", predictor)
  interaction <- if (int_name %in% names(co)) co[[int_name]] else NA_real_
  grid$simple_slope <- co[[predictor]] + interaction * grid$moderator_value
  list(
    test = "moderation",
    vars = vars,
    n = nrow(df),
    coefficients = as.data.frame(summary(fit)$coefficients),
    conditional_effects = grid,
    interaction_plot_data = grid,
    apa = "Moderation was tested with a linear interaction model.",
    prompt = "Report the interaction coefficient and conditional effects at low, mean, and high moderator values."
  )
}

sframe_boot_indirect <- function(df, predictor, mediator, outcome, nboot = 1000L,
                                 observed = NA_real_) {
  n <- nrow(df)
  reps <- numeric(nboot)
  for (i in seq_len(nboot)) {
    idx <- sample.int(n, n, replace = TRUE)
    bdf <- df[idx, , drop = FALSE]
    reps[[i]] <- tryCatch({
      a <- stats::coef(stats::lm(stats::as.formula(paste(mediator, "~", predictor)), data = bdf))[[predictor]]
      b <- stats::coef(stats::lm(stats::as.formula(paste(outcome, "~", predictor, "+", mediator)), data = bdf))[[mediator]]
      a * b
    }, error = function(e) NA_real_)
  }
  ci <- sframe_percentile_ci(reps, observed, 0.95)
  structure(c(lower = ci[["lower"]], upper = ci[["upper"]]),
            resamples = attr(ci, "resamples"),
            valid_resamples = attr(ci, "valid_resamples"),
            reason = attr(ci, "reason"))
}

sframe_run_mediation <- function(data, roles, options = list()) {
  predictor <- sframe_role_values(roles, c("predictor", "x"))[1]
  mediator <- sframe_role_values(roles, "mediator")[1]
  outcome <- sframe_role_values(roles, c("outcome", "dependent", "y"))[1]
  vars <- c(outcome, predictor, mediator)
  err <- sframe_require_columns(data, vars, "Mediation")
  if (!is.null(err)) return(list(test = "mediation", error = err))
  df <- as.data.frame(lapply(data[, vars, drop = FALSE], sframe_num))
  df <- df[stats::complete.cases(df), , drop = FALSE]
  if (nrow(df) < 5) return(list(test = "mediation", error = "Mediation requires at least five complete rows."))
  a_fit <- stats::lm(stats::as.formula(paste(mediator, "~", predictor)), data = df)
  b_fit <- stats::lm(stats::as.formula(paste(outcome, "~", predictor, "+", mediator)), data = df)
  total_fit <- stats::lm(stats::as.formula(paste(outcome, "~", predictor)), data = df)
  a <- stats::coef(a_fit)[[predictor]]
  b <- stats::coef(b_fit)[[mediator]]
  c_prime <- stats::coef(b_fit)[[predictor]]
  total <- stats::coef(total_fit)[[predictor]]
  indirect <- a * b
  nboot <- as.integer(options$bootstrap %||% 1000L)
  ci <- sframe_boot_indirect(df, predictor, mediator, outcome, nboot = nboot,
                             observed = indirect)
  list(
    test = "mediation",
    vars = vars,
    n = nrow(df),
    direct = c_prime,
    indirect = indirect,
    total = total,
    a_path = a,
    b_path = b,
    indirect_ci = ci,
    bootstrap = nboot,
    apa = if (is.null(attr(ci, "reason"))) {
      sprintf("Indirect effect = %.3f, 95%% bootstrap CI [%.3f, %.3f], from %d of %d resamples.",
              indirect, ci[["lower"]], ci[["upper"]],
              attr(ci, "valid_resamples"), attr(ci, "resamples"))
    } else {
      sprintf("Indirect effect = %.3f, with no bootstrap interval: %s.",
              indirect, attr(ci, "reason"))
    },
    prompt = "Report direct, indirect, and total effects with bootstrap confidence intervals."
  )
}

#' Validity report for construct models
#'
#' @param loadings A data.frame with columns `construct`, `item`, and
#'   `loading`, or a named list of loading vectors by construct.
#' @param construct_scores Optional data.frame of construct scores for
#'   Fornell-Larcker and inter-construct correlations.
#' @param items_by_construct Optional named list, one element per construct,
#'   each a data.frame of that construct's item-level responses. When
#'   supplied, `htmt` is the Henseler heterotrait-monotrait ratio: the mean
#'   absolute heterotrait-heteromethod correlation over the geometric mean
#'   of the two constructs' mean absolute monotrait-heteromethod
#'   correlations. Constructs with a single item have no monotrait
#'   correlations, so their HTMT entries are `NA`. Without this argument,
#'   `htmt` falls back to the absolute inter-construct correlation matrix
#'   from `construct_scores` (the pre-0.3.4 behaviour). The `htmt_method`
#'   element records which was computed.
#'
#' @return An object of class `sframe_validity_report`.
#' @export
#' @examples
#' loadings <- list(
#'   sq  = c(sq_1 = 0.80, sq_2 = 0.75, sq_3 = 0.78),
#'   sat = c(sat_1 = 0.85, sat_2 = 0.82)
#' )
#' vr <- validity_report(loadings)
#' vr$reliability
validity_report <- function(loadings, construct_scores = NULL,
                            items_by_construct = NULL) {
  if (is.list(loadings) && !is.data.frame(loadings)) {
    loadings <- do.call(rbind, lapply(names(loadings), function(con) {
      data.frame(
        construct = con,
        item = names(loadings[[con]]) %||% paste0(con, seq_along(loadings[[con]])),
        loading = as.numeric(loadings[[con]]),
        stringsAsFactors = FALSE
      )
    }))
  }
  stopifnot(is.data.frame(loadings))
  required <- c("construct", "loading")
  if (!all(required %in% colnames(loadings))) {
    rlang::abort("`loadings` must contain `construct` and `loading` columns.",
                 class = c("sframe_validation_error", "sframe_error"))
  }
  reliability <- do.call(rbind, lapply(split(loadings, loadings$construct), function(df) {
    lam <- sframe_num(df$loading)
    err <- 1 - lam^2
    cr <- sum(lam)^2 / (sum(lam)^2 + sum(err, na.rm = TRUE))
    ave <- mean(lam^2, na.rm = TRUE)
    data.frame(
      construct = df$construct[[1]],
      composite_reliability = cr,
      AVE = ave,
      n_items = length(lam),
      stringsAsFactors = FALSE
    )
  }))
  cor_mat <- if (!is.null(construct_scores)) {
    stats::cor(as.data.frame(lapply(construct_scores, sframe_num)),
               use = "pairwise.complete.obs")
  } else {
    NULL
  }
  fornell <- NULL
  if (!is.null(cor_mat)) {
    fornell <- cor_mat
    ave_lookup <- stats::setNames(sqrt(reliability$AVE), reliability$construct)
    for (nm in intersect(rownames(fornell), names(ave_lookup))) {
      fornell[nm, nm] <- ave_lookup[[nm]]
    }
  }
  htmt <- NULL
  htmt_method <- "none"
  if (!is.null(items_by_construct)) {
    htmt <- sframe_henseler_htmt(items_by_construct)
    htmt_method <- "henseler"
  } else if (!is.null(cor_mat)) {
    # Documented fallback: the absolute inter-construct correlation matrix,
    # the pre-0.3.4 behaviour when only construct scores are available.
    htmt <- abs(cor_mat)
    htmt_method <- "correlation_fallback"
  }
  structure(
    list(
      method = "validity",
      loading_summary = loadings,
      reliability = reliability,
      fornell_larcker = fornell,
      htmt = htmt,
      htmt_method = htmt_method,
      inter_construct_correlations = cor_mat,
      apa = "Construct validity summaries were computed from supplied loadings.",
      prompt = "Report composite reliability, AVE, Fornell-Larcker, HTMT, and the inter-construct correlation matrix."
    ),
    class = "sframe_validity_report"
  )
}

# Little's MCAR test when naniar is installed; the pre-0.3.4
# available = FALSE result verbatim when it is not, so behaviour without
# the optional package is unchanged. The test needs numeric columns with
# some missingness and can fail on degenerate patterns, so any error also
# falls back to the unavailable result.
sframe_mcar_test <- function(item_data) {
  unavailable <- list(
    available = FALSE,
    warning = "Little's MCAR test requires an optional package and was not run."
  )
  if (!requireNamespace("naniar", quietly = TRUE)) {
    return(unavailable)
  }
  num <- as.data.frame(lapply(item_data, function(x) {
    suppressWarnings(as.numeric(x))
  }))
  num <- num[, vapply(num, function(x) any(is.finite(x)), logical(1)),
             drop = FALSE]
  if (ncol(num) < 2 || !anyNA(num)) {
    return(unavailable)
  }
  mt <- tryCatch(naniar::mcar_test(num), error = function(e) NULL)
  if (is.null(mt)) {
    return(unavailable)
  }
  list(
    available = TRUE,
    statistic = mt$statistic,
    df = mt$df,
    p_value = mt$p.value,
    interpretation = if (mt$p.value < .05) {
      "Data are unlikely to be missing completely at random."
    } else {
      "No evidence against missing completely at random."
    }
  )
}

# Henseler heterotrait-monotrait ratio from item-level data. Takes a named
# list of per-construct item data.frames; returns a symmetric matrix with 1
# on the diagonal. Single-item constructs have no monotrait correlations,
# so their off-diagonal entries are NA.
sframe_henseler_htmt <- function(items_by_construct) {
  stopifnot(is.list(items_by_construct), !is.null(names(items_by_construct)))
  cons <- names(items_by_construct)
  mats <- lapply(items_by_construct, function(df) {
    as.data.frame(lapply(as.data.frame(df), sframe_num))
  })
  mono <- vapply(mats, function(df) {
    if (ncol(df) < 2) return(NA_real_)
    m <- abs(stats::cor(df, use = "pairwise.complete.obs"))
    mean(m[lower.tri(m)], na.rm = TRUE)
  }, numeric(1))
  k <- length(cons)
  htmt <- matrix(NA_real_, k, k, dimnames = list(cons, cons))
  diag(htmt) <- 1
  if (k < 2) return(htmt)
  for (i in seq_len(k - 1)) {
    for (j in seq((i + 1), k)) {
      if (is.na(mono[[i]]) || is.na(mono[[j]]) ||
          mono[[i]] <= 0 || mono[[j]] <= 0) next
      hetero <- abs(stats::cor(mats[[i]], mats[[j]],
                               use = "pairwise.complete.obs"))
      ratio <- mean(hetero, na.rm = TRUE) / sqrt(mono[[i]] * mono[[j]])
      htmt[i, j] <- ratio
      htmt[j, i] <- ratio
    }
  }
  htmt
}

#' Sample-size and power planning helper
#'
#' Estimates a total sample size for a planned analysis. The targets use 3
#' different methods, and the result says which one applied.
#'
#' # Power calculations
#'
#' `"t_test"`, `"anova"` and `"correlation"` are power calculations, so
#' `alpha`, `power` and the expected effect size all change the result. The
#' t test uses [stats::power.t.test()] for 2 independent groups with Cohen's
#' `d`. ANOVA uses [stats::power.anova.test()] with Cohen's `f`. Correlation
#' uses the Fisher z approximation with `r`. `"regression"` is a power
#' calculation when `f2` is supplied, from the noncentral F distribution for
#' the overall test of `predictors` predictors.
#'
#' When the effect size is left `NULL`, a conventional medium effect is
#' assumed (`d` 0.5, `f` 0.25, `r` 0.30) and a warning names it. An
#' assumed effect is a placeholder. Supply the effect you expect from prior
#' studies or a pilot.
#'
#' # Precision targets and rules of thumb
#'
#' `"proportion"` and `"mean"` size a confidence interval to a margin of
#' error, and use `alpha` for its confidence level. `power` has no bearing on
#' them. `"regression"` without `f2` returns the larger of 2 published rules
#' of thumb, `50 + 8k` and `104 + k`, which ignore `alpha` and `power`.
#' `"sem"` returns no estimate. Both say so in the returned warnings.
#'
#' @param type Planning target: `"proportion"`, `"mean"`, `"correlation"`,
#'   `"t_test"`, `"anova"`, `"regression"`, or `"sem"`.
#' @param margin_error Margin of error for mean/proportion planning.
#' @param sd Standard deviation for mean planning.
#' @param p Expected proportion.
#' @param r Expected correlation. Defaults to 0.30 with a warning.
#' @param alpha Significance level.
#' @param power Desired power, for the power calculations.
#' @param groups Number of groups for ANOVA planning. A t test has 2.
#' @param predictors Number of predictors for regression planning.
#' @param d Expected Cohen's d for a t test. Defaults to 0.5 with a warning.
#' @param f Expected Cohen's f for ANOVA. Defaults to 0.25 with a warning.
#' @param f2 Expected Cohen's f squared for regression. When `NULL`, a rule of
#'   thumb is returned in place of a power calculation.
#'
#' @return An `sframe_sample_size_plan` list holding `type`, `estimated_n`
#'   (total sample size), `method` (`"power"`, `"precision"`,
#'   `"rule_of_thumb"` or `"none"`), `alpha`, `power`, `effect_size`,
#'   `warnings`, `advisory` and `prompt`.
#' @usage
#' sample_size_plan(
#'   type = c("proportion", "mean", "correlation", "t_test", "anova",
#'     "regression", "sem"),
#'   margin_error = NULL,
#'   sd = NULL,
#'   p = 0.5,
#'   r = NULL,
#'   alpha = 0.05,
#'   power = 0.8,
#'   groups = 2L,
#'   predictors = NULL,
#'   d = NULL,
#'   f = NULL,
#'   f2 = NULL
#' )
#' @export
#' @examples
#' plan <- sample_size_plan("t_test", d = 0.5, power = 0.80)
#' plan$estimated_n
#' plan$method
sample_size_plan <- function(
    type = c("proportion", "mean", "correlation", "t_test", "anova", "regression", "sem"),
    margin_error = NULL,
    sd = NULL,
    p = 0.5,
    r = NULL,
    alpha = 0.05,
    power = 0.80,
    groups = 2L,
    predictors = NULL,
    d = NULL,
    f = NULL,
    f2 = NULL
) {
  type <- rlang::arg_match(type)
  z <- stats::qnorm(1 - alpha / 2)
  assumed <- character(0)
  if (type == "t_test" && is.null(d)) {
    d <- 0.5
    assumed <- "Assumed a medium effect, d = 0.5. Supply d for the effect you expect."
  }
  if (type == "anova" && is.null(f)) {
    f <- 0.25
    assumed <- "Assumed a medium effect, f = 0.25. Supply f for the effect you expect."
  }
  if (type == "correlation" && is.null(r)) {
    assumed <- "Assumed a medium correlation, r = 0.30. Supply r for the effect you expect."
  }
  estimate <- switch(
    type,
    proportion = {
      me <- margin_error %||% 0.05
      ceiling(z^2 * p * (1 - p) / me^2)
    },
    mean = {
      me <- margin_error %||% 0.05
      s <- sd %||% 1
      ceiling((z * s / me)^2)
    },
    correlation = {
      rr <- abs(r %||% 0.30)
      ceiling(((stats::qnorm(1 - alpha / 2) + stats::qnorm(power)) /
                 (0.5 * log((1 + rr) / (1 - rr))))^2 + 3)
    },
    # These returned 64 and 50 per group whatever alpha, power and effect were
    # requested, while the result printed the requested alpha and power.
    t_test = 2 * ceiling(stats::power.t.test(delta = abs(d), sd = 1,
                                             sig.level = alpha,
                                             power = power)$n),
    anova = {
      k <- as.integer(groups)
      k * ceiling(stats::power.anova.test(groups = k,
                                          between.var = f^2 * k / (k - 1),
                                          within.var = 1, sig.level = alpha,
                                          power = power)$n)
    },
    regression = {
      pnum <- predictors %||% 5L
      if (is.null(f2)) {
        max(50 + 8 * pnum, 104 + pnum)
      } else {
        achieved <- function(n) {
          v <- n - pnum - 1
          1 - stats::pf(stats::qf(1 - alpha, pnum, v), pnum, v, ncp = f2 * n)
        }
        n <- pnum + 3
        while (achieved(n) < power && n < 1e6) n <- n + 1
        n
      }
    },
    sem = NA_integer_
  )
  method <- switch(type,
    proportion = "precision", mean = "precision",
    regression = if (is.null(f2)) "rule_of_thumb" else "power",
    sem = "none",
    "power")
  effect_size <- switch(type,
    t_test = c(d = d), anova = c(f = f), correlation = c(r = r %||% 0.30),
    regression = if (is.null(f2)) NULL else c(f2 = f2),
    NULL)
  warnings <- switch(
    type,
    regression = if (is.null(f2)) "Regression rule of thumb only, which ignores alpha and power. Supply f2 for a power calculation." else character(0),
    sem = "SEM sample-size planning is design-dependent; use at least 200 cases or 10-20 cases per free parameter as a preliminary warning only.",
    character(0)
  )
  advisory <- sframe_small_sample_advisory(
    estimate,
    sprintf("a planned %s analysis", gsub("_", " ", type, fixed = TRUE))
  )
  structure(
    list(
      type = type,
      estimated_n = estimate,
      method = method,
      alpha = alpha,
      power = power,
      effect_size = effect_size,
      warnings = c(assumed, warnings),
      advisory = advisory,
      prompt = "Document assumptions, expected effect size, attrition allowance, and the final sample-size decision."
    ),
    class = "sframe_sample_size_plan"
  )
}

#' @exportS3Method print sframe_sample_size_plan
print.sframe_sample_size_plan <- function(x, ...) {
  cat("Sample-Size Plan\n")
  cat(sprintf("  Target:      %s\n", x$type))
  cat(sprintf("  Estimated n: %s\n",
              if (is.na(x$estimated_n)) "not available" else format(x$estimated_n)))
  cat(sprintf("  Method:      %s\n", gsub("_", " ", x$method %||% "power", fixed = TRUE)))
  if (identical(x$method, "power")) {
    cat(sprintf("  Alpha:       %.3f   Power: %.2f\n", x$alpha, x$power))
  } else if (identical(x$method, "precision")) {
    cat(sprintf("  Confidence:  %.0f%%\n", 100 * (1 - x$alpha)))
  }
  if (length(x$effect_size)) {
    cat(sprintf("  Effect size: %s = %s\n", names(x$effect_size), format(x$effect_size)))
  }
  for (w in x$warnings) cat(sprintf("\nWarning: %s\n", w))
  if (!is.null(x$advisory)) cat(sprintf("\nNote: %s\n", x$advisory))
  invisible(x)
}
