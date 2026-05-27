# analysis_plan.R
# Functions for running pre-planned research-question-driven analyses
# and generating APA-formatted results with interpretation prompts.

# ---------------------------------------------------------------------------
# Citation library
# ---------------------------------------------------------------------------

.sframe_citations <- list(
  field_2018 = list(
    key  = "field_2018",
    apa  = "Field, A. (2018). *Discovering statistics using IBM SPSS statistics* (5th ed.). SAGE.",
    use  = c("chi_square", "fisher_exact", "t_test_ind", "t_test_pair",
             "anova_one", "correlation_pearson", "regression_linear")
  ),
  cohen_1988 = list(
    key  = "cohen_1988",
    apa  = "Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Lawrence Erlbaum.",
    use  = c("chi_square", "fisher_exact", "t_test_ind", "mann_whitney", "anova_one",
             "t_test_pair", "correlation_pearson", "correlation_spearman",
             "correlation_kendall", "partial_correlation", "kruskal_wallis",
             "wilcoxon_pair", "anova_two", "ancova", "repeated_anova")
  ),
  fisher_1925 = list(
    key = "fisher_1925",
    apa = "Fisher, R. A. (1925). *Statistical methods for research workers*. Oliver & Boyd.",
    use = c("anova_one", "anova_two", "ancova", "repeated_anova", "t_test_pair")
  ),
  shapiro_1965 = list(
    key = "shapiro_1965",
    apa = "Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for normality. *Biometrika*, *52*(3-4), 591-611.",
    use = c("anova_one", "anova_two", "ancova", "t_test_pair", "wilcoxon_pair")
  ),
  mann_1947 = list(
    key  = "mann_1947",
    apa  = "Mann, H. B., & Whitney, D. R. (1947). On a test of whether one of two random variables is stochastically larger than the other. *Annals of Mathematical Statistics*, *18*(1), 50-60.",
    use  = "mann_whitney"
  ),
  kruskal_1952 = list(
    key  = "kruskal_1952",
    apa  = "Kruskal, W. H., & Wallis, W. A. (1952). Use of ranks in one-criterion variance analysis. *Journal of the American Statistical Association*, *47*(260), 583-621.",
    use  = "kruskal_wallis"
  ),
  spearman_1904 = list(
    key  = "spearman_1904",
    apa  = "Spearman, C. (1904). The proof and measurement of association between two things. *The American Journal of Psychology*, *15*(1), 72-101.",
    use  = "correlation_spearman"
  ),
  kendall_1938 = list(
    key = "kendall_1938",
    apa = "Kendall, M. G. (1938). A new measure of rank correlation. *Biometrika*, *30*(1/2), 81-93.",
    use = "correlation_kendall"
  ),
  cronbach_1951 = list(
    key  = "cronbach_1951",
    apa  = "Cronbach, L. J. (1951). Coefficient alpha and the internal structure of tests. *Psychometrika*, *16*(3), 297-334.",
    use  = c("reliability_alpha", "reliability_omega")
  ),
  mcdonald_1999 = list(
    key  = "mcdonald_1999",
    apa  = "McDonald, R. P. (1999). *Test theory: A unified treatment*. Lawrence Erlbaum.",
    use  = "reliability_omega"
  ),
  hosmer_2013 = list(
    key  = "hosmer_2013",
    apa  = "Hosmer, D. W., Lemeshow, S., & Sturdivant, R. X. (2013). *Applied logistic regression* (3rd ed.). Wiley.",
    use  = c("regression_logistic_binary", "regression_logistic_ordinal",
             "regression_logistic_multinomial")
  ),
  macKinnon_2008 = list(
    key = "mackinnon_2008",
    apa = "MacKinnon, D. P. (2008). *Introduction to statistical mediation analysis*. Lawrence Erlbaum.",
    use = "mediation"
  ),
  aiken_1991 = list(
    key = "aiken_1991",
    apa = "Aiken, L. S., & West, S. G. (1991). *Multiple regression: Testing and interpreting interactions*. SAGE.",
    use = "moderation"
  ),
  r_core = list(
    key  = "r_core",
    apa  = "R Core Team. (2026). *R: A language and environment for statistical computing*. R Foundation for Statistical Computing.",
    use  = "all"
  ),
  surveyframe = list(
    key  = "surveyframe",
    apa  = "Sharafuddin, M. A. (2026). *surveyframe: A survey instrument workflow for R* (Version 0.3.0) [Computer software]. https://github.com/MohammedAliSharafuddin/surveyframe",
    use  = "all"
  )
)

sframe_citations_for_test <- function(test) {
  citations <- .sframe_citations
  matching <- Filter(function(cit) {
    "all" %in% cit$use || test %in% cit$use
  }, citations)
  lapply(matching, function(cit) cit$apa)
}

# ---------------------------------------------------------------------------
# Effect size helpers
# ---------------------------------------------------------------------------

sframe_cohens_d <- function(x, y) {
  nx <- length(x[!is.na(x)])
  ny <- length(y[!is.na(y)])
  if (nx < 2 || ny < 2) return(NA_real_)
  pooled_sd <- sqrt(((nx - 1) * stats::var(x, na.rm = TRUE) +
                     (ny - 1) * stats::var(y, na.rm = TRUE)) / (nx + ny - 2))
  if (pooled_sd == 0) return(NA_real_)
  abs(mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)) / pooled_sd
}

sframe_effect_label <- function(d, type = "d") {
  if (is.na(d)) return("could not be computed")
  if (type == "d") {
    if (d < 0.2) return("negligible")
    if (d < 0.5) return("small")
    if (d < 0.8) return("medium")
    return("large")
  }
  if (type == "r") {
    if (d < 0.1) return("negligible")
    if (d < 0.3) return("small")
    if (d < 0.5) return("medium")
    return("large")
  }
  if (type == "eta2") {
    if (d < 0.01) return("negligible")
    if (d < 0.06) return("small")
    if (d < 0.14) return("medium")
    return("large")
  }
  "unknown"
}

sframe_p_string <- function(p) {
  if (is.na(p)) return("= NA")
  if (p < .001) return("< .001")
  sprintf("= %s", formatC(p, digits = 3, format = "f"))
}

# ---------------------------------------------------------------------------
# Individual test runners
# ---------------------------------------------------------------------------

sframe_run_frequency <- function(data, vars, weights = NULL) {
  col <- vars[1]
  err <- sframe_require_columns(data, c(col, weights), "Frequency table")
  if (!is.null(err)) return(list(test = "frequency", error = err))
  vals <- data[[col]]
  tbl <- sort(table(vals, useNA = "ifany"), decreasing = TRUE)
  pct <- round(prop.table(tbl) * 100, 1)
  weighted <- NULL
  if (!is.null(weights) && nzchar(weights) && weights %in% colnames(data)) {
    w <- suppressWarnings(as.numeric(data[[weights]]))
    weighted <- stats::aggregate(
      w,
      by = list(Value = ifelse(is.na(vals), NA, as.character(vals))),
      FUN = sum,
      na.rm = TRUE
    )
    names(weighted)[names(weighted) == "x"] <- "Weighted_Frequency"
    weighted$Weighted_Percent <- if (sum(weighted$Weighted_Frequency, na.rm = TRUE) > 0) {
      round(weighted$Weighted_Frequency / sum(weighted$Weighted_Frequency, na.rm = TRUE) * 100, 1)
    } else {
      NA_real_
    }
  }
  list(
    test      = "frequency",
    variable  = col,
    weights   = weights,
    n         = length(vals[!is.na(vals)]),
    table     = data.frame(
      Value     = names(tbl),
      Frequency = as.integer(tbl),
      Percent   = as.numeric(pct),
      stringsAsFactors = FALSE
    ),
    weighted_table = weighted,
    apa       = sprintf("Frequency distribution for %s (N = %d).", col, sum(tbl)),
    prompt    = "Describe the distribution. Note any dominant category and whether the distribution was expected."
  )
}

sframe_run_crosstab <- function(data, vars, options = list()) {
  weights <- options$weights %||% NULL
  err <- sframe_require_columns(data, c(vars[1:2], weights), "Cross-tabulation")
  if (!is.null(err)) return(list(test = "crosstab", error = err))
  r <- data[[vars[1]]]
  c <- data[[vars[2]]]
  complete <- !is.na(r) & !is.na(c)
  r <- r[complete]; c <- c[complete]
  tbl <- table(r, c)
  ct <- tryCatch(
    suppressWarnings(stats::chisq.test(tbl, correct = FALSE)),
    error = function(e) NULL
  )
  if (is.null(ct)) {
    return(list(test = "crosstab", error = "Chi-square could not be computed."))
  }
  if (any(ct$expected < 5) && isTRUE(options$simulate_p_value)) {
    ct <- tryCatch(
      suppressWarnings(stats::chisq.test(tbl, correct = FALSE, simulate.p.value = TRUE)),
      error = function(e) ct
    )
  }
  n <- sum(tbl)
  r_dim <- nrow(tbl)
  c_dim <- ncol(tbl)
  denom <- n * min(r_dim - 1, c_dim - 1)
  effect <- if (denom > 0) sqrt(unname(ct$statistic) / denom) else NA_real_
  effect_name <- if (r_dim == 2 && c_dim == 2) "phi" else "Cramer's V"
  effect_symbol <- if (identical(effect_name, "phi")) "\u03c6" else "V"
  weighted_table <- NULL
  if (!is.null(weights) && nzchar(weights) && weights %in% colnames(data)) {
    w <- suppressWarnings(as.numeric(data[[weights]][complete]))
    weighted_table <- stats::xtabs(w ~ r + c)
  }
  list(
    test     = "crosstab",
    vars     = vars,
    weights  = weights,
    n        = n,
    table    = as.data.frame.matrix(tbl),
    weighted_table = if (!is.null(weighted_table)) as.data.frame.matrix(weighted_table) else NULL,
    chi_sq   = unname(ct$statistic),
    df       = unname(ct$parameter),
    p        = ct$p.value,
    expected = ct$expected,
    sparse_warning = any(ct$expected < 5),
    effect   = unname(effect),
    effect_name = effect_name,
    phi      = if (identical(effect_name, "phi")) unname(effect) else NA_real_,
    cramer_v = if (!identical(effect_name, "phi")) unname(effect) else NA_real_,
    effect_label = sframe_effect_label(effect, "r"),
    apa      = sprintf(
      "\u03c7\u00b2(%d, N = %d) = %.2f, p %s, %s = %.2f",
      ct$parameter, n, ct$statistic, sframe_p_string(ct$p.value),
      effect_symbol, effect
    ),
    prompt   = sprintf(
      "The chi-square test %s a significant association between %s and %s (%s = %.2f, %s effect). Describe the pattern in the cross-tabulation.",
      if (ct$p.value < .05) "revealed" else "found no",
      vars[1], vars[2], effect_name, effect, sframe_effect_label(effect, "r")
    ),
    p_method = if (isTRUE(options$simulate_p_value) && any(ct$expected < 5)) "simulated" else "asymptotic",
    warnings = if (any(ct$expected < 5)) {
      "Some expected counts are below 5; consider Fisher's exact test or simulated p-values for sparse tables."
    } else character(0)
  )
}

sframe_run_mann_whitney <- function(data, vars) {
  group_col <- vars[1]
  outcome_col <- vars[2]
  groups <- unique(data[[group_col]][!is.na(data[[group_col]])])
  if (length(groups) != 2) {
    return(list(test = "mann_whitney",
                error = "Mann-Whitney requires exactly two groups."))
  }
  g1 <- suppressWarnings(as.numeric(data[[outcome_col]][data[[group_col]] == groups[1]]))
  g2 <- suppressWarnings(as.numeric(data[[outcome_col]][data[[group_col]] == groups[2]]))
  g1 <- g1[!is.na(g1)]; g2 <- g2[!is.na(g2)]
  wt <- tryCatch(stats::wilcox.test(g1, g2, exact = FALSE), error = function(e) NULL)
  if (is.null(wt)) return(list(test = "mann_whitney", error = "Test failed."))
  n <- length(g1) + length(g2)
  z <- stats::qnorm(wt$p.value / 2)
  r <- abs(z) / sqrt(n)
  list(
    test     = "mann_whitney",
    vars     = vars,
    groups   = as.character(groups),
    n1       = length(g1), n2 = length(g2),
    median1  = stats::median(g1), median2 = stats::median(g2),
    U        = unname(wt$statistic),
    z        = z, p = wt$p.value, r = r,
    effect_label = sframe_effect_label(r, "r"),
    apa      = sprintf(
      "U = %.0f, z = %.2f, p %s, r = %.2f",
      wt$statistic, z, sframe_p_string(wt$p.value), r
    ),
    prompt   = sprintf(
      "The Mann-Whitney test %s a significant difference between %s (Mdn = %.2f) and %s (Mdn = %.2f), U = %.0f, p %s, r = %.2f (%s effect). Interpret the direction and practical significance.",
      if (wt$p.value < .05) "revealed" else "did not reveal",
      groups[1], stats::median(g1),
      groups[2], stats::median(g2),
      wt$statistic, sframe_p_string(wt$p.value),
      r, sframe_effect_label(r, "r")
    )
  )
}

sframe_run_t_test <- function(data, vars) {
  group_col <- vars[1]
  outcome_col <- vars[2]
  groups <- unique(data[[group_col]][!is.na(data[[group_col]])])
  if (length(groups) != 2) {
    return(list(test = "t_test_ind",
                error = "Independent t-test requires exactly two groups."))
  }
  g1 <- suppressWarnings(as.numeric(data[[outcome_col]][data[[group_col]] == groups[1]]))
  g2 <- suppressWarnings(as.numeric(data[[outcome_col]][data[[group_col]] == groups[2]]))
  g1 <- g1[!is.na(g1)]; g2 <- g2[!is.na(g2)]
  tt <- tryCatch(stats::t.test(g1, g2), error = function(e) NULL)
  if (is.null(tt)) return(list(test = "t_test_ind", error = "Test failed."))
  d <- sframe_cohens_d(g1, g2)
  list(
    test     = "t_test_ind",
    vars     = vars,
    groups   = as.character(groups),
    n1       = length(g1), n2 = length(g2),
    mean1    = mean(g1), mean2 = mean(g2),
    sd1      = stats::sd(g1), sd2 = stats::sd(g2),
    t        = unname(tt$statistic),
    df       = unname(tt$parameter),
    p        = tt$p.value, d = d,
    effect_label = sframe_effect_label(d, "d"),
    apa      = sprintf(
      "t(%.2f) = %.2f, p %s, d = %.2f",
      tt$parameter, tt$statistic, sframe_p_string(tt$p.value), d
    ),
    prompt   = sprintf(
      "The independent-samples t-test %s a significant difference between %s (M = %.2f, SD = %.2f) and %s (M = %.2f, SD = %.2f), t(%.2f) = %.2f, p %s, d = %.2f (%s effect). Discuss the direction and magnitude.",
      if (tt$p.value < .05) "revealed" else "did not reveal",
      groups[1], mean(g1), stats::sd(g1),
      groups[2], mean(g2), stats::sd(g2),
      tt$parameter, tt$statistic,
      sframe_p_string(tt$p.value), d, sframe_effect_label(d, "d")
    )
  )
}

sframe_run_anova_one <- function(data, vars) {
  group_col <- vars[1]
  outcome_col <- vars[2]
  outcome <- suppressWarnings(as.numeric(data[[outcome_col]]))
  group <- as.factor(data[[group_col]])
  complete <- !is.na(outcome) & !is.na(group)
  n <- sum(complete)
  k <- length(levels(droplevels(group[complete])))

  if (n < 3 || k < 2) {
    return(list(test = "anova_one",
                error = "One-way ANOVA requires at least two groups and three complete observations."))
  }

  fit <- tryCatch(
    stats::aov(outcome[complete] ~ group[complete]),
    error = function(e) NULL
  )
  if (is.null(fit)) return(list(test = "anova_one", error = "ANOVA failed."))

  s <- summary(fit)[[1]]
  F_stat <- s[["F value"]][1]
  p_val <- s[["Pr(>F)"]][1]
  ss_between <- s[["Sum Sq"]][1]
  ss_total <- sum(s[["Sum Sq"]], na.rm = TRUE)
  eta2 <- ss_between / ss_total

  group_means <- tapply(outcome[complete], group[complete], mean, na.rm = TRUE)
  group_means_str <- paste(
    paste(names(group_means), sprintf("M=%.2f", group_means), sep = " "),
    collapse = ", "
  )

  tukey <- if (!is.na(p_val) && p_val < 0.05 && k > 2) {
    tryCatch(
      utils::capture.output(stats::TukeyHSD(fit)),
      error = function(e) NULL
    )
  } else NULL

  list(
    test = "anova_one",
    vars = vars,
    n = n,
    k = k,
    F_stat = F_stat,
    df1 = k - 1,
    df2 = n - k,
    p = p_val,
    eta2 = eta2,
    group_means = as.list(group_means),
    tukey = tukey,
    effect_label = sframe_effect_label(eta2, "eta2"),
    apa = sprintf(
      "F(%d, %d) = %.2f, p %s, \u03b7\u00b2 = %.3f",
      k - 1, n - k, F_stat, sframe_p_string(p_val), eta2
    ),
    prompt = sprintf(
      "The one-way ANOVA %s a significant effect of %s on %s, F(%d, %d) = %.2f, p %s, \u03b7\u00b2 = %.3f (%s effect). Group means: %s. %s",
      if (!is.na(p_val) && p_val < .05) "revealed" else "did not reveal",
      vars[1], vars[2],
      k - 1, n - k, F_stat, sframe_p_string(p_val),
      eta2, sframe_effect_label(eta2, "eta2"),
      group_means_str,
      if (!is.null(tukey))
        "Tukey HSD post-hoc comparisons are included in the result object."
      else ""
    )
  )
}

sframe_run_t_test_pair <- function(data, vars) {
  x <- suppressWarnings(as.numeric(data[[vars[1]]]))
  y <- suppressWarnings(as.numeric(data[[vars[2]]]))

  complete <- !is.na(x) & !is.na(y)
  x <- x[complete]
  y <- y[complete]
  n <- length(x)

  if (n < 2) {
    return(list(test = "t_test_pair",
                error = "Fewer than 2 complete pairs available."))
  }

  tt <- tryCatch(stats::t.test(x, y, paired = TRUE), error = function(e) NULL)
  if (is.null(tt)) return(list(test = "t_test_pair", error = "Test failed."))

  diff <- x - y
  dz <- mean(diff, na.rm = TRUE) / stats::sd(diff, na.rm = TRUE)

  list(
    test = "t_test_pair",
    vars = vars,
    n = n,
    mean_x = mean(x),
    mean_y = mean(y),
    mean_diff = mean(diff),
    sd_diff = stats::sd(diff),
    t = unname(tt$statistic),
    df = unname(tt$parameter),
    p = tt$p.value,
    d_z = dz,
    effect_label = sframe_effect_label(abs(dz), "d"),
    apa = sprintf(
      "t(%d) = %.2f, p %s, d_z = %.2f",
      unname(tt$parameter), unname(tt$statistic),
      sframe_p_string(tt$p.value), dz
    ),
    prompt = sprintf(
      "The paired-samples t-test %s a significant mean difference between %s (M = %.2f) and %s (M = %.2f), t(%d) = %.2f, p %s, d_z = %.2f (%s effect). Interpret the direction and practical significance of the change.",
      if (tt$p.value < .05) "revealed" else "did not reveal",
      vars[1], mean(x), vars[2], mean(y),
      unname(tt$parameter), unname(tt$statistic),
      sframe_p_string(tt$p.value),
      dz, sframe_effect_label(abs(dz), "d")
    )
  )
}

sframe_run_wilcoxon_pair <- function(data, vars) {
  x <- suppressWarnings(as.numeric(data[[vars[1]]]))
  y <- suppressWarnings(as.numeric(data[[vars[2]]]))

  complete <- !is.na(x) & !is.na(y)
  x <- x[complete]
  y <- y[complete]
  n <- length(x)

  if (n < 2) {
    return(list(test = "wilcoxon_pair",
                error = "Fewer than 2 complete pairs available."))
  }

  wt <- tryCatch(
    stats::wilcox.test(x, y, paired = TRUE, exact = FALSE),
    error = function(e) NULL
  )
  if (is.null(wt)) return(list(test = "wilcoxon_pair", error = "Test failed."))

  z <- stats::qnorm(wt$p.value / 2)
  r <- abs(z) / sqrt(n)

  list(
    test = "wilcoxon_pair",
    vars = vars,
    n = n,
    median_x = stats::median(x),
    median_y = stats::median(y),
    V = unname(wt$statistic),
    z = z,
    p = wt$p.value,
    r = r,
    effect_label = sframe_effect_label(r, "r"),
    apa = sprintf(
      "V = %.0f, z = %.2f, p %s, r = %.2f",
      wt$statistic, z, sframe_p_string(wt$p.value), r
    ),
    prompt = sprintf(
      "The Wilcoxon signed-rank test %s a significant difference between %s (Mdn = %.2f) and %s (Mdn = %.2f), V = %.0f, z = %.2f, p %s, r = %.2f (%s effect). Discuss the direction and practical significance.",
      if (wt$p.value < .05) "revealed" else "did not reveal",
      vars[1], stats::median(x),
      vars[2], stats::median(y),
      wt$statistic, z,
      sframe_p_string(wt$p.value),
      r, sframe_effect_label(r, "r")
    )
  )
}

sframe_run_kruskal <- function(data, vars) {
  group_col <- vars[1]
  outcome_col <- vars[2]
  outcome <- suppressWarnings(as.numeric(data[[outcome_col]]))
  group   <- data[[group_col]]
  complete <- !is.na(outcome) & !is.na(group)
  kt <- tryCatch(
    stats::kruskal.test(outcome[complete] ~ as.factor(group[complete])),
    error = function(e) NULL
  )
  if (is.null(kt)) return(list(test = "kruskal_wallis", error = "Test failed."))
  n <- sum(complete)
  eta2 <- (kt$statistic - length(unique(group[complete])) + 1) / (n - length(unique(group[complete])))
  eta2 <- max(0, unname(eta2))
  list(
    test     = "kruskal_wallis",
    vars     = vars,
    H        = unname(kt$statistic),
    df       = unname(kt$parameter),
    p        = kt$p.value,
    eta2     = eta2,
    effect_label = sframe_effect_label(eta2, "eta2"),
    apa      = sprintf(
      "H(%d) = %.2f, p %s, \u03b7\u00b2 = %.3f",
      kt$parameter, kt$statistic, sframe_p_string(kt$p.value), eta2
    ),
    prompt   = sprintf(
      "The Kruskal-Wallis test %s a significant difference across groups, H(%d) = %.2f, p %s, \u03b7\u00b2 = %.3f (%s effect). If significant, describe which groups differed and in what direction.",
      if (kt$p.value < .05) "revealed" else "did not reveal",
      kt$parameter, kt$statistic,
      sframe_p_string(kt$p.value), eta2, sframe_effect_label(eta2, "eta2")
    )
  )
}

sframe_run_correlation <- function(data, vars, method = "pearson") {
  x <- suppressWarnings(as.numeric(data[[vars[1]]]))
  y <- suppressWarnings(as.numeric(data[[vars[2]]]))
  complete <- !is.na(x) & !is.na(y)
  n <- sum(complete)
  ct <- tryCatch(
    suppressWarnings(stats::cor.test(x[complete], y[complete], method = method)),
    error = function(e) NULL
  )
  if (is.null(ct)) return(list(test = paste0("correlation_", method), error = "Test failed."))
  r <- unname(ct$estimate)
  sym <- switch(method, pearson = "r", spearman = "r_s", kendall = "tau", "r")
  list(
    test     = paste0("correlation_", method),
    vars     = vars,
    method   = method,
    n        = n,
    r        = r,
    df       = n - 2,
    p        = ct$p.value,
    effect_label = sframe_effect_label(abs(r), "r"),
    apa      = sprintf(
      "%s(%d) = %.2f, p %s",
      sym, n - 2, r, sframe_p_string(ct$p.value)
    ),
    prompt   = sprintf(
      "There was a %s, %s %s correlation between %s and %s, %s(%d) = %.2f, p %s. Explain what this means for your research question.",
      if (r > 0) "positive" else "negative",
      sframe_effect_label(abs(r), "r"),
      if (ct$p.value < .05) "significant" else "non-significant",
      vars[1], vars[2],
      sym, n - 2, r, sframe_p_string(ct$p.value)
    )
  )
}

sframe_run_regression <- function(data, vars) {
  outcome <- suppressWarnings(as.numeric(data[[vars[length(vars)]]]))
  predictors <- vars[-length(vars)]
  pred_data <- as.data.frame(lapply(data[predictors], function(x) {
    suppressWarnings(as.numeric(x))
  }))
  pred_data[[vars[length(vars)]]] <- outcome
  complete <- complete.cases(pred_data)
  n <- sum(complete)
  formula_str <- paste(vars[length(vars)], "~",
                       paste(predictors, collapse = " + "))
  fit <- tryCatch(
    stats::lm(stats::as.formula(formula_str), data = pred_data[complete, ]),
    error = function(e) NULL
  )
  if (is.null(fit)) return(list(test = "regression_linear", error = "Regression failed."))
  s <- summary(fit)
  f_stat <- s$fstatistic
  p_val  <- stats::pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
  list(
    test     = "regression_linear",
    vars     = vars,
    n        = n,
    r2       = s$r.squared,
    adj_r2   = s$adj.r.squared,
    F        = unname(f_stat[1]),
    df1      = unname(f_stat[2]),
    df2      = unname(f_stat[3]),
    p        = p_val,
    coefficients = as.data.frame(s$coefficients),
    apa      = sprintf(
      "R\u00b2 = %.3f, F(%d, %d) = %.2f, p %s",
      s$r.squared, f_stat[2], f_stat[3], f_stat[1], sframe_p_string(p_val)
    ),
    prompt   = sprintf(
      "The regression model %s (R\u00b2 = %.3f, F(%d, %d) = %.2f, p %s). Interpret the direction and significance of each predictor and discuss the variance explained.",
      if (p_val < .05) "was statistically significant" else "was not statistically significant",
      s$r.squared, f_stat[2], f_stat[3], f_stat[1], sframe_p_string(p_val)
    )
  )
}

sframe_run_logistic_binary <- function(data, vars) {
  outcome_col <- vars[length(vars)]
  predictors <- vars[-length(vars)]

  outcome_raw <- data[[outcome_col]]
  outcome_fac <- as.factor(outcome_raw)

  if (nlevels(droplevels(outcome_fac[!is.na(outcome_fac)])) != 2L) {
    return(list(test = "regression_logistic_binary",
                error = paste0("'", outcome_col,
                               "' must have exactly two non-missing levels.")))
  }

  pred_data <- as.data.frame(lapply(data[predictors], function(x) {
    suppressWarnings(as.numeric(x))
  }))
  pred_data[[outcome_col]] <- as.integer(outcome_fac) - 1L
  complete <- stats::complete.cases(pred_data)
  n <- sum(complete)

  if (n < 3) {
    return(list(test = "regression_logistic_binary",
                error = "Fewer than 3 complete observations available."))
  }

  formula_str <- paste(outcome_col, "~", paste(predictors, collapse = " + "))

  fit <- tryCatch(
    stats::glm(
      stats::as.formula(formula_str),
      data = pred_data[complete, ],
      family = stats::binomial(link = "logit")
    ),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(list(test = "regression_logistic_binary",
                error = "Logistic regression failed."))
  }

  s <- summary(fit)
  coefficients <- as.data.frame(s$coefficients)
  coefficients$odds_ratio <- exp(coefficients$Estimate)
  if ("Std. Error" %in% names(coefficients)) {
    coefficients$or_ci_low <- exp(coefficients$Estimate - 1.96 * coefficients[["Std. Error"]])
    coefficients$or_ci_high <- exp(coefficients$Estimate + 1.96 * coefficients[["Std. Error"]])
  }
  predicted <- stats::predict(fit, type = "response")
  class_table <- table(
    observed = pred_data[[outcome_col]][complete],
    predicted = ifelse(predicted >= 0.5, 1L, 0L)
  )
  null_dev <- fit$null.deviance
  resid_dev <- fit$deviance
  df_null <- fit$df.null
  mcfadden_r2 <- 1 - resid_dev / null_dev
  chi_val <- null_dev - resid_dev
  chi_df <- df_null - fit$df.residual
  chi_p <- stats::pchisq(chi_val, df = chi_df, lower.tail = FALSE)

  list(
    test = "regression_logistic_binary",
    vars = vars,
    n = n,
    mcFadden_r2 = mcfadden_r2,
    chi_sq = chi_val,
    chi_df = chi_df,
    chi_p = chi_p,
    coefficients = coefficients,
    classification_table = as.data.frame.matrix(class_table),
    apa = sprintf(
      "\u03c7\u00b2(%d) = %.2f, p %s, McFadden R\u00b2 = %.3f",
      chi_df, chi_val, sframe_p_string(chi_p), mcfadden_r2
    ),
    prompt = sprintf(
      "The binary logistic regression model was %s overall, \u03c7\u00b2(%d) = %.2f, p %s, McFadden R\u00b2 = %.3f. Interpret the direction and odds ratio for each significant predictor.",
      if (chi_p < .05) "statistically significant" else "not statistically significant",
      chi_df, chi_val, sframe_p_string(chi_p), mcfadden_r2
    )
  )
}

# ---------------------------------------------------------------------------
# Dispatcher
# ---------------------------------------------------------------------------

sframe_model_by_id <- function(instrument, id) {
  models <- instrument$models %||% list()
  if (length(models) == 0 || is.null(id) || !nzchar(id)) {
    return(NULL)
  }
  hit <- models[vapply(models, function(model) identical(model$id, id), logical(1))]
  if (length(hit) == 0) NULL else hit[[1]]
}

sframe_vars_for_method <- function(method, roles, block) {
  vars <- as.character(block$variables %||% character(0))
  if (length(roles) == 0) return(vars)
  switch(
    method,
    frequency = sframe_role_values(roles, "variable"),
    descriptives = sframe_role_values(roles, c("variables", "items", "scales")),
    missing_data = sframe_role_values(roles, c("variables", "items")),
    crosstab = sframe_role_values(roles, c("row", "column", "variables")),
    chi_square = sframe_role_values(roles, c("row", "column", "variables")),
    fisher_exact = sframe_role_values(roles, c("row", "column", "variables")),
    mcnemar = sframe_role_values(roles, c("before", "after", "variables")),
    cochran_q = sframe_role_values(roles, c("measures", "variables")),
    t_test_ind = c(sframe_role_values(roles, "group"),
                   sframe_role_values(roles, c("outcome", "dependent"))),
    t_test_pair = c(sframe_role_values(roles, c("before", "x")),
                    sframe_role_values(roles, c("after", "y"))),
    mann_whitney = c(sframe_role_values(roles, "group"),
                     sframe_role_values(roles, c("outcome", "dependent"))),
    wilcoxon_pair = c(sframe_role_values(roles, c("before", "x")),
                      sframe_role_values(roles, c("after", "y"))),
    anova_one = c(sframe_role_values(roles, "group"),
                  sframe_role_values(roles, c("outcome", "dependent"))),
    anova_two = c(sframe_role_values(roles, c("factor1", "factor_a")),
                  sframe_role_values(roles, c("factor2", "factor_b")),
                  sframe_role_values(roles, c("outcome", "dependent"))),
    ancova = c(sframe_role_values(roles, c("group", "factor")),
               sframe_role_values(roles, c("covariates", "covariate")),
               sframe_role_values(roles, c("outcome", "dependent"))),
    repeated_anova = sframe_role_values(roles, c("measures", "variables")),
    kruskal_wallis = c(sframe_role_values(roles, "group"),
                       sframe_role_values(roles, c("outcome", "dependent"))),
    friedman = sframe_role_values(roles, c("measures", "variables")),
    correlation_pearson = c(sframe_role_values(roles, "x"),
                            sframe_role_values(roles, "y")),
    correlation_spearman = c(sframe_role_values(roles, "x"),
                             sframe_role_values(roles, "y")),
    correlation_kendall = c(sframe_role_values(roles, "x"),
                            sframe_role_values(roles, "y")),
    partial_correlation = c(sframe_role_values(roles, "x"),
                            sframe_role_values(roles, "y"),
                            sframe_role_values(roles, c("controls", "covariates"))),
    regression_linear = c(sframe_role_values(roles, c("predictors", "covariates")),
                          sframe_role_values(roles, c("dependent", "outcome"))),
    regression_logistic_binary = c(sframe_role_values(roles, c("predictors", "covariates")),
                                   sframe_role_values(roles, c("dependent", "outcome"))),
    regression_logistic_ordinal = c(sframe_role_values(roles, c("predictors", "covariates")),
                                    sframe_role_values(roles, c("dependent", "outcome"))),
    regression_logistic_multinomial = c(sframe_role_values(roles, c("predictors", "covariates")),
                                        sframe_role_values(roles, c("dependent", "outcome"))),
    moderation = c(sframe_role_values(roles, "predictor"),
                   sframe_role_values(roles, "moderator"),
                   sframe_role_values(roles, c("outcome", "dependent"))),
    mediation = c(sframe_role_values(roles, "predictor"),
                  sframe_role_values(roles, "mediator"),
                  sframe_role_values(roles, c("outcome", "dependent"))),
    vars
  )
}

sframe_result_from_report <- function(report, test = report$method %||% "") {
  out <- unclass(report)
  out$test <- test
  out
}

sframe_run_one_block <- function(block, data, instrument) {
  test <- sframe_analysis_method(block)
  roles <- sframe_analysis_roles(block)
  vars <- sframe_vars_for_method(test, roles, block)
  options <- block$options %||% list()
  weights <- sframe_role_values(roles, c("weights", "weight"))
  if (length(weights) > 0 && is.null(options$weights)) {
    options$weights <- weights[1]
  }
  if (!is.null(block$alpha) && sframe_method_uses_alpha(test)) {
    options$alpha <- block$alpha
  }

  result <- tryCatch({
    switch(test,
      frequency          = sframe_run_frequency(data, vars, weights = options$weights),
      descriptives       = sframe_run_descriptives_result(data, roles, options),
      missing_data       = sframe_run_missing_result(data, instrument, roles),
      quality            = sframe_result_from_report(quality_report(data, instrument), "quality"),
      scale_descriptives = sframe_run_descriptives_result(data, list(variables = vapply(instrument$scales, function(s) s$id, character(1))), options),
      reliability_alpha  = sframe_result_from_report(reliability_report(data, instrument, omega = FALSE), "reliability_alpha"),
      reliability_omega  = sframe_result_from_report(reliability_report(data, instrument, alpha = FALSE, omega = TRUE), "reliability_omega"),
      item_diagnostics   = sframe_result_from_report(item_report(data, instrument), "item_diagnostics"),
      efa_readiness      = sframe_result_from_report(efa_report(data, instrument, nfactors = options$nfactors), "efa_readiness"),
      efa_solution       = sframe_result_from_report(efa_solution(data, instrument, items = sframe_role_values(roles, c("items", "variables")), nfactors = options$nfactors %||% 1L), "efa_solution"),
      cfa_lavaan_syntax  = {
        model_id <- sframe_role_values(roles, "model", "")[1]
        model <- sframe_model_by_id(instrument, model_id)
        syntax <- cfa_lavaan_syntax(instrument = instrument, model = model, ordered = isTRUE(options$ordered))
        list(test = test, syntax = syntax, apa = "CFA lavaan syntax generated.", prompt = "Fit the generated syntax with lavaan when ready.")
      },
      sem_lavaan_syntax  = {
        model_id <- sframe_role_values(roles, "model", "")[1]
        model <- sframe_model_by_id(instrument, model_id)
        if (is.null(model)) list(test = test, error = "SEM syntax requires a saved model role.")
        else list(test = test, syntax = sem_lavaan_syntax(model, instrument), apa = "CB-SEM lavaan syntax generated.", prompt = "Inspect measurement and structural syntax before fitting.")
      },
      seminr_syntax      = {
        model_id <- sframe_role_values(roles, "model", "")[1]
        model <- sframe_model_by_id(instrument, model_id)
        if (is.null(model)) list(test = test, error = "PLS-SEM syntax requires a saved model role.")
        else list(test = test, syntax = seminr_syntax(model), apa = "PLS-SEM seminr syntax generated.", prompt = "Fit with seminr only when the optional package is installed.")
      },
      crosstab           = sframe_run_crosstab(data, vars, options),
      chi_square         = sframe_run_crosstab(data, vars, options),
      fisher_exact       = sframe_run_fisher(data, roles, options),
      mcnemar            = sframe_run_mcnemar(data, roles, options),
      cochran_q          = sframe_run_cochran_q(data, roles),
      mann_whitney       = sframe_run_mann_whitney(data, vars),
      t_test_ind         = sframe_run_t_test(data, vars),
      t_test_pair        = sframe_run_t_test_pair(data, vars),
      kruskal_wallis     = sframe_run_kruskal(data, vars),
      anova_one          = sframe_run_anova_one(data, vars),
      anova_two          = sframe_run_anova_two(data, roles),
      ancova             = sframe_run_ancova(data, roles),
      repeated_anova     = sframe_run_repeated_anova(data, roles),
      friedman           = sframe_run_friedman(data, roles),
      wilcoxon_pair      = sframe_run_wilcoxon_pair(data, vars),
      correlation_pearson  = sframe_run_correlation(data, vars, "pearson"),
      correlation_spearman = sframe_run_correlation(data, vars, "spearman"),
      correlation_kendall  = sframe_run_kendall(data, roles),
      partial_correlation  = sframe_run_partial_correlation(data, roles, options),
      regression_linear    = sframe_run_regression(data, vars),
      regression_logistic_binary = sframe_run_logistic_binary(data, vars),
      regression_logistic_ordinal = sframe_run_ordinal_logistic(data, roles, options),
      regression_logistic_multinomial = sframe_run_multinomial_logistic(data, roles, options),
      moderation = sframe_run_moderation(data, roles),
      mediation = sframe_run_mediation(data, roles, options),
      list(test = test, error = paste0("Test '", test, "' is unavailable."))
    )
  }, error = function(e) {
    list(test = test, error = conditionMessage(e))
  })

  result$research_question <- block$research_question
  result$block_id          <- block$id
  result$family            <- block$family %||% ""
  result$roles             <- roles
  result$options           <- options
  result$hypotheses        <- block$hypotheses %||% NULL
  result$decision_rule     <- block$decision_rule %||% block$interpretation %||% ""
  result$interpretation    <- block$interpretation %||% block$decision_rule %||% ""
  result$citations         <- sframe_citations_for_test(test)
  result
}

#' Run a pre-planned analysis from an instrument's analysis plan
#'
#' Executes every analysis block defined in the instrument's `analysis_plan`
#' slot against the supplied response data. Each block corresponds to one
#' research question defined during instrument design in the SurveyBuilder.
#' Results include APA-formatted statistics, effect sizes, interpretation
#' prompts, and reporting references.
#'
#' @param data A `tibble` or `data.frame` of responses, typically produced by
#'   [read_responses()] or [read_sheet_responses()].
#' @param instrument An `sframe` object containing an `analysis_plan`.
#' @param scored Logical. Whether to automatically score scales before running
#'   the analysis. Defaults to `TRUE`.
#'
#' @return An object of class `sframe_analysis_results`, a list with one
#'   element per analysis block. Each element contains the test result,
#'   APA string, interpretation prompt, and reporting-reference metadata. Pass
#'   to [render_results()] to generate a formatted report.
#' @export
#' @seealso [render_results()], [read_sheet_responses()]
#'
#' @examples
#' instr <- read_sframe(
#'   system.file("extdata", "tourism_services_demo.sframe",
#'               package = "surveyframe")
#' )
#' responses <- read_responses(
#'   system.file("extdata", "tourism_services_responses.csv",
#'               package = "surveyframe"),
#'   instr,
#'   respondent_id = "respondent_id",
#'   submitted_at = "submitted_at",
#'   meta_cols = "started_at"
#' )
#' results <- run_analysis_plan(responses, instr)
#' print(results)
run_analysis_plan <- function(data, instrument, scored = TRUE) {
  stopifnot(inherits(instrument, "sframe"))
  stopifnot(is.data.frame(data))

  plan <- instrument$analysis_plan
  if (is.null(plan) || length(plan) == 0) {
    rlang::abort(
      "No analysis plan found in this instrument. Define research questions in the SurveyBuilder Analyse menu before running.",
      class = "sframe_error"
    )
  }

  # Score scales first if requested
  if (scored && length(instrument$scales) > 0) {
    data <- tryCatch(
      score_scales(data, instrument, keep_items = TRUE, keep_meta = TRUE),
      error = function(e) {
        sframe_warn_scoring("Scale scoring failed before analysis: skipping.")
        data
      }
    )
  }

  results <- lapply(plan, sframe_run_one_block, data = data, instrument = instrument)
  structure(results, class = "sframe_analysis_results")
}

#' @exportS3Method print sframe_analysis_results
print.sframe_analysis_results <- function(x, ...) {
  cat(sprintf("Analysis Results: %d research question(s)\n\n", length(x)))
  for (i in seq_along(x)) {
    r <- x[[i]]
    cat(sprintf("RQ %d: %s\n", i, r$research_question %||% "(untitled)"))
    if (!is.null(r$error)) {
      cat(sprintf("  Error: %s\n\n", r$error))
    } else {
      cat(sprintf("  Test: %s\n", r$test))
      cat(sprintf("  APA:  %s\n\n", r$apa %||% ""))
    }
  }
  invisible(x)
}

#' Render analysis results to a formatted HTML report
#'
#' Generates a self-contained HTML report from the output of
#' [run_analysis_plan()]. Each section corresponds to one research question
#' and includes the APA-formatted statistical result, an interpretation space,
#' and a reference list.
#'
#' @param results An `sframe_analysis_results` object from [run_analysis_plan()].
#' @param instrument An `sframe` object.
#' @param output_file Character or NULL. Path to the output HTML file. When
#'   NULL, a temporary file is written and its path returned.
#' @param output_path Character or NULL. Alias for `output_file`.
#' @param citation_format Character. Reference format. One of `"apa"`,
#'   `"ama"`, or `"vancouver"`. Defaults to `"apa"`.
#' @param title Character or NULL. Report title. Defaults to the instrument
#'   title with " -- Results" appended.
#'
#' @return The output file path, invisibly.
#' @export
#' @seealso [run_analysis_plan()], [render_report()]
#'
#' @examples
#' instr <- read_sframe(
#'   system.file("extdata", "tourism_services_demo.sframe",
#'               package = "surveyframe")
#' )
#' responses <- read_responses(
#'   system.file("extdata", "tourism_services_responses.csv",
#'               package = "surveyframe"),
#'   instr,
#'   respondent_id = "respondent_id",
#'   submitted_at = "submitted_at",
#'   meta_cols = "started_at"
#' )
#' results <- run_analysis_plan(responses, instr)
#' out <- render_results(results, instr,
#'                       output_file = tempfile(fileext = ".html"))
#' file.exists(out)
render_results <- function(
    results         = NULL,
    instrument,
    output_file     = NULL,
    output_path     = NULL,
    citation_format = c("apa", "ama", "vancouver"),
    title           = NULL
) {
  stopifnot(inherits(instrument, "sframe"))
  citation_format <- rlang::arg_match(citation_format)

  dest <- output_file %||% output_path %||% tempfile(fileext = ".html")

  # If called with instrument only (no pre-computed results), delegate
  if (is.null(results)) {
    return(render_report(instrument, output_file = dest))
  }

  stopifnot(inherits(results, "sframe_analysis_results"))
  report_title <- title %||% paste0(instrument$meta$title, " -- Results")
  report_title_html <- htmltools_escape(report_title)
  instrument_title_html <- htmltools_escape(instrument$meta$title)
  instrument_version_html <- htmltools_escape(instrument$meta$version)

  # Collect all unique citations
  all_citations <- list()
  for (r in results) {
    for (cit_key in names(r$citations %||% list())) {
      all_citations[[cit_key]] <- r$citations[[cit_key]]
    }
  }

  # Build HTML sections
  sections_html <- paste(vapply(seq_along(results), function(i) {
    r <- results[[i]]
    rq <- htmltools_escape(r$research_question %||% paste("Research Question", i))
    apa_str <- r$apa %||% ""
    prompt  <- r$interpretation_prompt %||% r$prompt %||% ""
    interp  <- r$interpretation %||% ""
    cits <- paste(
      vapply(unlist(r$citations), htmltools_escape, character(1)),
      collapse = "<br>"
    )

    if (!is.null(r$error)) {
      return(sprintf(
        '<section class="rq-block"><h2>RQ %d: %s</h2>
         <div class="error-box">Error: %s</div></section>',
        i, rq, htmltools_escape(r$error)
      ))
    }

    # Build APA-style results table
    tbl_html <- ""
    if (!is.null(r$table) && is.data.frame(r$table)) {
      rows <- paste(apply(r$table, 1, function(row) {
        cells <- paste(sprintf("<td>%s</td>", htmltools_escape(as.character(row))),
                      collapse = "")
        sprintf("<tr>%s</tr>", cells)
      }), collapse = "")
      headers <- paste(sprintf("<th>%s</th>", htmltools_escape(colnames(r$table))),
                      collapse = "")
      has_pval <- any(grepl(
        "^p$|^p\\.value$|^p_value$|^Pr\\(>",
        colnames(r$table), ignore.case = TRUE
      ))
      foot_html <- if (has_pval) {
        paste0(
          '<tfoot><tr><td colspan="', ncol(r$table), '">',
          "* <em>p</em> &lt; .05, ** <em>p</em> &lt; .01, *** <em>p</em> &lt; .001",
          "</td></tr></tfoot>"
        )
      } else ""
      tbl_html <- sprintf(
        '<table class="results-table"><thead><tr>%s</tr></thead><tbody>%s</tbody>%s</table>',
        headers, rows, foot_html
      )
    }

    effect_badge <- ""
    if (!is.null(r$effect_label)) {
      effect_class <- gsub("[^a-zA-Z0-9_-]", "", r$effect_label)
      effect_badge <- sprintf(
        '<span class="effect-badge effect-%s">%s effect</span>',
        effect_class, htmltools_escape(r$effect_label)
      )
    }

    sprintf(
      '<section class="rq-block">
        <div class="rq-header">
          <span class="rq-number">RQ %d</span>
          <h2>%s</h2>
        </div>
        <div class="result-box">
          <div class="apa-string">%s %s</div>
          %s
        </div>
        <div class="interpretation-section">
          <h3>Interpretation</h3>
          <div class="prompt-box">%s</div>
          <div class="interp-text">%s</div>
        </div>
        <div class="citations-section">
          <h4>References</h4>
          <div class="citation-list">%s</div>
        </div>
      </section>',
      i, rq, htmltools_escape(apa_str), effect_badge,
      tbl_html,
      htmltools_escape(prompt),
      if (nzchar(interp)) htmltools_escape(interp) else
        '<span class="empty-interp">Your interpretation will appear here.</span>',
      cits
    )
  }, character(1)), collapse = "\n")

  references_html <- if (length(all_citations) > 0) {
    items <- paste(
      sprintf(
        '<li>%s</li>',
        vapply(unlist(all_citations), htmltools_escape, character(1))
      ),
      collapse = "\n"
    )
    sprintf('<section class="references"><h2>References</h2><ol>%s</ol></section>', items)
  } else ""

  html <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>%s</title>
<style>
  body { font-family: "Helvetica Neue", Arial, sans-serif; max-width: 860px;
         margin: 0 auto; padding: 32px 24px; color: #1a1a2e; line-height: 1.6; }
  h1 { font-size: 26px; border-bottom: 3px solid #1a1a2e; padding-bottom: 12px; }
  h2 { font-size: 19px; color: #1a1a2e; margin-top: 0; }
  h3 { font-size: 15px; color: #444; margin: 16px 0 8px; }
  h4 { font-size: 13px; color: #666; margin: 12px 0 6px; }
  .rq-block { background: #fff; border: 1px solid #e0e3ea; border-radius: 10px;
               padding: 28px; margin-bottom: 28px;
               box-shadow: 0 1px 4px rgba(0,0,0,.07); }
  .rq-header { display: flex; align-items: flex-start; gap: 14px; margin-bottom: 20px; }
  .rq-number { background: #1a1a2e; color: #fff; border-radius: 20px;
                padding: 4px 12px; font-size: 12px; font-weight: 700;
                white-space: nowrap; margin-top: 3px; }
  .result-box { background: #f0f4ff; border-left: 4px solid #5b8dee;
                 padding: 14px 18px; border-radius: 0 8px 8px 0; margin-bottom: 18px; }
  .apa-string { font-family: "Georgia", serif; font-size: 15px; color: #1a1a2e; }
  .effect-badge { display: inline-block; margin-left: 10px; padding: 2px 9px;
                   border-radius: 12px; font-size: 12px; font-weight: 600; }
  .effect-negligible { background: #f5f5f5; color: #666; }
  .effect-small      { background: #e8f5e9; color: #2e7d32; }
  .effect-medium     { background: #fff3e0; color: #e65100; }
  .effect-large      { background: #fce4ec; color: #c62828; }
  .interpretation-section { margin-bottom: 16px; }
  .prompt-box { background: #fffde7; border: 1px solid #f9a825;
                 border-radius: 6px; padding: 10px 14px; font-size: 13px;
                 color: #5d4037; margin-bottom: 10px; }
  .prompt-box::before { content: "Writing prompt: "; font-weight: 700; }
  .interp-text { font-size: 15px; color: #333; padding: 8px 0; }
  .empty-interp { color: #aaa; font-style: italic; }
  .citations-section { border-top: 1px solid #eee; padding-top: 12px; }
  .citation-list { font-size: 13px; color: #555; line-height: 1.8; }
  .results-table { width: 100%%; border-collapse: collapse; font-size: 14px;
                    margin: 12px 0; font-family: Georgia, serif; }
  .results-table thead tr { border-top: 2px solid #000; border-bottom: 1px solid #000; }
  .results-table tbody tr:last-child td { border-bottom: 2px solid #000; }
  .results-table th { background: none; color: #000; padding: 6px 12px;
                       text-align: left; font-weight: 700; border: none; }
  .results-table td { padding: 5px 12px; border: none; }
  .results-table tfoot td { font-size: 12px; font-style: italic; color: #444;
                              border: none; padding-top: 4px; }
  .error-box { background: #fde8e8; border-left: 4px solid #b91c1c;
                padding: 10px 14px; border-radius: 0 6px 6px 0; color: #7f1d1d; }
  .references { margin-top: 40px; border-top: 2px solid #eee; padding-top: 20px; }
  .references ol { padding-left: 20px; }
  .references li { font-size: 14px; margin-bottom: 8px; color: #333; }
  .meta { color: #777; font-size: 13px; margin-bottom: 32px; }
</style>
</head>
<body>
<h1>%s</h1>
<div class="meta">
  Instrument: %s v%s &nbsp;|&nbsp;
  Generated: %s &nbsp;|&nbsp;
  surveyframe package
</div>
%s
%s
</body>
</html>',
    report_title_html, report_title_html,
    instrument_title_html, instrument_version_html,
    htmltools_escape(format(Sys.Date(), "%B %d, %Y")),
    sections_html, references_html
  )

  writeLines(html, dest)
  invisible(dest)
}
