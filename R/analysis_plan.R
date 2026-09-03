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
             "regression_logistic_multinomial", "firth_logistic")
  ),
  firth_1993 = list(
    key  = "firth_1993",
    apa  = "Firth, D. (1993). Bias reduction of maximum likelihood estimates. *Biometrika*, *80*(1), 27-38.",
    use  = "firth_logistic"
  ),
  heinze_2002 = list(
    key  = "heinze_2002",
    apa  = "Heinze, G., & Schemper, M. (2002). A solution to the problem of separation in logistic regression. *Statistics in Medicine*, *21*(16), 2409-2419.",
    use  = "firth_logistic"
  ),
  hwang_1981 = list(
    key  = "hwang_1981",
    apa  = "Hwang, C.-L., & Yoon, K. (1981). *Multiple attribute decision making: Methods and applications*. Springer.",
    use  = "topsis"
  ),
  saaty_1980 = list(
    key  = "saaty_1980",
    apa  = "Saaty, T. L. (1980). *The analytic hierarchy process*. McGraw-Hill.",
    use  = "ahp"
  ),
  # The 8 remaining decision methods. Every entry below was checked against
  # the publication record on 2026-07-31 rather than carried over from the
  # harvest audit's draft strings. That check produced one correction: ELECTRE
  # is volume 2 issue 1, not the "2(8)" the audit recorded. Numdam's scan of
  # the original journal is RO_1968__2_1_57_0.
  saaty_1996 = list(
    key  = "saaty_1996",
    apa  = "Saaty, T. L. (1996). *Decision making with dependence and feedback: The analytic network process*. RWS Publications.",
    use  = "anp"
  ),
  # DEMATEL has no single canonical publication. It was developed as a
  # programme at the Battelle Geneva Research Centre between 1972 and 1976
  # across at least 4 grey-literature reports, and secondary sources disagree
  # over which to cite and even over the author order. None can be sighted,
  # and an unverified reference must not be cited as though it were checked,
  # so the originating report is given for provenance and paired with a
  # peer-reviewed review a reader can actually obtain.
  gabus_1972 = list(
    key  = "gabus_1972",
    apa  = "Gabus, A., & Fontela, E. (1972). *World problems, an invitation to further thought within the framework of DEMATEL*. Battelle Geneva Research Centre.",
    use  = "dematel"
  ),
  si_2018 = list(
    key  = "si_2018",
    apa  = "Si, S.-L., You, X.-Y., Liu, H.-C., & Zhang, P. (2018). DEMATEL technique: A systematic review of the state-of-the-art literature on methodologies and applications. *Mathematical Problems in Engineering*, *2018*, 3696457. https://doi.org/10.1155/2018/3696457",
    use  = "dematel"
  ),
  opricovic_2004 = list(
    key  = "opricovic_2004",
    apa  = "Opricovic, S., & Tzeng, G.-H. (2004). Compromise solution by MCDM methods: A comparative analysis of VIKOR and TOPSIS. *European Journal of Operational Research*, *156*(2), 445-455. https://doi.org/10.1016/S0377-2217(03)00020-1",
    use  = "vikor"
  ),
  brauers_2006 = list(
    key  = "brauers_2006",
    apa  = "Brauers, W. K. M., & Zavadskas, E. K. (2006). The MOORA method and its application to privatization in a transition economy. *Control and Cybernetics*, *35*(2), 445-469.",
    use  = "moora"
  ),
  edwards_1977 = list(
    key  = "edwards_1977",
    apa  = "Edwards, W. (1977). How to use multiattribute utility measurement for social decisionmaking. *IEEE Transactions on Systems, Man, and Cybernetics*, *7*(5), 326-340. https://doi.org/10.1109/TSMC.1977.4309720",
    use  = "smart"
  ),
  zavadskas_2012 = list(
    key  = "zavadskas_2012",
    apa  = "Zavadskas, E. K., Turskis, Z., Antucheviciene, J., & Zakarevicius, A. (2012). Optimization of weighted aggregated sum product assessment. *Elektronika ir Elektrotechnika*, *122*(6), 3-6. https://doi.org/10.5755/j01.eee.122.6.1810",
    use  = "waspas"
  ),
  brans_1985 = list(
    key  = "brans_1985",
    apa  = "Brans, J. P., & Vincke, P. (1985). A preference ranking organisation method: The PROMETHEE method for multiple criteria decision-making. *Management Science*, *31*(6), 647-656. https://doi.org/10.1287/mnsc.31.6.647",
    use  = "promethee"
  ),
  roy_1968 = list(
    key  = "roy_1968",
    apa  = "Roy, B. (1968). Classement et choix en presence de points de vue multiples (la methode ELECTRE). *Revue Francaise d'Informatique et de Recherche Operationnelle*, *2*(1), 57-75.",
    use  = "electre"
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
  blondel_2008 = list(
    key  = "blondel_2008",
    apa  = "Blondel, V. D., Guillaume, J.-L., Lambiotte, R., & Lefebvre, E. (2008). Fast unfolding of communities in large networks. *Journal of Statistical Mechanics: Theory and Experiment*, *2008*(10), P10008. https://doi.org/10.1088/1742-5468/2008/10/P10008",
    use  = "co_occurrence_network"
  ),
  fruchterman_1991 = list(
    key  = "fruchterman_1991",
    apa  = "Fruchterman, T. M. J., & Reingold, E. M. (1991). Graph drawing by force-directed placement. *Software: Practice and Experience*, *21*(11), 1129-1164. https://doi.org/10.1002/spe.4380211102",
    use  = "co_occurrence_network"
  ),
  r_core = list(
    key  = "r_core",
    apa  = "R Core Team. (2026). *R: A language and environment for statistical computing*. R Foundation for Statistical Computing.",
    use  = "all"
  ),
  surveyframe = list(
    key  = "surveyframe",
    apa  = "Sharafuddin, M. A. (2026). *surveyframe: Survey Instrument Workflows* (Version %s) [Computer software]. https://github.com/MohammedAliSharafuddin/surveyframe",
    use  = "all"
  )
)

sframe_citations_for_test <- function(test) {
  citations <- .sframe_citations
  matching <- Filter(function(cit) {
    "all" %in% cit$use || test %in% cit$use
  }, citations)
  # Inject the live package version into any citation template (the surveyframe
  # self-citation), so the version never goes stale on a release bump.
  ver <- tryCatch(as.character(utils::packageVersion("surveyframe")),
                  error = function(e) "0.3.2")
  lapply(matching, function(cit) {
    if (grepl("%s", cit$apa, fixed = TRUE)) sprintf(cit$apa, ver) else cit$apa
  })
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
  # Empty strings are non-responses (a collected sheet stores "" for items a
  # respondent never saw); count them as missing, not as a category.
  vals[!is.na(vals) & vals == ""] <- NA
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
  # Empty strings are non-responses, not a category (see sframe_run_frequency)
  r[!is.na(r) & r == ""] <- NA
  c[!is.na(c) & c == ""] <- NA
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
  v_ci <- cramers_v_ci(tbl)
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
    v_ci     = v_ci,
    effect_label = sframe_effect_label(effect, "r"),
    apa      = sprintf(
      "\u03c7\u00b2(%d, N = %d) = %.2f, p %s, %s = %.2f%s",
      ct$parameter, n, ct$statistic, sframe_p_string(ct$p.value),
      effect_symbol, effect, sframe_ci_string(v_ci)
    ),
    prompt   = sprintf(
      "The chi-square test %s a significant association between %s and %s (%s = %.2f%s, %s effect). Describe the pattern in the cross-tabulation.",
      if (ct$p.value < .05) "revealed" else "found no",
      vars[1], vars[2], effect_name, effect, sframe_ci_string(v_ci),
      sframe_effect_label(effect, "r")
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
  wt <- tryCatch(
    stats::wilcox.test(g1, g2, exact = FALSE, conf.int = TRUE),
    error = function(e) NULL
  )
  if (is.null(wt)) return(list(test = "mann_whitney", error = "Test failed."))
  n <- length(g1) + length(g2)
  z <- stats::qnorm(wt$p.value / 2)
  r <- abs(z) / sqrt(n)
  r_ci <- sframe_rank_r_ci(g1, g2)
  hl_shift <- unname(wt$estimate)
  hl_conf_int <- as.numeric(wt$conf.int)
  hl_ci_named <- c(lower = hl_conf_int[1], upper = hl_conf_int[2])
  list(
    test     = "mann_whitney",
    vars     = vars,
    groups   = as.character(groups),
    n1       = length(g1), n2 = length(g2),
    median1  = stats::median(g1), median2 = stats::median(g2),
    U        = unname(wt$statistic),
    z        = z, p = wt$p.value, r = r,
    r_ci     = r_ci,
    hl_shift = hl_shift,
    hl_conf_int = hl_conf_int,
    effect_label = sframe_effect_label(r, "r"),
    apa      = sprintf(
      "U = %.0f, z = %.2f, p %s, r = %.2f%s, Hodges-Lehmann shift = %.2f%s",
      wt$statistic, z, sframe_p_string(wt$p.value), r,
      sframe_ci_string(r_ci), hl_shift, sframe_ci_string(hl_ci_named)
    ),
    prompt   = sprintf(
      "The Mann-Whitney test %s a significant difference between %s (Mdn = %.2f) and %s (Mdn = %.2f), U = %.0f, p %s, r = %.2f%s (%s effect). Interpret the direction and practical significance.",
      if (wt$p.value < .05) "revealed" else "did not reveal",
      groups[1], stats::median(g1),
      groups[2], stats::median(g2),
      wt$statistic, sframe_p_string(wt$p.value),
      r, sframe_ci_string(r_ci), sframe_effect_label(r, "r")
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
  d_ci <- cohens_d_ci(g1, g2)
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
    d_ci     = d_ci,
    effect_label = sframe_effect_label(d, "d"),
    apa      = sprintf(
      "t(%.2f) = %.2f, p %s, d = %.2f%s",
      tt$parameter, tt$statistic, sframe_p_string(tt$p.value), d,
      sframe_ci_string(d_ci)
    ),
    prompt   = sprintf(
      "The independent-samples t-test %s a significant difference between %s (M = %.2f, SD = %.2f) and %s (M = %.2f, SD = %.2f), t(%.2f) = %.2f, p %s, d = %.2f%s (%s effect). Discuss the direction and magnitude.",
      if (tt$p.value < .05) "revealed" else "did not reveal",
      groups[1], mean(g1), stats::sd(g1),
      groups[2], mean(g2), stats::sd(g2),
      tt$parameter, tt$statistic,
      sframe_p_string(tt$p.value), d, sframe_ci_string(d_ci),
      sframe_effect_label(d, "d")
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

  eta_ci <- eta_sq_ci(outcome[complete], group[complete])

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
    eta_ci = eta_ci,
    group_means = as.list(group_means),
    tukey = tukey,
    effect_label = sframe_effect_label(eta2, "eta2"),
    apa = sprintf(
      "F(%d, %d) = %.2f, p %s, \u03b7\u00b2 = %.3f%s",
      k - 1, n - k, F_stat, sframe_p_string(p_val), eta2,
      sframe_ci_string(eta_ci)
    ),
    prompt = sprintf(
      "The one-way ANOVA %s a significant effect of %s on %s, F(%d, %d) = %.2f, p %s, \u03b7\u00b2 = %.3f%s (%s effect). Group means: %s. %s",
      if (!is.na(p_val) && p_val < .05) "revealed" else "did not reveal",
      vars[1], vars[2],
      k - 1, n - k, F_stat, sframe_p_string(p_val),
      eta2, sframe_ci_string(eta_ci), sframe_effect_label(eta2, "eta2"),
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
  d_ci <- bootstrap_ci(diff, FUN = function(v) {
    s <- stats::sd(v)
    if (is.na(s) || s == 0) NA_real_ else mean(v) / s
  })

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
    d_ci = d_ci,
    effect_label = sframe_effect_label(abs(dz), "d"),
    apa = sprintf(
      "t(%d) = %.2f, p %s, d_z = %.2f%s",
      unname(tt$parameter), unname(tt$statistic),
      sframe_p_string(tt$p.value), dz, sframe_ci_string(d_ci)
    ),
    prompt = sprintf(
      "The paired-samples t-test %s a significant mean difference between %s (M = %.2f) and %s (M = %.2f), t(%d) = %.2f, p %s, d_z = %.2f%s (%s effect). Interpret the direction and practical significance of the change.",
      if (tt$p.value < .05) "revealed" else "did not reveal",
      vars[1], mean(x), vars[2], mean(y),
      unname(tt$parameter), unname(tt$statistic),
      sframe_p_string(tt$p.value),
      dz, sframe_ci_string(d_ci), sframe_effect_label(abs(dz), "d")
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
    stats::wilcox.test(x, y, paired = TRUE, exact = FALSE, conf.int = TRUE),
    error = function(e) NULL
  )
  if (is.null(wt)) return(list(test = "wilcoxon_pair", error = "Test failed."))

  z <- stats::qnorm(wt$p.value / 2)
  r <- abs(z) / sqrt(n)
  r_ci <- sframe_signed_rank_r_ci(x - y)
  pseudomedian <- unname(wt$estimate)
  pseudomedian_conf_int <- as.numeric(wt$conf.int)
  pm_ci_named <- c(lower = pseudomedian_conf_int[1], upper = pseudomedian_conf_int[2])

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
    r_ci = r_ci,
    pseudomedian = pseudomedian,
    pseudomedian_conf_int = pseudomedian_conf_int,
    effect_label = sframe_effect_label(r, "r"),
    apa = sprintf(
      "V = %.0f, z = %.2f, p %s, r = %.2f%s, pseudomedian = %.2f%s",
      wt$statistic, z, sframe_p_string(wt$p.value), r,
      sframe_ci_string(r_ci), pseudomedian, sframe_ci_string(pm_ci_named)
    ),
    prompt = sprintf(
      "The Wilcoxon signed-rank test %s a significant difference between %s (Mdn = %.2f) and %s (Mdn = %.2f), V = %.0f, z = %.2f, p %s, r = %.2f%s (%s effect). Discuss the direction and practical significance.",
      if (wt$p.value < .05) "revealed" else "did not reveal",
      vars[1], stats::median(x),
      vars[2], stats::median(y),
      wt$statistic, z,
      sframe_p_string(wt$p.value),
      r, sframe_ci_string(r_ci), sframe_effect_label(r, "r")
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
  eta_ci <- sframe_kw_eta_sq_ci(outcome[complete], group[complete])
  list(
    test     = "kruskal_wallis",
    vars     = vars,
    H        = unname(kt$statistic),
    df       = unname(kt$parameter),
    p        = kt$p.value,
    eta2     = eta2,
    eta_ci   = eta_ci,
    effect_label = sframe_effect_label(eta2, "eta2"),
    apa      = sprintf(
      "H(%d) = %.2f, p %s, \u03b7\u00b2 = %.3f%s",
      kt$parameter, kt$statistic, sframe_p_string(kt$p.value), eta2,
      sframe_ci_string(eta_ci)
    ),
    prompt   = sprintf(
      "The Kruskal-Wallis test %s a significant difference across groups, H(%d) = %.2f, p %s, \u03b7\u00b2 = %.3f%s (%s effect). If significant, describe which groups differed and in what direction.",
      if (kt$p.value < .05) "revealed" else "did not reveal",
      kt$parameter, kt$statistic,
      sframe_p_string(kt$p.value), eta2, sframe_ci_string(eta_ci),
      sframe_effect_label(eta2, "eta2")
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
  # Fisher z is analytic for Pearson; the rank correlations bootstrap.
  ci <- if (identical(method, "pearson")) {
    sframe_fisher_z_ci(r, n)
  } else {
    sframe_cor_boot_ci(x[complete], y[complete], method)
  }
  list(
    test     = paste0("correlation_", method),
    vars     = vars,
    method   = method,
    n        = n,
    r        = r,
    df       = n - 2,
    p        = ct$p.value,
    ci       = ci,
    effect_label = sframe_effect_label(abs(r), "r"),
    apa      = sprintf(
      "%s(%d) = %.2f%s, p %s",
      sym, n - 2, r, sframe_ci_string(ci), sframe_p_string(ct$p.value)
    ),
    prompt   = sprintf(
      "There was a %s, %s %s correlation between %s and %s, %s(%d) = %.2f%s, p %s. Explain what this means for your research question.",
      if (r > 0) "positive" else "negative",
      sframe_effect_label(abs(r), "r"),
      if (ct$p.value < .05) "significant" else "non-significant",
      vars[1], vars[2],
      sym, n - 2, r, sframe_ci_string(ci), sframe_p_string(ct$p.value)
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
  # Plain data frame, not the lm object itself: keeps the result
  # JSON-serialisable (jsonlite::toJSON on an lm object is unstable across
  # sessions) while still carrying what sframe_plot_regression_diagnostics()
  # needs for the four standard diagnostic panels.
  diagnostics <- data.frame(
    fitted    = unname(stats::fitted(fit)),
    resid     = unname(stats::residuals(fit)),
    std_resid = unname(stats::rstandard(fit)),
    hat       = unname(stats::hatvalues(fit)),
    cooksd    = unname(stats::cooks.distance(fit))
  )
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
    diagnostics  = diagnostics,
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
    firth_logistic = c(sframe_role_values(roles, c("predictors", "covariates")),
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
  # Keep the original classed object too: sframe_plot_for_result() dispatches
  # plot.sframe_quality_report()/plot.sframe_reliability_report()/etc. on
  # this, since the unclassed, analysis-plan-field-merged `out` above is not
  # safe to iterate as if every element were still a per-scale/report entry.
  out$report_obj <- report
  out
}

# v0.3.4: formatted result tables for the inferential runners, suitable for
# knitr::kable(). Built centrally from each runner's existing fields so the
# runners themselves stay untouched; results that already carry a table
# (frequency, crosstab) keep it.
sframe_result_table <- function(result) {
  if (!is.null(result$table) || !is.null(result$error)) return(result$table)
  fmt <- function(x, d = 2) {
    ifelse(is.na(x), "", formatC(as.numeric(x), digits = d, format = "f"))
  }
  test <- result$test %||% ""
  switch(
    test,
    correlation_pearson  = ,
    correlation_spearman = ,
    correlation_kendall  = data.frame(
      Statistic = switch(result$method,
                         pearson  = "Pearson r",
                         spearman = "Spearman rho",
                         kendall  = "Kendall tau",
                         "Correlation"),
      n = result$n,
      df = result$df,
      Estimate = fmt(result$r),
      p = sframe_p_string(result$p),
      `Effect size` = result$effect_label %||% "",
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    regression_linear = {
      co <- result$coefficients
      if (!is.data.frame(co)) return(NULL)
      data.frame(
        Term = rownames(co),
        Estimate = fmt(co[[1]]),
        `Std. error` = fmt(co[[2]]),
        t = fmt(co[[3]]),
        p = vapply(co[[4]], sframe_p_string, character(1)),
        check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
      )
    },
    t_test_ind = data.frame(
      Group = result$groups,
      n = c(result$n1, result$n2),
      Mean = fmt(c(result$mean1, result$mean2)),
      SD = fmt(c(result$sd1, result$sd2)),
      stringsAsFactors = FALSE
    ),
    mann_whitney = data.frame(
      Group = result$groups,
      n = c(result$n1, result$n2),
      Median = fmt(c(result$median1, result$median2)),
      stringsAsFactors = FALSE
    ),
    anova_one = data.frame(
      Statistic = "F",
      df1 = result$df1, df2 = result$df2,
      Estimate = fmt(result$F_stat),
      p = sframe_p_string(result$p),
      `Eta squared` = fmt(result$eta2, 3),
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    kruskal_wallis = data.frame(
      Statistic = "H",
      df = result$df,
      Estimate = fmt(result$H),
      p = sframe_p_string(result$p),
      `Eta squared` = fmt(result$eta2, 3),
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    t_test_pair = data.frame(
      Variable = result$vars,
      n = result$n,
      Mean = fmt(c(result$mean_x, result$mean_y)),
      stringsAsFactors = FALSE
    ),
    wilcoxon_pair = data.frame(
      Variable = result$vars,
      n = result$n,
      Median = fmt(c(result$median_x, result$median_y)),
      stringsAsFactors = FALSE
    ),
    friedman = data.frame(
      Statistic = "Friedman chi-square",
      df = result$df, n = result$n,
      Estimate = fmt(result$statistic),
      p = sframe_p_string(result$p),
      `Kendall's W` = fmt(result$statistic / (result$n * (length(result$vars) - 1)), 3),
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    partial_correlation = data.frame(
      Statistic = "Partial r",
      n = result$n,
      Estimate = fmt(result$r),
      p = sframe_p_string(result$p),
      Controls = paste(result$controls, collapse = ", "),
      stringsAsFactors = FALSE
    ),
    regression_logistic_binary = {
      co <- result$coefficients
      if (!is.data.frame(co)) return(NULL)
      data.frame(
        Term = rownames(co),
        Estimate = fmt(co[["Estimate"]]),
        `Odds ratio` = fmt(co[["odds_ratio"]]),
        p = vapply(co[["Pr(>|z|)"]], sframe_p_string, character(1)),
        check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
      )
    },
    firth_logistic = {
      co <- result$coefficients
      if (!is.data.frame(co)) return(NULL)
      data.frame(
        Term = rownames(co),
        Estimate = fmt(co[["Estimate"]]),
        `Odds ratio` = fmt(co[["odds_ratio"]]),
        `CI low` = fmt(co[["or_ci_low"]]),
        `CI high` = fmt(co[["or_ci_high"]]),
        p = vapply(co[["p"]], sframe_p_string, character(1)),
        check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
      )
    },
    regression_logistic_ordinal = {
      co <- result$coefficients
      if (!is.data.frame(co)) return(NULL)
      data.frame(
        Term = rownames(co),
        Estimate = fmt(co[["Value"]]),
        `Odds ratio` = fmt(co[["odds_ratio"]]),
        check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
      )
    },
    moderation = {
      co <- result$coefficients
      if (!is.data.frame(co)) return(NULL)
      data.frame(
        Term = rownames(co),
        Estimate = fmt(co[[1]]),
        `Std. error` = fmt(co[[2]]),
        t = fmt(co[[3]]),
        p = vapply(co[[4]], sframe_p_string, character(1)),
        check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
      )
    },
    mediation = data.frame(
      Effect = c("Direct (c')", "Indirect (a\u00D7b)", "Total (c)"),
      Estimate = fmt(c(result$direct, result$indirect, result$total)),
      `95% CI` = c("", sprintf("[%.3f, %.3f]", result$indirect_ci[[1]], result$indirect_ci[[2]]), ""),
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    quality = {
      qr <- result$report_obj
      if (is.null(qr)) NULL else sframe_quality_checks_table(qr)
    },
    reliability_alpha = {
      rr <- result$report_obj
      if (is.null(rr)) NULL else sframe_reliability_table(rr, c(alpha = "Alpha", alpha_std = "Alpha (std.)"))
    },
    reliability_omega = {
      rr <- result$report_obj
      if (is.null(rr)) NULL else sframe_reliability_table(rr, c(omega_h = "Omega (h)", omega_t = "Omega (t)"))
    },
    item_diagnostics = {
      ir <- result$report_obj
      if (is.null(ir)) NULL else sframe_item_diagnostics_table(ir)
    },
    efa_readiness = {
      er <- result$report_obj
      if (is.null(er)) NULL else data.frame(
        Statistic = c("KMO (MSA)", "Bartlett chi-square", "Bartlett df",
                      "Bartlett p", "Suggested factors"),
        Value = c(
          fmt(er$kmo$MSA), fmt(er$bartlett$chisq), er$bartlett$df,
          sframe_p_string(er$bartlett$p.value), er$suggested_nfactors
        ),
        stringsAsFactors = FALSE
      )
    },
    efa_solution = {
      es <- result$report_obj
      if (is.null(es) || !is.data.frame(es$loadings_long)) NULL else {
        wide <- stats::reshape(es$loadings_long, direction = "wide",
                               idvar = "item_id", timevar = "factor")
        names(wide) <- sub("^loading\\.", "", names(wide))
        num_cols <- vapply(wide, is.numeric, logical(1))
        wide[num_cols] <- lapply(wide[num_cols], function(col) round(col, 3))
        wide
      }
    },
    NULL
  )
}

# One row per scale, flattening reliability_report()'s per-scale summaries
# into a kable-ready table. `columns` is a named vector mapping the report's
# field name to the column heading to use, since reliability_alpha and
# reliability_omega only ever populate one coefficient or the other.
sframe_reliability_table <- function(rr, columns) {
  rows <- lapply(rr, function(s) {
    if (!is.list(s) || is.null(s$scale_id)) return(NULL)
    row <- data.frame(
      Scale = s$label %||% s$scale_id, `N items` = s$n_items, N = s$n,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    for (field in names(columns)) {
      row[[columns[[field]]]] <- round(s[[field]] %||% NA_real_, 3)
    }
    row
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

# item_report() is keyed by scale id, each holding its own item-level
# diagnostics data.frame; stack them into one table with a Scale column so
# every item across every scale shows in one place.
sframe_item_diagnostics_table <- function(ir) {
  rows <- lapply(ir, function(s) {
    if (!is.list(s) || !is.data.frame(s$diagnostics)) return(NULL)
    d <- s$diagnostics
    d$Scale <- s$label %||% s$scale_id
    d
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  out <- out[, c("Scale", setdiff(colnames(out), "Scale")), drop = FALSE]
  rownames(out) <- NULL
  out
}

# quality_report() covers several distinct checks (attention, straight-
# lining, duplicates) in separate nested sections; flatten them into one
# table, one row per check, so the "Do respondents meet quality
# thresholds?" research question has a single table to point at.
sframe_quality_checks_table <- function(qr) {
  rows <- list()
  for (a in qr$attention %||% list()) {
    rows[[length(rows) + 1]] <- data.frame(
      Check = a$check_id %||% "", Type = "Attention check",
      Result = sprintf("%.1f%% pass (%d fail)", (a$pass_rate %||% NA_real_) * 100, a$n_fail %||% NA_integer_),
      stringsAsFactors = FALSE
    )
  }
  for (s in qr$straightline %||% list()) {
    # A scale below straightline_min_items carries flag_rate = NA and
    # checked = FALSE (see quality_report()), so it is reported as
    # not checked rather than as sprintf() silently rendering "NA% flagged".
    result_text <- if (isFALSE(s$checked)) {
      sprintf("not checked, %d items", s$n_items %||% NA_integer_)
    } else {
      sprintf("%.1f%% flagged", (s$flag_rate %||% NA_real_) * 100)
    }
    rows[[length(rows) + 1]] <- data.frame(
      Check = s$scale_id %||% "", Type = "Straight-lining",
      Result = result_text,
      stringsAsFactors = FALSE
    )
  }
  rows[[length(rows) + 1]] <- data.frame(
    Check = "duplicates", Type = "Duplicate rows",
    Result = sprintf("%d flagged", qr$duplicates$n_duplicates %||% 0L),
    stringsAsFactors = FALSE
  )
  do.call(rbind, rows)
}

sframe_run_one_block <- function(block, data, instrument, plots = FALSE,
                                 plot_palette = "web") {
  test <- sframe_analysis_method(block)
  roles <- sframe_analysis_roles(block)
  vars <- sframe_vars_for_method(test, roles, block)
  options <- block$options %||% list()
  weights <- sframe_role_values(roles, c("weights", "weight"))
  # Decision methods take their criterion weights through `weights_item` or
  # `options$weights` (a numeric vector), so the frequency-weighting variable
  # role must never be folded into their options.
  if (test %in% sframe_decision_methods) weights <- character(0)
  if (length(weights) > 0 && is.null(options$weights)) {
    options$weights <- weights[1]
  }
  # Optional `group` role on term_freq/tidy_sentiment (todo_text_analysis.md section
  # 1a): resolved into options the same way weights is above, so the 2
  # runners read options$group without knowing about roles at all. Additive
  # only, so every other method's options are untouched.
  if (test %in% c("term_freq", "tidy_sentiment")) {
    group <- sframe_role_values(roles, "group")
    if (length(group) > 0 && is.null(options$group)) {
      options$group <- group[1]
    }
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
      # pls_sem is accepted as an alias: real instruments declare the method
      # by the model type, so both ids resolve to the seminr syntax runner
      pls_sem            = ,
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
      firth_logistic     = sframe_run_firth_logistic(data, roles, options),
      topsis             = sframe_run_topsis(data, roles, options, instrument),
      ahp                = sframe_run_ahp(data, roles, options, instrument),
      anp                = sframe_run_anp(data, roles, options, instrument),
      dematel            = sframe_run_dematel(data, roles, options, instrument),
      vikor              = sframe_run_vikor(data, roles, options, instrument),
      moora              = sframe_run_moora(data, roles, options, instrument),
      smart              = sframe_run_smart(data, roles, options, instrument),
      waspas             = sframe_run_waspas(data, roles, options, instrument),
      promethee          = sframe_run_promethee(data, roles, options, instrument),
      electre            = sframe_run_electre(data, roles, options, instrument),
      moderation = sframe_run_moderation(data, roles),
      mediation = sframe_run_mediation(data, roles, options),
      term_freq = sframe_run_term_freq(data, roles, options, instrument),
      co_occurrence = sframe_run_co_occurrence(data, roles, options, instrument),
      topic_model_lda = sframe_run_topic_model_lda(data, roles, options, instrument),
      stm_topics = sframe_run_stm_topics(data, roles, options, instrument),
      ngram_freq = sframe_run_ngram_freq(data, roles, options, instrument),
      term_context = sframe_run_term_context(data, roles, options, instrument),
      co_occurrence_network = sframe_run_cooccurrence_network(data, roles, options, instrument),
      tidy_sentiment = sframe_run_tidy_sentiment(data, roles, options, instrument),
      quanteda_dfm = sframe_run_quanteda_dfm(data, roles, options, instrument),
      list(test = test, error = paste0("Test '", test, "' is unavailable."))
    )
  }, error = function(e) {
    list(test = test, error = conditionMessage(e))
  })

  # Weight sensitivity is opt-in per block via options$sensitivity = TRUE.
  # Attached here, once, rather than inside each of the 7 ranking runners,
  # for the same reason label substitution below is handled centrally.
  result <- tryCatch(
    sframe_attach_sensitivity(result, data, roles, options, instrument),
    error = function(e) result
  )

  result$research_question <- block$research_question
  result$block_id          <- block$id
  result$family            <- block$family %||% ""
  result$roles             <- roles
  result$options           <- options
  result$hypotheses        <- block$hypotheses %||% NULL
  result$decision_rule     <- block$decision_rule %||% block$interpretation %||% ""
  result$interpretation    <- block$interpretation %||% block$decision_rule %||% ""
  result$citations         <- sframe_citations_for_test(test)
  if (is.null(result$table)) {
    result$table <- tryCatch(sframe_result_table(result), error = function(e) NULL)
  }
  # The runners above compute on the raw response codes (item ids, coded
  # choice values), so the table they build carries those, not what a
  # reader sees. Substitute labels here, once, for every test type, rather
  # than teaching each runner about the instrument's labels. A text-family
  # id's free-text columns (term, term_a/term_b, before/match/after) are
  # excluded via .sframe_text_free_text_cols: those hold a respondent's own
  # words, not coded values, and humanising them risks silently relabelling
  # a term that happens to collide with an unrelated item's choice code. A
  # coded column on the same table, such as term_freq's `group`, is not in
  # that exclusion list and keeps humanising normally.
  result$table <- tryCatch(
    sframe_humanize_table(result$table, sframe_label_lookup(instrument),
                          exclude_cols = .sframe_text_free_text_cols[[test]] %||% character(0)),
    error = function(e) result$table
  )
  if (isTRUE(plots) && is.null(result$plot)) {
    result$plot <- sframe_plot_for_result(result, data, palette = plot_palette)
  }
  if (isTRUE(plots) && identical(result$test, "regression_linear") &&
      is.data.frame(result$diagnostics) && is.null(result$diagnostic_plots)) {
    result$diagnostic_plots <-
      sframe_plot_regression_diagnostics(result, palette = plot_palette)
  }
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
#' @param plots Logical. When `TRUE` and ggplot2 is installed, supported
#'   blocks gain a `$plot` element holding a brand-styled ggplot object:
#'   bar charts for frequency and chi-square blocks, scatter plots with a
#'   regression overlay for correlation and linear-regression blocks.
#'   Defaults to `FALSE`.
#' @param seed Integer or `NULL`. The random seed the analysis runs under.
#'   Defaults to a fixed value so the same instrument and the same data give
#'   the same answer every time, which is what makes a rendered report usable
#'   as an audit artefact. Every bootstrap confidence interval in the plan, and
#'   the parallel analysis behind the EFA family, depend on it. Pass `NULL` for
#'   the unseeded behaviour of releases before 0.4.1. The caller's own random
#'   stream is restored afterwards, so seeding here does not affect anything
#'   that runs later.
#' @param plot_palette One of `"web"` (brand colours, for on-screen use) or
#'   `"print"` (black, grey, and white, for journal-ready print figures).
#'   Applied to every plot attached when `plots = TRUE`. See `sframe_brand()`.
#'
#' @return An object of class `sframe_analysis_results`, a list with one
#'   element per analysis block. Each element contains the test result,
#'   APA string, interpretation prompt, and reporting-reference metadata.
#'   Inferential blocks also carry a `$table` data frame suitable for
#'   `knitr::kable()`. Pass to [render_results()] to generate a formatted
#'   report.
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
#' \donttest{
#' results <- run_analysis_plan(responses, instr)
#' print(results)
#' }
run_analysis_plan <- function(data, instrument, scored = TRUE, plots = FALSE,
                              plot_palette = c("web", "print"), seed = 20260828L) {
  plot_palette <- match.arg(plot_palette)
  sframe_check_instrument(instrument)
  stopifnot(is.data.frame(data))

  # Seeded by default so the same instrument and data give the same answer
  # every time, which is what makes a rendered report usable as evidence.
  # Seeding once here covers every bootstrap interval and the EFA parallel
  # analysis. sframe_with_seed() restores the caller's RNG state on exit.
  # Pass seed = NULL for the pre-0.4.1 behaviour.
  if (!is.null(seed)) {
    out <- sframe_with_seed(seed, run_analysis_plan(
      data, instrument, scored = scored, plots = plots,
      plot_palette = plot_palette, seed = NULL))
    # Recorded so render_report() can print it next to the instrument hash: a
    # report should state what reproducing it would take.
    attr(out, "seed") <- seed
    return(out)
  }
  if (isTRUE(plots)) {
    rlang::check_installed("ggplot2",
      reason = "to attach plots to analysis results (plots = TRUE).")
  }

  plan <- instrument$analysis_plan
  if (is.null(plan) || length(plan) == 0) {
    rlang::abort(
      "No analysis plan found in this instrument. Add research questions to instrument$analysis_plan, or use the SurveyBuilder Analyse menu to build the plan visually.",
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

  results <- lapply(plan, sframe_run_one_block, data = data,
                    plot_palette = plot_palette,
                    instrument = instrument, plots = plots)
  names(results) <- vapply(plan, function(b) b$id %||% "", character(1))
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

# Escape HTML and render the limited markdown used in citations (*italic*) so
# references show as italics rather than literal asterisks.
sframe_md_em <- function(x) {
  x <- htmltools_escape(x)
  gsub("\\*([^*]+)\\*", "<em>\\1</em>", x)
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
#' @param interpretations Named list or NULL. Written interpretations keyed
#'   by analysis-plan block id, added after the results are known. A block
#'   with an entry shows that text in its Interpretation section in place
#'   of the pre-declared prompt fallback. Blocks without an entry render
#'   exactly as they do when this argument is NULL. Interpretations are
#'   report content only and are never written into the instrument.
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
#' \donttest{
#' results <- run_analysis_plan(responses, instr)
#' out <- render_results(results, instr,
#'                       output_file = tempfile(fileext = ".html"))
#' file.exists(out)
#' }
render_results <- function(
    results         = NULL,
    instrument,
    output_file     = NULL,
    output_path     = NULL,
    citation_format = c("apa", "ama", "vancouver"),
    title           = NULL,
    interpretations = NULL
) {
  sframe_check_instrument(instrument)
  citation_format <- rlang::arg_match(citation_format)
  interpretations <- sframe_clean_interpretations(interpretations)

  dest <- output_file %||% output_path %||% tempfile(fileext = ".html")

  # If called with instrument only (no pre-computed results), delegate
  if (is.null(results)) {
    return(render_report(instrument, output_file = dest,
                         interpretations = interpretations))
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
    interp  <- interpretations[[r$block_id %||% r$id %||% ""]] %||% r$interpretation %||% ""
    cits <- paste(
      vapply(unlist(r$citations), sframe_md_em, character(1)),
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
      tbl <- r$table
      num_cols <- vapply(tbl, is.numeric, logical(1))
      if (any(num_cols)) tbl[num_cols] <- lapply(tbl[num_cols], function(col) round(col, 2))
      rows <- paste(apply(tbl, 1, function(row) {
        cells <- paste(sprintf("<td>%s</td>", htmltools_escape(as.character(row))),
                      collapse = "")
        sprintf("<tr>%s</tr>", cells)
      }), collapse = "")
      headers <- paste(sprintf("<th>%s</th>", htmltools_escape(colnames(tbl))),
                      collapse = "")
      has_pval <- any(grepl(
        "^p$|^p\\.value$|^p_value$|^Pr\\(>",
        colnames(tbl), ignore.case = TRUE
      ))
      foot_html <- if (has_pval) {
        paste0(
          '<tfoot><tr><td colspan="', ncol(tbl), '">',
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
        vapply(unlist(all_citations), sframe_md_em, character(1))
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
  body { font-family: system-ui, -apple-system, "Segoe UI", Roboto, sans-serif; max-width: 900px;
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
  .result-box { background: #e6f7f7; border-left: 4px solid #16B3B1;
                 padding: 14px 18px; border-radius: 0 8px 8px 0; margin-bottom: 18px; }
  .sf-foot { text-align: center; font-size: 12px; color: #94a3b8; margin-top: 40px;
              padding-top: 16px; border-top: 1px solid #eee; }
  .sf-foot a { color: #16B3B1; font-weight: 600; text-decoration: none; }
  .apa-string { font-size: 15px; color: #1a1a2e; }
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
                    display: block; overflow-x: auto; max-width: 100%%;
                    margin: 12px 0; }
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
<div class="sf-foot">Built with <a href="https://cran.r-project.org/package=surveyframe" target="_blank" rel="noopener">surveyframe</a></div>
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
