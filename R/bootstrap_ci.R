# R/bootstrap_ci.R
# Percentile bootstrap confidence intervals for effect sizes. Base R only:
# these helpers back the CI keys the analysis-plan runners attach to their
# results, and v0.4's small-sample methods reuse bootstrap_ci() directly.

#' Percentile bootstrap confidence interval for a statistic
#'
#' Resamples `x` with replacement `R` times, applies `FUN` to each resample,
#' and returns the percentile interval of the resampled statistics together
#' with the observed value.
#'
#' @param x A numeric vector.
#' @param FUN A function of one vector returning a single number. Defaults
#'   to [stats::median()].
#' @param R Integer. Number of bootstrap resamples. Defaults to 2000.
#' @param conf.level Confidence level. Defaults to 0.95.
#' @param seed Integer or NULL. When supplied, sets the random seed so the
#'   interval is reproducible.
#'
#' @return A named numeric vector: `estimate`, `lower`, `upper`. The bounds
#'   are `NA` when `x` has fewer than 3 finite values.
#' @export
#' @seealso [cohens_d_ci()], [cramers_v_ci()], [eta_sq_ci()]
#'
#' @examples
#' bootstrap_ci(mtcars$mpg, seed = 42)
#' bootstrap_ci(mtcars$mpg, FUN = mean, conf.level = 0.90, seed = 42)
bootstrap_ci <- function(x, FUN = stats::median, R = 2000,
                         conf.level = 0.95, seed = NULL) {
  if (!is.null(seed)) {
    # set.seed() mutates the global RNG stream; restore whatever state the
    # caller had on exit so an opt-in, reproducible seed here does not
    # silently reset randomness for unrelated code that runs afterwards.
    has_state <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_state <- if (has_state) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit({
      if (has_state) {
        assign(".Random.seed", old_state, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(seed)
  }
  x <- x[is.finite(x)]
  obs <- if (length(x) > 0) FUN(x) else NA_real_
  if (length(x) < 3) {
    return(c(estimate = unname(obs), lower = NA_real_, upper = NA_real_))
  }
  boots <- replicate(R, FUN(sample(x, replace = TRUE)))
  a <- (1 - conf.level) / 2
  ci <- stats::quantile(boots, c(a, 1 - a), names = FALSE, na.rm = TRUE)
  c(estimate = unname(obs), lower = ci[[1]], upper = ci[[2]])
}

#' Bootstrap confidence interval for Cohen's d
#'
#' Percentile bootstrap for the standardised mean difference between two
#' independent groups. Each resample draws within each group, preserving the
#' group sizes.
#'
#' @param x,y Numeric vectors, one per group.
#' @param R Integer. Number of bootstrap resamples. Defaults to 2000.
#' @param conf.level Confidence level. Defaults to 0.95.
#' @param seed Integer or NULL. When supplied, sets the random seed.
#'
#' @return A named numeric vector: `estimate`, `lower`, `upper`. The bounds
#'   are `NA` when either group has fewer than 3 finite values.
#' @export
#' @seealso [bootstrap_ci()]
#'
#' @examples
#' cohens_d_ci(mtcars$mpg[mtcars$am == 1], mtcars$mpg[mtcars$am == 0],
#'             seed = 42)
cohens_d_ci <- function(x, y, R = 2000, conf.level = 0.95, seed = NULL) {
  if (!is.null(seed)) {
    # set.seed() mutates the global RNG stream; restore whatever state the
    # caller had on exit so an opt-in, reproducible seed here does not
    # silently reset randomness for unrelated code that runs afterwards.
    has_state <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_state <- if (has_state) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit({
      if (has_state) {
        assign(".Random.seed", old_state, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(seed)
  }
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  obs <- sframe_cohens_d(x, y)
  if (length(x) < 3 || length(y) < 3 || is.na(obs)) {
    return(c(estimate = unname(obs), lower = NA_real_, upper = NA_real_))
  }
  boots <- replicate(R, sframe_cohens_d(sample(x, replace = TRUE),
                                        sample(y, replace = TRUE)))
  a <- (1 - conf.level) / 2
  ci <- stats::quantile(boots, c(a, 1 - a), names = FALSE, na.rm = TRUE)
  c(estimate = unname(obs), lower = ci[[1]], upper = ci[[2]])
}

#' Bootstrap confidence interval for Cramer's V
#'
#' Percentile bootstrap for the association strength in a contingency table.
#' The table is expanded back to individual observations, which are resampled
#' jointly. For a 2 by 2 table the statistic equals phi.
#'
#' @param tab A contingency table (from [table()]) or a matrix of counts.
#' @param R Integer. Number of bootstrap resamples. Defaults to 2000.
#' @param conf.level Confidence level. Defaults to 0.95.
#' @param seed Integer or NULL. When supplied, sets the random seed.
#'
#' @return A named numeric vector: `estimate`, `lower`, `upper`. The bounds
#'   are `NA` when the table holds fewer than 3 observations.
#' @export
#' @seealso [bootstrap_ci()]
#'
#' @examples
#' cramers_v_ci(table(mtcars$am, mtcars$cyl), seed = 42)
cramers_v_ci <- function(tab, R = 2000, conf.level = 0.95, seed = NULL) {
  if (!is.null(seed)) {
    # set.seed() mutates the global RNG stream; restore whatever state the
    # caller had on exit so an opt-in, reproducible seed here does not
    # silently reset randomness for unrelated code that runs afterwards.
    has_state <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_state <- if (has_state) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit({
      if (has_state) {
        assign(".Random.seed", old_state, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(seed)
  }
  tab <- as.table(as.matrix(tab))
  cases <- as.data.frame(tab, stringsAsFactors = TRUE)
  cases <- cases[rep(seq_len(nrow(cases)), cases$Freq), 1:2, drop = FALSE]
  n <- nrow(cases)
  obs <- sframe_cramers_v(table(cases[[1]], cases[[2]]))
  if (n < 3 || is.na(obs)) {
    return(c(estimate = unname(obs), lower = NA_real_, upper = NA_real_))
  }
  boots <- replicate(R, {
    idx <- sample.int(n, replace = TRUE)
    sframe_cramers_v(table(cases[[1]][idx], cases[[2]][idx]))
  })
  a <- (1 - conf.level) / 2
  ci <- stats::quantile(boots, c(a, 1 - a), names = FALSE, na.rm = TRUE)
  c(estimate = unname(obs), lower = ci[[1]], upper = ci[[2]])
}

# Cramer's V (phi for 2 by 2) from a contingency table, NA when degenerate.
sframe_cramers_v <- function(tbl) {
  n <- sum(tbl)
  k <- min(nrow(tbl) - 1, ncol(tbl) - 1)
  if (n < 1 || k < 1) return(NA_real_)
  chi <- tryCatch(
    suppressWarnings(stats::chisq.test(tbl, correct = FALSE)$statistic),
    error = function(e) NA_real_
  )
  if (is.na(chi)) return(NA_real_)
  sqrt(unname(chi) / (n * k))
}

#' Bootstrap confidence interval for eta squared
#'
#' Percentile bootstrap for the proportion of variance in `outcome`
#' explained by `group`, resampling observations jointly so the group
#' structure travels with each resample.
#'
#' @param outcome A numeric vector.
#' @param group A grouping vector of the same length.
#' @param R Integer. Number of bootstrap resamples. Defaults to 2000.
#' @param conf.level Confidence level. Defaults to 0.95.
#' @param seed Integer or NULL. When supplied, sets the random seed.
#'
#' @return A named numeric vector: `estimate`, `lower`, `upper`. The bounds
#'   are `NA` with fewer than 3 complete observations or fewer than 2 groups.
#' @export
#' @seealso [bootstrap_ci()]
#'
#' @examples
#' eta_sq_ci(mtcars$mpg, mtcars$cyl, seed = 42)
eta_sq_ci <- function(outcome, group, R = 2000, conf.level = 0.95,
                      seed = NULL) {
  if (!is.null(seed)) {
    # set.seed() mutates the global RNG stream; restore whatever state the
    # caller had on exit so an opt-in, reproducible seed here does not
    # silently reset randomness for unrelated code that runs afterwards.
    has_state <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_state <- if (has_state) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit({
      if (has_state) {
        assign(".Random.seed", old_state, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(seed)
  }
  keep <- is.finite(suppressWarnings(as.numeric(outcome))) & !is.na(group)
  outcome <- as.numeric(outcome)[keep]
  group <- as.factor(as.vector(group)[keep])
  n <- length(outcome)
  obs <- sframe_eta_sq(outcome, group)
  if (n < 3 || nlevels(droplevels(group)) < 2 || is.na(obs)) {
    return(c(estimate = unname(obs), lower = NA_real_, upper = NA_real_))
  }
  boots <- replicate(R, {
    idx <- sample.int(n, replace = TRUE)
    sframe_eta_sq(outcome[idx], group[idx])
  })
  a <- (1 - conf.level) / 2
  ci <- stats::quantile(boots, c(a, 1 - a), names = FALSE, na.rm = TRUE)
  c(estimate = unname(obs), lower = ci[[1]], upper = ci[[2]])
}

# Eta squared as between-group over total sum of squares, NA when degenerate
# (a resample can collapse to one group).
sframe_eta_sq <- function(outcome, group) {
  group <- droplevels(as.factor(group))
  if (nlevels(group) < 2) return(NA_real_)
  grand <- mean(outcome)
  ss_total <- sum((outcome - grand)^2)
  if (ss_total == 0) return(NA_real_)
  means <- tapply(outcome, group, mean)
  counts <- tapply(outcome, group, length)
  ss_between <- sum(counts * (means - grand)^2)
  ss_between / ss_total
}

# Fisher-z confidence interval for a Pearson correlation. Analytic, so the
# Pearson runner does not pay for a bootstrap.
sframe_fisher_z_ci <- function(r, n, conf.level = 0.95) {
  if (is.na(r) || n < 4 || abs(r) >= 1) {
    return(c(estimate = r, lower = NA_real_, upper = NA_real_))
  }
  z <- atanh(r)
  se <- 1 / sqrt(n - 3)
  crit <- stats::qnorm(1 - (1 - conf.level) / 2)
  c(estimate = r, lower = tanh(z - crit * se), upper = tanh(z + crit * se))
}

# " [lower, upper]" for an apa string, or "" when the interval is missing,
# so runners on degenerate data keep their 0.3.3 output.
sframe_ci_string <- function(ci) {
  if (is.null(ci) || anyNA(ci[c("lower", "upper")])) return("")
  sprintf(" [%.2f, %.2f]", ci[["lower"]], ci[["upper"]])
}

# --- Internal helpers backing the runners' CI keys ------------------------
# Fast recomputations of the rank-based effect sizes the runners report, so
# a 2000-resample bootstrap does not pay wilcox.test/kruskal.test overhead
# on every draw. Each mirrors the normal approximation the corresponding
# test uses (tie-corrected).

# Rank effect r = |z| / sqrt(n) for two independent groups (Mann-Whitney).
sframe_rank_r <- function(g1, g2) {
  n1 <- length(g1); n2 <- length(g2); n <- n1 + n2
  if (n1 < 1 || n2 < 1) return(NA_real_)
  r_all <- rank(c(g1, g2))
  U <- sum(r_all[seq_len(n1)]) - n1 * (n1 + 1) / 2
  mu <- n1 * n2 / 2
  ties <- table(r_all)
  sig2 <- n1 * n2 / 12 * ((n + 1) - sum(ties^3 - ties) / (n * (n - 1)))
  if (!is.finite(sig2) || sig2 <= 0) return(NA_real_)
  abs((U - mu) / sqrt(sig2)) / sqrt(n)
}

sframe_rank_r_ci <- function(g1, g2, R = 2000, conf.level = 0.95) {
  obs <- sframe_rank_r(g1, g2)
  if (length(g1) < 3 || length(g2) < 3 || is.na(obs)) {
    return(c(estimate = unname(obs), lower = NA_real_, upper = NA_real_))
  }
  boots <- replicate(R, sframe_rank_r(sample(g1, replace = TRUE),
                                      sample(g2, replace = TRUE)))
  a <- (1 - conf.level) / 2
  ci <- stats::quantile(boots, c(a, 1 - a), names = FALSE, na.rm = TRUE)
  c(estimate = unname(obs), lower = ci[[1]], upper = ci[[2]])
}

# Signed-rank effect r = |z| / sqrt(n) for paired differences (Wilcoxon).
sframe_signed_rank_r <- function(d) {
  d <- d[is.finite(d) & d != 0]
  n <- length(d)
  if (n < 2) return(NA_real_)
  r <- rank(abs(d))
  V <- sum(r[d > 0])
  mu <- n * (n + 1) / 4
  ties <- table(r)
  sig2 <- n * (n + 1) * (2 * n + 1) / 24 - sum(ties^3 - ties) / 48
  if (!is.finite(sig2) || sig2 <= 0) return(NA_real_)
  abs((V - mu) / sqrt(sig2)) / sqrt(n)
}

sframe_signed_rank_r_ci <- function(d, R = 2000, conf.level = 0.95) {
  obs <- sframe_signed_rank_r(d)
  if (length(d) < 3 || is.na(obs)) {
    return(c(estimate = unname(obs), lower = NA_real_, upper = NA_real_))
  }
  boots <- replicate(R, sframe_signed_rank_r(sample(d, replace = TRUE)))
  a <- (1 - conf.level) / 2
  ci <- stats::quantile(boots, c(a, 1 - a), names = FALSE, na.rm = TRUE)
  c(estimate = unname(obs), lower = ci[[1]], upper = ci[[2]])
}

# The H-based eta squared the Kruskal-Wallis runner reports, recomputed
# from ranks: eta2 = (H - k + 1) / (n - k), tie-corrected, floored at 0.
sframe_kw_eta_sq <- function(outcome, group) {
  group <- droplevels(as.factor(group))
  k <- nlevels(group)
  n <- length(outcome)
  if (k < 2 || n <= k) return(NA_real_)
  r <- rank(outcome)
  Rj <- tapply(r, group, sum)
  nj <- tapply(r, group, length)
  H <- 12 / (n * (n + 1)) * sum(Rj^2 / nj) - 3 * (n + 1)
  ties <- table(r)
  corr <- 1 - sum(ties^3 - ties) / (n^3 - n)
  if (is.finite(corr) && corr > 0) H <- H / corr
  max(0, (H - k + 1) / (n - k))
}

sframe_kw_eta_sq_ci <- function(outcome, group, R = 2000, conf.level = 0.95) {
  group <- as.factor(group)
  n <- length(outcome)
  obs <- sframe_kw_eta_sq(outcome, group)
  if (n < 3 || is.na(obs)) {
    return(c(estimate = unname(obs), lower = NA_real_, upper = NA_real_))
  }
  boots <- replicate(R, {
    idx <- sample.int(n, replace = TRUE)
    sframe_kw_eta_sq(outcome[idx], group[idx])
  })
  a <- (1 - conf.level) / 2
  ci <- stats::quantile(boots, c(a, 1 - a), names = FALSE, na.rm = TRUE)
  c(estimate = unname(obs), lower = ci[[1]], upper = ci[[2]])
}

# Bootstrap CI for Spearman and Kendall correlations by resampling pairs.
sframe_cor_boot_ci <- function(x, y, method, R = 2000, conf.level = 0.95) {
  n <- length(x)
  obs <- suppressWarnings(stats::cor(x, y, method = method))
  if (n < 4 || is.na(obs)) {
    return(c(estimate = unname(obs), lower = NA_real_, upper = NA_real_))
  }
  boots <- replicate(R, {
    idx <- sample.int(n, replace = TRUE)
    suppressWarnings(stats::cor(x[idx], y[idx], method = method))
  })
  a <- (1 - conf.level) / 2
  ci <- stats::quantile(boots, c(a, 1 - a), names = FALSE, na.rm = TRUE)
  c(estimate = unname(obs), lower = ci[[1]], upper = ci[[2]])
}
