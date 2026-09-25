# analysis_syntax() shows the statistical call behind a result, so a report can
# display t.test() or cor.test() with the variables and options resolved instead
# of only the run_analysis_plan() call that dispatched it.
#
# The acceptance criterion is the one that matters: copy the code, run it, and
# get the same numbers. A generator maintained as a separate hand-written
# approximation would drift from what the package computes, and a report showing
# code that produces different numbers is worse than one showing no code. So
# every covered method is checked by running the generated code against the
# package's own result.

syntax_data <- function(n = 90) {
  set.seed(4)
  data.frame(
    grp   = rep(c("a", "b"), length.out = n),
    grp3  = rep(c("a", "b", "c"), length.out = n),
    grp_b = rep(c("x", "y"), each = n / 2),
    score = stats::rnorm(n, 10, 3),
    after = stats::rnorm(n, 11, 3),
    cov   = stats::rnorm(n, 5, 1),
    yes   = rep(c(0, 1), length.out = n),
    stringsAsFactors = FALSE
  )
}

# Runs the generated lines and returns the value of the last expression.
run_syntax <- function(code, data) {
  env <- new.env(parent = globalenv())
  assign("scored", data, envir = env)
  out <- NULL
  for (line in split_statements(code)) {
    out <- eval(parse(text = line), envir = env)
  }
  out
}

# The generated code is one statement per line, with a couple of calls wrapped
# across lines, so statements are rebuilt by parsing the whole block.
split_statements <- function(code) {
  text <- paste(code[!grepl("^\\s*#", code)], collapse = "\n")
  exprs <- parse(text = text)
  vapply(exprs, function(e) paste(deparse(e), collapse = "\n"), character(1))
}

result_for <- function(method, roles, data = syntax_data(), options = list()) {
  instr <- sf_instrument("Syntax", components = list(
    sf_item("score", "Score", type = "numeric"),
    sf_item("after", "After", type = "numeric"),
    sf_item("cov", "Covariate", type = "numeric"),
    sf_item("yes", "Outcome", type = "numeric"),
    sf_item("grp", "Group", type = "text"),
    sf_item("grp3", "Group of 3", type = "text"),
    sf_item("grp_b", "Second factor", type = "text")
  ))
  block <- list(id = "RQ1", research_question = "Does it agree?",
                family = "inferential", method = method, roles = roles)
  if (length(options)) block$options <- options
  sf_plan(instr) <- list(block)
  run_analysis_plan(data, instr)$RQ1
}

test_that("the generated code runs, and reproduces an independent t test", {
  res <- result_for("t_test_ind",
                    list(group = "grp", outcome = "score"))
  code <- analysis_syntax(res)
  expect_false(is.null(code))

  got <- run_syntax(code, syntax_data())
  expect_s3_class(got, "htest")
  expect_lt(abs(unname(got$statistic) - res$t), 1e-8)
  expect_lt(abs(got$p.value - res$p), 1e-8)
})

test_that("a Welch and a pooled t test each show their own option", {
  welch <- result_for("t_test_ind", list(group = "grp", outcome = "score"))
  pooled <- result_for("t_test_ind", list(group = "grp", outcome = "score"),
                       options = list(var_equal = TRUE))

  expect_match(paste(analysis_syntax(welch), collapse = " "),
               "var.equal = FALSE", fixed = TRUE)
  expect_match(paste(analysis_syntax(pooled), collapse = " "),
               "var.equal = TRUE", fixed = TRUE)

  # and each reproduces its own result, which is the point of resolving the
  # option rather than printing a default
  expect_lt(abs(unname(run_syntax(analysis_syntax(pooled),
                                  syntax_data())$statistic) - pooled$t), 1e-8)
})

test_that("a correlation reproduces its estimate and p", {
  for (method in c("pearson", "spearman", "kendall")) {
    res <- result_for(paste0("correlation_", method),
                      list(x = "score", y = "after"))
    got <- run_syntax(analysis_syntax(res), syntax_data())
    expect_lt(abs(unname(got$estimate) - res$r), 1e-8)
    expect_lt(abs(got$p.value - res$p), 1e-6)
  }
})

test_that("the rank and k-group families reproduce their own numbers", {
  cases <- list(
    list(m = "mann_whitney", roles = list(group = "grp", outcome = "score")),
    list(m = "kruskal_wallis", roles = list(group = "grp3", outcome = "score")),
    list(m = "wilcoxon_pair", roles = list(before = "score", after = "after"))
  )
  for (case in cases) {
    res <- result_for(case$m, case$roles)
    got <- run_syntax(analysis_syntax(res), syntax_data())
    expect_lt(abs(got$p.value - res$p), 1e-6,
              label = paste(case$m, "p"))
  }
})

test_that("a paired t test reproduces its statistic", {
  res <- result_for("t_test_pair", list(before = "score", after = "after"))
  got <- run_syntax(analysis_syntax(res), syntax_data())
  expect_lt(abs(unname(got$statistic) - res$t), 1e-8)
})

test_that("a one-way ANOVA reproduces its F", {
  res <- result_for("anova_one", list(group = "grp3", outcome = "score"))
  got <- run_syntax(analysis_syntax(res), syntax_data())
  f <- got[[1]][["F value"]][1]
  expect_lt(abs(f - res$F_stat), 1e-6)
})

test_that("a linear regression reproduces its coefficients", {
  res <- result_for("regression_linear",
                    list(outcome = "score", predictors = c("cov", "after")))
  got <- run_syntax(analysis_syntax(res), syntax_data())
  expect_s3_class(got, "summary.lm")
  expect_equal(nrow(got$coefficients), nrow(res$coefficients))
  expect_lt(max(abs(unname(got$coefficients[, 1]) -
                    unname(res$coefficients[[1]]))), 1e-8)
})

test_that("a chi-square reproduces its statistic", {
  # the roles are row and column, which is what sframe_vars_for_method() reads
  res <- result_for("chi_square", list(row = "grp", column = "grp_b"))
  expect_equal(res$test, "crosstab")
  got <- run_syntax(analysis_syntax(res), syntax_data())
  expect_lt(abs(unname(got$statistic) - res$chi_sq), 1e-8)
})

test_that("the header makes the code a script that stands alone", {
  res <- result_for("t_test_ind", list(group = "grp", outcome = "score"))
  code <- analysis_syntax(res, header = TRUE)
  expect_match(code[[1]], "library(surveyframe)", fixed = TRUE)
  expect_true(any(grepl("read_sframe", code, fixed = TRUE)))
  expect_true(any(grepl("score_scales", code, fixed = TRUE)))
  # and it still parses as a whole
  expect_no_error(parse(text = paste(code, collapse = "\n")))
})

test_that("the note records the method, the n and the resolved options", {
  res <- result_for("t_test_ind", list(group = "grp", outcome = "score"))
  code <- analysis_syntax(res)
  note <- grep("method:", code, value = TRUE)
  expect_length(note, 1)
  expect_match(note, "t_test_ind", fixed = TRUE)
  expect_match(note, "n = ", fixed = TRUE)
  # the research question heads the block, so a reader knows what it answers
  expect_match(code[[1]], "Does it agree?", fixed = TRUE)
})

test_that("a method outside the covered set says nothing rather than guessing", {
  res <- list(test = "cochran_q", vars = c("a", "b"), n = 10)
  expect_null(analysis_syntax(res))
  expect_false("cochran_q" %in% sframe_syntax_methods)
})

test_that("a whole result set gives one entry per block, named", {
  skip_on_cran()  # renders a report or runs a full plan: slow on CRAN's machines
  demo <- sframe_demo_data()
  results <- run_analysis_plan(demo$responses, demo$instrument)
  code <- analysis_syntax(results)
  expect_equal(names(code), names(results))
  # at least one block is covered, and every covered one parses
  covered <- code[!vapply(code, is.null, logical(1))]
  expect_gt(length(covered), 0)
  for (one in covered) {
    expect_no_error(parse(text = paste(one, collapse = "\n")))
  }
})

test_that("one block can be asked for by id, and an unknown id is refused", {
  skip_on_cran()  # renders a report or runs a full plan: slow on CRAN's machines
  demo <- sframe_demo_data()
  results <- run_analysis_plan(demo$responses, demo$instrument)
  first <- names(results)[[1]]
  expect_equal(analysis_syntax(results, which = first),
               analysis_syntax(results)[[first]])
  expect_error(analysis_syntax(results, which = "nope"),
               class = "sframe_error")
})

test_that("a column name with a space survives", {
  d <- syntax_data()
  names(d)[names(d) == "score"] <- "total score"
  instr <- sf_instrument("Spaces", components = list(
    sf_item("total score", "Total", type = "numeric"),
    sf_item("grp", "Group", type = "text")))
  sf_plan(instr) <- list(list(
    id = "RQ1", research_question = "Spaces?", family = "inferential",
    method = "t_test_ind", roles = list(group = "grp", outcome = "total score")))
  res <- run_analysis_plan(d, instr)$RQ1
  code <- analysis_syntax(res)
  expect_match(paste(code, collapse = " "), '[["total score"]]', fixed = TRUE)
  got <- run_syntax(code, d)
  expect_lt(abs(unname(got$statistic) - res$t), 1e-8)
})

# ── The coverage roster ──────────────────────────────────────────────────────
# Reopened by the pre-publication review. The header above claimed every covered
# method is run and compared. Eleven were. anova_two, ancova, friedman,
# regression_logistic_binary, fisher_exact, mcnemar and descriptives were
# declared covered by the generator and never executed, and the only set-level
# gate asked that at least one returned block be covered and that the text
# parse. It stayed green if a method lost its generator, or generated parseable
# code computing something else.
#
# The roster below is asserted equal to sframe_syntax_methods, so a method added
# to the generator without an execution case here fails this file.

roster_data <- function(n = 90) {
  set.seed(11)
  base <- syntax_data(n)
  base$cat2  <- rep(c("yes", "no"), length.out = n)
  base$cat2b <- rep(c("hi", "lo"), each = n / 2)
  # a paired binary pair with off-diagonal disagreement, which McNemar needs
  base$b1 <- rep(c(1, 0, 1, 0), length.out = n)
  base$b2 <- rep(c(1, 1, 0, 0), length.out = n)
  base$m1 <- stats::rnorm(n, 10, 2)
  base$m2 <- stats::rnorm(n, 11, 2)
  base$m3 <- stats::rnorm(n, 12, 2)
  base
}

roster_instrument <- function() {
  num <- c("score", "after", "cov", "yes", "b1", "b2", "m1", "m2", "m3")
  txt <- c("grp", "grp3", "grp_b", "cat2", "cat2b")
  sf_instrument("Roster", components = c(
    lapply(num, function(x) sf_item(x, x, type = "numeric")),
    lapply(txt, function(x) sf_item(x, x, type = "text"))
  ))
}

roster_result <- function(method, roles, options = list()) {
  instr <- roster_instrument()
  block <- list(id = "RQ1", research_question = "Agrees?",
                family = "inferential", method = method, roles = roles)
  if (length(options)) block$options <- options
  sf_plan(instr) <- list(block)
  run_analysis_plan(roster_data(), instr)$RQ1
}


# Every case names the number it compares, so a method cannot join the roster
# without one. The first version read a statistic only when the generated call
# returned an htest and the package happened to name the field, which silently
# skipped 11 of the 18: an aov, a summary.lm, a coefficient matrix and a bare
# numeric all fell through, and so did every correlation, because the package
# reports r where the field list looked for "statistic".
htest_stat <- function(got) unname(got$statistic[[1]])
htest_p    <- function(got) unname(got$p.value[[1]])
# The generated ANOVA code ends on summary(aov(...)), so the object already IS
# the summary. Calling summary() again indexes past its end.
aov_f <- function(got) {
  tab <- if (inherits(got, "summary.aov")) got[[1]] else summary(got)[[1]]
  unname(tab[["F value"]][[1]])
}

syntax_cases <- list(
  list(m = "t_test_ind", roles = list(group = "grp", outcome = "score"),
       want = function(r) c(r$t, r$p), have = function(g) c(htest_stat(g), htest_p(g))),
  list(m = "t_test_pair", roles = list(before = "score", after = "after"),
       want = function(r) c(r$t, r$p), have = function(g) c(htest_stat(g), htest_p(g))),
  list(m = "mann_whitney", roles = list(group = "grp", outcome = "score"),
       want = function(r) c(r$U, r$p), have = function(g) c(htest_stat(g), htest_p(g))),
  list(m = "wilcoxon_pair", roles = list(before = "score", after = "after"),
       want = function(r) c(r$V, r$p), have = function(g) c(htest_stat(g), htest_p(g))),
  list(m = "kruskal_wallis", roles = list(group = "grp3", outcome = "score"),
       want = function(r) c(r$H, r$p), have = function(g) c(htest_stat(g), htest_p(g))),
  # an aov, so the F comes out of summary() rather than off an htest
  list(m = "anova_one", roles = list(group = "grp3", outcome = "score"),
       want = function(r) r$F_stat, have = aov_f),
  list(m = "anova_two", roles = list(factor1 = "grp", factor2 = "grp_b",
                                     outcome = "score"),
       want = function(r) r$table[["F"]][[1]], have = aov_f),
  # ancova generates an anova data frame, and the package tables the same rows
  # drop1() lists <none> first and its F is NA, so the group row is taken by
  # name. The package tables the same adjusted effect.
  list(m = "ancova", roles = list(group = "grp3", covariates = "cov",
                                  outcome = "score"),
       want = function(r) r$table[["F"]][r$table$effect == "grp3"][[1]],
       have = function(g) unname(g[["F value"]][rownames(g) == "group"][[1]])),
  list(m = "friedman", roles = list(measures = c("m1", "m2", "m3")),
       want = function(r) c(r$chisq %||% r$statistic, r$p),
       have = function(g) c(htest_stat(g), htest_p(g))),
  # correlations report r, which is the estimate rather than the statistic
  list(m = "correlation_pearson", roles = list(x = "score", y = "after"),
       want = function(r) c(r$r, r$p),
       have = function(g) c(unname(g$estimate[[1]]), htest_p(g))),
  list(m = "correlation_spearman", roles = list(x = "score", y = "after"),
       want = function(r) c(r$r, r$p),
       have = function(g) c(unname(g$estimate[[1]]), htest_p(g))),
  list(m = "correlation_kendall", roles = list(x = "score", y = "after"),
       want = function(r) c(r$r, r$p),
       have = function(g) c(unname(g$estimate[[1]]), htest_p(g))),
  # a summary.lm: the slope on the first predictor, and the model R squared
  list(m = "regression_linear", roles = list(predictors = c("cov", "after"),
                                             dependent = "score"),
       want = function(r) c(r$coefficients[["Estimate"]][[2]], r$r2),
       have = function(g) c(unname(g$coefficients[2, 1]), unname(g$r.squared))),
  # The generated block ends on exp(cbind(...)), so what comes back is the odds
  # ratio and its interval, not the log-odds estimate. Comparing the package's
  # Estimate against it read -0.08 against 0.92, the same coefficient on two
  # scales.
  list(m = "regression_logistic_binary",
       roles = list(predictors = "score", dependent = "yes"),
       want = function(r) r$coefficients[["odds_ratio"]][[2]],
       have = function(g) unname(g[2, 1])),
  list(m = "crosstab", roles = list(row = "cat2", column = "cat2b"),
       want = function(r) c(r$chi_sq, r$p),
       have = function(g) c(htest_stat(g), htest_p(g))),
  # Fisher has no test statistic, so the p and the odds ratio are the claim
  list(m = "fisher_exact", roles = list(row = "cat2", column = "cat2b"),
       want = function(r) c(r$p, r$odds_ratio),
       have = function(g) c(htest_p(g), unname(g$estimate[[1]]))),
  list(m = "mcnemar", roles = list(before = "b1", after = "b2"),
       want = function(r) c(r$chi_sq %||% r$statistic, r$p),
       have = function(g) c(htest_stat(g), htest_p(g))),
  # The generated block ends on a vapply() of sd over the named variables, so
  # the sd is the number to compare, one per variable and in that order.
  list(m = "descriptives", roles = list(variables = c("score", "after")),
       want = function(r) as.numeric(r$table[["sd"]]),
       have = function(g) unname(g))
)

test_that("every method the generator declares has an execution case", {
  expect_setequal(vapply(syntax_cases, function(x) x$m, character(1)),
                  sframe_syntax_methods)
})

test_that("every case names the numbers it compares", {
  # Without this a case with no comparator would pass the roster check and
  # assert nothing, which is how the first 11 got through.
  for (case in syntax_cases) {
    expect_true(is.function(case$want), info = case$m)
    expect_true(is.function(case$have), info = case$m)
  }
})

test_that("each covered method's generated code reproduces its own numbers", {
  for (case in syntax_cases) {
    res <- roster_result(case$m, case$roles)
    code <- analysis_syntax(res)
    expect_false(is.null(code), info = case$m)

    got <- run_syntax(code, roster_data())
    want <- as.numeric(case$want(res))
    have <- as.numeric(case$have(got))

    expect_false(any(is.na(want)), info = paste(case$m, "package number missing"))
    expect_false(any(is.na(have)), info = paste(case$m, "generated number missing"))
    expect_equal(have, want, tolerance = 1e-6, info = case$m)
  }
})
