# analysis_syntax.R
#
# The R code behind a result, so a report can show the statistical call that
# produced each number instead of only the wrapper that dispatched it.
#
# The package already generates syntax for the model blocks (cfa_syntax(),
# sem_lavaan_syntax(), seminr_syntax(), efa_syntax()), and the report renders
# whatever a result carries in $syntax. This extends that to the ordinary
# analysis blocks.
#
# The code is built from the SAME resolved specification the runner executed:
# result$vars in the order the runner resolved them, and result$options after
# defaults were applied. A separate hand-written approximation would drift from
# what the package actually computes, which is the one failure mode that would
# make this feature worse than no feature at all.
# tests/testthat/test-analysis-syntax.R holds one execution case per method in
# sframe_syntax_methods, asserted equal to that roster, and each case names the
# number it compares against the package's own result. So a method cannot join
# the generator without an executed comparison, and drift breaks a test. Before
# the pre-publication review this comment claimed that coverage while 7 of the
# 18 methods were never executed.

# A quoted string, or a bare number for a numeric option.
sframe_code_value <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.logical(x)) return(if (isTRUE(x)) "TRUE" else "FALSE")
  if (is.numeric(x)) return(format(x, trim = TRUE))
  if (length(x) > 1) {
    return(paste0("c(", paste(vapply(x, sframe_code_value, character(1)),
                              collapse = ", "), ")"))
  }
  paste0("\"", x, "\"")
}

# A column reference that survives any name, including one with a space.
sframe_code_col <- function(frame, name) {
  sprintf("%s[[\"%s\"]]", frame, name)
}

# The numeric reading the runners use. sframe_num() is internal, and reads a
# factor's labels rather than its codes, so the generated code does the same
# through as.character() and needs no ::: to run.
sframe_code_num <- function(frame, name) {
  sprintf("as.numeric(as.character(%s))", sframe_code_col(frame, name))
}

# The 2 group vectors the independent t test and Mann-Whitney runners build.
# They take the groups in order of first appearance and drop missing values
# within each, which is a different order from the formula interface's sorted
# levels, and a different p from a formula call on a tied sample.
sframe_code_two_groups <- function(group_expr, outcome_expr) {
  c(sprintf("group   <- as.character(%s)", group_expr),
    sprintf("outcome <- %s", outcome_expr),
    "levels_seen <- unique(group[!is.na(group)])",
    "g1 <- outcome[group == levels_seen[1]]; g1 <- g1[!is.na(g1)]",
    "g2 <- outcome[group == levels_seen[2]]; g2 <- g2[!is.na(g2)]")
}

# Every method this generator covers. A method outside it returns NULL, and the
# report says the call sits inside the package rather than showing a guess.
sframe_syntax_methods <- c(
  "t_test_ind", "t_test_pair", "mann_whitney", "wilcoxon_pair",
  "kruskal_wallis", "anova_one", "anova_two", "ancova", "friedman",
  "correlation_pearson", "correlation_spearman", "correlation_kendall",
  "regression_linear", "regression_logistic_binary",
  # A chi-square block dispatches to the crosstab runner, and the result says
  # "crosstab", so that is the name to switch on.
  "crosstab", "fisher_exact", "mcnemar", "descriptives"
)

# The statistical call for one result, as lines of R. `frame` names the data
# frame the lines read from.
sframe_syntax_call <- function(result, frame = "scored") {
  test <- as.character(result$test %||% "")[1]
  # Most runners record the resolved variables as `vars`. The descriptives
  # runner records them as `variables`, so reading `vars` alone generated
  # `vars <- c()` and code that summarised nothing. The roster test compares the
  # generated number against the package's, which is how that surfaced.
  vars <- as.character(result$vars %||% result$variables %||% character(0))
  opts <- result$options %||% list()
  num <- function(i) sframe_code_num(frame, vars[i])
  col <- function(i) sframe_code_col(frame, vars[i])

  switch(test,
    t_test_ind = {
      var_equal <- isTRUE(opts$var_equal)
      c(sframe_code_two_groups(col(1), num(2)),
        sprintf("t.test(g1, g2, var.equal = %s)",
                sframe_code_value(var_equal)))
    },
    t_test_pair = c(
      sprintf("x <- %s", num(1)),
      sprintf("y <- %s", num(2)),
      "complete <- !is.na(x) & !is.na(y)",
      "x <- x[complete]; y <- y[complete]",
      "t.test(x, y, paired = TRUE)"
    ),
    mann_whitney = c(
      sframe_code_two_groups(col(1), num(2)),
      "wilcox.test(g1, g2, exact = FALSE, correct = FALSE, conf.int = TRUE)"
    ),
    wilcoxon_pair = c(
      sprintf("x <- %s", num(1)),
      sprintf("y <- %s", num(2)),
      "complete <- !is.na(x) & !is.na(y)",
      "x <- x[complete]; y <- y[complete]",
      "wilcox.test(x, y, paired = TRUE, exact = FALSE, correct = FALSE,",
      "            conf.int = TRUE)"
    ),
    kruskal_wallis = c(
      sprintf("group   <- as.character(%s)", col(1)),
      sprintf("outcome <- %s", num(2)),
      "complete <- !is.na(outcome) & !is.na(group)",
      "kruskal.test(outcome[complete] ~ as.factor(group[complete]))"
    ),
    anova_one = c(
      sprintf("group   <- factor(as.character(%s))", col(1)),
      sprintf("outcome <- %s", num(2)),
      "complete <- !is.na(outcome) & !is.na(group)",
      "summary(aov(outcome[complete] ~ droplevels(group[complete])))"
    ),
    anova_two = c(
      sprintf("factor_a <- factor(as.character(%s))", col(1)),
      sprintf("factor_b <- factor(as.character(%s))", col(2)),
      sprintf("outcome  <- %s", num(3)),
      "# Type I (sequential) sums of squares, which is what aov() gives.",
      "summary(aov(outcome ~ factor_a * factor_b))"
    ),
    ancova = c(
      sprintf("group    <- factor(as.character(%s))", col(1)),
      sprintf("outcome  <- %s", num(2)),
      sprintf("covariate <- %s", num(3)),
      "fit <- aov(outcome ~ covariate + group)",
      "# drop1() tests each term after the others, which is the adjusted",
      "# group effect an ANCOVA reports.",
      "drop1(fit, test = \"F\")"
    ),
    friedman = {
      measures <- paste(vapply(seq_along(vars), function(i) num(i),
                              character(1)), collapse = ",\n  ")
      c(sprintf("measures <- cbind(\n  %s\n)", measures),
        "measures <- measures[stats::complete.cases(measures), , drop = FALSE]",
        "friedman.test(measures)")
    },
    correlation_pearson = ,
    correlation_spearman = ,
    correlation_kendall = {
      method <- sub("^correlation_", "", test)
      c(sprintf("x <- %s", num(1)),
        sprintf("y <- %s", num(2)),
        "complete <- !is.na(x) & !is.na(y)",
        sprintf("cor.test(x[complete], y[complete], method = %s)",
                sframe_code_value(method)))
    },
    regression_linear = {
      # sframe_vars_for_method() resolves the predictors first and the outcome
      # last, so reading vars[1] as the outcome fits a different model.
      outcome <- vars[length(vars)]
      predictors <- vars[-length(vars)]
      c(sprintf("fit <- lm(`%s` ~ %s, data = %s)", outcome,
                paste(sprintf("`%s`", predictors), collapse = " + "), frame),
        "summary(fit)")
    },
    regression_logistic_binary = {
      outcome <- vars[length(vars)]
      predictors <- vars[-length(vars)]
      c(sprintf("fit <- glm(`%s` ~ %s, data = %s, family = binomial())",
                outcome, paste(sprintf("`%s`", predictors), collapse = " + "),
                frame),
        "summary(fit)",
        "exp(cbind(`odds ratio` = coef(fit), confint.default(fit)))")
    },
    crosstab = {
      # correct = FALSE, as the runner does: Yates' correction on a balanced
      # 2-by-2 can take the statistic to 0 where the uncorrected one is not.
      simulate <- isTRUE(opts$simulate_p_value)
      c(sprintf("tab <- table(%s, %s)", col(1), col(2)),
        if (simulate) {
          "chisq.test(tab, correct = FALSE, simulate.p.value = TRUE)"
        } else {
          "chisq.test(tab, correct = FALSE)"
        })
    },
    fisher_exact = c(
      sprintf("tab <- table(%s, %s)", col(1), col(2)),
      "fisher.test(tab)"
    ),
    mcnemar = c(
      sprintf("tab <- table(%s, %s)", col(1), col(2)),
      "mcnemar.test(tab)"
    ),
    descriptives = {
      cols <- paste(vapply(vars, function(v) sprintf("\"%s\"", v),
                           character(1)), collapse = ", ")
      c(sprintf("vars <- c(%s)", cols),
        sprintf("summary(%s[vars])", frame),
        sprintf("vapply(%s[vars], function(x) stats::sd(as.numeric(as.character(x)), na.rm = TRUE), numeric(1))",
                frame))
    },
    NULL
  )
}

#' The R code behind an analysis result
#'
#' Returns the statistical call that produced a result, as R code a reader can
#' copy and run. This is what lets a report show `t.test()` or `cor.test()` with
#' the variables and options resolved, rather than only the
#' [run_analysis_plan()] call that dispatched it.
#'
#' The code is built from the same resolved specification the runner executed:
#' the variables in the order it resolved them, and the options after defaults
#' were applied. Running it reproduces the statistic, degrees of freedom and p
#' value the package reports, which the package's own tests check by running
#' the generated code and comparing.
#'
#' # What it covers
#'
#' The 2-group, paired, k-group, correlation, regression and categorical
#' families, and descriptives. `sframe_syntax_methods` lists them. A method
#' outside that list returns `NULL`: the model families carry their own syntax
#' already, through [cfa_syntax()] and its neighbours, and for the rest the
#' computation has no single base-R equivalent to show honestly.
#'
#' @param x An `sframe_analysis_results` object from [run_analysis_plan()], or
#'   one block's result from it.
#' @param which Character or NULL. One block id, when `x` holds several.
#' @param data_expr Character. The expression the code should read its data
#'   from. Defaults to `"scored"`, the scored frame the header sets up.
#' @param header Logical. Whether to include the lines that load the
#'   instrument, read the responses and score the scales. `TRUE` gives a script
#'   that runs on its own.
#'
#' @return A character vector of R code lines, or `NULL` for a method this does
#'   not cover. For several blocks, a named list of such vectors.
#' @export
#' @seealso [run_analysis_plan()], [cfa_syntax()], [render_report()]
#'
#' @examples
#' instr <- sf_instrument("Syntax demo", components = list(
#'   sf_item("score", "Score", type = "numeric"),
#'   sf_item("arm", "Arm", type = "text")
#' ))
#' sf_plan(instr) <- list(list(
#'   id = "RQ1", research_question = "Do the arms differ?",
#'   family = "inferential", method = "t_test_ind",
#'   roles = list(group = "arm", outcome = "score")
#' ))
#'
#' set.seed(1)
#' responses <- data.frame(
#'   arm   = rep(c("control", "treatment"), each = 15),
#'   score = c(rnorm(15, 10), rnorm(15, 12))
#' )
#' results <- run_analysis_plan(responses, instr)
#'
#' # the call behind the number, with the variables and options resolved
#' cat(analysis_syntax(results, which = "RQ1"), sep = "\n")
analysis_syntax <- function(x, which = NULL, data_expr = "scored",
                            header = FALSE) {
  if (inherits(x, "sframe_analysis_results")) {
    if (!is.null(which)) {
      if (!which %in% names(x)) {
        rlang::abort(
          paste0("No analysis block called '", which, "'. This result holds: ",
                 paste(names(x), collapse = ", "), "."),
          class = "sframe_error")
      }
      return(analysis_syntax(x[[which]], data_expr = data_expr,
                             header = header))
    }
    out <- lapply(x, function(r) {
      analysis_syntax(r, data_expr = data_expr, header = header)
    })
    names(out) <- names(x)
    return(out)
  }

  if (!is.list(x) || is.null(x$test)) {
    rlang::abort(
      "`x` must be an analysis result from run_analysis_plan().",
      class = "sframe_error")
  }

  body <- sframe_syntax_call(x, frame = data_expr)
  if (is.null(body)) return(NULL)

  question <- as.character(x$research_question %||% "")[1]
  lines <- c(
    if (nzchar(question)) paste0("# ", question),
    sprintf("# %s", sframe_syntax_note(x)),
    body
  )
  if (isTRUE(header)) lines <- c(sframe_syntax_header(data_expr), "", lines)
  lines
}

# What a reader needs to know about the run beside the call itself.
sframe_syntax_note <- function(result) {
  parts <- sprintf("method: %s", result$test %||% "unknown")
  # Not every result carries a single n: a 2-group test carries n1 and n2.
  n_text <- if (!is.null(result$n)) {
    format(result$n, trim = TRUE)
  } else if (!is.null(result$n1) && !is.null(result$n2)) {
    sprintf("%s + %s", format(result$n1, trim = TRUE),
            format(result$n2, trim = TRUE))
  } else NULL
  if (!is.null(n_text)) parts <- c(parts, sprintf("n = %s", n_text))
  opts <- result$options %||% list()
  keep <- names(opts)[!vapply(opts, is.null, logical(1))]
  if (length(keep)) {
    parts <- c(parts, paste(vapply(keep, function(k) {
      sprintf("%s = %s", k, sframe_code_value(opts[[k]]))
    }, character(1)), collapse = ", "))
  }
  paste(parts, collapse = " | ")
}

# The lines that make the generated call runnable on its own: the instrument,
# the responses, and the scored frame every block reads from. run_analysis_plan()
# scores with keep_items and keep_meta, so this matches what the blocks saw.
sframe_syntax_header <- function(data_expr = "scored") {
  c(
    "library(surveyframe)",
    "",
    "instrument <- read_sframe(\"instrument.sframe\")",
    "responses  <- read_responses(\"responses.csv\", instrument)",
    sprintf("%s <- score_scales(responses, instrument, keep_items = TRUE, keep_meta = TRUE)",
            data_expr)
  )
}
