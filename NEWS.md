# surveyframe 0.4.2

A defect-fix release. An external review of 0.4.1 found defects that
silently alter or lose data, and this release corrects them. Collection
defects come first, because an answer recorded wrongly is lost for good.
This section grows as each group of fixes lands.

## What you need to change

* **A Shiny-collected response now carries a `respondent_id`**, as the first
  column, holding a generated identifier such as `RK3P8QX2A`. The static
  survey has written one since the first release, so the two collection
  routes produced different column sets for the same instrument, leaving the
  duplicate check and a retry to work from whatever they could find.
  **If you are collecting to a CSV written by 0.4.1 or earlier**, that file
  predates the column: `render_survey()` keeps appending to it and says once
  that the response went in unidentified. Collect into a new file to record an
  identifier for every response. Anything reading these files by column
  position needs updating to read by name.
* **`survey_module_server()` returns a different response shape.** The
  response is now the collection row, as a list: `response_id`,
  `started_at`, `submitted_at`, then one element per response column, named
  as `read_responses()` expects. A multiple-choice question becomes one
  element per option holding `"1"` or `"0"`, and a matrix, ranking or
  decision question becomes one element per row, option, pair or criterion.
  Values are character. An item hidden by branching, or left unanswered, is
  `NA`. The module previously returned one raw input value per item, and
  lacked controls for matrix, ranking, rating and decision questions.
* **Untouched sliders, rankings and dates are now unanswered** in
  `render_survey()` and the survey module. A slider counts once the
  respondent moves it, and a ranking once it is reordered or confirmed with
  "Keep this order". Previously a slider stored its starting position, a
  ranking stored the declared order, and a date question in the module
  stored today's date, for anyone who moved past them.
* **`render_survey(save_responses = "csv")` refuses a response file written
  for different questions**, when the app starts and on each submission.
  Collect a changed instrument into a new file. A file with the same columns
  in another order is still accepted, and rows are aligned by column name.
* **Google Sheets collection now needs the Sheets advanced service.** The
  generated collector stores an answer through the Sheets API's `RAW` option,
  which is the documented way to store a value without the spreadsheet parsing
  it. Regenerate and redeploy the collector with `export_google_sheet()`, and
  in the Apps Script editor add Services > Google Sheets API with the
  identifier `Sheets`. **A collector lacking it refuses each response** and
  replies with an error, in place of storing an answer it would have to alter.
  Code that reads the sheet directly, bypassing `read_sheet_responses()`,
  should expect text cells where numbers appeared before.

* **Scale scores and reliability can change.** Re-run analyses of scales
  that reverse-code items, share items with other scales, or had an absent
  item column. Reverse coding now applies within the scale that declares it,
  a scale with an absent item column counts that item as unanswered, and
  report figures now show the scores `score_scales()` computes.
* **Some instruments that validated before are now rejected.** An item and a
  scale sharing an ID, an ID equal to a response column or to `respondent_id`,
  `response_id`, `started_at` or `submitted_at`, reverse coding for an item
  outside the scale declaring it, repeated scale items, a `min_valid` outside
  1 to the number of items, and weights that are zero, negative or infinite
  are all reported by `validate_sframe()`, and `sf_scale()` refuses the scale
  parameters directly. Rename or correct the declaration.
* **A reverse-coded item needs declared response bounds**: a numeric choice
  set, `slider_min` and `slider_max`, or a rating maximum. Scoring reports an
  error for a reversed item that has none.
* **A factor column is scored through its labels.** A factor whose labels are
  text is an error. Convert it to numeric codes first.

* **Some statistics change.** Re-run analyses using Cochran's Q with missing
  answers, ANCOVA, partial correlation, a Firth likelihood ratio, PLS-SEM
  constructs with non-consecutive indicators, or a Mann-Whitney or Wilcoxon
  signed-rank test. The rank tests now use the normal approximation without
  continuity correction for z, p, r and its interval alike, so their p
  values move slightly, and z is now signed by the direction of the
  difference.
* **Repeated-measures ANOVA now uses complete respondents.** A respondent
  missing any repeated measure is excluded as a unit before fitting the
  classical balanced model. Results report the retained `n` and
  `n_excluded_incomplete`. This replaces a singular fit on incomplete rows.
* **ANCOVA tables hold adjusted tests**, each term tested after all others,
  with columns `effect`, `df`, `sum_sq`, `mean_sq`, `F` and `p`.
* **`sample_size_plan()` now calculates power** for t tests, ANOVA and, with
  the new `f2`, regression. The new `d` and `f` arguments give the expected
  effect, and a medium effect is assumed, with a warning, when they are
  left out. Estimates now depend on `alpha` and `power`.
* **`run_analysis_plan()` refuses repeated block IDs** and gains `strict`.
  Its results carry a `status` attribute counting failed blocks, so check
  it, or set `strict = TRUE`, before treating a returned object as success.
* **Bootstrap intervals can be withheld.** `bootstrap_ci()`, `cohens_d_ci()`,
  `cramers_v_ci()` and `eta_sq_ci()` return `NA` bounds with a `reason`
  attribute when fewer than 90% of resamples give a value or all give the
  same value, and every result records its resample counts.

* **Decision rankings can change.** Re-run TOPSIS, VIKOR, MOORA, SMART,
  WASPAS, PROMETHEE and ELECTRE analyses whose weights or criterion types
  were named in a different order from the performance matrix, AHP and ANP
  analyses given a data frame, and aggregations of judgement matrices named
  in different orders. Weights, criterion types and judgement matrices are
  now matched by criterion name.
* **Some decision inputs that ran before are now refused**: weights naming
  different criteria from the matrix, supplied AHP matrices that are not
  positive, unit-diagonal and reciprocal, WASPAS values of zero or below,
  infinite values, a VIKOR `v` or WASPAS `lambda` outside 0 to 1, ELECTRE
  cutoffs outside 0 to 1, and PROMETHEE thresholds that are negative or out
  of order. ANP refuses a reducible network, and DEMATEL a matrix whose total
  relation does not exist.
* **ELECTRE's kernel follows Roy's definition**, so it can hold more
  alternatives than before, and is reported as undefined when the
  outranking relation has a cycle.
* **`sensitivity_analysis()` results gain `n_perturbations`, `n_effective`
  and `n_failed`**, and `stable` is `FALSE` when no perturbation moved the
  weights. A requested sensitivity run that could not be made is recorded
  in the result's `sensitivity_error`.

* **`write_sframe()` refuses an undisclosed revision.** An instrument read
  with `read_sframe()` and then changed must record the change with
  `amend_sframe()` before it is written, and its amendment log must stay
  complete and in order. To publish changed content as a separate instrument,
  clear its amendment log and pass `new_instrument = TRUE`. SurveyStudio
  offers the same choice when exporting an edited file.
* **`amend_sframe()` sets the tier from what changed.** An amendment that
  changes the analysis plan, a model or a conjoint design is design tier and
  needs a `deviation_report`, whatever `reason_code` or `tier` says.
* **`.sframe` files write one-member collections as arrays**, as the
  published instrument profile and the builder already did. Reading an older
  file and writing it again gives it a new hash where it held such a
  collection. Older files still read and verify as they are.
* **`read_responses()` reads a CSV file as text** and converts only columns
  of items with numeric responses. Identifiers such as `001` and metadata
  columns now arrive as text, and numeric answers as doubles. A response file
  with 2 columns of the same name is refused.
* **`link_git_commit()` gains `path` and returns `verified`**, which is `TRUE`
  only when the instrument matches that file as committed. `linked` alone
  confirms a repository and a commit.
* **Some shipped demo results changed** to match the corrected statistics:
  `likert_scale`, `two_group`, `paired`, `multi_group`, `sem_pls`,
  `mcdm_choice` and `small_sample`.

## Collection fixes

* **The Shiny survey erased every answer.** In `render_survey()`, each answer
  re-drew the survey page, and the re-drawn questions reported themselves
  empty, so every answer was wiped about a second after it was given. A
  submitted response came back blank. This affected every standard-mode
  survey run with `render_survey()` in 0.4.1 and earlier. Answers now stay
  put, and the page stays as it is when a question is
  answered.
* **The Google Sheets collector could turn an answer into a formula.** An
  answer beginning with `=` was evaluated by the spreadsheet, so `=1+1` was
  stored as `2`, and a code such as `007` lost its leading zeros. A stored
  answer is now written through the Sheets API's `RAW` option, which stores it
  as submitted. A deployment lacking that option refuses the response and says
  so, in place of altering it. The same fix applies to collectors generated
  from the survey builder. See the setup note above.
* **The exported survey said a response had been recorded when it could not
  know.** The page posts with `no-cors`, which leaves the collector's reply
  unreadable, so the browser can establish that a request left and nothing
  further. The thank-you screen claimed the response had been recorded, hid the
  download, offered a restart that discarded the page's only copy, and could
  auto-redirect away from it. It now reports that the answers were sent and
  that receipt is unconfirmed, keeps the download available, withholds the
  restart that would clear the response, and leaves any redirect for the
  participant to choose.
* **A custom thank-you message no longer shows on a survey with a collector.**
  A message set through `render$thankyou$message` is fixed text, written before
  anyone knew how delivery would go, so a wording such as "your response has
  been recorded" put a claim on screen beside the page's own statement that
  receipt was unconfirmed. The package's own status is shown instead. A custom message still
  shows on a download-only survey, where nothing is being claimed about a
  collector. Put debrief text, contact details or payment instructions on a
  final display item or in the redirect target, where they always reach the
  participant.
* **The exported survey changed typed numbers.** Clearing a number field with
  a minimum wrote the minimum in, a number outside the range was replaced
  with the nearest limit, and in a points allocation `2.5` became `25`. What
  the respondent types is now kept, and the survey asks for a correction.
* **An option coded 0 was recorded as a blank** in the exported survey, so
  a respondent choosing it on a required question was blocked from
  continuing, and 0 looked identical to a blank. Affected 0/1 codings and 0 to 10
  scales.
* **A ranking could show one order and submit another** in the exported
  survey after dragging, and an untouched ranking was submitted as if the
  respondent had chosen the order shown.
* **Shiny matrix and ranking questions showed codes in place of labels.** A
  five-point agreement scale appeared as `1 2 3 4 5`. Shiny rankings also
  stored labels, so every rank came out empty wherever labels and codes
  differed.
* **Shiny rankings needed a mouse.** Each option now
  has move up and move down buttons, and the new position is announced.
* **Shiny decision questions started with an answer selected**, "Equally
  important" or "No influence", and points allocations started at 0, so an
  untouched question submitted an invented judgement. They
  now start empty. Every Shiny date question was also pre-filled with
  today's date, and now starts empty.
* **Appending to a Shiny response file ignored its header**, so a changed
  instrument wrote answers under another question's heading. An existing
  empty file also received headerless rows.
* **The survey module lacked 5 item types.** Matrix, rating,
  ranking, pairwise comparison and criteria weight showed only a
  placeholder, and a required one made the survey impossible to finish. The
  module now uses the same questions as `render_survey()`.
* **The survey module submitted answers the respondent had removed.** A
  cleared answer, and an answer to a question branching later hid, were both
  submitted. A multiple-choice answer lost all but its first selection when
  the page was revisited.
* **A failed save in the survey module showed the thank-you screen.** The
  survey was marked complete before `on_submit` ran, and an error there
  ended the session. The respondent now sees a message, stays on the page
  and can try again.
* Changing a reactive instrument now resets the survey module, as its help
  said it did, and starts every answer blank. Moving between pages
  scrolls the module into view, where it scrolled the whole host page.
* **The static survey said a response had been recorded before it knew.** A
  failed send was discarded and the thank-you screen appeared anyway, and a
  configured redirect then carried the participant away with the answers in
  nobody's hands. A failure now says so, keeps the CSV download reachable,
  offers a retry, and withholds the redirect. The submission is a `no-cors`
  POST, so the collector's reply is unreadable and acceptance cannot be
  confirmed from the page. The screen claims only that the request was sent.
* **A completed comparison or points allocation counted as unanswered** in
  the progress display, because progress read the question's own answer
  where these types store one answer per pair or per criterion. Progress and
  the required-question check now share one rule.
* **A rating left the question that depends on it hidden** until another
  control was touched, while the required check still demanded an answer to
  it.
* **A comparison answered on a phone could show the wrong selection on a
  wider screen**, and the reverse. Each question renders a button strip and
  a dropdown, shown by screen width, and each recorded the answer while
  leaving the other as it was. The stored judgement was always the one given.
* **Long rating scales now stack on a phone.** An 11-point scale needed 550
  pixels, so a participant on a 390-pixel screen saw part of it with both
  ends off screen. Below 600 pixels each point becomes a full-width row.
* **Choice groups, rating scales and validation errors now read correctly to
  a screen reader.** A group carries its question as its name, each rating
  star reports whether it is the one chosen, and an error is announced with
  the control that has it. Validation and a page change move focus to the
  task instead of only scrolling.

* **A multi-select answer failed a branching rule that allowed any of its
  options.** Selecting two options stored them together, and the static
  survey compared the pair as one value, so a rule showing a follow-up for
  either option stayed closed. Any selected option the rule allows now
  satisfies it, which is what the Shiny survey already did.
* **A second branching rule on the same question replaced the first** in the
  Shiny survey, so a question gated on two conditions ran on one. Every rule
  is kept and they combine, and a question controlled by one that is itself
  hidden now counts as unanswered, so a stale answer behind a closed branch
  keeps the questions below it closed too. The static survey already worked this way.
* **One-question-at-a-time mode ignored branching**, so a participant could
  be required to answer a question the rules exclude, which was then blanked
  on submission. Navigation follows the same visible sequence the rest of
  the survey uses.
* **A failed callback after a saved response invited a duplicate.** The save
  and the `on_submit` callback shared one error handler, so a callback
  failure reported that nothing was saved and submitting again wrote the
  answers a second time. The two steps are tracked separately, a retry
  repeats only what failed, and the two failures now read differently.

## Researcher interface fixes

* **SurveyStudio's preview could send test answers to the live collector.**
  It exported the real instrument and the export fell back to the
  instrument's configured Google Sheets endpoint, so a test response could
  land in a running study's sheet beside real participants'. The preview now
  exports with `preview = TRUE`, a new `export_static_survey()` argument that
  removes the collector and the completion redirect. **If you previewed a
  configured instrument in Studio on 0.4.1 or earlier, check the collecting
  sheet for test rows.**
* **The SurveyBuilder question list can be worked from the keyboard.**
  A question row takes focus, opens on Enter or Space, and moves with Alt and
  an arrow key, and named move-up and move-down buttons sit beside duplicate
  and delete. Ordering was previously drag-only.
* **An autosaved builder session is offered however old it is.** Recovery
  refused anything older than two hours while the session sat in storage, so
  returning the next day showed an empty builder. The banner reports the age
  and clears the session only when dismissed. Where the browser refuses to
  store anything, the builder now says so, where it used to appear to save.
* Opening a builder dialog moves focus into it, and closing one returns focus
  where it was.
* **The builder's Preview is labelled a layout preview**, which is what it
  is: it shows wording, order and branding and leaves out answering, required
  checks and branching. Export the survey, or use Studio's preview, to test
  the respondent's path.
* **The RStudio add-ins are renamed after what they do**, all under a
  `surveyframe:` prefix: Design an instrument, Open analysis workspace,
  Analyse an existing instrument, and Insert starter instrument.
  SurveyBuilder was described as a Shiny app, and it is a client-side HTML
  page.
* **"Analyse an existing instrument", formerly "Open Dashboard", opens
  SurveyStudio on Upload Responses** with the chosen instrument loaded, ready
  for a response file. It used to open an empty dashboard.
* **Every add-in stops with one message saying what to do** when RStudio is
  closed, the console has focus during an insert, the file dialog fails, or
  the chosen `.sframe` fails to load. It used to handle a missing rstudioapi
  alone. The exported launchers keep raising their errors.
* The starter instrument keeps its validation result as `validation`, and
  ends with a commented `write_sframe()` line for saving it.
* **`library(surveyframe)` now says where to start.** In an interactive
  session it prints a short note: 3 commands that load a branded demo and
  show it as a respondent sees it and inside SurveyStudio, the functions to
  start your own study with, `citation("surveyframe")`, and the website.
  Scripts, Quarto renders and `R CMD check` stay silent, and
  `suppressPackageStartupMessages()` removes it.
* SurveyStudio's preview points at the builder, where it used to name a
  "Build Survey" screen of its own.

## Scoring fixes

* **A scale could overwrite a question's answers.** A scale's score is stored
  in a column named by its ID, and an item and a scale were allowed the same
  ID, so `score_scales()` replaced the item's answers with the score, and an
  analysis of that item read the score. Shared names are now rejected at
  validation, and `score_scales()` refuses to overwrite data.
* **One scale could reverse another scale's item.** A scale listing an item in
  `reverse_items` reversed it everywhere, which changed the scores and the
  alpha of any other scale using that item.
* **Reports published a different scale score from the instrument's.** The
  report figures and the Quarto report template took a plain row mean,
  ignoring the declared method, reverse coding, weights and `min_valid`, so a
  declared sum of 2 and 4 was shown as 3.
* **A missing item column lowered the scoring threshold.** With `min_valid =
  NULL`, meaning every item, a 3-item scale with 1 absent column was scored on
  2 items. The absent item now counts as unanswered, with a warning.
* **Reversal used the sample's own range** for an item with no numeric choice
  set, so the same answer received a different reversed value as respondents
  were added. Slider and rating items now reverse on their declared limits.
* **A factor was scored on its level positions**, so a factor holding 10 and
  20 was scored as 1 and 2.
* **`item_report()` diagnostics now match the scale's scoring.** Item-rest
  correlations use the reversed orientation, where a reverse-keyed item used
  to come out strongly negative beside a high alpha. They use respondents who
  answered every item, where a missing item counted as 0. Floor and ceiling
  use the item's declared lowest and highest response, where they used the
  sample's extremes, and are `NA` for an item that declares no bounds.
* `sf_scale()` now checks its parameters, and refuses a constructed item
  passed inside a scale's `items`, which was silently dropped from the
  instrument.

## Statistics fixes

* **Cochran's Q counted a missing answer as an observed 0**, changing both Q
  and the analysed N, and read any unrecognised code as 0. Missing answers
  now remove the respondent, and codes other than 1/0, TRUE/FALSE and yes/no
  are reported.
* **PLS-SEM syntax could add an indicator the model left out.** A construct
  over `Q1` and `Q3` was written as the range `Q1` to `Q3`, adding `Q2`.
* **Indirect effects could use a path the model never declared**, and a
  model with parallel mediators wrote its total effect twice, each copy
  holding one route. Both are now caught or corrected.
* **ANCOVA reported an unadjusted group test as adjusted.** The group was
  tested before the covariates. The slope check also left out every
  covariate after the first.
* **Partial correlation p values used the wrong degrees of freedom**, and
  partial Spearman ranked the residuals where it must rank the variables.
* **`sample_size_plan()` returned 64 or 50 per group for t tests and ANOVA**
  whatever alpha and power were requested, while printing both.
* **The Firth likelihood-ratio statistic was half its true value.**
* **Two-way ANOVA gave the residual row a partial eta squared of 0.5.**
* **Results tables rounded p values to 2 decimals**, showing .004 as 0.
  They now show 3 decimals, or <.001.
* **Every `render_results()` table put a whole row in a single cell**,
  headers included. Each value now has its own cell.
* **Rank-test effect sizes and their intervals described different
  statistics.** A signed-rank r also divided by pairs the test had dropped.
* **Bootstrap intervals hid failed resamples**, and a collapsed distribution
  gave a zero-width interval. A Kruskal-Wallis resample with every value
  tied gave an effect of 0.
* **An analysis plan whose blocks all failed returned as if it had
  succeeded**, and a scale-scoring failure before analysis went unrecorded.
* **The t test ignored `var_equal`**, and t tests and one-way ANOVA judged
  significance at .05 whatever `alpha` the block declared. Options a method
  never reads are now listed in `options_ignored`.
* **Reliability, item and EFA blocks analysed every scale** whatever the
  block selected. The quality block ran its duplicate check only when given
  a respondent ID it was never passed, and reported 0 duplicates. It now
  reads the collected ID column, and says when a check did not run.
* **Statistics read a factor on its level positions**, and linear regression
  turned a text predictor into a number. Factors are read through their
  labels, and text predictors stay categorical.

## Decision analysis fixes

* **Weights could be applied to the wrong criteria.** They were matched to
  the performance matrix by count and applied by position, so a weight item
  listing quality before price gave price the weight meant for quality.
  Rated items paired with a differently named weight item still pair in
  declared order, and the result now lists each pairing.
* **Aggregating judgements combined them by position**, so the same
  judgement from 2 respondents whose matrices listed criteria in different
  orders averaged to indifference.
* **AHP and ANP read a data-frame matrix transposed**, reversing every
  judgement while keeping perfect consistency.
* **Factor judgements were read as level positions**, so a judgement of -9
  became 1.
* **A supplied AHP matrix got a consistency verdict without being
  reciprocal**, so rows (1, 9) and (9, 1) received a consistency ratio of 0.
* **Unavailable consistency ratios**, past 10 criteria, produced infinite
  and undefined summaries, and filtering called them too inconsistent.
* **ANP could report a limit that does not exist.** A periodic network now
  gets its stationary priorities, and a reducible one is refused.
* **DEMATEL could fail on an undefined inverse**, and named the first
  criterion the strongest cause when every net relation was 0. It now
  explains the undefined case and names a cause only when one exists.
* **WASPAS reversed its normalisation for negative values.**
* **ELECTRE's kernel could exclude an alternative nothing in it outranked.**
* **ELECTRE sensitivity re-ranked at the default cutoffs** in place of the
  cutoffs the result used.
* **Sensitivity called a ranking stable when no weight had moved**, as
  happens with weights such as (1, 0).
* **Conjoint balance rewarded leaving out an attribute level**, scoring a
  design that never showed one as perfectly balanced. Balance now counts
  every declared level, and the design lists any level it never shows.

## Text analysis fixes

* **Words with accents and text in other scripts were being cut up.** The
  tokeniser kept ASCII letters alone, so "café" was counted as "caf",
  "naïve" became two fragments, and a response written in a non-Latin script
  could come out empty. Every Unicode letter and digit is now kept, in term
  frequency, n-grams, co-occurrence, topic models and sentiment.
  **Term counts and topic models over non-English text will change.**
* **N-grams reported phrases nobody wrote.** Stop words were removed before
  the window slid over what was left, so "clean but not comfortable" produced
  the bigram "clean comfortable". An n-gram is now built only from words that
  were next to each other, which is what makes it a phrase a respondent used.
  **Bigram and trigram tables will change, and some will be smaller.**
* **A respondent with no group value was counted into every group** in
  grouped sentiment. With twelve responses per group, both groups reported
  thirteen, the extra one scored as neutral. Grouped term frequency had the
  same mismatch between the text and the rows it came from.
* **A response of punctuation alone counted as a usable response.** Rows were
  chosen before punctuation and numbers were stripped, so such a response
  survived as an empty string, counted towards the minimum corpus, and scored
  as a neutral observation in sentiment.
* **`term_context(window = 0)` copied the match into its own context.** A zero
  window now gives empty context, and a negative one is refused, as is
  `max_matches` below 1 and an n-gram size below 2.

## New: the R code behind each result

* **A report now shows the statistical call that produced each number.** Every
  result carries a folded "Show R code" block with the call, the variables and
  the options resolved, so a reader sees `cor.test(x, y, method = "pearson")`
  where the wrapper call was all that showed before. Both
  report engines show it, and `render_report(show_code = FALSE)` leaves it
  out.
* **`analysis_syntax()` returns that code**, for one result or a whole set.
  `header = TRUE` prepends the lines that load the instrument, read the
  responses and score the scales, giving a script that runs on its own.
* The code is built from the same resolved specification the analysis ran, and
  the package's tests run the generated code and compare its statistic and p
  against the package's own result. Where a method falls outside its coverage
  the block is omitted, which keeps the report honest about what it can show. The model families already carried their
  syntax, through `cfa_syntax()` and its neighbours.

## Figure fixes

* **Grouped rating charts drew two segments over each other.** The negative
  block started at zero while the neutral block straddled zero, so with five
  equally frequent options ten percentage points of the bar were drawn twice
  and the bar was ten points short. This affected the matrix and scale charts
  in every report. The single-item chart was always correct.
* **A repeated-measures figure described more respondents than its test.** The
  test drops anyone missing a measure, where the figure dropped missing
  values measure by measure, so an excluded respondent could move the plotted
  medians. The figure now uses the same respondents and says how many.
* **A Q-Q plot compared raw values against the wrong line.** The reference was
  y = x, so a normal variable with a mean of 100 looked severely non-normal.
  The line now follows the sample's own quartiles.
* **A network figure renumbered its clusters.** Clusters were relabelled by
  size while the legend said "Cluster", so the cluster a table called 2 could
  appear as Cluster 1. The figure now uses the same numbers as the table.

## Report fixes

* **A report could not render into a folder whose name contains a space.**
  The Quarto renderer's arguments were passed to the shell unquoted, so such
  a path split into several arguments and the render failed, falling back to
  the built-in HTML engine with no explanation. Each argument is now quoted.
* **Distributions disappeared when ggplot2 was absent.** A scale's rating
  items are grouped into one chart, and every item in such a scale was skipped
  whether or not that chart could be drawn, though the built-in charts draw
  each one. A missing optional package now costs the grouping and keeps the
  information.
* **A print-palette report mixed monochrome and colour figures**, because the
  single-item diverging chart was drawn with the default palette.
* **Collected multiple-choice answers were missing from the distributions.**
  A multiple-choice question is collected as one column per option, and the
  section looked for a single column under the question's own name, so every
  such question was skipped. Both report engines now count the options, label
  them from the choice set, and state how many respondents answered, since one
  respondent can pick several.
* **A quanteda result's leading features never reached the report**, though its
  own prompt asked a reader to review them. A moderation's conditional slopes
  at the moderator's own values were missing in the same way. Both now render
  as a second table under the result.
* **A mediation table did not say which model produced it.** It gave Direct,
  Indirect and Total with no variable names and no a or b path, so two
  negative paths looked the same as two positive ones: both give a positive
  indirect effect. The table now names the predictor, mediator and outcome and
  reports a and b with their signs.
* **APA numbers follow APA 7 more closely.** `p` loses its leading zero,
  as `p = .032`, and an interval says its level, as `95% CI [0.12, 0.48]`.
  The sentence is plain text, so italicising the symbols stays the author's
  step, and `?sf_apa` now says so.
* **A report claimed an analysis seed when analysis was switched off.**
  `render_report(include_analysis = FALSE)` still printed a seed beside the
  instrument hash, which reads as provenance for an analysis that never ran.

## Provenance and file fixes

* **A revised instrument could be written with its change undisclosed.**
  Reading a file, editing it in memory and writing it produced a consistent
  file with nothing in its amendment log, and a log with entries removed or
  reordered was written as readily.
* **An amendment could record a fingerprint of content that was never
  written**, because the fingerprint was taken before validation updated the
  instrument.
* **A plan or model change could be recorded as a routine pipeline
  amendment**, skipping its deviation report.
* **A one-item scale was written as a single value** where the published
  instrument profile requires an array, so a valid instrument failed the
  profile, and the builder and R hashed the same instrument differently.
* **The file reader, the bundled schema and the published profile disagreed**
  on required fields. They now share one core, `hash`, `meta` and `items`, and
  the reader refuses a file from a newer major format version.
* **`read_responses()` changed values on the way in.** A respondent ID of `001`
  became `1`, a text answer of `NA` became missing, a matrix missing a row
  column raised no warning, and a duplicated column was silently dropped.
* **The Google Sheets collector could mis-file answers under a duplicate or
  blank heading**, and 2 submissions could interleave while the header was
  being extended. Submissions now run inside a lock, and an ambiguous header
  sends the raw submission to an "Unmapped submissions" sheet.
* **`sframe_export_labelled()` left matrix cells, option columns and text-held
  codes unlabelled.** Every response column now carries its row, option or
  criterion wording, and every column of choice codes carries value labels.
* **Generated notebooks hid failed analyses**, never ran their report step,
  and broke on a title containing a quote. Each failed block now shows its
  error, the report renders when at least one analysis succeeded, and titles
  are escaped.
* **Shipped demo results could fall out of step with the package.** A test
  now holds every demo's results file equal to what the package computes.

## SurveyStudio round-trip fixes

* **A rebuild in SurveyStudio dropped item-level reverse coding.** Studio
  replaces the instrument it holds with a rebuild from its editor whenever
  the draft is valid, and that rebuild cleared every item's `reverse` flag,
  keeping only the reversal declared on a scale. An item marked
  `reverse = TRUE` was scored as though answered in the same direction as
  the rest, moving every composite and alpha built on it.
* **A rebuild dropped conjoint designs**, so a declared design was lost as
  soon as the instrument passed through Studio.
* **An item held as a plain list lost its settings**: date limits, matrix
  rows, slider and rating settings, comparison items and the comparison
  scale. Every field the item constructor accepts is now carried.
* **Studio's item-save handler replaced an item wholesale**, which would
  have cleared reverse coding, scale membership, page and type settings on
  any edit. Studio ships no item form, so the handler is unreachable from
  the interface today and no released version could reach it. It now applies
  an edit to the item already there, and drops settings belonging to the
  previous type only when the type itself changes.
* Reverse coding stays where it was declared. A scale's `reverse_items` stay
  on the scale, where a rebuild used to copy them onto each item, which
  changed a loaded instrument's content simply by opening it.

## Documentation

* **`export_google_sheet()` told researchers to let anyone with the link edit
  the response sheet**, which exposes participant data to anyone holding the
  URL. The collector writes through the sheet it is attached to, so link
  sharing was always unnecessary. The help now says to keep the sheet private. If you
  followed the old advice, review the sheet's sharing settings.
* **The cross-check claim made for the decision methods is narrowed to what
  the suite holds.** 0.4.0 said an independent computation had to agree
  before a method was accepted. The suite calls RMCDA for 5 of the 10
  methods, and 4 of those compare numbers: AHP weights, VIKOR's S, R and Q,
  MOORA's ratio system and WASPAS scores. The ELECTRE call compares the
  ordering of one concordance pair. ANP, DEMATEL, SMART, PROMETHEE and
  TOPSIS are checked against hand-derived values and published worked
  examples, with no second implementation. Both kinds of evidence are
  recorded per method in `vignette("mcdm-analysis")`.
* The `survey_module_ui()` and `survey_module_server()` help is rewritten,
  covering supported item types, what is submitted, a failed save, and
  changing the instrument, with a complete example that stores responses.
* The `item_report()` help describes item-rest correlations, where it said
  item-total, and the nested result it returns, with both ways to extract it.
  The `sf_scale()` help states the rules for `min_valid`, `weights` and
  `reverse_items`.
* The `sample_size_plan()` help separates power calculations from precision
  targets and rules of thumb, and states which arguments each one uses.
* The `sensitivity_analysis()`, `sframe_dematel_compute()` and
  `sframe_rated_matrix()` help describe the new counts, the undefined
  total relation, and how rated items pair with a weight item.
* The provenance help for `amend_sframe()`, `link_git_commit()`,
  `read_sframe()` and `write_sframe()` states what the hashes establish: a
  canonical content check, local and unsigned. They identify content, and
  who wrote an instrument or when needs other evidence. The bundled schema
  says the same.
* The `read_responses()` help describes one contract for undeclared columns
  and documents expansion columns and value conversion.
* **`vignette("scale-reliability-validity")` said that supplying construct
  scores returns the HTMT matrix.** It returns the absolute inter-construct
  correlations, and records `htmt_method = "correlation_fallback"` to say which
  it computed. The Henseler heterotrait-monotrait ratio needs
  `items_by_construct`, which records `htmt_method = "henseler"`.
  `validity_report()`'s own help was already accurate, and the vignette
  overstated it in an unevaluated chunk. The section now names what each
  argument gives, and its examples run, so a build would contradict the claim
  if it drifted again.
* **`citation("surveyframe")` abbreviated the author as "Sharafuddin M".**
  The given names Mohammed Ali were passed to `person()` as one string, which
  R shortens to a single initial. They are now 2 given names, so the text
  citation reads "Sharafuddin MA". The BibTeX entry keeps its correct form. The citation year now comes from the release date, where it
  followed the date the citation was run.
* **The PDF reference manual kept its text inside the page margins.** Long
  function names in running text, 2 tables in `?sframe_plots` and
  `?sframe_as_data_frame`, and the Usage of `sf_item()` and
  `sample_size_plan()` ran past the right margin in 83 places. The tables are
  now lists, the Usage lines wrap, and the manual lets a line stretch
  its spaces before letting a name overflow.
* **The APA citations attached to results carried a fixed year of 2026.**
  surveyframe's own citation now takes its year from the installed release,
  and R Core Team's from the running version of R.

## Dependencies

`callr`, `chromote`, `httpuv` and `pkgload` join Suggests. They are used only
by tests that drive a survey in a real browser, which are skipped on CRAN.

# surveyframe 0.4.1

## New

* **A demo library.** Twenty-two small demos, each showing one thing, in
  `sframe_demos()`. Load one with `sframe_demo("two_group")`, or get a
  Quarto notebook to edit with `sframe_demo_qmd("two_group")`. Together they
  cover every analysis method and every question type.
* **`vignette("learn-by-example")`.** Pick the demo that matches the data you
  have, and follow it from questionnaire to report.
* **`sframe_export_labelled()`** writes SPSS `.sav` or Stata `.dta` with the
  question wording and response options attached, so variables arrive
  labelled rather than as codes.
* Each demo also ships a codebook of variable and value labels, and the
  results surveyframe produced, so you can check the numbers in other
  software.

## Reports are now reproducible

`run_analysis_plan()` gains a `seed` argument, set by default. Bootstrap
confidence intervals and the EFA parallel analysis previously drew from the
random stream unseeded, so the same data gave a slightly different interval
on every run.

**Confidence intervals will move once** when you re-run an older analysis.
Test statistics and p values are unaffected. Use `seed = NULL` for the
previous behaviour.

`render_report()` now says which engine produced the file, Quarto or the
built-in writer, in a message, in an `engine` attribute, and in the report
itself beside the instrument hash and the seed.

## Bug fixes

* **The Google Sheets collector could corrupt collected data.** Adding a
  question mid-study left the sheet's header stale while new rows used the
  new order, so values landed under the wrong headings with no error. The
  collector now matches columns by name.
* **Branching rules using `%in%` with more than one value never worked in an
  exported survey.** The question stayed hidden whatever the respondent
  answered. Present since 0.3.0.
* **`sem_lavaan_syntax()` produced a mediation model lavaan could not fit.**
  Indirect effects referred to path labels that were never written.
* Straight-lining no longer flags scales shorter than four items, where
  identical answers are normal rather than careless. `quality_report()` gains
  `straightline_min_items`. When no scale is long enough to check, the report
  now says so instead of omitting the chart silently.
* Reading a response file with `read.csv()` no longer mangles matrix columns
  whose labels contain spaces. Use `check.names = FALSE`.

## Dependencies

`haven` and `V8` join Suggests, both optional.

# surveyframe 0.4.0

A major release. It adds multi-criteria decision analysis (10 methods),
small-sample statistics, text and open-ended response analysis (9 methods),
and a disclosed-amendment and Git-linked provenance trail for `.sframe`
files, alongside 4 corrected results and 2 breaking changes. See below for
full detail on each.

## New: multi-criteria decision analysis (MCDA)

surveyframe's decision-family extension links survey collection directly to
10 MCDA methods, closing the gap between MCDA computation packages, which
assume a clean matrix already exists, and survey software, which has no
concept of a decision method at all.

* 10 decision methods: the Analytic Hierarchy Process (AHP), the Analytic
  Network Process (ANP), the Decision Making Trial and Evaluation Laboratory
  method (DEMATEL), VIKOR, MOORA, SMART, WASPAS, PROMETHEE, ELECTRE, and
  TOPSIS. Every method carries a verified literature citation.
* 2 new item types collect judgement data directly inside the survey
  instrument: `pairwise_comparison` (Saaty's 1-to-9 ratio scale for AHP and
  ANP, or a 0-to-4 directed influence scale for DEMATEL) and
  `criteria_weight` (a constant-sum allocation across criteria).
* A documented aggregation layer (`R/decision_data.R`) turns per-respondent
  answers into the matrices the methods consume: `sframe_assemble_pairwise()`
  builds one matrix per respondent and validates every pair was answered,
  `sframe_aggregate_judgements()` combines them (geometric mean for AHP/ANP,
  which preserves reciprocity, or arithmetic mean for DEMATEL), and
  `sframe_rated_matrix()` builds a performance matrix from ordinary matrix
  items. AHP judgements are additionally screened for consistency against
  Saaty's random-index table, with the CR distribution reported whether or
  not a study has pre-declared a filtering threshold.
* Every ranking method resolves its matrix and weight inputs in the same
  order (a researcher-supplied override, then a collected item, then a
  typed error naming what is missing) and records where each input came
  from, so a results table states the provenance of every number.
* `sensitivity_analysis()` reports how far a ranking moves under a declared
  perturbation of the weights, and carries a `degenerate` flag so a
  ranking that never separated its alternatives cannot report false
  stability (see "Decision analysis: non-results now say so" below).
* Both the visual builder and SurveyStudio support the 2 new item types,
  and the static HTML survey, the Shiny module, and the builder preview
  render all 3 judgement-collection structures identically.
* RMCDA joins Suggests as a test-time cross-check oracle: an independent
  computation of the same method on the same matrix is compared with the
  package's own result. This practice caught a real defect during
  development, a WASPAS runner that had inherited SMART's normalisation
  step by mistake. (Corrected in 0.4.2: this bullet first said independent
  agreement was required before any method was accepted, which was wider
  than the suite. See 0.4.2's note for the coverage each method has.)

## New: small-sample statistics

A track of corrections for comparisons run on small samples, where the
ordinary versions of these tests can flip significance on repeated draws
from data whose true difference never changed.

* The Hodges-Lehmann shift estimator as an alternative to the independent
  two-group Mann-Whitney comparison.
* The paired Wilcoxon pseudomedian confidence interval as an alternative to
  the paired t-test.
* The exact odds-ratio confidence interval on Fisher's test for small
  2x2 tables, avoiding the ad hoc continuity correction a conventional
  Wald interval needs when a cell is zero.
* Firth's bias-reduced logistic regression (`logistf` in Suggests) for
  regression prone to separation at small n.
* A small-sample advisory surfaced on `assumption_report()` and
  `sample_size_plan()`, flagging when a study's sample size falls in the
  range where these corrections are worth considering.
* `vignettes/small-sample.Rmd` walks through when to prefer each
  correction over its conventional counterpart.

## New: text and open-ended response analysis

A 9-method text-analysis family for open-ended survey items, from term
frequency through topic modelling, sharing the same analysis-plan,
role-resolution, and reporting pipeline every other method family uses.

* `term_freq`: top terms by frequency, optionally split by a group
  variable, rendered as a bar chart or a word cloud.
* `ngram_freq`: top bigrams or trigrams by frequency.
* `term_context`: a keyword-in-context concordance table (before/match/
  after) for a chosen keyword.
* `co_occurrence`: pairwise within-response co-occurrence counts on the
  top terms, rendered as a heatmap.
* `co_occurrence_network`: a Louvain-clustered (Blondel et al. 2008),
  force-directed (Fruchterman & Reingold 1991) term co-occurrence
  network; requires the optional igraph package.
* `tidy_sentiment`: positive/negative sentiment counts and proportion
  positive using the bing lexicon, optionally split by a group variable,
  rendered as a diverging bar chart or a positive/negative comparison
  word cloud; requires the optional tidytext package.
* `quanteda_dfm`: a document-feature matrix summary (feature count,
  sparsity, top features); requires the optional quanteda package.
* `topic_model_lda`: Latent Dirichlet Allocation topic modelling, top
  terms per topic as a ranked table and a faceted bar chart; requires
  the optional tidytext and topicmodels packages.
* `stm_topics`: structural topic modelling, the same top-terms-per-topic
  output; requires the optional stm and tidytext packages.
* A shared cleaning step (`clean_text_responses()`) and a 174-word
  Snowball-based English stopword list, both exported so a study can
  reuse or override them outside a runner.
* Both the visual builder and SurveyStudio support all 9 methods,
  including the word-cloud, top-N, seed, and topic-count (`k`) options
  that steer their plots and models.
* `vignettes/text-analysis.Rmd` walks through cleaning, each method, and
  what the family deliberately does not attempt (stemming/lemmatisation,
  tf-idf, and keyness comparison are not yet implemented).

## New: disclosed amendments and a Git-linked provenance trail

`write_sframe()`'s SHA-256 hash proves a `.sframe` file is unchanged since
it was written, but gives no way to distinguish a legitimate revision
(a data-entry correction, bot-response removal, a documented model
respecification) from an undisclosed edit -- both break the hash
identically. This release adds a disclosed-revision path alongside the
existing hash check, without weakening it.

* `amend_sframe()` compares an instrument before and after a change and
  appends a structured, timestamped entry to an ordered amendment log --
  never overwrites -- recording the reason (a controlled vocabulary:
  `data_correction`, `bot_removal`, `model_respecification`,
  `instrument_revision`, `other`), a free-text explanation, and which
  top-level fields changed.
* Two tiers, by default inferred from the reason: `"pipeline"` amendments
  (data corrections, bot removal) need only a reason. `"design"`
  amendments (anything touching the analysis plan or a model) require a
  `deviation_report` describing what changed in the research question,
  method, or model and why, matching how a formal preregistration
  deviation is normally handled. `signoff` is never left blank -- it
  records a reviewer's name or the literal `"none"`, so an unreviewed
  design change stays visible to an auditor.
* `amendment_log()` returns the full history as a data frame, one row per
  disclosed amendment, exportable with `write.csv()`.
* An edit made directly to a `.sframe` file, bypassing `amend_sframe()`,
  still fails `read_sframe()`'s integrity check exactly as before. The
  amendment log adds a disclosed path alongside the existing hash check.
* `link_git_commit()` records the current Git commit SHA and subject line
  alongside an instrument. This ties the SHA-256 hash to a specific,
  already-explained commit. It returns an informative message when Git
  isn't installed or the path isn't a repository. Git is optional.
* `inst/schema/sframe_schema.json` documents the `.sframe` format (every
  top-level field, including the new `amendments` log) as a standalone
  JSON Schema, so a reviewer or a second tool can read and validate a
  `.sframe` file without installing the package. `.sframe` was already
  plain, git-diffable JSON before this release; the schema makes that
  format explicit and independently checkable.
* `vignettes/surveyframe.Rmd` gains a "What the SHA-256 hash proves, and
  what it does not" section, stating plainly that the hash proves file
  identity, not methodological validity, and pointing to the
  design-time `analysis_plan` binding and `run_analysis_plan()`'s
  single-pass execution as the package's separate, complementary defence
  against HARKing and p-hacking.

## Corrected results (read before comparing against earlier output)

Four defects found by independent cross-validation are fixed. Each
produced normal-looking numbers with no error or warning, so re-run any
results computed with an earlier version.

* `item_report()` returned the wrong item-rest correlation. It subtracted
  each item from a `rowMeans()` total, which leaves roughly noise carrying
  the item negatively, so a highly reliable scale reported strong negative
  values. On simulated data with alpha 0.947 every item came back at about
  -0.46. The statistic is now the item against the sum of the other items
  in its scale, and matches `psych::alpha()`'s `item.stats$r.drop` to
  1e-10.
* Repeated-measures ANOVA tested the condition effect against the wrong
  error term, because the subject identifier was left as an integer and
  `aov()` treated it as a continuous covariate. On a fixture where
  `jmv::anovaRM()` gives F(2, 78) = 86.93, surveyframe reported F = 1.45,
  p = 0.24. Correcting the identifier alone was not sufficient: the
  corrected design produces no `Error: Within` stratum, so the effect is
  now located by searching the strata directly.
* `validate_sframe()` rejected valid instruments. Its known-variable list
  held only base item and scale ids, so an analysis plan naming an
  expansion column (`item__sub`, `item__option`, `item__a__vs__b`,
  `item__crit`) failed validation for variables that do exist, including
  real exports from the visual builder. `read_responses()` already
  accepted those columns. Both now derive the list from one shared helper.
* The SEM syntax generators ignored the model type. `seminr_syntax()`,
  `sem_lavaan_syntax()`, and `cfa_lavaan_syntax()` never checked
  `model$type`, and the builder offered every saved model to all 3
  generators, so a covariance-based model produced PLS-SEM syntax with no
  complaint. That is a runnable script estimating a model the researcher
  never declared. All 3 now refuse a mismatched estimation family, and the
  builder filters each model role to the types its generator can produce.

## Breaking: `validate_sframe()` and `validate_model()` return a diagnostic

Both validators previously returned two different things depending on
`strict`: the object itself, invisibly, when `strict = TRUE`, and a bare
unclassed list when `strict = FALSE`. Neither was a diagnostic, the
success path printed nothing at all, and the `strict = FALSE` return had
no methods. Both now return an `sframe_validation` object, and they
return it visibly, so `validate_sframe(instrument)` typed at the console
shows the user what it found.

* The object records `valid`, every `problems` message, and a `checks`
  table listing all 18 instrument checks (10 for a model) whether or not
  each found anything. A diagnostic that lists only failures cannot tell
  a user that a check passed from one that was never reached.
* Read it with `print()`, `summary()` for the check roster,
  `as.data.frame()` for one row per problem, `sf_is_valid()`, and
  `sf_problems()`.
* `strict = TRUE` still aborts with `sframe_validation_error` when
  anything is wrong. That has not changed.
* **`$valid` and `$problems` keep working**, so the common reading
  pattern needs no migration.
* **What breaks**: code using the `strict = TRUE` return as an
  instrument, as in `instrument <- validate_sframe(instrument)`. Wrap it
  in `as_sframe()`. Passing a validation result where an instrument is
  expected now raises a directed error naming `as_sframe()` immediately.

Raised by a Journal of Statistical Software editor reviewing the code:
"we would at least expect that the object is not silently returned and
that the print method is adapted to allow the user to read directly the
diagnostic".

## New: accessor and exploration methods for every class

The same review found that the classes carried `print`, `summary` and
`format` only, so user code had no route to their contents except `$` on
the underlying list, which makes the internal layout part of the public
contract. Two facts made that concrete: `as.data.frame()` failed on all
14 result classes with "cannot coerce class ... to a data.frame", and `[`
dropped the class on the list-backed reports, so `results[1:2]` silently
degraded to a bare list and lost its print method.

* `as.data.frame()` now works on the instrument and on every report
  class, returning that object's primary table.
* `[` keeps the class on `sframe_analysis_results`,
  `sframe_reliability_report` and `sframe_item_report`.
* Instrument accessors: `sf_meta()`, `sf_items()`, `sf_scales()`,
  `sf_choice_sets()`, `sf_branches()`, `sf_checks()`, `sf_models()` and
  `sf_plan()`, with `sf_plan<-` for declaring the plan. The component
  accessors return an `sf_component_list` named by ID, so
  `sf_items(instrument)[["sat_1"]]` reaches one item.
* Component accessors: `sf_id()` and `sf_label()`.
* Report accessors: `sf_apa()` and `sf_flagged()`.
* Coercion: `as_sframe()`.

The vignettes and the examples are rewritten to use these accessors. The
registered S3 method count goes from 41 to 103.

## Breaking: the Shiny collector now emits expansion columns

* `render_survey()` pipe-joined a matrix item's cells into a single column,
  so a matrix question answered in the Shiny survey arrived as
  `mx = "4|5"` where `read_responses()` and the whole analysis layer expect
  `mx__r1` and `mx__r2`. Data collected that way could not be read back by
  the package at all, and nothing said so at collection time. Ranking and
  multiple-choice items had the same shape problem.
* All 3 now emit the expansion columns that the static template and the
  Google Sheets collector already emitted: one column per matrix sub-item
  carrying its value, one per ranking option carrying its rank position, and
  one per multi-select option carrying 0 or 1.
* **This changes the output shape of the Shiny collector.** A study
  mid-collection through `render_survey()` will see its matrix, ranking, and
  multi-select columns change name and layout between versions. Responses
  already gathered under the old shape need re-shaping before they can be
  read, and the decision item types are unaffected because they emitted the
  correct columns from the start.

## The rated performance matrix can now be wired in both GUIs

* SurveyStudio and the visual builder both offered an empty "Performance
  matrix items" dropdown for all 7 ranking methods (TOPSIS, VIKOR, MOORA,
  SMART, WASPAS, PROMETHEE, ELECTRE). That role matches on a `"matrix"`
  level, and neither surface gave matrix items one: the studio classified
  them as `"identifier"` and the builder grouped them under `"expanded"`,
  which no role accepts. The effect was that the rated-matrix path, where
  respondents rate every alternative on every criterion, could only be built
  by writing R directly, even though it is one of the 3 declared ways to
  supply a decision matrix. Matrix items now carry their own `"matrix"`
  level in both surfaces.

## Decision analysis: non-results now say so

* ELECTRE I reports when it establishes no outranking relation at all. On a
  9-criterion problem at the default 0.70 and 0.30 thresholds no alternative
  clears concordance against any other, so every score is 0 and every
  alternative ranks 1. That is legitimate behaviour for the method, but the
  results table read as "all 9 alternatives are jointly best" and the APA
  sentence reported a kernel containing every alternative. A note now
  explains the equal ranks as an absence of evidence.
* `sensitivity_analysis()` gains a `degenerate` flag for the same reason. A
  ranking that never separated the alternatives cannot be changed by
  perturbing a weight, so every check passed and `stable` came back `TRUE`:
  the strongest robustness signal the function can give, produced by the
  weakest result it can be handed. `print()` now leads with "No result to
  test" instead of "Stable" in that case.

## Data quality

* `quality_report()` counted only columns matching a bare item id, and
  multi-column items never post under those, so every expansion column was
  invisible to the missingness check. A respondent who skipped an entire
  pairwise battery was reported at 0 percent missing. Expansion columns now
  count as item data, which brings matrix, ranking, multi-select, and the 2
  decision item types into the missingness figures for the first time.
  **Reported missingness rates will change for any instrument using those
  item types**, because columns that were silently excluded are now counted.
  Straight-lining and timing are unaffected: straight-lining runs over
  declared scales, and timing is measured on the clock.

## Bundled demo data

* Both bundled demo instruments wired their seminr block to a `cb_sem`
  model, so `sframe_demo_data()` generated PLS-SEM syntax from a
  covariance-based model, and every vignette and example loading it
  inherited the same mismatch. Each demo now carries a real `pls_sem`
  model with composite constructs. The instrument hashes changed with it.

## Decision analysis

* All 10 MCDM methods now return a citation. Previously only TOPSIS and
  AHP had one. Every reference was checked against the publication record.
* `sframe_decision_options()` documents PROMETHEE's preference functions
  and records why the default is `"usual"`, Brans and Vincke's type I step
  function, chosen over the linear function several other implementations
  default to. Net flows differ between the 2 functions, and the ranking
  changed in 226 of 400 randomly drawn 4-alternative by 3-criterion
  matrices.

# surveyframe 0.3.4

This release completes the plotting, interface, statistics, and reporting
work started in 0.3.3. Every analysis family now has a chart, every effect
size ships with a confidence interval, reports accept written
interpretations and print to PDF, both dashboards gain quality and
correlation panels, date questions gain bounds, and the builder and
vignettes pass a WCAG 2.2 AA accessibility audit. Hard dependencies are
unchanged. naniar and pagedown join Suggests.

## Effect sizes and intervals

* Four new exported helpers, all base R: `bootstrap_ci()` (percentile
  bootstrap for any statistic), `cohens_d_ci()`, `cramers_v_ci()`, and
  `eta_sq_ci()`.
* Analysis-plan runners attach a confidence interval to their effect size
  as a new result key: `d_ci` on the t-tests, `r_ci` on Mann-Whitney and
  Wilcoxon, `eta_ci` on ANOVA and Kruskal-Wallis, `ci` on the correlations
  (analytic Fisher z for Pearson, bootstrap for the rank methods), and
  `v_ci` on chi-square and cross-tabulation.
* APA strings and writing prompts now carry the interval, for example
  `d = 0.62 [0.18, 1.05]`. Data too small for an interval keeps the
  previous string.

## Psychometrics

* `validity_report()` computes the Henseler heterotrait-monotrait ratio
  when item-level data is supplied through the new `items_by_construct`
  argument. Without it, the previous correlation-based fallback applies
  and the `htmt_method` element records which was used.
* `missing_data_report()` runs Little's MCAR test when naniar is
  installed. Without naniar the result is unchanged.
* `reliability_report()` records why omega is unavailable for a scale in
  an `omega_note`, and the reliability chart names those scales in its
  subtitle.
* `efa_solution()` adds three tidy data frames ready for plotting and
  reporting: `loadings_long`, `communalities_table`, and
  `variance_table`.

## Reports and codebook

* `render_report(format = "pdf")` prints the HTML report to PDF through
  pagedown, which requires a local Chrome or Chromium. HTML output is
  unchanged and remains the default.
* The report's built-in styling now uses a small set of CSS variables, so
  a re-theme is a one-line change, and a print stylesheet paginates the
  report cleanly. Tables carry captions and header scopes, and every
  embedded chart has descriptive alternative text.
* The codebook now includes the pre-declared analysis plan and the saved
  measurement and structural models, so one document fully records the
  instrument a study used.
* The codebook's items table shows each item's actual response options and
  scale label directly, instead of an id that needed a separate choice-sets
  table to decode.
* Analysis-result tables (frequency, cross-tabulation, group comparisons,
  regression coefficients, and the rest) show item, scale, and
  response-option labels instead of the underlying ids and coded values.
* Report tables render as properly split HTML tables in both the Quarto and
  internal HTML report paths.

## Written interpretations in reports

* New `interpretations` argument on `render_report()` and
  `render_results()`. Pass a named list keyed by analysis-plan block id to
  add a written interpretation to each research question after the results
  are known. The report shows it beside the pre-declared decision rule, so
  the prospective plan stays visible next to the post-hoc narrative.
  Interpretations are report content only and are never written into the
  instrument file.
* SurveyStudio's Export screen gains an Interpretations card: one block per
  research question, in reading order (result table, chart, planned
  decision rule, then the interpretation), shown with the live result once
  responses are loaded. The generated report includes whatever you write
  there.
* A "Copy result" button on each Interpretations block copies the whole
  result, table, chart, and the interpretation as written, as one block,
  for pasting into a document.
* The SurveyBuilder Report outline now edits the planned decision rule
  inline, in sync with the research-question dialogue.

## Charts

* `run_analysis_plan(plots = TRUE)` now attaches a chart to every
  supported family: regression diagnostics (4 panels), EFA scree and
  loadings heatmap, reliability bars, mosaic and crosstab, correlation
  heatmap, quality flag rates, group-comparison boxplots, paired slope
  charts, raw-variable distributions, repeated-measures profiles, a
  partial-correlation residual scatter, logistic-regression odds-ratio
  forest plots, a moderation interaction plot, and a mediation effect
  chart. Every analysis-plan block now returns a table, a chart, or
  generated syntax.
* Distribution shape by variable draws as a violin per variable, instead
  of a bar chart of the skewness and kurtosis summary statistics.
* A scale's separate Likert items, and a matrix question's rows, draw as
  one grouped diverging chart, instead of one chart per item.
* New `plot()` methods for descriptives, EFA, quality, reliability,
  validity, missing-data, and analysis-results objects. `plot(results)`
  draws every attached chart, and `plot(results, which = "rq_id")` returns
  one.
* New `plot_palette` argument on `run_analysis_plan()` and
  `render_report()`: `"web"` for brand colour on screen, `"print"` for
  black and white suitable for print and journal submission. SurveyStudio
  exposes the choice as a Chart theme option on the Export screen.
* SurveyStudio's Analyse screen shows one result card per research
  question with its chart beneath the statistic.
* Both dashboards (the standalone response dashboard and the SurveyStudio
  Dashboard tab) gain a straight-lining flag-rate chart, a missing-data
  chart, and a scale-score correlation heatmap. All dashboard charts keep
  a base-graphics fallback, so ggplot2 remains optional.

## Survey design

* Date questions accept `date_min` and `date_max` bounds in `sf_item()`,
  the SurveyBuilder, and the exported survey. The date picker enforces the
  bounds and typed dates outside them show a clear message.
* The SurveyBuilder ships a library of 14 preset choice sets, regenerates
  item ids safely when the response type changes, and expands matrix,
  ranking, and multiple-choice items into the same per-option variables
  that `read_responses()` produces.
* The survey thank-you page no longer forces a CSV download. It offers a
  "Download my response" button and honours a configured redirect.

## Accessibility

* The SurveyBuilder interface passes an instrumented WCAG 2.2 AA audit
  with zero findings across its build, preview, and analyse screens and
  dialogues.
* All 7 vignettes pass the same audit: language metadata, AA contrast for
  links and code highlighting, wrapped code blocks, keyboard-reachable
  content, and alternative text on every chart.

## Bug fixes

* `sf_item()`'s `date_min` and `date_max` accept only `"YYYY-MM-DD"` or a
  `Date` object now (an ambiguous string such as `"01/02/2024"` used to
  parse silently into a specific date depending on locale). Anything
  else raises a validation error.
* `bootstrap_ci()`, `cohens_d_ci()`, `cramers_v_ci()`, and `eta_sq_ci()`
  no longer alter the random-number seed for code that runs after a
  reproducible, seeded call.

# surveyframe 0.3.3

This release adds an opt-in plotting layer, fixes bugs surfaced by the
package's first field deployment, and redesigns the survey-taking
experience. ggplot2 joins Suggests; hard dependencies are unchanged.

## Analysis and plotting

* New `plots` argument on `run_analysis_plan()` (default `FALSE`). When
  `TRUE`, supported analysis blocks return a ggplot object in `$plot`: bar
  charts for frequency and chi-square blocks, and scatter plots with a
  regression overlay for correlation and regression blocks.
* New exported `theme_surveyframe()`, a publication-oriented ggplot2 theme
  with an accessible fixed-order series palette. All plots use it.
* Inferential runners return a `$table` data frame ready for
  `knitr::kable()`; the HTML report shows these tables automatically.
* Frequency and cross-tab runners treat empty strings as missing values, so
  partially completed responses no longer form a blank category.
* Ranking items now export one column per option holding its rank
  (`item__option = 1` for the top choice), so ranks are directly analysable.
  Multiple-choice items likewise export one 0/1 column per option instead
  of a single comma-joined column. `read_responses()` accepts the expanded
  columns for ranking, matrix, and multiple-choice items without warnings.
* `render_report()` now attaches each analysis block's chart directly under
  its result table, in both the Quarto and internal HTML report paths,
  instead of tables and plots appearing in separate places.
* Likert items in the report's response-distributions section get a
  diverging stacked bar chart (darkest at each pole, lightest next to
  neutral) instead of a plain frequency bar, so the direction and strength
  of opinion is visible at a glance.

## Survey experience

* A full redesign of the exported survey: larger serif question typography,
  bordered option cards with selection ticks, numbered Likert squares,
  restyled matrix, slider, and ranking blocks, and a slim top progress bar.
  Every colour derives from the instrument's single theme colour, so one
  colour choice re-skins the whole survey. Touch targets meet a 44 pixel
  minimum on phones.
* Branching rules on one question now combine with AND, and hiding a
  controlling question also hides everything that depends on it, so
  screening logic behaves as declared even when answers change.
* Single-page surveys show answered-questions progress (for example
  "12 of 44 answered"); numeric questions respect declared minimum and
  maximum bounds.
* Branching rules can now show and hide section breaks and text blocks, so
  a branched text block works as a screen-out message ("Sorry, you are not
  eligible") and section headings disappear with their questions.
* A matrix question reflows into stacked, labelled row-cards below 600
  pixels instead of a table that needs horizontal scrolling to complete.
* The exported survey meets WCAG 2.2 AA on an instrumented audit: every
  input carries an accessible name, keyboard focus is visible on option
  cards, errors are announced to assistive technology, required questions
  are marked beyond colour, ranking items gain keyboard reorder buttons,
  headings are real headings, and all touch targets and text contrast meet
  the standard.

## Data collection

* Fixed a bug that silently blocked submissions from hosted surveys: the
  Google Apps Script POST now avoids the CORS preflight that Apps Script
  never answers. Collectors no longer emit columns for section breaks or
  text blocks.
* `read_sheet_responses()` gains a `meta_cols` argument for extra sheet
  columns a host application appends, and SurveyStudio's dashboard now
  computes completion times from imported sheet responses.

## Model syntax

* `sem_lavaan_syntax()` turns free-text path labels into valid lavaan
  parameter names (a label starting "H1:" becomes the parameter `H1`).
* `seminr_syntax()` output loads seminr and uses `summary()` accessors, so
  the generated code runs as pasted.
* `run_analysis_plan()` accepts `pls_sem` as an alias for `seminr_syntax`.

## SurveyBuilder and report

* Opening a `.sframe` verifies and reports its SHA-256 integrity status.
* New choice questions start with a fresh option set, and editing shared
  options forks the set first, so options never leak between questions.
* The analysis-plan modal blocks using one variable in two roles, and the
  test suggester handles Likert items and multi-group comparisons sensibly.
* Reports print generated model syntax in code blocks and show reliability
  results as a table.
* "+ Add question" now opens the question-type picker instead of silently
  adding a Likert item, and the redundant icon-only button next to it is
  gone. Survey settings live only in the sidebar; the top bar no longer
  duplicates that entry point.

# surveyframe 0.3.2

This release corrects the package citation, completes the S3 method surface for
the component classes, and improves the graphical tools and the HTML report. It
adds no new exported functions, no new statistical methods, and no new bundled
datasets.

## Citation and methods

* `inst/CITATION` now reports the correct package title and reads the version
  dynamically from the package metadata, so the citation no longer pins an old
  version or an outdated title.
* Added `print()`, `format()`, and `summary()` methods for the component
  classes `sf_choices`, `sf_item`, `sf_scale`, `sf_branch`, `sf_check`, and
  `sf_model`, so each class now has a visible, documented S3 surface.
* `lavaan` is declared in `Suggests`. It is used only to fit the syntax produced
  by `cfa_syntax()`. The package itself generates syntax and never requires
  `lavaan` to be installed.

## Graphical tools

* SurveyBuilder exports a deployable survey in the browser through a new Export
  survey button, and generates the Google Sheets Apps Script collector through a
  Generate collector button. Both reuse the same templates the R functions use,
  so the output matches `export_static_survey()` and `export_google_sheet()`.
* The builder Analyse tab shows three distinct stages: Plan, Run preview (the
  methods that need response data), and Report outline (analyses plus
  measurement models). Analysis plans can be reordered by dragging.
* The exported survey carries a "Built with surveyframe" footer, sizes a header
  logo consistently across aspect ratios, shows a page progress indicator only
  on multi-page surveys, and gains a mobile layout.
* SurveyStudio opens an instrument designed in the builder, previews the exact
  deployable survey in a frame, and analyses uploaded responses. The response
  dashboard, with an overview, item and scale distributions, and a raw-data
  table, is now built into the studio. A button loads the bundled sample survey
  and 120 responses. Survey design moved entirely to the builder.

## HTML report

* `render_report()` renders reliably through Quarto when it is installed. A path
  defect that made the Quarto render fail and fall back to the plain internal
  output is fixed.
* Reports include a response distributions section, with one chart per item and
  one per scale, in both the Quarto output and the built-in HTML fallback.
* Tables are formatted, wide tables scroll within the page, the table of
  contents sits on the left, and numeric values are rounded to two decimal
  places.

## Documentation

* Added the "Deploying a survey and collecting responses on free hosting"
  vignette, covering the Apps Script collector, GitHub Pages and Blogger
  hosting, and reading the responses back into R.

# surveyframe 0.3.1

This is a patch release. It fixes the static-survey to Google Sheets to R
collection loop, repairs a serialisation defect, and improves the first-time
user experience. There are no new exported functions, no new statistical
methods, and no new bundled datasets.

## Bug fixes

### Data collection round-trip

* `export_static_survey()` now renders the header logo and institution name
  from `render$header`, so exported surveys match the Shiny renderer and
  the builder preview.
* `export_static_survey()` now falls back to the instrument's
  `render$google_sheets_endpoint` when `endpoint_url` is not supplied, so
  a Google Sheets endpoint set in the builder is honoured on export.
* The static survey now posts the respondent identifier under the column name
  `respondent_id`, matching the Google Apps Script collector and
  `read_responses()`. The collection round-trip now preserves the identifier.
* `export_google_sheet()` now includes matrix sub-item columns
  (`item_id__sub`) in the Apps Script header row, so matrix answers are
  stored in the Sheet.
* `read_sheet_responses()` now declares `started_at` as a meta column and
  no longer raises a warning on every read.
* Survey logos now keep their original MIME type (`image/png`, `image/jpeg`,
  `image/gif`), so JPEG and GIF logos display correctly in the builder,
  the Shiny renderer, and the static export.

### Serialisation

* `write_sframe()` now strips list-level names from the item, choice, scale,
  branching, check, and model collections before serialisation. Instruments
  built with `Map()` or other helpers that attach element names (for example,
  using item IDs as names) previously serialised those collections as
  keyed JSON objects, producing a hash mismatch and an integrity error
  on `read_sframe()`. Saved instruments now round-trip correctly
  regardless of how the component lists were constructed.

## User experience

* Every exported function that takes an instrument now reports a clear,
  actionable message when passed something that is not an `sframe` object.
  The message points the user to `sf_instrument()` and `read_sframe()`
  instead of showing a raw `inherits()` assertion failure.
* `reliability_report()` no longer prints `psych` internal warnings to the
  console. McDonald's omega is skipped silently for scales with fewer than
  three items, where the statistic is not meaningful.
* The error message from `run_analysis_plan()` when no analysis plan is
  present now describes both the programmatic route
  (`instrument$analysis_plan`) and the visual SurveyBuilder route.

## Documentation

* Rewrote the main vignette (`surveyframe.Rmd`) as an end-to-end worked
  example: design the questionnaire, export it as a hosted survey with a
  Google Sheets backend, collect responses, score them, run the analysis
  plan, and render a report. The results section uses simulated responses so
  the vignette builds offline; a single `read_sheet_responses()` call
  connects the same workflow to live responses. The questionnaire and concept
  are adopted from Sharafuddin, Madhavan, and Wangtueai (2024,
  *Administrative Sciences*, 14(11), 273,
  <doi:10.3390/admsci14110273>), with generic destination wording so the
  example transfers to any tourism services context.
* Updated the supporting vignettes to reflect the research-design-first
  workflow where the instrument holds the questions, the analysis plan, and
  the measurement model.
* `sf_instrument()` examples now include a complete `analysis_plan` block.
* The README now leads with `install.packages("surveyframe")`, adds a short
  path for users who already have a response CSV, and points to
  `browseVignettes("surveyframe")`.

# surveyframe 0.3.0

The first CRAN release of the full workflow: a typed instrument object
carrying the questions, the analysis plan, and the measurement model, with
deployment, collection, analysis, and reporting built around it.

## New features

### Analysis planning, survey statistics, and model syntax

* Added a role-based analysis-plan structure while preserving old
  `variables`/`test` analysis blocks. New plans can store `family`, `method`,
  `roles`, `options`, `hypotheses`, `decision_rule`,
  `reporting_references`, `status`, and `requires_data`.
* Added common survey analysis helpers: `descriptives_report()`,
  `missing_data_report()`, `assumption_report()`, `posthoc_report()`,
  `validity_report()`, and `sample_size_plan()`.
* Expanded `run_analysis_plan()` to dispatch the v0.3 method registry,
  including descriptives, missing data, sparse-table tests, related-sample
  tests, Kendall and partial correlations, two-way ANOVA, ANCOVA, repeated
  ANOVA, ordinal and multinomial logistic regression, mediation, moderation,
  and model-syntax output.
* Added a model specification layer: `sf_construct()`, `sf_path()`,
  `sf_covariance()`, `sf_indirect()`, `sf_model()`, `validate_model()`,
  `model_json()`, `add_model()`, `efa_solution()`, `efa_syntax()`,
  `cfa_lavaan_syntax()`, `sem_lavaan_syntax()`, `seminr_syntax()`, and
  `model_report_template()`. Syntax generation does not require `lavaan` or
  `seminr`.
* `cfa_syntax()` remains available as a backward-compatible wrapper around
  `cfa_lavaan_syntax()`.
* SurveyBuilder Analyse mode now uses a three-panel Plan/Run/Report workspace
  with variable metadata badges, role-based variable assignment, method
  options, output preview, reporting references, and a table-based model
  builder. Significance level is shown only for inferential methods.

### Static HTML survey export

* Added `export_static_survey()`. This produces a single, self-contained
  HTML file that runs the survey in any modern browser without a Shiny
  server or an internet connection. All thirteen item types are fully
  rendered (Likert, single choice, multiple choice, matrix, numeric, text,
  long text, date, slider, rating, ranking, section break, text block).
  Branching logic, required-field validation, a progress bar, welcome and
  thank-you pages are all handled in client-side JavaScript. On submission
  the browser downloads a per-respondent CSV file. An optional
  `endpoint_url` argument adds a parallel JSON POST to any serverless
  endpoint (Google Apps Script, Netlify function, etc.).

  The exported file is suitable for hosting on GitHub Pages, Netlify, or
  any static file server, and can also be shared directly as an e-mail
  attachment and opened from disk.

  The SHA-256 hash written into `.sframe` files by `write_sframe()` and by
  the SurveyBuilder HTML is computed using the same canonicalisation
  algorithm, so instruments round-trip correctly between the browser and R.

### Interactive response dashboard

* Added `launch_dashboard()`. Opens a five-panel Shiny dashboard for
  exploring collected response data alongside the instrument definition,
  without modifying either. The panels are: Overview (response count, date
  range, instrument metadata), Items (per-item bar charts, histograms, and
  frequency tables), Scales (scale score distributions with mean overlay),
  Quality (attention-check pass rates), and Raw data (a scrollable response
  table with CSV download).

  When called without arguments the dashboard loads the bundled tourism
  services demo. When called with a user-supplied instrument and no
  `responses` argument, it opens in metadata-only mode showing instrument
  structure.

* Added `sframe_demo_data()`, `sframe_input_types_demo_data()`,
  `launch_builder_demo()`, `launch_studio_demo()`, and
  `launch_dashboard_demo()` for CRAN-safe examples, training, and local GUI
  testing.

* Added a bundled input-types demo instrument and simulated response dataset
  for testing SurveyBuilder, SurveyStudio, the dashboard, and all supported
  item controls.

* `launch_studio()` now accepts preloaded instruments, response data frames,
  CSV response paths, initial screen selection, host, port, and browser
  control. SurveyStudio reads these preloaded values during startup.

### Shiny survey module

* Added `survey_module_ui()` and `survey_module_server()`. These allow a
  survey to be embedded inside a larger Shiny application as a first-class
  module. `survey_module_server()` returns a `reactive` that holds `NULL`
  until the form is submitted, then returns the response as a named list
  keyed by item ID.

  ```r
  ui <- fluidPage(survey_module_ui("s1"))
  server <- function(input, output, session) {
    resp <- survey_module_server("s1", instrument = instr)
    observeEvent(resp(), { saveRDS(resp(), "response.rds") })
  }
  ```

  An optional `on_submit` callback fires immediately on submission, before
  any `observeEvent()` elsewhere in the app.

### Extended analysis plan tests

`run_analysis_plan()` now implements four additional tests used by the
SurveyBuilder's test dropdown:

* `anova_one`: One-way ANOVA with eta-squared effect size. When the
  result is significant and there are more than two groups, Tukey HSD
  post-hoc output is included in the result object.
* `t_test_pair`: Paired-samples t-test with Cohen's d_z.
* `wilcoxon_pair`: Wilcoxon signed-rank test with r effect size.
* `regression_logistic_binary`: Binary logistic regression with
  McFadden R-squared and an overall model chi-square test. The full
  coefficient table is returned for interpretation.

All four runners produce an APA-formatted summary string and an
interpretation `prompt` field to guide write-up.

## Bug fixes

* `write_sframe()` validates the instrument and writes the validated object,
  preserving `meta$validated = TRUE` in the saved `.sframe` file.
* `.sframe` serialisation now includes a `models` field and continues to read
  older `.sframe` files where `models` is absent.
* `read_responses()` no longer requires display-only items such as
  `section_break` and `text_block` to appear as response columns.
* `validate_sframe()` now checks model references, analysis-plan roles,
  invalid model IDs, duplicate model IDs, and model indicator/path integrity.

* `launch_builder(open = TRUE)` opens the SurveyBuilder HTML in the system's
  default browser via `utils::browseURL()`.
* `R/studio_builder.R` contains three fully implemented internal
  functions (`sframe_builder_empty_state`, `sframe_builder_state_from_instrument`,
  `sframe_builder_validate_draft`) used by SurveyStudio startup and draft
  validation.
* SHA-256 hashing in the SurveyBuilder HTML includes a pure-JavaScript
  fallback for environments where `crypto.subtle` is unavailable on
  `file://` origins, including common Firefox `file://` configurations.
  Saving a `.sframe` file from the builder now always succeeds.
* The SurveyBuilder's `rqSuggest` box now appears with an icon and a
  plain-language recommendation when two or more variables are selected in
  the RQ modal.
* The undo and redo buttons in the SurveyBuilder topbar are now correctly
  disabled when their respective history stacks are empty.

## Security hardening

* `export_google_sheet()` now writes Google Apps Script using JSON-encoded
  JavaScript literals instead of interpolating instrument metadata directly
  into executable code. The generated endpoint also rejects missing,
  over-large, and non-object JSON POST bodies.
* SurveyStudio upload handlers now validate uploaded `.sframe` and `.csv`
  files by extension, size, and text-content checks before passing them to
  import functions.
* `read_sframe()` now validates the top-level `.sframe` payload structure
  before hash verification and object reconstruction.
* Internal HTML report generation now applies escaping consistently to
  report titles, instrument metadata, citations, and effect labels.
* Quarto report rendering now cleans temporary render directories and RDS
  files with `on.exit()` even when rendering fails.

## Documentation

* `validate_sframe()`, `score_scales()`, `codebook_report()`, `cfa_syntax()`,
  and `launch_builder(open = FALSE)` have fully runnable examples.
* Reworked the vignette set into a coherent workflow covering instrument
  building, response analysis, reliability and validity, EFA/CFA/SEM/PLS
  syntax generation, and GUI usage.
* The demo launchers (`launch_builder_demo()`, `launch_studio_demo()`, and
  `launch_dashboard_demo()`) open in the browser with the demo instrument,
  scales, and analysis plan preloaded, so no manual file loading is needed.
* The interactive package demo (`demo("survey")`) walks through the whole
  workflow with step-by-step prompts.

## Dashboard and report polish

* The dashboard parses response dates in the common formats (ISO 8601,
  date-only, UK and US day orders) instead of erroring on non-standard
  strings, colour-codes quality rows by flag status, matches the download
  button to the active theme, and draws the Items and Scales charts as soon
  as their tabs open.
* HTML report tables use APA formatting, with horizontal rules only and a
  significance footnote added automatically when a p-value column is
  present.

# surveyframe 0.1.0

* Initial release.
* Core S3 object system: `sf_instrument()`, `sf_item()`, `sf_choices()`,
  `sf_scale()`.
* Serialisation: `write_sframe()`, `read_sframe()` with SHA-256 integrity
  checking.
* Shiny survey renderer: `render_survey()`.
* Static SurveyBuilder HTML: `launch_builder()`.
* Response reader: `read_responses()`.
