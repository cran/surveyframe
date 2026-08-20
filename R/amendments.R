# amendments.R
#
# A disclosed-revision path around read_sframe()'s hash check. Without this,
# any edit to an instrument -- a legitimate data-cleaning correction as much
# as an undisclosed change -- breaks the file's hash identically, and the
# only record of "why" lives outside the file (an email, a lab notebook, a
# memory). amend_sframe() makes the "why" part of the file itself: every
# amendment is appended to an ordered log, never overwritten, so the full
# revision history survives in the .sframe file alongside the content it
# describes. The hash still catches an undisclosed edit exactly as before --
# this adds a disclosed path next to it, it does not weaken the check.
#
# Two tiers, because not every amendment carries the same risk. A dataset or
# collection-pipeline correction ("bot_removal", "data_correction") is
# expected researcher behaviour and stays low-friction. A change to the
# analysis plan or the measurement/structural model after data collection has
# begun is exactly the behaviour the design-time plan binding
# (sf_instrument()'s analysis_plan slot, run_analysis_plan()) exists to
# guard against, so it is disclosed but made more effortful: a mandatory
# `deviation_report`, and a `signoff` field that is never silently blank --
# it explicitly records "none" when no second reviewer is named, so the gap
# is visible to an auditor rather than indistinguishable from a signed-off
# change.

sframe_amendment_reason_codes <- c(
  "data_correction", "bot_removal", "model_respecification",
  "instrument_revision", "other"
)

sframe_amendment_tiers <- c("pipeline", "design")

# reason_code -> default tier. data_correction/bot_removal are ordinary
# pipeline hygiene; model_respecification is design-level by construction;
# instrument_revision and other default to design-level too, since a
# generic instrument edit or an unclassified reason should get the more
# careful path unless the caller explicitly asks for "pipeline".
sframe_amendment_default_tier <- function(reason_code) {
  if (reason_code %in% c("data_correction", "bot_removal")) "pipeline" else "design"
}

# Content-only hash: the same canonical-JSON SHA-256 sframe_hash_value() uses,
# but computed over the payload with `hash` AND `amendments` excluded. This
# sidesteps the self-reference a file-level hash would create (an amendment
# entry cannot record the hash of a payload that includes that entry's own
# hash field) while still giving a well-defined, independently reproducible
# digest of "what the substantive instrument content was at this point" --
# items, choices, scales, branching, checks, analysis plan, models, designs.
# Documented as distinct from write_sframe()'s file-level hash in
# amend_sframe()'s and amendment_log()'s roxygen so the two are never
# confused: `previous_hash`/`new_hash` in an amendment entry are a content
# fingerprint, not the .sframe file's own integrity hash.
sframe_content_hash <- function(instrument) {
  payload <- sframe_serialization_payload(instrument)
  payload$hash <- NULL
  payload$amendments <- NULL
  as.character(openssl::sha256(sframe_hash_json(payload, canonical = TRUE)))
}

sframe_restore_amendment <- function(a) {
  a$timestamp <- as.character(a$timestamp %||% "")
  a$reason_code <- as.character(a$reason_code %||% "other")
  a$reason_text <- as.character(a$reason_text %||% "")
  a$tier <- as.character(a$tier %||% "pipeline")
  a$author <- sframe_empty_to_null(a$author)
  a$deviation_report <- sframe_empty_to_null(a$deviation_report)
  a$signoff <- as.character(a$signoff %||% "none")
  a$previous_hash <- as.character(a$previous_hash %||% "")
  a$new_hash <- as.character(a$new_hash %||% "")
  a$changed_fields <- sframe_as_vector(
    sframe_empty_to_null(a$changed_fields), "character"
  ) %||% character(0)
  class(a) <- "sf_amendment"
  a
}

sframe_amendment_plain <- function(a) {
  out <- unclass(a)
  out$author <- sframe_empty_to_null(out$author)
  out$deviation_report <- sframe_empty_to_null(out$deviation_report)
  out
}

#' Record a disclosed amendment to an instrument
#'
#' Appends a structured, disclosed-revision entry to an instrument's
#' amendment log, comparing `previous` against `instrument` to record what
#' changed and why. This is the path around [read_sframe()]'s hash check for
#' *legitimate* revision: a data-entry correction, bot-response removal, or a
#' documented model respecification. It does not weaken that check --
#' [read_sframe()] still hard-aborts on any edit that never went through
#' `amend_sframe()`. What it adds is a place for a disclosed change to be
#' recorded inside the file itself, alongside the content it explains,
#' rather than only in an email or a lab notebook.
#'
#' Amendments come in two tiers. A `"pipeline"` amendment (data corrections,
#' bot removal) is expected researcher behaviour and needs only a reason. A
#' `"design"` amendment (anything touching the analysis plan or a measurement
#' or structural model) is exactly the kind of post-hoc change the
#' design-time analysis plan exists to guard against, so it additionally
#' requires a `deviation_report` describing what changed in the research
#' question, method, or model and why. `second_signoff` is optional at
#' either tier; when omitted, the log entry records `signoff = "none"`
#' rather than leaving the field blank, so the absence of independent
#' review is visible to anyone auditing the log later.
#'
#' `previous_hash` and `new_hash` on each entry are a **content** fingerprint
#' (a SHA-256 over the instrument's substantive fields -- items, choices,
#' scales, branching, checks, analysis plan, models, designs -- with the
#' `hash` and `amendments` fields themselves excluded), not the `.sframe`
#' file's own integrity hash from [write_sframe()]. The two serve different
#' purposes: the file hash (via [read_sframe()]) proves the file on disk is
#' byte-identical to what was written; an amendment's content hash proves
#' what the instrument's substance was immediately before and after this
#' specific, disclosed change.
#'
#' @param previous An `sframe` object: the instrument's state before this
#'   amendment.
#' @param instrument An `sframe` object: the instrument's state after the
#'   change this call discloses.
#' @param reason_code One of `"data_correction"`, `"bot_removal"`,
#'   `"model_respecification"`, `"instrument_revision"`, `"other"`.
#' @param reason_text Character. A free-text explanation. Required and must
#'   be non-empty regardless of `reason_code`.
#' @param tier `"pipeline"` or `"design"`. When `NULL` (the default), inferred
#'   from `reason_code`: `data_correction`/`bot_removal` default to
#'   `"pipeline"`; everything else defaults to `"design"`. Pass explicitly to
#'   override the default in either direction.
#' @param author Character or `NULL`. Who made the change.
#' @param deviation_report Character or `NULL`. Required when `tier` is
#'   `"design"`: what changed in the research question, method, or model, and
#'   why. Ignored (may be `NULL`) for `"pipeline"` amendments.
#' @param second_signoff Character or `NULL`. A second reviewer's name or
#'   identifier (an ethics board reference, a co-author). When omitted, the
#'   entry records `signoff = "none"`.
#'
#' @return The amended `sframe` object, with the new entry appended to its
#'   amendment log. Call [write_sframe()] to persist it.
#' @export
#' @seealso [amendment_log()], [write_sframe()], [read_sframe()]
#'
#' @examples
#' item  <- sf_item("q1", "How satisfied are you?", type = "text")
#' instr <- sf_instrument("Demo", components = list(item))
#' item2 <- sf_item("q1", "How satisfied are you overall?", type = "text")
#' revised <- sf_instrument("Demo", components = list(item2))
#' amended <- amend_sframe(
#'   instr, revised,
#'   reason_code = "instrument_revision",
#'   reason_text = "Clarified item wording after a pilot round.",
#'   deviation_report = "Wording only; no change to the construct measured."
#' )
#' nrow(amendment_log(amended))
amend_sframe <- function(previous, instrument, reason_code, reason_text,
                         tier = NULL, author = NULL, deviation_report = NULL,
                         second_signoff = NULL) {
  sframe_check_instrument(previous, arg = "previous")
  sframe_check_instrument(instrument, arg = "instrument")

  reason_code <- rlang::arg_match(reason_code, sframe_amendment_reason_codes)

  if (!is.character(reason_text) || length(reason_text) != 1 ||
      !nzchar(trimws(reason_text))) {
    rlang::abort(
      "`reason_text` must be a non-empty character string.",
      class = c("sframe_validation_error", "sframe_error")
    )
  }

  if (is.null(tier)) {
    tier <- sframe_amendment_default_tier(reason_code)
  } else {
    tier <- rlang::arg_match(tier, sframe_amendment_tiers)
  }

  if (identical(tier, "design") &&
      (is.null(deviation_report) || !nzchar(trimws(deviation_report)))) {
    rlang::abort(
      paste0(
        "A \"design\"-tier amendment requires `deviation_report`: describe ",
        "what changed in the research question, method, or model, and why. ",
        "Pipeline-tier amendments (data_correction, bot_removal) do not ",
        "need one."
      ),
      class = c("sframe_validation_error", "sframe_error")
    )
  }

  prev_payload <- sframe_serialization_payload(previous)
  new_payload  <- sframe_serialization_payload(instrument)
  compare_keys <- setdiff(union(names(prev_payload), names(new_payload)),
                          c("hash", "amendments"))
  changed_fields <- Filter(
    function(k) !identical(prev_payload[[k]], new_payload[[k]]),
    compare_keys
  )

  signoff <- if (!is.null(second_signoff) && nzchar(trimws(second_signoff))) {
    trimws(second_signoff)
  } else {
    "none"
  }

  entry <- structure(
    list(
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      reason_code = reason_code,
      reason_text = trimws(reason_text),
      tier = tier,
      author = author,
      deviation_report = deviation_report,
      signoff = signoff,
      previous_hash = sframe_content_hash(previous),
      new_hash = sframe_content_hash(instrument),
      changed_fields = as.character(changed_fields)
    ),
    class = "sf_amendment"
  )

  amended <- instrument
  amended$amendments <- c(amended$amendments %||% list(), list(entry))

  as_sframe(validate_sframe(amended, strict = TRUE))
}

#' Read an instrument's amendment log
#'
#' Returns the disclosed-amendment history recorded by [amend_sframe()] as a
#' data frame, one row per amendment in the order they were recorded.
#'
#' @param instrument An `sframe` object.
#'
#' @return A data frame with columns `timestamp`, `reason_code`,
#'   `reason_text`, `tier`, `author`, `deviation_report`, `signoff`,
#'   `previous_hash`, `new_hash`, and `changed_fields` (a comma-joined
#'   string). Zero rows if the instrument has no recorded amendments.
#'   Export with `write.csv()` for an external audit trail.
#' @export
#' @seealso [amend_sframe()]
#'
#' @examples
#' item  <- sf_item("q1", "How satisfied are you?", type = "text")
#' instr <- sf_instrument("Demo", components = list(item))
#' amendment_log(instr)
amendment_log <- function(instrument) {
  sframe_check_instrument(instrument)
  amendments <- instrument$amendments %||% list()

  if (length(amendments) == 0) {
    return(data.frame(
      timestamp = character(0), reason_code = character(0),
      reason_text = character(0), tier = character(0),
      author = character(0), deviation_report = character(0),
      signoff = character(0), previous_hash = character(0),
      new_hash = character(0), changed_fields = character(0),
      stringsAsFactors = FALSE
    ))
  }

  col <- function(field, default = NA_character_) {
    vapply(amendments, function(a) {
      v <- a[[field]]
      if (is.null(v) || !length(v)) default else as.character(v)[1]
    }, character(1))
  }

  data.frame(
    timestamp = col("timestamp"),
    reason_code = col("reason_code"),
    reason_text = col("reason_text"),
    tier = col("tier"),
    author = col("author"),
    deviation_report = col("deviation_report"),
    signoff = col("signoff"),
    previous_hash = col("previous_hash"),
    new_hash = col("new_hash"),
    changed_fields = vapply(amendments, function(a) {
      paste(a$changed_fields %||% character(0), collapse = ", ")
    }, character(1)),
    stringsAsFactors = FALSE
  )
}
