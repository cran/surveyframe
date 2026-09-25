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
# A canonical fingerprint of one amendment entry, over every field it records.
# The boundary used to compare each entry's new_hash alone, which covers the
# content the amendment produced and says nothing about the disclosure itself,
# so an entry's author, reason, tier or sign-off could be rewritten afterwards
# and the log still passed as intact.
sframe_amendment_entry_hash <- function(entry) {
  if (!is.list(entry)) return("")
  entry <- entry[order(names(entry))]
  as.character(openssl::sha256(sframe_hash_json(entry, canonical = TRUE)))
}

sframe_amendment_entry_hashes <- function(amendments) {
  vapply(amendments %||% list(), sframe_amendment_entry_hash, character(1))
}

sframe_content_hash <- function(instrument, box = TRUE) {
  payload <- sframe_serialization_payload(instrument, box = box)
  payload$hash <- NULL
  payload$amendments <- NULL
  as.character(openssl::sha256(sframe_hash_json(payload, canonical = TRUE)))
}

# The content fingerprints an earlier version could have recorded for the same
# content: before 0.4.2 one-member collections were serialised as scalars, and
# a fingerprint could be taken before validation set meta$validated. Used only
# to recognise an existing log, so older files are not refused.
sframe_content_hash_candidates <- function(instrument) {
  out <- character(0)
  for (box in c(TRUE, FALSE)) {
    for (v in list("keep", "absent", FALSE, TRUE)) {
      ins <- instrument
      if (identical(v, "absent")) ins$meta$validated <- NULL
      else if (!identical(v, "keep")) ins$meta$validated <- v
      out <- c(out, sframe_content_hash(ins, box = box))
    }
  }
  unique(out)
}

# The amendment checks write_sframe() applies. An amendment log is only worth
# something if it cannot be quietly bypassed, shortened or reordered, so:
# entries must follow one another, the content must match the last entry, and
# an instrument read from a file must keep that file's entries and record any
# change to its content. Writing changed content as a new instrument is an
# explicit declaration.
sframe_check_amendment_boundary <- function(instrument, new_instrument = FALSE) {
  abort <- function(msg) rlang::abort(msg, class = c("sframe_validation_error", "sframe_error"))
  am <- instrument$amendments %||% list()
  origin <- attr(instrument, "sframe_origin")

  if (length(am) > 1) {
    for (i in 2:length(am)) {
      if (!identical(am[[i]]$previous_hash, am[[i - 1]]$new_hash)) {
        abort(sprintf(paste0(
          "The amendment log is out of sequence at entry %d: it records a ",
          "previous state that entry %d did not produce. Entries may have been ",
          "removed or reordered."), i, i - 1))
      }
      if (isTRUE(am[[i]]$timestamp < am[[i - 1]]$timestamp)) {
        abort(sprintf("The amendment log timestamps run backwards at entry %d.", i))
      }
    }
  }

  if (isTRUE(new_instrument)) {
    if (length(am) > 0) {
      abort(paste0(
        "new_instrument = TRUE declares a new instrument, which cannot carry ",
        "another instrument's amendment log. Clear it first with ",
        "instrument$amendments <- list()."))
    }
    return(invisible(TRUE))
  }

  current <- sframe_content_hash(instrument)
  if (length(am) > 0) {
    last <- am[[length(am)]]$new_hash %||% ""
    if (!identical(last, current) && !last %in% sframe_content_hash_candidates(instrument)) {
      abort(paste0(
        "The instrument has changed since its last recorded amendment. Record ",
        "the change with amend_sframe(), or write it as a new instrument with ",
        "new_instrument = TRUE."))
    }
  }

  if (!is.null(origin)) {
    # Compared over whole entries, so editing an existing amendment's author,
    # reason, tier or sign-off is caught. Comparing new_hash alone left every
    # field of the disclosure rewritable after the fact.
    loaded <- origin$amendment_entry_hashes %||%
      (origin$amendment_new_hashes %||% character(0))
    now <- if (is.null(origin$amendment_entry_hashes)) {
      vapply(am, function(a) a$new_hash %||% "", character(1))
    } else {
      sframe_amendment_entry_hashes(am)
    }
    if (length(now) < length(loaded) || !identical(now[seq_along(loaded)], loaded)) {
      abort(sprintf(paste0(
        "The amendment log read from '%s' has been shortened or altered. ",
        "An amendment log only grows, and a recorded entry stays as it was ",
        "written."), origin$path))
    }
    if (!identical(current, origin$content_hash) && length(now) == length(loaded)) {
      abort(sprintf(paste0(
        "This instrument was read from '%s' and its content has changed with ",
        "no amendment recorded. Record the change with amend_sframe(), or write ",
        "it as a new instrument with new_instrument = TRUE."), origin$path))
    }
  }
  invisible(TRUE)
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
#' changed and why: a data-entry correction, bot-response removal, or a
#' documented model respecification. The record is kept inside the file,
#' beside the content it explains.
#'
#' [write_sframe()] refuses to write an instrument read from a file whose
#' content has changed with no amendment recorded, one that changed after its
#' last amendment, and one whose amendment log was shortened or reordered.
#' [read_sframe()] refuses a file edited on disk without its hash being
#' recomputed. These checks are local and the hashes are unsigned: someone
#' who rewrites both a file and its log, and recomputes the hashes, is not
#' detected, and nothing here establishes who made a change or when.
#'
#' Amendments come in two tiers. A `"pipeline"` amendment (data corrections,
#' bot removal) needs only a reason. A `"design"` amendment also requires a
#' `deviation_report` describing what changed in the research question,
#' method or model, and why. The tier follows the change itself: an amendment
#' that changes the analysis plan, a model or a conjoint design is always
#' design tier, whatever `reason_code` or `tier` says. `second_signoff` is
#' optional. When omitted, the entry records `signoff = "none"`, so the
#' absence of a named reviewer is visible. The tier, report and signoff are
#' what the author records. None of them is independent approval.
#'
#' `previous_hash` and `new_hash` on each entry are a content fingerprint: a
#' SHA-256 over a canonical serialisation of the instrument, with the `hash`
#' and `amendments` fields excluded, taken after validation, so `new_hash` is
#' the content that is written. It is distinct from the file's own integrity
#' hash from [write_sframe()], which also covers the amendment log. Both
#' identify content. Neither is byte identity.
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
#'   from `reason_code`: `data_correction` and `bot_removal` default to
#'   `"pipeline"`, and everything else to `"design"`. A change to the
#'   analysis plan, a model or a conjoint design is always `"design"`, and
#'   asking for `"pipeline"` on one is an error.
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

  tier_requested <- !is.null(tier)
  if (is.null(tier)) {
    tier <- sframe_amendment_default_tier(reason_code)
  } else {
    tier <- rlang::arg_match(tier, sframe_amendment_tiers)
  }

  # Fingerprints and the diff are taken on validated states, since validation
  # sets meta$validated and write_sframe() writes the validated state. Taken
  # before validation, new_hash described content that was never written.
  previous_v <- as_sframe(validate_sframe(previous, strict = TRUE))
  instrument_v <- as_sframe(validate_sframe(instrument, strict = TRUE))
  prev_payload <- sframe_serialization_payload(previous_v)
  new_payload  <- sframe_serialization_payload(instrument_v)
  compare_keys <- setdiff(union(names(prev_payload), names(new_payload)),
                          c("hash", "amendments"))
  changed_fields <- Filter(
    function(k) !identical(prev_payload[[k]], new_payload[[k]]),
    compare_keys
  )

  # The tier follows what changed. A plan, model or design change is design
  # tier, whatever the caller asked for, since that is the change the
  # declared-before-collection contract exists to expose.
  design_changes <- intersect(unlist(changed_fields), c("analysis_plan", "models", "designs"))
  if (length(design_changes) > 0) {
    if (tier_requested && identical(tier, "pipeline")) {
      rlang::abort(
        sprintf(paste0("This amendment changes %s, which makes it a design-tier ",
                       "amendment. It cannot be recorded as \"pipeline\"."),
                paste(design_changes, collapse = " and ")),
        class = c("sframe_validation_error", "sframe_error"))
    }
    tier <- "design"
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
      previous_hash = sframe_content_hash(previous_v),
      new_hash = sframe_content_hash(instrument_v),
      changed_fields = as.character(changed_fields)
    ),
    class = "sf_amendment"
  )

  # Base the appended log on previous$amendments, not instrument$amendments.
  # `instrument` is the "after" object a caller supplies; nothing guarantees
  # it already carries previous's full history forward (a builder or export
  # round trip unaware of this field could easily drop it), and this is an
  # audit log, so a caller's incomplete "after" state must never truncate it.
  amended <- instrument
  amended$amendments <- c(previous$amendments %||% list(), list(entry))

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
