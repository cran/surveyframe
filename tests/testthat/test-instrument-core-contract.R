# Batch 1's correctness findings on the instrument core.
#
# #3. A failed non-strict validation left meta$validated = TRUE on the
#     instrument it carried, so the diagnostic said invalid while the object
#     printed as valid. Changing the plan left the stamp in place too.
# #4. Plan validation read references without establishing that a block was a
#     plan block, so analysis_plan = list(list()) validated clean.
# #5. Malformed scalar fields reached ordinary R failures before the
#     diagnostic was built, so strict = FALSE stopped where it promised to
#     report.

one_item <- function() {
  sf_instrument("Core", components = list(
    sf_item("q1", "How satisfied are you?", type = "numeric")
  ))
}

test_that("3: a failed validation clears the validated stamp", {
  instr <- as_sframe(validate_sframe(one_item(), strict = TRUE))
  expect_true(isTRUE(sf_meta(instr)$validated))

  sf_plan(instr) <- list(
    list(id = "RQ1", research_question = "What is the average?",
         family = "descriptive", method = "descriptives",
         roles = list(variables = "nowhere"))
  )
  # the setter alone invalidates the stamp, since the content it validated
  # has changed
  expect_false(isTRUE(sf_meta(instr)$validated))

  res <- validate_sframe(instr, strict = FALSE)
  expect_false(res$valid)
  expect_false(isTRUE(sf_meta(sf_object(res))$validated))
  expect_false(isTRUE(sf_meta(as_sframe(res))$validated))
})

test_that("3: validation clears a stamp it finds already set", {
  # reaching the stamp without the setter, which is the case the setter's own
  # fix would otherwise hide
  instr <- as_sframe(validate_sframe(one_item(), strict = TRUE))
  instr$items[[1]]$scale_id <- "no_such_scale"
  expect_true(isTRUE(sf_meta(instr)$validated))

  res <- validate_sframe(instr, strict = FALSE)
  expect_false(res$valid)
  expect_false(isTRUE(sf_meta(sf_object(res))$validated))
})

test_that("3: the print status follows the diagnostic", {
  instr <- as_sframe(validate_sframe(one_item(), strict = TRUE))
  sf_plan(instr) <- list(list(id = "RQ1", method = "descriptives",
                              roles = list(variables = "nowhere")))
  out <- paste(capture.output(print(instr)), collapse = " ")
  expect_false(grepl("\\bvalid\\b", out))
})

test_that("4: a malformed plan block is a validation problem", {
  instr <- one_item()
  instr$analysis_plan <- list(list())
  res <- validate_sframe(instr, strict = FALSE)
  expect_false(res$valid)
  expect_match(paste(sf_problems(res), collapse = " "), "[Aa]nalysis plan")

  # a role container that is a bare string bypassed reference checking
  instr2 <- one_item()
  instr2$analysis_plan <- list(list(id = "RQ1", method = "descriptives",
                                    roles = "nowhere"))
  res2 <- validate_sframe(instr2, strict = FALSE)
  expect_false(res2$valid)

  # an unknown method is a problem, where the vocabulary went unchecked
  instr3 <- one_item()
  instr3$analysis_plan <- list(list(id = "RQ1", method = "telepathy",
                                    roles = list(variables = "q1")))
  res3 <- validate_sframe(instr3, strict = FALSE)
  expect_false(res3$valid)

  # duplicate block IDs
  block <- list(id = "RQ1", method = "descriptives",
                roles = list(variables = "q1"))
  instr4 <- one_item()
  instr4$analysis_plan <- list(block, block)
  res4 <- validate_sframe(instr4, strict = FALSE)
  expect_false(res4$valid)

  # a plan of the documented legacy shape still validates
  instr5 <- one_item()
  instr5$analysis_plan <- list(list(id = "RQ1", method = "descriptives",
                                    variables = "q1"))
  expect_true(validate_sframe(instr5, strict = FALSE)$valid)
})

test_that("5: malformed scalar fields are reported, never raised", {
  # an empty ID reached a vapply() length mismatch
  instr <- one_item()
  instr$items[[1]]$id <- character(0)
  res <- expect_no_error(validate_sframe(instr, strict = FALSE))
  expect_false(res$valid)

  # a missing label reached an indeterminate if() condition
  instr2 <- one_item()
  instr2$items[[1]]$label <- NA_character_
  res2 <- expect_no_error(validate_sframe(instr2, strict = FALSE))
  expect_false(res2$valid)

  # a vector version survived validation and failed when the diagnostic printed
  instr3 <- one_item()
  instr3$meta$version <- c("1.0", "2.0")
  res3 <- expect_no_error(validate_sframe(instr3, strict = FALSE))
  expect_false(res3$valid)
  expect_no_error(capture.output(print(res3)))
})
