context("test-wrap_result.R")

test_that("wrap_result handles zero length groups", {
  # Issue #9
  # Type 'q()' to quit R.
  # (?<=\()([^)]*)(?=\))
  text <- "Type 'q()' to quit R."
  pattern <- "(?<=\\()([^)]*)(?=\\))"
  res <- wrap_result(run_regex(text, pattern, perl = TRUE)[[1]])
  expect_equal(res, "Type 'q(<span class=\"group g00\"><span class=\"group g01\"></span></span>)' to quit R.")
})

test_that("wrap_results generally works", {
  text <- "apples"
  pattern <- "apples"
  res <- wrap_result(run_regex(text, pattern, perl = TRUE)[[1]])
  expect_equal(res, "<span class=\"group g00\">apples</span>")

  text <- "He wheeled the bike past the winding road."
  pattern <- "(a|the) ([^ ]+)"
  res <- wrap_result(run_regex(text, pattern, perl = TRUE)[[1]])
  expect_equal(res, "He wheeled <span class=\"group g00\"><span class=\"group g01\">the</span> <span class=\"group g02\">bike</span></span> past the winding road.")

  text <- ".15in"
  pattern <- "^(auto|inherit|((\\.\\d+)|(\\d+(\\.\\d+)?))(%|in|cm|mm|em|ex|pt|pc|px|vh|vw|vmin|vmax))$"
  res <- wrap_result(run_regex(text, pattern, perl = TRUE)[[1]])
  expect_equal(res, "<span class=\"group g00\"><span class=\"group g01\"><span class=\"group g02 pad01\"><span class=\"group g03 pad02\">.15</span></span><span class=\"group g06 pad01\">in</span></span></span>")
})

test_that("wrap_results works when groups start and end at same index", {
  text <- "7282298386"
  pattern <- "\\(?(\\d{3})[-). ]?(\\d{3})[- .]?(\\d{4})"
  res <-  wrap_result(run_regex(text, pattern, perl = TRUE)[[1]])
  expect_equal(res, "<span class=\"group g00\"><span class=\"group g01\">728</span><span class=\"group g02\">229</span><span class=\"group g03\">8386</span></span>")
})
