context("test-wrap_result.R")

test_that("wrap_result handles zero length groups", {
  # Issue #9
  # Type 'q()' to quit R.
  # (?<=\()([^)]*)(?=\))
  text <- "Type 'q()' to quit R."
  pattern <- "(?<=\\()([^)]*)(?=\\))"
  res <- wrap_result(regex(text, pattern, perl = TRUE, global = FALSE)[[1]])
  expect_equal(res, "Type 'q(<span class=\"group g00\"><span class=\"group g01\"></span></span>)' to quit R.")
})

test_that("wrap_results generally works", {
  text <- "apples"
  pattern <- "apples"
  res <- wrap_result(regex(text, pattern, perl = TRUE, global = FALSE)[[1]])
  expect_equal(res, "<span class=\"group g00\">apples</span>")

  text <- "He wheeled the bike past the winding road."
  pattern <- "(a|the) ([^ ]+)"
  res <- wrap_result(regex(text, pattern, perl = TRUE, global = FALSE)[[1]])
  expect_equal(res, "He wheeled <span class=\"group g00\"><span class=\"group g01\">the</span> <span class=\"group g02\">bike</span></span> past the winding road.")

  text <- ".15in"
  pattern <- "^(auto|inherit|((\\.\\d+)|(\\d+(\\.\\d+)?))(%|in|cm|mm|em|ex|pt|pc|px|vh|vw|vmin|vmax))$"
  res <- wrap_result(regex(text, pattern, perl = TRUE, global = FALSE)[[1]])
  expect_equal(res, "<span class=\"group g00\"><span class=\"group g01\"><span class=\"group g02 pad01\"><span class=\"group g03 pad02\">.15</span></span><span class=\"group g06 pad01\">in</span></span></span>")
})

test_that("wrap_results works when groups start and end at same index", {
  text <- "7282298386"
  pattern <- "\\(?(\\d{3})[-). ]?(\\d{3})[- .]?(\\d{4})"
  res <-  wrap_result(regex(text, pattern, perl = TRUE, global = FALSE)[[1]])
  expect_equal(res, "<span class=\"group g00\"><span class=\"group g01\">728</span><span class=\"group g02\">229</span><span class=\"group g03\">8386</span></span>")
})

test_that("wrap_result searches globally", {
  text <- "ab ab"
  pattern <- "(a)(b)"
  result <- paste(rep("<span class=\"group g00\"><span class=\"group g01\">a</span><span class=\"group g02\">b</span></span>", 2), collapse = " ")
  expect_equal(wrap_result(regex(text, pattern, global = TRUE)[[1]]), result)
})

test_that("wrap_result starts/ends correctly with touching groups", {
  text <- "The big red apple fell to the ground."
  pattern <- "(\\w+) (\\w+) "
  result <- paste0(
    '<span class=\"group g00\"><span class=\"group g01\">The</span> <span class=\"group g02\">big</span> </span>',
    '<span class=\"group g00\"><span class=\"group g01\">red</span> <span class=\"group g02\">apple</span> </span>',
    '<span class=\"group g00\"><span class=\"group g01\">fell</span> <span class=\"group g02\">to</span> </span>',
    'the ground.'
  )
  expect_equal(wrap_result(regex(text, pattern, global = TRUE)[[1]]), result)
})
