context("test-regex")

test_that("expand_matches gives data frame of indices with groups", {
  m <- regexec("(a)(b)(d)?", "abcaba")
  idx <- data.frame(
    start = c(1L, 1L, 2L, NA_integer_),
    end = c(3L, 2L, 3L, NA_integer_),
    group = c(0L, 1L, 2L, 3L),
    pass = rep(1L, 4)
  )
  expect_equal(expand_matches(m[[1]]), idx)
})

test_that("start/end indices are integers", {
  text <- "ab ab"
  pattern <- "(a)(b)"
  m <- regex(text, pattern, global = TRUE)
  expect_type(m[[1]]$idx$start, "integer")
  expect_type(m[[1]]$idx$end, "integer")
  expect_type(m[[1]]$idx$group, "integer")
})

test_that("length-zero match is NULL", {
  m <- regex(c("other", "thing"), "thing|")
  expect_null(m[[1]]$idx)
  expect_equal(m[[2]]$idx$start, 1L)
  expect_equal(m[[2]]$idx$end, 6L)
})

test_that("max_match_index works", {
  m <- regex(c("abcaba", "aba", "z"), c("(a)(b)(d)?c?"), global = FALSE)
  expect_equal(max_match_index(m), c(4, 3, NA_integer_))
})

test_that("results group (pass) is calculated correctly", {
  text <- "ab ab"
  pattern <- "(a)(b)"
  m <- regex(text, pattern, global = TRUE)
  expect_equal(unique(m[[1]]$idx$pass), c(1L, 2L))
})

test_that("view_regex generally works", {
  result <- "<p class=\"regexplain  \"> <span class=\"group g00\"><span class=\"group g01\">t</span>e</span><span class=\"group g00\"><span class=\"group g01\">s</span>t</span> </p>"
  expect_equal(view_regex("test", "(\\w)\\w", render = FALSE), result)
})
