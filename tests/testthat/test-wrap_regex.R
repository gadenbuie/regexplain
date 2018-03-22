context("test-wrap_regex.R")

test_that("wrap_regex generally works", {
  expect_equal(wrap_regex("(a)(b)"),
               "<span class=\"g01\">(a)</span><span class=\"g02\">(b)</span>")
})

test_that("wrap_regex doesn't add parens", {
  expect_equal(wrap_regex("\\ba", exact = FALSE), "\\ba")
})

test_that("wrap_regex doesn't wrap non-capture groups", {
  expect_equal(wrap_regex("(?:a)(b)"), "(?:a)<span class=\"g01\">(b)</span>")
  expect_equal(wrap_regex("((?:a(b))c)"),
               "<span class=\"g01\">((?:a<span class=\"g02\">(b)</span>)c)</span>")
})
