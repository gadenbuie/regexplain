context("test-wrap_regex.R")

test_that("wrap_regex generally works", {
  expect_equal(wrap_regex("(a)(b)"),
               "<span class=\"g01\">(a)</span><span class=\"g02\">(b)</span>")
})

test_that("wrap_regex doesn't add parens", {
  expect_equal(wrap_regex("\\ba"), "\\ba")
})
