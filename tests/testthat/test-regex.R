context("test-regex")

test_that("expand_matches gives data frame of indices with groups", {
  m <- regexec("(a)(b)(d)?", "abcaba")
  idx <- dplyr::data_frame(
    start = c(1L, 1L, 2L, NA_integer_),
    end   = c(3L, 2L, 3L, NA_integer_),
    group = c(0L, 1L, 2L, 3L)
  )
  expect_equal(expand_matches(m[[1]]), idx)
})

test_that("max_match_index works", {
  m <- run_regex(c("abcaba", "aba", "z"), c("(a)(b)(d)?c?"), global = TRUE)
})
