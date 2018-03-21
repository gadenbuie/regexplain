context("test-sanitize_text_input.R")

test_that("doesn't treat backreferences as octals", {
  expect_equal(sanitize_text_input("\\1 \\2 \\3"), "\\1 \\2 \\3")
})

test_that("sanitizes unicode", {
  # rx_unicode <- "\\u[0-9a-f]{4,8}"
  expect_equal(sanitize_text_input("\\u2019"), "\u2019")
  expect_equal(sanitize_text_input("\\u000D"), "\r")
  expect_equal(sanitize_text_input("\\U0001F575"), "\U001F575")
})

test_that("sanitizes hex", {
  # rx_hex <- "\\\\x[0-9a-f]{2}|\\\\x\\{[0-9a-f]{1,6}\\}"
  expect_equal(sanitize_text_input("\\x0D"), "\r")
  expect_equal(sanitize_text_input("\\x{20AC}"), "\u20AC")
})

test_that("sanitizes octal", {
  # rx_octal <- "\\\\[0][0-7]{1,3}"
  expect_equal(sanitize_text_input("\\02"), "\002")
})

test_that("doesn't escape normal letters", {
  # "\\u[0-9a-f]{4,8}|\\x[0-9a-f]{2}|\\x\\{[0-9a-f]{1,6}\\}|\\N|\\0[0-8]{1,3}"
  expect_equal(sanitize_text_input("a"), "a")
  expect_equal(sanitize_text_input("\a"), "\a")
  expect_equal(sanitize_text_input("\\a"), "\\a")
  expect_equal(sanitize_text_input("x"), "x")
  #expect_error(sanitize_text_input("\x"))
  expect_equal(sanitize_text_input("\\x"), "\\x")
})
