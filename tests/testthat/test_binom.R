context("binom")
library(tidydice)

test_that("binom_dice() returns correct values", {
  expect_equal(names(binom_dice(times = 6)), c("success", "p", "pct"))
  expect_equal(is.numeric(binom_dice(times = 6)$success), TRUE)
  expect_equal(is.numeric(binom_dice(times = 6)$p), TRUE)
  expect_equal(is.numeric(binom_dice(times = 6)$pct), TRUE)
  expect_equal(nrow(binom_dice(times = 6)), 6)
  expect_equal(binom_dice(times = 6)$success, 1:6)
})

test_that("binom_coin() returns correct values", {
  expect_equal(names(binom_coin(times = 6)), c("success", "p", "pct"))
  expect_equal(is.numeric(binom_coin(times = 6)$success), TRUE)
  expect_equal(is.numeric(binom_coin(times = 6)$p), TRUE)
  expect_equal(is.numeric(binom_coin(times = 6)$pct), TRUE)
  expect_equal(nrow(binom_coin(times = 6)), 6)
  expect_equal(binom_coin(times = 6)$success, 1:6)
})

