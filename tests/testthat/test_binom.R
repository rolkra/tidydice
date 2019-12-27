context("binom")
library(tidydice)

test_that("binom_dice() returns correct values", {
  expect_equal(names(binom_dice(times = 6)), c("success", "p", "pct"))
  expect_equal(is.numeric(binom_dice(times = 6)$success), TRUE)
  expect_equal(is.numeric(binom_dice(times = 6)$p), TRUE)
  expect_equal(is.numeric(binom_dice(times = 6)$pct), TRUE)
  expect_equal(nrow(binom_dice(times = 6)), 7)
  expect_equal(binom_dice(times = 6)$success, 0:6)
  expect_equal(sum(binom_dice(times = 6)$p), 1, tolerance = 0.0001)
  expect_equal(sum(binom_dice(times = 6)$pct), 100, tolerance = 0.01)
  expect_equal(binom_dice(times = 2)$pct, c(69.4,27.8,2.78), tolerance = 0.01)
})

test_that("binom_coin() returns correct values", {
  expect_equal(names(binom_coin(times = 6)), c("success", "p", "pct"))
  expect_equal(is.numeric(binom_coin(times = 6)$success), TRUE)
  expect_equal(is.numeric(binom_coin(times = 6)$p), TRUE)
  expect_equal(is.numeric(binom_coin(times = 6)$pct), TRUE)
  expect_equal(nrow(binom_coin(times = 6)), 7)
  expect_equal(binom_coin(times = 6)$success, 0:6)
  expect_equal(sum(binom_dice(times = 6)$p), 1, tolerance = 0.0001)
  expect_equal(sum(binom_dice(times = 6)$pct), 100, tolerance = 0.01)
  expect_equal(binom_coin(times = 2)$pct, c(25,50,25), tolerance = 0.01)
})

test_that("binom() returns correct values", {
  expect_equal(binom(times = 6, prob_success = 1/6), binom_dice(times = 6))
  expect_equal(binom(times = 6, prob_success = 1/2), binom_coin(times = 6))
  expect_equal(binom(times = 2, prob_success = 1/6)$pct, c(69.4,27.8,2.78), tolerance = 0.01)
  expect_equal(binom(times = 2, prob_success = 1/2)$pct, c(25,50,25), tolerance = 0.01)
})
