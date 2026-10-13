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

test_that("binom_dice() and binom_coin() with unfair dice", {
  # fair dice are the default
  expect_equal(binom_dice(times = 4, prob = rep(1, 6)), binom_dice(times = 4))
  # the weights do not need to sum to 1
  expect_equal(binom_dice(times = 4, prob = c(1, 1, 1, 1, 1, 2)), 
               binom(times = 4, prob_success = 2 / 7))
  expect_equal(binom_dice(times = 4, prob = c(1, 1, 1, 1, 1, 2), success = c(5, 6)), 
               binom(times = 4, prob_success = 3 / 7))
  expect_equal(binom_coin(times = 5, prob = c(0.3, 0.7)), 
               binom(times = 5, prob_success = 0.7))
  expect_equal(binom_coin(times = 5, success = 1, prob = c(0.3, 0.7)), 
               binom(times = 5, prob_success = 0.3))
  
  # same result as simulating the dice
  p <- binom_dice(times = 3, sides = 3, success = 3, prob = c(0, 1, 1))$p
  sims <- replicate(4000, sum(roll_dice(times = 3, sides = 3, prob = c(0, 1, 1))$result == 3))
  expect_equal(as.numeric(table(factor(sims, levels = 0:3))) / 4000, p, tolerance = 0.05)
  
  # sides that do not exist, duplicates and 0 are not successes
  expect_equal(binom_dice(times = 3, success = c(6, 6, 0, 7)), binom_dice(times = 3))
  
  expect_error(binom_dice(times = 3, prob = c(1, 1)), "probability for each side")
  expect_error(binom_dice(times = 3, prob = c(-1, rep(1, 5))), "must be positive")
  expect_error(binom_dice(times = 3, prob = rep(0, 6)), "must be positive")
  expect_error(binom_dice(times = 3, prob = "a"), "must be numeric")
})


test_that("binomial weights must be finite and normalize without overflow", {
  for (bad in c(Inf, -Inf, NA_real_, NaN)) {
    expect_error(binom_dice(2, prob = c(rep(1, 5), bad)), "must be positive")
    expect_error(binom_coin(2, prob = c(1, bad)), "must be positive")
  }
  expect_equal(binom_coin(2, prob = c(1e308, 1e308)), binom_coin(2))
})
