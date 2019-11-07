context("parameter")
library(tidydice)

test_that("roll_dice() default parameter", {
  expect_equal(roll_dice(times = 10, seed = 123),
               roll_dice(10, seed = 123))
  expect_equal(roll_dice(times = 10, seed = 123),
               roll_dice(times = 10, rounds = 1, seed = 123))
  expect_equal(roll_dice(times = 10, seed = 123),
               roll_dice(times = 10, sides = 6, seed = 123))
  expect_equal(roll_dice(times = 10, seed = 123),
               roll_dice(times = 10, success = c(6), seed = 123))
})

test_that("flip_coin() default parameter", {
  expect_equal(flip_coin(times = 10, seed = 123),
               flip_coin(10, seed = 123))
  expect_equal(flip_coin(times = 10, seed = 123),
               flip_coin(times = 10, rounds = 1, seed = 123))
  expect_equal(flip_coin(times = 10, seed = 123),
               flip_coin(times = 10, sides = 2, seed = 123))
  expect_equal(flip_coin(times = 10, seed = 123),
               flip_coin(times = 10, success = c(2), seed = 123))
})
