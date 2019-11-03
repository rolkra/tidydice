context("success")
library(tidydice)

test_that("roll_dice() returns correct success", {
  expect_equal(sum(roll_dice(times = 10, success = c(1:6))$success), 10)
  expect_equal(sum(roll_dice(times = 10, success = c(0))$success), 0)
  expect_equal(roll_dice(times = 10, success = c(1:6), agg = TRUE)$success, 10)
  expect_equal(roll_dice(times = 10, success = c(0), agg = TRUE)$success, 0)
})

test_that("flip_coin() returns correct success", {
  expect_equal(sum(flip_coin(times = 10, success = c(1:6))$success), 10)
  expect_equal(sum(flip_coin(times = 10, success = c(0))$success), 0)
  expect_equal(flip_coin(times = 10, success = c(1:6), agg = TRUE)$success, 10)
  expect_equal(flip_coin(times = 10, success = c(0), agg = TRUE)$success, 0)
})
