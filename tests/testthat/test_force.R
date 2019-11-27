context("force")
library(tidydice)

test_that("force_dice() returns correct values", {
  expect_equal(force_dice(6)$result, 6)
  expect_equal(force_dice(1:6)$result, 1:6)
  expect_equal(force_dice(1:6)$success, c(FALSE,FALSE,FALSE,FALSE,FALSE,TRUE))
  expect_equal(force_dice(1:6)$nr, 1:6)
  expect_equal(force_dice(1:6)$round, rep(1,6))
  expect_equal(force_dice(1:6)$experiment, rep(1,6))
  expect_equal(force_dice(6, experiment = 2)$experiment, 2)
  expect_equal(force_dice(6, round = 2)$round, 2)
  expect_equal(force_dice(6, success = 1)$success, FALSE)
})

test_that("force_coin() returns correct values", {
  expect_equal(force_coin(1)$result, 1)
  expect_equal(force_coin(1:2)$result, 1:2)
  expect_equal(force_coin(1:2)$success, c(FALSE,TRUE))
  expect_equal(force_coin(1:2)$nr, 1:2)
  expect_equal(force_coin(1:2)$round, rep(1,2))
  expect_equal(force_coin(1:2)$experiment, rep(1,2))
  expect_equal(force_coin(2, experiment = 2)$experiment, 2)
  expect_equal(force_coin(2, round = 2)$round, 2)
  expect_equal(force_coin(2, success = 1)$success, FALSE)
})

