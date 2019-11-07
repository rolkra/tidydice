context("unusual")
library(tidydice)

test_that("roll_dice() unusual arguments", {
  expect_error(roll_dice(0))
  expect_error(roll_dice(times = 0))
  expect_error(roll_dice(rounds = 0))
  expect_error(roll_dice(sides = 0))
})

test_that("roll_dice() unusual arguments", {
  expect_error(flip_coin(0))
  expect_error(flip_coin(times = 0))
  expect_error(flip_coin(rounds = 0))
  expect_error(flip_coin(sides = 0))
})
