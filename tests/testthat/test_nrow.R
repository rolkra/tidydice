context("tibble number of rows")
library(tidydice)

test_that("roll_dice returns correct number of rows", {
  expect_equal(nrow(roll_dice(times = 1)), 1)
  expect_equal(nrow(roll_dice(times = 10)), 10)
  expect_equal(nrow(roll_dice(times = 10, agg = TRUE)), 1)
  expect_equal(nrow(roll_dice(times = 1, rounds = 2)), 2)
  expect_equal(nrow(roll_dice(times = 10, rounds = 2)), 20)
  expect_equal(nrow(roll_dice(times = 10, rounds = 2, agg = TRUE)), 2)
  expect_equal(nrow(roll_dice(times = 10, rounds = 2)), 
               nrow(roll_dice(times = 2, rounds = 10)))
})

test_that("flip_coin returns correct number of rows", {
  expect_equal(nrow(flip_coin(times = 1)), 1)
  expect_equal(nrow(flip_coin(times = 10)), 10)
  expect_equal(nrow(flip_coin(times = 10, agg = TRUE)), 1)
  expect_equal(nrow(flip_coin(times = 1, rounds = 2)), 2)
  expect_equal(nrow(flip_coin(times = 10, rounds = 2)), 20)
  expect_equal(nrow(flip_coin(times = 10, rounds = 2, agg = TRUE)), 2)
  expect_equal(nrow(flip_coin(times = 10, rounds = 2)), 
               nrow(flip_coin(times = 2, rounds = 10)))
})
