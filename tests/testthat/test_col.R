context("tibble columns")
library(tidydice)

test_that("roll_dice() returns correct columns", {
  expect_equal(names(roll_dice(times = 1)), 
               c("experiment","round","nr","result","success"))
  expect_equal(is.numeric(roll_dice(times = 1)$experiment), TRUE)
  expect_equal(is.numeric(roll_dice(times = 1)$round), TRUE)
  expect_equal(is.numeric(roll_dice(times = 1)$nr), TRUE)
  expect_equal(is.numeric(roll_dice(times = 1)$result), TRUE)
  expect_equal(is.logical(roll_dice(times = 1)$success), TRUE)

  expect_equal(names(roll_dice(times = 10, agg = TRUE)), 
               c("experiment","round","times","success"))
  expect_equal(is.numeric(roll_dice(times = 10, agg = TRUE)$experiment), TRUE)
  expect_equal(is.numeric(roll_dice(times = 10, agg = TRUE)$round), TRUE)
  expect_equal(is.numeric(roll_dice(times = 10, agg = TRUE)$times), TRUE)
  expect_equal(is.numeric(roll_dice(times = 10, agg = TRUE)$success), TRUE)
  })

test_that("flip_coin() returns correct columns", {
  expect_equal(names(roll_dice(times = 1)), 
               c("experiment","round","nr","result","success"))
  expect_equal(is.numeric(flip_coin(times = 1)$experiment), TRUE)
  expect_equal(is.numeric(flip_coin(times = 1)$round), TRUE)
  expect_equal(is.numeric(flip_coin(times = 1)$nr), TRUE)
  expect_equal(is.numeric(flip_coin(times = 1)$result), TRUE)
  expect_equal(is.logical(flip_coin(times = 1)$success), TRUE)

  expect_equal(names(roll_dice(times = 10, agg = TRUE)), 
               c("experiment","round","times","success"))
  expect_equal(is.numeric(flip_coin(times = 10, agg = TRUE)$experiment), TRUE)
  expect_equal(is.numeric(flip_coin(times = 10, agg = TRUE)$round), TRUE)
  expect_equal(is.numeric(flip_coin(times = 10, agg = TRUE)$times), TRUE)
  expect_equal(is.numeric(flip_coin(times = 10, agg = TRUE)$success), TRUE)
})
