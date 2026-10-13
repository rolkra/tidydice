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

test_that("the experiment of roll_dice() and flip_coin() counts up", {
  expect_equal(unique(roll_dice()$experiment), 1)
  expect_equal(unique(roll_dice(3)$experiment), 1)
  expect_equal(unique(flip_coin()$experiment), 1)
  expect_equal(unique(roll_dice(NULL, times = 2)$experiment), 1)
  two <- roll_dice(times = 2) %>% roll_dice(times = 3)
  expect_equal(two$experiment, c(1, 1, 2, 2, 2))
  three <- flip_coin(times = 2) %>% flip_coin(times = 2) %>% flip_coin(times = 1)
  expect_equal(three$experiment, c(1, 1, 2, 2, 3))
  expect_equal(unique(roll_dice(times = 2) %>% roll_dice(3) %>% .$experiment), c(1, 2))
  expect_equal(unique(roll_dice(times = 2, agg = TRUE)$experiment), 1)
})
