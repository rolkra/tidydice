context("dice_formula")
library(tidydice)


test_that("parse_dice_formula works correctly", {
  
  dice_formula = "11d44"
  formula_df = parse_dice_formula(dice_formula)
  expected_output = tribble(
    ~subgroup_id, ~subgroup_formula, ~subgroup_sign, ~raw_set, ~operator, ~selector, ~value,
    "1", "11d44", "+", "11d44", "11", "d", 44
  )
  expect_equal(formula_df, expected_output)
  
  dice_formula = "-2d9"
  formula_df = parse_dice_formula(dice_formula)
  expected_output = tribble(
    ~subgroup_id, ~subgroup_formula, ~subgroup_sign, ~raw_set, ~operator, ~selector, ~value,
    "1", "-2d9", "-", "-2d9", "2", "d", 9
  )
  expect_equal(formula_df, expected_output)
  
  dice_formula = "d4-1d5e2+1d2rr3-1d2kl2p<4*3d2+9+9+d98"
  formula_df = parse_dice_formula(dice_formula)
})

test_that("base dice formula works correctly", {

  expect_equal(nrow(roll_dice_formula("1d6")),  1)
  expect_equal(nrow(roll_dice_formula("12d6")), 1)
  expect_equal(nrow(roll_dice_formula("3D8")), 1)
  expect_equal(nrow(roll_dice_formula("1d46")), 1)
  expect_equal(nrow(roll_dice_formula("d7")),   1)

  expect_error(nrow(roll_dice_formula("0d5")), "cannot roll 0 dice!")
  expect_error(nrow(roll_dice_formula("2d0")), "cannot roll a d0!")  
  
  expect_lt(max(roll_dice_formula("1d6", times=200)$result), 7)
  expect_lt(max(roll_dice_formula("d6",  times=200)$result), 7)
  expect_lt(max(roll_dice_formula("8d6", times=200)$result), 7*8)
  expect_gt(min(roll_dice_formula("8d6", times=200)$result), 7)
    
  expect_gt(mean(roll_dice_formula("1d30", times=2000)$result),
            mean(roll_dice_formula("1d15", times=2000)$result)
  )
  
  expect_gt(mean(roll_dice_formula("2d8", times=200)$result),
            mean(roll_dice_formula("1d8", times=200)$result)
  )
  
  expect_equal(mean(roll_dice_formula("1d6", times=2000)$result), 
               sum(1:6)/6,
               tolerance=0.2)
  expect_equal(mean(roll_dice_formula("2d8", times=2000)$result), 
               2*(sum(1:8)/8),
               tolerance=0.2)
  expect_equal(mean(roll_dice_formula("d33", times=2000)$result), 
               sum(1:33)/33,
               tolerance=0.2)
  
})


test_that("exploding dice", {
  
  expect_equal(nrow(roll_dice_formula("1d6e6")),  1)
  expect_equal(nrow(roll_dice_formula("1d3E2")),  1)
  
  # Exploding rolls should never be multiple of exploded die
  expect_equal(sum( 
    roll_dice_formula("1d7e7", times=200)$result %% 7 == 0),
    0)  
  
  # roll 1d2e2 200 times, results should never be even
  expect_equal(sum( 
    roll_dice_formula("1d2e2", times=200)$result %% 2 == 0),
    0)  

  # roll 1d6e2 (exploding a dice that is not the maximum)
  expect_equal(2 %in% roll_dice_formula("1d6e2", times=200)$result, F) 
  
  # exploding dice should always be lower or equal to n. sides
  expect_error(roll_dice_formula("1d4e6"))

  # 1d2e1 not implemented because of sample() syntax, 1d1e1 not implemented
  expect_error(roll_dice_formula("1d2e1"), "1d1e1 and 1d2e1 not implemented")
  expect_error(roll_dice_formula("1d1e1"), "1d1e1 and 1d2e1 not implemented")
   
  # Expected values over many rolls
  expect_equal(
    mean(roll_dice_formula("1d6e6", times=2000)$result),
    sum(1:5)/5 + mean(6*rgeom(2000, 1-1/6)), #4.2
    tolerance=0.5)

  expect_equal(
    mean(roll_dice_formula("1d10e10", times=2000)$result),
    sum(1:9)/9 + mean(10*rgeom(2000, 1-1/10)), #4.2
    tolerance=0.5)
  
  expect_gt(mean(roll_dice_formula("1d6e6", times=2000)$result),
            mean(roll_dice_formula("1d6", times=2000)$result),
  )
  
  }
)


test_that("Keep High/Low dice", {
  expect_gt(mean(roll_dice_formula("3d6kh1", times=200)$result),2)
  
  expect_lt(mean(roll_dice_formula("3d6KH1", times=200)$result),18)
  expect_equal(mean(roll_dice_formula("4d4kH2", times=2000)$result), 6.5, 
               tolerance=0.2)

  expect_equal(mean(roll_dice_formula("6d4Kl3", times=2000)$result), 5, 
               tolerance=0.2)
  
  # D&D Adv/Dis
  expect_equal(mean(roll_dice_formula("1d20", times=2000)$result), 
               sum(1:20)/20, 
               tolerance=0.2)
  expect_equal(mean(roll_dice_formula("2d20Kh1", times=2000)$result), 
               13.7, # Can't work out the correct formula for this, right now
               tolerance=0.2)
  expect_equal(mean(roll_dice_formula("2d20kL1", times=2000)$result), 
               7.2, 
               tolerance=0.2)
  
  # Invalid formulas
  expect_error(roll_dice_formula("2d6kh3"), "invalid kh/kl formula, can't keep more dice than rolled")
  expect_error(roll_dice_formula("2d6kh0"), "invalid kh/kl formula, can't keep less than 1 die")
  
  expect_gt(mean(roll_dice_formula("2d15kh1", times=200)$result),
            mean(roll_dice_formula("2d15kl1", times=200)$result)
            )
  
  # Exploding + kh/kl
  expect_equal(mean(roll_dice_formula("3d6e6kh2", times=2000)$result), 10, tolerance=0.2)
  expect_equal(mean(roll_dice_formula("3d6e6kh2", times=2000)$result), 9, tolerance=0.2)
  
    }  
)

test_that("arithmetic operations", {
  expect_equal(
    mean(roll_dice_formula("1d20+5", times=2000)$result), 
    sum(1:20)/20+5, 
    tolerance=0.2)
  #expect_equal(roll_dice_formula("1d1")$result, 1)
  expect_equal(
    mean(roll_dice_formula("1d18 + 3", times=2000)$result), 
    sum(1:18)/18+3, 
    tolerance=0.2)  
  expect_equal(
    mean(roll_dice_formula("1d20-2", times=2000)$result), 
    sum(1:20)/20-2, 
    tolerance=0.2)
  expect_equal(
    mean(roll_dice_formula("1d10*5", times=2000)$result), 
    5*(sum(1:10)/10), 
    tolerance=0.2)
  expect_equal(
    mean(roll_dice_formula("1d9/3", times=2000)$result), 
    (sum(1:10)/10)/3, 
    tolerance=0.2)
  expect_equal(
    mean(roll_dice_formula("1d10^2", times=2000)$result), 
    (sum((1:10)**2)/10), 
    tolerance=0.2)
  expect_equal(
    mean(roll_dice_formula("1d5**3", times=2000)$result), 
    (sum((1:5)**3)/5), 
    tolerance=0.2)
})
test_that("piping", {
  expect_equal(
    roll_dice_formula("1d5**3", times=20) %>% 
      roll_dice_formula("1d5**3", times=20) %>%
      count(experiment) %>%
      nrow,
    2)
  expect_equal(
    roll_dice_formula("1d5", times=20) %>% 
      roll_dice_formula("2d4", times=20) %>%
      count(dice_formula) %>%
      nrow,
    2)
  expect_equal(
    roll_dice_formula("1d5", times=20) %>% 
      roll_dice_formula("2d4", times=20) %>%
      nrow,
    40)
  expect_equal(
    roll_dice_formula("2d20h1", times=20, label="adv") %>% 
      roll_dice_formula("2d20kl1", times=20, label="dis") %>%
      count(label) %>%
      nrow,
    2) 
})

test_that("other function parameters", {
  # Check prob
  expect_equal(
    mean(roll_dice_formula("1d3", prob=c(0,0,1))$result),
    3
  )
  expect_error(roll_dice_formula("1d4", prob=c(1,0.2)))
  expect_error(roll_dice_formula("1d4", prob=c(-1,-1,-1,-1)))
  
  # Check random seed
  expect_equal(
    roll_dice_formula("1d1000", seed=1234),
    roll_dice_formula("1d1000", seed=1234)
  )
  # Check random seed
  expect_equal(
    roll_dice_formula("19d1000kh4", seed=1234),
    roll_dice_formula("19d1000kh4", seed=1234)
  )
  # Check random seed
  expect_equal(
    roll_dice_formula("1d1000", seed=456),
    roll_dice_formula("1d1000", seed=456)
  )
  # Check random seed
  expect_equal(
    roll_dice_formula("24d4kl10e4", seed=-23),
    roll_dice_formula("24d4kl10e4", seed=-23)
  )
  expect_equal(
    roll_dice_formula("24d4kl10e4", seed=1)$result != roll_dice_formula("24d4kl10e4", seed=2)$result, 
    T
  )
})