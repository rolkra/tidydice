context("dice_formula")
library(tidydice)


test_that("parse_dice_formula works correctly", {
  
  dice_formula = "11d44"
  formula_df = parse_dice_formula(dice_formula)
  expected_output = tibble::tribble(
    ~subgroup_id, ~subgroup_formula, ~subgroup_sign, ~raw_set, ~operator, ~selector, ~value,
    1, "+11d44", "+", "11d44", "11", "d", 44
  )
  expect_equal(formula_df, expected_output)
  
  dice_formula = "-2d9"
  formula_df = parse_dice_formula(dice_formula)
  expected_output = tibble::tribble(
    ~subgroup_id, ~subgroup_formula, ~subgroup_sign, ~raw_set, ~operator, ~selector, ~value,
    1, "-2d9", "-", "2d9", "2", "d", 9
  )
  expect_equal(formula_df, expected_output)
  
  # one group, two operations
  dice_formula = "1d5rr>2"
  formula_df = parse_dice_formula(dice_formula)
  expected_output = tibble::tribble(
    ~subgroup_id, ~subgroup_formula, ~subgroup_sign, ~raw_set, ~operator, ~selector, ~value,
    1, "+1d5rr>2", "+", "1d5", "1", "d", 5,
    1, "+1d5rr>2", "+", "rr>2", "rr", ">", 2
  )
  expect_equal(formula_df, expected_output)
  
  # a formula cannot start with * or /
  expect_error(parse_dice_formula("*1d5"), "cannot parse '\\*1d5'")
  
  # one group, multiple operations
  dice_formula = "1d5e2e3rr<2";   
  formula_df = parse_dice_formula(dice_formula) 
  expected_output = tibble::tribble(
    ~subgroup_id, ~subgroup_formula, ~subgroup_sign, ~raw_set, ~operator, ~selector, ~value,
    1, "+1d5e2e3rr<2", "+", "1d5", "1", "d", 5,
    1, "+1d5e2e3rr<2", "+", "e2", "e", "", 2,
    1, "+1d5e2e3rr<2", "+", "e3", "e", "", 3,
    1, "+1d5e2e3rr<2", "+", "rr<2", "rr", "<", 2
  )
  expect_equal(formula_df, expected_output)
  
  # Two groups, with multiple operations
  dice_formula = "1d3+9d11e3rr>6"
  formula_df = parse_dice_formula(dice_formula)
  expected_output = tibble::tribble(
    ~subgroup_id, ~subgroup_formula, ~subgroup_sign, ~raw_set, ~operator, ~selector, ~value,
    1, "+1d3",        "+", "1d3",   "1", "d", 3,
    2, "+9d11e3rr>6", "+", "9d11",  "9", "d", 11,
    2, "+9d11e3rr>6", "+", "e3",    "e", "",   3,
    2, "+9d11e3rr>6", "+", "rr>6",  "rr", ">", 6
  )
  expect_equal(formula_df, expected_output)
  
  dice_formula = "d4-1d5e2+1d2rr3-d2kl2p<4*3d2+9/9+d98"
  formula_df = parse_dice_formula(dice_formula)
  expected_output = tibble::tribble(
    ~subgroup_id, ~subgroup_formula, ~subgroup_sign, ~raw_set, ~operator, ~selector, ~value,
    1,   "+d4",       "+",    "d4",    "1",    "d",   4,
    2,   "-1d5e2",    "-",    "1d5",   "1",    "d",   5,
    2,   "-1d5e2",    "-",    "e2",    "e",    "",   2,
    3,   "+1d2rr3",   "+",    "1d2",   "1",    "d",   2,
    3,   "+1d2rr3",   "+",    "rr3",   "rr",    "",   3,
    4,   "-d2kl2p<4", "-",    "d2",    "1",    "d",   2,
    4,   "-d2kl2p<4", "-",    "kl2",   "k",    "l",   2,
    4,   "-d2kl2p<4", "-",    "p<4",   "p",    "<",   4,
    5,   "*3d2",      "*",    "3d2",   "3",    "d",   2,
    6,   "+9",        "+",    "9",     "9",    "",   9,
    7,   "/9",        "/",    "9",     "9",    "",   9,
    8,   "+d98",      "+",    "d98",   "1",    "d",   98,
  )
  expect_equal(formula_df, expected_output)
  
  # Same as previous, but with spaces
  dice_formula = "d4- 1d5e2+ 1d2rr3   - d2kl2p<4 * 3d2+9/9+d98"
  formula_df = parse_dice_formula(dice_formula)
  expect_equal(formula_df, expected_output)
  
  # Same as previous, but with even more spaces
  dice_formula = "d 4- 1d5 e2+ 1d2 rr3   - d2kl 2 p<4 * 3d2+9 / 9+ d 9 8"
  formula_df = parse_dice_formula(dice_formula)
  expect_equal(formula_df, expected_output)
  
  # Same parser as roll_dice_formula(): d%, several selectors, ^ and **, case
  expect_equal(
    parse_dice_formula("2D%k>3<2^2**3"),
    tibble::tribble(
      ~subgroup_id, ~subgroup_formula, ~subgroup_sign, ~raw_set, ~operator, ~selector, ~value,
      1, "+2D%k>3<2", "+", "2D%",  "2", "d", 100,
      1, "+2D%k>3<2", "+", "k>3",  "k", ">", 3,
      1, "+2D%k>3<2", "+", "k<2",  "k", "<", 2,
      2, "^2",        "^", "2",    "2", "",  2,
      3, "**3",       "**", "3",   "3", "",  3
    ))
  
  # Leading minus, parentheses: groups are listed from left to right
  expect_equal(
    parse_dice_formula("-(1d6+2)*3"),
    tibble::tribble(
      ~subgroup_id, ~subgroup_formula, ~subgroup_sign, ~raw_set, ~operator, ~selector, ~value,
      1, "-1d6", "-", "1d6", "1", "d", 6,
      2, "+2",   "+", "2",   "2", "",  2,
      3, "*3",   "*", "3",   "3", "",  3
    ))
  
  # Only the syntax is checked, not whether the formula can be rolled
  expect_equal(nrow(parse_dice_formula("1d2rr3")), 2)
  
  # Invalid syntax is an error, also here
  expect_error(parse_dice_formula("1d6x3"), "cannot parse 'x3'")
  expect_error(parse_dice_formula("1d6e"), "needs a selector")
  expect_error(parse_dice_formula("(1d6"), "missing closing parenthesis")
  expect_error(parse_dice_formula(c("1d6", "1d4")), "single character string")
  
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
  expect_error(roll_dice_formula("1d4e6"), "no side of the dice matches")

  # 1d2e1 is valid (the die stops exploding on a 2), 1d1e1 would explode forever
  expect_equal(mean(roll_dice_formula("1d2e1", times=20000, seed=1)$result), 3, tolerance=0.03)
  expect_error(roll_dice_formula("1d1e1"), "every side would explode")
  expect_error(roll_dice_formula("1d6e>0"), "every side would explode")
  expect_error(roll_dice_formula("1d6e0"), "no side of the dice matches")
   
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
  
  # Like in Avrae, "e" needs a selector
  expect_error(roll_dice_formula("1d6e"), "needs a selector")
  
  # Exact expected values: E = 21/5 when exactly one side explodes
  expect_equal(mean(roll_dice_formula("1d6e6", times=20000, seed=1)$result), 
               4.2, tolerance=0.03)
  expect_equal(mean(roll_dice_formula("1d6e2", times=20000, seed=1)$result), 
               4.2, tolerance=0.03)
  
  # Comparators are strict, like in Avrae: e>4 explodes on 5 and 6, e<3 on 1 and 2
  expect_equal(mean(roll_dice_formula("1d6e>4", times=20000, seed=1)$result),
               5.25, tolerance=0.03)
  expect_equal(mean(roll_dice_formula("1d6e<3", times=20000, seed=1)$result),
               21/4, tolerance=0.03)
  expect_equal(mean(roll_dice_formula("1d6e>5", times=20000, seed=1)$result),
               4.2, tolerance=0.03)
  
  # Explosions are rolled per die: 10d6e6 mean is 10 * 4.2
  expect_equal(mean(roll_dice_formula("10d6e6", times=5000, seed=1)$result),
               42, tolerance=0.02)
  
  # Exploded dice can explode again
  expect_true(any(roll_dice_formula("1d2e2", times=2000, seed=1)$result > 3))
  
  # Exploded dice are added to the set, so they compete for kh/kl as separate dice
  expect_gt(mean(roll_dice_formula("2d6e6kh2", times=5000, seed=1)$result),
            mean(roll_dice_formula("2d6kh2", times=5000, seed=1)$result))
  expect_lte(max(roll_dice_formula("2d6e6kh1", times=2000, seed=1)$result), 6)
  
  # prob is honoured by the dice that explode
  expect_true(all(roll_dice_formula("1d3e3", times=50, prob=c(1,0,0))$result == 1))
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
  expect_gt(mean(roll_dice_formula("3d6e6kh2", times=5000, seed=1)$result),
            mean(roll_dice_formula("3d6kh2", times=5000, seed=1)$result))
  
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
    roll_dice_formula("2d20kh1", times=20, label="adv") %>% 
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


test_that("multiple dice groups and operator precedence", {
  expect_equal(mean(roll_dice_formula("1d6+1d4", times=5000, seed=1)$result), 
               3.5 + 2.5, tolerance=0.03)
  expect_equal(mean(roll_dice_formula("2d6-1d4", times=5000, seed=1)$result), 
               7 - 2.5, tolerance=0.03)
  expect_equal(mean(roll_dice_formula("1d6e6+1d4+3", times=20000, seed=1)$result), 
               4.2 + 2.5 + 3, tolerance=0.02)
  expect_equal(mean(roll_dice_formula("-1d4+10", times=5000, seed=1)$result), 
               10 - 2.5, tolerance=0.03)
  
  # * before +, parentheses, ^ before *
  expect_equal(roll_dice_formula("1d1+2*3")$result, 7)
  expect_equal(roll_dice_formula("(1d1+2)*3")$result, 9)
  expect_equal(roll_dice_formula("2*1d1^3")$result, 2)
  expect_equal(roll_dice_formula("2^3^2+1d1")$result, 513)
  expect_equal(roll_dice_formula("2**3+1d1")$result, 9)
  expect_equal(roll_dice_formula("1d1 + 1 d 1")$result, 2)
  expect_error(roll_dice_formula("(1d6+2"), "missing closing parenthesis")
})

test_that("Avrae modifiers", {
  # d20 syntax: k, p, rr, ro, ra, e, mi, ma, d%
  mean_of <- function(f, times = 20000) {
    mean(roll_dice_formula(f, times = times, seed = 1)$result)
  }
  
  # Keep / drop with selectors
  expect_equal(mean_of("4d6kh3"), 12.24, tolerance = 0.01)
  expect_equal(mean_of("4d6pl1"), mean_of("4d6kh3"), tolerance = 0.01)
  expect_equal(mean_of("1d6k>4"), 11/6, tolerance = 0.03) # 5, 6 kept, else 0
  expect_equal(mean_of("1d6k5"), 5/6, tolerance = 0.03)
  expect_equal(mean_of("1d6p<3"), 18/6, tolerance = 0.03) # 1, 2 dropped
  expect_equal(mean_of("1d6p3"), (21-3)/6, tolerance = 0.03)
  
  # Reroll: rr until the die does not match, ro once, ra adds
  expect_equal(mean_of("1d6rr1"), 4, tolerance = 0.01) # uniform on 2..6
  expect_equal(mean_of("1d6rr<3"), 4.5, tolerance = 0.01) # uniform on 3..6
  expect_equal(min(roll_dice_formula("1d6rr<3", times = 500)$result), 3)
  expect_equal(mean_of("1d6ro1"), 3.5 + 1/6*(3.5-1), tolerance = 0.01)
  expect_equal(mean_of("1d6ro<3"), 4/6*4.5 + 2/6*3.5 + 0, tolerance = 0.03) 
  expect_equal(mean_of("1d6ra1"), 3.5 + 3.5/6, tolerance = 0.01)
  # ra explodes at most one die
  expect_true(all(roll_dice_formula("5d6ra<7", times = 50, prob = c(0,0,0,0,0,1))$result == 36))
  expect_true(all(roll_dice_formula("5d6ra>5", times = 50, prob = c(0,0,0,0,0,1))$result == 36))
  
  # Min / max
  expect_equal(mean_of("1d6mi3"), (3+3+3+4+5+6)/6, tolerance = 0.01)
  expect_equal(mean_of("1d6ma4"), (1+2+3+4+4+4)/6, tolerance = 0.01)
  expect_equal(min(roll_dice_formula("3d6mi3", times = 200)$result), 9)
  
  # Modifiers are applied in the order they are written
  expect_lte(max(roll_dice_formula("2d6e6kh1", times = 2000, seed = 1)$result), 6)
  expect_equal(mean_of("1d6kh1e6"), 4.2, tolerance = 0.03)
  expect_equal(max(roll_dice_formula("3d6mi4ma4", times = 200)$result), 12)
  
  # Consecutive identical modifiers are merged, like in d20: e5e6 is e(5,6)
  expect_equal(mean_of("1d6e5e6"), 5.25, tolerance = 0.03)
  expect_equal(mean_of("1d6e5e6"), mean_of("1d6e5>5"), tolerance = 0.03)
  expect_error(roll_dice_formula("1d2e1e2"), "every side would explode")
  
  # Several selectors are a union
  expect_equal(mean_of("1d6k1>5"), 7/6, tolerance = 0.03)
  
  # Percentile dice
  expect_equal(mean_of("d%"), 50.5, tolerance = 0.02)
  expect_equal(max(roll_dice_formula("1d%", times = 2000, seed = 1)$result), 100)
  
  # prob is used for every roll, also rerolls
  expect_true(all(roll_dice_formula("1d3rr1", times = 50, prob = c(.5, .5, 0))$result == 2))
  
  # Limits like in d20: no endless loops
  expect_error(roll_dice_formula("1d6rr>0"), "every side would be rerolled")
  expect_error(roll_dice_formula("2d6rr7"), "no side of the dice matches")
})

test_that("unsupported or invalid syntax is an error, not ignored", {
  expect_error(roll_dice_formula("1d6x3"), "cannot parse 'x3'")
  expect_error(roll_dice_formula("2d20h1"), "cannot parse 'h1'")
  expect_error(roll_dice_formula("1d6rr"), "needs a selector")
  expect_error(roll_dice_formula("2d6kh"), "needs a selector")
  expect_error(roll_dice_formula("1d6mi>2"), "needs a plain number")
  expect_error(roll_dice_formula("1d6+"), "cannot parse")
  expect_error(roll_dice_formula("5+3"), "at least one d statement")
  expect_error(roll_dice_formula("1d6 [fire]"), "cannot parse")
})


test_that("review fixes: precedence, big dice, inputs, chunks", {
  # unary minus is weaker than ^, like in R
  expect_equal(roll_dice_formula("-2^2+1d1")$result, -3)
  expect_equal(roll_dice_formula("2*-3+1d1")$result, -5)
  expect_equal(roll_dice_formula("2^-1+1d1")$result, 1.5)
  expect_equal(roll_dice_formula("-1d1^2")$result, -1)
  
  # huge dice are not enumerated when the modifiers are checked
  expect_lt(system.time(roll_dice_formula("1d1000000000rr1"))[["elapsed"]], 1)
  expect_error(roll_dice_formula("1d1000000000rr>0"), "every side would be rerolled")
  expect_error(roll_dice_formula("1d1000000000e<1000000001"), "every side would explode")
  expect_error(roll_dice_formula("1d6e7"), "no side of the dice matches")
  # selectors that together cover every side
  expect_error(roll_dice_formula("1d6e<4>3"), "every side would explode")
  expect_error(roll_dice_formula("1d6e<3e3e>3"), "every side would explode")
  expect_equal(nrow(roll_dice_formula("1d6e<3>4")), 1) # 3 and 4 never explode
  
  # inputs
  expect_error(roll_dice_formula("1d6", times = 0), "times must be")
  expect_error(roll_dice_formula("1d6", rounds = 0), "rounds must be")
  expect_error(roll_dice_formula(c("1d6", "1d4")), "single character string")
  
  # big jobs are rolled in chunks: same shape and values as in a single block
  big <- roll_dice_formula("1000d6", times = 6000, seed = 1)
  expect_equal(nrow(big), 6000)
  expect_equal(mean(big$result), 3500, tolerance = 0.001)
  expect_equal(big$nr, 1:6000)
  chunked <- roll_dice_formula("1d6e6", times = 120000, rounds = 1, seed = 1)
  expect_equal(nrow(chunked), 120000)
  expect_equal(mean(chunked$result), 4.2, tolerance = 0.02)
})

test_that("data argument: first experiment, shortcuts and piping", {
  # formula or times given as first argument start a new experiment
  expect_equal(unique(roll_dice_formula("2d6")$experiment), 1)
  expect_equal(unique(roll_dice_formula(5, dice_formula = "2d6")$experiment), 1)
  expect_equal(nrow(roll_dice_formula(5, dice_formula = "2d6")), 5)
  expect_equal(unique(roll_dice_formula(NULL, "2d6")$experiment), 1)
  
  # a data frame is extended with the next experiment
  two <- roll_dice_formula("1d6", times = 2) %>% roll_dice_formula("2d6", times = 3)
  expect_equal(two$experiment, c(1, 1, 2, 2, 2))
  three <- two %>% roll_dice_formula("3d6")
  expect_equal(max(three$experiment), 3)
})

test_that("prob: unfair dice", {
  mean_of <- function(f, ..., times = 20000) {
    mean(roll_dice_formula(f, times = times, seed = 1, ...)$result)
  }
  
  # one vector is used for all dice of the formula
  expect_equal(mean_of("2d3", prob = c(0, 0, 1)), 6)
  expect_equal(mean_of("1d3+1d3", prob = c(0, 1, 0)), 4)
  # weights do not need to sum to 1
  expect_equal(mean_of("1d3", prob = c(1, 1, 2)), (1 + 2 + 6) / 4, tolerance = 0.02)
  
  # a vector needs the same number of sides in every group
  expect_error(roll_dice_formula("1d6+1d4", prob = rep(1, 6)), 
               "has 6 values, but dice group 2 has 4 sides")
  
  # a list: one vector (or NULL = fair) for each group of dice, left to right
  expect_equal(mean_of("1d6+1d4", prob = list(c(0,0,0,0,0,1), NULL)), 6 + 2.5, tolerance = 0.01)
  expect_equal(mean_of("1d6+1d4", prob = list(NULL, c(0,0,0,1))), 3.5 + 4, tolerance = 0.01)
  expect_equal(mean_of("(1d6+1d4)*2", prob = list(c(1,0,0,0,0,0), c(0,0,0,1))), 10)
  expect_equal(mean_of("1d6", prob = list(c(0,0,0,0,0,1))), 6)
  expect_error(roll_dice_formula("1d6+1d4", prob = list(NULL)), 
               "list with 1 elements, but the formula has 2 groups")
  expect_error(roll_dice_formula("1d6+1d4", prob = list(NULL, c(1, 1))), 
               "dice group 2 has 4 sides")
  expect_error(roll_dice_formula("1d6", prob = "a"), "prob must be numeric")
  
  # with modifiers: rerolls, explosions and extra dice use the probabilities
  expect_true(all(roll_dice_formula("2d3rr1", times = 50, prob = c(.5, .5, 0))$result == 4))
  expect_equal(mean_of("1d3e3", prob = c(0, .5, .5)), 5, tolerance = 0.03) # E = .5 * 2 + .5 * (3 + E)
  expect_equal(mean_of("1d3e1+1d2", prob = list(c(0, 1, 0), c(0, 1))), 4)
})

test_that("detail: the dice that were rolled", {
  # not there by default, nor with agg
  expect_false("dice" %in% names(roll_dice_formula("2d6")))
  expect_false("dice" %in% names(roll_dice_formula("2d6", detail = TRUE, agg = TRUE)))
  
  d <- roll_dice_formula("4d6pl1", times = 5, rounds = 2, detail = TRUE, seed = 1)
  expect_equal(nrow(d), 10)
  expect_true(is.list(d$dice))
  expect_equal(names(d$dice[[1]]), c("group", "sides", "value", "kept"))
  expect_true(all(vapply(d$dice, nrow, integer(1)) == 4))
  expect_true(all(vapply(d$dice, function(x) sum(!x$kept), numeric(1)) == 1))
  
  # the kept dice add up to the result: for additions, with every modifier
  for (f in c("3d6", "4d6pl1", "4d6kh3", "5d6e6kh3", "3d6rr1ro2", "2d6mi3", "3d6ra<3", 
              "1d6+1d4", "2d6e5+1d4e>2+3", "50d6k>3")) {
    extra <- if (grepl("\\+3$", f)) 3 else 0
    d <- roll_dice_formula(f, times = 200, detail = TRUE, seed = 2)
    sums <- vapply(d$dice, function(x) sum(x$value[x$kept]), numeric(1)) + extra
    expect_equal(sums, d$result, info = f)
  }
  
  # group and sides of each die
  d <- roll_dice_formula("2d6+1d4", times = 20, detail = TRUE, seed = 3)
  expect_true(all(vapply(d$dice, function(x) all(x$group == c(1, 1, 2)), logical(1))))
  expect_true(all(vapply(d$dice, function(x) all(x$sides == c(6, 6, 4)), logical(1))))
  expect_true(all(vapply(d$dice, function(x) all(x$value[3] <= 4), logical(1))))
  
  # exploded dice are added, dice that are not rolled are not there
  d <- roll_dice_formula("1d6e6", times = 300, detail = TRUE, seed = 4)
  n_dice <- vapply(d$dice, nrow, integer(1))
  expect_true(any(n_dice > 1))
  expect_true(all(vapply(d$dice, function(x) all(utils::head(x$value, -1) == 6), logical(1))))
  d <- roll_dice_formula("3d6ra<7", times = 10, detail = TRUE)
  expect_true(all(vapply(d$dice, nrow, integer(1)) == 4))
  
  # numbers have no dice
  d <- roll_dice_formula("1d1+5", detail = TRUE)
  expect_equal(nrow(d$dice[[1]]), 1)
  
  # same results with and without detail
  expect_equal(roll_dice_formula("4d6e6kh3", times = 50, seed = 5)$result,
               roll_dice_formula("4d6e6kh3", times = 50, seed = 5, detail = TRUE)$result)
  
  # chunks: every roll has its dice
  d <- roll_dice_formula("2d6e6", times = 60000, detail = TRUE, seed = 6)
  expect_equal(length(d$dice), 60000)
  expect_equal(vapply(d$dice[c(1, 50001, 60000)], function(x) sum(x$value), numeric(1)), 
               d$result[c(1, 50001, 60000)])
  
  # piping: experiments with and without dice
  two <- roll_dice_formula("1d6", times = 2) %>% 
    roll_dice_formula("2d6", times = 2, detail = TRUE)
  expect_equal(nrow(two), 4)
  expect_equal(vapply(two$dice, is.null, logical(1)), c(TRUE, TRUE, FALSE, FALSE))
})

test_that("there is no limit to the number of dice, but formulas that never end are errors", {
  # many dice, also above the 1000 of Avrae
  expect_equal(nrow(roll_dice_formula("1000d6")), 1)
  expect_equal(mean(roll_dice_formula("2000d6", times = 50, seed = 1)$result), 7000, tolerance = 0.01)
  expect_equal(mean(roll_dice_formula("500d6+501d6", times = 50, seed = 1)$result), 3503.5, 
               tolerance = 0.01)
  # keeping 4000 of 5000 dice: more than the average of 4000 dice, at most 4000 sixes
  kept <- roll_dice_formula("5000d6kh4000", times = 5, seed = 1)$result
  expect_true(all(kept > 4000 * 3.5 & kept <= 4000 * 6))
  expect_equal(nrow(roll_dice_formula("3000d6e6", times = 3, detail = TRUE)$dice[[1]]) > 3000, TRUE)
  
  # long chains of explosions are possible, and have the right distribution: 
  # 1d6e6 with a 99% chance of a 6 has a mean of 6 * 0.99 / 0.01 + ... 
  p <- c(.002, .002, .002, .002, .002, .99)
  expect_equal(mean(roll_dice_formula("1d6e6", prob = p, times = 20000, seed = 1)$result), 
               (sum((1:5) * .002) + 6 * .99) / (1 - .99), tolerance = 0.05)
  
  # formulas that would never end: rr on the highest or lowest dice, and rerolling
  # or exploding on the sides that have all the probability
  expect_error(roll_dice_formula("3d6rrh1"), "would never end")
  expect_error(roll_dice_formula("3d6rrl1"), "would never end")
  expect_error(roll_dice_formula("3d6rr1h1"), "would never end")
  expect_equal(nrow(roll_dice_formula("3d6roh1")), 1) # once is fine
  expect_error(roll_dice_formula("1d6e6", prob = c(0, 0, 0, 0, 0, 1)), "never ends")
  expect_error(roll_dice_formula("1d6e>4", prob = c(0, 0, 0, 0, 1, 1)), "never ends")
  expect_error(roll_dice_formula("1d6rr1", prob = c(1, 0, 0, 0, 0, 0)), "never ends")
  expect_error(roll_dice_formula("1d6+1d4rr<3", prob = list(NULL, c(1, 1, 0, 0))), "never ends")
  expect_equal(nrow(roll_dice_formula("1d6ro1", prob = c(1, 0, 0, 0, 0, 0))), 1)
  expect_equal(nrow(roll_dice_formula("1d6e6", prob = c(1, 0, 0, 0, 0, 0))), 1) # never a 6
  # h or l together with selectors that match every side (or all the probability)
  expect_error(roll_dice_formula("1d1e1h1"), "every side would explode")
  expect_error(roll_dice_formula("1d6e<7h1"), "every side would explode")
  expect_error(roll_dice_formula("1d6e6h1", prob = c(0, 0, 0, 0, 0, 1)), "never ends")
  expect_equal(nrow(roll_dice_formula("1d6e6h1", times = 10)), 10)
  expect_equal(nrow(roll_dice_formula("1d6e6h1", prob = c(1, 0, 0, 0, 0, 1), times = 10)), 10)
  
  # numbers are not dice
  expect_equal(roll_dice_formula("1d1+1000", times = 2)$result, c(1001, 1001))
})


test_that("explosion history is local to each operation", {
  evaluate <- function(formula, draws) {
    node <- tidydice:::parse_dice_expression(formula)
    cursor <- 0L
    roll <- function(k) {
      out <- draws[cursor + seq_len(k)]
      cursor <<- cursor + k
      out
    }
    tidydice:::roll_dice_sets(1, node$count, node$mods, roll, TRUE)
  }
  # ra adds a 2; the later e must also explode the original 1.
  result <- evaluate("1d2ra1e1", c(1, 2, 2))
  expect_equal(result$total, 5)
  expect_equal(as.numeric(result$values), c(1, 2, 2))
  # Separate e operations can select dice that exploded earlier.
  result <- evaluate("1d2e1ma1e1", c(1, 2, 2, 2))
  expect_equal(result$total, 6)
  expect_equal(as.numeric(result$values), c(1, 1, 2, 2))
})

test_that("selectors can match values created by mi and ma", {
  expect_equal(roll_dice_formula("1d6mi7e7", prob = c(1, 0, 0, 0, 0, 0))$result, 8)
  expect_equal(roll_dice_formula("1d6ma0rr0", prob = c(1, 0, 0, 0, 0, 0))$result, 1)
  expect_equal(roll_dice_formula("1d6mi7ro7", prob = c(0, 1, 0, 0, 0, 0))$result, 2)
  expect_equal(roll_dice_formula("1d6ma0ra0", prob = c(0, 1, 0, 0, 0, 0))$result, 2)
  expect_error(roll_dice_formula("1d6mi7e<7"), "every side would explode")
  expect_error(roll_dice_formula("1d6ma0rr1", prob = c(1, 0, 0, 0, 0, 0)), "never ends")
})
