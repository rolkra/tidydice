##############################################################################
## setup
##############################################################################

library(tidyverse)
library(explore)

##############################################################################
## roll_dice  
##############################################################################

roll_dice <- function(times = 1, rounds = 1, success = c(6), agg = FALSE, sides = 6, prob = NULL)  {

  # check if possible
  stopifnot(times >= 0)
  stopifnot(rounds >= 1)
  stopifnot(sides >= 2)
  
  # create empty tibble
  result <- tibble(round = integer(), nr = integer(), result = integer())
  
  # roll the dice: rounds x times
  for(i in 1:rounds)  {
    w <- sample(1:sides, times, replace = TRUE, prob = prob)
    t <- tibble(nr = seq_along(w), result = w, round = i)
    result <- bind_rows(result, t)      
  }
  
  # determine success
  result <- result %>% 
    mutate(success = ifelse(result %in% success, TRUE, FALSE))

   # final result
  if (agg)  {
    result %>% 
      group_by(round) %>% 
      summarise(n = n(), success_sum = sum(success))
  } else {
    result
  }
  
} # roll_dice

##############################################################################
## flip_coin
##############################################################################

flip_coin <- function(times = 1, rounds = 1, success = c(2), agg = FALSE, sides = 2, prob = NULL)  {
  
  # check if possible
  stopifnot(times >= 0)
  stopifnot(rounds >= 1)
  stopifnot(sides == 2)
  
  # coin = dice with 2 sides
  roll_dice(times, rounds, success, agg, sides, prob)  
}
  
##############################################################################
## examples
##############################################################################

# reproducible random numbers
set.seed(123)

# rolling a dice 10 times
roll_dice(10)

# rolling a dice 60 times, repeat it 1000 rounds
data <- roll_dice(60, rounds = 1000, agg = TRUE)
data %>% describe(success_sum)
data %>% explore(success_sum)

# how likely is it to get at least 20 six
data %>% 
  mutate(check = ifelse(success_sum >= 20, 1, 0)) %>% 
  summarise(prop = 100.0 * sum(check)/ n())


