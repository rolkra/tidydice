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
  
  # roll the dice: rounds x times
  result <- tibble(round = rep(1:rounds, each = times),
                   nr = rep(1:times, times = rounds),
                   result = sample(x = 1:sides, size = rounds * times, replace = TRUE, prob = prob)
                   )
  
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

# plot absolute
data %>% 
  ggplot(aes(success_sum)) + 
  geom_bar() +
  theme_minimal

# plot percentate
data %>% 
  ggplot(aes(success_sum)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)*100.0)) +
  theme_minimal() +
  labs(y = "percent")



