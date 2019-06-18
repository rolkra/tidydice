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
      summarise(times = n(), success = sum(success))
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

# rolling a dice 60 times
roll_dice(60) %>% explore(result)

# rolling a dice 60 times, repeat it 100 rounds
roll_dice(60, rounds = 100) %>% explore(result)

# rolling an unfair dice 60 times, repeat it 100 rounds
roll_dice(60, rounds = 100, prob = c(1/12,1/6,1/6,1/6,1/6,1/6+1/12)) %>%
  explore(result)

# rolling a dice 60 times, repeat it 1000 rounds
data <- roll_dice(60, rounds = 1000, agg = TRUE)
data %>% explore(success, auto_scale = FALSE)
data %>% describe(success)

# plot absolute
data %>%
  ggplot(aes(success)) +
  geom_bar() +
  theme_minimal()

# plot percentate
data %>%
  ggplot(aes(success)) +
  geom_bar(aes(y = (..count..)/sum(..count..)*100.0)) +
  theme_minimal() +
  labs(y = "percent")

# plot percentate + label
ggplot(data, aes(x = success)) +
  geom_bar(aes(y = (..count..)/sum(..count..)*100.0)) +
  geom_text(aes(y = ((..count..)/sum(..count..)*100.0),
                label = ((..count..)/sum(..count..)*100.0)),
            stat = "count",
            vjust = -0.30,
            size = 2) +
  theme_minimal() +
  labs(y = "percent")
