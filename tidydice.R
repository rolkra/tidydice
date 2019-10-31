##############################################################################
## setup
##############################################################################

library(tidyverse)
library(explore)

##############################################################################
## roll_dice
##############################################################################
#' Simulating rolling a dice. The result is returned as a tibble.
#'
#' @param data Data from a previous experiment
#' @param times How many times a dice is rolled (or how many dice are rolled at the same time)
#' @param rounds Number of rounds 
#' @param success Which result is a success (default = 6)
#' @param agg If TRUE, the result is aggregated (by experiment, rounds)
#' @param sides Number of sides of the dice (default = 6)
#' @param prob Vector of probability for each side of the dice
#' @return Result of experiment as a tibble
#' @examples
#' # rolling a dice once
#' roll_dice()
#'
#' # rolling a dice 10 times
#' roll_dice(times = 10)
#'
#' # aggregate result
#' roll_dice(times = 10, agg = TRUE)
#'
#' # rounds 
#' roll_dice(times = 10, rounds = 3, agg = TRUE)
#' 
#' # experiments
#' library(dplyr)
#' roll_dice(times = 10, rounds = 3, agg = TRUE) %>% 
#'   roll_dice(times = 12, rounds = 3, agg = TRUE) 
#' @export

roll_dice <- function(data, times = 1, rounds = 1, success = c(6), agg = FALSE, sides = 6, prob = NULL)  {
  
  # check if possible
  stopifnot(times >= 0)
  stopifnot(rounds >= 1)
  stopifnot(sides >= 2)
  
  if (agg)  {
    
    # roll dice and aggregate result
    rounds_seq <- 1:rounds
    success <- map_int(rounds_seq, ~sum(sample(x = 1:sides, size = times, replace = TRUE, prob = prob) %in% success))
    
    # create result tibble
    result <- tibble(round = rounds_seq,
                     times = rep(times, rounds),
                     success = success)
    
  } else { 
    
    # roll the dice: rounds x times
    result <- tibble(round = rep(1:rounds, each = times),
                     nr = rep(1:times, times = rounds),
                     result = sample(x = 1:sides, size = rounds * times, replace = TRUE, prob = prob)
    )
    
    # determine success
    result <- result %>%
      mutate(success = ifelse(result %in% success, TRUE, FALSE))
    
  } # if agg
  
  if (missing(data))  {
    
    # result of roll_dice (first experiment)
    result <- result %>% 
      mutate(experiment = 1) %>% 
      select(experiment, everything())
    result
    
  } else {
    
    # existing experiment variable?
    if ("experiment" %in% names(data)) {
      max_experiment <- max(data$experiment)
    } else {
      data$experiment <- 1
    }
    
    # new experiment (+1)
    result$experiment <- max_experiment + 1
    
    # bind result to data (pipe)
    result <- bind_rows(data, result) %>% 
      select(experiment, everything())
    result
  }
  
} # roll_dice

##############################################################################
## flip_coin
##############################################################################
#' Simulating flipping a coin. The result is returned as a tibble.
#'
#' @param data Data from a previous experiment
#' @param times How many times coin is flipped (or how many coins are flipped at the same time)
#' @param rounds Number of rounds 
#' @param success Which result is a success (default = 2)
#' @param agg If TRUE, the result is aggregated (by experiment, rounds)
#' @param sides Number of sides of the coin (default = 2)
#' @param prob Vector of probability for each side of the coin
#' @return Result of experiment as a tibble
#' @examples
#' # flipping a coin
#' flip_coin()
#'
#' # flipping a coin 10 times
#' flip_coin(times = 10)
#'
#' # aggregate result
#' flip_coin(times = 10, agg = TRUE)
#'
#' # rounds 
#' flip_coin(times = 10, rounds = 3, agg = TRUE)
#' 
#' # experiments
#' library(dplyr)
#' flip_coin(times = 10, rounds = 3, agg = TRUE) %>% 
#'   flip_coin(times = 12, rounds = 3, agg = TRUE) 
#' @export

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

# rolling a dice once
roll_dice()

# rolling a dice 10 times
roll_dice(times = 10)

# aggregate result
roll_dice(times = 10, agg = TRUE)

# rounds 
roll_dice(times = 10, rounds = 3, agg = TRUE)

# experiments 
roll_dice(times = 10, rounds = 3, agg = TRUE) %>% 
  roll_dice(times = 12, rounds = 3, agg = TRUE) 
  
# analysing
d <- roll_dice(100)
d %>% describe(result)
d %>% explore(result)
d %>% describe(success)
d %>% explore(success)

# compare rolling a dice 10 times and then 20 times
roll_dice(times = 10, agg = TRUE) %>% 
  roll_dice(times = 20, agg = TRUE) 

# adding a title
roll_dice(times = 10) %>% 
  roll_dice(times = 20) %>% 
  explore(success, target = experiment, title = "Dice 10x vs 20x")

# comparing 3 experiments
roll_dice(times = 60, rounds = 10000, agg = TRUE) %>% 
  roll_dice(times = 70, rounds = 10000, agg = TRUE) %>% 
  roll_dice(times = 100, rounds = 10000, agg = TRUE) %>% 
  explore(success, target = experiment, title = "Dice 60x vs 70x vs 100x")

# rolling a dice 60 times, repeat it 100 rounds
roll_dice(times = 60, rounds = 100, agg = TRUE) %>% 
  explore(success)

# rolling an unfair dice 60 times, repeat it 100 rounds
roll_dice(times = 60, rounds = 100, prob = c(1/12,1/6,1/6,1/6,1/6,1/6+1/12), agg = TRUE) %>%
  explore(success)

# compare two experiments
roll_dice(times = 60, rounds = 100, prob = c(1/6,1/6,1/6,1/6,1/6,1/6), agg = TRUE) %>%
  roll_dice(times = 60, rounds = 100, prob = c(1/12,1/6,1/6,1/6,1/6,1/6+1/12), agg = TRUE) %>%
  explore(success, target = experiment)

# compare two experiments (bar chart)
roll_dice(times = 60, rounds = 100, prob = c(1/6,1/6,1/6,1/6,1/6,1/6), agg = TRUE) %>%
  roll_dice(times = 60, rounds = 100, prob = c(1/12,1/6,1/6,1/6,1/6,1/6+1/12), agg = TRUE) %>%
  explore_bar(success, 
              target = experiment, 
              flip = FALSE,
              title = "Rolling a dice 60 times, 100 rounds")

