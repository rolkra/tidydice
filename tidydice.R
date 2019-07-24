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
  
  result
  
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
## plot_success
##############################################################################

plot_success <- function(data, title = "", color = "#cccccc", label = TRUE, label_size = 2)  {

    p <- ggplot(data, aes(x = success)) +
           geom_bar(aes(y = (..count..)/sum(..count..)*100.0),
                    fill = color) +
           theme_minimal() +
           labs(y = "percent")
    
    if (length(unique(data$success)) <= 30 & label)  {
       p <- p + geom_text(aes(y = ((..count..)/sum(..count..)*100.0),
                          label = formatC((..count..)/sum(..count..)*100.0, format = "f", digits = 1)),
                          stat = "count",
                          vjust = 1,
                          size = label_size)
    } # if
    
    if (nchar(title) > 0)  {
      
      p <- p + ggtitle(title)
      
    }

    # plot result
    p
    
} # plot_success


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

# plot success
roll_dice(60, rounds = 1000, agg = FALSE) %>% 
  plot_success(label_size = 3, title = "Experiment")

roll_dice(60, rounds = 1000, agg = TRUE) %>% 
  plot_success(label_size = 2)

roll_dice(100, rounds = 1000, agg = TRUE) %>% 
  plot_success()

flip_coin(60, rounds = 1000, agg = FALSE) %>% 
  plot_success(label_size = 3, title = "Experiment")
