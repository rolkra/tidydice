##############################################################################
## setup
##############################################################################

library(tidyverse)
library(explore)

##############################################################################
## roll_dice
##############################################################################

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

plot_success <- function(data, title = "", label, label_size = 2)  {
  
  # if no label parameter, decide on
  # number of bars to be plotted
  bars <- length(unique(data$success)) * max(data$experiment)
  if (missing(label)) {
    if (bars <= 20)  {
       label <- TRUE
    } else {
       label <- FALSE
    }
  }
  
  # use a factor for experiment so that fill works
  data$experiment <- factor(data$experiment)
  
  data_experiment <- data %>% 
    group_by(experiment) %>% 
    summarise(experiment_n = n())
  
  data_success <- data %>% 
    group_by(experiment, success) %>% 
    summarise(success_sum = sum(success),
              n = n())
  
  data_bar <- data_success %>% 
    inner_join(data_experiment, by = "experiment") %>% 
    mutate(pct = n / experiment_n * 100.0)
  
  #data_bar$success <- factor(data_bar$success)
  
  p <- ggplot(data_bar, aes(x = success)) +
    geom_col(aes(y = pct, fill = experiment), position = "dodge") +
    theme_minimal() +
    labs(y = "percent")
  
  # plot labels?
  if (label == TRUE)  {
    p <- p + geom_text(aes(y = pct, 
                           label = pct,
                           group = experiment),
                      #     label = formatC(success_pct, format = "f", digits = 1)),
                       position = position_dodge(width = 1),
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

# rolling a dice 60 times
d <- roll_dice(times = 60)
d %>% describe(result)
d %>% describe(success)
d %>% plot_success()

# compare rolling a dice 10 times and then 20 times
roll_dice(times = 10, agg = TRUE) %>% 
  roll_dice(times = 20, agg = TRUE) 

# adding a title
roll_dice(times = 10, agg = TRUE) %>% 
  roll_dice(times = 20, agg = TRUE) %>% 
  plot_success(title = "Dice 10x vs 20x")

# comparing 3 experiments

roll_dice(times = 60, rounds = 10000, agg = TRUE) %>% 
  roll_dice(times = 70, rounds = 10000, agg = TRUE) %>% 
  roll_dice(times = 100, rounds = 10000, agg = TRUE) %>% 
  plot_success()

# rolling a dice 60 times, repeat it 100 rounds
roll_dice(times = 60, rounds = 100) %>% 
  describe(result)

# rolling an unfair dice 60 times, repeat it 100 rounds
roll_dice(times = 60, rounds = 100, prob = c(1/12,1/6,1/6,1/6,1/6,1/6+1/12)) %>%
  explore(result)
