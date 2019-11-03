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
#' @param prob Vector of probabilities for each side of the dice
#' @return Result of experiment as a tibble
#' @importFrom magrittr "%>%"
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

roll_dice <- function(data = NULL, times = 1, rounds = 1, success = c(6), agg = FALSE, sides = 6, prob = NULL)  {
  
  # check if possible
  assertthat::assert_that(times > 0)
  assertthat::assert_that(rounds > 0)
  assertthat::assert_that(sides > 0)
  
  # make sure that parameters are integer
  times <- floor(times)
  rounds <- floor(rounds)
  success <- floor(success)
  
  # check if first parameter is times instead of data
  if (!missing(data) & is.numeric(data))  {
    times <- data
    data <- NULL
  }
  
  # definition to pass CRAN test
  experiment <- 0L
  
  if (agg)  {
    
    # roll dice and aggregate result
    rounds_seq <- 1:rounds
    success <- purrr::map_int(rounds_seq, ~sum(sample(x = 1:sides, size = times, replace = TRUE, prob = prob) %in% success))
    
    # create result tibble
    result <- tibble::tibble(round = as.integer(rounds_seq),
                             times = as.integer(rep(times, rounds)),
                             success = as.integer(success))
    
  } else { 
    
    # roll the dice: rounds x times
    result <- tibble::tibble(round = as.integer(rep(1:rounds, each = times)),
                             nr = as.integer(rep(1:times, times = rounds)),
                             result = as.integer(sample(x = 1:sides, size = rounds * times, replace = TRUE, prob = prob))
    )
    
    # determine success
    result <- result %>%
      dplyr::mutate(success = ifelse(result %in% success, TRUE, FALSE))
    
  } # if agg
  
  if (missing(data))  {
    
    # result of roll_dice (first experiment)
    result <- result %>% 
      dplyr::mutate(experiment = as.integer(1)) %>% 
      dplyr::select(experiment, dplyr::everything())
    result
    
  } else {
    
    # existing experiment variable?
    max_experiment <- 1L
    if ("experiment" %in% names(data)) {
      max_experiment <- max(data$experiment)
    }
    
    # new experiment (+1)
    result <- result %>% 
      dplyr::mutate(experiment = as.integer(max_experiment + 1))
    
    # bind result to data (pipe)
    result <- dplyr::bind_rows(data, result) %>% 
      dplyr::select(experiment, dplyr::everything())
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
#' @param prob Vector of probabilities for each side of the coin
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

flip_coin <- function(data = NULL, times = 1, rounds = 1, success = c(2), agg = FALSE, sides = 2, prob = NULL)  {
  
  # coin = dice with 2 sides
  roll_dice(data = data,
            times = times, 
            rounds = rounds, 
            success = success, 
            agg = agg, 
            sides = sides, 
            prob = prob)
}

