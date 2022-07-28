#' Simulating flipping a coin.
#'
#' Flipping a coin is simulated using sample(). The default coin has 2 sides and is fair.
#' The properties of the coin can be changed.
#' The result is returned as a tibble.
#'
#' @param data Data from a previous experiment
#' @param times How many times coin is flipped (or how many coins are flipped at the same time)
#' @param rounds Number of rounds 
#' @param success Which result is a success (default = 2)
#' @param agg If TRUE, the result is aggregated (by experiment, rounds)
#' @param sides Number of sides of the coin (default = 2)
#' @param prob Vector of probabilities for each side of the coin
#' @param seed Seed to produce reproducible results
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

flip_coin <- function(data = NULL, times = 1, rounds = 1, success = c(2), agg = FALSE, sides = 2, prob = NULL, seed = NULL)  {

  # coin = dice with 2 sides
  roll_dice(data = data,
            times = times, 
            rounds = rounds, 
            success = success, 
            agg = agg, 
            sides = sides, 
            prob = prob,
            seed = seed)
}

#' Force a coin flipping result.
#'
#' The forced result is returned as a tibble.
#'
#' @param data Data from a previous experiment
#' @param result Vector of flipping coin results
#' @param round Round of flipping coin
#' @param experiment Experiment Number 
#' @param success Which result is a success (default = 6)
#' @return Result of experiment as a tibble
#' @importFrom magrittr "%>%"
#' @examples
#' force_coin(6)
#' force_coin(1:6)
#' @export

force_coin <- function(data = NULL, result = 6, round = 1, experiment = 1, success = 2)  {
 
  force_dice(data, result, round, experiment, success)
   
} # force_coin
