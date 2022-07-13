##############################################################################
## parse_dice_formula_part
##############################################################################
parse_dice_formula_part <- function(dice_formula_part){
  
  dice_base = str_match_all(
    string=dice_formula_part,
    pattern="^(\\d*)?([dD]*)(\\d*)") %>%
    .[[1]] %>%
    as_tibble(.name_repair="minimal") %>%
    set_names(c("raw_set", "operator", "selector", "value")) %>%
    mutate(value=as.numeric(value)) %>%
    mutate(
      value = case_when(
        is.na(value) ~ as.numeric(operator),
        T ~ value),
      operator = case_when(
        operator == "" ~ "1",
        T ~ operator
      )
    )
  
  dice_filters = str_match_all(
    string=dice_formula_part, 
    pattern="([kKeEpP]|rr|ro|ra|mi|ma)([HhlL><]*)(\\d*)") %>% 
    .[[1]] %>%
    as_tibble(.name_repair="minimal") %>%
    set_names(c("raw_set", "operator", "selector", "value")) %>%
    mutate(value=as.numeric(value))
  
  
  bind_rows(
    dice_base,
    dice_filters
  )
}

##############################################################################
## parse_dice_formula
##############################################################################
#' 
#' Based on https://github.com/avrae/d20
parse_dice_formula <- function(dice_formula) {

  tibble(dice_formula_part = c(str_split(string=dice_formula, pattern = "\\s*[+*-/]\\s*", simplify=T))) %>% 
    rowwise %>% 
    mutate(parts = list(parse_dice_formula_part(dice_formula_part))) %>% 
    unnest(parts)
  

# 
#   dice_ops = str_match_all(
#       string=dice_formula,
#       pattern="\\s*([+-/*^][*]*)(\\s*)(\\d*)") %>%
#     .[[1]] %>%
#     as_tibble(.name_repair="minimal") %>%
#     set_names(c("raw_set", "operator", "selector", "value")) %>%
#     mutate(value=as.numeric(value))
  
}
##############################################################################
## roll_dice_formula
##############################################################################
#' Simulating rolling a dice, using a formula
#' 
#' Example dice_formula:
#'     1d6      > roll one 6-sided dice
#'     1d8      > roll one 8-sided dice
#'     1d12     > roll one 12-sided dice
#'     2d6      > roll two 6-sided dice
#'     1d6e6    > roll one 6-sided dice, explode dice on a 6
#'     3d6kh2   > roll three 6-sided dice, keep top 2 rolls
#'     3d6kl2   > roll three 6-sided dice, keep lowest 2 rolls
#'     4d6kh3e6 > roll four 6-sided dice, keep top 3 rolls, but explode on a 6
#'     1d20+4   > roll one 20-sided dice, and add 4
#'     
#' @param dice_formula 
#' @param times How many times a dice is rolled (or how many dice are rolled at the same time)
#' @param rounds Number of rounds 
#' @param success Which result is a success (default = 6)
#' @param agg If TRUE, the result is aggregated (by experiment, rounds) (not implemented)
#' @param sides Number of sides of the dice (default = 6)
#' @param prob Vector of probabilities for each side of the dice
#' @param seed Seed to produce reproducible results
#' @param label Custom text to distinguish an experiment, can be used for plotting etc.
#' @return Result of experiment as a tibble
#' @import stringr  
roll_dice_formula <- function(data=NULL,
                              dice_formula = "1d6", 
                              times = 1, 
                              rounds = 1,
                              seed=NULL, 
                              prob=NULL, 
                              success = c(6),
                              agg=FALSE,
                              label=NULL
                              ) {
  assertthat::assert_that(is.character(dice_formula), msg = "dice_formula must be character")

  # check seed parameter
  if (!missing(seed)) {
    set.seed(seed)
  }
  # check if first parameter is dice_formula instead of data
  if (!missing(data) & is.character(data))  {
    dice_formula <- data
    data <- NULL
    assertthat::assert_that(is.character(dice_formula))
  }
  if (missing(label)) {
    label=dice_formula
  }
  # check if first parameter is times instead of data
  if (!missing(data) & is.numeric(data))  {
    times <- data
    data <- NULL
    assertthat::assert_that(times > 0)
  }
  
  # Parse Dice Type (1d6)
  dicestr1 = str_match(string=dice_formula, pattern="(\\d*)?[dD](\\d*)")
  assertthat::assert_that(is.character(dicestr1[1,1]), 
                          msg = "dice_formula need to contain at least one d statement")
  dice_count  = as.numeric(dicestr1[1,2])
  if (is.na(dice_count)) {dice_count = 1}
  dice_sides  = as.numeric(dicestr1[1,3])
  dice_intervals = 1:dice_sides
  assertthat::assert_that(dice_count >= 1, 
                          msg = "cannot roll 0 dice!")
  assertthat::assert_that(dice_sides >= 1, 
                          msg = "cannot roll a d0!")
  
  
  # Parse Exploding Dice
  dicestr_expl = str_match(string=dice_formula, pattern="[eE](\\d*)")
  dice_exploding_number  = as.numeric(dicestr_expl[1,2])
  dice_intervals = setdiff(dice_intervals, dice_exploding_number)
  if (!is.na(dice_exploding_number)) {
    assertthat::assert_that(dice_exploding_number <= dice_sides, 
                            msg = "invalid exploding dice specification")
    assertthat::assert_that(!((dice_exploding_number == 1) & (dice_sides  %in% 1:2)), 
                            msg = "1d1e1 and 1d2e1 not implemented")
  } else {
    dice_exploding_number = 0
  }
  
  # Parse Keep Higher/Lower
  dice_khl = str_match(string=dice_formula, pattern="[kK]([HhlL])(\\d*)")
  dice_khl_sign = case_when(
    is.na(dice_khl[1,2]) ~ T,
    dice_khl[1,2] %in% c("h", "H") ~ T,
    dice_khl[1,2] %in% c("l", "L") ~ F
  )
  dice_khl_n = as.numeric(ifelse(is.na(dice_khl[1,3]), dice_count, dice_khl[1,3]))
  assertthat::assert_that(dice_khl_n <= dice_count, 
                          msg = "invalid kh/kl formula, can't keep more dice than rolled")
  assertthat::assert_that(dice_khl_n > 0, 
                          msg = "invalid kh/kl formula, can't keep less than 1 die")

  # Parse [+-*/]
  dice_op = str_match(string=dice_formula, pattern="\\s*([+-/*^][*]*)\\s*(\\d*)")
  dice_op_sign = dice_op[1,2]
  dice_op_n = as.numeric(dice_op[1,3])

  if (agg){
    stop("Not Implemented Yet") # Probably implement this as a summarise
  } else {
    result_df <- tibble::tibble(
      round = as.integer(rep(1:rounds, each = times)),
      nr    = as.integer(rep(1:times, times = rounds)),
      result_t = 
        tibble(
          round = as.integer(rep(1:rounds, each = times)),
          nr    = as.integer(rep(1:times, times = rounds)),
          diceroll = list(
            c(
              sample( x = dice_intervals, size=dice_count, replace=T), 
              rep(dice_exploding_number, times = dice_exploding_number*rgeom(1, 1-1/2))
            )
        )),
      result_m =
        matrix(
          sample(x = dice_intervals, 
                 size = dice_count * rounds * times, 
                 replace = TRUE,
                 prob=prob
          ), 
          ncol=dice_count),
      result = apply(
            result_m,
                1,
                top_n_dice, n=dice_khl_n, dec=dice_khl_sign
                )
              ) 
    if (!is.na(dice_exploding_number)){
      # Exploding Dice is implemented using a Geom distribution to predict the number of outcomes
      result_df = result_df %>% 
        mutate(result = 
                 result + 
                 dice_exploding_number * 
                 rowSums(
                  matrix(
                    rgeom(dice_count*nrow(result_df), 1-1/dice_sides), # this prob is wrong, it should scale with the n of dice
                    ncol=dice_count)
                  )
               )
    }
    # arithmetic operations
    result_df = result_df %>%
      mutate(
        result = case_when(
          dice_op_sign == "+" ~ result + dice_op_n,
          dice_op_sign == "-" ~ result - dice_op_n,
          dice_op_sign == "*" ~ result * dice_op_n,
          dice_op_sign == "/" ~ result / dice_op_n,
          dice_op_sign %in% c("^", "**") ~ result ** dice_op_n,
          T ~ as.numeric(result),
        )
      )
      
  }
  # Compute success
  result_df = result_df %>%
    mutate(success = result %in% success)
  
  # Format the result df before returning it
  result_df = result_df %>% 
    mutate(experiment_id = 1,
           dice_formula = dice_formula,
           label=label) %>%
    select(experiment_id, dice_formula, label, round, nr, result, success, -result_m) 
  
  if (missing(data))  {
    
    # result of roll_dice (first experiment)
    result_df <- result_df %>% 
      dplyr::mutate(experiment = as.integer(1)) %>% 
      dplyr::select(experiment, dplyr::everything())
    result_df
    
  } else {
    
    # existing experiment variable?
    max_experiment <- 0L
    if ("experiment" %in% names(data)) {
      max_experiment <- max(data$experiment)
    }
    
    # new experiment (+1)
    result_df <- result_df %>% 
      dplyr::mutate(experiment = as.integer(max_experiment + 1))
    
    # bind result to data (pipe)
    result_df <- dplyr::bind_rows(data, result_df) %>% 
      dplyr::select(experiment, dplyr::everything())
    result_df
  }
}

top_n_dice = function(x, n, dec=F) {
  sum(x[order(x, decreasing = dec)][1:n], na.rm=T)
}

##############################################################################
## roll_dice
##############################################################################
#' Simulating rolling a dice.
#'
#' Rolling a dice is simulated using sample(). The default dice has 6 sides and is fair.
#' The properties of the dice can be changed.
#' The result is returned as a tibble.
#'
#' @param data Data from a previous experiment
#' @param times How many times a dice is rolled (or how many dice are rolled at the same time)
#' @param rounds Number of rounds 
#' @param success Which result is a success (default = 6)
#' @param agg If TRUE, the result is aggregated (by experiment, rounds)
#' @param sides Number of sides of the dice (default = 6)
#' @param prob Vector of probabilities for each side of the dice
#' @param seed Seed to produce reproducible results
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

roll_dice <- function(data = NULL, times = 1, rounds = 1, success = c(6), agg = FALSE, sides = 6, prob = NULL, seed = NULL)  {
  
  # check if meaningful parameters
  assertthat::assert_that(is.numeric(times), msg = "times must be numeric")
  assertthat::assert_that(is.numeric(rounds),msg = "rounds must be numeric")
  assertthat::assert_that(is.numeric(sides), msg = "sides must be numeric")
  assertthat::assert_that(is.logical(agg),   msg = "agg must be logical")
  assertthat::assert_that(length(times) == 1)
  assertthat::assert_that(length(rounds) == 1)
  assertthat::assert_that(length(sides) == 1)
  assertthat::assert_that(times > 0,         msg = "times must be greater than 0")
  assertthat::assert_that(rounds > 0,        msg = "rounds must be greater than 0")
  assertthat::assert_that(sides > 0,         msg = "sides must be greater than 0")

  # make sure that parameters are integer
  times <- floor(times)
  rounds <- floor(rounds)
  success <- floor(success)
  
  # check if first parameter is times instead of data
  if (!missing(data) & is.numeric(data))  {
    times <- data
    data <- NULL
    assertthat::assert_that(times > 0)
  }

  # check seed parameter
  if (!missing(seed)) {
    set.seed(seed)
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
    max_experiment <- 0L
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

##############################################################################
## force_dice
##############################################################################
#' Force a dice rolling result.
#'
#' The forced result is returned as a tibble.
#'
#' @param data Data from a previous experiment
#' @param result Vector of rolling dice results
#' @param round Round of rolling dice
#' @param experiment Experiment Number 
#' @param success Which result is a success (default = 6)
#' @return Result of experiment as a tibble
#' @importFrom magrittr "%>%"
#' @examples
#' force_dice(6)
#' force_dice(1:6)
#' @export

force_dice <- function(data = NULL, result = 6, round = 1, experiment = 1, success = 6)  {

  # passing result without data  
  if (!missing(data) & is.numeric(data))  {
    result <- data
    data <- NULL
  }

  length <- length(result)
  if (length < 0)  {
    stop("result must contain a numeric value")  
  }
  
  data_new <- tibble::tibble(
                  experiment = as.integer(rep(experiment, times = length)),
                  round = as.integer(rep(round, times = length)),
                  nr = as.integer(1:length),
                  result = as.integer(result))

  # determine success
  data_new <- data_new %>%
    dplyr::mutate(success = ifelse(result %in% success, TRUE, FALSE))
  
  # add to data
  data <- dplyr::bind_rows(data, data_new) %>% 
    dplyr::select(experiment, dplyr::everything())
  
  # return data
  data
  
} # force_dice

##############################################################################
## force_coin
##############################################################################
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
  