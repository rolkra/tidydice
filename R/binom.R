##############################################################################
## binom_dice
##############################################################################
#' Binomial distribution of rolling a dice.
#'
#' Generates a tibble containing the binomial distribution of rolling the dice.
#'
#' @param times How many times a dice is rolled (or how many dice are rolled at the same time)
#' @param success Which result is a success (default = 6)
#' @param sides Number of sides of the dice (default = 6)
#' @return Binomial distribution as a tibble
#' @examples
#' binom_dice(times = 10)
#' @export

binom_dice <- function(times, sides = 6, success = 6) {
  
  # check if meaningful parameters
  assertthat::assert_that(is.numeric(times), msg = "times must be numeric")
  assertthat::assert_that(is.numeric(sides), msg = "sides must be numeric")
  assertthat::assert_that(is.numeric(success), msg = "success must be numeric")
  assertthat::assert_that(length(times) == 1)
  assertthat::assert_that(length(sides) == 1)
  assertthat::assert_that(times > 0,         msg = "times must be greater than 0")
  assertthat::assert_that(sides > 0,         msg = "sides must be greater than 0")
  
  # make sure that parameters are integer
  times <- floor(times)
  sides <- floor(sides)
  success <- floor(success)
  
  # prepare binomial
  n_success <- sum(success <= sides)
  
  # binomial distribution
  x_seq <- 1:times
  #p <- purrr::map_dbl(x_seq, ~dbinom(size = times, x =  .x, p = n_success/sides))  
  p <- stats::dbinom(size = times, x = x_seq, prob = n_success/sides)
  tbl <- tibble(success = x_seq, p = p, pct = round(p *100.00, 2))
  
  # return result
  tbl
  
} # binom_dice

##############################################################################
## binom_coin
##############################################################################
#' Binomial distribution of flipping a coin.
#'
#' Generates a tibble containing the binomial distribution of flipping a coin
#'
#' @param times how many times a coin is flipped (or how many coins are flipped at the same time)
#' @param success which result is a success (default = 2)
#' @param sides number of sides of the coin (default = 2)
#' @return binomial distribution as a tibble
#' @examples
#' binom_coin(times = 10)
#' @export

binom_coin <- function(times, sides = 2, success = 2) {
  
  binom_dice(times, sides, success)
  
} # binom_coin 

##############################################################################
## plot_binom
##############################################################################
#' Plot a binomial distribution.
#'
#' Plot a binomial distribution generated with dice_binom() or coin_binom()
#'
#' @param data data containing values for binomial distribution
#' @param title title of the plot
#' @param color color of bars
#' @param label add labels to plot?
#' @param label_size size of label
#' @param min_pct surpress values < min_pct
#' @return ggplot object
#' @examples
#' plot_binom(data = binom_dice(times = 10))
#' @export
#' 
plot_binom <- function(data , title = "Binomial distribution", color = "darkgrey", label = TRUE, label_size = 3, min_pct = 0.05)  {
  
  assertthat::assert_that("success" %in% names(data), msg = "success not found in data")
  assertthat::assert_that("pct" %in% names(data), msg = "pct not found in data")
  
  # define variables to pass CRAN checks
  pct <- NULL
  success <- NULL
  
  # drop if pct < 0.05
  data <- data %>% filter(pct >= min_pct)
  
  # plot
  p <- data %>% ggplot(aes(success, pct)) + 
    geom_col(fill = color) + 
    ylim(0, max(data$pct * 1.1)) +
    ylab("percent") +
    theme_light()
  
  # label
  if (label) {
    p <- p + geom_text(aes(x = success, y = pct, label = pct), 
                       size = label_size, vjust = 0, nudge_y = 0.1)
  }  
  
  # add title
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  
  # return result
  p
  
} # plot_binom

