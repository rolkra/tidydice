#' Draw a single coin
#'
#' @param ggplot ggplot-Object. If passed, the dice will be added to plot
#' @param result Result of flip coin (0/1)
#' @param x X-coordinate of dice (center)
#' @param y y-coordinate of dice (center)
#' @param width Width of coin
#' @param fill Fill color
#' @param detailed If TRUE, the dice is plotted with more details
#' @param line_size Size of Lines
#' @param line_color Color of Lines
#' @return ggplot-Object 
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import ggplot2

plot_single_coin <- function(ggplot = NULL, result = 1, x = 0, y = 0, width = 0.9, fill = "white", detailed = FALSE, line_size = 0.8, line_color = "black")  {
  
  circle <- circle_points(diameter = width,
                          center = c(x + width/2, y + width/2))
  
  # plot new dice    
  if (!missing(ggplot))  {
    # plot dice  
    p <- ggplot +
      geom_polygon(data = circle, aes(x,y), 
                   color = line_color, fill = fill, size = line_size) 
  } else  {
    # add plot dice  
    p <- ggplot() +
      geom_polygon(data = circle, aes(x,y,), color = line_color, fill = fill, size = line_size) 
  } #if  
  
  # return ggplot object
  p
  
} # plot_single_coin


#' Plot result of flip_coin()
#'
#' @param data result of flip_coin()
#' @param detailed not supported at moment
#' @param fill Fill color
#' @param fill_success Fill color if result is a success
#' @param line_color Color of Lines
#' @param line_size Size of Lines
#' @return ggplot-Object 
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import ggplot2
#' @examples
#' library(magrittr)  
#' plot_coin()
#' flip_coin(times = 3, rounds = 3) %>% plot_coin())
#' flip_coin(times = 3, rounds = 3) %>% plot_coin(fill_success = "red")
#' @export

plot_coin <- function(data, detailed = FALSE, fill = "white", fill_success = "gold", line_color = "black", line_size = 0.8)  {
  
  # check data
  if (missing(data))  {
    data <- flip_coin()
  }
  
  # check result
  if (!"result" %in% names(data))  {
    stop("data must contain result variable")
  }
  
  # check result
  if (!"experiment" %in% names(data))  {
    stop("data must contain experiment variable")
  }
  
  experiments <- unique(data$experiment)
  rounds <- unique(data$round)
  nr <- unique(data$nr)
  
  if (length(experiments) != 1)  {
    stop("can't plot more than one experiment")
  }
  
  if (length(rounds) > 10)  {
    stop("can't plot more than 10 rounds")
  }
  
  if (length(nr) > 10)  {
    stop("can't plot more than 10 rolls per round")
  }
  
  p <- ggplot() + coord_fixed() + theme_void()
  pos_x <- 1
  pos_y <- 1
  
  for (ii in seq_along(rounds)) {
    
    tmp <- data[data$round == rounds[[ii]], ] 
    
    for (i in seq_along(tmp$result))  {
      
      p <- p  %>% plot_single_coin(result = tmp$result[[i]], 
                                   x = pos_x, 
                                   y = pos_y,
                                   detailed = detailed,
                                   fill = ifelse(tmp$success[[i]],fill_success,fill),
                                   line_color = line_color)
      pos_x <- pos_x + 1
      
    } # for i
    
    # next round
    pos_x <- 1
    pos_y <- pos_y - 1
    
  } # for ii
  # plot all dice  
  p
  
} # plot_coin
