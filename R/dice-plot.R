#' Helper function to draw a circle
#'
#' @param center Vector with x and y coordinate of center
#' @param diameter Diameter of circle
#' @param npoints Number of points used for drawing a circle
#' @return Dataframe with x and y coordinates to draw a circle 

circle_points <- function(center = c(0, 0), diameter = 1, npoints = 61)  {
  
  r <- diameter / 2
  tt <- seq(0, 2*pi, length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
  
} # circle_points

#' Draw a single dice
#'
#' @param ggplot ggplot-Object. If passed, the dice will be added to plot
#' @param result Result of dice rolling (0..6)
#' @param x X-coordinate of dice (center)
#' @param y y-coordinate of dice (center)
#' @param width Width of dice
#' @param fill Fill color
#' @param detailed If TRUE, the dice is plotted with more details
#' @param rounding Rounding of dice (only used if detailed == TRUE)
#' @param line_size Size of Lines
#' @param line_color Color of Lines
#' @param point_size Size of Points
#' @param point_color Color of Points
#' @return ggplot-Object 
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import ggplot2

plot_single_dice <- function(ggplot = NULL, result = 6, x = 0, y = 0, width = 0.9, fill = "white", detailed = FALSE, rounding = width/5, line_size = 0.8, line_color = "black", point_size = width/6, point_color = "black")  {
  
  dice_width = width / 2
  dice_rounding = rounding
  
  # left top
  circle_lt <- circle_points(diameter = dice_rounding * 2,
                             center = c(x - dice_width + dice_rounding, y + dice_width - dice_rounding))
  # left bottom
  circle_lb <- circle_points(diameter = dice_rounding * 2,
                             center = c(x - dice_width + dice_rounding, y - dice_width + dice_rounding))
  
  # right top
  circle_rt <- circle_points(diameter = dice_rounding * 2,
                             center = c(x + dice_width - dice_rounding, y + dice_width - dice_rounding))
  # right bottom
  circle_rb <- circle_points(diameter = dice_rounding * 2,
                             center = c(x + dice_width - dice_rounding, y - dice_width + dice_rounding))
  
  # line top (from right to left, to prevent small step in line)
  line_t <- data.frame(x = c(x + dice_width - dice_rounding, x - dice_width + dice_rounding), 
                       y = c(y + dice_width, y + dice_width))
  # line bottom
  line_b <- data.frame(x = c(x - dice_width + dice_rounding, x + dice_width - dice_rounding), 
                       y = c(y - dice_width, y - dice_width))
  # line left
  line_l <- data.frame(x = c(x - dice_width, x - dice_width), 
                       y = c(y + dice_width - dice_rounding, y - dice_width + dice_rounding))
  # line right
  line_r <- data.frame(x = c(x + dice_width, x + dice_width), 
                       y = c(y + dice_width - dice_rounding, y - dice_width + dice_rounding))
  
  # dice outside
  dice_cube_round <- rbind(circle_lt[16:29,], line_l, 
                           circle_lb[31:44,], line_b,
                           circle_rb[46:60,], line_r, 
                           circle_rt[1:14,], line_t) 
  
  #dice_cube_round <- tail(dice_cube_round, -1)
  
  # dice outside (simple rectangle)
  dice_cube_simple <- data.frame(x = c(x-dice_width, x+dice_width, x+dice_width, x-dice_width, x-dice_width),
                                 y = c(y+dice_width, y+dice_width, y-dice_width, y-dice_width, y+dice_width))
  # points
  points_x <-rep(c(x - dice_width/2, x + 0, x + dice_width/2), 3)
  points_y <-c(rep(y + dice_width/2,3), rep(y + 0, 3), rep(y - dice_width/2,3))
  
  # define dots for each result
  points_dots <- list(c(0,0,0,0,1,0,0,0,0),   # result 1
                      c(0,0,1,0,0,0,1,0,0),   # result 2
                      c(0,0,1,0,1,0,1,0,0),   # result 3
                      c(1,0,1,0,0,0,1,0,1),   # result 4
                      c(1,0,1,0,1,0,1,0,1),   # result 5
                      c(1,1,1,0,0,0,1,1,1))   # result 6
  
  # plot result 1..6 as dots, any other result (e.g. a sum, or a decimal) as number
  is_face <- is.finite(result) && result == round(result) && result >= 1 && result <= 6
  if (is_face) {
    
    points <- data.frame(x = points_x[points_dots[[result]] == 1],
                         y = points_y[points_dots[[result]] == 1])
  } else {
    
    points <- data.frame(x = integer(),
                         y = integer())
    
  }
  
  # plot new dice    
  if (!missing(ggplot))  {
    # plot dice  
    
    if (detailed) {
      p <- ggplot +
             geom_polygon(data = dice_cube_round, aes(x, y), 
                          color = line_color, linewidth = line_size*0.9, fill = fill)
    } else {
      p <- ggplot +
             geom_tile(data = data.frame(x=x, y=y), aes(x,y), 
                       width = dice_width * 1.9, height = dice_width * 1.9,
                       color = line_color, linewidth = line_size * 0.9, fill = fill)
    }
  } else  {
      # add plot dice  
    if (detailed) {
      p <- ggplot() +
        geom_polygon(data = dice_cube_round, aes(x, y), 
                     color = line_color, linewidth = line_size*0.9, fill = fill)
    } else {
      p <- ggplot() +
        geom_tile(data = data.frame(x=x, y=y), aes(x,y), 
                  width = dice_width * 1.9, height = dice_width * 1.9,
                  color = line_color, linewidth = line_size * 0.9, fill = fill)
    } #if  
  } #missing ggplot
  
  # plot dots
  for (i in seq_len(nrow(points)))  {
    x <- points$x[i]
    y <- points$y[i]
    if (detailed)  {
        dice_dot <- circle_points(center = c(x, y), diameter = point_size)
        p <- p + geom_polygon(data = dice_dot, aes(x, y), color = point_color, fill = point_color)
    } else {
        #dice_dot <- data.frame(x = c(x-point_size/2*0.9, x+point_size/2*0.9, x+point_size/2*0.9, x-point_size/2*0.9, x-point_size/2*0.9),
        #                       y = c(y+point_size/2*0.9, y+point_size/2*0.9, y-point_size/2*0.9, y-point_size/2*0.9, y+point_size/2*0.9))
        dice_dot <- data.frame(x = x, y = y)
        p <- p + geom_tile(data = dice_dot, aes(x, y), width = point_size, height = point_size, color = point_color, fill = point_color)
    } #if detailed
    #p <- p + geom_polygon(data = dice_dot, aes(x, y), color = point_color, fill = point_color)
    #p <- p + geom_path(data = dice_dot, aes(x, y), color = point_color, lineend = line_end)
  } #for
    
  # results that are not a dice face: write the number on the dice
  if (!is_face) {
    label <- NULL # to pass CRAN checks
    p <- p + geom_text(data = data.frame(x = x, y = y, label = format(round(result, 2))),
                       aes(x, y, label = label), color = point_color, 
                       size = if (detailed) 4 else 3.5)
  }
  
  # fix coordinates
  if (missing(ggplot))  { 
    p <- p +
      coord_fixed() +
      theme_void()
  } #if
  
  # output
  p
  
} # plot_single_dice

#' Plot result of roll_dice()
#'
#' @param data result of roll_dice() or roll_dice_formula()
#' @param detailed If TRUE, the dice is plotted with more details
#' @param fill Fill color
#' @param fill_success Fill color if result is a success
#' @param fill_dropped Fill color of dice that were dropped (e.g. the lowest of `4d6pl1`)
#' @param by_die If TRUE, every die that was rolled is plotted, and the dice of a roll 
#'   are grouped together. Needs data from roll_dice_formula(detail = TRUE), 
#'   and is the default for it. If FALSE, a roll is plotted as one dice, 
#'   showing the result as dots (1 to 6) or as a number.
#' @param point_color Color of Points
#' @param line_color Color of Lines
#' @param line_size Size of Lines
#' @return ggplot-Object 
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import ggplot2
#' @examples
#' library(magrittr)  
#' plot_dice()
#' roll_dice(times = 3, rounds = 3) %>% plot_dice()
#' roll_dice(times = 3, rounds = 3) %>% plot_dice(fill_success = "red")
#' 
#' # plot every die of a formula
#' roll_dice_formula("4d6pl1", times = 3, rounds = 3, success = 15:18, detail = TRUE) %>% 
#'   plot_dice()
#' @export

plot_dice <- function(data, detailed = FALSE, fill = "white", fill_success = "gold", point_color = "black", line_color = "black", line_size = 0.8, fill_dropped = "grey85", by_die = NULL)  {
  
  # check data
  if (missing(data))  {
    data <- roll_dice()
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
  
  # plot every die of a roll?
  if (is.null(by_die)) {
    by_die <- "dice" %in% names(data)
  }
  if (by_die && !"dice" %in% names(data)) {
    stop("by_die needs the dice that were rolled, use roll_dice_formula(detail = TRUE)")
  }
  
  if (by_die) {
    dice_per_round <- tapply(
      vapply(data$dice, nrow, integer(1)) , data$round, sum)
    if (any(dice_per_round > 20)) {
      stop("can't plot more than 20 dice per round")
    }
  } else if (length(nr) > 10)  {
    stop("can't plot more than 10 rolls per round")
  }
  
  p <- ggplot() + coord_fixed() + theme_void()
  pos_x <- 1
  pos_y <- 1
  
  for (ii in seq_along(rounds)) {
    
    tmp <- data[data$round == rounds[[ii]], ] 
    
    for (i in seq_along(tmp$result))  {
      
      if (by_die) {
        
        # every die of the roll, dropped dice in another color
        roll_dice <- tmp$dice[[i]]
        for (j in seq_len(nrow(roll_dice))) {
          p <- p %>% plot_single_dice(
            result = roll_dice$value[[j]], 
            x = pos_x, 
            y = pos_y,
            detailed = detailed,
            fill = if (!roll_dice$kept[[j]]) fill_dropped else 
              ifelse(tmp$success[[i]], fill_success, fill),
            point_color = point_color,
            line_color = line_color)
          pos_x <- pos_x + 1
        } # for j
        
        # space between the rolls
        pos_x <- pos_x + 0.5
        next
      }
      
      p <- p  %>% plot_single_dice(result = tmp$result[[i]], 
                                   x = pos_x, 
                                   y = pos_y,
                                   detailed = detailed,
                                   fill = ifelse(tmp$success[[i]],fill_success,fill),
                                   point_color = point_color,
                                   line_color = line_color)
      pos_x <- pos_x + 1
      
    } # for i
    
    # next round
    pos_x <- 1
    pos_y <- pos_y - 1
    
  } # for ii
  # plot all dice  
  p + ggtitle(paste0(
    "Success: ", sum(data$success), " of ", nrow(data),
    " (", round(100 * sum(data$success) / nrow(data), 1), "%)"
  ))
  
} # plot_dice
