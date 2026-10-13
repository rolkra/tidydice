context("plot_dice")
library(tidydice)

# draw the plot, return warnings (a ggplot only warns when it is drawn)
draw_warnings <- function(p) {
  warnings <- character()
  withCallingHandlers(
    ggplot2::ggsave(tempfile(fileext = ".png"), p, width = 3, height = 2, dpi = 30),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  warnings
}

test_that("plot_dice draws results that are not a dice face", {
  # sums, results above 6 or below 1, decimals: no warnings
  expect_equal(draw_warnings(plot_dice(roll_dice_formula("3d6", times = 3, seed = 1))), character(0))
  expect_equal(draw_warnings(plot_dice(roll_dice_formula("1d20", times = 3, seed = 1))), character(0))
  expect_equal(draw_warnings(plot_dice(roll_dice_formula("1d6-3", times = 6, seed = 1))), character(0))
  expect_equal(draw_warnings(plot_dice(roll_dice_formula("1d6/4", times = 6, seed = 1))), character(0))
  expect_equal(draw_warnings(plot_dice(roll_dice(times = 4, sides = 8, seed = 5))), character(0))
  expect_equal(draw_warnings(plot_dice(roll_dice_formula("3d6", times = 3, seed = 1), 
                                       detailed = TRUE)), character(0))
  
  # a decimal is not drawn as the face of its integer part
  p <- plot_dice(roll_dice_formula("1d1/2"))
  layers <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomText" %in% layers)
  
  # faces 1..6 are still drawn with dots, without a number
  p <- plot_dice(roll_dice_formula("1d1+2"))
  layers <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomText" %in% layers)
})

test_that("plot_dice draws every die of a formula", {
  d <- roll_dice_formula("4d6pl1", times = 3, rounds = 2, detail = TRUE, seed = 1)
  p <- plot_dice(d)
  expect_equal(draw_warnings(p), character(0))
  
  # one tile for each die (dropped ones too) and for each dot: more tiles than 
  # plotting one die for each roll
  count_tiles <- function(p) sum(vapply(p$layers, function(l) class(l$geom)[1] == "GeomTile", logical(1)))
  expect_gt(count_tiles(p), count_tiles(plot_dice(d, by_die = FALSE)))
  
  # dropped dice have their own color
  fills <- vapply(p$layers, function(l) {
    f <- l$aes_params$fill
    if (is.null(f)) NA_character_ else f
  }, character(1))
  expect_equal(sum(fills == "grey85", na.rm = TRUE), 6) # one dropped die in 6 rolls
  red <- vapply(plot_dice(d, fill_dropped = "red")$layers, 
                function(l) identical(l$aes_params$fill, "red"), logical(1))
  expect_equal(sum(red), 6)
  
  # exploding and several groups, detailed dice
  d <- roll_dice_formula("2d6e6+1d4", times = 2, rounds = 2, detail = TRUE, seed = 9)
  expect_equal(draw_warnings(plot_dice(d, detailed = TRUE)), character(0))
  
  # by_die = FALSE plots a roll as one dice, as before
  expect_equal(draw_warnings(plot_dice(d, by_die = FALSE)), character(0))
  
  # without the dice it is not possible, and the default is one dice for each roll
  expect_error(plot_dice(roll_dice_formula("2d6"), by_die = TRUE), "detail = TRUE")
  expect_equal(draw_warnings(plot_dice(roll_dice_formula("2d6", times = 3))), character(0))
  
  # limits
  expect_error(plot_dice(roll_dice_formula("5d6", times = 5, detail = TRUE)), 
               "more than 20 dice per round")
  expect_error(plot_dice(roll_dice_formula("1d6", rounds = 11, detail = TRUE)), 
               "more than 10 rounds")
})

test_that("plot_coin and plot_binom draw without warnings", {
  expect_equal(draw_warnings(plot_coin(flip_coin(times = 3, rounds = 2, seed = 1))), character(0))
  expect_equal(draw_warnings(plot_coin(roll_dice_formula("1d2", times = 3, seed = 1), 
                                       detailed = TRUE)), character(0))
  expect_equal(draw_warnings(plot_binom(binom_dice(times = 10, prob = c(1, 1, 1, 1, 1, 3)))), 
               character(0))
})

test_that("plot_coin writes the result on the coins", {
  texts <- function(p) {
    unlist(lapply(p$layers, function(l) if (inherits(l$geom, "GeomText")) l$data$label), 
           use.names = FALSE)
  }
  d <- force_coin(c(1, 2, 2))
  expect_equal(texts(plot_coin(d)), c("1", "2", "2"))
  expect_null(texts(plot_coin(d, show_result = FALSE)))
  # results of formulas
  expect_equal(texts(plot_coin(roll_dice_formula("1d1+1", times = 2))), c("2", "2"))
  expect_equal(draw_warnings(plot_coin(flip_coin(times = 4, rounds = 2, seed = 3))), character(0))
})
