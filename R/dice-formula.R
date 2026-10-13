#' Given a dice formula string, split it and return a dataframe with the list 
#' of its parts.
#'
#' This parses a dice formula with the same parser as [roll_dice_formula()], 
#' see there for the syntax. It returns a tibble with one or more rows for each
#' group (a number, or dice with their modifiers) of the formula:
#' 
#' - `subgroup_id`: position of the group in the formula
#' - `subgroup_formula`: the group, with the operator before it (`+` for the first one)
#' - `subgroup_sign`: the operator before the group (`+ - * / ^ **`)
#' - `raw_set`: the text of one part of the group, e.g. `4d6`, `e6` or `kh3`
#' - `operator`, `selector`, `value`: the part, split. For the dice itself, 
#'   `operator` is the number of dice, `selector` is `d` and `value` the number
#'   of sides. For a modifier, `operator` is the modifier (e.g. `k`), 
#'   `selector` is `h`, `l`, `<`, `>` or empty and `value` its number. 
#'   A modifier with several selectors, or repeated modifiers, 
#'   get one row for each selector (`e5e6`, `k>3<2`).
#'   For a plain number, `operator` is the number and `value` too.
#' 
#' The groups are listed from left to right, without the precedence of 
#' the operators and the parentheses: `(1d6+2)*3` returns the groups `1d6`, 
#' `+2` and `*3`. 
#' 
#' The input can be a string containing specifications for multiple dice, e.g.:
#' - 1d6e6          -> roll 1 six-sided dice, explode on 6
#' - 1d6e6+2d4-1d10 -> Roll 1 six-sided dice, explode on 6, plus two 4-sided 
#'                    dice, subract one 10-sided dice
#' 
#' This is inspired by Avrae's bot syntax for rolling dice. See https://github.com/avrae/d20
#' 
#' @param dice_formula A string containing a dice formula, e.g. 1d6e6+1d4
#' @return A tibble
#' @importFrom tibble tibble
#' @export
#' @examples
#' parse_dice_formula("4d6e6kh3+2")
#' parse_dice_formula("1d8+1d6-1")

parse_dice_formula <- function(dice_formula) {

  assertthat::assert_that(is.character(dice_formula) && length(dice_formula) == 1, 
                          msg = "dice_formula must be a single character string")
  tree <- parse_dice_expression(dice_formula, require_dice = FALSE, validate = FALSE)
  
  groups <- flatten_dice_expression(tree)
  
  parts <- lapply(seq_along(groups), function(i) {
    group <- groups[[i]]
    node <- group$node
    
    if (node$type == "num") {
      rows <- tibble::tibble(raw_set = node$text, operator = node$text, 
                             selector = "", value = node$value)
    } else {
      rows <- tibble::tibble(
        raw_set = c(node$base_text, 
                    vapply(node$raw_mods, function(m) paste0(m$op, m$cat, m$num), 
                           character(1))),
        operator = c(as.character(node$count), 
                     vapply(node$raw_mods, function(m) m$op, character(1))),
        selector = c("d", vapply(node$raw_mods, function(m) m$cat, character(1))),
        value = c(node$sides, vapply(node$raw_mods, function(m) m$num, numeric(1))))
    }
    
    tibble::tibble(subgroup_id = i, 
                   subgroup_formula = paste0(group$sign, node$text),
                   subgroup_sign = group$sign) %>%
      dplyr::bind_cols(rows)
  })
  
  dplyr::bind_rows(parts)
}

# Flatten the tree of an expression into a list of groups (numbers and dice), 
# each with the operator written before it
flatten_dice_expression <- function(node, sign = "+") {
  switch(node$type,
    neg = flatten_dice_expression(
      node$x, 
      sign = switch(sign, "+" = "-", "-" = "+", paste0(sign, "-"))),
    binop = c(flatten_dice_expression(node$lhs, sign), 
              flatten_dice_expression(node$rhs, node$op_text)),
    list(list(sign = sign, node = node))
  )
}

# roll_dice_part <- function(current_set = c(), specs=c()){
#   
# }

#' Simulating rolling a dice, using a formula
#' 
#' @details
#' The syntax is based on [Avrae's d20 library](https://github.com/avrae/d20).
#' Spaces and case are ignored. 
#' 
#' **Dice**: `NdS` rolls N dice with S sides (`d6` = `1d6`, `d%` = `d100`). 
#' Dice can be combined with numbers using `+ - * / ^`, 
#' parentheses and the usual precedence, e.g. `(2d6+3)*2` or `1d8+1d6+2`. 
#' 
#' **Modifiers** follow the dice and are applied in the order they are written.
#' Each needs a selector:
#' 
#' | Modifier | Meaning |
#' |:--|:--|
#' | `k` | keep the dice matching the selector, drop the others |
#' | `p` | drop the dice matching the selector |
#' | `rr` | reroll dice matching the selector, until none match |
#' | `ro` | reroll dice matching the selector once |
#' | `ra` | roll one extra die (once) if a die matches the selector |
#' | `e` | explode: roll an extra die for each die matching the selector, 
#' and repeat for the new dice |
#' | `mi` | minimum: dice below the number are raised to it |
#' | `ma` | maximum: dice above the number are lowered to it |
#' 
#' **Selectors**: `N` (equal to N), `<N` (less than N), `>N` (greater than N), 
#' `hN` (the N highest dice) and `lN` (the N lowest dice). Several selectors can 
#' be combined (`k>3<2`), and so can consecutive identical modifiers 
#' (`e5e6` is the same as `e5>5`). `mi` and `ma` only take a plain number.
#' Only the dice that are not dropped can be selected.
#' 
#' Not supported (yet): comments, lists of numbers like `(1,2,3)kh1`, 
#' functions, `//` and `%` operators. Invalid syntax is an error.
#' There is no limit to the number of dice (Avrae stops at 1000), but formulas 
#' that would never end are an error: `rr` with `h` or `l` (the highest dice are 
#' always there to be rerolled), and rerolling (`rr`) or exploding (`e`) on sides 
#' that hold all the probability of an unfair dice. Dice that almost always explode
#' (e.g. a 99.999% chance) take a very long time.
#' 
#' @param data Data from a previous experiment
#' @param dice_formula Dice formula (e.g. "1d6" = 1 dice with 6 sides). 
#'   The syntax follows Avrae's dice bot (see Details).
#' @param times How many times a dice is rolled (or how many dice are rolled at the same time)
#' @param rounds Number of rounds 
#' @param success Which result is a success (default = 6)
#' @param agg If TRUE, the result is aggregated (by experiment, rounds) (not implemented)
#' @param prob Vector of probabilities for each side of the dice (unfair dice). 
#'   A vector is used for all the dice of the formula, so all of them need the 
#'   same number of sides. To mix dice, use a list with one vector 
#'   for each group of dice, from left to right (`NULL` = fair dice), 
#'   e.g. `prob = list(c(.5, .1, .1, .1, .1, .1), NULL)` for `"1d6+1d4"`.
#' @param seed Seed to produce reproducible results
#' @param label Custom text to distinguish an experiment, can be used for plotting etc.
#' @param detail If TRUE, the result has a column `dice` with the dice that were 
#'   rolled for each result: a tibble with the columns `group` (position of the 
#'   dice group in the formula), `sides`, `value` and `kept` (FALSE for dice dropped 
#'   by a modifier, e.g. the lowest die in `4d6pl1`). Rerolled dice show their last 
#'   value, and exploded dice are added. plot_dice() draws one die for each of them.
#'   The column is not created if `agg` is TRUE.
#' @return Result of experiment as a tibble
#' @export
#' @examples
#' # roll one 6-sided dice
#' roll_dice_formula(dice_formula = "1d6")
#' 
#' # roll one 8-sided dice
#' roll_dice_formula(dice_formula = "1d8")
#' 
#' # roll two 6-sided dice
#' roll_dice_formula(dice_formula = "2d6")
#' 
#' # roll two 6-sided dice, explode dice on a 6
#' roll_dice_formula(dice_formula = "2d6e6")
#' 
#' # roll three 6-sided dice, keep highest 2 rolls
#' roll_dice_formula(dice_formula = "3d6kh2")
#' 
#' # roll three 6-sided dice, keep lowest 2 rolls
#' roll_dice_formula(dice_formula = "3d6kl2")
#' 
#' # roll four 6-sided dice, keep highest 3 rolls, but explode on a 6
#' roll_dice_formula(dice_formula = "4d6kh3e6")
#' 
#' # explode on 5 or 6 (">" and "<" are strict, as in Avrae)
#' roll_dice_formula(dice_formula = "2d6e>4")
#' 
#' # roll four 6-sided dice, drop the lowest, like for D&D abilities
#' roll_dice_formula(dice_formula = "4d6pl1")
#' 
#' # reroll 1s until they are gone, reroll 2s once
#' roll_dice_formula(dice_formula = "3d6rr1ro2")
#' 
#' # minimum of 2 on each dice
#' roll_dice_formula(dice_formula = "4d6mi2")
#' 
#' # more than one group of dice
#' roll_dice_formula(dice_formula = "1d8+1d6+2")
#' 
#' # unfair dice: the 6 is twice as likely as any other side
#' roll_dice_formula(dice_formula = "3d6", prob = c(1, 1, 1, 1, 1, 2))
#' 
#' # unfair d6 plus fair d4
#' roll_dice_formula(dice_formula = "1d6+1d4", prob = list(c(1, 1, 1, 1, 1, 5), NULL))
#' 
#' # keep the dice that were rolled, e.g. to plot them
#' roll_dice_formula(dice_formula = "4d6pl1", times = 2, detail = TRUE)$dice
#' 
#' # roll one 20-sided dice, and add 4
#' roll_dice_formula(dice_formula = "1d20+4")
#' 
#' # roll one 4-sided dice and one 6-sided dice, and sum the results
#' roll_dice_formula(dice_formula = "1d4+1d6")

roll_dice_formula <- function(data=NULL,
                              dice_formula = "1d6", 
                              times = 1, 
                              rounds = 1,
                              seed=NULL, 
                              prob=NULL, 
                              success = c(6),
                              agg=FALSE,
                              label=NULL,
                              detail=FALSE
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

  assertthat::assert_that(is.character(dice_formula) && length(dice_formula) == 1, 
                          msg = "dice_formula must be a single character string")
  assertthat::assert_that(is.numeric(times) && length(times) == 1 && times >= 1, 
                          msg = "times must be a number >= 1")
  assertthat::assert_that(is.numeric(rounds) && length(rounds) == 1 && rounds >= 1, 
                          msg = "rounds must be a number >= 1")

  # define variables to pass CRAN checks
  result <- NULL
  experiment_id <- NULL
  experiment <- NULL
  nr <- NULL
  
  # Parse the formula (see dice-eval.R), then roll it
  dice_expression <- parse_dice_expression(dice_formula)
  
  result <- eval_dice_expression(dice_expression, n = rounds * times, prob = prob, 
                                 detail = detail && !agg)
  result_df <- tibble::tibble(
    round = as.integer(rep(1:rounds, each = times)),
    nr    = as.integer(rep(1:times, times = rounds)),
    result = as.vector(result)
  )
  if (!is.null(attr(result, "dice"))) {
    result_df$dice <- attr(result, "dice")
  }
      
  # Compute success
  result_df = result_df %>%
    mutate(success = result %in% success)
  
  # Format the result df before returning it
  result_df = result_df %>% 
    mutate(experiment_id = 1,
           dice_formula = dice_formula,
           label=label) %>%
    select(experiment_id, dice_formula, label, round, nr, result, success, 
           dplyr::any_of("dice")) 
  
  if (!is.data.frame(data))  {
    
    # result of roll_dice (first experiment)
    result_df <- result_df %>% 
      dplyr::mutate(experiment = as.integer(1)) %>% 
      dplyr::select(experiment, dplyr::everything()) %>%
      dplyr::select(-experiment_id)
    
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
      dplyr::select(experiment, dplyr::everything()) %>%
      dplyr::select(-experiment_id)
  }
  
  # agg?
  if (agg)  {
    
    result_df <- result_df %>%
      dplyr::group_by(experiment, round) %>%
      summarize(
        dice_formula = min(dice_formula),
        times = n(),
        success = as.integer(sum(success))
      )
  }
  
  # return data frame
  result_df
  
}
