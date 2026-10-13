## tidydice (development version)

* roll_dice_formula() tracks explosions separately for each modifier, and accepts
  selectors matching values created by earlier `mi` or `ma` modifiers.
* binom_dice() and binom_coin() reject non-finite probability weights and
  normalize large finite weights without overflow.

* plot_coin() writes the result on each coin (`show_result = FALSE` for blank
  coins). The result was ignored before, so only the color showed a success.
* roll_dice_formula() has no limit to the number of dice. Avrae stops at 1000
  dice to protect a chat bot, but here that error also stopped legitimate 
  formulas (a long chain of explosions). Instead, formulas that would never 
  end are an error: `rr` with `h` or `l` (`3d6rrh1`), and `rr` or `e` on the 
  sides that have all the probability of an unfair dice.
* roll_dice() decides the first experiment from `data`, not from `missing(data)`, 
  which was wrong for flip_coin() and for `roll_dice(3)`. The result did not change.
* plot_dice() and plot_coin() use `linewidth` instead of `size` for lines, which is 
  deprecated in ggplot2 3.4.0 (needs ggplot2 >= 3.4.0). Before, ggplot2 warned about it 
  the first time a plot was drawn in a session.
* roll_dice_formula(detail = TRUE) adds a column `dice` with the dice that were 
  rolled for each result (value, sides, dice group, and whether the die was kept 
  or dropped). plot_dice() uses it to draw every die of a roll (`4d6pl1` shows 
  four dice, the dropped one in grey), see the new `by_die` and `fill_dropped` 
  parameters.
* binom_dice() and binom_coin() have a `prob` parameter for unfair dice and coins
  (like roll_dice() and flip_coin()), so plot_binom() can show them. Sides that 
  are listed twice in `success`, or that don't exist (0, 7 on a d6), are no longer 
  counted as a success.
* plot_dice(): results that are not a dice face (a sum like 13, 0 or less, a 
  decimal) are drawn as a number on a blank dice. Before, they gave a ggplot 
  warning, and a decimal like 2.5 was drawn as the face 2.
* roll_dice_formula(): `prob` can be a list with the probabilities of each group
  of dice (`prob = list(c(.5, .1, .1, .1, .1, .1), NULL)` for `"1d6+1d4"`), so
  unfair dice can be mixed with dice with a different number of sides. A vector 
  that doesn't fit a group now gives a clear error.
* roll_dice_formula() has a new parser and evaluator that follows the syntax of
  Avrae's d20 library:
    * several dice groups and numbers now work (`1d8+1d6+2`); before, only the
      first group was rolled and the rest was misread as a number
    * parentheses and operator precedence (`(1d6+2)*2`)
    * new modifiers: `p` (drop), `rr`, `ro`, `ra` (rerolls), `mi`, `ma`, 
      `k` with selectors (`k>3`), `d%`
    * selectors `N`, `<N`, `>N`, `hN`, `lN`; `<` and `>` are strict
    * invalid or unsupported syntax is now an error instead of being silently 
      ignored (e.g. `2d20h1` used to roll a plain `2d20`)
* parse_dice_formula() uses the same parser. It returns the same columns as 
  before, but now handles every syntax of roll_dice_formula() (`d%`, several
  selectors, `^`, parentheses) and fails on invalid syntax. A formula can no
  longer start with `*` or `/`. tidyr and stringr are no longer needed.
* roll_dice_formula() handles modifiers for all the rolls at once (matrices 
  instead of a loop over each roll), which is about 80 times faster: a million
  rolls of `4d6kh3` take less than a second.
* Exploding dice are simulated die by die, replacing the geometric-distribution 
  approximation. This fixes wrong results with several dice, `prob` and
  `kh`/`kl`. Exploded dice are part of the set that `k` and `p` work on.
* `1d2e1` is now valid. An exploding or `rr` selector that matches every side
  (endless loop) or no side is an error.

## tidydice 1.0.0 (2022-02-01)

* add roll_dice_formula()
* fix help plot_binom()
* change default color in plot_binom() to "coral"
* Update Vignette
* Update README
* Default color for success in plot_dice() & plot_coin() is "gold"

## tidydice 0.0.6 (2020-01-07)

* improve vignette
* add function binom()
* add function binom_dice()
* add function binom_coin()
* add function plot_binom()
* plot_dice() detailed = FALSE as default

## tidydice 0.0.4 (2019-11-26)

* add function plot_dice()
* add function force_dice()
* add function force_coin()
* rename and improve vignette

## tidydice 0.0.3 (2019-11-15)

* roll_dice()
* flip_coin()