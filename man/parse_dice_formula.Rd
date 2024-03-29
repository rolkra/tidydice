% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dice-formula.R
\name{parse_dice_formula}
\alias{parse_dice_formula}
\title{Given a dice formula string, split it and return a dataframe with the list
of functions.}
\usage{
parse_dice_formula(dice_formula)
}
\arguments{
\item{dice_formula}{A string containing a dice formula, e.g. 1d6e2+1d4}
}
\description{
This is the main function to parse a string containing complex formula
specifications for rolling dice.
}
\details{
The input can be a string containing specifications for multiple dice, e.g.:
\itemize{
\item 1d6e6          -> roll 1 six-sided dice, explode on 6
\item 1d6e6+2d4-1d10 -> Roll 1 six-sided dice, explode on 6, plus two 4-sided
dice, subract one 10-sided dice
}

This is inspired by Avrae's bot syntax for rolling dice. See https://github.com/avrae/d20
}
