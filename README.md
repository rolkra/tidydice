<img src="man/figures/hex_tidydice.png" align="right" width="130" height="150"/>

[![CRAN Version](http://www.r-pkg.org/badges/version/tidydice)](https://cran.r-project.org/package=tidydice)
[![Downloads](http://cranlogs.r-pkg.org/badges/tidydice)](https://cran.r-project.org/package=tidydice)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/tidydice)](https://cran.r-project.org/package=tidydice)

# tidydice
Simulates Dice Rolls and Coin Flips.

### Introduction

A basic understanding of probability and statistics is crucial for data understanding. A great way to teach probability and statistics is to start with an experiment, like rolling a dice or flipping a coin.

This package simulates rolling a dice and flipping a coin. Each experiment generates a tibble. Dice rolls and coin flips are simulated using sample(). The properties of the dice can be changed, like the number of sides. A coin flip is simulated using a two sided dice. Experiments can be combined with the pipe-operator.

## Installation

### CRAN
```r
install.packages("tidydice")
```

### DEV version (github)
```r
# install from github
if (!require(devtools)) install.packages("devtools")
devtools::install_github("rolkra/tidydice")
```
if you are behind a firewall, you may want to:

* Download and unzip the tidydice package
* Then install it with devtools::install_local

```r
# install local
if (!require(devtools)) install.packages("devtools")
devtools::install_local(path = <path of local package>, force = TRUE)
```
