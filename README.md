
# ceiglobal

<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-Experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#Experimental)
![](https://img.shields.io/badge/Most%20used%20package-in%20CEI-green.svg)
<!-- badges: end -->

The goal of ceiglobal is to provide an easy to use extension to
`ggplot2` to allow users to easily create beautiful figures consistent
with CEI’s style guide.

## Installation

This is a ‘private’ package that you can only install if you are a
member of the ceiglobal organisation on github. To obtain access you
will need: a) a github account, b) access to CEI’s organisation on
github, c) a personal access token (PAT) to prevent unauthorised access
and d) a couple of lines of code to install. The steps for each are
outlined below.

### Get a github account

This is nice and easy. Go to [github.com](www.github.com) and sign up
for a free account and pick a username. Your username can be anything,
it doesn’t have to be work-related.

### Get access to CEI’s organisation on github

Send Dave your github username and he’ll add you manually.

### Get a personal access token so you can download the package

This is a little tricker, but we’ll walk you through it.

The personal access token prevents any unauthorised access to our
repositories on github.

1.  Go [here](https://github.com/settings/tokens)
2.  Select “generate access token”
3.  Add a note in the box to remind yourself what this is for, something
    like “theme\_ceiglobal”.
4.  Select the checkbox called “repo”, scroll to the bottom and
    “generate token”
5.  You will be taken to a new page with your PAT create a copy of it
6.  You need to store this PAT in your R profile, do this in RStudio by
    running the code `usethis::edit_r_profile()`
7.  This will open up a new tab called `.RProfile*`. You’ll need to run
    another bit of code `Sys.setenv(GITHUB_PAT = "this is where you
    paste you PAT")` then save code and close it.
8.  All done, RStudio now has a record of your PAT\!

### Final installation

You can on install this package by running this code below. Note: you
will need the install and load the package `devtools` first. To install
run `install.package("devtools")`, to load run`library("devtools")`.

Install `ceiglobal` by running:

``` r
devtools::install_github("ceiglobal/ceiglobal")
```

## Example

This is a very basic example which shows you how to the package takes a
standard `ggplot2` object and applies CEI’s style guide.

``` r
## LOAD PACKAGES

library(ggplot2)
library(ceiglobal)

## This what a plot looks like using standard ggplot2 output

diamonds %>%
  ggplot() +
  aes(x = carat, y = price, colour = color) +
  geom_point() +
  facet_wrap(~cut, ncol = 1)
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r

## This is what happens when you sprinkle a little bit of cei magic on it

diamonds %>%
  ggplot() +
  aes(x = carat, y = price, colour = color) +
  geom_point() +
  facet_wrap(~cut, ncol = 1) +
  theme_ceiglobal()
```

<img src="man/figures/README-example-2.png" width="100%" />
