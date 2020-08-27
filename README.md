
<!-- README.md is generated from README.Rmd. Please edit that file -->

# broom.helpers

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.com/larmarange/broom.helpers.svg?branch=master)](https://travis-ci.com/larmarange/broom.helpers)
<!-- badges: end -->

The broom.helpers package provides suite of functions to work with
regression model `broom::tidy()` tibbles. The suite includes functions
to group regression model terms by variable, insert reference and header
rows for categorical variables, add variable labels, and more.

## Installation

This package is still experimental and under development.

To install it for testing purpose, use:

``` r
devtools::install_github("larmarange/broom.helpers")
```

## Examples

``` r
library(broom.helpers)

# build regression model
lm(mpg ~ factor(cyl) + hp, mtcars) %>%
  # perform initial tidying of model
  tidy_and_attach() %>%
  # add reference row cyl
  tidy_add_reference_rows() %>%
  # add the cyl levels
  tidy_add_term_labels() %>%
  # remove intercept
  tidy_remove_intercept()
#> # A tibble: 4 x 12
#>   term  variable var_class var_type estimate std.error statistic  p.value
#>   <chr> <chr>    <chr>     <chr>       <dbl>     <dbl>     <dbl>    <dbl>
#> 1 fact~ factor(~ factor    categor~  NA        NA          NA    NA      
#> 2 fact~ factor(~ factor    categor~  -5.97      1.64       -3.64  0.00109
#> 3 fact~ factor(~ factor    categor~  -8.52      2.33       -3.66  0.00103
#> 4 hp    hp       numeric   continu~  -0.0240    0.0154     -1.56  0.130  
#> # ... with 4 more variables: contrasts <chr>, reference_row <lgl>,
#> #   var_label <chr>, label <chr>
```

The package also includes a handy wrapper for the most commonly used
functions.

``` r
lm(mpg ~ factor(cyl) + hp, mtcars) %>%
  tidy_plus_plus()
#> # A tibble: 4 x 14
#>   term  variable var_class var_type estimate std.error statistic  p.value
#>   <chr> <chr>    <chr>     <chr>       <dbl>     <dbl>     <dbl>    <dbl>
#> 1 fact~ factor(~ factor    categor~     0      NA          NA    NA      
#> 2 fact~ factor(~ factor    categor~    -5.97    1.64       -3.64  0.00109
#> 3 fact~ factor(~ factor    categor~    -8.52    2.33       -3.66  0.00103
#> 4 hp    hp       numeric   continu~    NA       0.0154     -1.56  0.130  
#> # ... with 6 more variables: conf.low <dbl>, conf.high <dbl>, contrasts <chr>,
#> #   reference_row <lgl>, var_label <chr>, label <chr>
```
