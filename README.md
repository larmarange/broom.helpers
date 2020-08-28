
<!-- README.md is generated from README.Rmd. Please edit that file -->

# broom.helpers

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.com/larmarange/broom.helpers.svg?branch=master)](https://travis-ci.com/larmarange/broom.helpers)
[![R build
status](https://github.com/larmarange/broom.helpers/workflows/R-CMD-check/badge.svg)](https://github.com/larmarange/broom.helpers/actions)
[![Codecov test
coverage](https://codecov.io/gh/larmarange/broom.helpers/branch/master/graph/badge.svg)](https://codecov.io/gh/larmarange/broom.helpers?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/broom.helpers)](https://CRAN.R-project.org/package=broom.helpers)
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
#>   term  variable var_label var_class var_type contrasts reference_row label
#>   <chr> <chr>    <chr>     <chr>     <chr>    <chr>     <lgl>         <chr>
#> 1 fact~ factor(~ factor(c~ factor    categor~ contr.tr~ TRUE          4    
#> 2 fact~ factor(~ factor(c~ factor    categor~ contr.tr~ FALSE         6    
#> 3 fact~ factor(~ factor(c~ factor    categor~ contr.tr~ FALSE         8    
#> 4 hp    hp       hp        numeric   continu~ <NA>      NA            hp   
#> # ... with 4 more variables: estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>
```

The package also includes a handy wrapper for the most commonly used
functions.

``` r
lm(mpg ~ factor(cyl) + hp, mtcars) %>%
  tidy_plus_plus()
#> # A tibble: 4 x 14
#>   term  variable var_label var_class var_type contrasts reference_row label
#>   <chr> <chr>    <chr>     <chr>     <chr>    <chr>     <lgl>         <chr>
#> 1 fact~ factor(~ factor(c~ factor    categor~ contr.tr~ TRUE          4    
#> 2 fact~ factor(~ factor(c~ factor    categor~ contr.tr~ FALSE         6    
#> 3 fact~ factor(~ factor(c~ factor    categor~ contr.tr~ FALSE         8    
#> 4 hp    hp       hp        numeric   continu~ <NA>      NA            hp   
#> # ... with 6 more variables: estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>, conf.low <dbl>, conf.high <dbl>
```
