
<!-- README.md is generated from README.Rmd. Please edit that file -->

# broom.helpers <img src="man/figures/broom.helpers.png" align="right" width="120" />

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

### all-in-one wrapper

``` r
mod1 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
library(broom.helpers)
mod1 %>% tidy_plus_plus()
#> # A tibble: 4 x 14
#>   term  variable var_label var_class var_type contrasts reference_row label
#>   <chr> <chr>    <chr>     <chr>     <chr>    <chr>     <lgl>         <chr>
#> 1 Sepa~ Sepal.W~ Sepal.Wi~ numeric   continu~ <NA>      NA            Sepa~
#> 2 Spec~ Species  Species   factor    categor~ contr.tr~ TRUE          seto~
#> 3 Spec~ Species  Species   factor    categor~ contr.tr~ FALSE         vers~
#> 4 Spec~ Species  Species   factor    categor~ contr.tr~ FALSE         virg~
#> # ... with 6 more variables: estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>, conf.low <dbl>, conf.high <dbl>

mod2 <- glm(
  response ~ poly(age, 3) + stage + grade * trt,
  na.omit(gtsummary::trial),
  family = binomial,
  contrasts = list(
    stage = contr.treatment(4, base = 3),
    grade = contr.sum
  )
)
mod2 %>% 
  tidy_plus_plus(
    exponentiate = TRUE,
    variable_labels = c(age = "Age (in years)"),
    add_header_rows = TRUE,
    show_single_row = "trt"
  )
#> # A tibble: 17 x 15
#>    term  variable var_label var_class var_type header_row contrasts
#>    <chr> <chr>    <chr>     <chr>     <chr>    <lgl>      <chr>    
#>  1 <NA>  age      Age (in ~ nmatrix.3 continu~ TRUE       <NA>     
#>  2 poly~ age      Age (in ~ nmatrix.3 continu~ FALSE      <NA>     
#>  3 poly~ age      Age (in ~ nmatrix.3 continu~ FALSE      <NA>     
#>  4 poly~ age      Age (in ~ nmatrix.3 continu~ FALSE      <NA>     
#>  5 <NA>  stage    T Stage   factor    categor~ TRUE       contr.tr~
#>  6 stag~ stage    T Stage   factor    categor~ FALSE      contr.tr~
#>  7 stag~ stage    T Stage   factor    categor~ FALSE      contr.tr~
#>  8 stag~ stage    T Stage   factor    categor~ FALSE      contr.tr~
#>  9 stag~ stage    T Stage   factor    categor~ FALSE      contr.tr~
#> 10 <NA>  grade    Grade     factor    categor~ TRUE       contr.sum
#> 11 grad~ grade    Grade     factor    categor~ FALSE      contr.sum
#> 12 grad~ grade    Grade     factor    categor~ FALSE      contr.sum
#> 13 grad~ grade    Grade     factor    categor~ FALSE      contr.sum
#> 14 trtD~ trt      Chemothe~ character categor~ NA         contr.tr~
#> 15 <NA>  grade:t~ Grade * ~ <NA>      interac~ TRUE       <NA>     
#> 16 grad~ grade:t~ Grade * ~ <NA>      interac~ FALSE      <NA>     
#> 17 grad~ grade:t~ Grade * ~ <NA>      interac~ FALSE      <NA>     
#> # ... with 8 more variables: reference_row <lgl>, label <chr>, estimate <dbl>,
#> #   std.error <dbl>, statistic <dbl>, p.value <dbl>, conf.low <dbl>,
#> #   conf.high <dbl>
```

### fine control

``` r
mod1 %>%
  # perform initial tidying of model
  tidy_and_attach() %>%
  # add reference row
  tidy_add_reference_rows() %>%
  # add term labels
  tidy_add_term_labels() %>%
  # remove intercept
  tidy_remove_intercept()
#> # A tibble: 4 x 12
#>   term  variable var_label var_class var_type contrasts reference_row label
#>   <chr> <chr>    <chr>     <chr>     <chr>    <chr>     <lgl>         <chr>
#> 1 Sepa~ Sepal.W~ Sepal.Wi~ numeric   continu~ <NA>      NA            Sepa~
#> 2 Spec~ Species  Species   factor    categor~ contr.tr~ TRUE          seto~
#> 3 Spec~ Species  Species   factor    categor~ contr.tr~ FALSE         vers~
#> 4 Spec~ Species  Species   factor    categor~ contr.tr~ FALSE         virg~
#> # ... with 4 more variables: estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>

mod2 %>%
  # perform initial tidying of model
  tidy_and_attach(exponentiate = TRUE) %>%
  # add variable labels, including a custom value for age
  tidy_add_variable_labels(labels = c(age = "Age in years")) %>%
  # add reference rows for categorical variables
  tidy_add_reference_rows() %>%
  # add a, estimate value of reference terms
  tidy_add_estimate_to_reference_rows(exponentiate = TRUE) %>%
  # add header rows for categorical variables
  tidy_add_header_rows()
#> # A tibble: 20 x 13
#>    term  variable var_label var_class var_type header_row contrasts
#>    <chr> <chr>    <chr>     <chr>     <chr>    <lgl>      <chr>    
#>  1 (Int~ <NA>     (Interce~ <NA>      interce~ NA         <NA>     
#>  2 <NA>  age      Age in y~ nmatrix.3 continu~ TRUE       <NA>     
#>  3 poly~ age      Age in y~ nmatrix.3 continu~ FALSE      <NA>     
#>  4 poly~ age      Age in y~ nmatrix.3 continu~ FALSE      <NA>     
#>  5 poly~ age      Age in y~ nmatrix.3 continu~ FALSE      <NA>     
#>  6 <NA>  stage    T Stage   factor    categor~ TRUE       contr.tr~
#>  7 stag~ stage    T Stage   factor    categor~ FALSE      contr.tr~
#>  8 stag~ stage    T Stage   factor    categor~ FALSE      contr.tr~
#>  9 stag~ stage    T Stage   factor    categor~ FALSE      contr.tr~
#> 10 stag~ stage    T Stage   factor    categor~ FALSE      contr.tr~
#> 11 <NA>  grade    Grade     factor    categor~ TRUE       contr.sum
#> 12 grad~ grade    Grade     factor    categor~ FALSE      contr.sum
#> 13 grad~ grade    Grade     factor    categor~ FALSE      contr.sum
#> 14 grad~ grade    Grade     factor    categor~ FALSE      contr.sum
#> 15 <NA>  trt      Chemothe~ character categor~ TRUE       contr.tr~
#> 16 trtD~ trt      Chemothe~ character categor~ FALSE      contr.tr~
#> 17 trtD~ trt      Chemothe~ character categor~ FALSE      contr.tr~
#> 18 <NA>  grade:t~ Grade * ~ <NA>      interac~ TRUE       <NA>     
#> 19 grad~ grade:t~ Grade * ~ <NA>      interac~ FALSE      <NA>     
#> 20 grad~ grade:t~ Grade * ~ <NA>      interac~ FALSE      <NA>     
#> # ... with 6 more variables: reference_row <lgl>, label <chr>, estimate <dbl>,
#> #   std.error <dbl>, statistic <dbl>, p.value <dbl>
```
