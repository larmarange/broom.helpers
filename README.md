
<!-- README.md is generated from README.Rmd. Please edit that file -->

# broom.helpers <img src="man/figures/broom.helpers.png" align="right" width="120" />

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R build
status](https://github.com/larmarange/broom.helpers/workflows/R-CMD-check/badge.svg)](https://github.com/larmarange/broom.helpers/actions)
[![Codecov test
coverage](https://codecov.io/gh/larmarange/broom.helpers/branch/main/graph/badge.svg)](https://codecov.io/gh/larmarange/broom.helpers?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/broom.helpers)](https://CRAN.R-project.org/package=broom.helpers)
[![DOI](https://zenodo.org/badge/286680847.svg)](https://zenodo.org/badge/latestdoi/286680847)
<!-- badges: end -->

The broom.helpers package provides suite of functions to work with
regression model `broom::tidy()` tibbles.

The suite includes functions to group regression model terms by
variable, insert reference and header rows for categorical variables,
add variable labels, and more.

`broom.helpers` is used, in particular, by `gtsummary::tbl_regression()`
for producing [nice formatted tables of model
coefficients](http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html)
and by `GGally::ggcoef_model()` for [plotting model
coefficients](http://ggobi.github.io/ggally/articles/ggcoef_model.html).

## Installation

To install stable version:

``` r
install.packages("broom.helpers")
```

To install development version:

``` r
devtools::install_github("larmarange/broom.helpers")
```

## Examples

### all-in-one wrapper

``` r
mod1 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
library(broom.helpers)
ex1 <- mod1 %>% tidy_plus_plus()
ex1
#> # A tibble: 4 × 17
#>   term              variable  var_label var_class var_type var_nlevels contrasts
#>   <chr>             <chr>     <chr>     <chr>     <chr>          <int> <chr>    
#> 1 Sepal.Width       Sepal.Wi… Sepal.Wi… numeric   continu…          NA <NA>     
#> 2 Speciessetosa     Species   Species   factor    categor…           3 contr.tr…
#> 3 Speciesversicolor Species   Species   factor    categor…           3 contr.tr…
#> 4 Speciesvirginica  Species   Species   factor    categor…           3 contr.tr…
#> # … with 10 more variables: contrasts_type <chr>, reference_row <lgl>,
#> #   label <chr>, n_obs <dbl>, estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>, conf.low <dbl>, conf.high <dbl>
dplyr::glimpse(ex1)
#> Rows: 4
#> Columns: 17
#> $ term           <chr> "Sepal.Width", "Speciessetosa", "Speciesversicolor", "S…
#> $ variable       <chr> "Sepal.Width", "Species", "Species", "Species"
#> $ var_label      <chr> "Sepal.Width", "Species", "Species", "Species"
#> $ var_class      <chr> "numeric", "factor", "factor", "factor"
#> $ var_type       <chr> "continuous", "categorical", "categorical", "categorica…
#> $ var_nlevels    <int> NA, 3, 3, 3
#> $ contrasts      <chr> NA, "contr.treatment", "contr.treatment", "contr.treatm…
#> $ contrasts_type <chr> NA, "treatment", "treatment", "treatment"
#> $ reference_row  <lgl> NA, TRUE, FALSE, FALSE
#> $ label          <chr> "Sepal.Width", "setosa", "versicolor", "virginica"
#> $ n_obs          <dbl> 150, 50, 50, 50
#> $ estimate       <dbl> 0.8035609, 0.0000000, 1.4587431, 1.9468166
#> $ std.error      <dbl> 0.1063390, NA, 0.1121079, 0.1000150
#> $ statistic      <dbl> 7.556598, NA, 13.011954, 19.465255
#> $ p.value        <dbl> 4.187340e-12, NA, 3.478232e-26, 2.094475e-42
#> $ conf.low       <dbl> 0.5933983, NA, 1.2371791, 1.7491525
#> $ conf.high      <dbl> 1.013723, NA, 1.680307, 2.144481

mod2 <- glm(
  response ~ poly(age, 3) + stage + grade * trt,
  na.omit(gtsummary::trial),
  family = binomial,
  contrasts = list(
    stage = contr.treatment(4, base = 3),
    grade = contr.sum
  )
)
ex2 <- mod2 %>% 
  tidy_plus_plus(
    exponentiate = TRUE,
    variable_labels = c(age = "Age (in years)"),
    add_header_rows = TRUE,
    show_single_row = "trt"
  )
#> Loading required namespace: emmeans
ex2
#> # A tibble: 17 × 19
#>    term   variable var_label var_class var_type var_nlevels header_row contrasts
#>    <chr>  <chr>    <chr>     <chr>     <chr>          <int> <lgl>      <chr>    
#>  1 <NA>   age      Age (in … nmatrix.3 continu…          NA TRUE       <NA>     
#>  2 poly(… age      Age (in … nmatrix.3 continu…          NA FALSE      <NA>     
#>  3 poly(… age      Age (in … nmatrix.3 continu…          NA FALSE      <NA>     
#>  4 poly(… age      Age (in … nmatrix.3 continu…          NA FALSE      <NA>     
#>  5 <NA>   stage    T Stage   factor    categor…           4 TRUE       contr.tr…
#>  6 stage1 stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#>  7 stage2 stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#>  8 stage3 stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#>  9 stage4 stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#> 10 <NA>   grade    Grade     factor    categor…           3 TRUE       contr.sum
#> 11 grade1 grade    Grade     factor    categor…           3 FALSE      contr.sum
#> 12 grade2 grade    Grade     factor    categor…           3 FALSE      contr.sum
#> 13 grade3 grade    Grade     factor    categor…           3 FALSE      contr.sum
#> 14 trtDr… trt      Chemothe… character dichoto…           2 NA         contr.tr…
#> 15 <NA>   grade:t… Grade * … <NA>      interac…          NA TRUE       <NA>     
#> 16 grade… grade:t… Grade * … <NA>      interac…          NA FALSE      <NA>     
#> 17 grade… grade:t… Grade * … <NA>      interac…          NA FALSE      <NA>     
#> # … with 11 more variables: contrasts_type <chr>, reference_row <lgl>,
#> #   label <chr>, n_obs <dbl>, n_event <dbl>, estimate <dbl>, std.error <dbl>,
#> #   statistic <dbl>, p.value <dbl>, conf.low <dbl>, conf.high <dbl>
dplyr::glimpse(ex2)
#> Rows: 17
#> Columns: 19
#> $ term           <chr> NA, "poly(age, 3)1", "poly(age, 3)2", "poly(age, 3)3", …
#> $ variable       <chr> "age", "age", "age", "age", "stage", "stage", "stage", …
#> $ var_label      <chr> "Age (in years)", "Age (in years)", "Age (in years)", "…
#> $ var_class      <chr> "nmatrix.3", "nmatrix.3", "nmatrix.3", "nmatrix.3", "fa…
#> $ var_type       <chr> "continuous", "continuous", "continuous", "continuous",…
#> $ var_nlevels    <int> NA, NA, NA, NA, 4, 4, 4, 4, 4, 3, 3, 3, 3, 2, NA, NA, NA
#> $ header_row     <lgl> TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, F…
#> $ contrasts      <chr> NA, NA, NA, NA, "contr.treatment(base=3)", "contr.treat…
#> $ contrasts_type <chr> NA, NA, NA, NA, "treatment", "treatment", "treatment", …
#> $ reference_row  <lgl> NA, NA, NA, NA, NA, FALSE, FALSE, TRUE, FALSE, NA, FALS…
#> $ label          <chr> "Age (in years)", "Age (in years)", "Age (in years)²", …
#> $ n_obs          <dbl> NA, 92, 56, 80, NA, 46, 50, 35, 42, NA, 63, 53, 57, 90,…
#> $ n_event        <dbl> NA, 31, 17, 22, NA, 17, 12, 13, 12, NA, 20, 16, 18, 30,…
#> $ estimate       <dbl> NA, 20.2416394, 1.2337899, 0.4931553, NA, 1.0047885, 0.…
#> $ std.error      <dbl> NA, 2.3254455, 2.3512842, 2.3936657, NA, 0.4959893, 0.5…
#> $ statistic      <dbl> NA, 1.29340459, 0.08935144, -0.29533409, NA, 0.00963137…
#> $ p.value        <dbl> NA, 0.1958712, 0.9288026, 0.7677387, NA, 0.9923154, 0.1…
#> $ conf.low       <dbl> NA, 0.225454425, 0.007493208, 0.004745694, NA, 0.379776…
#> $ conf.high      <dbl> NA, 2315.587655, 100.318341, 74.226179, NA, 2.683385, 1…
```

### fine control

``` r
ex3 <- mod1 %>%
  # perform initial tidying of model
  tidy_and_attach() %>%
  # add reference row
  tidy_add_reference_rows() %>%
  # add term labels
  tidy_add_term_labels() %>%
  # remove intercept
  tidy_remove_intercept
ex3
#> # A tibble: 4 × 16
#>   term              variable  var_label var_class var_type var_nlevels contrasts
#>   <chr>             <chr>     <chr>     <chr>     <chr>          <int> <chr>    
#> 1 Sepal.Width       Sepal.Wi… Sepal.Wi… numeric   continu…          NA <NA>     
#> 2 Speciessetosa     Species   Species   factor    categor…           3 contr.tr…
#> 3 Speciesversicolor Species   Species   factor    categor…           3 contr.tr…
#> 4 Speciesvirginica  Species   Species   factor    categor…           3 contr.tr…
#> # … with 9 more variables: contrasts_type <chr>, reference_row <lgl>,
#> #   label <chr>, estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>, conf.low <dbl>, conf.high <dbl>
dplyr::glimpse(ex3)
#> Rows: 4
#> Columns: 16
#> $ term           <chr> "Sepal.Width", "Speciessetosa", "Speciesversicolor", "S…
#> $ variable       <chr> "Sepal.Width", "Species", "Species", "Species"
#> $ var_label      <chr> "Sepal.Width", "Species", "Species", "Species"
#> $ var_class      <chr> "numeric", "factor", "factor", "factor"
#> $ var_type       <chr> "continuous", "categorical", "categorical", "categorica…
#> $ var_nlevels    <int> NA, 3, 3, 3
#> $ contrasts      <chr> NA, "contr.treatment", "contr.treatment", "contr.treatm…
#> $ contrasts_type <chr> NA, "treatment", "treatment", "treatment"
#> $ reference_row  <lgl> NA, TRUE, FALSE, FALSE
#> $ label          <chr> "Sepal.Width", "setosa", "versicolor", "virginica"
#> $ estimate       <dbl> 0.8035609, NA, 1.4587431, 1.9468166
#> $ std.error      <dbl> 0.1063390, NA, 0.1121079, 0.1000150
#> $ statistic      <dbl> 7.556598, NA, 13.011954, 19.465255
#> $ p.value        <dbl> 4.187340e-12, NA, 3.478232e-26, 2.094475e-42
#> $ conf.low       <dbl> 0.5933983, NA, 1.2371791, 1.7491525
#> $ conf.high      <dbl> 1.013723, NA, 1.680307, 2.144481

ex4 <- mod2 %>%
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
ex4
#> # A tibble: 20 × 17
#>    term   variable var_label var_class var_type var_nlevels header_row contrasts
#>    <chr>  <chr>    <chr>     <chr>     <chr>          <int> <lgl>      <chr>    
#>  1 (Inte… (Interc… (Interce… <NA>      interce…          NA NA         <NA>     
#>  2 <NA>   age      Age in y… nmatrix.3 continu…          NA TRUE       <NA>     
#>  3 poly(… age      Age in y… nmatrix.3 continu…          NA FALSE      <NA>     
#>  4 poly(… age      Age in y… nmatrix.3 continu…          NA FALSE      <NA>     
#>  5 poly(… age      Age in y… nmatrix.3 continu…          NA FALSE      <NA>     
#>  6 <NA>   stage    T Stage   factor    categor…           4 TRUE       contr.tr…
#>  7 stage1 stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#>  8 stage2 stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#>  9 stage3 stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#> 10 stage4 stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#> 11 <NA>   grade    Grade     factor    categor…           3 TRUE       contr.sum
#> 12 grade1 grade    Grade     factor    categor…           3 FALSE      contr.sum
#> 13 grade2 grade    Grade     factor    categor…           3 FALSE      contr.sum
#> 14 grade3 grade    Grade     factor    categor…           3 FALSE      contr.sum
#> 15 <NA>   trt      Chemothe… character dichoto…           2 TRUE       contr.tr…
#> 16 trtDr… trt      Chemothe… character dichoto…           2 FALSE      contr.tr…
#> 17 trtDr… trt      Chemothe… character dichoto…           2 FALSE      contr.tr…
#> 18 <NA>   grade:t… Grade * … <NA>      interac…          NA TRUE       <NA>     
#> 19 grade… grade:t… Grade * … <NA>      interac…          NA FALSE      <NA>     
#> 20 grade… grade:t… Grade * … <NA>      interac…          NA FALSE      <NA>     
#> # … with 9 more variables: contrasts_type <chr>, reference_row <lgl>,
#> #   label <chr>, estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>, conf.low <dbl>, conf.high <dbl>
dplyr::glimpse(ex4)
#> Rows: 20
#> Columns: 17
#> $ term           <chr> "(Intercept)", NA, "poly(age, 3)1", "poly(age, 3)2", "p…
#> $ variable       <chr> "(Intercept)", "age", "age", "age", "age", "stage", "st…
#> $ var_label      <chr> "(Intercept)", "Age in years", "Age in years", "Age in …
#> $ var_class      <chr> NA, "nmatrix.3", "nmatrix.3", "nmatrix.3", "nmatrix.3",…
#> $ var_type       <chr> "intercept", "continuous", "continuous", "continuous", …
#> $ var_nlevels    <int> NA, NA, NA, NA, NA, 4, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2,…
#> $ header_row     <lgl> NA, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALS…
#> $ contrasts      <chr> NA, NA, NA, NA, NA, "contr.treatment(base=3)", "contr.t…
#> $ contrasts_type <chr> NA, NA, NA, NA, NA, "treatment", "treatment", "treatmen…
#> $ reference_row  <lgl> NA, NA, NA, NA, NA, NA, FALSE, FALSE, TRUE, FALSE, NA, …
#> $ label          <chr> "(Intercept)", "Age in years", "Age in years", "Age in …
#> $ estimate       <dbl> 0.5266376, NA, 20.2416394, 1.2337899, 0.4931553, NA, 1.…
#> $ std.error      <dbl> 0.4130930, NA, 2.3254455, 2.3512842, 2.3936657, NA, 0.4…
#> $ statistic      <dbl> -1.55229592, NA, 1.29340459, 0.08935144, -0.29533409, N…
#> $ p.value        <dbl> 0.1205914, NA, 0.1958712, 0.9288026, 0.7677387, NA, 0.9…
#> $ conf.low       <dbl> 0.227717775, NA, 0.225454425, 0.007493208, 0.004745694,…
#> $ conf.high      <dbl> 1.164600, NA, 2315.587655, 100.318341, 74.226179, NA, 2…
```
