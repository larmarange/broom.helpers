
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
#> [90m# A tibble: 4 x 17[39m
#>   term  variable var_label var_class var_type var_nlevels contrasts
#>   [3m[90m<chr>[39m[23m [3m[90m<chr>[39m[23m    [3m[90m<chr>[39m[23m     [3m[90m<chr>[39m[23m     [3m[90m<chr>[39m[23m          [3m[90m<int>[39m[23m [3m[90m<chr>[39m[23m    
#> [90m1[39m Sepa… Sepal.W… Sepal.Wi… numeric   continu…          [31mNA[39m [31mNA[39m       
#> [90m2[39m Spec… Species  Species   factor    categor…           3 contr.tr…
#> [90m3[39m Spec… Species  Species   factor    categor…           3 contr.tr…
#> [90m4[39m Spec… Species  Species   factor    categor…           3 contr.tr…
#> [90m# … with 10 more variables: contrasts_type [3m[90m<chr>[90m[23m, reference_row [3m[90m<lgl>[90m[23m,[39m
#> [90m#   label [3m[90m<chr>[90m[23m, n_obs [3m[90m<dbl>[90m[23m, estimate [3m[90m<dbl>[90m[23m, std.error [3m[90m<dbl>[90m[23m, statistic [3m[90m<dbl>[90m[23m,[39m
#> [90m#   p.value [3m[90m<dbl>[90m[23m, conf.low [3m[90m<dbl>[90m[23m, conf.high [3m[90m<dbl>[90m[23m[39m
dplyr::glimpse(ex1)
#> Rows: 4
#> Columns: 17
#> $ term           [3m[90m<chr>[39m[23m "Sepal.Width", "Speciessetosa", "Speciesversicolor", "…
#> $ variable       [3m[90m<chr>[39m[23m "Sepal.Width", "Species", "Species", "Species"
#> $ var_label      [3m[90m<chr>[39m[23m "Sepal.Width", "Species", "Species", "Species"
#> $ var_class      [3m[90m<chr>[39m[23m "numeric", "factor", "factor", "factor"
#> $ var_type       [3m[90m<chr>[39m[23m "continuous", "categorical", "categorical", "categoric…
#> $ var_nlevels    [3m[90m<int>[39m[23m NA, 3, 3, 3
#> $ contrasts      [3m[90m<chr>[39m[23m NA, "contr.treatment", "contr.treatment", "contr.treat…
#> $ contrasts_type [3m[90m<chr>[39m[23m NA, "treatment", "treatment", "treatment"
#> $ reference_row  [3m[90m<lgl>[39m[23m NA, TRUE, FALSE, FALSE
#> $ label          [3m[90m<chr>[39m[23m "Sepal.Width", "setosa", "versicolor", "virginica"
#> $ n_obs          [3m[90m<dbl>[39m[23m 150, 50, 50, 50
#> $ estimate       [3m[90m<dbl>[39m[23m 0.8035609, 0.0000000, 1.4587431, 1.9468166
#> $ std.error      [3m[90m<dbl>[39m[23m 0.1063390, NA, 0.1121079, 0.1000150
#> $ statistic      [3m[90m<dbl>[39m[23m 7.556598, NA, 13.011954, 19.465255
#> $ p.value        [3m[90m<dbl>[39m[23m 4.187340e-12, NA, 3.478232e-26, 2.094475e-42
#> $ conf.low       [3m[90m<dbl>[39m[23m 0.5933983, NA, 1.2371791, 1.7491525
#> $ conf.high      [3m[90m<dbl>[39m[23m 1.013723, NA, 1.680307, 2.144481

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
#> [90m# A tibble: 17 x 19[39m
#>    term  variable var_label var_class var_type var_nlevels header_row contrasts
#>    [3m[90m<chr>[39m[23m [3m[90m<chr>[39m[23m    [3m[90m<chr>[39m[23m     [3m[90m<chr>[39m[23m     [3m[90m<chr>[39m[23m          [3m[90m<int>[39m[23m [3m[90m<lgl>[39m[23m      [3m[90m<chr>[39m[23m    
#> [90m 1[39m [31mNA[39m    age      Age (in … nmatrix.3 continu…          [31mNA[39m TRUE       [31mNA[39m       
#> [90m 2[39m poly… age      Age (in … nmatrix.3 continu…          [31mNA[39m FALSE      [31mNA[39m       
#> [90m 3[39m poly… age      Age (in … nmatrix.3 continu…          [31mNA[39m FALSE      [31mNA[39m       
#> [90m 4[39m poly… age      Age (in … nmatrix.3 continu…          [31mNA[39m FALSE      [31mNA[39m       
#> [90m 5[39m [31mNA[39m    stage    T Stage   factor    categor…           4 TRUE       contr.tr…
#> [90m 6[39m stag… stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#> [90m 7[39m stag… stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#> [90m 8[39m stag… stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#> [90m 9[39m stag… stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#> [90m10[39m [31mNA[39m    grade    Grade     factor    categor…           3 TRUE       contr.sum
#> [90m11[39m grad… grade    Grade     factor    categor…           3 FALSE      contr.sum
#> [90m12[39m grad… grade    Grade     factor    categor…           3 FALSE      contr.sum
#> [90m13[39m grad… grade    Grade     factor    categor…           3 FALSE      contr.sum
#> [90m14[39m trtD… trt      Chemothe… character dichoto…           2 [31mNA[39m         contr.tr…
#> [90m15[39m [31mNA[39m    grade:t… Grade * … [31mNA[39m        interac…          [31mNA[39m TRUE       [31mNA[39m       
#> [90m16[39m grad… grade:t… Grade * … [31mNA[39m        interac…          [31mNA[39m FALSE      [31mNA[39m       
#> [90m17[39m grad… grade:t… Grade * … [31mNA[39m        interac…          [31mNA[39m FALSE      [31mNA[39m       
#> [90m# … with 11 more variables: contrasts_type [3m[90m<chr>[90m[23m, reference_row [3m[90m<lgl>[90m[23m,[39m
#> [90m#   label [3m[90m<chr>[90m[23m, n_obs [3m[90m<dbl>[90m[23m, n_event [3m[90m<dbl>[90m[23m, estimate [3m[90m<dbl>[90m[23m, std.error [3m[90m<dbl>[90m[23m,[39m
#> [90m#   statistic [3m[90m<dbl>[90m[23m, p.value [3m[90m<dbl>[90m[23m, conf.low [3m[90m<dbl>[90m[23m, conf.high [3m[90m<dbl>[90m[23m[39m
dplyr::glimpse(ex2)
#> Rows: 17
#> Columns: 19
#> $ term           [3m[90m<chr>[39m[23m NA, "poly(age, 3)1", "poly(age, 3)2", "poly(age, 3)3",…
#> $ variable       [3m[90m<chr>[39m[23m "age", "age", "age", "age", "stage", "stage", "stage",…
#> $ var_label      [3m[90m<chr>[39m[23m "Age (in years)", "Age (in years)", "Age (in years)", …
#> $ var_class      [3m[90m<chr>[39m[23m "nmatrix.3", "nmatrix.3", "nmatrix.3", "nmatrix.3", "f…
#> $ var_type       [3m[90m<chr>[39m[23m "continuous", "continuous", "continuous", "continuous"…
#> $ var_nlevels    [3m[90m<int>[39m[23m NA, NA, NA, NA, 4, 4, 4, 4, 4, 3, 3, 3, 3, 2, NA, NA, …
#> $ header_row     [3m[90m<lgl>[39m[23m TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, …
#> $ contrasts      [3m[90m<chr>[39m[23m NA, NA, NA, NA, "contr.treatment(base=3)", "contr.trea…
#> $ contrasts_type [3m[90m<chr>[39m[23m NA, NA, NA, NA, "treatment", "treatment", "treatment",…
#> $ reference_row  [3m[90m<lgl>[39m[23m NA, NA, NA, NA, NA, FALSE, FALSE, TRUE, FALSE, NA, FAL…
#> $ label          [3m[90m<chr>[39m[23m "Age (in years)", "Age (in years)", "Age (in years)²",…
#> $ n_obs          [3m[90m<dbl>[39m[23m NA, 92, 56, 80, NA, 46, 50, 35, 42, NA, 63, 53, 57, 90…
#> $ n_event        [3m[90m<dbl>[39m[23m NA, 31, 17, 22, NA, 17, 12, 13, 12, NA, 20, 16, 18, 30…
#> $ estimate       [3m[90m<dbl>[39m[23m NA, 20.2416394, 1.2337899, 0.4931553, NA, 1.0047885, 0…
#> $ std.error      [3m[90m<dbl>[39m[23m NA, 2.3254455, 2.3512842, 2.3936657, NA, 0.4959893, 0.…
#> $ statistic      [3m[90m<dbl>[39m[23m NA, 1.29340459, 0.08935144, -0.29533409, NA, 0.0096313…
#> $ p.value        [3m[90m<dbl>[39m[23m NA, 0.1958712, 0.9288026, 0.7677387, NA, 0.9923154, 0.…
#> $ conf.low       [3m[90m<dbl>[39m[23m NA, 0.225454425, 0.007493208, 0.004745694, NA, 0.37977…
#> $ conf.high      [3m[90m<dbl>[39m[23m NA, 2315.587655, 100.318341, 74.226179, NA, 2.683385, …
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
#> [90m# A tibble: 4 x 14[39m
#>   term  variable var_label var_class var_type var_nlevels contrasts
#>   [3m[90m<chr>[39m[23m [3m[90m<chr>[39m[23m    [3m[90m<chr>[39m[23m     [3m[90m<chr>[39m[23m     [3m[90m<chr>[39m[23m          [3m[90m<int>[39m[23m [3m[90m<chr>[39m[23m    
#> [90m1[39m Sepa… Sepal.W… Sepal.Wi… numeric   continu…          [31mNA[39m [31mNA[39m       
#> [90m2[39m Spec… Species  Species   factor    categor…           3 contr.tr…
#> [90m3[39m Spec… Species  Species   factor    categor…           3 contr.tr…
#> [90m4[39m Spec… Species  Species   factor    categor…           3 contr.tr…
#> [90m# … with 7 more variables: contrasts_type [3m[90m<chr>[90m[23m, reference_row [3m[90m<lgl>[90m[23m,[39m
#> [90m#   label [3m[90m<chr>[90m[23m, estimate [3m[90m<dbl>[90m[23m, std.error [3m[90m<dbl>[90m[23m, statistic [3m[90m<dbl>[90m[23m,[39m
#> [90m#   p.value [3m[90m<dbl>[90m[23m[39m
dplyr::glimpse(ex3)
#> Rows: 4
#> Columns: 14
#> $ term           [3m[90m<chr>[39m[23m "Sepal.Width", "Speciessetosa", "Speciesversicolor", "…
#> $ variable       [3m[90m<chr>[39m[23m "Sepal.Width", "Species", "Species", "Species"
#> $ var_label      [3m[90m<chr>[39m[23m "Sepal.Width", "Species", "Species", "Species"
#> $ var_class      [3m[90m<chr>[39m[23m "numeric", "factor", "factor", "factor"
#> $ var_type       [3m[90m<chr>[39m[23m "continuous", "categorical", "categorical", "categoric…
#> $ var_nlevels    [3m[90m<int>[39m[23m NA, 3, 3, 3
#> $ contrasts      [3m[90m<chr>[39m[23m NA, "contr.treatment", "contr.treatment", "contr.treat…
#> $ contrasts_type [3m[90m<chr>[39m[23m NA, "treatment", "treatment", "treatment"
#> $ reference_row  [3m[90m<lgl>[39m[23m NA, TRUE, FALSE, FALSE
#> $ label          [3m[90m<chr>[39m[23m "Sepal.Width", "setosa", "versicolor", "virginica"
#> $ estimate       [3m[90m<dbl>[39m[23m 0.8035609, NA, 1.4587431, 1.9468166
#> $ std.error      [3m[90m<dbl>[39m[23m 0.1063390, NA, 0.1121079, 0.1000150
#> $ statistic      [3m[90m<dbl>[39m[23m 7.556598, NA, 13.011954, 19.465255
#> $ p.value        [3m[90m<dbl>[39m[23m 4.187340e-12, NA, 3.478232e-26, 2.094475e-42

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
#> [90m# A tibble: 20 x 15[39m
#>    term  variable var_label var_class var_type var_nlevels header_row contrasts
#>    [3m[90m<chr>[39m[23m [3m[90m<chr>[39m[23m    [3m[90m<chr>[39m[23m     [3m[90m<chr>[39m[23m     [3m[90m<chr>[39m[23m          [3m[90m<int>[39m[23m [3m[90m<lgl>[39m[23m      [3m[90m<chr>[39m[23m    
#> [90m 1[39m (Int… (Interc… (Interce… [31mNA[39m        interce…          [31mNA[39m [31mNA[39m         [31mNA[39m       
#> [90m 2[39m [31mNA[39m    age      Age in y… nmatrix.3 continu…          [31mNA[39m TRUE       [31mNA[39m       
#> [90m 3[39m poly… age      Age in y… nmatrix.3 continu…          [31mNA[39m FALSE      [31mNA[39m       
#> [90m 4[39m poly… age      Age in y… nmatrix.3 continu…          [31mNA[39m FALSE      [31mNA[39m       
#> [90m 5[39m poly… age      Age in y… nmatrix.3 continu…          [31mNA[39m FALSE      [31mNA[39m       
#> [90m 6[39m [31mNA[39m    stage    T Stage   factor    categor…           4 TRUE       contr.tr…
#> [90m 7[39m stag… stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#> [90m 8[39m stag… stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#> [90m 9[39m stag… stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#> [90m10[39m stag… stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#> [90m11[39m [31mNA[39m    grade    Grade     factor    categor…           3 TRUE       contr.sum
#> [90m12[39m grad… grade    Grade     factor    categor…           3 FALSE      contr.sum
#> [90m13[39m grad… grade    Grade     factor    categor…           3 FALSE      contr.sum
#> [90m14[39m grad… grade    Grade     factor    categor…           3 FALSE      contr.sum
#> [90m15[39m [31mNA[39m    trt      Chemothe… character dichoto…           2 TRUE       contr.tr…
#> [90m16[39m trtD… trt      Chemothe… character dichoto…           2 FALSE      contr.tr…
#> [90m17[39m trtD… trt      Chemothe… character dichoto…           2 FALSE      contr.tr…
#> [90m18[39m [31mNA[39m    grade:t… Grade * … [31mNA[39m        interac…          [31mNA[39m TRUE       [31mNA[39m       
#> [90m19[39m grad… grade:t… Grade * … [31mNA[39m        interac…          [31mNA[39m FALSE      [31mNA[39m       
#> [90m20[39m grad… grade:t… Grade * … [31mNA[39m        interac…          [31mNA[39m FALSE      [31mNA[39m       
#> [90m# … with 7 more variables: contrasts_type [3m[90m<chr>[90m[23m, reference_row [3m[90m<lgl>[90m[23m,[39m
#> [90m#   label [3m[90m<chr>[90m[23m, estimate [3m[90m<dbl>[90m[23m, std.error [3m[90m<dbl>[90m[23m, statistic [3m[90m<dbl>[90m[23m,[39m
#> [90m#   p.value [3m[90m<dbl>[90m[23m[39m
dplyr::glimpse(ex4)
#> Rows: 20
#> Columns: 15
#> $ term           [3m[90m<chr>[39m[23m "(Intercept)", NA, "poly(age, 3)1", "poly(age, 3)2", "…
#> $ variable       [3m[90m<chr>[39m[23m "(Intercept)", "age", "age", "age", "age", "stage", "s…
#> $ var_label      [3m[90m<chr>[39m[23m "(Intercept)", "Age in years", "Age in years", "Age in…
#> $ var_class      [3m[90m<chr>[39m[23m NA, "nmatrix.3", "nmatrix.3", "nmatrix.3", "nmatrix.3"…
#> $ var_type       [3m[90m<chr>[39m[23m "intercept", "continuous", "continuous", "continuous",…
#> $ var_nlevels    [3m[90m<int>[39m[23m NA, NA, NA, NA, NA, 4, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2…
#> $ header_row     [3m[90m<lgl>[39m[23m NA, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FAL…
#> $ contrasts      [3m[90m<chr>[39m[23m NA, NA, NA, NA, NA, "contr.treatment(base=3)", "contr.…
#> $ contrasts_type [3m[90m<chr>[39m[23m NA, NA, NA, NA, NA, "treatment", "treatment", "treatme…
#> $ reference_row  [3m[90m<lgl>[39m[23m NA, NA, NA, NA, NA, NA, FALSE, FALSE, TRUE, FALSE, NA,…
#> $ label          [3m[90m<chr>[39m[23m "(Intercept)", "Age in years", "Age in years", "Age in…
#> $ estimate       [3m[90m<dbl>[39m[23m 0.5266376, NA, 20.2416394, 1.2337899, 0.4931553, NA, 1…
#> $ std.error      [3m[90m<dbl>[39m[23m 0.4130930, NA, 2.3254455, 2.3512842, 2.3936657, NA, 0.…
#> $ statistic      [3m[90m<dbl>[39m[23m -1.55229592, NA, 1.29340459, 0.08935144, -0.29533409, …
#> $ p.value        [3m[90m<dbl>[39m[23m 0.1205914, NA, 0.1958712, 0.9288026, 0.7677387, NA, 0.…
```
