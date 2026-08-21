# Select helper functions

Set of functions to supplement the *tidyselect* set of functions for
selecting columns of data frames (and other items as well).

- `all_continuous()` selects continuous variables

- `all_categorical()` selects categorical (including `"dichotomous"`)
  variables

- `all_dichotomous()` selects only type `"dichotomous"`

- `all_interaction()` selects interaction terms from a regression model

- `all_intercepts()` selects intercept terms from a regression model

- `all_contrasts()` selects variables in regression model based on their
  type of contrast

- `all_ran_pars()` and `all_ran_vals()` for random-effect parameters and
  values from a mixed model (see
  [`vignette("broom_mixed_intro", package = "broom.mixed")`](https://cran.rstudio.com/web/packages/broom.mixed/vignettes/broom_mixed_intro.html))

## Usage

``` r
all_continuous(continuous2 = TRUE)

all_categorical(dichotomous = TRUE)

all_dichotomous()

all_interaction()

all_ran_pars()

all_ran_vals()

all_intercepts()

all_contrasts(
  contrasts_type = c("treatment", "sum", "poly", "helmert", "sdif", "other")
)
```

## Arguments

- continuous2:

  (`logical`)  
  Whether to include continuous2 variables, default is `TRUE`. For
  compatibility with `{gtsummary}`), see
  [`gtsummary::all_continuous2()`](https://www.danieldsjoberg.com/gtsummary/reference/select_helpers.html).

- dichotomous:

  (`logical`)  
  Whether to include dichotomous variables, default is `TRUE`.

- contrasts_type:

  (`string`)  
  Type of contrast to select. When `NULL`, all variables with a contrast
  will be selected. Default is `NULL`. Select among contrast types
  `c("treatment", "sum", "poly", "helmert", "sdif", "other")`.

## Value

A character vector of column names selected.

## See also

[`scope_tidy()`](https://larmarange.github.io/broom.helpers/reference/scope_tidy.md)

## Examples

``` r
# \donttest{
glm(response ~ age * trt + grade, gtsummary::trial, family = binomial) |>
  tidy_plus_plus(exponentiate = TRUE, include = all_categorical())
#> # A tibble: 5 × 18
#>   term      variable var_label          var_class var_type var_nlevels contrasts
#>   <chr>     <chr>    <chr>              <chr>     <chr>          <int> <chr>    
#> 1 trtDrug A trt      Chemotherapy Trea… character dichoto…           2 contr.tr…
#> 2 trtDrug B trt      Chemotherapy Trea… character dichoto…           2 contr.tr…
#> 3 gradeI    grade    Grade              factor    categor…           3 contr.tr…
#> 4 gradeII   grade    Grade              factor    categor…           3 contr.tr…
#> 5 gradeIII  grade    Grade              factor    categor…           3 contr.tr…
#> # ℹ 11 more variables: contrasts_type <chr>, reference_row <lgl>, label <chr>,
#> #   n_obs <dbl>, n_event <dbl>, estimate <dbl>, std.error <dbl>,
#> #   statistic <dbl>, p.value <dbl>, conf.low <dbl>, conf.high <dbl>
# }
# \donttest{
  glm(response ~ age + trt + grade + stage,
    gtsummary::trial,
    family = binomial,
    contrasts = list(trt = contr.SAS, grade = contr.sum, stage = contr.poly)
  ) |>
    tidy_plus_plus(
      exponentiate = TRUE,
      include = all_contrasts(c("treatment", "sum"))
    )
#> # A tibble: 5 × 18
#>   term      variable var_label          var_class var_type var_nlevels contrasts
#>   <chr>     <chr>    <chr>              <chr>     <chr>          <int> <chr>    
#> 1 trtDrug A trt      Chemotherapy Trea… character dichoto…           2 contr.SAS
#> 2 trtDrug B trt      Chemotherapy Trea… character dichoto…           2 contr.SAS
#> 3 grade1    grade    Grade              factor    categor…           3 contr.sum
#> 4 grade2    grade    Grade              factor    categor…           3 contr.sum
#> 5 grade3    grade    Grade              factor    categor…           3 contr.sum
#> # ℹ 11 more variables: contrasts_type <chr>, reference_row <lgl>, label <chr>,
#> #   n_obs <dbl>, n_event <dbl>, estimate <dbl>, std.error <dbl>,
#> #   statistic <dbl>, p.value <dbl>, conf.low <dbl>, conf.high <dbl>
# }
```
