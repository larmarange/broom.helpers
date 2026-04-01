# Group results by selected columns

Indicates that results should be grouped. By default
(`group_by = auto_group_by()`), results will be grouped according to the
`y.level` column (for multinomial models) or the `component` column
(multi-components models) if any.

## Usage

``` r
tidy_group_by(
  x,
  group_by = auto_group_by(),
  group_labels = NULL,
  model = tidy_get_model(x)
)

auto_group_by()
```

## Arguments

- x:

  (`data.frame`)  
  A tidy tibble as produced by `tidy_*()` functions.

- group_by:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  One or several variables to group by. Default is `auto_group_by()`.
  Use `NULL` to force ungrouping.

- group_labels:

  (`string`)  
  An optional named vector of custom term labels.

- model:

  (a model object, e.g. `glm`)  
  The corresponding model, if not attached to `x`.

## Value

The `x` tibble with, if relevant, an additional `group_by` column.

## See also

Other tidy_helpers:
[`tidy_add_coefficients_type()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_coefficients_type.md),
[`tidy_add_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_contrasts.md),
[`tidy_add_estimate_to_reference_rows()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_estimate_to_reference_rows.md),
[`tidy_add_header_rows()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_header_rows.md),
[`tidy_add_n()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_n.md),
[`tidy_add_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_pairwise_contrasts.md),
[`tidy_add_reference_rows()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_reference_rows.md),
[`tidy_add_term_labels()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_term_labels.md),
[`tidy_add_variable_labels()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_variable_labels.md),
[`tidy_attach_model()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_attach_model.md),
[`tidy_disambiguate_terms()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_disambiguate_terms.md),
[`tidy_identify_variables()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_identify_variables.md),
[`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_plus_plus.md),
[`tidy_remove_intercept()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_remove_intercept.md),
[`tidy_select_variables()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_select_variables.md)

## Examples

``` r
mod <- multinom(Species ~ Petal.Width + Petal.Length, data = iris)
#> # weights:  12 (6 variable)
#> initial  value 164.791843 
#> iter  10 value 12.657828
#> iter  20 value 10.374056
#> iter  30 value 10.330881
#> iter  40 value 10.306926
#> iter  50 value 10.300057
#> iter  60 value 10.296452
#> iter  70 value 10.294046
#> iter  80 value 10.292029
#> iter  90 value 10.291154
#> iter 100 value 10.289505
#> final  value 10.289505 
#> stopped after 100 iterations
mod |> tidy_and_attach() |> tidy_group_by()
#> # A tibble: 6 × 9
#>   group_by y.level term  estimate std.error statistic p.value conf.low conf.high
#>   <fct>    <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 versico… versic… (Int…   -22.8       44.4   -0.514    0.607   -110.       64.2
#> 2 versico… versic… Peta…     7.88      81.0    0.0973   0.923   -151.      167. 
#> 3 versico… versic… Peta…     6.92      37.6    0.184    0.854    -66.7      80.6
#> 4 virgini… virgin… (Int…   -67.8       46.4   -1.46     0.144   -159.       23.1
#> 5 virgini… virgin… Peta…    18.3       81.1    0.225    0.822   -141.      177. 
#> 6 virgini… virgin… Peta…    12.6       37.7    0.336    0.737    -61.2      86.5

mod |>
  tidy_and_attach() |>
  tidy_group_by(group_labels = c(versicolor = "harlequin blueflag"))
#> # A tibble: 6 × 9
#>   group_by y.level term  estimate std.error statistic p.value conf.low conf.high
#>   <fct>    <chr>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 harlequ… versic… (Int…   -22.8       44.4   -0.514    0.607   -110.       64.2
#> 2 harlequ… versic… Peta…     7.88      81.0    0.0973   0.923   -151.      167. 
#> 3 harlequ… versic… Peta…     6.92      37.6    0.184    0.854    -66.7      80.6
#> 4 virgini… virgin… (Int…   -67.8       46.4   -1.46     0.144   -159.       23.1
#> 5 virgini… virgin… Peta…    18.3       81.1    0.225    0.822   -141.      177. 
#> 6 virgini… virgin… Peta…    12.6       37.7    0.336    0.737    -61.2      86.5

mod |> tidy_and_attach() |> tidy_group_by(group_by = NULL)
#> # A tibble: 6 × 8
#>   y.level    term        estimate std.error statistic p.value conf.low conf.high
#>   <chr>      <chr>          <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 versicolor (Intercept)   -22.8       44.4   -0.514    0.607   -110.       64.2
#> 2 versicolor Petal.Width     7.88      81.0    0.0973   0.923   -151.      167. 
#> 3 versicolor Petal.Leng…     6.92      37.6    0.184    0.854    -66.7      80.6
#> 4 virginica  (Intercept)   -67.8       46.4   -1.46     0.144   -159.       23.1
#> 5 virginica  Petal.Width    18.3       81.1    0.225    0.822   -141.      177. 
#> 6 virginica  Petal.Leng…    12.6       37.7    0.336    0.737    -61.2      86.5

mod |>
  tidy_and_attach() |>
  tidy_identify_variables() |>
  tidy_group_by(group_by = variable)
#> # A tibble: 6 × 13
#>   group_by     y.level    term  variable var_class var_type var_nlevels estimate
#>   <fct>        <chr>      <chr> <chr>    <chr>     <chr>          <int>    <dbl>
#> 1 (Intercept)  versicolor (Int… (Interc… NA        interce…          NA   -22.8 
#> 2 (Intercept)  virginica  (Int… (Interc… NA        interce…          NA   -67.8 
#> 3 Petal.Width  versicolor Peta… Petal.W… numeric   continu…          NA     7.88
#> 4 Petal.Width  virginica  Peta… Petal.W… numeric   continu…          NA    18.3 
#> 5 Petal.Length versicolor Peta… Petal.L… numeric   continu…          NA     6.92
#> 6 Petal.Length virginica  Peta… Petal.L… numeric   continu…          NA    12.6 
#> # ℹ 5 more variables: std.error <dbl>, statistic <dbl>, p.value <dbl>,
#> #   conf.low <dbl>, conf.high <dbl>
```
