# Get pairwise comparison of the levels of a categorical variable

It is computed with
[`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html).

## Usage

``` r
model_get_pairwise_contrasts(
  model,
  variables,
  pairwise_reverse = TRUE,
  contrasts_adjust = NULL,
  conf.level = 0.95,
  emmeans_args = list()
)
```

## Arguments

- model:

  (a model object, e.g. `glm`)  
  A model object.

- variables:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to add pairwise contrasts.

- pairwise_reverse:

  (`logical`)  
  Determines whether to use `"pairwise"` (if `TRUE`) or `"revpairwise"`
  (if `FALSE`), see
  [`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html).

- contrasts_adjust:

  optional adjustment method when computing contrasts, see
  [`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html)
  (if `NULL`, use `emmeans` default)

- conf.level:

  (`numeric`)  
  Level of confidence for confidence intervals (default: 95%).

- emmeans_args:

  (`logical`)  
  List of additional parameter to pass to
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
  when computing pairwise contrasts.

## Details

For [`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) and
[`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html) models,
pairwise contrasts are computed separately for each component, using
`mode = "count"` and `mode = "zero"` (see documentation of `emmeans`)
and a component column is added to the results.

## See also

Other model_helpers:
[`model_compute_terms_contributions()`](https://larmarange.github.io/broom.helpers/dev/reference/model_compute_terms_contributions.md),
[`model_get_assign()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_assign.md),
[`model_get_coefficients_type()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_coefficients_type.md),
[`model_get_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_contrasts.md),
[`model_get_model()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_model.md),
[`model_get_model_frame()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_model_frame.md),
[`model_get_model_matrix()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_model_matrix.md),
[`model_get_n()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_n.md),
[`model_get_nlevels()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_nlevels.md),
[`model_get_offset()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_offset.md),
[`model_get_response()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_response.md),
[`model_get_response_variable()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_response_variable.md),
[`model_get_terms()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_terms.md),
[`model_get_weights()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_weights.md),
[`model_get_xlevels()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_xlevels.md),
[`model_identify_variables()`](https://larmarange.github.io/broom.helpers/dev/reference/model_identify_variables.md),
[`model_list_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/model_list_contrasts.md),
[`model_list_higher_order_variables()`](https://larmarange.github.io/broom.helpers/dev/reference/model_list_higher_order_variables.md),
[`model_list_terms_levels()`](https://larmarange.github.io/broom.helpers/dev/reference/model_list_terms_levels.md),
[`model_list_variables()`](https://larmarange.github.io/broom.helpers/dev/reference/model_list_variables.md)

## Examples

``` r
# \donttest{
  mod <- lm(Sepal.Length ~ Species, data = iris)
  mod |> model_get_pairwise_contrasts(variables = "Species")
#> # A tibble: 3 × 10
#>   variable term         estimate std.error statistic  p.value conf.low conf.high
#>   <chr>    <chr>           <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
#> 1 Species  versicolor …    0.93      0.103      9.03 3.39e-14    0.686     1.17 
#> 2 Species  virginica -…    1.58      0.103     15.4  3.00e-15    1.34      1.83 
#> 3 Species  virginica -…    0.652     0.103      6.33 8.29e- 9    0.408     0.896
#> # ℹ 2 more variables: contrasts <chr>, contrasts_type <chr>
  mod |>
    model_get_pairwise_contrasts(
      variables = "Species",
      contrasts_adjust = "none"
    )
#> # A tibble: 3 × 10
#>   variable term         estimate std.error statistic  p.value conf.low conf.high
#>   <chr>    <chr>           <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
#> 1 Species  versicolor …    0.93      0.103      9.03 8.77e-16    0.727     1.13 
#> 2 Species  virginica -…    1.58      0.103     15.4  2.21e-32    1.38      1.79 
#> 3 Species  virginica -…    0.652     0.103      6.33 2.77e- 9    0.449     0.855
#> # ℹ 2 more variables: contrasts <chr>, contrasts_type <chr>
# }
```
