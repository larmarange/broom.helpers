# Get model offset

This function does not cover `lavaan` models (`NULL` is returned).

## Usage

``` r
model_get_offset(model)

# Default S3 method
model_get_offset(model)
```

## Arguments

- model:

  (a model object, e.g. `glm`)  
  A model object.

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
[`model_get_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_pairwise_contrasts.md),
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
mod <- glm(
  response ~ trt + offset(log(ttdeath)),
  gtsummary::trial,
  family = poisson
)
mod |> model_get_offset()
#>   [1] 3.178054 3.178054 3.178054 2.870169 2.799109 2.749832 3.178054 2.913980
#>   [9] 3.178054 2.354228 3.178054 3.178054 2.663053 2.556452 3.121483 2.164472
#>  [17] 3.178054 2.721953 3.178054 3.178054 3.178054 3.178054 2.828496 3.173460
#>  [25] 1.843719 2.758109 3.178054 3.178054 2.737609 2.858193 3.178054 3.039749
#>  [33] 3.178054 3.178054 3.178054 2.527327 3.178054 2.746630 2.890372 2.891482
#>  [41] 2.520113 2.493205 3.178054 2.857619 3.178054 3.178054 3.178054 2.500616
#>  [49] 2.304583 2.903069 2.343727 3.178054 3.178054 2.962175 2.502255 2.671386
#>  [57] 2.962175 2.318458 2.576422 2.314514 3.178054 3.125444 3.178054 3.096934
#>  [65] 3.178054 3.026261 3.145445 1.998774 3.178054 3.178054 3.178054 3.178054
#>  [73] 3.178054 2.955951 1.983756 3.173041 2.786861 3.178054 2.643334 3.178054
#>  [81] 3.178054 3.178054 2.799717 3.170106 3.178054 2.910719 2.437116 3.041661
#>  [89] 1.673351 3.132010 2.335052 3.178054 3.178054 2.676903 2.951780 3.178054
#>  [97] 3.053529 2.776954 2.299581 3.178054 3.178054 2.983153 2.813611 2.414126
#> [105] 2.906354 3.178054 2.865624 2.859340 3.178054 3.129389 2.615935 3.178054
#> [113] 3.178054 2.859913 3.178054 3.178054 3.178054 3.178054 3.178054 2.564949
#> [121] 2.275214 2.750471 3.178054 1.261298 3.013081 3.153163 2.801541 3.178054
#> [129] 3.178054 2.684440 2.879760 3.178054 3.083285 3.178054 3.178054 2.540026
#> [137] 3.178054 3.178054 3.178054 3.178054 2.309561 3.178054 3.178054 3.178054
#> [145] 3.012098 3.178054 3.178054 3.060115 2.536075 2.571084 3.178054 2.714695
#> [153] 3.002708 2.356126 3.178054 3.178054 3.178054 3.178054 3.161247 3.178054
#> [161] 2.994732 2.744061 3.166319 3.109507 2.972464 2.807594 3.178054 3.178054
#> [169] 3.178054 3.086943 3.178054 2.528126 3.178054 2.695978 2.800933 3.178054
#> [177] 2.223542 2.877512 3.178054 3.178054 2.294553 2.782539 2.352327 3.035434
#> [185] 3.178054 2.799717 3.178054 3.109061 3.178054 3.072693 3.178054 2.986187
#> [193] 3.178054
#> attr(,"label")
#> [1] "Months to Death/Censor"
```
