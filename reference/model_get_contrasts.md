# Get contrasts used in the model

Get contrasts used in the model

## Usage

``` r
model_get_contrasts(model)

# S3 method for class 'model_fit'
model_get_contrasts(model)

# S3 method for class 'zeroinfl'
model_get_contrasts(model)

# S3 method for class 'hurdle'
model_get_contrasts(model)

# S3 method for class 'betareg'
model_get_contrasts(model)

# S3 method for class 'svy_vglm'
model_get_contrasts(model)

# S3 method for class 'fixest'
model_get_contrasts(model)
```

## Arguments

- model:

  (a model object, e.g. `glm`)  
  A model object.

## See also

Other model_helpers:
[`model_compute_terms_contributions()`](https://larmarange.github.io/broom.helpers/reference/model_compute_terms_contributions.md),
[`model_get_assign()`](https://larmarange.github.io/broom.helpers/reference/model_get_assign.md),
[`model_get_coefficients_type()`](https://larmarange.github.io/broom.helpers/reference/model_get_coefficients_type.md),
[`model_get_model()`](https://larmarange.github.io/broom.helpers/reference/model_get_model.md),
[`model_get_model_frame()`](https://larmarange.github.io/broom.helpers/reference/model_get_model_frame.md),
[`model_get_model_matrix()`](https://larmarange.github.io/broom.helpers/reference/model_get_model_matrix.md),
[`model_get_n()`](https://larmarange.github.io/broom.helpers/reference/model_get_n.md),
[`model_get_nlevels()`](https://larmarange.github.io/broom.helpers/reference/model_get_nlevels.md),
[`model_get_offset()`](https://larmarange.github.io/broom.helpers/reference/model_get_offset.md),
[`model_get_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/reference/model_get_pairwise_contrasts.md),
[`model_get_response()`](https://larmarange.github.io/broom.helpers/reference/model_get_response.md),
[`model_get_response_variable()`](https://larmarange.github.io/broom.helpers/reference/model_get_response_variable.md),
[`model_get_terms()`](https://larmarange.github.io/broom.helpers/reference/model_get_terms.md),
[`model_get_weights()`](https://larmarange.github.io/broom.helpers/reference/model_get_weights.md),
[`model_get_xlevels()`](https://larmarange.github.io/broom.helpers/reference/model_get_xlevels.md),
[`model_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/model_identify_variables.md),
[`model_list_contrasts()`](https://larmarange.github.io/broom.helpers/reference/model_list_contrasts.md),
[`model_list_higher_order_variables()`](https://larmarange.github.io/broom.helpers/reference/model_list_higher_order_variables.md),
[`model_list_terms_levels()`](https://larmarange.github.io/broom.helpers/reference/model_list_terms_levels.md),
[`model_list_variables()`](https://larmarange.github.io/broom.helpers/reference/model_list_variables.md)

## Examples

``` r
glm(
  am ~ mpg + factor(cyl),
  data = mtcars,
  family = binomial,
  contrasts = list(`factor(cyl)` = contr.sum)
) |>
  model_get_contrasts()
#> $`factor(cyl)`
#>   [,1] [,2]
#> 4    1    0
#> 6    0    1
#> 8   -1   -1
#> 
```
