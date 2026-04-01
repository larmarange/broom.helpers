# Get the model frame of a model

The structure of the object returned by
[`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html) could
slightly differ for certain types of models. `model_get_model_frame()`
will always return an object with the same data structure or `NULL` if
it is not possible to compute model frame from `model`.

## Usage

``` r
model_get_model_frame(model)

# Default S3 method
model_get_model_frame(model)

# S3 method for class 'coxph'
model_get_model_frame(model)

# S3 method for class 'svycoxph'
model_get_model_frame(model)

# S3 method for class 'survreg'
model_get_model_frame(model)

# S3 method for class 'biglm'
model_get_model_frame(model)

# S3 method for class 'model_fit'
model_get_model_frame(model)

# S3 method for class 'fixest'
model_get_model_frame(model)

# S3 method for class 'svy_vglm'
model_get_model_frame(model)
```

## Arguments

- model:

  (a model object, e.g. `glm`)  
  A model object.

## See also

[`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html)

Other model_helpers:
[`model_compute_terms_contributions()`](https://larmarange.github.io/broom.helpers/dev/reference/model_compute_terms_contributions.md),
[`model_get_assign()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_assign.md),
[`model_get_coefficients_type()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_coefficients_type.md),
[`model_get_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_contrasts.md),
[`model_get_model()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_model.md),
[`model_get_model_matrix()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_model_matrix.md),
[`model_get_n()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_n.md),
[`model_get_nlevels()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_nlevels.md),
[`model_get_offset()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_offset.md),
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
lm(hp ~ mpg + factor(cyl), mtcars) |>
  model_get_model_frame() |>
  head()
#>                    hp  mpg factor(cyl)
#> Mazda RX4         110 21.0           6
#> Mazda RX4 Wag     110 21.0           6
#> Datsun 710         93 22.8           4
#> Hornet 4 Drive    110 21.4           6
#> Hornet Sportabout 175 18.7           8
#> Valiant           105 18.1           6
```
