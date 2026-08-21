# Get the model matrix of a model

The structure of the object returned by
[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
could slightly differ for certain types of models.
`model_get_model_matrix()` will always return an object with the same
structure as
[`stats::model.matrix.default()`](https://rdrr.io/r/stats/model.matrix.html).

## Usage

``` r
model_get_model_matrix(model, ...)

# Default S3 method
model_get_model_matrix(model, ...)

# S3 method for class 'multinom'
model_get_model_matrix(model, ...)

# S3 method for class 'clm'
model_get_model_matrix(model, ...)

# S3 method for class 'brmsfit'
model_get_model_matrix(model, ...)

# S3 method for class 'glmmTMB'
model_get_model_matrix(model, ...)

# S3 method for class 'plm'
model_get_model_matrix(model, ...)

# S3 method for class 'biglm'
model_get_model_matrix(model, ...)

# S3 method for class 'model_fit'
model_get_model_matrix(model, ...)

# S3 method for class 'LORgee'
model_get_model_matrix(model, ...)

# S3 method for class 'betareg'
model_get_model_matrix(model, ...)

# S3 method for class 'cch'
model_get_model_matrix(model, ...)

# S3 method for class 'vglm'
model_get_model_matrix(model, ...)

# S3 method for class 'vgam'
model_get_model_matrix(model, ...)

# S3 method for class 'svy_vglm'
model_get_model_matrix(model, ...)
```

## Arguments

- model:

  (a model object, e.g. `glm`)  
  A model object.

- ...:

  Additional arguments passed to
  [`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html).

## Details

For models fitted with
[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html), it
will return a model matrix taking into account all components ("cond",
"zi" and "disp"). For a more restricted model matrix, please refer to
[`glmmTMB::model.matrix.glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB_methods.html).

For [`plm::plm()`](https://rdrr.io/pkg/plm/man/plm.html) models,
constant columns are not removed.

## See also

[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)

Other model_helpers:
[`model_compute_terms_contributions()`](https://larmarange.github.io/broom.helpers/reference/model_compute_terms_contributions.md),
[`model_get_assign()`](https://larmarange.github.io/broom.helpers/reference/model_get_assign.md),
[`model_get_coefficients_type()`](https://larmarange.github.io/broom.helpers/reference/model_get_coefficients_type.md),
[`model_get_contrasts()`](https://larmarange.github.io/broom.helpers/reference/model_get_contrasts.md),
[`model_get_model()`](https://larmarange.github.io/broom.helpers/reference/model_get_model.md),
[`model_get_model_frame()`](https://larmarange.github.io/broom.helpers/reference/model_get_model_frame.md),
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
lm(hp ~ mpg + factor(cyl), mtcars) |>
  model_get_model_matrix() |>
  head()
#>                   (Intercept)  mpg factor(cyl)6 factor(cyl)8
#> Mazda RX4                   1 21.0            1            0
#> Mazda RX4 Wag               1 21.0            1            0
#> Datsun 710                  1 22.8            0            0
#> Hornet 4 Drive              1 21.4            1            0
#> Hornet Sportabout           1 18.7            0            1
#> Valiant                     1 18.1            1            0
```
