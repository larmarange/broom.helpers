# Get the terms of a model

Return the result of
[`stats::terms()`](https://rdrr.io/r/stats/terms.html) applied to the
model or `NULL` if it is not possible to get terms from `model`.

## Usage

``` r
model_get_terms(model)

# Default S3 method
model_get_terms(model)

# S3 method for class 'brmsfit'
model_get_terms(model)

# S3 method for class 'glmmTMB'
model_get_terms(model)

# S3 method for class 'model_fit'
model_get_terms(model)

# S3 method for class 'betareg'
model_get_terms(model)

# S3 method for class 'betareg'
model_get_terms(model)

# S3 method for class 'cch'
model_get_terms(model)

# S3 method for class 'fixest'
model_get_terms(model)

# S3 method for class 'svy_vglm'
model_get_terms(model)
```

## Arguments

- model:

  (a model object, e.g. `glm`)  
  A model object.

## Details

For models fitted with
[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html), it
will return a terms object taking into account all components ("cond"
and "zi"). For a more restricted terms object, please refer to
[`glmmTMB::terms.glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB_methods.html).

For `fixest` models, return a term object combining main variables and
instrumental variables.

## See also

[`stats::terms()`](https://rdrr.io/r/stats/terms.html)

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
[`model_get_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_pairwise_contrasts.md),
[`model_get_response()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_response.md),
[`model_get_response_variable()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_response_variable.md),
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
  model_get_terms()
#> hp ~ mpg + factor(cyl)
#> attr(,"variables")
#> list(hp, mpg, factor(cyl))
#> attr(,"factors")
#>             mpg factor(cyl)
#> hp            0           0
#> mpg           1           0
#> factor(cyl)   0           1
#> attr(,"term.labels")
#> [1] "mpg"         "factor(cyl)"
#> attr(,"order")
#> [1] 1 1
#> attr(,"intercept")
#> [1] 1
#> attr(,"response")
#> [1] 1
#> attr(,".Environment")
#> <environment: 0x55bbe2d74210>
#> attr(,"predvars")
#> list(hp, mpg, factor(cyl))
#> attr(,"dataClasses")
#>          hp         mpg factor(cyl) 
#>   "numeric"   "numeric"    "factor" 
```
