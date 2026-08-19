# Get coefficient type

Indicate the type of coefficient among "generic", "logistic", "poisson",
"relative_risk" or "prop_hazard".

## Usage

``` r
model_get_coefficients_type(model)

# Default S3 method
model_get_coefficients_type(model)

# S3 method for class 'glm'
model_get_coefficients_type(model)

# S3 method for class 'negbin'
model_get_coefficients_type(model)

# S3 method for class 'geeglm'
model_get_coefficients_type(model)

# S3 method for class 'fixest'
model_get_coefficients_type(model)

# S3 method for class 'biglm'
model_get_coefficients_type(model)

# S3 method for class 'glmerMod'
model_get_coefficients_type(model)

# S3 method for class 'clogit'
model_get_coefficients_type(model)

# S3 method for class 'polr'
model_get_coefficients_type(model)

# S3 method for class 'multinom'
model_get_coefficients_type(model)

# S3 method for class 'svyolr'
model_get_coefficients_type(model)

# S3 method for class 'clm'
model_get_coefficients_type(model)

# S3 method for class 'clmm'
model_get_coefficients_type(model)

# S3 method for class 'coxph'
model_get_coefficients_type(model)

# S3 method for class 'crr'
model_get_coefficients_type(model)

# S3 method for class 'tidycrr'
model_get_coefficients_type(model)

# S3 method for class 'cch'
model_get_coefficients_type(model)

# S3 method for class 'model_fit'
model_get_coefficients_type(model)

# S3 method for class 'LORgee'
model_get_coefficients_type(model)

# S3 method for class 'vglm'
model_get_coefficients_type(model)

# S3 method for class 'vgam'
model_get_coefficients_type(model)

# S3 method for class 'svy_vglm'
model_get_coefficients_type(model)

# S3 method for class 'brmsfit'
model_get_coefficients_type(model)
```

## Arguments

- model:

  (a model object, e.g. `glm`)  
  A model object.

## See also

Other model_helpers:
[`model_compute_terms_contributions()`](https://larmarange.github.io/broom.helpers/dev/reference/model_compute_terms_contributions.md),
[`model_get_assign()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_assign.md),
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
  model_get_coefficients_type()
#> [1] "generic"

df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
glm(Survived ~ Class + Age * Sex, data = df, weights = df$n, family = binomial) |>
  model_get_coefficients_type()
#> [1] "logistic"
```
