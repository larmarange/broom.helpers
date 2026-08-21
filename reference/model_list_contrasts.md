# List contrasts used by a model

List contrasts used by a model

## Usage

``` r
model_list_contrasts(model)

# Default S3 method
model_list_contrasts(model)
```

## Arguments

- model:

  (a model object, e.g. `glm`)  
  A model object.

## Value

A tibble with three columns:

- `variable`: variable name

- `contrasts`: contrasts used

- `contrasts_type`: type of contrasts ("treatment", "sum", "poly",
  "helmert", "sdiff, "other" or "no.contrast")

- `reference`: for variables with treatment, SAS or sum contrasts,
  position of the reference level

## Details

For models with no intercept, no contrasts will be applied to one of the
categorical variable. In such case, one dummy term will be returned for
each level of the categorical variable.

## See also

Other model_helpers:
[`model_compute_terms_contributions()`](https://larmarange.github.io/broom.helpers/reference/model_compute_terms_contributions.md),
[`model_get_assign()`](https://larmarange.github.io/broom.helpers/reference/model_get_assign.md),
[`model_get_coefficients_type()`](https://larmarange.github.io/broom.helpers/reference/model_get_coefficients_type.md),
[`model_get_contrasts()`](https://larmarange.github.io/broom.helpers/reference/model_get_contrasts.md),
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
  model_list_contrasts()
#> # A tibble: 1 × 4
#>   variable    contrasts reference contrasts_type
#>   <chr>       <chr>         <int> <chr>         
#> 1 factor(cyl) contr.sum         3 sum           
```
