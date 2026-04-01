# Identify for each coefficient of a model the corresponding variable

It will also identify interaction terms and intercept(s).

## Usage

``` r
model_identify_variables(model)

# Default S3 method
model_identify_variables(model)

# S3 method for class 'lavaan'
model_identify_variables(model)

# S3 method for class 'aov'
model_identify_variables(model)

# S3 method for class 'clm'
model_identify_variables(model)

# S3 method for class 'clmm'
model_identify_variables(model)

# S3 method for class 'gam'
model_identify_variables(model)

# S3 method for class 'model_fit'
model_identify_variables(model)

# S3 method for class 'logitr'
model_identify_variables(model)

# S3 method for class 'svy_vglm'
model_identify_variables(model)
```

## Arguments

- model:

  (a model object, e.g. `glm`)  
  A model object.

## Value

A tibble with four columns:

- `term`: coefficients of the model

- `variable`: the corresponding variable

- `var_class`: class of the variable (cf.
  [`stats::.MFclass()`](https://rdrr.io/r/stats/checkMFClasses.html))

- `var_type`: `"continuous"`, `"dichotomous"` (categorical variable with
  2 levels), `"categorical"` (categorical variable with 3 or more
  levels), `"intercept"` or `"interaction"`

- `var_nlevels`: number of original levels for categorical variables

## See also

[`tidy_identify_variables()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_identify_variables.md)

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
[`model_get_terms()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_terms.md),
[`model_get_weights()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_weights.md),
[`model_get_xlevels()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_xlevels.md),
[`model_list_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/model_list_contrasts.md),
[`model_list_higher_order_variables()`](https://larmarange.github.io/broom.helpers/dev/reference/model_list_higher_order_variables.md),
[`model_list_terms_levels()`](https://larmarange.github.io/broom.helpers/dev/reference/model_list_terms_levels.md),
[`model_list_variables()`](https://larmarange.github.io/broom.helpers/dev/reference/model_list_variables.md)

## Examples

``` r
df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
glm(
  Survived ~ Class + Age * Sex,
  data = df, weights = df$n,
  family = binomial
) |>
  model_identify_variables()
#> # A tibble: 7 × 5
#>   term             variable var_class var_nlevels var_type   
#>   <chr>            <chr>    <chr>           <int> <chr>      
#> 1 (Intercept)      NA       NA                 NA intercept  
#> 2 Class2nd         Class    character           4 categorical
#> 3 Class3rd         Class    character           4 categorical
#> 4 ClassCrew        Class    character           4 categorical
#> 5 AgeChild         Age      character           2 dichotomous
#> 6 SexMale          Sex      character           2 dichotomous
#> 7 AgeChild:SexMale Age:Sex  NA                 NA interaction

lm(
  Sepal.Length ~ poly(Sepal.Width, 2) + Species,
  data = iris,
  contrasts = list(Species = contr.sum)
) |>
  model_identify_variables()
#> # A tibble: 5 × 5
#>   term                  variable    var_class var_nlevels var_type   
#>   <chr>                 <chr>       <chr>           <int> <chr>      
#> 1 (Intercept)           NA          NA                 NA intercept  
#> 2 poly(Sepal.Width, 2)1 Sepal.Width nmatrix.2          NA continuous 
#> 3 poly(Sepal.Width, 2)2 Sepal.Width nmatrix.2          NA continuous 
#> 4 Species1              Species     factor              3 categorical
#> 5 Species2              Species     factor              3 categorical
```
