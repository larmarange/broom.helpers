# List all the variables used in a model

Including variables used only in an interaction.

## Usage

``` r
model_list_variables(
  model,
  labels = NULL,
  only_variable = FALSE,
  add_var_type = FALSE,
  instrumental_suffix = " (instrumental)"
)

# Default S3 method
model_list_variables(
  model,
  labels = NULL,
  only_variable = FALSE,
  add_var_type = FALSE,
  instrumental_suffix = " (instrumental)"
)

# S3 method for class 'lavaan'
model_list_variables(
  model,
  labels = NULL,
  only_variable = FALSE,
  add_var_type = FALSE,
  instrumental_suffix = " (instrumental)"
)

# S3 method for class 'logitr'
model_list_variables(
  model,
  labels = NULL,
  only_variable = FALSE,
  add_var_type = FALSE,
  instrumental_suffix = " (instrumental)"
)
```

## Arguments

- model:

  (a model object, e.g. `glm`)  
  A model object.

- labels:

  (`list` or `string`)  
  An optional named list or named vector of custom variable labels.

- only_variable:

  (`logical`)  
  If `TRUE`, will return only "variable" column.

- add_var_type:

  (`logical`)  
  If `TRUE`, add `var_nlevels` and `var_type` columns.

- instrumental_suffix:

  (`string`)  
  Suffix added to variable labels for instrumental variables (`fixest`
  models). `NULL` to add nothing.

## Value

A tibble with three columns:

- `variable`: the corresponding variable

- `var_class`: class of the variable (cf.
  [`stats::.MFclass()`](https://rdrr.io/r/stats/checkMFClasses.html))

- `label_attr`: variable label defined in the original data frame with
  the label attribute (cf.
  [`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html))

- `var_label`: a variable label (by priority, `labels` if defined,
  `label_attr` if available, otherwise `variable`)

If `add_var_type = TRUE`:

- `var_type`: `"continuous"`, `"dichotomous"` (categorical variable with
  2 levels), `"categorical"` (categorical variable with 3 or more
  levels), `"intercept"` or `"interaction"`

- `var_nlevels`: number of original levels for categorical variables

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
[`model_list_contrasts()`](https://larmarange.github.io/broom.helpers/reference/model_list_contrasts.md),
[`model_list_higher_order_variables()`](https://larmarange.github.io/broom.helpers/reference/model_list_higher_order_variables.md),
[`model_list_terms_levels()`](https://larmarange.github.io/broom.helpers/reference/model_list_terms_levels.md)

## Examples

``` r
# \donttest{
  df <- Titanic |>
    dplyr::as_tibble() |>
    dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
  glm(
    Survived ~ Class + Age:Sex,
    data = df, weights = df$n,
    family = binomial
  ) |>
  model_list_variables()
#> # A tibble: 6 × 4
#>   variable  var_class label_attr var_label
#>   <chr>     <chr>     <chr>      <chr>    
#> 1 Survived  factor    NA         Survived 
#> 2 Class     character NA         Class    
#> 3 Age       character NA         Age      
#> 4 Sex       character NA         Sex      
#> 5 (weights) numeric   NA         (weights)
#> 6 Age:Sex   NA        NA         Age:Sex  

lm(
   Sepal.Length ~ poly(Sepal.Width, 2) + Species,
   data = iris,
   contrasts = list(Species = contr.sum)
  ) |>
  model_list_variables()
#> # A tibble: 3 × 4
#>   variable     var_class label_attr var_label   
#>   <chr>        <chr>     <chr>      <chr>       
#> 1 Sepal.Length numeric   NA         Sepal.Length
#> 2 Sepal.Width  nmatrix.2 NA         Sepal.Width 
#> 3 Species      factor    NA         Species     

glm(
  response ~ poly(age, 3) + stage + grade * trt,
  na.omit(gtsummary::trial),
  family = binomial,
) |>
  model_list_variables()
#> # A tibble: 6 × 4
#>   variable  var_class label_attr             var_label             
#>   <chr>     <chr>     <chr>                  <chr>                 
#> 1 response  integer   Tumor Response         Tumor Response        
#> 2 age       nmatrix.3 NA                     age                   
#> 3 stage     factor    T Stage                T Stage               
#> 4 grade     factor    Grade                  Grade                 
#> 5 trt       character Chemotherapy Treatment Chemotherapy Treatment
#> 6 grade:trt NA        NA                     grade:trt             
# }
```
