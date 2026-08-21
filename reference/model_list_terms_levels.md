# List levels of categorical terms

Only for categorical variables with treatment, SAS, sum or successive
differences contrasts (cf.
[`MASS::contr.sdif()`](https://rdrr.io/pkg/MASS/man/contr.sdif.html)),
and categorical variables with no contrast.

## Usage

``` r
model_list_terms_levels(
  model,
  label_pattern = "{level}",
  variable_labels = NULL,
  sdif_term_level = c("diff", "ratio")
)

# Default S3 method
model_list_terms_levels(
  model,
  label_pattern = "{level}",
  variable_labels = NULL,
  sdif_term_level = c("diff", "ratio")
)
```

## Arguments

- model:

  (a model object, e.g. `glm`)  
  A model object.

- label_pattern:

  ([`glue pattern`](https://glue.tidyverse.org/reference/glue.html))  
  A [glue pattern](https://glue.tidyverse.org/reference/glue.html) for
  term labels (see examples).

- variable_labels:

  (`list` or `string`)  
  An optional named list or named vector of custom variable labels
  passed to
  [`model_list_variables()`](https://larmarange.github.io/broom.helpers/reference/model_list_variables.md)

- sdif_term_level:

  (`string`)  
  For successive differences contrasts, how should term levels be named?
  `"diff"` for `"B - A"` (default), `"ratio"` for `"B / A"`.

## Value

A tibble with ten columns:

- `variable`: variable

- `contrasts_type`: type of contrasts ("sum" or "treatment")

- `term`: term name

- `level`: term level

- `level_rank`: rank of the level

- `reference`: logical indicating which term is the reference level

- `reference_level`: level of the reference term

- `var_label`: variable label obtained with
  [`model_list_variables()`](https://larmarange.github.io/broom.helpers/reference/model_list_variables.md)

- `var_nlevels`: number of levels in this variable

- `dichotomous`: logical indicating if the variable is dichotomous

- `label`: term label (by default equal to term level) The first nine
  columns can be used in `label_pattern`.

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
[`model_list_variables()`](https://larmarange.github.io/broom.helpers/reference/model_list_variables.md)

## Examples

``` r
glm(
  am ~ mpg + factor(cyl),
  data = mtcars,
  family = binomial,
  contrasts = list(`factor(cyl)` = contr.sum)
) |>
  model_list_terms_levels()
#> # A tibble: 3 × 11
#>   variable    contrasts_type term     level level_rank reference reference_level
#>   <chr>       <chr>          <chr>    <chr>      <int> <lgl>     <chr>          
#> 1 factor(cyl) sum            factor(… 4              1 FALSE     8              
#> 2 factor(cyl) sum            factor(… 6              2 FALSE     8              
#> 3 factor(cyl) sum            factor(… 8              3 TRUE      8              
#> # ℹ 4 more variables: var_label <chr>, var_nlevels <int>, dichotomous <lgl>,
#> #   label <glue>

df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))

mod <- glm(
  Survived ~ Class + Age + Sex,
  data = df, weights = df$n, family = binomial,
  contrasts = list(Age = contr.sum, Class = "contr.helmert")
)
mod |> model_list_terms_levels()
#> # A tibble: 4 × 11
#>   variable contrasts_type term      level  level_rank reference reference_level
#>   <chr>    <chr>          <chr>     <chr>       <int> <lgl>     <chr>          
#> 1 Age      sum            Age1      Adult           1 FALSE     Child          
#> 2 Age      sum            Age2      Child           2 TRUE      Child          
#> 3 Sex      treatment      SexFemale Female          1 TRUE      Female         
#> 4 Sex      treatment      SexMale   Male            2 FALSE     Female         
#> # ℹ 4 more variables: var_label <chr>, var_nlevels <int>, dichotomous <lgl>,
#> #   label <glue>
mod |> model_list_terms_levels("{level} vs {reference_level}")
#> # A tibble: 4 × 11
#>   variable contrasts_type term      level  level_rank reference reference_level
#>   <chr>    <chr>          <chr>     <chr>       <int> <lgl>     <chr>          
#> 1 Age      sum            Age1      Adult           1 FALSE     Child          
#> 2 Age      sum            Age2      Child           2 TRUE      Child          
#> 3 Sex      treatment      SexFemale Female          1 TRUE      Female         
#> 4 Sex      treatment      SexMale   Male            2 FALSE     Female         
#> # ℹ 4 more variables: var_label <chr>, var_nlevels <int>, dichotomous <lgl>,
#> #   label <glue>
mod |> model_list_terms_levels("{variable} [{level} - {reference_level}]")
#> # A tibble: 4 × 11
#>   variable contrasts_type term      level  level_rank reference reference_level
#>   <chr>    <chr>          <chr>     <chr>       <int> <lgl>     <chr>          
#> 1 Age      sum            Age1      Adult           1 FALSE     Child          
#> 2 Age      sum            Age2      Child           2 TRUE      Child          
#> 3 Sex      treatment      SexFemale Female          1 TRUE      Female         
#> 4 Sex      treatment      SexMale   Male            2 FALSE     Female         
#> # ℹ 4 more variables: var_label <chr>, var_nlevels <int>, dichotomous <lgl>,
#> #   label <glue>
mod |> model_list_terms_levels(
  "{ifelse(reference, level, paste(level, '-', reference_level))}"
)
#> # A tibble: 4 × 11
#>   variable contrasts_type term      level  level_rank reference reference_level
#>   <chr>    <chr>          <chr>     <chr>       <int> <lgl>     <chr>          
#> 1 Age      sum            Age1      Adult           1 FALSE     Child          
#> 2 Age      sum            Age2      Child           2 TRUE      Child          
#> 3 Sex      treatment      SexFemale Female          1 TRUE      Female         
#> 4 Sex      treatment      SexMale   Male            2 FALSE     Female         
#> # ℹ 4 more variables: var_label <chr>, var_nlevels <int>, dichotomous <lgl>,
#> #   label <glue>
```
