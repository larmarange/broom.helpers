# Identify the variable corresponding to each model coefficient

`tidy_identify_variables()` will add to the tidy tibble three additional
columns: `variable`, `var_class`, `var_type` and `var_nlevels`.

## Usage

``` r
tidy_identify_variables(x, model = tidy_get_model(x), quiet = FALSE)
```

## Arguments

- x:

  (`data.frame`)  
  A tidy tibble as produced by `tidy_*()` functions.

- model:

  (a model object, e.g. `glm`)  
  The corresponding model, if not attached to `x`.

- quiet:

  (`logical`)  
  Whether `broom.helpers` should not return a message when requested
  output cannot be generated. Default is `FALSE`.

## Details

It will also identify interaction terms and intercept(s).

`var_type` could be:

- `"continuous"`,

- `"dichotomous"` (categorical variable with 2 levels),

- `"categorical"` (categorical variable with 3 levels or more),

- `"intercept"`

- `"interaction"`

- `"ran_pars` (random-effect parameters for mixed models)

- `"ran_vals"` (random-effect values for mixed models)

- `"unknown"` in the rare cases where `tidy_identify_variables()` will
  fail to identify the list of variables

For dichotomous and categorical variables, `var_nlevels` corresponds to
the number of original levels in the corresponding variables.

For `fixest` models, a new column `instrumental` is added to indicate
instrumental variables.

## See also

[`model_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/model_identify_variables.md)

Other tidy_helpers:
[`tidy_add_coefficients_type()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_coefficients_type.md),
[`tidy_add_contrasts()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_contrasts.md),
[`tidy_add_estimate_to_reference_rows()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_estimate_to_reference_rows.md),
[`tidy_add_header_rows()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_header_rows.md),
[`tidy_add_n()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_n.md),
[`tidy_add_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_pairwise_contrasts.md),
[`tidy_add_reference_rows()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_reference_rows.md),
[`tidy_add_term_labels()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_term_labels.md),
[`tidy_add_variable_labels()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_variable_labels.md),
[`tidy_attach_model()`](https://larmarange.github.io/broom.helpers/reference/tidy_attach_model.md),
[`tidy_disambiguate_terms()`](https://larmarange.github.io/broom.helpers/reference/tidy_disambiguate_terms.md),
[`tidy_group_by()`](https://larmarange.github.io/broom.helpers/reference/tidy_group_by.md),
[`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.md),
[`tidy_remove_intercept()`](https://larmarange.github.io/broom.helpers/reference/tidy_remove_intercept.md),
[`tidy_select_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_select_variables.md)

## Examples

``` r
df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
glm(
  Survived ~ Class + Age * Sex,
  data = df,
  weights = df$n,
  family = binomial
) |>
  tidy_and_attach() |>
  tidy_identify_variables()
#> # A tibble: 7 × 11
#>   term      variable var_class var_type var_nlevels estimate std.error statistic
#>   <chr>     <chr>    <chr>     <chr>          <int>    <dbl>     <dbl>     <dbl>
#> 1 (Interce… (Interc… NA        interce…          NA    2.18      0.176    12.4  
#> 2 Class2nd  Class    character categor…           4   -1.03      0.200    -5.17 
#> 3 Class3rd  Class    character categor…           4   -1.81      0.176   -10.3  
#> 4 ClassCrew Class    character categor…           4   -0.803     0.160    -5.03 
#> 5 AgeChild  Age      character dichoto…           2   -0.110     0.335    -0.328
#> 6 SexMale   Sex      character dichoto…           2   -2.62      0.151   -17.3  
#> 7 AgeChild… Age:Sex  NA        interac…          NA    1.90      0.433     4.39 
#> # ℹ 3 more variables: p.value <dbl>, conf.low <dbl>, conf.high <dbl>

lm(
  Sepal.Length ~ poly(Sepal.Width, 2) + Species,
  data = iris,
  contrasts = list(Species = contr.sum)
) |>
  tidy_and_attach(conf.int = TRUE) |>
  tidy_identify_variables()
#> # A tibble: 5 × 11
#>   term      variable var_class var_type var_nlevels estimate std.error statistic
#>   <chr>     <chr>    <chr>     <chr>          <int>    <dbl>     <dbl>     <dbl>
#> 1 (Interce… (Interc… NA        interce…          NA   5.84      0.0359   163.   
#> 2 poly(Sep… Sepal.W… nmatrix.2 continu…          NA   4.27      0.568      7.52 
#> 3 poly(Sep… Sepal.W… nmatrix.2 continu…          NA  -0.0720    0.447     -0.161
#> 4 Species1  Species  factor    categor…           3  -1.13      0.0647   -17.5  
#> 5 Species2  Species  factor    categor…           3   0.324     0.0593     5.46 
#> # ℹ 3 more variables: p.value <dbl>, conf.low <dbl>, conf.high <dbl>
```
