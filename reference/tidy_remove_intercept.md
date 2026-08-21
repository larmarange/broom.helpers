# Remove intercept(s)

Will remove terms where `var_type == "intercept"`.

## Usage

``` r
tidy_remove_intercept(x, model = tidy_get_model(x))
```

## Arguments

- x:

  (`data.frame`)  
  A tidy tibble as produced by `tidy_*()` functions.

- model:

  (a model object, e.g. `glm`)  
  The corresponding model, if not attached to `x`.

## Details

If the `variable` column is not yet available in `x`,
[`tidy_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_identify_variables.md)
will be automatically applied.

## See also

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
[`tidy_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_identify_variables.md),
[`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.md),
[`tidy_select_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_select_variables.md)

## Examples

``` r
df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived))
glm(Survived ~ Class + Age + Sex, data = df, weights = df$n, family = binomial) |>
  tidy_and_attach() |>
  tidy_remove_intercept()
#> # A tibble: 5 × 11
#>   term      variable var_class var_type var_nlevels estimate std.error statistic
#>   <chr>     <chr>    <chr>     <chr>          <int>    <dbl>     <dbl>     <dbl>
#> 1 Class2nd  Class    character categor…           4   -1.02      0.196     -5.19
#> 2 Class3rd  Class    character categor…           4   -1.78      0.172    -10.4 
#> 3 ClassCrew Class    character categor…           4   -0.858     0.157     -5.45
#> 4 AgeChild  Age      character dichoto…           2    1.06      0.244      4.35
#> 5 SexMale   Sex      character dichoto…           2   -2.42      0.140    -17.2 
#> # ℹ 3 more variables: p.value <dbl>, conf.low <dbl>, conf.high <dbl>
```
