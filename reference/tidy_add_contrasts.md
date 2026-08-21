# Add contrasts type for categorical variables

Add a `contrasts` column corresponding to contrasts used for a
categorical variable and a `contrasts_type` column equal to "treatment",
"sum", "poly", "helmert", "other" or "no.contrast".

## Usage

``` r
tidy_add_contrasts(x, model = tidy_get_model(x), quiet = FALSE)
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
  Whether broom.helpers should not return a message when
  [`tidy_disambiguate_terms()`](https://larmarange.github.io/broom.helpers/reference/tidy_disambiguate_terms.md)
  was already applied

## Details

If the `variable` column is not yet available in `x`,
[`tidy_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_identify_variables.md)
will be automatically applied.

## See also

Other tidy_helpers:
[`tidy_add_coefficients_type()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_coefficients_type.md),
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
[`tidy_remove_intercept()`](https://larmarange.github.io/broom.helpers/reference/tidy_remove_intercept.md),
[`tidy_select_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_select_variables.md)

## Examples

``` r
df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))

glm(
  Survived ~ Class + Age + Sex,
  data = df, weights = df$n, family = binomial,
  contrasts = list(Age = contr.sum, Class = "contr.helmert")
) |>
  tidy_and_attach() |>
  tidy_add_contrasts()
#> # A tibble: 6 × 13
#>   term        variable   var_class var_type var_nlevels contrasts contrasts_type
#>   <chr>       <chr>      <chr>     <chr>          <int> <chr>     <chr>         
#> 1 (Intercept) (Intercep… NA        interce…          NA NA        NA            
#> 2 Class1      Class      character categor…           4 contr.he… helmert       
#> 3 Class2      Class      character categor…           4 contr.he… helmert       
#> 4 Class3      Class      character categor…           4 contr.he… helmert       
#> 5 Age1        Age        character dichoto…           2 contr.sum sum           
#> 6 SexMale     Sex        character dichoto…           2 contr.tr… treatment     
#> # ℹ 6 more variables: estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>, conf.low <dbl>, conf.high <dbl>
```
