# Disambiguate terms

For mixed models, the `term` column returned by `broom.mixed` may have
duplicated values for random-effect parameters and random-effect values.
In such case, the terms could be disambiguated be prefixing them with
the value of the `group` column. `tidy_disambiguate_terms()` will not
change any term if there is no `group` column in `x`. The original term
value is kept in a new column `original_term`.

## Usage

``` r
tidy_disambiguate_terms(x, sep = ".", model = tidy_get_model(x), quiet = FALSE)
```

## Arguments

- x:

  (`data.frame`)  
  A tidy tibble as produced by `tidy_*()` functions.

- sep:

  (`string`)  
  Separator added between group name and term.

- model:

  (a model object, e.g. `glm`)  
  The corresponding model, if not attached to `x`.

- quiet:

  (`logical`)  
  Whether `broom.helpers` should not return a message when requested
  output cannot be generated. Default is `FALSE`.

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
[`tidy_group_by()`](https://larmarange.github.io/broom.helpers/reference/tidy_group_by.md),
[`tidy_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_identify_variables.md),
[`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.md),
[`tidy_remove_intercept()`](https://larmarange.github.io/broom.helpers/reference/tidy_remove_intercept.md),
[`tidy_select_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_select_variables.md)

## Examples

``` r
# \donttest{
if (
  .assert_package("lme4", boolean = TRUE) &&
    .assert_package("broom.mixed", boolean = TRUE) &&
    .assert_package("gtsummary", boolean = TRUE)
) {
  mod <- lme4::lmer(marker ~ stage + (1 | grade) + (death | response), gtsummary::trial)
  mod |>
    tidy_and_attach() |>
    tidy_disambiguate_terms()
}
#> boundary (singular) fit: see help('isSingular')
#> # A tibble: 9 × 9
#>   term          original_term effect group estimate std.error statistic conf.low
#>   <chr>         <chr>         <chr>  <chr>    <dbl>     <dbl>     <dbl>    <dbl>
#> 1 (Intercept)   (Intercept)   fixed  NA      0.772      0.179     4.32    0.422 
#> 2 stageT2       stageT2       fixed  NA      0.350      0.170     2.06    0.0166
#> 3 stageT3       stageT3       fixed  NA      0.272      0.187     1.45   -0.0945
#> 4 stageT4       stageT4       fixed  NA      0.131      0.173     0.757  -0.208 
#> 5 grade.sd__(I… sd__(Interce… ran_p… grade   0.153     NA        NA      NA     
#> 6 response.sd_… sd__(Interce… ran_p… resp…   0.121     NA        NA      NA     
#> 7 response.sd_… sd__death     ran_p… resp…   0.0468    NA        NA      NA     
#> 8 response.cor… cor__(Interc… ran_p… resp…   1         NA        NA      NA     
#> 9 Residual.sd_… sd__Observat… ran_p… Resi…   0.841     NA        NA      NA     
#> # ℹ 1 more variable: conf.high <dbl>
# }
```
