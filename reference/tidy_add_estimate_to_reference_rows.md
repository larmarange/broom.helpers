# Add an estimate value to references rows for categorical variables

For categorical variables with a treatment contrast
([`stats::contr.treatment()`](https://rdrr.io/r/stats/contrast.html)) or
a SAS contrast
([`stats::contr.SAS()`](https://rdrr.io/r/stats/contrast.html)), will
add an estimate equal to `0` (or `1` if `exponentiate = TRUE`) to the
reference row.

## Usage

``` r
tidy_add_estimate_to_reference_rows(
  x,
  exponentiate = attr(x, "exponentiate"),
  conf.level = attr(x, "conf.level"),
  model = tidy_get_model(x),
  quiet = FALSE
)
```

## Arguments

- x:

  (`data.frame`)  
  A tidy tibble as produced by `tidy_*()` functions.

- exponentiate:

  (`logical`)  
  Whether or not to exponentiate the coefficient estimates. It should be
  consistent with the original call to
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)

- conf.level:

  (`numeric`)  
  Confidence level, by default use the value indicated previously in
  [`tidy_and_attach()`](https://larmarange.github.io/broom.helpers/reference/tidy_attach_model.md),
  used only for sum contrasts.

- model:

  (a model object, e.g. `glm`)  
  The corresponding model, if not attached to `x`.

- quiet:

  (`logical`)  
  Whether `broom.helpers` should not return a message when requested
  output cannot be generated. Default is `FALSE`.

## Details

For categorical variables with a sum contrast
([`stats::contr.sum()`](https://rdrr.io/r/stats/contrast.html)), the
estimate value of the reference row will be equal to the sum of all
other coefficients multiplied by `-1` (eventually exponentiated if
`exponentiate = TRUE`), and obtained with
[`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html).
The `emmeans` package should therefore be installed. For sum contrasts,
the model coefficient corresponds to the difference of each level with
the grand mean. For sum contrasts, confidence intervals and p-values
will also be computed and added to the reference rows.

For other variables, no change will be made.

If the `reference_row` column is not yet available in `x`,
[`tidy_add_reference_rows()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_reference_rows.md)
will be automatically applied.

## See also

Other tidy_helpers:
[`tidy_add_coefficients_type()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_coefficients_type.md),
[`tidy_add_contrasts()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_contrasts.md),
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
# \donttest{
  df <- Titanic |>
    dplyr::as_tibble() |>
    dplyr::mutate(dplyr::across(where(is.character), factor))

  glm(
    Survived ~ Class + Age + Sex,
    data = df, weights = df$n, family = binomial,
    contrasts = list(Age = contr.sum, Class = "contr.SAS")
  ) |>
    tidy_and_attach(exponentiate = TRUE) |>
    tidy_add_reference_rows() |>
    tidy_add_estimate_to_reference_rows()
#> # A tibble: 9 × 14
#>   term        variable   var_class var_type var_nlevels contrasts contrasts_type
#>   <chr>       <chr>      <chr>     <chr>          <int> <chr>     <chr>         
#> 1 (Intercept) (Intercep… NA        interce…          NA NA        NA            
#> 2 Class1st    Class      factor    categor…           4 contr.SAS treatment     
#> 3 Class2nd    Class      factor    categor…           4 contr.SAS treatment     
#> 4 Class3rd    Class      factor    categor…           4 contr.SAS treatment     
#> 5 ClassCrew   Class      factor    categor…           4 contr.SAS treatment     
#> 6 Age1        Age        factor    dichoto…           2 contr.sum sum           
#> 7 Age2        Age        factor    dichoto…           2 contr.sum sum           
#> 8 SexFemale   Sex        factor    dichoto…           2 contr.tr… treatment     
#> 9 SexMale     Sex        factor    dichoto…           2 contr.tr… treatment     
#> # ℹ 7 more variables: reference_row <lgl>, estimate <dbl>, std.error <dbl>,
#> #   statistic <dbl>, p.value <dbl>, conf.low <dbl>, conf.high <dbl>

  glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(
      stage = contr.treatment(4, base = 3),
      grade = contr.treatment(3, base = 2),
      trt = contr.treatment(2, base = 2)
    )
  ) |>
    tidy_and_attach() |>
    tidy_add_reference_rows() |>
    tidy_add_estimate_to_reference_rows()
#> # A tibble: 12 × 14
#>    term        variable  var_class var_type var_nlevels contrasts contrasts_type
#>    <chr>       <chr>     <chr>     <chr>          <int> <chr>     <chr>         
#>  1 (Intercept) (Interce… NA        interce…          NA NA        NA            
#>  2 stage1      stage     factor    categor…           4 contr.tr… treatment     
#>  3 stage2      stage     factor    categor…           4 contr.tr… treatment     
#>  4 stage3      stage     factor    categor…           4 contr.tr… treatment     
#>  5 stage4      stage     factor    categor…           4 contr.tr… treatment     
#>  6 grade1      grade     factor    categor…           3 contr.tr… treatment     
#>  7 grade2      grade     factor    categor…           3 contr.tr… treatment     
#>  8 grade3      grade     factor    categor…           3 contr.tr… treatment     
#>  9 trt1        trt       character dichoto…           2 contr.SAS treatment     
#> 10 trt2        trt       character dichoto…           2 contr.SAS treatment     
#> 11 grade1:trt1 grade:trt NA        interac…          NA NA        NA            
#> 12 grade3:trt1 grade:trt NA        interac…          NA NA        NA            
#> # ℹ 7 more variables: reference_row <lgl>, estimate <dbl>, std.error <dbl>,
#> #   statistic <dbl>, p.value <dbl>, conf.low <dbl>, conf.high <dbl>
# }
```
