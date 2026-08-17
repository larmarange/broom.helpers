# Add term labels

Will add term labels in a `label` column, based on:

1.  labels provided in `labels` argument if provided;

2.  factor levels for categorical variables coded with treatment, SAS or
    sum contrasts (the label could be customized with
    `categorical_terms_pattern` argument);

3.  variable labels when there is only one term per variable;

4.  term name otherwise.

## Usage

``` r
tidy_add_term_labels(
  x,
  labels = NULL,
  interaction_sep = " * ",
  categorical_terms_pattern = "{level}",
  relabel_poly = FALSE,
  model = tidy_get_model(x),
  quiet = FALSE,
  strict = FALSE
)
```

## Arguments

- x:

  (`data.frame`)  
  A tidy tibble as produced by `tidy_*()` functions.

- labels:

  (`list` or `string`)  
  An optional named list or named vector of custom term labels.

- interaction_sep:

  (`string`)  
  Separator for interaction terms.

- categorical_terms_pattern:

  ([`glue pattern`](https://glue.tidyverse.org/reference/glue.html))  
  A [glue pattern](https://glue.tidyverse.org/reference/glue.html) for
  labels of categorical terms with treatment or sum contrasts (see
  examples and
  [`model_list_terms_levels()`](https://larmarange.github.io/broom.helpers/dev/reference/model_list_terms_levels.md)).

- relabel_poly:

  Should terms generated with
  [`stats::poly()`](https://rdrr.io/r/stats/poly.html) be relabeled?

- model:

  (a model object, e.g. `glm`)  
  The corresponding model, if not attached to `x`.

- quiet:

  (`logical`)  
  Whether `broom.helpers` should not return a message when requested
  output cannot be generated. Default is `FALSE`.

- strict:

  (`logical`)  
  Whether `broom.helpers` should return an error when requested output
  cannot be generated. Default is `FALSE`.

## Details

If the `variable_label` column is not yet available in `x`,
[`tidy_add_variable_labels()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_variable_labels.md)
will be automatically applied. If the `contrasts` column is not yet
available in `x`,
[`tidy_add_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_contrasts.md)
will be automatically applied.

It is possible to pass a custom label for any term in `labels`,
including interaction terms.

## See also

Other tidy_helpers:
[`tidy_add_coefficients_type()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_coefficients_type.md),
[`tidy_add_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_contrasts.md),
[`tidy_add_estimate_to_reference_rows()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_estimate_to_reference_rows.md),
[`tidy_add_header_rows()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_header_rows.md),
[`tidy_add_n()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_n.md),
[`tidy_add_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_pairwise_contrasts.md),
[`tidy_add_reference_rows()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_reference_rows.md),
[`tidy_add_variable_labels()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_variable_labels.md),
[`tidy_attach_model()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_attach_model.md),
[`tidy_disambiguate_terms()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_disambiguate_terms.md),
[`tidy_group_by()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_group_by.md),
[`tidy_identify_variables()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_identify_variables.md),
[`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_plus_plus.md),
[`tidy_remove_intercept()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_remove_intercept.md),
[`tidy_select_variables()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_select_variables.md)

## Examples

``` r
# \donttest{
df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) |>
  labelled::set_variable_labels(
    Class = "Passenger's class",
    Sex = "Sex"
  )

mod <-
  glm(Survived ~ Class * Age * Sex, data = df, weights = df$n, family = binomial)
mod |>
  tidy_and_attach() |>
  tidy_add_term_labels()
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> # A tibble: 16 × 15
#>    term              variable var_label var_class var_type var_nlevels contrasts
#>    <chr>             <chr>    <chr>     <chr>     <chr>          <int> <chr>    
#>  1 (Intercept)       (Interc… (Interce… NA        interce…          NA NA       
#>  2 Class2nd          Class    Passenge… character categor…           4 contr.tr…
#>  3 Class3rd          Class    Passenge… character categor…           4 contr.tr…
#>  4 ClassCrew         Class    Passenge… character categor…           4 contr.tr…
#>  5 AgeChild          Age      Age       character dichoto…           2 contr.tr…
#>  6 SexMale           Sex      Sex       character dichoto…           2 contr.tr…
#>  7 Class2nd:AgeChild Class:A… Passenge… NA        interac…          NA NA       
#>  8 Class3rd:AgeChild Class:A… Passenge… NA        interac…          NA NA       
#>  9 ClassCrew:AgeChi… Class:A… Passenge… NA        interac…          NA NA       
#> 10 Class2nd:SexMale  Class:S… Passenge… NA        interac…          NA NA       
#> 11 Class3rd:SexMale  Class:S… Passenge… NA        interac…          NA NA       
#> 12 ClassCrew:SexMale Class:S… Passenge… NA        interac…          NA NA       
#> 13 AgeChild:SexMale  Age:Sex  Age * Sex NA        interac…          NA NA       
#> 14 Class2nd:AgeChil… Class:A… Passenge… NA        interac…          NA NA       
#> 15 Class3rd:AgeChil… Class:A… Passenge… NA        interac…          NA NA       
#> 16 ClassCrew:AgeChi… Class:A… Passenge… NA        interac…          NA NA       
#> # ℹ 8 more variables: contrasts_type <chr>, label <chr>, estimate <dbl>,
#> #   std.error <dbl>, statistic <dbl>, p.value <dbl>, conf.low <dbl>,
#> #   conf.high <dbl>
mod |>
  tidy_and_attach() |>
  tidy_add_term_labels(
    interaction_sep = " x ",
    categorical_terms_pattern = "{level} / {reference_level}"
  )
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> # A tibble: 16 × 15
#>    term              variable var_label var_class var_type var_nlevels contrasts
#>    <chr>             <chr>    <chr>     <chr>     <chr>          <int> <chr>    
#>  1 (Intercept)       (Interc… (Interce… NA        interce…          NA NA       
#>  2 Class2nd          Class    Passenge… character categor…           4 contr.tr…
#>  3 Class3rd          Class    Passenge… character categor…           4 contr.tr…
#>  4 ClassCrew         Class    Passenge… character categor…           4 contr.tr…
#>  5 AgeChild          Age      Age       character dichoto…           2 contr.tr…
#>  6 SexMale           Sex      Sex       character dichoto…           2 contr.tr…
#>  7 Class2nd:AgeChild Class:A… Passenge… NA        interac…          NA NA       
#>  8 Class3rd:AgeChild Class:A… Passenge… NA        interac…          NA NA       
#>  9 ClassCrew:AgeChi… Class:A… Passenge… NA        interac…          NA NA       
#> 10 Class2nd:SexMale  Class:S… Passenge… NA        interac…          NA NA       
#> 11 Class3rd:SexMale  Class:S… Passenge… NA        interac…          NA NA       
#> 12 ClassCrew:SexMale Class:S… Passenge… NA        interac…          NA NA       
#> 13 AgeChild:SexMale  Age:Sex  Age * Sex NA        interac…          NA NA       
#> 14 Class2nd:AgeChil… Class:A… Passenge… NA        interac…          NA NA       
#> 15 Class3rd:AgeChil… Class:A… Passenge… NA        interac…          NA NA       
#> 16 ClassCrew:AgeChi… Class:A… Passenge… NA        interac…          NA NA       
#> # ℹ 8 more variables: contrasts_type <chr>, label <chr>, estimate <dbl>,
#> #   std.error <dbl>, statistic <dbl>, p.value <dbl>, conf.low <dbl>,
#> #   conf.high <dbl>
# }
```
