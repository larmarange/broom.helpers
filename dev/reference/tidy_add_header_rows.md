# Add header rows variables with several terms

For variables with several terms (usually categorical variables but
could also be the case of continuous variables with polynomial terms or
splines), `tidy_add_header_rows()` will add an additional row per
variable, where `label` will be equal to `var_label`. These additional
rows could be identified with `header_row` column.

## Usage

``` r
tidy_add_header_rows(
  x,
  show_single_row = NULL,
  model = tidy_get_model(x),
  quiet = FALSE,
  strict = FALSE
)
```

## Arguments

- x:

  (`data.frame`)  
  A tidy tibble as produced by `tidy_*()` functions.

- show_single_row:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Names of dichotomous variables that should be displayed on a single
  row. See also
  [`all_dichotomous()`](https://larmarange.github.io/broom.helpers/dev/reference/select_helpers.md).

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

The `show_single_row` argument allows to specify a list of dichotomous
variables that should be displayed on a single row instead of two rows.

The added `header_row` column will be equal to:

- `TRUE` for an header row;

- `FALSE` for a normal row of a variable with an header row;

- `NA` for variables without an header row.

If the `label` column is not yet available in `x`,
[`tidy_add_term_labels()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_term_labels.md)
will be automatically applied.

## See also

Other tidy_helpers:
[`tidy_add_coefficients_type()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_coefficients_type.md),
[`tidy_add_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_contrasts.md),
[`tidy_add_estimate_to_reference_rows()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_estimate_to_reference_rows.md),
[`tidy_add_n()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_n.md),
[`tidy_add_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_pairwise_contrasts.md),
[`tidy_add_reference_rows()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_reference_rows.md),
[`tidy_add_term_labels()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_term_labels.md),
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
    dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))

  res <-
    glm(
      Survived ~ Class + Age + Sex,
      data = df, weights = df$n, family = binomial,
      contrasts = list(Age = contr.sum, Class = "contr.SAS")
    ) |>
    tidy_and_attach() |>
    tidy_add_variable_labels(labels = list(Class = "Custom label for Class")) |>
    tidy_add_reference_rows()
  res |> tidy_add_header_rows()
#> # A tibble: 12 × 17
#>    term   variable var_label var_class var_type var_nlevels header_row contrasts
#>    <chr>  <chr>    <chr>     <chr>     <chr>          <int> <lgl>      <chr>    
#>  1 (Inte… (Interc… (Interce… NA        interce…          NA NA         NA       
#>  2 NA     Class    Custom l… character categor…           4 TRUE       contr.SAS
#>  3 Class… Class    Custom l… character categor…           4 FALSE      contr.SAS
#>  4 Class… Class    Custom l… character categor…           4 FALSE      contr.SAS
#>  5 Class… Class    Custom l… character categor…           4 FALSE      contr.SAS
#>  6 Class… Class    Custom l… character categor…           4 FALSE      contr.SAS
#>  7 NA     Age      Age       character dichoto…           2 TRUE       contr.sum
#>  8 Age1   Age      Age       character dichoto…           2 FALSE      contr.sum
#>  9 Age2   Age      Age       character dichoto…           2 FALSE      contr.sum
#> 10 NA     Sex      Sex       character dichoto…           2 TRUE       contr.tr…
#> 11 SexFe… Sex      Sex       character dichoto…           2 FALSE      contr.tr…
#> 12 SexMa… Sex      Sex       character dichoto…           2 FALSE      contr.tr…
#> # ℹ 9 more variables: contrasts_type <chr>, reference_row <lgl>, label <chr>,
#> #   estimate <dbl>, std.error <dbl>, statistic <dbl>, p.value <dbl>,
#> #   conf.low <dbl>, conf.high <dbl>
  res |> tidy_add_header_rows(show_single_row = all_dichotomous())
#> # A tibble: 8 × 17
#>   term    variable var_label var_class var_type var_nlevels header_row contrasts
#>   <chr>   <chr>    <chr>     <chr>     <chr>          <int> <lgl>      <chr>    
#> 1 (Inter… (Interc… (Interce… NA        interce…          NA NA         NA       
#> 2 NA      Class    Custom l… character categor…           4 TRUE       contr.SAS
#> 3 Class1… Class    Custom l… character categor…           4 FALSE      contr.SAS
#> 4 Class2… Class    Custom l… character categor…           4 FALSE      contr.SAS
#> 5 Class3… Class    Custom l… character categor…           4 FALSE      contr.SAS
#> 6 ClassC… Class    Custom l… character categor…           4 FALSE      contr.SAS
#> 7 Age1    Age      Age       character dichoto…           2 NA         contr.sum
#> 8 SexMale Sex      Sex       character dichoto…           2 NA         contr.tr…
#> # ℹ 9 more variables: contrasts_type <chr>, reference_row <lgl>, label <chr>,
#> #   estimate <dbl>, std.error <dbl>, statistic <dbl>, p.value <dbl>,
#> #   conf.low <dbl>, conf.high <dbl>

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
    tidy_add_header_rows()
#> # A tibble: 16 × 17
#>    term   variable var_label var_class var_type var_nlevels header_row contrasts
#>    <chr>  <chr>    <chr>     <chr>     <chr>          <int> <lgl>      <chr>    
#>  1 (Inte… (Interc… (Interce… NA        interce…          NA NA         NA       
#>  2 NA     stage    T Stage   factor    categor…           4 TRUE       contr.tr…
#>  3 stage1 stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#>  4 stage2 stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#>  5 stage3 stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#>  6 stage4 stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#>  7 NA     grade    Grade     factor    categor…           3 TRUE       contr.tr…
#>  8 grade1 grade    Grade     factor    categor…           3 FALSE      contr.tr…
#>  9 grade2 grade    Grade     factor    categor…           3 FALSE      contr.tr…
#> 10 grade3 grade    Grade     factor    categor…           3 FALSE      contr.tr…
#> 11 NA     trt      Chemothe… character dichoto…           2 TRUE       contr.SAS
#> 12 trt1   trt      Chemothe… character dichoto…           2 FALSE      contr.SAS
#> 13 trt2   trt      Chemothe… character dichoto…           2 FALSE      contr.SAS
#> 14 NA     grade:t… Grade * … NA        interac…          NA TRUE       NA       
#> 15 grade… grade:t… Grade * … NA        interac…          NA FALSE      NA       
#> 16 grade… grade:t… Grade * … NA        interac…          NA FALSE      NA       
#> # ℹ 9 more variables: contrasts_type <chr>, reference_row <lgl>, label <chr>,
#> #   estimate <dbl>, std.error <dbl>, statistic <dbl>, p.value <dbl>,
#> #   conf.low <dbl>, conf.high <dbl>
# }
```
