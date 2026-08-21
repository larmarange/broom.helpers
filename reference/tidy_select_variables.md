# Select variables to keep/drop

Will remove unselected variables from the results. To remove the
intercept, use
[`tidy_remove_intercept()`](https://larmarange.github.io/broom.helpers/reference/tidy_remove_intercept.md).

## Usage

``` r
tidy_select_variables(x, include = everything(), model = tidy_get_model(x))
```

## Arguments

- x:

  (`data.frame`)  
  A tidy tibble as produced by `tidy_*()` functions.

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).
  See also
  [`all_continuous()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md),
  [`all_categorical()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md),
  [`all_dichotomous()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md)
  and
  [`all_interaction()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md).

- model:

  (a model object, e.g. `glm`)  
  The corresponding model, if not attached to `x`.

## Value

The `x` tibble limited to the included variables (and eventually the
intercept), sorted according to the `include` parameter.

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
[`tidy_remove_intercept()`](https://larmarange.github.io/broom.helpers/reference/tidy_remove_intercept.md)

## Examples

``` r
df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived))
res <-
  glm(Survived ~ Class + Age * Sex, data = df, weights = df$n, family = binomial) |>
  tidy_and_attach() |>
  tidy_identify_variables()

res
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
res |> tidy_select_variables()
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
res |> tidy_select_variables(include = "Class")
#> # A tibble: 4 × 11
#>   term      variable var_class var_type var_nlevels estimate std.error statistic
#>   <chr>     <chr>    <chr>     <chr>          <int>    <dbl>     <dbl>     <dbl>
#> 1 (Interce… (Interc… NA        interce…          NA    2.18      0.176     12.4 
#> 2 Class2nd  Class    character categor…           4   -1.03      0.200     -5.17
#> 3 Class3rd  Class    character categor…           4   -1.81      0.176    -10.3 
#> 4 ClassCrew Class    character categor…           4   -0.803     0.160     -5.03
#> # ℹ 3 more variables: p.value <dbl>, conf.low <dbl>, conf.high <dbl>
res |> tidy_select_variables(include = -c("Age", "Sex"))
#> # A tibble: 5 × 11
#>   term      variable var_class var_type var_nlevels estimate std.error statistic
#>   <chr>     <chr>    <chr>     <chr>          <int>    <dbl>     <dbl>     <dbl>
#> 1 (Interce… (Interc… NA        interce…          NA    2.18      0.176     12.4 
#> 2 Class2nd  Class    character categor…           4   -1.03      0.200     -5.17
#> 3 Class3rd  Class    character categor…           4   -1.81      0.176    -10.3 
#> 4 ClassCrew Class    character categor…           4   -0.803     0.160     -5.03
#> 5 AgeChild… Age:Sex  NA        interac…          NA    1.90      0.433      4.39
#> # ℹ 3 more variables: p.value <dbl>, conf.low <dbl>, conf.high <dbl>
res |> tidy_select_variables(include = starts_with("A"))
#> # A tibble: 3 × 11
#>   term      variable var_class var_type var_nlevels estimate std.error statistic
#>   <chr>     <chr>    <chr>     <chr>          <int>    <dbl>     <dbl>     <dbl>
#> 1 (Interce… (Interc… NA        interce…          NA    2.18      0.176    12.4  
#> 2 AgeChild  Age      character dichoto…           2   -0.110     0.335    -0.328
#> 3 AgeChild… Age:Sex  NA        interac…          NA    1.90      0.433     4.39 
#> # ℹ 3 more variables: p.value <dbl>, conf.low <dbl>, conf.high <dbl>
res |> tidy_select_variables(include = all_categorical())
#> # A tibble: 6 × 11
#>   term      variable var_class var_type var_nlevels estimate std.error statistic
#>   <chr>     <chr>    <chr>     <chr>          <int>    <dbl>     <dbl>     <dbl>
#> 1 (Interce… (Interc… NA        interce…          NA    2.18      0.176    12.4  
#> 2 Class2nd  Class    character categor…           4   -1.03      0.200    -5.17 
#> 3 Class3rd  Class    character categor…           4   -1.81      0.176   -10.3  
#> 4 ClassCrew Class    character categor…           4   -0.803     0.160    -5.03 
#> 5 AgeChild  Age      character dichoto…           2   -0.110     0.335    -0.328
#> 6 SexMale   Sex      character dichoto…           2   -2.62      0.151   -17.3  
#> # ℹ 3 more variables: p.value <dbl>, conf.low <dbl>, conf.high <dbl>
res |> tidy_select_variables(include = all_dichotomous())
#> # A tibble: 3 × 11
#>   term      variable var_class var_type var_nlevels estimate std.error statistic
#>   <chr>     <chr>    <chr>     <chr>          <int>    <dbl>     <dbl>     <dbl>
#> 1 (Interce… (Interc… NA        interce…          NA    2.18      0.176    12.4  
#> 2 AgeChild  Age      character dichoto…           2   -0.110     0.335    -0.328
#> 3 SexMale   Sex      character dichoto…           2   -2.62      0.151   -17.3  
#> # ℹ 3 more variables: p.value <dbl>, conf.low <dbl>, conf.high <dbl>
res |> tidy_select_variables(include = all_interaction())
#> # A tibble: 2 × 11
#>   term      variable var_class var_type var_nlevels estimate std.error statistic
#>   <chr>     <chr>    <chr>     <chr>          <int>    <dbl>     <dbl>     <dbl>
#> 1 (Interce… (Interc… NA        interce…          NA     2.18     0.176     12.4 
#> 2 AgeChild… Age:Sex  NA        interac…          NA     1.90     0.433      4.39
#> # ℹ 3 more variables: p.value <dbl>, conf.low <dbl>, conf.high <dbl>
res |> tidy_select_variables(
  include = c("Age", all_categorical(dichotomous = FALSE), all_interaction())
)
#> # A tibble: 6 × 11
#>   term      variable var_class var_type var_nlevels estimate std.error statistic
#>   <chr>     <chr>    <chr>     <chr>          <int>    <dbl>     <dbl>     <dbl>
#> 1 (Interce… (Interc… NA        interce…          NA    2.18      0.176    12.4  
#> 2 AgeChild  Age      character dichoto…           2   -0.110     0.335    -0.328
#> 3 Class2nd  Class    character categor…           4   -1.03      0.200    -5.17 
#> 4 Class3rd  Class    character categor…           4   -1.81      0.176   -10.3  
#> 5 ClassCrew Class    character categor…           4   -0.803     0.160    -5.03 
#> 6 AgeChild… Age:Sex  NA        interac…          NA    1.90      0.433     4.39 
#> # ℹ 3 more variables: p.value <dbl>, conf.low <dbl>, conf.high <dbl>
```
