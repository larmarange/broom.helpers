# Add pairwise contrasts for categorical variables

Computes pairwise contrasts with
[`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
and add them to the results tibble. Works only with models supported by
`emmeans`, see
[`vignette("models", package = "emmeans")`](https://rvlenth.github.io/emmeans/articles/models.html).

## Usage

``` r
tidy_add_pairwise_contrasts(
  x,
  variables = all_categorical(),
  keep_model_terms = FALSE,
  pairwise_reverse = TRUE,
  contrasts_adjust = NULL,
  conf.level = attr(x, "conf.level"),
  emmeans_args = list(),
  model = tidy_get_model(x),
  quiet = FALSE
)
```

## Arguments

- x:

  (`data.frame`)  
  A tidy tibble as produced by `tidy_*()` functions.

- variables:

  include
  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables for those pairwise contrasts should be added. Default is
  [`all_categorical()`](https://larmarange.github.io/broom.helpers/dev/reference/select_helpers.md).

- keep_model_terms:

  (`logical`)  
  Keep terms from the model?

- pairwise_reverse:

  (`logical`)  
  Determines whether to use `"pairwise"` (if `TRUE`) or `"revpairwise"`
  (if `FALSE`), see
  [`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html).

- contrasts_adjust:

  (`string`)  
  Optional adjustment method when computing contrasts, see
  [`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html)
  (if `NULL`, use `emmeans` default).

- conf.level:

  (`numeric`)  
  Confidence level, by default use the value indicated previously in
  [`tidy_and_attach()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_attach_model.md).

- emmeans_args:

  (`list`)  
  List of additional parameter to pass to
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
  when computing pairwise contrasts.

- model:

  (a model object, e.g. `glm`)  
  The corresponding model, if not attached to `x`.

- quiet:

  (`logical`)  
  Whether `broom.helpers` should not return a message when requested
  output cannot be generated. Default is `FALSE`.

## Note

If the `contrasts` column is not yet available in `x`,
[`tidy_add_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_contrasts.md)
will be automatically applied.

For multi-components models, such as zero-inflated Poisson or beta
regression, support of pairwise contrasts is still experimental.

## See also

Other tidy_helpers:
[`tidy_add_coefficients_type()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_coefficients_type.md),
[`tidy_add_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_contrasts.md),
[`tidy_add_estimate_to_reference_rows()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_estimate_to_reference_rows.md),
[`tidy_add_header_rows()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_header_rows.md),
[`tidy_add_n()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_n.md),
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
  mod1 <- lm(Sepal.Length ~ Species, data = iris)
  mod1 |>
    tidy_and_attach() |>
    tidy_add_pairwise_contrasts()
#> # A tibble: 4 × 13
#>   term          variable var_class var_type var_nlevels contrasts contrasts_type
#>   <chr>         <chr>    <chr>     <chr>          <int> <chr>     <chr>         
#> 1 (Intercept)   (Interc… NA        interce…          NA NA        NA            
#> 2 versicolor -… Species  factor    categor…           3 pairwise  pairwise      
#> 3 virginica - … Species  factor    categor…           3 pairwise  pairwise      
#> 4 virginica - … Species  factor    categor…           3 pairwise  pairwise      
#> # ℹ 6 more variables: estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>, conf.low <dbl>, conf.high <dbl>

  mod1 |>
    tidy_and_attach() |>
    tidy_add_pairwise_contrasts(pairwise_reverse = FALSE)
#> # A tibble: 4 × 13
#>   term          variable var_class var_type var_nlevels contrasts contrasts_type
#>   <chr>         <chr>    <chr>     <chr>          <int> <chr>     <chr>         
#> 1 (Intercept)   (Interc… NA        interce…          NA NA        NA            
#> 2 setosa - ver… Species  factor    categor…           3 revpairw… pairwise      
#> 3 setosa - vir… Species  factor    categor…           3 revpairw… pairwise      
#> 4 versicolor -… Species  factor    categor…           3 revpairw… pairwise      
#> # ℹ 6 more variables: estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>, conf.low <dbl>, conf.high <dbl>

  mod1 |>
    tidy_and_attach() |>
    tidy_add_pairwise_contrasts(keep_model_terms = TRUE)
#> # A tibble: 6 × 13
#>   term          variable var_class var_type var_nlevels contrasts contrasts_type
#>   <chr>         <chr>    <chr>     <chr>          <int> <chr>     <chr>         
#> 1 (Intercept)   (Interc… NA        interce…          NA NA        NA            
#> 2 Speciesversi… Species  factor    categor…           3 contr.tr… treatment     
#> 3 Speciesvirgi… Species  factor    categor…           3 contr.tr… treatment     
#> 4 versicolor -… Species  factor    categor…           3 pairwise  pairwise      
#> 5 virginica - … Species  factor    categor…           3 pairwise  pairwise      
#> 6 virginica - … Species  factor    categor…           3 pairwise  pairwise      
#> # ℹ 6 more variables: estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>, conf.low <dbl>, conf.high <dbl>

  mod1 |>
    tidy_and_attach() |>
    tidy_add_pairwise_contrasts(contrasts_adjust = "none")
#> # A tibble: 4 × 13
#>   term          variable var_class var_type var_nlevels contrasts contrasts_type
#>   <chr>         <chr>    <chr>     <chr>          <int> <chr>     <chr>         
#> 1 (Intercept)   (Interc… NA        interce…          NA NA        NA            
#> 2 versicolor -… Species  factor    categor…           3 pairwise  pairwise      
#> 3 virginica - … Species  factor    categor…           3 pairwise  pairwise      
#> 4 virginica - … Species  factor    categor…           3 pairwise  pairwise      
#> # ℹ 6 more variables: estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>, conf.low <dbl>, conf.high <dbl>

  if (.assert_package("gtsummary", boolean = TRUE)) {
    mod2 <- glm(
      response ~ age + trt + grade,
      data = gtsummary::trial,
      family = binomial
    )
    mod2 |>
      tidy_and_attach(exponentiate = TRUE) |>
      tidy_add_pairwise_contrasts()
  }
#> # A tibble: 6 × 13
#>   term          variable var_class var_type var_nlevels contrasts contrasts_type
#>   <chr>         <chr>    <chr>     <chr>          <int> <chr>     <chr>         
#> 1 (Intercept)   (Interc… NA        interce…          NA NA        NA            
#> 2 age           age      numeric   continu…          NA NA        NA            
#> 3 Drug B / Dru… trt      character dichoto…           2 pairwise  pairwise      
#> 4 II / I        grade    factor    categor…           3 pairwise  pairwise      
#> 5 III / I       grade    factor    categor…           3 pairwise  pairwise      
#> 6 III / II      grade    factor    categor…           3 pairwise  pairwise      
#> # ℹ 6 more variables: estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>, conf.low <dbl>, conf.high <dbl>
# }
```
