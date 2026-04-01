# Marginal Predictions with `ggeffects::ggpredict()`

Use
[`ggeffects::ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.html)
to estimate marginal predictions and return a tibble tidied in a way
that it could be used by `broom.helpers` functions. See
<https://strengejacke.github.io/ggeffects/> for a list of supported
models.

## Usage

``` r
tidy_ggpredict(x, conf.int = TRUE, conf.level = 0.95, ...)
```

## Arguments

- x:

  (a model object, e.g. `glm`)  
  A model to be tidied.

- conf.int:

  (`logical`)  
  Whether or not to include a confidence interval in the tidied output.

- conf.level:

  (`numeric`)  
  The confidence level to use for the confidence interval (between `0`
  ans `1`).

- ...:

  Additional parameters passed to
  [`ggeffects::ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.html).

## Details

By default,
[`ggeffects::ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.html)
estimate marginal predictions at the observed mean of continuous
variables and at the first modality of categorical variables (regardless
of the type of contrasts used in the model).

For more information, see
`vignette("marginal_tidiers", "broom.helpers")`.

## Note

By default,
[`ggeffects::ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.html)
estimates marginal predictions for each individual variable, regardless
of eventual interactions.

## See also

[`ggeffects::ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.html)

Other marginal_tieders:
[`tidy_all_effects()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_all_effects.md),
[`tidy_avg_comparisons()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_avg_comparisons.md),
[`tidy_avg_slopes()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_avg_slopes.md),
[`tidy_marginal_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_marginal_contrasts.md),
[`tidy_marginal_predictions()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_marginal_predictions.md),
[`tidy_margins()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_margins.md)

## Examples

``` r
# \donttest{
df <- Titanic |>
  dplyr::as_tibble() |>
  tidyr::uncount(n) |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
mod <- glm(
  Survived ~ Class + Age + Sex,
  data = df, family = binomial
)
tidy_ggpredict(mod)
#> Some of the focal terms are of type `character`. This may lead to
#>   unexpected results. It is recommended to convert these variables to
#>   factors before fitting the model.
#>   The following variables are of type character: `Class`
#> Some of the focal terms are of type `character`. This may lead to
#>   unexpected results. It is recommended to convert these variables to
#>   factors before fitting the model.
#>   The following variables are of type character: `Age`
#> Some of the focal terms are of type `character`. This may lead to
#>   unexpected results. It is recommended to convert these variables to
#>   factors before fitting the model.
#>   The following variables are of type character: `Sex`
#> # A tibble: 8 × 6
#>   variable term   estimate std.error conf.low conf.high
#>   <chr>    <chr>     <dbl>     <dbl>    <dbl>     <dbl>
#> 1 Class    3rd       0.104    0.127    0.0830     0.130
#> 2 Class    1st       0.407    0.136    0.345      0.473
#> 3 Class    2nd       0.199    0.155    0.155      0.252
#> 4 Class    Crew      0.225    0.0805   0.199      0.254
#> 5 Age      Child     0.457    0.257    0.337      0.582
#> 6 Age      Adult     0.225    0.0805   0.199      0.254
#> 7 Sex      Male      0.225    0.0805   0.199      0.254
#> 8 Sex      Female    0.766    0.159    0.706      0.817
tidy_plus_plus(mod, tidy_fun = tidy_ggpredict)
#> Some of the focal terms are of type `character`. This may lead to
#>   unexpected results. It is recommended to convert these variables to
#>   factors before fitting the model.
#>   The following variables are of type character: `Class`
#> Some of the focal terms are of type `character`. This may lead to
#>   unexpected results. It is recommended to convert these variables to
#>   factors before fitting the model.
#>   The following variables are of type character: `Age`
#> Some of the focal terms are of type `character`. This may lead to
#>   unexpected results. It is recommended to convert these variables to
#>   factors before fitting the model.
#>   The following variables are of type character: `Sex`
#> # A tibble: 8 × 17
#>   term   variable var_label var_class var_type    var_nlevels contrasts      
#>   <chr>  <chr>    <chr>     <chr>     <chr>             <int> <chr>          
#> 1 3rd    Class    Class     character categorical           4 contr.treatment
#> 2 1st    Class    Class     character categorical           4 contr.treatment
#> 3 2nd    Class    Class     character categorical           4 contr.treatment
#> 4 Crew   Class    Class     character categorical           4 contr.treatment
#> 5 Child  Age      Age       character dichotomous           2 contr.treatment
#> 6 Adult  Age      Age       character dichotomous           2 contr.treatment
#> 7 Male   Sex      Sex       character dichotomous           2 contr.treatment
#> 8 Female Sex      Sex       character dichotomous           2 contr.treatment
#> # ℹ 10 more variables: contrasts_type <chr>, reference_row <lgl>, label <chr>,
#> #   n_obs <dbl>, n_event <dbl>, estimate <dbl>, std.error <dbl>,
#> #   conf.low <dbl>, conf.high <dbl>, label_attr <chr>
# }
```
