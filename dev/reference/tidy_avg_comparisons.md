# Marginal Contrasts with `marginaleffects::avg_comparisons()`

Use
[`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html)
to estimate marginal contrasts and return a tibble tidied in a way that
it could be used by `broom.helpers` functions. See
[`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html)
for a list of supported models.

## Usage

``` r
tidy_avg_comparisons(x, conf.int = TRUE, conf.level = 0.95, ...)
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
  [`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html).

## Details

By default,
[`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html)
estimate average marginal contrasts: a contrast is computed for each
observed value in the original dataset (counterfactual approach) before
being averaged. Marginal Contrasts at the Mean could be computed by
specifying `newdata = "mean"`. The `variables` argument can be used to
select the contrasts to be computed. Please refer to the documentation
page of
[`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html).

See also
[`tidy_marginal_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_marginal_contrasts.md)
for taking into account interactions. For more information, see
`vignette("marginal_tidiers", "broom.helpers")`.

## See also

[`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html)

Other marginal_tieders:
[`tidy_all_effects()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_all_effects.md),
[`tidy_avg_slopes()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_avg_slopes.md),
[`tidy_ggpredict()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_ggpredict.md),
[`tidy_marginal_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_marginal_contrasts.md),
[`tidy_marginal_predictions()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_marginal_predictions.md),
[`tidy_margins()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_margins.md)

## Examples

``` r
# \donttest{
# Average Marginal Contrasts

df <- Titanic |>
  dplyr::as_tibble() |>
  tidyr::uncount(n) |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
mod <- glm(
  Survived ~ Class + Age + Sex,
  data = df, family = binomial
)
tidy_avg_comparisons(mod)
#> # A tibble: 5 × 9
#>   variable term          estimate std.error statistic  p.value s.value conf.low
#>   <chr>    <chr>            <dbl>     <dbl>     <dbl>    <dbl>   <dbl>    <dbl>
#> 1 Age      Child - Adult    0.197    0.0487      4.05 5.09e- 5    14.3    0.102
#> 2 Class    2nd - 1st       -0.195    0.0365     -5.34 9.24e- 8    23.4   -0.267
#> 3 Class    3rd - 1st       -0.307    0.0307     -9.99 1.71e-23    75.6   -0.367
#> 4 Class    Crew - 1st      -0.168    0.0323     -5.19 2.10e- 7    22.2   -0.231
#> 5 Sex      Male - Female   -0.507    0.0245    -20.7  4.25e-95   313.    -0.555
#> # ℹ 1 more variable: conf.high <dbl>
tidy_plus_plus(mod, tidy_fun = tidy_avg_comparisons)
#> # A tibble: 5 × 20
#>   term          variable var_label var_class var_type    var_nlevels contrasts  
#>   <chr>         <chr>    <chr>     <chr>     <chr>             <int> <chr>      
#> 1 Child - Adult Age      Age       character dichotomous           2 contr.trea…
#> 2 2nd - 1st     Class    Class     character categorical           4 contr.trea…
#> 3 3rd - 1st     Class    Class     character categorical           4 contr.trea…
#> 4 Crew - 1st    Class    Class     character categorical           4 contr.trea…
#> 5 Male - Female Sex      Sex       character dichotomous           2 contr.trea…
#> # ℹ 13 more variables: contrasts_type <chr>, reference_row <lgl>, label <chr>,
#> #   n_obs <dbl>, n_event <dbl>, estimate <dbl>, std.error <dbl>,
#> #   statistic <dbl>, p.value <dbl>, s.value <dbl>, conf.low <dbl>,
#> #   conf.high <dbl>, label_attr <chr>

mod2 <- lm(Petal.Length ~ poly(Petal.Width, 2) + Species, data = iris)
tidy_avg_comparisons(mod2)
#> # A tibble: 3 × 9
#>   variable    term        estimate std.error statistic  p.value s.value conf.low
#>   <chr>       <chr>          <dbl>     <dbl>     <dbl>    <dbl>   <dbl>    <dbl>
#> 1 Petal.Width +1             0.996     0.152      6.54 6.28e-11    33.9    0.697
#> 2 Species     versicolor…    1.32      0.315      4.19 2.83e- 5    15.1    0.700
#> 3 Species     virginica …    1.91      0.373      5.12 3.02e- 7    21.7    1.18 
#> # ℹ 1 more variable: conf.high <dbl>

# Custumizing the type of contrasts
tidy_avg_comparisons(
  mod2,
  variables = list(Petal.Width = 2, Species = "pairwise")
)
#> # A tibble: 4 × 9
#>   variable term  estimate std.error statistic p.value s.value conf.low conf.high
#>   <chr>    <chr>    <dbl>     <dbl>     <dbl>   <dbl>   <dbl>    <dbl>     <dbl>
#> 1 Petal.W… +2       1.58      0.435      3.62 2.96e-4    11.7    0.722     2.43 
#> 2 Species  vers…    1.32      0.315      4.19 2.83e-5    15.1    0.700     1.93 
#> 3 Species  virg…    1.91      0.373      5.12 3.02e-7    21.7    1.18      2.64 
#> 4 Species  virg…    0.595     0.131      4.56 5.14e-6    17.6    0.339     0.851

# Marginal Contrasts at the Mean
tidy_avg_comparisons(mod, newdata = "mean")
#> # A tibble: 5 × 9
#>   variable term          estimate std.error statistic   p.value s.value conf.low
#>   <chr>    <chr>            <dbl>     <dbl>     <dbl>     <dbl>   <dbl>    <dbl>
#> 1 Age      Child - Adult    0.232    0.0608      3.81 1.41e-  4    12.8    0.112
#> 2 Class    2nd - 1st       -0.208    0.0391     -5.33 9.87e-  8    23.3   -0.285
#> 3 Class    3rd - 1st       -0.303    0.0332     -9.13 7.06e- 20    63.6   -0.368
#> 4 Class    Crew - 1st      -0.182    0.0356     -5.10 3.38e-  7    21.5   -0.251
#> 5 Sex      Male - Female   -0.541    0.0251    -21.5  1.77e-102   338.    -0.590
#> # ℹ 1 more variable: conf.high <dbl>
tidy_plus_plus(mod, tidy_fun = tidy_avg_comparisons, newdata = "mean")
#> # A tibble: 5 × 20
#>   term          variable var_label var_class var_type    var_nlevels contrasts  
#>   <chr>         <chr>    <chr>     <chr>     <chr>             <int> <chr>      
#> 1 Child - Adult Age      Age       character dichotomous           2 contr.trea…
#> 2 2nd - 1st     Class    Class     character categorical           4 contr.trea…
#> 3 3rd - 1st     Class    Class     character categorical           4 contr.trea…
#> 4 Crew - 1st    Class    Class     character categorical           4 contr.trea…
#> 5 Male - Female Sex      Sex       character dichotomous           2 contr.trea…
#> # ℹ 13 more variables: contrasts_type <chr>, reference_row <lgl>, label <chr>,
#> #   n_obs <dbl>, n_event <dbl>, estimate <dbl>, std.error <dbl>,
#> #   statistic <dbl>, p.value <dbl>, s.value <dbl>, conf.low <dbl>,
#> #   conf.high <dbl>, label_attr <chr>
# }
```
