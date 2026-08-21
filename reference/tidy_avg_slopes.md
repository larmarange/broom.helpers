# Marginal Slopes / Effects with `marginaleffects::avg_slopes()`

Use
[`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)
to estimate marginal slopes / effects and return a tibble tidied in a
way that it could be used by `broom.helpers` functions. See
[`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)
for a list of supported models.

## Usage

``` r
tidy_avg_slopes(x, conf.int = TRUE, conf.level = 0.95, ...)
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
  [`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html).

## Details

By default,
[`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)
estimate average marginal effects (AME): an effect is computed for each
observed value in the original dataset before being averaged. Marginal
Effects at the Mean (MEM) could be computed by specifying
`newdata = "mean"`. Other types of marginal effects could be computed.
Please refer to the documentation page of
[`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html).

For more information, see
`vignette("marginal_tidiers", "broom.helpers")`.

## See also

[`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)

Other marginal_tieders:
[`tidy_all_effects()`](https://larmarange.github.io/broom.helpers/reference/tidy_all_effects.md),
[`tidy_avg_comparisons()`](https://larmarange.github.io/broom.helpers/reference/tidy_avg_comparisons.md),
[`tidy_ggpredict()`](https://larmarange.github.io/broom.helpers/reference/tidy_ggpredict.md),
[`tidy_marginal_contrasts()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_contrasts.md),
[`tidy_marginal_predictions()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_predictions.md),
[`tidy_margins()`](https://larmarange.github.io/broom.helpers/reference/tidy_margins.md)

## Examples

``` r
# \donttest{
# Average Marginal Effects (AME)

df <- Titanic |>
  dplyr::as_tibble() |>
  tidyr::uncount(n) |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
mod <- glm(
  Survived ~ Class + Age + Sex,
  data = df, family = binomial
)
tidy_avg_slopes(mod)
#> # A tibble: 5 × 9
#>   variable term          estimate std.error statistic  p.value s.value conf.low
#>   <chr>    <chr>            <dbl>     <dbl>     <dbl>    <dbl>   <dbl>    <dbl>
#> 1 Age      Child - Adult    0.197    0.0487      4.05 5.09e- 5    14.3    0.102
#> 2 Class    2nd - 1st       -0.195    0.0365     -5.34 9.24e- 8    23.4   -0.267
#> 3 Class    3rd - 1st       -0.307    0.0307     -9.99 1.71e-23    75.6   -0.367
#> 4 Class    Crew - 1st      -0.168    0.0323     -5.19 2.10e- 7    22.2   -0.231
#> 5 Sex      Male - Female   -0.507    0.0245    -20.7  4.25e-95   313.    -0.555
#> # ℹ 1 more variable: conf.high <dbl>
tidy_plus_plus(mod, tidy_fun = tidy_avg_slopes)
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
tidy_avg_slopes(mod2)
#> # A tibble: 3 × 9
#>   variable    term        estimate std.error statistic  p.value s.value conf.low
#>   <chr>       <chr>          <dbl>     <dbl>     <dbl>    <dbl>   <dbl>    <dbl>
#> 1 Petal.Width dY/dX           1.20     0.197      6.11 9.70e-10    29.9    0.818
#> 2 Species     versicolor…     1.32     0.315      4.19 2.83e- 5    15.1    0.700
#> 3 Species     virginica …     1.91     0.373      5.12 3.02e- 7    21.7    1.18 
#> # ℹ 1 more variable: conf.high <dbl>

# Marginal Effects at the Mean (MEM)
tidy_avg_slopes(mod, newdata = "mean")
#> # A tibble: 5 × 9
#>   variable term          estimate std.error statistic   p.value s.value conf.low
#>   <chr>    <chr>            <dbl>     <dbl>     <dbl>     <dbl>   <dbl>    <dbl>
#> 1 Age      Child - Adult    0.232    0.0608      3.81 1.41e-  4    12.8    0.112
#> 2 Class    2nd - 1st       -0.208    0.0391     -5.33 9.87e-  8    23.3   -0.285
#> 3 Class    3rd - 1st       -0.303    0.0332     -9.13 7.06e- 20    63.6   -0.368
#> 4 Class    Crew - 1st      -0.182    0.0356     -5.10 3.38e-  7    21.5   -0.251
#> 5 Sex      Male - Female   -0.541    0.0251    -21.5  1.77e-102   338.    -0.590
#> # ℹ 1 more variable: conf.high <dbl>
tidy_plus_plus(mod, tidy_fun = tidy_avg_slopes, newdata = "mean")
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
