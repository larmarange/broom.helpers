# Marginal Predictions at the mean with `effects::allEffects()`

Use
[`effects::allEffects()`](https://rdrr.io/pkg/effects/man/effect.html)
to estimate marginal predictions and return a tibble tidied in a way
that it could be used by `broom.helpers` functions. See
[`vignette("functions-supported-by-effects", package = "effects")`](https://cran.rstudio.com/web/packages/effects/vignettes/functions-supported-by-effects.pdf)
for a list of supported models.

## Usage

``` r
tidy_all_effects(x, conf.int = TRUE, conf.level = 0.95, ...)
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
  [`effects::allEffects()`](https://rdrr.io/pkg/effects/man/effect.html).

## Details

By default,
[`effects::allEffects()`](https://rdrr.io/pkg/effects/man/effect.html)
estimate marginal predictions at the mean at the observed means for
continuous variables and weighting modalities of categorical variables
according to their observed distribution in the original dataset.
Marginal predictions are therefore computed at a sort of averaged
situation / typical values for the other variables fixed in the model.

For more information, see
`vignette("marginal_tidiers", "broom.helpers")`.

## Note

If the model contains interactions,
[`effects::allEffects()`](https://rdrr.io/pkg/effects/man/effect.html)
will return marginal predictions for the different levels of the
interactions.

## See also

[`effects::allEffects()`](https://rdrr.io/pkg/effects/man/effect.html)

Other marginal_tieders:
[`tidy_avg_comparisons()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_avg_comparisons.md),
[`tidy_avg_slopes()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_avg_slopes.md),
[`tidy_ggpredict()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_ggpredict.md),
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
tidy_all_effects(mod)
#>   variable   term  estimate  std.error  conf.low conf.high
#> 1    Class    1st 0.5481319 0.03259139 0.4838117 0.6108835
#> 2    Class    2nd 0.3047099 0.03088545 0.2477413 0.3683636
#> 3    Class    3rd 0.1701399 0.01556823 0.1417601 0.2028582
#> 4    Class   Crew 0.3397182 0.01922242 0.3031143 0.3783425
#> 5      Age  Adult 0.2877415 0.01134968 0.2660183 0.3104882
#> 6      Age  Child 0.5387134 0.05844277 0.4241446 0.6493334
#> 7      Sex Female 0.7406740 0.02264426 0.6938988 0.7825415
#> 8      Sex   Male 0.2025252 0.01029278 0.1830988 0.2234490
tidy_plus_plus(mod, tidy_fun = tidy_all_effects)
#> # A tibble: 8 × 17
#>   term   variable var_label var_class var_type    var_nlevels contrasts      
#>   <chr>  <chr>    <chr>     <chr>     <chr>             <int> <chr>          
#> 1 1st    Class    Class     character categorical           4 contr.treatment
#> 2 2nd    Class    Class     character categorical           4 contr.treatment
#> 3 3rd    Class    Class     character categorical           4 contr.treatment
#> 4 Crew   Class    Class     character categorical           4 contr.treatment
#> 5 Adult  Age      Age       character dichotomous           2 contr.treatment
#> 6 Child  Age      Age       character dichotomous           2 contr.treatment
#> 7 Female Sex      Sex       character dichotomous           2 contr.treatment
#> 8 Male   Sex      Sex       character dichotomous           2 contr.treatment
#> # ℹ 10 more variables: contrasts_type <chr>, reference_row <lgl>, label <chr>,
#> #   n_obs <dbl>, n_event <dbl>, estimate <dbl>, std.error <dbl>,
#> #   conf.low <dbl>, conf.high <dbl>, label_attr <chr>
# }
```
