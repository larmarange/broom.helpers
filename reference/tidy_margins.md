# Average Marginal Effects with `margins::margins()`

**\[superseded\]**

## Usage

``` r
tidy_margins(x, conf.int = TRUE, conf.level = 0.95, ...)
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
  [`margins::margins()`](https://rdrr.io/pkg/margins/man/margins.html).

## Details

The `margins` package is no longer under active development and may be
removed from CRAN sooner or later. It is advised to use the
`marginaleffects` package instead, offering more functionalities. You
could have a look at the
[article](https://larmarange.github.io/broom.helpers/articles/marginal_tidiers.html)
dedicated to marginal estimates with `broom.helpers`.
[`tidy_avg_slopes()`](https://larmarange.github.io/broom.helpers/reference/tidy_avg_slopes.md)
could be used as an alternative.

Use [`margins::margins()`](https://rdrr.io/pkg/margins/man/margins.html)
to estimate average marginal effects (AME) and return a tibble tidied in
a way that it could be used by `broom.helpers` functions. See
[`margins::margins()`](https://rdrr.io/pkg/margins/man/margins.html) for
a list of supported models.

By default,
[`margins::margins()`](https://rdrr.io/pkg/margins/man/margins.html)
estimate average marginal effects (AME): an effect is computed for each
observed value in the original dataset before being averaged.

For more information, see
`vignette("marginal_tidiers", "broom.helpers")`.

## Note

When applying
[`margins::margins()`](https://rdrr.io/pkg/margins/man/margins.html),
custom contrasts are ignored. Treatment contrasts
([`stats::contr.treatment()`](https://rdrr.io/r/stats/contrast.html))
are applied to all categorical variables. Interactions are also ignored.

## See also

[`margins::margins()`](https://rdrr.io/pkg/margins/man/margins.html)

Other marginal_tieders:
[`tidy_all_effects()`](https://larmarange.github.io/broom.helpers/reference/tidy_all_effects.md),
[`tidy_avg_comparisons()`](https://larmarange.github.io/broom.helpers/reference/tidy_avg_comparisons.md),
[`tidy_avg_slopes()`](https://larmarange.github.io/broom.helpers/reference/tidy_avg_slopes.md),
[`tidy_ggpredict()`](https://larmarange.github.io/broom.helpers/reference/tidy_ggpredict.md),
[`tidy_marginal_contrasts()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_contrasts.md),
[`tidy_marginal_predictions()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_predictions.md)

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
tidy_margins(mod)
#> Error in data[[variable]]: object of type 'closure' is not subsettable
tidy_plus_plus(mod, tidy_fun = tidy_margins)
#> ✖ There was an error calling `tidy_fun()`. Most likely, this is because the
#> function supplied in `tidy_fun=` was misspelled, does not exist, is not
#> compatible with your object, or was missing necessary arguments (e.g. `conf.level=` or `conf.int=`). See error message below.
#> Error: Error in data[[variable]]: object of type 'closure' is not subsettable
# }
```
