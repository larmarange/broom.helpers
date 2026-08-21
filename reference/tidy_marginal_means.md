# Marginal Means with deprecated `marginaleffects::marginal_means()`

**\[deprecated\]** This function is deprecated. `marginal_means()` is
not anymore exported by `marginaleffects`. Use instead
[`tidy_marginal_predictions()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_predictions.md)
with the option `newdata = "balanced"`.

## Usage

``` r
tidy_marginal_means(x, conf.int = TRUE, conf.level = 0.95, ...)
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

  Additional parameters.
