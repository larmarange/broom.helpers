# Tidy a model with broom or parameters

Try to tidy a model with
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html). If it
fails, will try to tidy the model using
[`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
through
[`tidy_parameters()`](https://larmarange.github.io/broom.helpers/reference/tidy_parameters.md).

## Usage

``` r
tidy_with_broom_or_parameters(x, conf.int = TRUE, conf.level = 0.95, ...)
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
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) or
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html).

## Note

For [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) models,
if the result contains several *tau* values, a `"component"` column is
added and populated with the value of the `"tau"` column.

## See also

Other custom_tidiers:
[`tidy_broom()`](https://larmarange.github.io/broom.helpers/reference/tidy_broom.md),
[`tidy_coxphms()`](https://larmarange.github.io/broom.helpers/reference/tidy_coxphms.md),
[`tidy_multgee()`](https://larmarange.github.io/broom.helpers/reference/tidy_multgee.md),
[`tidy_parameters()`](https://larmarange.github.io/broom.helpers/reference/tidy_parameters.md),
[`tidy_svy_vglm()`](https://larmarange.github.io/broom.helpers/reference/tidy_svy_vglm.md),
[`tidy_vgam()`](https://larmarange.github.io/broom.helpers/reference/tidy_vgam.md),
[`tidy_zeroinfl()`](https://larmarange.github.io/broom.helpers/reference/tidy_zeroinfl.md)
