# Tidy a model with parameters package

Use
[`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
to tidy a model and apply
`parameters::standardize_names(style = "broom")` to the output

## Usage

``` r
tidy_parameters(x, conf.int = TRUE, conf.level = 0.95, ...)
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
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html).

## Note

For [`betareg::betareg()`](https://rdrr.io/pkg/betareg/man/betareg.html)
models, the component column in the results is standardized with
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html), using
`"mean"` and `"precision"` values.

## See also

Other custom_tidiers:
[`tidy_broom()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_broom.md),
[`tidy_coxphms()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_coxphms.md),
[`tidy_multgee()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_multgee.md),
[`tidy_svy_vglm()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_svy_vglm.md),
[`tidy_vgam()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_vgam.md),
[`tidy_with_broom_or_parameters()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_with_broom_or_parameters.md),
[`tidy_zeroinfl()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_zeroinfl.md)

## Examples

``` r
# \donttest{
  lm(Sepal.Length ~ Sepal.Width + Species, data = iris) |>
    tidy_parameters()
#>                term  estimate std.error conf.level  conf.low conf.high
#> 1       (Intercept) 2.2513932 0.3697543       0.95 1.5206309  2.982156
#> 2       Sepal.Width 0.8035609 0.1063390       0.95 0.5933983  1.013723
#> 3 Speciesversicolor 1.4587431 0.1121079       0.95 1.2371791  1.680307
#> 4  Speciesvirginica 1.9468166 0.1000150       0.95 1.7491525  2.144481
#>   statistic df.error      p.value
#> 1  6.088890      146 9.568102e-09
#> 2  7.556598      146 4.187340e-12
#> 3 13.011954      146 3.478232e-26
#> 4 19.465255      146 2.094475e-42
# }
```
