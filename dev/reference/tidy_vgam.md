# Tidy a `vglm` or a `vgam` model

**\[experimental\]** A tidier for models generated with
[`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) or
[`VGAM::vgam()`](https://rdrr.io/pkg/VGAM/man/vgam.html). Term names
will be updated to be consistent with generic models. The original term
names are preserved in an `"original_term"` column. Depending on the
model, additional column `"group"`, `"component"` and/or `"y.level"` may
be added to the results.

## Usage

``` r
tidy_vgam(x, conf.int = TRUE, conf.level = 0.95, ...)
```

## Arguments

- x:

  (`vglm` or `vgam`)  
  A [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) or a
  [`VGAM::vgam()`](https://rdrr.io/pkg/VGAM/man/vgam.html) model.

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

## See also

Other custom_tidiers:
[`tidy_broom()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_broom.md),
[`tidy_multgee()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_multgee.md),
[`tidy_parameters()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_parameters.md),
[`tidy_svy_vglm()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_svy_vglm.md),
[`tidy_with_broom_or_parameters()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_with_broom_or_parameters.md),
[`tidy_zeroinfl()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_zeroinfl.md)

## Examples

``` r
# \donttest{
  library(VGAM)
  mod <- vglm(
    Species ~ Sepal.Length + Sepal.Width,
    family = multinomial(),
    data = iris
  )
#> Warning: 5 diagonal elements of the working weights variable 'wz' have been replaced by 1.819e-12
#> Warning: 15 diagonal elements of the working weights variable 'wz' have been replaced by 1.819e-12
#> Warning: 45 diagonal elements of the working weights variable 'wz' have been replaced by 1.819e-12
#> Warning: 68 diagonal elements of the working weights variable 'wz' have been replaced by 1.819e-12
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: 72 diagonal elements of the working weights variable 'wz' have been replaced by 1.819e-12
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: 75 diagonal elements of the working weights variable 'wz' have been replaced by 1.819e-12
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: 75 diagonal elements of the working weights variable 'wz' have been replaced by 1.819e-12
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: 75 diagonal elements of the working weights variable 'wz' have been replaced by 1.819e-12
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: 75 diagonal elements of the working weights variable 'wz' have been replaced by 1.819e-12
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: 75 diagonal elements of the working weights variable 'wz' have been replaced by 1.819e-12
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: 75 diagonal elements of the working weights variable 'wz' have been replaced by 1.819e-12
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: fitted probabilities numerically 0 or 1 occurred
#> Warning: fitted values close to 0 or 1
#> Warning: some quantities such as z, residuals, SEs may be inaccurate due to convergence at a half-step
  mod |> tidy_vgam(exponentiate = TRUE)
#>    original_term         term group     estimate    std.error conf.level
#> 1  (Intercept):1  (Intercept)     1 1.145503e+40 8.898639e+41       0.95
#> 2  (Intercept):2  (Intercept)     2 4.632101e+05 1.435034e+06       0.95
#> 3 Sepal.Length:1 Sepal.Length     1 4.850629e-15 1.314042e-13       0.95
#> 4 Sepal.Length:2 Sepal.Length     2 1.492052e-01 7.712864e-02       0.95
#> 5  Sepal.Width:1  Sepal.Width     1 8.789873e+11 2.197615e+13       0.95
#> 6  Sepal.Width:2  Sepal.Width     2 6.673158e-01 5.756157e-01       0.95
#>       conf.low     conf.high  statistic df.error      p.value
#> 1 8.607659e-27 1.524429e+106  1.1873761      Inf 2.350793e-01
#> 2 1.068411e+03  2.008250e+08  4.2110557      Inf 2.541801e-05
#> 3 4.232840e-38  5.558586e+08 -1.2166667      Inf 2.237310e-01
#> 4 5.417171e-02  4.109562e-01 -3.6802527      Inf 2.330030e-04
#> 5 4.597519e-10  1.680512e+33  1.1000080      Inf 2.713286e-01
#> 6 1.230549e-01  3.618794e+00 -0.4689306      Inf 6.391192e-01
#>            component    y.level
#> 1 log(mu[,1]/mu[,3])     setosa
#> 2 log(mu[,2]/mu[,3]) versicolor
#> 3 log(mu[,1]/mu[,3])     setosa
#> 4 log(mu[,2]/mu[,3]) versicolor
#> 5 log(mu[,1]/mu[,3])     setosa
#> 6 log(mu[,2]/mu[,3]) versicolor
  mod <- vglm(
    Species ~ Sepal.Length + Sepal.Width,
    family = multinomial(parallel = TRUE),
    data = iris
  )
  mod |> tidy_vgam(exponentiate = TRUE)
#>   original_term         term group     estimate    std.error conf.level
#> 1   (Intercept)  (Intercept)       7.224907e+05 2.252168e+06       0.95
#> 2  Sepal.Length Sepal.Length       7.408586e-02 3.264124e-02       0.95
#> 3   Sepal.Width  Sepal.Width       2.108096e+00 1.351928e+00       0.95
#>       conf.low    conf.high statistic df.error      p.value component  y.level
#> 1 1.604882e+03 3.252532e+08  4.327710      Inf 1.506675e-05  parallel parallel
#> 2 3.123973e-02 1.756966e-01 -5.906966      Inf 3.484646e-09  parallel parallel
#> 3 5.998083e-01 7.409148e+00  1.162922      Inf 2.448611e-01  parallel parallel
# }
```
