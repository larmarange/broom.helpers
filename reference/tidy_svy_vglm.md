# Tidy a `svy_vglm` model

**\[experimental\]** A tidier for models generated with
[`svyVGAM::svy_vglm()`](https://rdrr.io/pkg/svyVGAM/man/svy_vglm.html).
Term names will be updated to be consistent with generic models. The
original term names are preserved in an `"original_term"` column.
Depending on the model, additional column `"group"`, `"component"`
and/or `"y.level"` may be added to the results.

## Usage

``` r
tidy_svy_vglm(x, conf.int = TRUE, conf.level = 0.95, ...)
```

## Arguments

- x:

  (`svy_vglm`)  
  A
  [`svyVGAM::svy_vglm()`](https://rdrr.io/pkg/svyVGAM/man/svy_vglm.html)
  model.

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
[`tidy_broom()`](https://larmarange.github.io/broom.helpers/reference/tidy_broom.md),
[`tidy_coxphms()`](https://larmarange.github.io/broom.helpers/reference/tidy_coxphms.md),
[`tidy_multgee()`](https://larmarange.github.io/broom.helpers/reference/tidy_multgee.md),
[`tidy_parameters()`](https://larmarange.github.io/broom.helpers/reference/tidy_parameters.md),
[`tidy_vgam()`](https://larmarange.github.io/broom.helpers/reference/tidy_vgam.md),
[`tidy_with_broom_or_parameters()`](https://larmarange.github.io/broom.helpers/reference/tidy_with_broom_or_parameters.md),
[`tidy_zeroinfl()`](https://larmarange.github.io/broom.helpers/reference/tidy_zeroinfl.md)

## Examples

``` r
# \donttest{
  library(svyVGAM)
#> Loading required package: VGAM
#> Loading required package: stats4
#> Loading required package: splines
#> Loading required package: survey
#> Loading required package: grid
#> Loading required package: Matrix
#> 
#> Attaching package: ‘survey’
#> The following object is masked from ‘package:VGAM’:
#> 
#>     calibrate
#> The following object is masked from ‘package:graphics’:
#> 
#>     dotchart

  mod <- svy_vglm(
    Species ~ Sepal.Length + Sepal.Width,
    family = multinomial(),
    design = survey::svydesign(~1, data = iris)
  )
#> Warning: No weights or probabilities supplied, assuming equal probability
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
  mod |> tidy_svy_vglm(exponentiate = TRUE)
#>    original_term         term group     estimate    std.error conf.level
#> 1  (Intercept):1  (Intercept)     1 1.145503e+40 1.261243e+41       0.95
#> 2  (Intercept):2  (Intercept)     2 4.632101e+05 1.442845e+06       0.95
#> 3 Sepal.Length:1 Sepal.Length     1 4.850629e-15 1.760312e-14       0.95
#> 4 Sepal.Length:2 Sepal.Length     2 1.492052e-01 7.378632e-02       0.95
#> 5  Sepal.Width:1  Sepal.Width     1 8.789873e+11 2.663515e+12       0.95
#> 6  Sepal.Width:2  Sepal.Width     2 6.673158e-01 5.168039e-01       0.95
#>       conf.low    conf.high  statistic df.error      p.value          component
#> 1 4.863346e+30 2.698094e+49  8.3774732      Inf 5.407607e-17 log(mu[,1]/mu[,3])
#> 2 1.033679e+03 2.075728e+08  4.1882600      Inf 2.811014e-05 log(mu[,2]/mu[,3])
#> 3 3.951429e-18 5.954454e-12 -9.0822020      Inf 1.064001e-19 log(mu[,1]/mu[,3])
#> 4 5.660309e-02 3.933036e-01 -3.8469580      Inf 1.195935e-04 log(mu[,2]/mu[,3])
#> 5 2.315605e+09 3.336574e+14  9.0759552      Inf 1.126847e-19 log(mu[,1]/mu[,3])
#> 6 1.462571e-01 3.044710e+00 -0.5222945      Inf 6.014653e-01 log(mu[,2]/mu[,3])
#>      y.level
#> 1     setosa
#> 2 versicolor
#> 3     setosa
#> 4 versicolor
#> 5     setosa
#> 6 versicolor
# }
```
