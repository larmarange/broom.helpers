# Tidy a `zeroinfl` or a `hurdle` model

A tidier for models generated with
[`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) or
[`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html). Term names
will be updated to be consistent with generic models. The original term
names are preserved in an `"original_term"` column.

## Usage

``` r
tidy_zeroinfl(x, conf.int = TRUE, conf.level = 0.95, component = NULL, ...)
```

## Arguments

- x:

  (`zeroinfl` or `hurdle`)  
  A [`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) or
  a [`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html) model.

- conf.int:

  (`logical`)  
  Whether or not to include a confidence interval in the tidied output.

- conf.level:

  (`numeric`)  
  The confidence level to use for the confidence interval (between `0`
  ans `1`).

- component:

  (`string`)  
  `NULL` or one of `"all"`, `"conditional"`, `"zi"`, or
  `"zero_inflated"`.

- ...:

  Additional parameters passed to
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html).

## See also

Other custom_tidiers:
[`tidy_broom()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_broom.md),
[`tidy_coxphms()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_coxphms.md),
[`tidy_multgee()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_multgee.md),
[`tidy_parameters()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_parameters.md),
[`tidy_svy_vglm()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_svy_vglm.md),
[`tidy_vgam()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_vgam.md),
[`tidy_with_broom_or_parameters()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_with_broom_or_parameters.md)

## Examples

``` r
# \donttest{
  library(pscl)
#> Classes and Methods for R originally developed in the
#> Political Science Computational Laboratory
#> Department of Political Science
#> Stanford University (2002-2015),
#> by and under the direction of Simon Jackman.
#> hurdle and zeroinfl functions by Achim Zeileis.
  mod <- zeroinfl(
    art ~ fem + mar + phd,
    data = pscl::bioChemists
  )

  mod |> tidy_zeroinfl(exponentiate = TRUE)
#>          term  estimate  std.error conf.level  conf.low conf.high    statistic
#> 1 (Intercept) 1.9931582 0.24207622       0.95 1.5709425 2.5288511  5.678880601
#> 2    femWomen 0.7927569 0.05065926       0.95 0.6994328 0.8985331 -3.634257379
#> 3  marMarried 1.0205525 0.06836479       0.95 0.8949835 1.1637392  0.303698153
#> 4         phd 1.0476698 0.03191840       0.95 0.9869420 1.1121342  1.528534270
#> 5 (Intercept) 0.4927629 0.21953942       0.95 0.2057805 1.1799720 -1.588515135
#> 6    femWomen 0.9996043 0.24730683       0.95 0.6155125 1.6233771 -0.001599636
#> 7  marMarried 0.8705892 0.22301499       0.95 0.5269443 1.4383408 -0.540998011
#> 8         phd 0.8268979 0.09602765       0.95 0.6585699 1.0382500 -1.636734762
#>   df.error      p.value     component     original_term
#> 1      Inf 1.355791e-08   conditional count_(Intercept)
#> 2      Inf 2.787825e-04   conditional    count_femWomen
#> 3      Inf 7.613579e-01   conditional  count_marMarried
#> 4      Inf 1.263799e-01   conditional         count_phd
#> 5      Inf 1.121699e-01 zero_inflated  zero_(Intercept)
#> 6      Inf 9.987237e-01 zero_inflated     zero_femWomen
#> 7      Inf 5.885090e-01 zero_inflated   zero_marMarried
#> 8      Inf 1.016859e-01 zero_inflated          zero_phd
# }
```
