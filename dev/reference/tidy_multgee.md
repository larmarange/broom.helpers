# Tidy a `multgee` model

A tidier for models generated with
[`multgee::nomLORgee()`](https://rdrr.io/pkg/multgee/man/nomLORgee.html)
or
[`multgee::ordLORgee()`](https://rdrr.io/pkg/multgee/man/ordLORgee.html).
Term names will be updated to be consistent with generic models. The
original term names are preserved in an `"original_term"` column.

## Usage

``` r
tidy_multgee(x, conf.int = TRUE, conf.level = 0.95, ...)
```

## Arguments

- x:

  (`LORgee`)  
  A
  [`multgee::nomLORgee()`](https://rdrr.io/pkg/multgee/man/nomLORgee.html)
  or a
  [`multgee::ordLORgee()`](https://rdrr.io/pkg/multgee/man/ordLORgee.html)
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

## Details

To be noted, for
[`multgee::nomLORgee()`](https://rdrr.io/pkg/multgee/man/nomLORgee.html),
the baseline `y` category is the latest modality of `y`.

## See also

Other custom_tidiers:
[`tidy_broom()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_broom.md),
[`tidy_coxphms()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_coxphms.md),
[`tidy_parameters()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_parameters.md),
[`tidy_svy_vglm()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_svy_vglm.md),
[`tidy_vgam()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_vgam.md),
[`tidy_with_broom_or_parameters()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_with_broom_or_parameters.md),
[`tidy_zeroinfl()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_zeroinfl.md)

## Examples

``` r
# \donttest{
  library(multgee)
#> Loading required package: gnm

  h <- housing
  h$status <- factor(
    h$y,
    labels = c("street", "community", "independant")
  )

  mod <- multgee::nomLORgee(
    status ~ factor(time) * sec,
    data = h,
    id = id,
    repeated = time,
  )
  mod |> tidy_multgee()
#>                  term    estimate std.error conf.level   conf.low   conf.high
#> 1         (Intercept)  1.66073121   0.25026       0.95  1.1702306  2.15123179
#> 2       factor(time)6 -1.87010328   0.31876       0.95 -2.4948614 -1.24534516
#> 3      factor(time)12 -2.92505659   0.36829       0.95 -3.6468917 -2.20322145
#> 4      factor(time)24 -2.81358954   0.34258       0.95 -3.4850340 -2.14214508
#> 5                 sec -0.53680111   0.33704       0.95 -1.1973874  0.12378515
#> 6   factor(time)6:sec -1.18218145   0.46036       0.95 -2.0844705 -0.27989243
#> 7  factor(time)12:sec  0.07915600   0.48306       0.95 -0.8676242  1.02593621
#> 8  factor(time)24:sec  0.03272988   0.46558       0.95 -0.8797902  0.94524991
#> 9         (Intercept)  1.16643489   0.26273       0.95  0.6514935  1.68137622
#> 10      factor(time)6 -0.25454473   0.30080       0.95 -0.8441019  0.33501244
#> 11     factor(time)12 -0.57052156   0.31176       0.95 -1.1815599  0.04051681
#> 12     factor(time)24 -1.04101141   0.30716       0.95 -1.6430339 -0.43898887
#> 13                sec -0.10704331   0.34759       0.95 -0.7883072  0.57422057
#> 14  factor(time)6:sec -1.62341648   0.41349       0.95 -2.4338420 -0.81299097
#> 15 factor(time)12:sec -2.04850478   0.44543       0.95 -2.9215315 -1.17547803
#> 16 factor(time)24:sec -1.04965297   0.41831       0.95 -1.8695255 -0.22978044
#>    statistic df.error      p.value        original_term   y.level
#> 1    6.63594      Inf 3.224410e-11               beta10    street
#> 2   -5.86682      Inf 4.442325e-09      factor(time)6:1    street
#> 3   -7.94235      Inf 1.983862e-15     factor(time)12:1    street
#> 4   -8.21301      Inf 2.157116e-16     factor(time)24:1    street
#> 5   -1.59271      Inf 1.112253e-01                sec:1    street
#> 6   -2.56797      Inf 1.022960e-02  factor(time)6:sec:1    street
#> 7    0.16386      Inf 8.698414e-01 factor(time)12:sec:1    street
#> 8    0.07030      Inf 9.439549e-01 factor(time)24:sec:1    street
#> 9    4.43974      Inf 9.006762e-06               beta20 community
#> 10  -0.84623      Inf 3.974244e-01      factor(time)6:2 community
#> 11  -1.83000      Inf 6.724994e-02     factor(time)12:2 community
#> 12  -3.38916      Inf 7.010709e-04     factor(time)24:2 community
#> 13  -0.30796      Inf 7.581128e-01                sec:2 community
#> 14  -3.92613      Inf 8.632351e-05  factor(time)6:sec:2 community
#> 15  -4.59898      Inf 4.245645e-06 factor(time)12:sec:2 community
#> 16  -2.50925      Inf 1.209878e-02 factor(time)24:sec:2 community

  mod2 <- ordLORgee(
    formula = y ~ factor(time) + factor(trt) + factor(baseline),
    data = multgee::arthritis,
    id = id,
    repeated = time,
    LORstr = "uniform"
  )
  mod2 |> tidy_multgee()
#>                 term     estimate std.error conf.level   conf.low   conf.high
#> 1             beta10 -1.843153449   0.38929       0.95 -2.6061478 -1.08015907
#> 2             beta20  0.266916287   0.35013       0.95 -0.4193259  0.95315848
#> 3             beta30  2.231319749   0.36625       0.95  1.5134829  2.94915656
#> 4             beta40  4.525419750   0.42123       0.95  3.6998241  5.35101538
#> 5      factor(time)3  0.001404066   0.12183       0.95 -0.2373783  0.24018648
#> 6      factor(time)5 -0.361715436   0.11395       0.95 -0.5850533 -0.13837754
#> 7       factor(trt)2 -0.512124154   0.16799       0.95 -0.8413785 -0.18286980
#> 8  factor(baseline)2 -0.669628138   0.38036       0.95 -1.4151200  0.07586376
#> 9  factor(baseline)3 -1.260703357   0.35252       0.95 -1.9516299 -0.56977685
#> 10 factor(baseline)4 -2.643726955   0.41282       0.95 -3.4528393 -1.83461462
#> 11 factor(baseline)5 -3.966126562   0.53164       0.95 -5.0081218 -2.92413131
#>    statistic df.error      p.value     original_term
#> 1   -4.73464      Inf 2.194443e-06            beta10
#> 2    0.76235      Inf 4.458511e-01            beta20
#> 3    6.09237      Inf 1.112512e-09            beta30
#> 4   10.74338      Inf 6.366982e-27            beta40
#> 5    0.01152      Inf 9.908086e-01     factor(time)3
#> 6   -3.17433      Inf 1.501828e-03     factor(time)5
#> 7   -3.04855      Inf 2.299486e-03      factor(trt)2
#> 8   -1.76050      Inf 7.832307e-02 factor(baseline)2
#> 9   -3.57628      Inf 3.485184e-04 factor(baseline)3
#> 10  -6.40414      Inf 1.512193e-10 factor(baseline)4
#> 11  -7.46020      Inf 8.639124e-14 factor(baseline)5
# }
```
