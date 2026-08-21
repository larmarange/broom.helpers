# Add the (weighted) number of observations

Add the number of observations in a new column `n_obs`, taking into
account any weights if they have been defined.

## Usage

``` r
tidy_add_n(x, model = tidy_get_model(x))
```

## Arguments

- x:

  (`data.frame`)  
  A tidy tibble as produced by `tidy_*()` functions.

- model:

  (a model object, e.g. `glm`)  
  The corresponding model, if not attached to `x`.

## Details

For continuous variables, it corresponds to all valid observations
contributing to the model.

For categorical variables coded with treatment or sum contrasts, each
model term could be associated to only one level of the original
categorical variable. Therefore, `n_obs` will correspond to the number
of observations associated with that level. `n_obs` will also be
computed for reference rows. For polynomial contrasts (defined with
[`stats::contr.poly()`](https://rdrr.io/r/stats/contrast.html)), all
levels will contribute to the computation of each model term. Therefore,
`n_obs` will be equal to the total number of observations. For Helmert
and custom contrasts, only rows contributing positively (i.e. with a
positive contrast) to the computation of a term will be considered for
estimating `n_obs`. The result could therefore be difficult to
interpret. For a better understanding of which observations are taken
into account to compute `n_obs` values, you could look at
[`model_compute_terms_contributions()`](https://larmarange.github.io/broom.helpers/reference/model_compute_terms_contributions.md).

For interaction terms, only rows contributing to all the terms of the
interaction will be considered to compute `n_obs`.

For binomial logistic models, `tidy_add_n()` will also return the
corresponding number of events (`n_event`) for each term, taking into
account any defined weights. Observed proportions could be obtained as
`n_obs / n_event`.

Similarly, a number of events will be computed for multinomial logistic
models
([`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html)) for
each level of the outcome (`y.level`), corresponding to the number of
observations equal to that outcome level.

For Poisson models, `n_event` will be equal to the number of counts per
term. In addition, a third column `exposure` will be computed. If no
offset is defined, exposure is assumed to be equal to 1 (eventually
multiplied by weights) per observation. If an offset is defined,
`exposure` will be equal to the (weighted) sum of the exponential of the
offset (as a reminder, to model the effect of `x` on the ratio `y / z`,
a Poisson model will be defined as
`glm(y ~ x + offset(log(z)), family = poisson)`). Observed rates could
be obtained with `n_event / exposure`.

For Cox models
([`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)), an
individual could be coded with several observations (several rows).
`n_obs` will correspond to the weighted number of observations which
could be different from the number of individuals `n_ind`.
`tidy_add_n()` will also compute a (weighted) number of events
(`n_event`) according to the definition of the
[`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html) object.
Exposure time is also returned in `exposure` column. It is equal to the
(weighted) sum of the time variable if only one variable time is passed
to [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html), and
to the (weighted) sum of `time2 - time` if two time variables are
defined in
[`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html).

For competing risk regression models
([`tidycmprsk::crr()`](https://mskcc-epi-bio.github.io/tidycmprsk/reference/crr.html)),
`n_event` takes into account only the event of interest defined by
`failcode.`

The (weighted) total number of observations (`N_obs`), of individuals
(`N_ind`), of events (`N_event`) and of exposure time (`Exposure`) are
stored as attributes of the returned tibble.

## See also

Other tidy_helpers:
[`tidy_add_coefficients_type()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_coefficients_type.md),
[`tidy_add_contrasts()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_contrasts.md),
[`tidy_add_estimate_to_reference_rows()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_estimate_to_reference_rows.md),
[`tidy_add_header_rows()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_header_rows.md),
[`tidy_add_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_pairwise_contrasts.md),
[`tidy_add_reference_rows()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_reference_rows.md),
[`tidy_add_term_labels()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_term_labels.md),
[`tidy_add_variable_labels()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_variable_labels.md),
[`tidy_attach_model()`](https://larmarange.github.io/broom.helpers/reference/tidy_attach_model.md),
[`tidy_disambiguate_terms()`](https://larmarange.github.io/broom.helpers/reference/tidy_disambiguate_terms.md),
[`tidy_group_by()`](https://larmarange.github.io/broom.helpers/reference/tidy_group_by.md),
[`tidy_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_identify_variables.md),
[`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.md),
[`tidy_remove_intercept()`](https://larmarange.github.io/broom.helpers/reference/tidy_remove_intercept.md),
[`tidy_select_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_select_variables.md)

## Examples

``` r
# \donttest{
lm(Petal.Length ~ ., data = iris) |>
  tidy_and_attach() |>
  tidy_add_n()
#> # A tibble: 6 × 8
#>   term            n_obs estimate std.error statistic  p.value conf.low conf.high
#>   <chr>           <dbl>    <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
#> 1 (Intercept)       150   -1.11     0.270      -4.12 6.45e- 5   -1.64    -0.578 
#> 2 Sepal.Length      150    0.608    0.0502     12.1  1.07e-23    0.509    0.707 
#> 3 Sepal.Width       150   -0.181    0.0804     -2.25 2.62e- 2   -0.339   -0.0217
#> 4 Petal.Width       150    0.602    0.121       4.96 1.97e- 6    0.362    0.842 
#> 5 Speciesversico…    50    1.46     0.173       8.44 3.14e-14    1.12     1.81  
#> 6 Speciesvirgini…    50    1.97     0.245       8.06 2.60e-13    1.49     2.46  

lm(Petal.Length ~ ., data = iris, contrasts = list(Species = contr.sum)) |>
  tidy_and_attach() |>
  tidy_add_n()
#> # A tibble: 6 × 8
#>   term         n_obs estimate std.error statistic  p.value conf.low conf.high
#>   <chr>        <dbl>    <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
#> 1 (Intercept)    150   0.0349    0.259      0.135 8.93e- 1   -0.477    0.546 
#> 2 Sepal.Length   150   0.608     0.0502    12.1   1.07e-23    0.509    0.707 
#> 3 Sepal.Width    150  -0.181     0.0804    -2.25  2.62e- 2   -0.339   -0.0217
#> 4 Petal.Width    150   0.602     0.121      4.96  1.97e- 6    0.362    0.842 
#> 5 Species1        50  -1.15      0.138     -8.31  6.37e-14   -1.42    -0.873 
#> 6 Species2        50   0.318     0.0451     7.04  7.19e-11    0.228    0.407 

lm(Petal.Length ~ ., data = iris, contrasts = list(Species = contr.poly)) |>
  tidy_and_attach() |>
  tidy_add_n()
#> # A tibble: 6 × 8
#>   term         n_obs estimate std.error statistic  p.value conf.low conf.high
#>   <chr>        <dbl>    <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
#> 1 (Intercept)    150   0.0349    0.259      0.135 8.93e- 1   -0.477    0.546 
#> 2 Sepal.Length   150   0.608     0.0502    12.1   1.07e-23    0.509    0.707 
#> 3 Sepal.Width    150  -0.181     0.0804    -2.25  2.62e- 2   -0.339   -0.0217
#> 4 Petal.Width    150   0.602     0.121      4.96  1.97e- 6    0.362    0.842 
#> 5 Species.L      150   1.40      0.173      8.06  2.60e-13    1.05     1.74  
#> 6 Species.Q      150  -0.389     0.0552    -7.04  7.19e-11   -0.498   -0.280 

lm(Petal.Length ~ poly(Sepal.Length, 2), data = iris) |>
  tidy_and_attach() |>
  tidy_add_n()
#> # A tibble: 3 × 8
#>   term           n_obs estimate std.error statistic   p.value conf.low conf.high
#>   <chr>          <dbl>    <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
#> 1 (Intercept)      150     3.76    0.0685     54.9  7.97e-100     3.62      3.89
#> 2 poly(Sepal.Le…   150    18.8     0.839      22.4  3.01e- 49    17.1      20.4 
#> 3 poly(Sepal.Le…   150    -2.85    0.839      -3.39 8.87e-  4    -4.50     -1.19

df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))

glm(
  Survived ~ Class + Age + Sex,
  data = df, weights = df$n, family = binomial,
  contrasts = list(Age = contr.sum, Class = "contr.helmert")
) |>
  tidy_and_attach() |>
  tidy_add_n()
#> # A tibble: 6 × 9
#>   term    n_obs n_event estimate std.error statistic  p.value conf.low conf.high
#>   <chr>   <dbl>   <dbl>    <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
#> 1 (Inter…  2201     711   1.66      0.162     10.2   1.52e-24   1.35      1.99  
#> 2 Class1    285     118  -0.509     0.0980    -5.19  2.05e- 7  -0.703    -0.318 
#> 3 Class2    706     178  -0.423     0.0479    -8.83  1.09e-18  -0.518    -0.330 
#> 4 Class3    885     212   0.0186    0.0303     0.613 5.40e- 1  -0.0406    0.0782
#> 5 Age1     2092     654  -0.531     0.122     -4.35  1.36e- 5  -0.771    -0.292 
#> 6 SexMale  1731     367  -2.42      0.140    -17.2   1.43e-66  -2.70     -2.15  

glm(
  Survived ~ Class * (Age:Sex),
  data = df, weights = df$n, family = binomial,
  contrasts = list(Age = contr.sum, Class = "contr.helmert")
) |>
  tidy_and_attach() |>
  tidy_add_n()
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: collapsing to unique 'x' values
#> Warning: collapsing to unique 'x' values
#> Warning: collapsing to unique 'x' values
#> Warning: collapsing to unique 'x' values
#> # A tibble: 20 × 9
#>    term    n_obs n_event estimate std.error statistic p.value conf.low conf.high
#>    <chr>   <dbl>   <dbl>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#>  1 (Inter…  2201     711  1.03e+1  294.       3.52e-2  0.972  -1.14e+1    NA    
#>  2 Class1    285     118  3.33e-1  440.       7.57e-4  0.999  -9.87e+0    11.3  
#>  3 Class2    706     178 -5.62e+0  147.      -3.83e-2  0.969  NA           5.22 
#>  4 Class3    885     212  8.48e-2    0.0354   2.40e+0  0.0166  1.67e-2     0.156
#>  5 AgeAdu…   425     316 -8.56e+0  294.      -2.92e-2  0.977  NA          30.7  
#>  6 AgeChi…    45      28 -7.90e-3  461.      -1.71e-5  1.000  -6.52e+0     5.51 
#>  7 AgeAdu…  1667     338 -1.18e+1  294.      -4.03e-2  0.968  NA           9.55 
#>  8 AgeChi…    64      29 NA         NA       NA       NA      NA          NA    
#>  9 Class1…    93      80 -1.20e+0  440.      -2.73e-3  0.998  -1.73e+2     6.47 
#> 10 Class2…   165      76  4.67e+0  147.       3.18e-2  0.975  -1.15e+1    NA    
#> 11 Class3…    23      20 -4.50e-2    0.167   -2.70e-1  0.787  -3.43e-1     0.333
#> 12 Class1…    13      13  5.60e-1  692.       8.10e-4  0.999  -1.72e+1    16.7  
#> 13 Class2…    31      14  4.02e-1  231.       1.74e-3  0.999  -3.97e+0     5.57 
#> 14 Class3…     0       0 NA         NA       NA       NA      NA          NA    
#> 15 Class1…   168      14 -1.17e+0  440.      -2.65e-3  0.998  -9.86e+1     1.62 
#> 16 Class2…   462      75  5.59e+0  147.       3.81e-2  0.970  -8.68e+0    NA    
#> 17 Class3…   862     192 NA         NA       NA       NA      NA          NA    
#> 18 Class1…    11      11 NA         NA       NA       NA      NA          NA    
#> 19 Class2…    48      13 NA         NA       NA       NA      NA          NA    
#> 20 Class3…     0       0 NA         NA       NA       NA      NA          NA    

glm(response ~ age + grade * trt, gtsummary::trial, family = poisson) |>
  tidy_and_attach() |>
  tidy_add_n()
#> # A tibble: 7 × 10
#>   term      n_obs n_event exposure estimate std.error statistic p.value conf.low
#>   <chr>     <dbl>   <dbl>    <dbl>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>
#> 1 (Interce…   183      58      183 -1.95      0.581     -3.36   7.89e-4 -3.15   
#> 2 age         183      58      183  0.0113    0.00947    1.19   2.34e-1 -0.00719
#> 3 gradeII      58      17       58  0.00792   0.518      0.0153 9.88e-1 -1.04   
#> 4 gradeIII     60      20       60  0.553     0.458      1.21   2.27e-1 -0.333  
#> 5 trtDrug B    94      31       94  0.511     0.449      1.14   2.55e-1 -0.353  
#> 6 gradeII:…    29      10       29 -0.201     0.668     -0.301  7.64e-1 -1.52   
#> 7 gradeIII…    33       8       33 -1.06      0.642     -1.65   9.85e-2 -2.36   
#> # ℹ 1 more variable: conf.high <dbl>

glm(
  response ~ trt * grade + offset(log(ttdeath)),
  gtsummary::trial,
  family = poisson
) |>
  tidy_and_attach() |>
  tidy_add_n()
#> # A tibble: 6 × 10
#>   term     n_obs n_event exposure estimate std.error statistic  p.value conf.low
#>   <chr>    <dbl>   <dbl>    <dbl>    <dbl>     <dbl>     <dbl>    <dbl>    <dbl>
#> 1 (Interc…   193      61    3795.  -4.52       0.354  -12.8    2.01e-37   -5.30 
#> 2 trtDrug…    98      33    1853.   0.576      0.449    1.28   2.00e- 1   -0.288
#> 3 gradeII     63      19    1207.   0.0184     0.518    0.0355 9.72e- 1   -1.03 
#> 4 gradeIII    63      21    1183.   0.728      0.449    1.62   1.05e- 1   -0.136
#> 5 trtDrug…    33      12     576.   0.0548     0.654    0.0838 9.33e- 1   -1.24 
#> 6 trtDrug…    33       8     607.  -1.11       0.635   -1.75   8.00e- 2   -2.40 
#> # ℹ 1 more variable: conf.high <dbl>
# }
```
