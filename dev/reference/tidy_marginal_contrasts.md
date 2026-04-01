# Marginal Contrasts with `marginaleffects::avg_comparisons()`

Use
[`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html)
to estimate marginal contrasts for each variable of a model and return a
tibble tidied in a way that it could be used by `broom.helpers`
functions. See
[`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html)
for a list of supported models.

## Usage

``` r
tidy_marginal_contrasts(
  x,
  variables_list = "auto",
  conf.int = TRUE,
  conf.level = 0.95,
  ...
)

variables_to_contrast(
  model,
  interactions = TRUE,
  cross = FALSE,
  var_categorical = "reference",
  var_continuous = 1,
  by_categorical = unique,
  by_continuous = stats::fivenum
)
```

## Arguments

- x:

  (a model object, e.g. `glm`)  
  A model to be tidied.

- variables_list:

  (`list` or `string`)  
  A list whose elements will be sequentially passed to `variables` in
  [`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html)
  (see details below); alternatively, it could also be the string
  `"auto"` (default), `"cross"` or `"no_interaction"`

- conf.int:

  (`logical`)  
  Whether or not to include a confidence interval in the tidied output.

- conf.level:

  (`numeric`)  
  The confidence level to use for the confidence interval (between `0`
  ans `1`).

- ...:

  Additional parameters passed to
  [`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html).

- model:

  (a model object, e.g. `glm`)  
  A model.

- interactions:

  (`logical`)  
  Should combinations of variables corresponding to interactions be
  returned?

- cross:

  (`logical`)  
  If `interaction` is `TRUE`, should "cross-contrasts" be computed? (if
  `FALSE`, only the last term of an interaction is passed to `variable`
  and the other terms are passed to `by`)

- var_categorical:

  ([`predictor values`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html))  
  Default `variable` value for categorical variables.

- var_continuous:

  ([`predictor values`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html))  
  Default `variable` value for continuous variables.

- by_categorical:

  ([`predictor values`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html))  
  Default `by` value for categorical variables.

- by_continuous:

  ([`predictor values`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html))  
  Default `by` value for continuous variables.

## Details

Marginal contrasts are obtained by calling, for each variable or
combination of variables,
[`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html).

`tidy_marginal_contrasts()` will compute marginal contrasts for each
variable or combination of variables, before stacking the results in a
unique tibble. This is why `tidy_marginal_contrasts()` has a
`variables_list` argument consisting of a list of specifications that
will be passed sequentially to the `variables` and the `by` argument of
[`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html).

Considering a single categorical variable named `cat`,
`tidy_marginal_contrasts()` will call
`avg_comparisons(model, variables = list(cat = "reference"))` to obtain
average marginal contrasts for this variable.

Considering a single continuous variable named `cont`,
`tidy_marginalcontrasts()` will call
`avg_comparisons(model, variables = list(cont = 1))` to obtain average
marginal contrasts for an increase of one unit.

For a combination of variables, there are several possibilities. You
could compute "cross-contrasts" by providing simultaneously several
variables to `variables` and specifying `cross = TRUE` to
[`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html).
Alternatively, you could compute the contrasts of a first variable
specified to `variables` for the different values of a second variable
specified to `by`.

The helper function `variables_to_contrast()` could be used to
automatically generate a suitable list to be used with `variables_list`.
Each combination of variables should be a list with two named elements:
`"variables"` a list of named elements passed to `variables` and `"by"`
a list of named elements used for creating a relevant `datagrid` and
whose names are passed to `by`.

`variables_list`'s default value, `"auto"`, calls
`variables_to_contrast(interactions = TRUE, cross = FALSE)` while
`"no_interaction"` is a shortcut for
`variables_to_contrast(interactions = FALSE)`. `"cross"` calls
`variables_to_contrast(interactions = TRUE, cross = TRUE)`

You can also provide custom specifications (see examples).

By default, *average marginal contrasts* are computed: contrasts are
computed using a counterfactual grid for each value of the variable of
interest, before averaging the results. *Marginal contrasts at the mean*
could be obtained by indicating `newdata = "mean"`. Other assumptions
are possible, see the help file of
[`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html).

For more information, see
`vignette("marginal_tidiers", "broom.helpers")`.

## See also

[`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html),
[`tidy_avg_comparisons()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_avg_comparisons.md)

Other marginal_tieders:
[`tidy_all_effects()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_all_effects.md),
[`tidy_avg_comparisons()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_avg_comparisons.md),
[`tidy_avg_slopes()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_avg_slopes.md),
[`tidy_ggpredict()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_ggpredict.md),
[`tidy_marginal_predictions()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_marginal_predictions.md),
[`tidy_margins()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_margins.md)

## Examples

``` r
# \donttest{
# Average Marginal Contrasts
df <- Titanic |>
  dplyr::as_tibble() |>
  tidyr::uncount(n) |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
mod <- glm(
  Survived ~ Class + Age + Sex,
  data = df, family = binomial
)
tidy_marginal_contrasts(mod)
#>   variable          term   estimate  std.error  statistic      p.value
#> 1    Class     2nd - 1st -0.1951651 0.03654057  -5.341051 9.240906e-08
#> 2    Class     3rd - 1st -0.3066202 0.03069777  -9.988355 1.714025e-23
#> 3    Class    Crew - 1st -0.1676466 0.03230024  -5.190258 2.100029e-07
#> 4      Age Child - Adult  0.1972150 0.04867887   4.051347 5.092362e-05
#> 5      Sex Male - Female -0.5073533 0.02452155 -20.690097 4.253562e-95
#>     s.value   conf.low  conf.high
#> 1  23.36739 -0.2667833 -0.1235469
#> 2  75.62696 -0.3667867 -0.2464537
#> 3  22.18309 -0.2309539 -0.1043393
#> 4  14.26131  0.1018062  0.2926238
#> 5 313.49450 -0.5554146 -0.4592919
tidy_plus_plus(mod, tidy_fun = tidy_marginal_contrasts)
#> # A tibble: 5 × 20
#>   term          variable var_label var_class var_type    var_nlevels contrasts  
#>   <chr>         <chr>    <chr>     <chr>     <chr>             <int> <chr>      
#> 1 2nd - 1st     Class    Class     character categorical           4 contr.trea…
#> 2 3rd - 1st     Class    Class     character categorical           4 contr.trea…
#> 3 Crew - 1st    Class    Class     character categorical           4 contr.trea…
#> 4 Child - Adult Age      Age       character dichotomous           2 contr.trea…
#> 5 Male - Female Sex      Sex       character dichotomous           2 contr.trea…
#> # ℹ 13 more variables: contrasts_type <chr>, reference_row <lgl>, label <chr>,
#> #   n_obs <dbl>, n_event <dbl>, estimate <dbl>, std.error <dbl>,
#> #   statistic <dbl>, p.value <dbl>, s.value <dbl>, conf.low <dbl>,
#> #   conf.high <dbl>, label_attr <chr>

mod2 <- lm(Petal.Length ~ poly(Petal.Width, 2) + Species, data = iris)
tidy_marginal_contrasts(mod2)
#>      variable                term  estimate std.error statistic      p.value
#> 1 Petal.Width                  +1 0.9961779 0.1523954  6.536799 6.284962e-11
#> 2     Species versicolor - setosa 1.3170279 0.3145654  4.186817 2.828938e-05
#> 3     Species  virginica - setosa 1.9123142 0.3733402  5.122176 3.020300e-07
#>    s.value  conf.low conf.high
#> 1 33.88931 0.6974884  1.294867
#> 2 15.10938 0.7004910  1.933565
#> 3 21.65880 1.1805808  2.644048
tidy_marginal_contrasts(
  mod2,
  variables_list = variables_to_predict(
    mod2,
    continuous = 3,
    categorical = "pairwise"
  )
)
#>      variable                   term  estimate std.error statistic      p.value
#> 1 Petal.Width                     +3 1.7390991 1.0011046  1.737180 8.235537e-02
#> 2     Species    versicolor - setosa 1.3170279 0.3145654  4.186817 2.828938e-05
#> 3     Species     virginica - setosa 1.9123142 0.3733402  5.122176 3.020300e-07
#> 4     Species virginica - versicolor 0.5952863 0.1305792  4.558814 5.144328e-06
#>     s.value   conf.low conf.high
#> 1  3.601993 -0.2230299 3.7012282
#> 2 15.109380  0.7004910 1.9335648
#> 3 21.658805  1.1805808 2.6440476
#> 4 17.568586  0.3393558 0.8512168

# Model with interactions
mod3 <- glm(
  Survived ~ Sex * Age + Class,
  data = df, family = binomial
)
tidy_marginal_contrasts(mod3)
#>   variable                   term    estimate  std.error statistic      p.value
#> 1    Class              2nd - 1st -0.19208546 0.03613415 -5.315898 1.061324e-07
#> 2    Class              3rd - 1st -0.30271279 0.03049293 -9.927311 3.166809e-23
#> 3    Class             Crew - 1st -0.15351506 0.03217010 -4.771980 1.824240e-06
#> 4  Sex:Age Female * Child - Adult -0.02017115 0.06255589 -0.322450 7.471118e-01
#> 5  Sex:Age   Male * Child - Adult  0.36915555 0.06129914  6.022198 1.720646e-09
#>      s.value   conf.low   conf.high
#> 1 23.1676312 -0.2629071 -0.12126383
#> 2 74.7413161 -0.3624778 -0.24294775
#> 3 19.0642732 -0.2165673 -0.09046282
#> 4  0.4206039 -0.1427784  0.10243615
#> 5 29.1144025  0.2490114  0.48929966
tidy_marginal_contrasts(mod3, "no_interaction")
#>   variable          term   estimate  std.error  statistic       p.value
#> 1      Sex Male - Female -0.5216026 0.02386423 -21.857087 6.655897e-106
#> 2      Age Child - Adult  0.2891015 0.05091793   5.677793  1.364433e-08
#> 3    Class     2nd - 1st -0.1920855 0.03613415  -5.315898  1.061324e-07
#> 4    Class     3rd - 1st -0.3027128 0.03049293  -9.927311  3.166809e-23
#> 5    Class    Crew - 1st -0.1535151 0.03217010  -4.771980  1.824240e-06
#>     s.value   conf.low   conf.high
#> 1 349.38974 -0.5683756 -0.47482955
#> 2  26.12712  0.1893042  0.38889880
#> 3  23.16763 -0.2629071 -0.12126383
#> 4  74.74132 -0.3624778 -0.24294775
#> 5  19.06427 -0.2165673 -0.09046282
tidy_marginal_contrasts(mod3, "cross")
#>   variable                          term   estimate  std.error statistic
#> 1    Class                     2nd - 1st -0.1920855 0.03613415 -5.315898
#> 2    Class                     3rd - 1st -0.3027128 0.03049293 -9.927311
#> 3    Class                    Crew - 1st -0.1535151 0.03217010 -4.771980
#> 4  Sex:Age Child - Adult * Male - Female -0.1695523 0.06280633 -2.699605
#>        p.value   s.value   conf.low   conf.high
#> 1 1.061324e-07 23.167631 -0.2629071 -0.12126383
#> 2 3.166809e-23 74.741316 -0.3624778 -0.24294775
#> 3 1.824240e-06 19.064273 -0.2165673 -0.09046282
#> 4 6.942175e-03  7.170397 -0.2926505 -0.04645417
tidy_marginal_contrasts(
  mod3,
  variables_list = list(
    list(variables = list(Class = "pairwise"), by = list(Sex = unique)),
    list(variables = list(Age = "all")),
    list(variables = list(Class = "sequential", Sex = "reference"))
  )
)
#>     variable                       term    estimate  std.error  statistic
#> 1  Sex:Class         Female * 2nd - 1st -0.13997749 0.02991494  -4.679184
#> 2  Sex:Class         Female * 3rd - 1st -0.30766670 0.02947081 -10.439711
#> 3  Sex:Class         Female * 3rd - 2nd -0.16768921 0.03644975  -4.600559
#> 4  Sex:Class        Female * Crew - 1st -0.10025576 0.02214882  -4.526461
#> 5  Sex:Class        Female * Crew - 2nd  0.03972173 0.03075434   1.291581
#> 6  Sex:Class        Female * Crew - 3rd  0.20741095 0.02966665   6.991383
#> 7  Sex:Class           Male * 2nd - 1st -0.20622418 0.03884619  -5.308736
#> 8  Sex:Class           Male * 3rd - 1st -0.30253894 0.03297582  -9.174568
#> 9  Sex:Class           Male * 3rd - 2nd -0.09631477 0.02470330  -3.898862
#> 10 Sex:Class          Male * Crew - 1st -0.16777963 0.03553556  -4.721458
#> 11 Sex:Class          Male * Crew - 2nd  0.03844455 0.02847019   1.350344
#> 12 Sex:Class          Male * Crew - 3rd  0.13475931 0.01866581   7.219578
#> 13       Age              Adult - Child -0.28910149 0.05091793  -5.677793
#> 14       Age              Child - Adult  0.28910149 0.05091793   5.677793
#> 15 Class:Sex  2nd - 1st * Male - Female -0.69156750 0.03221261 -21.468846
#> 16 Class:Sex  3rd - 2nd * Male - Female -0.64790478 0.03583072 -18.082385
#> 17 Class:Sex Crew - 3rd * Male - Female -0.34545626 0.03495969  -9.881560
#>          p.value    s.value    conf.low   conf.high
#> 1   2.880191e-06  18.405404 -0.19860969 -0.08134529
#> 2   1.633079e-25  82.340608 -0.36542842 -0.24990498
#> 3   4.213585e-06  17.856520 -0.23912940 -0.09624902
#> 4   5.997974e-06  17.347093 -0.14366664 -0.05684487
#> 5   1.965021e-01   2.347383 -0.02055566  0.09999913
#> 6   2.721895e-12  38.418525  0.14926537  0.26555652
#> 7   1.103879e-07  23.110914 -0.28236131 -0.13008704
#> 8   4.533874e-20  64.257818 -0.36717037 -0.23790752
#> 9   9.664587e-05  13.336932 -0.14473235 -0.04789718
#> 10  2.341602e-06  18.704073 -0.23742804 -0.09813122
#> 11  1.769057e-01   2.498948 -0.01735600  0.09424509
#> 12  5.214898e-13  40.802426  0.09817499  0.17134364
#> 13  1.364433e-08  26.127123 -0.38889880 -0.18930418
#> 14  1.364433e-08  26.127123  0.18930418  0.38889880
#> 15 3.044879e-102 337.230281 -0.75470306 -0.62843195
#> 16  4.386928e-73 240.367540 -0.71813169 -0.57767787
#> 17  5.004764e-23  74.081044 -0.41397599 -0.27693653

mod4 <- lm(Sepal.Length ~ Petal.Length * Petal.Width + Species, data = iris)
tidy_marginal_contrasts(mod4)
#>                   variable                term     estimate std.error
#> 1                  Species versicolor - setosa -1.462933106 0.3719381
#> 2                  Species  virginica - setosa -1.984239711 0.4231549
#> 3 Petal.Length:Petal.Width              1 * +1 -0.125145768 0.3141612
#> 4 Petal.Length:Petal.Width            1.6 * +1 -0.105804839 0.2767317
#> 5 Petal.Length:Petal.Width           4.35 * +1 -0.017158912 0.1587604
#> 6 Petal.Length:Petal.Width            5.1 * +1  0.007017249 0.1594941
#> 7 Petal.Length:Petal.Width            6.9 * +1  0.065040037 0.2256260
#>     statistic      p.value     s.value   conf.low  conf.high
#> 1 -3.93326990 8.379805e-05 13.54272375 -2.1919185 -0.7339477
#> 2 -4.68915674 2.743332e-06 18.47563935 -2.8136081 -1.1548713
#> 3 -0.39834887 6.903730e-01  0.53455197 -0.7408904  0.4905989
#> 4 -0.38233719 7.022113e-01  0.51002294 -0.6481891  0.4365794
#> 5 -0.10808053 9.139318e-01  0.12984157 -0.3283236  0.2940058
#> 6  0.04399693 9.649069e-01  0.05153842 -0.3055854  0.3196199
#> 7  0.28826481 7.731440e-01  0.37119087 -0.3771788  0.5072589
tidy_marginal_contrasts(
  mod4,
  variables_list = list(
    list(
      variables = list(Species = "sequential"),
      by = list(Petal.Length = c(2, 5))
    ),
    list(
      variables = list(Petal.Length = 2),
      by = list(Species = unique, Petal.Width = 2:4)
    )
  )
)
#>                            variable                       term   estimate
#> 1              Petal.Length:Species    2 * versicolor - setosa -1.4629331
#> 2              Petal.Length:Species 2 * virginica - versicolor -0.5213066
#> 3              Petal.Length:Species    5 * versicolor - setosa -1.4629331
#> 4              Petal.Length:Species 5 * virginica - versicolor -0.5213066
#> 5  Species:Petal.Width:Petal.Length            setosa * 2 * +2  1.8375120
#> 6  Species:Petal.Width:Petal.Length            setosa * 3 * +2  1.9019817
#> 7  Species:Petal.Width:Petal.Length            setosa * 4 * +2  1.9664515
#> 8  Species:Petal.Width:Petal.Length        versicolor * 2 * +2  1.8375120
#> 9  Species:Petal.Width:Petal.Length        versicolor * 3 * +2  1.9019817
#> 10 Species:Petal.Width:Petal.Length        versicolor * 4 * +2  1.9664515
#> 11 Species:Petal.Width:Petal.Length         virginica * 2 * +2  1.8375120
#> 12 Species:Petal.Width:Petal.Length         virginica * 3 * +2  1.9019817
#> 13 Species:Petal.Width:Petal.Length         virginica * 4 * +2  1.9664515
#>    std.error statistic      p.value  s.value   conf.low  conf.high
#> 1  0.3719381 -3.933270 8.379806e-05 13.54272 -2.1919185 -0.7339477
#> 2  0.1263081 -4.127262 3.671076e-05 14.73344 -0.7688659 -0.2737473
#> 3  0.3719381 -3.933270 8.379795e-05 13.54273 -2.1919184 -0.7339478
#> 4  0.1263081 -4.127262 3.671073e-05 14.73344 -0.7688659 -0.2737473
#> 5  0.1601261 11.475404 1.753568e-30 98.84755  1.5236705  2.1513534
#> 6  0.2541643  7.483278 7.249131e-14 43.64919  1.4038290  2.4001345
#> 7  0.3833691  5.129395 2.906751e-07 21.71409  1.2150619  2.7178412
#> 8  0.1601261 11.475405 1.753550e-30 98.84756  1.5236706  2.1513534
#> 9  0.2541643  7.483276 7.249230e-14 43.64917  1.4038288  2.4001346
#> 10 0.3833691  5.129395 2.906751e-07 21.71409  1.2150619  2.7178411
#> 11 0.1601261 11.475403 1.753586e-30 98.84753  1.5236705  2.1513534
#> 12 0.2541643  7.483278 7.249132e-14 43.64919  1.4038290  2.4001345
#> 13 0.3833691  5.129395 2.906751e-07 21.71409  1.2150619  2.7178412

# Marginal Contrasts at the Mean
tidy_marginal_contrasts(mod, newdata = "mean")
#>   variable          term   estimate  std.error  statistic       p.value
#> 1    Class     2nd - 1st -0.2083189 0.03909115  -5.329054  9.872545e-08
#> 2    Class     3rd - 1st -0.3030788 0.03320759  -9.126792  7.055934e-20
#> 3    Class    Crew - 1st -0.1815385 0.03558936  -5.100919  3.380076e-07
#> 4      Age Child - Adult  0.2315175 0.06082479   3.806301  1.410609e-04
#> 5      Sex Male - Female -0.5405541 0.02514911 -21.493967 1.772950e-102
#>     s.value   conf.low  conf.high rowid Age.x Class.x Sex.x Survived.x Age.y
#> 1  23.27200 -0.2849361 -0.1317016    NA  <NA>    <NA>  <NA>       <NA>  <NA>
#> 2  63.61972 -0.3681645 -0.2379931    NA  <NA>    <NA>  <NA>       <NA>  <NA>
#> 3  21.49644 -0.2512924 -0.1117846    NA  <NA>    <NA>  <NA>       <NA>  <NA>
#> 4  12.79139  0.1123030  0.3507319     1 Adult    Crew  Male         No Adult
#> 5 338.01051 -0.5898454 -0.4912627     1 Adult    Crew  Male         No Adult
#>   Class.y Sex.y Survived.y predicted_lo predicted_hi predicted
#> 1    <NA>  <NA>       <NA>           NA           NA        NA
#> 2    <NA>  <NA>       <NA>           NA           NA        NA
#> 3    <NA>  <NA>       <NA>           NA           NA        NA
#> 4    Crew  Male         No    0.2254997    0.4570172        NA
#> 5    Crew  Male         No    0.7660538    0.2254997        NA
tidy_marginal_contrasts(mod3, newdata = "mean")
#>   variable                   term    estimate  std.error  statistic
#> 1    Class              2nd - 1st -0.20575641 0.03889922 -5.2894733
#> 2    Class              3rd - 1st -0.29710778 0.03330055 -8.9220082
#> 3    Class             Crew - 1st -0.16816852 0.03583195 -4.6932563
#> 4  Sex:Age Female * Child - Adult -0.01826595 0.05710961 -0.3198401
#> 5  Sex:Age   Male * Child - Adult  0.41025175 0.06569429  6.2448616
#>        p.value    s.value   conf.low   conf.high
#> 1 1.226691e-07 22.9587252 -0.2819975 -0.12951533
#> 2 4.579082e-19 60.9215753 -0.3623757 -0.23183990
#> 3 2.688903e-06 18.5045510 -0.2383978 -0.09793919
#> 4 7.490895e-01  0.4167899 -0.1301987  0.09366684
#> 5 4.241755e-10 31.1346196  0.2814933  0.53901020
# }
```
