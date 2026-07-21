# Tidy a multi-state survival model

**\[experimental\]** A tidier for multi-state models
([`survival::coxphms.object`](https://rdrr.io/pkg/survival/man/coxphms.object.html))
generated with
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html). Term
names will be updated to be consistent with generic models. The original
term names are preserved in an `"original_term"` column. An additional
column `"state"` will be added to provide status detail (i.e. the right
part of original terms) and a column `"y.level"` will be populated by
using values stored in `x$states`.

## Usage

``` r
tidy_coxphms(x, conf.int = TRUE, conf.level = 0.95, ...)
```

## Arguments

- x:

  (`coxphms`)  
  A
  [`survival::coxphms.object`](https://rdrr.io/pkg/survival/man/coxphms.object.html)
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
[`tidy_broom()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_broom.md),
[`tidy_multgee()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_multgee.md),
[`tidy_parameters()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_parameters.md),
[`tidy_svy_vglm()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_svy_vglm.md),
[`tidy_vgam()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_vgam.md),
[`tidy_with_broom_or_parameters()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_with_broom_or_parameters.md),
[`tidy_zeroinfl()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_zeroinfl.md)

## Examples

``` r
# \donttest{
  library(survival)
  # dataset with competing-risk-style status
  df <- MASS::Melanoma
  df$id <-
    df |>
    row.names()
  df$sex <-
    df$sex |>
    factor(0:1, c("male", "female"))
  df$status <-
    df$status |>
    factor(c(2, 1, 3), c("alive", "died from melanoma", "dead from other causes"))

  mstate_model <- coxph(Surv(time, status) ~ sex, data = df, id = id)
  mstate_model |> tidy_coxphms()
#>    estimate std.error conf.level   conf.low conf.high statistic df.error
#> 1 0.6621781 0.2636234       0.95  0.1423868  1.181969  2.511833      203
#> 2 0.6302185 0.5283391       0.95 -0.4115176  1.671955  1.192830      203
#>     p.value original_term      term state                y.level
#> 1 0.1129944 sexfemale_1:2 sexfemale   1:2     died from melanoma
#> 2 0.2747595 sexfemale_1:3 sexfemale   1:3 dead from other causes
  mstate_model |> tidy_plus_plus()
#> ℹ <coxphms> model detected.
#> ✔ `tidy_coxphms()` used instead.
#> ℹ Add `tidy_fun = broom.helpers::tidy_coxphms` to quiet these messages.
#> # A tibble: 4 × 26
#>   group_by     y.level term  original_term variable var_label var_class var_type
#>   <fct>        <chr>   <chr> <chr>         <chr>    <chr>     <chr>     <chr>   
#> 1 died from m… died f… sexm… NA            sex      sex       factor    dichoto…
#> 2 died from m… died f… sexf… sexfemale_1:2 sex      sex       factor    dichoto…
#> 3 dead from o… dead f… sexm… NA            sex      sex       factor    dichoto…
#> 4 dead from o… dead f… sexf… sexfemale_1:3 sex      sex       factor    dichoto…
#> # ℹ 18 more variables: var_nlevels <int>, contrasts <chr>,
#> #   contrasts_type <chr>, reference_row <lgl>, label <chr>, n_obs <dbl>,
#> #   n_ind <dbl>, n_event <dbl>, exposure <dbl>, estimate <dbl>,
#> #   std.error <dbl>, conf.level <dbl>, conf.low <dbl>, conf.high <dbl>,
#> #   statistic <dbl>, df.error <int>, p.value <dbl>, state <chr>
# }
```
