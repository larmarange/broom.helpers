# Tidy with `broom::tidy()` and checks that all arguments are used

Tidy with
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) and
checks that all arguments are used

## Usage

``` r
tidy_broom(x, ...)
```

## Arguments

- x:

  (a model object, e.g. `glm`)  
  A model to be tidied.

- ...:

  Additional parameters passed to
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html).

## See also

Other custom_tidiers:
[`tidy_coxphms()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_coxphms.md),
[`tidy_multgee()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_multgee.md),
[`tidy_parameters()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_parameters.md),
[`tidy_svy_vglm()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_svy_vglm.md),
[`tidy_vgam()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_vgam.md),
[`tidy_with_broom_or_parameters()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_with_broom_or_parameters.md),
[`tidy_zeroinfl()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_zeroinfl.md)
