# Get the number of observations

For binomial and multinomial logistic models, will also return the
number of events.

## Usage

``` r
model_get_n(model)

# Default S3 method
model_get_n(model)

# S3 method for class 'glm'
model_get_n(model)

# S3 method for class 'glmerMod'
model_get_n(model)

# S3 method for class 'multinom'
model_get_n(model)

# S3 method for class 'LORgee'
model_get_n(model)

# S3 method for class 'coxph'
model_get_n(model)

# S3 method for class 'survreg'
model_get_n(model)

# S3 method for class 'model_fit'
model_get_n(model)

# S3 method for class 'tidycrr'
model_get_n(model)
```

## Arguments

- model:

  (a model object, e.g. `glm`)  
  A model object.

## Details

For Poisson models, will return the number of events and exposure time
(defined with [`stats::offset()`](https://rdrr.io/r/stats/offset.html)).

For Cox models
([`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)),
will return the number of events, exposure time and the number of
individuals.

For competing risk regression models
([`tidycmprsk::crr()`](https://mskcc-epi-bio.github.io/tidycmprsk/reference/crr.html)),
`n_event` takes into account only the event of interest defined by
`failcode.`

See
[`tidy_add_n()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_n.md)
for more details.

The total number of observations (`N_obs`), of individuals (`N_ind`), of
events (`N_event`) and of exposure time (`Exposure`) are stored as
attributes of the returned tibble.

This function does not cover `lavaan` models (`NULL` is returned).

## See also

Other model_helpers:
[`model_compute_terms_contributions()`](https://larmarange.github.io/broom.helpers/dev/reference/model_compute_terms_contributions.md),
[`model_get_assign()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_assign.md),
[`model_get_coefficients_type()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_coefficients_type.md),
[`model_get_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_contrasts.md),
[`model_get_model()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_model.md),
[`model_get_model_frame()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_model_frame.md),
[`model_get_model_matrix()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_model_matrix.md),
[`model_get_nlevels()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_nlevels.md),
[`model_get_offset()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_offset.md),
[`model_get_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_pairwise_contrasts.md),
[`model_get_response()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_response.md),
[`model_get_response_variable()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_response_variable.md),
[`model_get_terms()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_terms.md),
[`model_get_weights()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_weights.md),
[`model_get_xlevels()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_xlevels.md),
[`model_identify_variables()`](https://larmarange.github.io/broom.helpers/dev/reference/model_identify_variables.md),
[`model_list_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/model_list_contrasts.md),
[`model_list_higher_order_variables()`](https://larmarange.github.io/broom.helpers/dev/reference/model_list_higher_order_variables.md),
[`model_list_terms_levels()`](https://larmarange.github.io/broom.helpers/dev/reference/model_list_terms_levels.md),
[`model_list_variables()`](https://larmarange.github.io/broom.helpers/dev/reference/model_list_variables.md)

## Examples

``` r
lm(hp ~ mpg + factor(cyl) + disp:hp, mtcars) |>
  model_get_n()
#> # A tibble: 6 × 2
#>   term         n_obs
#>   <chr>        <dbl>
#> 1 (Intercept)     32
#> 2 mpg             32
#> 3 factor(cyl)6     7
#> 4 factor(cyl)8    14
#> 5 hp:disp         32
#> 6 factor(cyl)4    11

mod <- glm(
  response ~ stage * grade + trt,
  gtsummary::trial,
  family = binomial,
  contrasts = list(stage = contr.sum, grade = contr.treatment(3, 2), trt = "contr.SAS")
)
mod |> model_get_n()
#> # A tibble: 16 × 3
#>    term          n_obs n_event
#>    <chr>         <dbl>   <dbl>
#>  1 (Intercept)     193      61
#>  2 stage1           52      18
#>  3 stage2           52      13
#>  4 stage3           40      15
#>  5 grade1           67      21
#>  6 grade3           63      21
#>  7 trtDrug A        95      28
#>  8 stage1:grade1    17       6
#>  9 stage2:grade1    17       4
#> 10 stage3:grade1    18       7
#> 11 stage1:grade3    13       6
#> 12 stage2:grade3    19       5
#> 13 stage3:grade3    13       4
#> 14 stage4           49      15
#> 15 grade2           63      19
#> 16 trtDrug B        98      33

if (FALSE) { # \dontrun{
mod <- glm(
  Survived ~ Class * Age + Sex,
  data = Titanic |> as.data.frame(),
  weights = Freq, family = binomial
)
mod |> model_get_n()

d <- dplyr::as_tibble(Titanic) |>
  dplyr::group_by(Class, Sex, Age) |>
  dplyr::summarise(
    n_survived = sum(n * (Survived == "Yes")),
    n_dead = sum(n * (Survived == "No"))
  )
mod <- glm(cbind(n_survived, n_dead) ~ Class * Age + Sex, data = d, family = binomial)
mod |> model_get_n()

mod <- glm(response ~ age + grade * trt, gtsummary::trial, family = poisson)
mod |> model_get_n()

mod <- glm(
  response ~ trt * grade + offset(ttdeath),
  gtsummary::trial,
  family = poisson
)
mod |> model_get_n()

dont
df <- survival::lung |> dplyr::mutate(sex = factor(sex))
mod <- survival::coxph(survival::Surv(time, status) ~ ph.ecog + age + sex, data = df)
mod |> model_get_n()

mod <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
mod |> model_get_n()

mod <- lme4::glmer(response ~ trt * grade + (1 | stage),
  family = binomial, data = gtsummary::trial
)
mod |> model_get_n()

mod <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
  family = binomial, data = lme4::cbpp
)
mod |> model_get_n()
} # }
```
