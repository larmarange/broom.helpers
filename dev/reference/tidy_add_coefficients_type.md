# Add coefficients type and label as attributes

Add the type of coefficients ("generic", "logistic", "poisson",
"relative_risk" or "prop_hazard") and the corresponding coefficient
labels, as attributes to `x` (respectively named `coefficients_type` and
`coefficients_label`).

## Usage

``` r
tidy_add_coefficients_type(
  x,
  exponentiate = attr(x, "exponentiate"),
  model = tidy_get_model(x)
)
```

## Arguments

- x:

  (`data.frame`)  
  A tidy tibble as produced by `tidy_*()` functions.

- exponentiate:

  (`logical`)  
  Whether or not to exponentiate the coefficient estimates. It should be
  consistent with the original call to
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html).

- model:

  (a model object, e.g. `glm`)  
  The corresponding model, if not attached to `x`.

## See also

Other tidy_helpers:
[`tidy_add_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_contrasts.md),
[`tidy_add_estimate_to_reference_rows()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_estimate_to_reference_rows.md),
[`tidy_add_header_rows()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_header_rows.md),
[`tidy_add_n()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_n.md),
[`tidy_add_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_pairwise_contrasts.md),
[`tidy_add_reference_rows()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_reference_rows.md),
[`tidy_add_term_labels()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_term_labels.md),
[`tidy_add_variable_labels()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_variable_labels.md),
[`tidy_attach_model()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_attach_model.md),
[`tidy_disambiguate_terms()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_disambiguate_terms.md),
[`tidy_group_by()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_group_by.md),
[`tidy_identify_variables()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_identify_variables.md),
[`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_plus_plus.md),
[`tidy_remove_intercept()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_remove_intercept.md),
[`tidy_select_variables()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_select_variables.md)

## Examples

``` r
ex1 <- lm(hp ~ mpg + factor(cyl), mtcars) |>
  tidy_and_attach() |>
  tidy_add_coefficients_type()
attr(ex1, "coefficients_type")
#> [1] "generic"
attr(ex1, "coefficients_label")
#> [1] "Beta"

df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
ex2 <- glm(
  Survived ~ Class + Age * Sex,
  data = df,
  weights = df$n,
  family = binomial
) |>
  tidy_and_attach(exponentiate = TRUE) |>
  tidy_add_coefficients_type()
attr(ex2, "coefficients_type")
#> [1] "logistic"
attr(ex2, "coefficients_label")
#> [1] "OR"
```
