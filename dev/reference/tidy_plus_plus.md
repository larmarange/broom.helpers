# Tidy a model and compute additional informations

This function will apply sequentially:

- [`tidy_and_attach()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_attach_model.md)

- [`tidy_disambiguate_terms()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_disambiguate_terms.md)

- [`tidy_identify_variables()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_identify_variables.md)

- [`tidy_add_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_contrasts.md)

- [`tidy_add_reference_rows()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_reference_rows.md)

- [`tidy_add_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_pairwise_contrasts.md)

- [`tidy_add_estimate_to_reference_rows()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_estimate_to_reference_rows.md)

- [`tidy_add_variable_labels()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_variable_labels.md)

- [`tidy_add_term_labels()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_term_labels.md)

- [`tidy_add_header_rows()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_header_rows.md)

- [`tidy_add_n()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_n.md)

- [`tidy_remove_intercept()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_remove_intercept.md)

- [`tidy_select_variables()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_select_variables.md)

- [`tidy_group_by()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_group_by.md)

- [`tidy_add_coefficients_type()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_coefficients_type.md)

- [`tidy_detach_model()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_attach_model.md)

## Usage

``` r
tidy_plus_plus(
  model,
  tidy_fun = tidy_with_broom_or_parameters,
  conf.int = TRUE,
  conf.level = 0.95,
  exponentiate = FALSE,
  model_matrix_attr = TRUE,
  variable_labels = NULL,
  instrumental_suffix = " (instrumental)",
  term_labels = NULL,
  interaction_sep = " * ",
  categorical_terms_pattern = "{level}",
  relabel_poly = FALSE,
  disambiguate_terms = TRUE,
  disambiguate_sep = ".",
  add_reference_rows = TRUE,
  no_reference_row = NULL,
  add_pairwise_contrasts = FALSE,
  pairwise_variables = all_categorical(),
  keep_model_terms = FALSE,
  pairwise_reverse = TRUE,
  contrasts_adjust = NULL,
  emmeans_args = list(),
  add_estimate_to_reference_rows = TRUE,
  add_header_rows = FALSE,
  show_single_row = NULL,
  add_n = TRUE,
  intercept = FALSE,
  include = everything(),
  group_by = auto_group_by(),
  group_labels = NULL,
  keep_model = FALSE,
  tidy_post_fun = NULL,
  quiet = FALSE,
  strict = FALSE,
  ...
)
```

## Arguments

- model:

  (a model object, e.g. `glm`)  
  A model to be attached/tidied.

- tidy_fun:

  (`function`)  
  Option to specify a custom tidier function.

- conf.int:

  (`logical`)  
  Should confidence intervals be computed? (see
  [`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html))

- conf.level:

  (`numeric`)  
  Level of confidence for confidence intervals (default: 95%).

- exponentiate:

  (`logical`)  
  Whether or not to exponentiate the coefficient estimates. This is
  typical for logistic, Poisson and Cox models, but a bad idea if there
  is no log or logit link; defaults to `FALSE`.

- model_matrix_attr:

  (`logical`)  
  Whether model frame and model matrix should be added as attributes of
  `model` (respectively named `"model_frame"` and `"model_matrix"`) and
  passed through.

- variable_labels:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.html))  
  A named list or a named vector of custom variable labels.

- instrumental_suffix:

  (`string`)  
  Suffix added to variable labels for instrumental variables (`fixest`
  models). `NULL` to add nothing.

- term_labels:

  (`list` or `vector`)  
  A named list or a named vector of custom term labels.

- interaction_sep:

  (`string`)  
  Separator for interaction terms.

- categorical_terms_pattern:

  ([`glue pattern`](https://glue.tidyverse.org/reference/glue.html))  
  A [glue pattern](https://glue.tidyverse.org/reference/glue.html) for
  labels of categorical terms with treatment or sum contrasts (see
  [`model_list_terms_levels()`](https://larmarange.github.io/broom.helpers/dev/reference/model_list_terms_levels.md)).

- relabel_poly:

  Should terms generated with
  [`stats::poly()`](https://rdrr.io/r/stats/poly.html) be relabeled?

- disambiguate_terms:

  (`logical`)  
  Should terms be disambiguated with
  [`tidy_disambiguate_terms()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_disambiguate_terms.md)?
  (default `TRUE`)

- disambiguate_sep:

  (`string`)  
  Separator for
  [`tidy_disambiguate_terms()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_disambiguate_terms.md).

- add_reference_rows:

  (`logical`)  
  Should reference rows be added?

- no_reference_row:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables for those no reference row should be added, when
  `add_reference_rows = TRUE`.

- add_pairwise_contrasts:

  (`logical`)  
  Apply
  [`tidy_add_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_pairwise_contrasts.md)?

- pairwise_variables:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to add pairwise contrasts.

- keep_model_terms:

  (`logical`)  
  Keep original model terms for variables where pairwise contrasts are
  added? (default is `FALSE`)

- pairwise_reverse:

  (`logical`)  
  Determines whether to use `"pairwise"` (if `TRUE`) or `"revpairwise"`
  (if `FALSE`), see
  [`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html).

- contrasts_adjust:

  (`string`)  
  Optional adjustment method when computing contrasts, see
  [`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html)
  (if `NULL`, use `emmeans` default).

- emmeans_args:

  (`list`)  
  List of additional parameter to pass to
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
  when computing pairwise contrasts.

- add_estimate_to_reference_rows:

  (`logical`)  
  Should an estimate value be added to reference rows?

- add_header_rows:

  (`logical`)  
  Should header rows be added?

- show_single_row:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables that should be displayed on a single row, when
  `add_header_rows` is `TRUE`.

- add_n:

  (`logical`)  
  Should the number of observations be added?

- intercept:

  (`logical`)  
  Should the intercept(s) be included?

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).
  See also
  [`all_continuous()`](https://larmarange.github.io/broom.helpers/dev/reference/select_helpers.md),
  [`all_categorical()`](https://larmarange.github.io/broom.helpers/dev/reference/select_helpers.md),
  [`all_dichotomous()`](https://larmarange.github.io/broom.helpers/dev/reference/select_helpers.md)
  and
  [`all_interaction()`](https://larmarange.github.io/broom.helpers/dev/reference/select_helpers.md).

- group_by:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  One or several variables to group by. Default is
  [`auto_group_by()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_group_by.md).
  Use `NULL` to force ungrouping.

- group_labels:

  (`string`)  
  An optional named vector of custom term labels.

- keep_model:

  (`logical`)  
  Should the model be kept as an attribute of the final result?

- tidy_post_fun:

  (`function`)  
  Custom function applied to the results at the end of
  `tidy_plus_plus()` (see note)

- quiet:

  (`logical`)  
  Whether `broom.helpers` should not return a message when requested
  output cannot be generated. Default is `FALSE`.

- strict:

  (`logical`)  
  Whether `broom.helpers` should return an error when requested output
  cannot be generated. Default is `FALSE`.

- ...:

  other arguments passed to `tidy_fun()`

## Note

`tidy_post_fun` is applied to the result at the end of
`tidy_plus_plus()` and receive only one argument (the result of
`tidy_plus_plus()`). However, if needed, the model is still attached to
the tibble as an attribute, even if `keep_model = FALSE`. Therefore, it
is possible to use
[`tidy_get_model()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_attach_model.md)
within `tidy_fun` if, for any reason, you need to access the source
model.

## See also

Other tidy_helpers:
[`tidy_add_coefficients_type()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_add_coefficients_type.md),
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
[`tidy_remove_intercept()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_remove_intercept.md),
[`tidy_select_variables()`](https://larmarange.github.io/broom.helpers/dev/reference/tidy_select_variables.md)

## Examples

``` r
# \donttest{
ex1 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris) |>
  tidy_plus_plus()
ex1
#> # A tibble: 4 × 17
#>   term              variable  var_label var_class var_type var_nlevels contrasts
#>   <chr>             <chr>     <chr>     <chr>     <chr>          <int> <chr>    
#> 1 Sepal.Width       Sepal.Wi… Sepal.Wi… numeric   continu…          NA NA       
#> 2 Speciessetosa     Species   Species   factor    categor…           3 contr.tr…
#> 3 Speciesversicolor Species   Species   factor    categor…           3 contr.tr…
#> 4 Speciesvirginica  Species   Species   factor    categor…           3 contr.tr…
#> # ℹ 10 more variables: contrasts_type <chr>, reference_row <lgl>, label <chr>,
#> #   n_obs <dbl>, estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>, conf.low <dbl>, conf.high <dbl>

df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    Survived = factor(Survived, c("No", "Yes"))
  ) |>
  labelled::set_variable_labels(
    Class = "Passenger's class",
    Sex = "Gender"
  )
ex2 <- glm(
  Survived ~ Class + Age * Sex,
  data = df, weights = df$n,
  family = binomial
) |>
  tidy_plus_plus(
    exponentiate = TRUE,
    add_reference_rows = FALSE,
    categorical_terms_pattern = "{level} / {reference_level}",
    add_n = TRUE
  )
ex2
#> # A tibble: 6 × 17
#>   term             variable var_label   var_class var_type var_nlevels contrasts
#>   <chr>            <chr>    <chr>       <chr>     <chr>          <int> <chr>    
#> 1 Class2nd         Class    Passenger'… character categor…           4 contr.tr…
#> 2 Class3rd         Class    Passenger'… character categor…           4 contr.tr…
#> 3 ClassCrew        Class    Passenger'… character categor…           4 contr.tr…
#> 4 AgeChild         Age      Age         character dichoto…           2 contr.tr…
#> 5 SexMale          Sex      Gender      character dichoto…           2 contr.tr…
#> 6 AgeChild:SexMale Age:Sex  Age * Gend… NA        interac…          NA NA       
#> # ℹ 10 more variables: contrasts_type <chr>, label <chr>, n_obs <dbl>,
#> #   n_event <dbl>, estimate <dbl>, std.error <dbl>, statistic <dbl>,
#> #   p.value <dbl>, conf.low <dbl>, conf.high <dbl>
# }
# \donttest{
ex3 <-
  glm(
    response ~ poly(age, 3) + stage + grade * trt,
    na.omit(gtsummary::trial),
    family = binomial,
    contrasts = list(
      stage = contr.treatment(4, base = 3),
      grade = contr.sum
    )
  ) |>
  tidy_plus_plus(
    exponentiate = TRUE,
    variable_labels = c(age = "Age (in years)"),
    add_header_rows = TRUE,
    show_single_row = all_dichotomous(),
    term_labels = c("poly(age, 3)3" = "Cubic age"),
    keep_model = TRUE
  )
ex3
#> # A tibble: 17 × 19
#>    term   variable var_label var_class var_type var_nlevels header_row contrasts
#>    <chr>  <chr>    <chr>     <chr>     <chr>          <int> <lgl>      <chr>    
#>  1 NA     age      Age (in … nmatrix.3 continu…          NA TRUE       NA       
#>  2 poly(… age      Age (in … nmatrix.3 continu…          NA FALSE      NA       
#>  3 poly(… age      Age (in … nmatrix.3 continu…          NA FALSE      NA       
#>  4 poly(… age      Age (in … nmatrix.3 continu…          NA FALSE      NA       
#>  5 NA     stage    T Stage   factor    categor…           4 TRUE       contr.tr…
#>  6 stage1 stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#>  7 stage2 stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#>  8 stage3 stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#>  9 stage4 stage    T Stage   factor    categor…           4 FALSE      contr.tr…
#> 10 NA     grade    Grade     factor    categor…           3 TRUE       contr.sum
#> 11 grade1 grade    Grade     factor    categor…           3 FALSE      contr.sum
#> 12 grade2 grade    Grade     factor    categor…           3 FALSE      contr.sum
#> 13 grade3 grade    Grade     factor    categor…           3 FALSE      contr.sum
#> 14 trtDr… trt      Chemothe… character dichoto…           2 NA         contr.tr…
#> 15 NA     grade:t… Grade * … NA        interac…          NA TRUE       NA       
#> 16 grade… grade:t… Grade * … NA        interac…          NA FALSE      NA       
#> 17 grade… grade:t… Grade * … NA        interac…          NA FALSE      NA       
#> # ℹ 11 more variables: contrasts_type <chr>, reference_row <lgl>, label <chr>,
#> #   n_obs <dbl>, n_event <dbl>, estimate <dbl>, std.error <dbl>,
#> #   statistic <dbl>, p.value <dbl>, conf.low <dbl>, conf.high <dbl>
# }
```
