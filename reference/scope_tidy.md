# Scoping a tidy tibble allowing to tidy select

This function uses the information from a model tidy tibble to generate
a data frame exposing the different variables of the model, data frame
that could be used for tidy selection. In addition, columns
`"var_type"`, `"var_class"` and `"contrasts_type"` are scoped and their
values are added as attributes to the data frame. For example, if
`var_type='continuous'` for variable `"age"`, then the attribute
`attr(.$age, 'gtsummary.var_type') <- 'continuous'` is set. That
attribute is then used in a selector like
[`all_continuous()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md).
Note: attributes are prefixed with `"gtsummary."` to be compatible with
selectors provided by `{gtsummary}`.

## Usage

``` r
scope_tidy(x, data = NULL)
```

## Arguments

- x:

  (`data.frame`)  
  A tidy tibble, with a `"variable"` column, as returned by
  [`tidy_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_identify_variables.md).

- data:

  (`data.frame`)  
  An optional data frame the attributes will be added to.

## Value

A data frame.

## Examples

``` r
mod <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)
tt <- mod |> tidy_and_attach() |> tidy_add_contrasts()

scope_tidy(tt) |> str()
#> tibble [1 × 4] (S3: tbl_df/tbl/data.frame)
#>  $ (Intercept)        : logi NA
#>   ..- attr(*, "gtsummary.var_type")= chr "intercept"
#>  $ Sepal.Width        : num NA
#>   ..- attr(*, "gtsummary.var_type")= chr "continuous"
#>   ..- attr(*, "gtsummary.var_class")= Named chr "numeric"
#>   .. ..- attr(*, "names")= chr "Sepal.Width"
#>  $ Species            : Factor w/ 0 levels: NA
#>   ..- attr(*, "gtsummary.var_type")= chr "categorical"
#>   ..- attr(*, "gtsummary.var_class")= Named chr "factor"
#>   .. ..- attr(*, "names")= chr "Species"
#>   ..- attr(*, "gtsummary.contrasts_type")= chr "treatment"
#>  $ Sepal.Width:Species: logi NA
#>   ..- attr(*, "gtsummary.var_type")= chr "interaction"
scope_tidy(tt, data = model_get_model_frame(mod)) |> str()
#> 'data.frame':    150 obs. of  3 variables:
#>  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
#>  $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
#>   ..- attr(*, "gtsummary.var_type")= chr "continuous"
#>   ..- attr(*, "gtsummary.var_class")= Named chr "numeric"
#>   .. ..- attr(*, "names")= chr "Sepal.Width"
#>  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
#>   ..- attr(*, "gtsummary.var_type")= chr "categorical"
#>   ..- attr(*, "gtsummary.var_class")= Named chr "factor"
#>   .. ..- attr(*, "names")= chr "Species"
#>   ..- attr(*, "gtsummary.contrasts_type")= chr "treatment"
#>  - attr(*, "terms")=Classes 'terms', 'formula'  language Sepal.Length ~ Sepal.Width * Species
#>   .. ..- attr(*, "variables")= language list(Sepal.Length, Sepal.Width, Species)
#>   .. ..- attr(*, "factors")= int [1:3, 1:3] 0 1 0 0 0 1 0 1 1
#>   .. .. ..- attr(*, "dimnames")=List of 2
#>   .. .. .. ..$ : chr [1:3] "Sepal.Length" "Sepal.Width" "Species"
#>   .. .. .. ..$ : chr [1:3] "Sepal.Width" "Species" "Sepal.Width:Species"
#>   .. ..- attr(*, "term.labels")= chr [1:3] "Sepal.Width" "Species" "Sepal.Width:Species"
#>   .. ..- attr(*, "order")= int [1:3] 1 1 2
#>   .. ..- attr(*, "intercept")= int 1
#>   .. ..- attr(*, "response")= int 1
#>   .. ..- attr(*, ".Environment")=<environment: 0x564b903b2380> 
#>   .. ..- attr(*, "predvars")= language list(Sepal.Length, Sepal.Width, Species)
#>   .. ..- attr(*, "dataClasses")= Named chr [1:3] "numeric" "numeric" "factor"
#>   .. .. ..- attr(*, "names")= chr [1:3] "Sepal.Length" "Sepal.Width" "Species"

scope_tidy(tt) |> dplyr::select(dplyr::starts_with("Se")) |> names()
#> [1] "Sepal.Width"         "Sepal.Width:Species"
scope_tidy(tt) |> dplyr::select(where(is.factor)) |> names()
#> [1] "Species"
scope_tidy(tt) |> dplyr::select(all_continuous()) |> names()
#> [1] "Sepal.Width"
scope_tidy(tt) |> dplyr::select(all_contrasts()) |> names()
#> [1] "Species"
scope_tidy(tt) |> dplyr::select(all_interaction()) |> names()
#> [1] "Sepal.Width:Species"
scope_tidy(tt) |> dplyr::select(all_intercepts()) |> names()
#> [1] "(Intercept)"
```
