# Get the assign attribute of model matrix of a model

Return the assign attribute attached to the object returned by
[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html).

## Usage

``` r
model_get_assign(model)

# Default S3 method
model_get_assign(model)

# S3 method for class 'vglm'
model_get_assign(model)

# S3 method for class 'svy_vglm'
model_get_assign(model)

# S3 method for class 'model_fit'
model_get_assign(model)

# S3 method for class 'fixest'
model_get_assign(model)
```

## Arguments

- model:

  (a model object, e.g. `glm`)  
  A model object.

## See also

[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)

Other model_helpers:
[`model_compute_terms_contributions()`](https://larmarange.github.io/broom.helpers/reference/model_compute_terms_contributions.md),
[`model_get_coefficients_type()`](https://larmarange.github.io/broom.helpers/reference/model_get_coefficients_type.md),
[`model_get_contrasts()`](https://larmarange.github.io/broom.helpers/reference/model_get_contrasts.md),
[`model_get_model()`](https://larmarange.github.io/broom.helpers/reference/model_get_model.md),
[`model_get_model_frame()`](https://larmarange.github.io/broom.helpers/reference/model_get_model_frame.md),
[`model_get_model_matrix()`](https://larmarange.github.io/broom.helpers/reference/model_get_model_matrix.md),
[`model_get_n()`](https://larmarange.github.io/broom.helpers/reference/model_get_n.md),
[`model_get_nlevels()`](https://larmarange.github.io/broom.helpers/reference/model_get_nlevels.md),
[`model_get_offset()`](https://larmarange.github.io/broom.helpers/reference/model_get_offset.md),
[`model_get_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/reference/model_get_pairwise_contrasts.md),
[`model_get_response()`](https://larmarange.github.io/broom.helpers/reference/model_get_response.md),
[`model_get_response_variable()`](https://larmarange.github.io/broom.helpers/reference/model_get_response_variable.md),
[`model_get_terms()`](https://larmarange.github.io/broom.helpers/reference/model_get_terms.md),
[`model_get_weights()`](https://larmarange.github.io/broom.helpers/reference/model_get_weights.md),
[`model_get_xlevels()`](https://larmarange.github.io/broom.helpers/reference/model_get_xlevels.md),
[`model_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/model_identify_variables.md),
[`model_list_contrasts()`](https://larmarange.github.io/broom.helpers/reference/model_list_contrasts.md),
[`model_list_higher_order_variables()`](https://larmarange.github.io/broom.helpers/reference/model_list_higher_order_variables.md),
[`model_list_terms_levels()`](https://larmarange.github.io/broom.helpers/reference/model_list_terms_levels.md),
[`model_list_variables()`](https://larmarange.github.io/broom.helpers/reference/model_list_variables.md)

## Examples

``` r
lm(hp ~ mpg + factor(cyl), mtcars) |>
  model_get_assign()
#> [1] 0 1 2 2
#> attr(,"model_matrix")
#>                     (Intercept)  mpg factor(cyl)6 factor(cyl)8
#> Mazda RX4                     1 21.0            1            0
#> Mazda RX4 Wag                 1 21.0            1            0
#> Datsun 710                    1 22.8            0            0
#> Hornet 4 Drive                1 21.4            1            0
#> Hornet Sportabout             1 18.7            0            1
#> Valiant                       1 18.1            1            0
#> Duster 360                    1 14.3            0            1
#> Merc 240D                     1 24.4            0            0
#> Merc 230                      1 22.8            0            0
#> Merc 280                      1 19.2            1            0
#> Merc 280C                     1 17.8            1            0
#> Merc 450SE                    1 16.4            0            1
#> Merc 450SL                    1 17.3            0            1
#> Merc 450SLC                   1 15.2            0            1
#> Cadillac Fleetwood            1 10.4            0            1
#> Lincoln Continental           1 10.4            0            1
#> Chrysler Imperial             1 14.7            0            1
#> Fiat 128                      1 32.4            0            0
#> Honda Civic                   1 30.4            0            0
#> Toyota Corolla                1 33.9            0            0
#> Toyota Corona                 1 21.5            0            0
#> Dodge Challenger              1 15.5            0            1
#> AMC Javelin                   1 15.2            0            1
#> Camaro Z28                    1 13.3            0            1
#> Pontiac Firebird              1 19.2            0            1
#> Fiat X1-9                     1 27.3            0            0
#> Porsche 914-2                 1 26.0            0            0
#> Lotus Europa                  1 30.4            0            0
#> Ford Pantera L                1 15.8            0            1
#> Ferrari Dino                  1 19.7            1            0
#> Maserati Bora                 1 15.0            0            1
#> Volvo 142E                    1 21.4            0            0
#> attr(,"model_matrix")attr(,"assign")
#> [1] 0 1 2 2
#> attr(,"model_matrix")attr(,"contrasts")
#> attr(,"model_matrix")attr(,"contrasts")$`factor(cyl)`
#> [1] "contr.treatment"
#> 
```
