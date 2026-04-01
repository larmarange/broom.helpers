# Compute a matrix of terms contributions

Used for
[`model_get_n()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_n.md).
For each row and term, equal 1 if this row should be taken into account
in the estimate of the number of observations, 0 otherwise.

## Usage

``` r
model_compute_terms_contributions(model)

# Default S3 method
model_compute_terms_contributions(model)
```

## Arguments

- model:

  (a model object, e.g. `glm`)  
  A model object.

## Details

This function does not cover `lavaan` models (`NULL` is returned).

## See also

Other model_helpers:
[`model_get_assign()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_assign.md),
[`model_get_coefficients_type()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_coefficients_type.md),
[`model_get_contrasts()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_contrasts.md),
[`model_get_model()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_model.md),
[`model_get_model_frame()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_model_frame.md),
[`model_get_model_matrix()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_model_matrix.md),
[`model_get_n()`](https://larmarange.github.io/broom.helpers/dev/reference/model_get_n.md),
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
# \donttest{
mod <- lm(Sepal.Length ~ Sepal.Width, iris)
mod |> model_compute_terms_contributions()
#>     (Intercept) Sepal.Width
#> 1             1           1
#> 2             1           1
#> 3             1           1
#> 4             1           1
#> 5             1           1
#> 6             1           1
#> 7             1           1
#> 8             1           1
#> 9             1           1
#> 10            1           1
#> 11            1           1
#> 12            1           1
#> 13            1           1
#> 14            1           1
#> 15            1           1
#> 16            1           1
#> 17            1           1
#> 18            1           1
#> 19            1           1
#> 20            1           1
#> 21            1           1
#> 22            1           1
#> 23            1           1
#> 24            1           1
#> 25            1           1
#> 26            1           1
#> 27            1           1
#> 28            1           1
#> 29            1           1
#> 30            1           1
#> 31            1           1
#> 32            1           1
#> 33            1           1
#> 34            1           1
#> 35            1           1
#> 36            1           1
#> 37            1           1
#> 38            1           1
#> 39            1           1
#> 40            1           1
#> 41            1           1
#> 42            1           1
#> 43            1           1
#> 44            1           1
#> 45            1           1
#> 46            1           1
#> 47            1           1
#> 48            1           1
#> 49            1           1
#> 50            1           1
#> 51            1           1
#> 52            1           1
#> 53            1           1
#> 54            1           1
#> 55            1           1
#> 56            1           1
#> 57            1           1
#> 58            1           1
#> 59            1           1
#> 60            1           1
#> 61            1           1
#> 62            1           1
#> 63            1           1
#> 64            1           1
#> 65            1           1
#> 66            1           1
#> 67            1           1
#> 68            1           1
#> 69            1           1
#> 70            1           1
#> 71            1           1
#> 72            1           1
#> 73            1           1
#> 74            1           1
#> 75            1           1
#> 76            1           1
#> 77            1           1
#> 78            1           1
#> 79            1           1
#> 80            1           1
#> 81            1           1
#> 82            1           1
#> 83            1           1
#> 84            1           1
#> 85            1           1
#> 86            1           1
#> 87            1           1
#> 88            1           1
#> 89            1           1
#> 90            1           1
#> 91            1           1
#> 92            1           1
#> 93            1           1
#> 94            1           1
#> 95            1           1
#> 96            1           1
#> 97            1           1
#> 98            1           1
#> 99            1           1
#> 100           1           1
#> 101           1           1
#> 102           1           1
#> 103           1           1
#> 104           1           1
#> 105           1           1
#> 106           1           1
#> 107           1           1
#> 108           1           1
#> 109           1           1
#> 110           1           1
#> 111           1           1
#> 112           1           1
#> 113           1           1
#> 114           1           1
#> 115           1           1
#> 116           1           1
#> 117           1           1
#> 118           1           1
#> 119           1           1
#> 120           1           1
#> 121           1           1
#> 122           1           1
#> 123           1           1
#> 124           1           1
#> 125           1           1
#> 126           1           1
#> 127           1           1
#> 128           1           1
#> 129           1           1
#> 130           1           1
#> 131           1           1
#> 132           1           1
#> 133           1           1
#> 134           1           1
#> 135           1           1
#> 136           1           1
#> 137           1           1
#> 138           1           1
#> 139           1           1
#> 140           1           1
#> 141           1           1
#> 142           1           1
#> 143           1           1
#> 144           1           1
#> 145           1           1
#> 146           1           1
#> 147           1           1
#> 148           1           1
#> 149           1           1
#> 150           1           1

mod <- lm(hp ~ mpg + factor(cyl) + disp:hp, mtcars)
mod |> model_compute_terms_contributions()
#>                     (Intercept) mpg factor(cyl)6 factor(cyl)8 hp:disp
#> Mazda RX4                     1   1            1            0       1
#> Mazda RX4 Wag                 1   1            1            0       1
#> Datsun 710                    1   1            0            0       1
#> Hornet 4 Drive                1   1            1            0       1
#> Hornet Sportabout             1   1            0            1       1
#> Valiant                       1   1            1            0       1
#> Duster 360                    1   1            0            1       1
#> Merc 240D                     1   1            0            0       1
#> Merc 230                      1   1            0            0       1
#> Merc 280                      1   1            1            0       1
#> Merc 280C                     1   1            1            0       1
#> Merc 450SE                    1   1            0            1       1
#> Merc 450SL                    1   1            0            1       1
#> Merc 450SLC                   1   1            0            1       1
#> Cadillac Fleetwood            1   1            0            1       1
#> Lincoln Continental           1   1            0            1       1
#> Chrysler Imperial             1   1            0            1       1
#> Fiat 128                      1   1            0            0       1
#> Honda Civic                   1   1            0            0       1
#> Toyota Corolla                1   1            0            0       1
#> Toyota Corona                 1   1            0            0       1
#> Dodge Challenger              1   1            0            1       1
#> AMC Javelin                   1   1            0            1       1
#> Camaro Z28                    1   1            0            1       1
#> Pontiac Firebird              1   1            0            1       1
#> Fiat X1-9                     1   1            0            0       1
#> Porsche 914-2                 1   1            0            0       1
#> Lotus Europa                  1   1            0            0       1
#> Ford Pantera L                1   1            0            1       1
#> Ferrari Dino                  1   1            1            0       1
#> Maserati Bora                 1   1            0            1       1
#> Volvo 142E                    1   1            0            0       1
#>                     factor(cyl)4
#> Mazda RX4                      0
#> Mazda RX4 Wag                  0
#> Datsun 710                     1
#> Hornet 4 Drive                 0
#> Hornet Sportabout              0
#> Valiant                        0
#> Duster 360                     0
#> Merc 240D                      1
#> Merc 230                       1
#> Merc 280                       0
#> Merc 280C                      0
#> Merc 450SE                     0
#> Merc 450SL                     0
#> Merc 450SLC                    0
#> Cadillac Fleetwood             0
#> Lincoln Continental            0
#> Chrysler Imperial              0
#> Fiat 128                       1
#> Honda Civic                    1
#> Toyota Corolla                 1
#> Toyota Corona                  1
#> Dodge Challenger               0
#> AMC Javelin                    0
#> Camaro Z28                     0
#> Pontiac Firebird               0
#> Fiat X1-9                      1
#> Porsche 914-2                  1
#> Lotus Europa                   1
#> Ford Pantera L                 0
#> Ferrari Dino                   0
#> Maserati Bora                  0
#> Volvo 142E                     1

mod <- glm(
  response ~ stage * grade + trt,
  gtsummary::trial,
  family = binomial,
  contrasts = list(
    stage = contr.sum,
    grade = contr.treatment(3, 2),
    trt = "contr.SAS"
  )
)
mod |> model_compute_terms_contributions()
#>     (Intercept) stage1 stage2 stage3 grade1 grade3 trtDrug A stage1:grade1
#> 1             1      1      0      0      0      0         1             0
#> 2             1      0      1      0      1      0         0             0
#> 3             1      1      0      0      0      0         1             0
#> 4             1      0      0      1      0      1         1             0
#> 5             1      0      0      0      0      1         1             0
#> 6             1      0      0      0      1      0         0             0
#> 7             1      1      0      0      0      0         1             0
#> 8             1      1      0      0      1      0         1             1
#> 9             1      1      0      0      0      0         1             0
#> 10            1      0      0      1      1      0         0             0
#> 11            1      1      0      0      0      1         0             0
#> 12            1      0      0      1      1      0         0             0
#> 13            1      0      0      0      0      1         0             0
#> 14            1      0      0      0      1      0         0             0
#> 15            1      1      0      0      1      0         0             1
#> 16            1      0      0      0      0      1         0             0
#> 17            1      0      0      0      0      1         1             0
#> 18            1      0      1      0      0      0         0             0
#> 19            1      1      0      0      0      1         1             0
#> 20            1      1      0      0      0      0         1             0
#> 21            1      0      1      0      1      0         1             0
#> 22            1      0      1      0      0      0         1             0
#> 23            1      0      0      0      1      0         1             0
#> 24            1      0      0      0      1      0         1             0
#> 25            1      0      0      0      0      0         0             0
#> 26            1      0      1      0      0      1         0             0
#> 27            1      0      0      0      0      0         1             0
#> 28            1      1      0      0      0      0         0             0
#> 29            1      0      0      0      0      0         0             0
#> 30            1      0      0      1      1      0         1             0
#> 31            1      1      0      0      0      1         0             0
#> 32            1      0      1      0      0      0         1             0
#> 33            1      0      0      1      1      0         0             0
#> 34            1      0      0      1      1      0         1             0
#> 35            1      0      0      1      1      0         1             0
#> 37            1      1      0      0      0      0         0             0
#> 38            1      1      0      0      0      0         1             0
#> 39            1      0      0      0      0      1         0             0
#> 40            1      0      0      0      0      0         0             0
#> 41            1      0      0      1      0      0         0             0
#> 42            1      1      0      0      0      1         0             0
#> 43            1      0      0      0      0      1         1             0
#> 44            1      0      0      1      1      0         0             0
#> 45            1      0      0      0      0      1         1             0
#> 46            1      1      0      0      1      0         0             1
#> 47            1      1      0      0      0      1         1             0
#> 48            1      0      1      0      0      0         0             0
#> 49            1      1      0      0      0      0         0             0
#> 50            1      0      0      0      0      0         0             0
#> 51            1      1      0      0      0      1         0             0
#> 52            1      0      1      0      0      1         0             0
#> 53            1      0      1      0      0      1         0             0
#> 54            1      0      0      1      0      0         0             0
#> 55            1      0      0      1      0      1         0             0
#> 56            1      0      1      0      1      0         0             0
#> 57            1      0      0      0      0      1         0             0
#> 58            1      1      0      0      1      0         1             1
#> 59            1      0      0      0      0      0         0             0
#> 60            1      0      1      0      0      0         0             0
#> 61            1      0      0      0      0      1         1             0
#> 62            1      1      0      0      1      0         1             1
#> 63            1      0      0      0      0      0         1             0
#> 64            1      1      0      0      1      0         0             1
#> 66            1      1      0      0      1      0         0             1
#> 67            1      1      0      0      1      0         0             1
#> 68            1      1      0      0      1      0         0             1
#> 69            1      0      0      0      0      1         0             0
#> 70            1      1      0      0      0      0         1             0
#> 71            1      0      1      0      1      0         1             0
#> 72            1      0      1      0      0      1         1             0
#> 73            1      0      1      0      0      1         1             0
#> 74            1      1      0      0      0      1         0             0
#> 75            1      0      0      0      0      0         1             0
#> 76            1      0      0      0      0      1         1             0
#> 77            1      0      1      0      0      1         1             0
#> 78            1      0      1      0      1      0         0             0
#> 79            1      0      0      0      0      0         0             0
#> 80            1      0      0      0      1      0         1             0
#> 81            1      0      0      1      1      0         1             0
#> 82            1      0      1      0      1      0         1             0
#> 83            1      0      0      0      1      0         1             0
#> 84            1      0      0      1      0      1         0             0
#> 85            1      0      1      0      1      0         1             0
#> 86            1      0      0      0      0      1         1             0
#> 87            1      1      0      0      1      0         1             1
#> 88            1      0      1      0      1      0         1             0
#> 89            1      1      0      0      1      0         1             1
#> 90            1      0      0      0      0      1         0             0
#> 91            1      0      0      1      0      1         0             0
#> 92            1      0      0      1      0      1         0             0
#> 93            1      1      0      0      0      1         1             0
#> 94            1      0      0      1      0      1         0             0
#> 95            1      0      1      0      0      1         0             0
#> 96            1      0      0      1      0      1         0             0
#> 97            1      0      1      0      0      1         0             0
#> 98            1      0      1      0      0      0         0             0
#> 99            1      0      0      1      0      1         1             0
#> 100           1      0      0      0      1      0         1             0
#> 101           1      0      0      0      0      1         0             0
#> 102           1      0      0      1      1      0         0             0
#> 104           1      0      1      0      0      0         1             0
#> 105           1      0      0      1      1      0         1             0
#> 106           1      0      1      0      0      0         0             0
#> 107           1      0      1      0      0      0         0             0
#> 108           1      0      0      1      0      1         1             0
#> 109           1      0      1      0      1      0         1             0
#> 110           1      1      0      0      0      1         1             0
#> 111           1      1      0      0      0      1         0             0
#> 112           1      0      0      1      1      0         1             0
#> 113           1      0      0      0      1      0         1             0
#> 114           1      1      0      0      1      0         1             1
#> 115           1      0      0      0      0      0         0             0
#> 116           1      0      0      1      0      1         1             0
#> 117           1      0      0      1      0      1         0             0
#> 118           1      0      1      0      0      0         0             0
#> 119           1      0      0      0      1      0         1             0
#> 120           1      0      1      0      0      1         1             0
#> 121           1      1      0      0      0      0         0             0
#> 122           1      0      1      0      1      0         1             0
#> 123           1      1      0      0      0      0         0             0
#> 124           1      0      0      0      0      0         0             0
#> 125           1      0      0      1      0      0         1             0
#> 126           1      0      0      1      1      0         1             0
#> 127           1      0      0      1      0      1         1             0
#> 128           1      0      1      0      0      1         0             0
#> 129           1      1      0      0      0      0         1             0
#> 130           1      0      1      0      0      1         0             0
#> 131           1      1      0      0      1      0         0             1
#> 132           1      0      1      0      1      0         0             0
#> 133           1      0      1      0      1      0         0             0
#> 134           1      1      0      0      0      0         1             0
#> 135           1      0      0      1      0      0         0             0
#> 136           1      0      1      0      0      1         1             0
#> 137           1      1      0      0      0      0         1             0
#> 138           1      1      0      0      0      0         1             0
#> 140           1      0      0      1      0      0         0             0
#> 141           1      0      0      1      0      0         1             0
#> 142           1      0      0      0      1      0         0             0
#> 143           1      1      0      0      1      0         1             1
#> 144           1      0      1      0      0      1         0             0
#> 145           1      0      1      0      0      0         0             0
#> 146           1      1      0      0      0      0         1             0
#> 147           1      0      0      1      0      0         0             0
#> 148           1      0      0      0      0      0         1             0
#> 150           1      0      0      0      1      0         0             0
#> 151           1      0      1      0      1      0         1             0
#> 152           1      1      0      0      0      1         0             0
#> 153           1      0      1      0      0      0         1             0
#> 154           1      0      0      1      0      0         1             0
#> 155           1      1      0      0      1      0         0             1
#> 156           1      0      0      1      0      0         1             0
#> 157           1      0      0      0      1      0         0             0
#> 158           1      0      1      0      0      0         1             0
#> 159           1      0      1      0      0      1         0             0
#> 160           1      1      0      0      0      0         0             0
#> 161           1      0      1      0      1      0         0             0
#> 162           1      0      1      0      1      0         0             0
#> 163           1      0      0      1      1      0         0             0
#> 164           1      1      0      0      0      1         1             0
#> 165           1      0      1      0      1      0         0             0
#> 166           1      0      1      0      1      0         0             0
#> 167           1      1      0      0      0      0         1             0
#> 168           1      1      0      0      1      0         0             1
#> 169           1      0      0      1      1      0         1             0
#> 170           1      0      1      0      0      1         1             0
#> 171           1      0      0      1      0      1         0             0
#> 172           1      1      0      0      1      0         1             1
#> 173           1      1      0      0      0      1         1             0
#> 174           1      0      1      0      0      0         0             0
#> 175           1      0      0      0      1      0         0             0
#> 176           1      0      1      0      0      1         0             0
#> 177           1      0      0      0      1      0         1             0
#> 178           1      0      0      0      0      0         0             0
#> 180           1      0      0      1      1      0         1             0
#> 181           1      0      0      0      0      1         1             0
#> 182           1      0      0      1      1      0         1             0
#> 183           1      0      0      0      0      0         0             0
#> 184           1      0      0      0      1      0         0             0
#> 185           1      1      0      0      0      0         0             0
#> 186           1      1      0      0      0      0         0             0
#> 187           1      0      0      0      0      0         0             0
#> 188           1      0      0      0      0      1         1             0
#> 189           1      0      0      0      0      0         1             0
#> 190           1      1      0      0      0      0         1             0
#> 191           1      0      1      0      0      1         1             0
#> 192           1      0      1      0      0      0         1             0
#> 193           1      0      1      0      0      1         1             0
#> 194           1      0      0      0      0      1         0             0
#> 196           1      0      0      1      1      0         0             0
#> 197           1      0      1      0      0      0         1             0
#> 198           1      0      1      0      0      1         1             0
#> 199           1      0      0      0      0      1         1             0
#> 200           1      0      0      1      1      0         1             0
#>     stage2:grade1 stage3:grade1 stage1:grade3 stage2:grade3 stage3:grade3
#> 1               0             0             0             0             0
#> 2               1             0             0             0             0
#> 3               0             0             0             0             0
#> 4               0             0             0             0             1
#> 5               0             0             0             0             0
#> 6               0             0             0             0             0
#> 7               0             0             0             0             0
#> 8               0             0             0             0             0
#> 9               0             0             0             0             0
#> 10              0             1             0             0             0
#> 11              0             0             1             0             0
#> 12              0             1             0             0             0
#> 13              0             0             0             0             0
#> 14              0             0             0             0             0
#> 15              0             0             0             0             0
#> 16              0             0             0             0             0
#> 17              0             0             0             0             0
#> 18              0             0             0             0             0
#> 19              0             0             1             0             0
#> 20              0             0             0             0             0
#> 21              1             0             0             0             0
#> 22              0             0             0             0             0
#> 23              0             0             0             0             0
#> 24              0             0             0             0             0
#> 25              0             0             0             0             0
#> 26              0             0             0             1             0
#> 27              0             0             0             0             0
#> 28              0             0             0             0             0
#> 29              0             0             0             0             0
#> 30              0             1             0             0             0
#> 31              0             0             1             0             0
#> 32              0             0             0             0             0
#> 33              0             1             0             0             0
#> 34              0             1             0             0             0
#> 35              0             1             0             0             0
#> 37              0             0             0             0             0
#> 38              0             0             0             0             0
#> 39              0             0             0             0             0
#> 40              0             0             0             0             0
#> 41              0             0             0             0             0
#> 42              0             0             1             0             0
#> 43              0             0             0             0             0
#> 44              0             1             0             0             0
#> 45              0             0             0             0             0
#> 46              0             0             0             0             0
#> 47              0             0             1             0             0
#> 48              0             0             0             0             0
#> 49              0             0             0             0             0
#> 50              0             0             0             0             0
#> 51              0             0             1             0             0
#> 52              0             0             0             1             0
#> 53              0             0             0             1             0
#> 54              0             0             0             0             0
#> 55              0             0             0             0             1
#> 56              1             0             0             0             0
#> 57              0             0             0             0             0
#> 58              0             0             0             0             0
#> 59              0             0             0             0             0
#> 60              0             0             0             0             0
#> 61              0             0             0             0             0
#> 62              0             0             0             0             0
#> 63              0             0             0             0             0
#> 64              0             0             0             0             0
#> 66              0             0             0             0             0
#> 67              0             0             0             0             0
#> 68              0             0             0             0             0
#> 69              0             0             0             0             0
#> 70              0             0             0             0             0
#> 71              1             0             0             0             0
#> 72              0             0             0             1             0
#> 73              0             0             0             1             0
#> 74              0             0             1             0             0
#> 75              0             0             0             0             0
#> 76              0             0             0             0             0
#> 77              0             0             0             1             0
#> 78              1             0             0             0             0
#> 79              0             0             0             0             0
#> 80              0             0             0             0             0
#> 81              0             1             0             0             0
#> 82              1             0             0             0             0
#> 83              0             0             0             0             0
#> 84              0             0             0             0             1
#> 85              1             0             0             0             0
#> 86              0             0             0             0             0
#> 87              0             0             0             0             0
#> 88              1             0             0             0             0
#> 89              0             0             0             0             0
#> 90              0             0             0             0             0
#> 91              0             0             0             0             1
#> 92              0             0             0             0             1
#> 93              0             0             1             0             0
#> 94              0             0             0             0             1
#> 95              0             0             0             1             0
#> 96              0             0             0             0             1
#> 97              0             0             0             1             0
#> 98              0             0             0             0             0
#> 99              0             0             0             0             1
#> 100             0             0             0             0             0
#> 101             0             0             0             0             0
#> 102             0             1             0             0             0
#> 104             0             0             0             0             0
#> 105             0             1             0             0             0
#> 106             0             0             0             0             0
#> 107             0             0             0             0             0
#> 108             0             0             0             0             1
#> 109             1             0             0             0             0
#> 110             0             0             1             0             0
#> 111             0             0             1             0             0
#> 112             0             1             0             0             0
#> 113             0             0             0             0             0
#> 114             0             0             0             0             0
#> 115             0             0             0             0             0
#> 116             0             0             0             0             1
#> 117             0             0             0             0             1
#> 118             0             0             0             0             0
#> 119             0             0             0             0             0
#> 120             0             0             0             1             0
#> 121             0             0             0             0             0
#> 122             1             0             0             0             0
#> 123             0             0             0             0             0
#> 124             0             0             0             0             0
#> 125             0             0             0             0             0
#> 126             0             1             0             0             0
#> 127             0             0             0             0             1
#> 128             0             0             0             1             0
#> 129             0             0             0             0             0
#> 130             0             0             0             1             0
#> 131             0             0             0             0             0
#> 132             1             0             0             0             0
#> 133             1             0             0             0             0
#> 134             0             0             0             0             0
#> 135             0             0             0             0             0
#> 136             0             0             0             1             0
#> 137             0             0             0             0             0
#> 138             0             0             0             0             0
#> 140             0             0             0             0             0
#> 141             0             0             0             0             0
#> 142             0             0             0             0             0
#> 143             0             0             0             0             0
#> 144             0             0             0             1             0
#> 145             0             0             0             0             0
#> 146             0             0             0             0             0
#> 147             0             0             0             0             0
#> 148             0             0             0             0             0
#> 150             0             0             0             0             0
#> 151             1             0             0             0             0
#> 152             0             0             1             0             0
#> 153             0             0             0             0             0
#> 154             0             0             0             0             0
#> 155             0             0             0             0             0
#> 156             0             0             0             0             0
#> 157             0             0             0             0             0
#> 158             0             0             0             0             0
#> 159             0             0             0             1             0
#> 160             0             0             0             0             0
#> 161             1             0             0             0             0
#> 162             1             0             0             0             0
#> 163             0             1             0             0             0
#> 164             0             0             1             0             0
#> 165             1             0             0             0             0
#> 166             1             0             0             0             0
#> 167             0             0             0             0             0
#> 168             0             0             0             0             0
#> 169             0             1             0             0             0
#> 170             0             0             0             1             0
#> 171             0             0             0             0             1
#> 172             0             0             0             0             0
#> 173             0             0             1             0             0
#> 174             0             0             0             0             0
#> 175             0             0             0             0             0
#> 176             0             0             0             1             0
#> 177             0             0             0             0             0
#> 178             0             0             0             0             0
#> 180             0             1             0             0             0
#> 181             0             0             0             0             0
#> 182             0             1             0             0             0
#> 183             0             0             0             0             0
#> 184             0             0             0             0             0
#> 185             0             0             0             0             0
#> 186             0             0             0             0             0
#> 187             0             0             0             0             0
#> 188             0             0             0             0             0
#> 189             0             0             0             0             0
#> 190             0             0             0             0             0
#> 191             0             0             0             1             0
#> 192             0             0             0             0             0
#> 193             0             0             0             1             0
#> 194             0             0             0             0             0
#> 196             0             1             0             0             0
#> 197             0             0             0             0             0
#> 198             0             0             0             1             0
#> 199             0             0             0             0             0
#> 200             0             1             0             0             0
#>     stage4 grade2 trtDrug B
#> 1        0      1         0
#> 2        0      0         1
#> 3        0      1         0
#> 4        0      0         0
#> 5        1      0         0
#> 6        1      0         1
#> 7        0      1         0
#> 8        0      0         0
#> 9        0      1         0
#> 10       0      0         1
#> 11       0      0         1
#> 12       0      0         1
#> 13       1      0         1
#> 14       1      0         1
#> 15       0      0         1
#> 16       1      0         1
#> 17       1      0         0
#> 18       0      1         1
#> 19       0      0         0
#> 20       0      1         0
#> 21       0      0         0
#> 22       0      1         0
#> 23       1      0         0
#> 24       1      0         0
#> 25       1      1         1
#> 26       0      0         1
#> 27       1      1         0
#> 28       0      1         1
#> 29       1      1         1
#> 30       0      0         0
#> 31       0      0         1
#> 32       0      1         0
#> 33       0      0         1
#> 34       0      0         0
#> 35       0      0         0
#> 37       0      1         1
#> 38       0      1         0
#> 39       1      0         1
#> 40       1      1         1
#> 41       0      1         1
#> 42       0      0         1
#> 43       1      0         0
#> 44       0      0         1
#> 45       1      0         0
#> 46       0      0         1
#> 47       0      0         0
#> 48       0      1         1
#> 49       0      1         1
#> 50       1      1         1
#> 51       0      0         1
#> 52       0      0         1
#> 53       0      0         1
#> 54       0      1         1
#> 55       0      0         1
#> 56       0      0         1
#> 57       1      0         1
#> 58       0      0         0
#> 59       1      1         1
#> 60       0      1         1
#> 61       1      0         0
#> 62       0      0         0
#> 63       1      1         0
#> 64       0      0         1
#> 66       0      0         1
#> 67       0      0         1
#> 68       0      0         1
#> 69       1      0         1
#> 70       0      1         0
#> 71       0      0         0
#> 72       0      0         0
#> 73       0      0         0
#> 74       0      0         1
#> 75       1      1         0
#> 76       1      0         0
#> 77       0      0         0
#> 78       0      0         1
#> 79       1      1         1
#> 80       1      0         0
#> 81       0      0         0
#> 82       0      0         0
#> 83       1      0         0
#> 84       0      0         1
#> 85       0      0         0
#> 86       1      0         0
#> 87       0      0         0
#> 88       0      0         0
#> 89       0      0         0
#> 90       1      0         1
#> 91       0      0         1
#> 92       0      0         1
#> 93       0      0         0
#> 94       0      0         1
#> 95       0      0         1
#> 96       0      0         1
#> 97       0      0         1
#> 98       0      1         1
#> 99       0      0         0
#> 100      1      0         0
#> 101      1      0         1
#> 102      0      0         1
#> 104      0      1         0
#> 105      0      0         0
#> 106      0      1         1
#> 107      0      1         1
#> 108      0      0         0
#> 109      0      0         0
#> 110      0      0         0
#> 111      0      0         1
#> 112      0      0         0
#> 113      1      0         0
#> 114      0      0         0
#> 115      1      1         1
#> 116      0      0         0
#> 117      0      0         1
#> 118      0      1         1
#> 119      1      0         0
#> 120      0      0         0
#> 121      0      1         1
#> 122      0      0         0
#> 123      0      1         1
#> 124      1      1         1
#> 125      0      1         0
#> 126      0      0         0
#> 127      0      0         0
#> 128      0      0         1
#> 129      0      1         0
#> 130      0      0         1
#> 131      0      0         1
#> 132      0      0         1
#> 133      0      0         1
#> 134      0      1         0
#> 135      0      1         1
#> 136      0      0         0
#> 137      0      1         0
#> 138      0      1         0
#> 140      0      1         1
#> 141      0      1         0
#> 142      1      0         1
#> 143      0      0         0
#> 144      0      0         1
#> 145      0      1         1
#> 146      0      1         0
#> 147      0      1         1
#> 148      1      1         0
#> 150      1      0         1
#> 151      0      0         0
#> 152      0      0         1
#> 153      0      1         0
#> 154      0      1         0
#> 155      0      0         1
#> 156      0      1         0
#> 157      1      0         1
#> 158      0      1         0
#> 159      0      0         1
#> 160      0      1         1
#> 161      0      0         1
#> 162      0      0         1
#> 163      0      0         1
#> 164      0      0         0
#> 165      0      0         1
#> 166      0      0         1
#> 167      0      1         0
#> 168      0      0         1
#> 169      0      0         0
#> 170      0      0         0
#> 171      0      0         1
#> 172      0      0         0
#> 173      0      0         0
#> 174      0      1         1
#> 175      1      0         1
#> 176      0      0         1
#> 177      1      0         0
#> 178      1      1         1
#> 180      0      0         0
#> 181      1      0         0
#> 182      0      0         0
#> 183      1      1         1
#> 184      1      0         1
#> 185      0      1         1
#> 186      0      1         1
#> 187      1      1         1
#> 188      1      0         0
#> 189      1      1         0
#> 190      0      1         0
#> 191      0      0         0
#> 192      0      1         0
#> 193      0      0         0
#> 194      1      0         1
#> 196      0      0         1
#> 197      0      1         0
#> 198      0      0         0
#> 199      1      0         0
#> 200      0      0         0

mod <- glm(
  response ~ stage * trt,
  gtsummary::trial,
  family = binomial,
  contrasts = list(stage = contr.poly)
)
mod |> model_compute_terms_contributions()
#>     (Intercept) stage.L stage.Q stage.C trtDrug B stage.L:trtDrug B
#> 1             1       1       1       1         0                 0
#> 2             1       1       1       1         1                 1
#> 3             1       1       1       1         0                 0
#> 4             1       1       1       1         0                 0
#> 5             1       1       1       1         0                 0
#> 6             1       1       1       1         1                 1
#> 7             1       1       1       1         0                 0
#> 8             1       1       1       1         0                 0
#> 9             1       1       1       1         0                 0
#> 10            1       1       1       1         1                 1
#> 11            1       1       1       1         1                 1
#> 12            1       1       1       1         1                 1
#> 13            1       1       1       1         1                 1
#> 14            1       1       1       1         1                 1
#> 15            1       1       1       1         1                 1
#> 16            1       1       1       1         1                 1
#> 17            1       1       1       1         0                 0
#> 18            1       1       1       1         1                 1
#> 19            1       1       1       1         0                 0
#> 20            1       1       1       1         0                 0
#> 21            1       1       1       1         0                 0
#> 22            1       1       1       1         0                 0
#> 23            1       1       1       1         0                 0
#> 24            1       1       1       1         0                 0
#> 25            1       1       1       1         1                 1
#> 26            1       1       1       1         1                 1
#> 27            1       1       1       1         0                 0
#> 28            1       1       1       1         1                 1
#> 29            1       1       1       1         1                 1
#> 30            1       1       1       1         0                 0
#> 31            1       1       1       1         1                 1
#> 32            1       1       1       1         0                 0
#> 33            1       1       1       1         1                 1
#> 34            1       1       1       1         0                 0
#> 35            1       1       1       1         0                 0
#> 37            1       1       1       1         1                 1
#> 38            1       1       1       1         0                 0
#> 39            1       1       1       1         1                 1
#> 40            1       1       1       1         1                 1
#> 41            1       1       1       1         1                 1
#> 42            1       1       1       1         1                 1
#> 43            1       1       1       1         0                 0
#> 44            1       1       1       1         1                 1
#> 45            1       1       1       1         0                 0
#> 46            1       1       1       1         1                 1
#> 47            1       1       1       1         0                 0
#> 48            1       1       1       1         1                 1
#> 49            1       1       1       1         1                 1
#> 50            1       1       1       1         1                 1
#> 51            1       1       1       1         1                 1
#> 52            1       1       1       1         1                 1
#> 53            1       1       1       1         1                 1
#> 54            1       1       1       1         1                 1
#> 55            1       1       1       1         1                 1
#> 56            1       1       1       1         1                 1
#> 57            1       1       1       1         1                 1
#> 58            1       1       1       1         0                 0
#> 59            1       1       1       1         1                 1
#> 60            1       1       1       1         1                 1
#> 61            1       1       1       1         0                 0
#> 62            1       1       1       1         0                 0
#> 63            1       1       1       1         0                 0
#> 64            1       1       1       1         1                 1
#> 66            1       1       1       1         1                 1
#> 67            1       1       1       1         1                 1
#> 68            1       1       1       1         1                 1
#> 69            1       1       1       1         1                 1
#> 70            1       1       1       1         0                 0
#> 71            1       1       1       1         0                 0
#> 72            1       1       1       1         0                 0
#> 73            1       1       1       1         0                 0
#> 74            1       1       1       1         1                 1
#> 75            1       1       1       1         0                 0
#> 76            1       1       1       1         0                 0
#> 77            1       1       1       1         0                 0
#> 78            1       1       1       1         1                 1
#> 79            1       1       1       1         1                 1
#> 80            1       1       1       1         0                 0
#> 81            1       1       1       1         0                 0
#> 82            1       1       1       1         0                 0
#> 83            1       1       1       1         0                 0
#> 84            1       1       1       1         1                 1
#> 85            1       1       1       1         0                 0
#> 86            1       1       1       1         0                 0
#> 87            1       1       1       1         0                 0
#> 88            1       1       1       1         0                 0
#> 89            1       1       1       1         0                 0
#> 90            1       1       1       1         1                 1
#> 91            1       1       1       1         1                 1
#> 92            1       1       1       1         1                 1
#> 93            1       1       1       1         0                 0
#> 94            1       1       1       1         1                 1
#> 95            1       1       1       1         1                 1
#> 96            1       1       1       1         1                 1
#> 97            1       1       1       1         1                 1
#> 98            1       1       1       1         1                 1
#> 99            1       1       1       1         0                 0
#> 100           1       1       1       1         0                 0
#> 101           1       1       1       1         1                 1
#> 102           1       1       1       1         1                 1
#> 104           1       1       1       1         0                 0
#> 105           1       1       1       1         0                 0
#> 106           1       1       1       1         1                 1
#> 107           1       1       1       1         1                 1
#> 108           1       1       1       1         0                 0
#> 109           1       1       1       1         0                 0
#> 110           1       1       1       1         0                 0
#> 111           1       1       1       1         1                 1
#> 112           1       1       1       1         0                 0
#> 113           1       1       1       1         0                 0
#> 114           1       1       1       1         0                 0
#> 115           1       1       1       1         1                 1
#> 116           1       1       1       1         0                 0
#> 117           1       1       1       1         1                 1
#> 118           1       1       1       1         1                 1
#> 119           1       1       1       1         0                 0
#> 120           1       1       1       1         0                 0
#> 121           1       1       1       1         1                 1
#> 122           1       1       1       1         0                 0
#> 123           1       1       1       1         1                 1
#> 124           1       1       1       1         1                 1
#> 125           1       1       1       1         0                 0
#> 126           1       1       1       1         0                 0
#> 127           1       1       1       1         0                 0
#> 128           1       1       1       1         1                 1
#> 129           1       1       1       1         0                 0
#> 130           1       1       1       1         1                 1
#> 131           1       1       1       1         1                 1
#> 132           1       1       1       1         1                 1
#> 133           1       1       1       1         1                 1
#> 134           1       1       1       1         0                 0
#> 135           1       1       1       1         1                 1
#> 136           1       1       1       1         0                 0
#> 137           1       1       1       1         0                 0
#> 138           1       1       1       1         0                 0
#> 140           1       1       1       1         1                 1
#> 141           1       1       1       1         0                 0
#> 142           1       1       1       1         1                 1
#> 143           1       1       1       1         0                 0
#> 144           1       1       1       1         1                 1
#> 145           1       1       1       1         1                 1
#> 146           1       1       1       1         0                 0
#> 147           1       1       1       1         1                 1
#> 148           1       1       1       1         0                 0
#> 150           1       1       1       1         1                 1
#> 151           1       1       1       1         0                 0
#> 152           1       1       1       1         1                 1
#> 153           1       1       1       1         0                 0
#> 154           1       1       1       1         0                 0
#> 155           1       1       1       1         1                 1
#> 156           1       1       1       1         0                 0
#> 157           1       1       1       1         1                 1
#> 158           1       1       1       1         0                 0
#> 159           1       1       1       1         1                 1
#> 160           1       1       1       1         1                 1
#> 161           1       1       1       1         1                 1
#> 162           1       1       1       1         1                 1
#> 163           1       1       1       1         1                 1
#> 164           1       1       1       1         0                 0
#> 165           1       1       1       1         1                 1
#> 166           1       1       1       1         1                 1
#> 167           1       1       1       1         0                 0
#> 168           1       1       1       1         1                 1
#> 169           1       1       1       1         0                 0
#> 170           1       1       1       1         0                 0
#> 171           1       1       1       1         1                 1
#> 172           1       1       1       1         0                 0
#> 173           1       1       1       1         0                 0
#> 174           1       1       1       1         1                 1
#> 175           1       1       1       1         1                 1
#> 176           1       1       1       1         1                 1
#> 177           1       1       1       1         0                 0
#> 178           1       1       1       1         1                 1
#> 180           1       1       1       1         0                 0
#> 181           1       1       1       1         0                 0
#> 182           1       1       1       1         0                 0
#> 183           1       1       1       1         1                 1
#> 184           1       1       1       1         1                 1
#> 185           1       1       1       1         1                 1
#> 186           1       1       1       1         1                 1
#> 187           1       1       1       1         1                 1
#> 188           1       1       1       1         0                 0
#> 189           1       1       1       1         0                 0
#> 190           1       1       1       1         0                 0
#> 191           1       1       1       1         0                 0
#> 192           1       1       1       1         0                 0
#> 193           1       1       1       1         0                 0
#> 194           1       1       1       1         1                 1
#> 196           1       1       1       1         1                 1
#> 197           1       1       1       1         0                 0
#> 198           1       1       1       1         0                 0
#> 199           1       1       1       1         0                 0
#> 200           1       1       1       1         0                 0
#>     stage.Q:trtDrug B stage.C:trtDrug B trtDrug A
#> 1                   0                 0         1
#> 2                   1                 1         0
#> 3                   0                 0         1
#> 4                   0                 0         1
#> 5                   0                 0         1
#> 6                   1                 1         0
#> 7                   0                 0         1
#> 8                   0                 0         1
#> 9                   0                 0         1
#> 10                  1                 1         0
#> 11                  1                 1         0
#> 12                  1                 1         0
#> 13                  1                 1         0
#> 14                  1                 1         0
#> 15                  1                 1         0
#> 16                  1                 1         0
#> 17                  0                 0         1
#> 18                  1                 1         0
#> 19                  0                 0         1
#> 20                  0                 0         1
#> 21                  0                 0         1
#> 22                  0                 0         1
#> 23                  0                 0         1
#> 24                  0                 0         1
#> 25                  1                 1         0
#> 26                  1                 1         0
#> 27                  0                 0         1
#> 28                  1                 1         0
#> 29                  1                 1         0
#> 30                  0                 0         1
#> 31                  1                 1         0
#> 32                  0                 0         1
#> 33                  1                 1         0
#> 34                  0                 0         1
#> 35                  0                 0         1
#> 37                  1                 1         0
#> 38                  0                 0         1
#> 39                  1                 1         0
#> 40                  1                 1         0
#> 41                  1                 1         0
#> 42                  1                 1         0
#> 43                  0                 0         1
#> 44                  1                 1         0
#> 45                  0                 0         1
#> 46                  1                 1         0
#> 47                  0                 0         1
#> 48                  1                 1         0
#> 49                  1                 1         0
#> 50                  1                 1         0
#> 51                  1                 1         0
#> 52                  1                 1         0
#> 53                  1                 1         0
#> 54                  1                 1         0
#> 55                  1                 1         0
#> 56                  1                 1         0
#> 57                  1                 1         0
#> 58                  0                 0         1
#> 59                  1                 1         0
#> 60                  1                 1         0
#> 61                  0                 0         1
#> 62                  0                 0         1
#> 63                  0                 0         1
#> 64                  1                 1         0
#> 66                  1                 1         0
#> 67                  1                 1         0
#> 68                  1                 1         0
#> 69                  1                 1         0
#> 70                  0                 0         1
#> 71                  0                 0         1
#> 72                  0                 0         1
#> 73                  0                 0         1
#> 74                  1                 1         0
#> 75                  0                 0         1
#> 76                  0                 0         1
#> 77                  0                 0         1
#> 78                  1                 1         0
#> 79                  1                 1         0
#> 80                  0                 0         1
#> 81                  0                 0         1
#> 82                  0                 0         1
#> 83                  0                 0         1
#> 84                  1                 1         0
#> 85                  0                 0         1
#> 86                  0                 0         1
#> 87                  0                 0         1
#> 88                  0                 0         1
#> 89                  0                 0         1
#> 90                  1                 1         0
#> 91                  1                 1         0
#> 92                  1                 1         0
#> 93                  0                 0         1
#> 94                  1                 1         0
#> 95                  1                 1         0
#> 96                  1                 1         0
#> 97                  1                 1         0
#> 98                  1                 1         0
#> 99                  0                 0         1
#> 100                 0                 0         1
#> 101                 1                 1         0
#> 102                 1                 1         0
#> 104                 0                 0         1
#> 105                 0                 0         1
#> 106                 1                 1         0
#> 107                 1                 1         0
#> 108                 0                 0         1
#> 109                 0                 0         1
#> 110                 0                 0         1
#> 111                 1                 1         0
#> 112                 0                 0         1
#> 113                 0                 0         1
#> 114                 0                 0         1
#> 115                 1                 1         0
#> 116                 0                 0         1
#> 117                 1                 1         0
#> 118                 1                 1         0
#> 119                 0                 0         1
#> 120                 0                 0         1
#> 121                 1                 1         0
#> 122                 0                 0         1
#> 123                 1                 1         0
#> 124                 1                 1         0
#> 125                 0                 0         1
#> 126                 0                 0         1
#> 127                 0                 0         1
#> 128                 1                 1         0
#> 129                 0                 0         1
#> 130                 1                 1         0
#> 131                 1                 1         0
#> 132                 1                 1         0
#> 133                 1                 1         0
#> 134                 0                 0         1
#> 135                 1                 1         0
#> 136                 0                 0         1
#> 137                 0                 0         1
#> 138                 0                 0         1
#> 140                 1                 1         0
#> 141                 0                 0         1
#> 142                 1                 1         0
#> 143                 0                 0         1
#> 144                 1                 1         0
#> 145                 1                 1         0
#> 146                 0                 0         1
#> 147                 1                 1         0
#> 148                 0                 0         1
#> 150                 1                 1         0
#> 151                 0                 0         1
#> 152                 1                 1         0
#> 153                 0                 0         1
#> 154                 0                 0         1
#> 155                 1                 1         0
#> 156                 0                 0         1
#> 157                 1                 1         0
#> 158                 0                 0         1
#> 159                 1                 1         0
#> 160                 1                 1         0
#> 161                 1                 1         0
#> 162                 1                 1         0
#> 163                 1                 1         0
#> 164                 0                 0         1
#> 165                 1                 1         0
#> 166                 1                 1         0
#> 167                 0                 0         1
#> 168                 1                 1         0
#> 169                 0                 0         1
#> 170                 0                 0         1
#> 171                 1                 1         0
#> 172                 0                 0         1
#> 173                 0                 0         1
#> 174                 1                 1         0
#> 175                 1                 1         0
#> 176                 1                 1         0
#> 177                 0                 0         1
#> 178                 1                 1         0
#> 180                 0                 0         1
#> 181                 0                 0         1
#> 182                 0                 0         1
#> 183                 1                 1         0
#> 184                 1                 1         0
#> 185                 1                 1         0
#> 186                 1                 1         0
#> 187                 1                 1         0
#> 188                 0                 0         1
#> 189                 0                 0         1
#> 190                 0                 0         1
#> 191                 0                 0         1
#> 192                 0                 0         1
#> 193                 0                 0         1
#> 194                 1                 1         0
#> 196                 1                 1         0
#> 197                 0                 0         1
#> 198                 0                 0         1
#> 199                 0                 0         1
#> 200                 0                 0         1

mod <- glm(
  Survived ~ Class * Age + Sex,
  data = Titanic |> as.data.frame(),
  weights = Freq, family = binomial
)
mod |> model_compute_terms_contributions()
#>    (Intercept) Class2nd Class3rd ClassCrew AgeAdult SexFemale Class2nd:AgeAdult
#> 1            1        0        0         0        0         0                 0
#> 2            1        1        0         0        0         0                 0
#> 3            1        0        1         0        0         0                 0
#> 4            1        0        0         1        0         0                 0
#> 5            1        0        0         0        0         1                 0
#> 6            1        1        0         0        0         1                 0
#> 7            1        0        1         0        0         1                 0
#> 8            1        0        0         1        0         1                 0
#> 9            1        0        0         0        1         0                 0
#> 10           1        1        0         0        1         0                 1
#> 11           1        0        1         0        1         0                 0
#> 12           1        0        0         1        1         0                 0
#> 13           1        0        0         0        1         1                 0
#> 14           1        1        0         0        1         1                 1
#> 15           1        0        1         0        1         1                 0
#> 16           1        0        0         1        1         1                 0
#> 17           1        0        0         0        0         0                 0
#> 18           1        1        0         0        0         0                 0
#> 19           1        0        1         0        0         0                 0
#> 20           1        0        0         1        0         0                 0
#> 21           1        0        0         0        0         1                 0
#> 22           1        1        0         0        0         1                 0
#> 23           1        0        1         0        0         1                 0
#> 24           1        0        0         1        0         1                 0
#> 25           1        0        0         0        1         0                 0
#> 26           1        1        0         0        1         0                 1
#> 27           1        0        1         0        1         0                 0
#> 28           1        0        0         1        1         0                 0
#> 29           1        0        0         0        1         1                 0
#> 30           1        1        0         0        1         1                 1
#> 31           1        0        1         0        1         1                 0
#> 32           1        0        0         1        1         1                 0
#>    Class3rd:AgeAdult ClassCrew:AgeAdult Class1st AgeChild SexMale
#> 1                  0                  0        1        1       1
#> 2                  0                  0        0        1       1
#> 3                  0                  0        0        1       1
#> 4                  0                  0        0        1       1
#> 5                  0                  0        1        1       0
#> 6                  0                  0        0        1       0
#> 7                  0                  0        0        1       0
#> 8                  0                  0        0        1       0
#> 9                  0                  0        1        0       1
#> 10                 0                  0        0        0       1
#> 11                 1                  0        0        0       1
#> 12                 0                  1        0        0       1
#> 13                 0                  0        1        0       0
#> 14                 0                  0        0        0       0
#> 15                 1                  0        0        0       0
#> 16                 0                  1        0        0       0
#> 17                 0                  0        1        1       1
#> 18                 0                  0        0        1       1
#> 19                 0                  0        0        1       1
#> 20                 0                  0        0        1       1
#> 21                 0                  0        1        1       0
#> 22                 0                  0        0        1       0
#> 23                 0                  0        0        1       0
#> 24                 0                  0        0        1       0
#> 25                 0                  0        1        0       1
#> 26                 0                  0        0        0       1
#> 27                 1                  0        0        0       1
#> 28                 0                  1        0        0       1
#> 29                 0                  0        1        0       0
#> 30                 0                  0        0        0       0
#> 31                 1                  0        0        0       0
#> 32                 0                  1        0        0       0

d <- dplyr::as_tibble(Titanic) |>
  dplyr::group_by(Class, Sex, Age) |>
  dplyr::summarise(
    n_survived = sum(n * (Survived == "Yes")),
    n_dead = sum(n * (Survived == "No"))
  )
#> `summarise()` has regrouped the output.
#> ℹ Summaries were computed grouped by Class, Sex, and Age.
#> ℹ Output is grouped by Class and Sex.
#> ℹ Use `summarise(.groups = "drop_last")` to silence this message.
#> ℹ Use `summarise(.by = c(Class, Sex, Age))` for per-operation grouping
#>   (`?dplyr::dplyr_by`) instead.
mod <- glm(cbind(n_survived, n_dead) ~ Class * Age + Sex, data = d, family = binomial)
mod |> model_compute_terms_contributions()
#>    (Intercept) Class2nd Class3rd ClassCrew AgeChild SexMale Class2nd:AgeChild
#> 1            1        0        0         0        0       0                 0
#> 2            1        0        0         0        1       0                 0
#> 3            1        0        0         0        0       1                 0
#> 4            1        0        0         0        1       1                 0
#> 5            1        1        0         0        0       0                 0
#> 6            1        1        0         0        1       0                 1
#> 7            1        1        0         0        0       1                 0
#> 8            1        1        0         0        1       1                 1
#> 9            1        0        1         0        0       0                 0
#> 10           1        0        1         0        1       0                 0
#> 11           1        0        1         0        0       1                 0
#> 12           1        0        1         0        1       1                 0
#> 13           1        0        0         1        0       0                 0
#> 14           1        0        0         1        1       0                 0
#> 15           1        0        0         1        0       1                 0
#> 16           1        0        0         1        1       1                 0
#>    Class3rd:AgeChild ClassCrew:AgeChild Class1st AgeAdult SexFemale
#> 1                  0                  0        1        1         1
#> 2                  0                  0        1        0         1
#> 3                  0                  0        1        1         0
#> 4                  0                  0        1        0         0
#> 5                  0                  0        0        1         1
#> 6                  0                  0        0        0         1
#> 7                  0                  0        0        1         0
#> 8                  0                  0        0        0         0
#> 9                  0                  0        0        1         1
#> 10                 1                  0        0        0         1
#> 11                 0                  0        0        1         0
#> 12                 1                  0        0        0         0
#> 13                 0                  0        0        1         1
#> 14                 0                  1        0        0         1
#> 15                 0                  0        0        1         0
#> 16                 0                  1        0        0         0
# }
```
