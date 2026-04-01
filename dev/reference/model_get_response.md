# Get model response

This function does not cover `lavaan` models (`NULL` is returned).

## Usage

``` r
model_get_response(model)

# Default S3 method
model_get_response(model)

# S3 method for class 'glm'
model_get_response(model)

# S3 method for class 'glmerMod'
model_get_response(model)

# S3 method for class 'model_fit'
model_get_response(model)
```

## Arguments

- model:

  (a model object, e.g. `glm`)  
  A model object.

## See also

Other model_helpers:
[`model_compute_terms_contributions()`](https://larmarange.github.io/broom.helpers/dev/reference/model_compute_terms_contributions.md),
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
  model_get_response()
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
#>                 110                 110                  93                 110 
#>   Hornet Sportabout             Valiant          Duster 360           Merc 240D 
#>                 175                 105                 245                  62 
#>            Merc 230            Merc 280           Merc 280C          Merc 450SE 
#>                  95                 123                 123                 180 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
#>                 180                 180                 205                 215 
#>   Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
#>                 230                  66                  52                  65 
#>       Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
#>                  97                 150                 150                 245 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
#>                 175                  66                  91                 113 
#>      Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
#>                 264                 175                 335                 109 

mod <- glm(
  response ~ stage * grade + trt,
  gtsummary::trial,
  family = binomial,
  contrasts = list(stage = contr.sum, grade = contr.treatment(3, 2), trt = "contr.SAS")
)
mod |> model_get_response()
#>   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
#>   0   1   0   1   1   0   0   0   0   0   0   1   0   0   0   0   1   0   0   0 
#>  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  37  38  39  40  41 
#>   0   0   0   0   0   0   1   0   0   1   1   0   0   1   0   0   1   0   0   0 
#>  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61 
#>   1   0   1   1   1   1   0   0   0   0   0   0   0   0   0   1   0   1   1   0 
#>  62  63  64  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82 
#>   0   0   0   0   1   1   0   0   1   0   0   0   1   0   0   0   0   0   0   1 
#>  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 
#>   0   0   0   1   0   1   0   0   0   0   1   1   1   0   0   0   0   0   0   0 
#> 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 
#>   0   0   1   0   0   0   0   1   1   0   0   1   1   0   0   0   0   1   0   0 
#> 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 140 141 142 143 144 
#>   0   0   1   1   0   0   0   1   0   0   0   1   0   1   1   1   1   1   0   1 
#> 145 146 147 148 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 
#>   0   0   1   0   1   0   0   0   0   0   0   0   0   0   1   0   0   1   1   0 
#> 166 167 168 169 170 171 172 173 174 175 176 177 178 180 181 182 183 184 185 186 
#>   0   0   1   0   1   0   1   0   1   1   1   0   1   0   1   0   0   1   0   1 
#> 187 188 189 190 191 192 193 194 196 197 198 199 200 
#>   0   0   0   0   0   0   0   0   0   1   1   0   0 
#> attr(,"label")
#> [1] "Tumor Response"

mod <- glm(
  Survived ~ Class * Age + Sex,
  data = Titanic |> as.data.frame(),
  weights = Freq,
  family = binomial
)
mod |> model_get_response()
#>  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 
#>  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  1  1  0  1  1  1  0  1  1 
#> 27 28 29 30 31 32 
#>  1  1  1  1  1  1 

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
mod <- glm(cbind(n_survived, n_dead) ~ Class * Age + Sex, data = d, family = binomial, y = FALSE)
mod |> model_get_response()
#>          1          2          3          4          5          6          7 
#> 0.97222222 1.00000000 0.32571429 1.00000000 0.86021505 1.00000000 0.08333333 
#>          8          9         10         11         12         13         14 
#> 1.00000000 0.46060606 0.45161290 0.16233766 0.27083333 0.86956522 0.00000000 
#>         15         16 
#> 0.22273782 0.00000000 
```
