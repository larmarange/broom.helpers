# Get sampling weights used by a model

This function does not cover `lavaan` models (`NULL` is returned).

## Usage

``` r
model_get_weights(model)

# Default S3 method
model_get_weights(model)

# S3 method for class 'svyglm'
model_get_weights(model)

# S3 method for class 'svrepglm'
model_get_weights(model)

# S3 method for class 'model_fit'
model_get_weights(model)

# S3 method for class 'svy_vglm'
model_get_weights(model)

# S3 method for class 'fixest'
model_get_weights(model)
```

## Arguments

- model:

  (a model object, e.g. `glm`)  
  A model object.

## Note

For class `svrepglm` objects (GLM on a survey object with replicate
weights), it will return the original sampling weights of the data, not
the replicate weights.

## See also

Other model_helpers:
[`model_compute_terms_contributions()`](https://larmarange.github.io/broom.helpers/reference/model_compute_terms_contributions.md),
[`model_get_assign()`](https://larmarange.github.io/broom.helpers/reference/model_get_assign.md),
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
[`model_get_xlevels()`](https://larmarange.github.io/broom.helpers/reference/model_get_xlevels.md),
[`model_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/model_identify_variables.md),
[`model_list_contrasts()`](https://larmarange.github.io/broom.helpers/reference/model_list_contrasts.md),
[`model_list_higher_order_variables()`](https://larmarange.github.io/broom.helpers/reference/model_list_higher_order_variables.md),
[`model_list_terms_levels()`](https://larmarange.github.io/broom.helpers/reference/model_list_terms_levels.md),
[`model_list_variables()`](https://larmarange.github.io/broom.helpers/reference/model_list_variables.md)

## Examples

``` r
mod <- lm(Sepal.Length ~ Sepal.Width, iris)
mod |> model_get_weights()
#>   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [38] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [75] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#> [112] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#> [149] 1 1

mod <- lm(hp ~ mpg + factor(cyl) + disp:hp, mtcars, weights = mtcars$gear)
mod |> model_get_weights()
#>  [1] 4 4 4 3 3 3 3 4 4 4 4 3 3 3 3 3 3 4 4 4 3 3 3 3 3 4 5 5 5 5 5 4

mod <- glm(
  response ~ stage * grade + trt,
  gtsummary::trial,
  family = binomial
)
mod |> model_get_weights()
#>   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
#>   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
#>  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  37  38  39  40  41 
#>   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
#>  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61 
#>   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
#>  62  63  64  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82 
#>   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
#>  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 
#>   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
#> 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 
#>   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
#> 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 140 141 142 143 144 
#>   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
#> 145 146 147 148 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 
#>   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
#> 166 167 168 169 170 171 172 173 174 175 176 177 178 180 181 182 183 184 185 186 
#>   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
#> 187 188 189 190 191 192 193 194 196 197 198 199 200 
#>   1   1   1   1   1   1   1   1   1   1   1   1   1 

mod <- glm(
  Survived ~ Class * Age + Sex,
  data = Titanic |> as.data.frame(),
  weights = Freq,
  family = binomial
)
mod |> model_get_weights()
#>   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
#>   0   0  35   0   0   0  17   0 118 154 387 670   4  13  89   3   5  11  13   0 
#>  21  22  23  24  25  26  27  28  29  30  31  32 
#>   1  13  14   0  57  14  75 192 140  80  76  20 

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
mod |> model_get_weights()
#>   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16 
#> 144   1 175   5  93  13 168  11 165  31 462  48  23   0 862   0 
```
