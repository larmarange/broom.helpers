# Remove backticks around variable names

Remove backticks around variable names

## Usage

``` r
.clean_backticks(x, variable_names = x)
```

## Arguments

- x:

  (`string`)  
  A character vector to be cleaned.

- variable_names:

  (`string`)  
  Optional vector of variable names, could be obtained with
  [model_list_variables(only_variable =
  TRUE)](https://larmarange.github.io/broom.helpers/reference/model_list_variables.md),
  to properly take into account interaction only terms/variables.

## See also

Other other_helpers:
[`.escape_regex()`](https://larmarange.github.io/broom.helpers/reference/dot-escape_regex.md)
