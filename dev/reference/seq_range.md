# Sequence generation between min and max

Sequence generation between min and max

## Usage

``` r
seq_range(x, length.out = 25)
```

## Arguments

- x:

  (`numeric`)  
  A numeric vector.

- length.out:

  (`integer`)  
  Desired length of the sequence (a positive integer).

## Value

a numeric vector

## Details

`seq_range(x, length.out)` is a shortcut for
`seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = length.out)`

## Examples

``` r
seq_range(iris$Petal.Length)
#>  [1] 1.000000 1.245833 1.491667 1.737500 1.983333 2.229167 2.475000 2.720833
#>  [9] 2.966667 3.212500 3.458333 3.704167 3.950000 4.195833 4.441667 4.687500
#> [17] 4.933333 5.179167 5.425000 5.670833 5.916667 6.162500 6.408333 6.654167
#> [25] 6.900000
```
