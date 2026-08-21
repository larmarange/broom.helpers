# Check a package installation status or minimum required version

The function `.assert_package()` checks whether a package is installed
and returns an error or `FALSE` if not available. If a package search is
provided, the function will check whether a minimum version of a package
is required. The function `.get_package_dependencies()` returns a tibble
with all dependencies of a specific package. Finally,
`.get_min_version_required()` will return, if any, the minimum version
of `pkg` required by `pkg_search`, `NULL` if no minimum version
required.

## Usage

``` r
.assert_package(pkg, fn = NULL, pkg_search = "broom.helpers", boolean = FALSE)

.get_package_dependencies(pkg_search = "broom.helpers")

.get_all_packages_dependencies(
  pkg_search = NULL,
  remove_duplicates = FALSE,
  lib.loc = NULL
)

.get_min_version_required(pkg, pkg_search = "broom.helpers")
```

## Arguments

- pkg:

  (`string`)  
  Name of the required package.

- fn:

  (`string`)  
  Name of the calling function from the user perspective. Used to write
  informative error messages.

- pkg_search:

  (`string`)  
  Name of the package the function will search for a minimum required
  version from.

- boolean:

  (`logical`)  
  Whether to return a `TRUE`/`FALSE`, rather than error when
  package/package version not available. Default is `FALSE`, which will
  return an error if `pkg` is not installed.

- remove_duplicates:

  (`logical`)  
  If several versions of a package are installed, should only the first
  one be returned?

- lib.loc:

  (`string`)  
  Location of `R` library trees to search through, see
  [`utils::installed.packages()`](https://rdrr.io/r/utils/installed.packages.html).

## Value

logical or error for `.assert_package()`, `NULL` or character with the
minimum version required for `.get_min_version_required()`, a tibble for
`.get_package_dependencies()`.

## Details

`get_all_packages_dependencies()` could be used to get the list of
dependencies of all installed packages.

## Examples

``` r
# \donttest{
.assert_package("broom", boolean = TRUE)
#> [1] TRUE
.get_package_dependencies()
#> # A tibble: 66 × 6
#>    pkg_search    pkg_search_version dependency_type pkg       version compare
#>    <chr>         <chr>              <chr>           <chr>     <chr>   <chr>  
#>  1 broom.helpers 1.23.0             Imports         broom     0.8     >=     
#>  2 broom.helpers 1.23.0             Imports         cards     NA      NA     
#>  3 broom.helpers 1.23.0             Imports         cli       NA      NA     
#>  4 broom.helpers 1.23.0             Imports         dplyr     1.1.0   >=     
#>  5 broom.helpers 1.23.0             Imports         labelled  NA      NA     
#>  6 broom.helpers 1.23.0             Imports         lifecycle NA      NA     
#>  7 broom.helpers 1.23.0             Imports         purrr     NA      NA     
#>  8 broom.helpers 1.23.0             Imports         rlang     1.0.1   >=     
#>  9 broom.helpers 1.23.0             Imports         stats     NA      NA     
#> 10 broom.helpers 1.23.0             Imports         stringr   NA      NA     
#> # ℹ 56 more rows
.get_min_version_required("brms")
#> Suggests 
#> "2.13.0" 
#> attr(,"compare")
#> [1] ">="
# }
```
