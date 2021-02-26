## Test environments

* local R installation: R 4.0.4
* mac OS (on github actions): R-release, R-oldrel
* windows (on github actions): R-devel, R-release, R-oldrel
* ubuntu 20.04 (on github actions): R-release
* ubuntu 18.04 (on github actions): R-devel, R-release
* ubuntu 16.04 (on github actions): R-release, R-oldrel

cf. https://github.com/larmarange/broom.helpers/actions

## R CMD check results

0 errors | 0 warnings | 0 note

## Comments about CRAN current check results

Following email sent by CRAN on 2021-02-26

https://cran.r-project.org/web/checks/check_results_broom.helpers.html

Version: 1.2.0
Check: Rd cross-references
Result: NOTE
    Undeclared package ‘glue’ in Rd xrefs
Flavor: r-devel-linux-x86_64-fedora-clang

**I has been fixed. glue has been added to Suggests**

Version: 1.2.0
Check: package dependencies
Result: NOTE
    Package suggested but not available for checking: ‘gtsummary’
Flavor: r-patched-solaris-x86

**gtsumary has been removed from CRAN at the time of test.**
**gtsummary is now back on CRAN.**

Version: 1.2.0
Check: examples
Result: ERROR
    Running examples in ‘broom.helpers-Ex.R’ failed

**Running examples failed to the absence of gtsummary**

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN 
and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


## Additional checks

* ubuntu 16.04 (on github actions): R 3.5, R 3.4
    - with `_R_CHECK_FORCE_SUGGESTS_: false`
    - using historical snapshots of CRAN
