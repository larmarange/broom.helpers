## Test environments

* local R installation: R 4.0.5
* mac OS (on github actions): R-release, R-oldrel
* windows (on github actions): R-devel, R-release, R-oldrel
* ubuntu 20.04 (on github actions): R-release
* ubuntu 18.04 (on github actions): R-devel, R-release
* ubuntu 16.04 (on github actions): R-release, R-oldrel

cf. https://github.com/larmarange/broom.helpers/actions

## R CMD check results

0 errors | 0 warnings | 0 note

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN 
and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


## Additional checks

* ubuntu 16.04 (on github actions): R 3.5, R 3.4
    - with `_R_CHECK_FORCE_SUGGESTS_: false`
    - using historical snapshots of CRAN

cf. https://github.com/larmarange/broom.helpers/actions
