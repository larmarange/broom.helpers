## Test environments

* local R installation: R 4.1.3
* mac OS (on github actions): R-release
* windows (on github actions): R-release, 3.6
* ubuntu 18.04 (on github actions): R-devel, R-release, R-oldrel-1, R-oldrel-2, R-oldrel-3

cf. https://github.com/larmarange/broom.helpers/actions

## R CMD check results

0 errors | 0 warnings | 0 note

## revdepcheck results

We checked 3 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

## Additional checks

* ubuntu 18.04 (on github actions): R 3.6, R 3.5
    - with `_R_CHECK_FORCE_SUGGESTS_: false`
    - using historical snapshots of CRAN

cf. https://github.com/larmarange/broom.helpers/actions
