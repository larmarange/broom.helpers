## Test environments

* local R installation: R 4.2.2
* mac OS (on github actions): R-release
* windows (on github actions): R-release, 3.6
* ubuntu 18.04 (on github actions): R-devel, R-release, R-oldrel-1

cf. https://github.com/larmarange/broom.helpers/actions

## R CMD check results

0 errors | 0 warnings | 0 note

## revdepcheck results

We checked 5 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 1 packages

Issues with CRAN packages are summarised below.

### Failed to check

* GGally (NA)

## Additional checks

* ubuntu 18.04 (on github actions): R-oldrel-2, R-oldrel-3, R-oldrel-4
    - with `_R_CHECK_FORCE_SUGGESTS_: false`
    - using historical snapshots of CRAN

cf. https://github.com/larmarange/broom.helpers/actions
