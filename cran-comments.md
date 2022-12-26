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

 * We saw 1 new problems
 * We failed to check 0 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* ggstats
  checking tests ...
  
The developer of `ggtstats` is aware of the issue
(see https://github.com/larmarange/ggstats/pull/15) and a new release of `ggstats`
is planned once `broom.helpers` 1.11.0 on CRAN.
  
