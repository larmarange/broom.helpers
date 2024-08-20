## Test environments

* local R installation (windows 11): R 4.4.1
* macos-latest (on github actions): R-release
* windows-latest (on github actions): R-release
* ubuntu-latest  (on github actions): R-devel, R-release, R-oldrel-1

cf. https://github.com/larmarange/broom.helpers/actions/workflows/R-CMD-check.yaml

## R CMD check results

0 errors | 0 warnings | 0 note

## revdepcheck results

We checked 11 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 1 packages

Issues with CRAN packages are summarised below.

### Failed to check

* gtsummary (NA)

## recheck GitHub action

------- Check results summary ------
Check status summary:
                  OK
  Source packages  1
  Reverse depends 11

Check results summary:
broom.helpers ... OK
rdepends_GGally ... OK
rdepends_cardx ... OK
rdepends_dcurves ... OK
rdepends_ggstats ... OK
rdepends_gtreg ... OK
rdepends_gtsummary ... OK
rdepends_iNZightRegression ... OK
rdepends_logitr ... OK
rdepends_regport ... OK
rdepends_simstudy ... OK
rdepends_smdi ... OK

------- Check for regressions ------
No changes between old and new version

cf. https://github.com/larmarange/broom.helpers/actions/workflows/recheck.yml
