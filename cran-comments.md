## Test environments

* local R installation (windows 11): R 4.4.1
* macos-latest (on github actions): R-release
* windows-latest (on github actions): R-release
* ubuntu-latest  (on github actions): R-devel, R-release, R-oldrel-1

cf. https://github.com/larmarange/broom.helpers/actions/workflows/R-CMD-check.yaml

## R CMD check results

0 errors | 0 warnings | 0 note

## revdepcheck results

We checked 14 reverse dependencies (13 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 4 packages

Issues with CRAN packages are summarised below.

### Failed to check

* cardx     (NA)
* gtsummary (NA)
* mmrm      (NA)
* smdi      (NA)

## recheck GitHub action

------- Check results summary ------
Check status summary:
                  NOTE OK
  Source packages    0  1
  Reverse depends    1 13

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
rdepends_mmrm ... NOTE
* checking installed package size ... NOTE
rdepends_pubh ... OK
rdepends_regport ... OK
rdepends_simstudy ... OK
rdepends_smdi ... OK
rdepends_tidycmprsk ... OK

------- Check for regressions ------
No changes between old and new version

cf. https://github.com/larmarange/broom.helpers/actions/workflows/recheck.yml
