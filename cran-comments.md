## Test environments

* local R installation (windows 11): R 4.5.0
* macos-latest (on github actions): R-release
* windows-latest (on github actions): R-release
* ubuntu-latest  (on github actions): R-devel, R-release, R-oldrel-1

cf. https://github.com/larmarange/broom.helpers/actions/workflows/R-CMD-check.yaml

## R CMD check results

0 errors | 0 warnings | 0 note

## revdepcheck results

We checked 17 reverse dependencies (15 from CRAN + 2 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 2 packages

Issues with CRAN packages are summarised below.

### Failed to check

* mmrm (NA)
* smdi (NA)

## recheck GitHub action

------- Check results summary ------
Check status summary:
                  ERROR NOTE OK
  Source packages     0    0  1
  Reverse depends     2    2 13

Check results summary:
broom.helpers ... OK
rdepends_GGally ... OK
rdepends_bregr ... ERROR
* checking tests ... ERROR
rdepends_cardx ... OK
rdepends_dcurves ... OK
rdepends_descriptio ... OK
rdepends_ggstats ... OK
rdepends_gtregression ... OK
rdepends_gtsummary ... OK
rdepends_guideR ... OK
rdepends_iNZightRegression ... OK
rdepends_logitr ... OK
rdepends_mmrm ... NOTE
* checking compiled code ... NOTE
rdepends_pubh ... ERROR
* checking examples ... ERROR
rdepends_regport ... OK
rdepends_simstudy ... NOTE
* checking DESCRIPTION meta-information ... NOTE
* checking compiled code ... NOTE
rdepends_smdi ... OK
rdepends_tidycmprsk ... OK

------- Check for regressions ------
No changes between old and new version

cf. https://github.com/larmarange/broom.helpers/actions/workflows/recheck.yml
