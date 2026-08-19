## Test environments

* local R installation (windows 11): R 4.6.1
* macos-latest (on github actions): R-release
* windows-latest (on github actions): R-release
* ubuntu-latest  (on github actions): R-devel, R-release, R-oldrel-1

cf. https://github.com/larmarange/broom.helpers/actions/workflows/R-CMD-check.yaml

## R CMD check results

0 errors | 0 warnings | 0 note

## revdepcheck results

We checked 24 reverse dependencies (22 from CRAN + 2 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 9 packages

Issues with CRAN packages are summarised below.

### Failed to check

* crane     (NA)
* GGally    (NA)
* ggstats   (NA)
* gtsummary (NA)
* logitr    (NA)
* mmrm      (NA)
* simstudy  (NA)
* smdi      (NA)
* sumExtras (NA)

## recheck GitHub action


------- Check results summary ------
Check status summary:
                  ERROR NOTE OK
  Source packages     0    0  1
  Reverse depends     1    4 18

Check results summary:
broom.helpers ... OK
rdepends_GGally ... OK
rdepends_SocialFacts ... OK
rdepends_bregr ... OK
rdepends_cardx ... OK
rdepends_crane ... OK
rdepends_dcurves ... OK
rdepends_descriptio ... OK
rdepends_epitabulate ... OK
rdepends_ggstats ... OK
rdepends_gtregression ... ERROR
* checking examples ... ERROR
* checking tests ... ERROR
rdepends_gtsummary ... OK
rdepends_guideR ... OK
rdepends_logitr ... NOTE
* checking compiled code ... NOTE
rdepends_mmrm ... NOTE
* checking compiled code ... NOTE
rdepends_parglm ... NOTE
* checking compiled code ... NOTE
rdepends_pubh ... OK
rdepends_regport ... OK
rdepends_siera ... OK
rdepends_simstudy ... NOTE
* checking compiled code ... NOTE
rdepends_smdi ... OK
rdepends_sumExtras ... OK
rdepends_tidyCDISC ... OK
rdepends_tidycmprsk ... OK

------- Check for regressions ------
No changes between old and new version

cf. https://github.com/larmarange/broom.helpers/actions/workflows/recheck.yml
