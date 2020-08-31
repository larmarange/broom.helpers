## Test environments
* local R installation, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder (devel)
* mac OS (on github actions) R 4.0.2
* Windows (on github actions) R 4.0.2
* ubuntu 20.04 (on github actions) R-devel, R 4.0.2

## R CMD check results

0 errors | 0 warnings | 1 note

* Possibly mis-spelled words in DESCRIPTION:
    Tibbles (2:39)
    tibbles (18:58)
    
  This is expected. Tibbles refers here to the type of data frames
  produced by the 'tibble' package.
