# bregr

<details>

* Version: 
* GitHub: https://github.com/larmarange/broom.helpers
* Source code: NA
* Number of recursive dependencies: 0

</details>

## Error before installation

### Devel

```

  Des versions binaires sont disponibles mais les versions des sources
  sont plus récentes:
            binary source needs_compilation
broom        1.0.9 1.0.10             FALSE
ggalign      1.0.2  1.1.0             FALSE
ggside       0.3.2  0.4.0             FALSE
ggstats     0.10.0 0.11.0             FALSE
ggstatsplot 0.13.1 0.13.2             FALSE
labelled    2.14.1 2.15.0             FALSE
...
magrittr     2.0.3  2.0.4              TRUE

  Binaries will be installed
le package 'abind' a été décompressé et les sommes MD5 ont été vérifiées avec succés
le package 'afex' a été décompressé et les sommes MD5 ont été vérifiées avec succés
le package 'askpass' a été décompressé et les sommes MD5 ont été vérifiées avec succés
le package 'backports' a été décompressé et les sommes MD5 ont été vérifiées avec succés
le package 'base64enc' a été décompressé et les sommes MD5 ont été vérifiées avec succés
le package 'BayesFactor' a été décompressé et les sommes MD5 ont été vérifiées avec succés
le package 'bayestestR' a été décompressé et les sommes MD5 ont été vérifiées avec succés





```
### CRAN

```

  Des versions binaires sont disponibles mais les versions des sources
  sont plus récentes:
            binary source needs_compilation
broom        1.0.9 1.0.10             FALSE
ggalign      1.0.2  1.1.0             FALSE
ggside       0.3.2  0.4.0             FALSE
ggstats     0.10.0 0.11.0             FALSE
ggstatsplot 0.13.1 0.13.2             FALSE
labelled    2.14.1 2.15.0             FALSE
...
magrittr     2.0.3  2.0.4              TRUE

  Binaries will be installed
le package 'abind' a été décompressé et les sommes MD5 ont été vérifiées avec succés
le package 'afex' a été décompressé et les sommes MD5 ont été vérifiées avec succés
le package 'askpass' a été décompressé et les sommes MD5 ont été vérifiées avec succés
le package 'backports' a été décompressé et les sommes MD5 ont été vérifiées avec succés
le package 'base64enc' a été décompressé et les sommes MD5 ont été vérifiées avec succés
le package 'BayesFactor' a été décompressé et les sommes MD5 ont été vérifiées avec succés
le package 'bayestestR' a été décompressé et les sommes MD5 ont été vérifiées avec succés





```
# mmrm

<details>

* Version: 0.3.15
* GitHub: https://github.com/openpharma/mmrm
* Source code: https://github.com/cran/mmrm
* Date/Publication: 2025-06-10 10:50:02 UTC
* Number of recursive dependencies: 173

Run `revdepcheck::revdep_details(, "mmrm")` for more info

</details>

## In both

*   R CMD check timed out
    

# simstudy

<details>

* Version: 
* GitHub: https://github.com/larmarange/broom.helpers
* Source code: NA
* Number of recursive dependencies: 0

</details>

## Error before installation

### Devel

```

  Des versions binaires sont disponibles mais les versions des sources
  sont plus récentes:
         binary source needs_compilation
broom     1.0.9 1.0.10             FALSE
labelled 2.14.1 2.15.0             FALSE
magrittr  2.0.3  2.0.4              TRUE

  Binaries will be installed
le package 'abind' a été décompressé et les sommes MD5 ont été vérifiées avec succés
le package 'askpass' a été décompressé et les sommes MD5 ont été vérifiées avec succés
le package 'backports' a été décompressé et les sommes MD5 ont été vérifiées avec succés
le package 'base64enc' a été décompressé et les sommes MD5 ont été vérifiées avec succés





```
### CRAN

```

  Des versions binaires sont disponibles mais les versions des sources
  sont plus récentes:
         binary source needs_compilation
broom     1.0.9 1.0.10             FALSE
labelled 2.14.1 2.15.0             FALSE
magrittr  2.0.3  2.0.4              TRUE

  Binaries will be installed
le package 'abind' a été décompressé et les sommes MD5 ont été vérifiées avec succés
le package 'askpass' a été décompressé et les sommes MD5 ont été vérifiées avec succés
le package 'backports' a été décompressé et les sommes MD5 ont été vérifiées avec succés
le package 'base64enc' a été décompressé et les sommes MD5 ont été vérifiées avec succés





```
# smdi

<details>

* Version: 0.3.1
* GitHub: NA
* Source code: https://github.com/cran/smdi
* Date/Publication: 2024-10-04 07:10:02 UTC
* Number of recursive dependencies: 220

Run `revdepcheck::revdep_details(, "smdi")` for more info

</details>

## In both

*   checking running R code from vignettes ...
    ```
      'a_data_generation.Rmd' using 'UTF-8'... failed
      'b_routine_diagnostics.Rmd' using 'UTF-8'... failed
      'c_multivariate_missingness.Rmd' using 'UTF-8'... OK
      'd_narfcs_sensitivity_analysis.Rmd' using 'UTF-8'... OK
      'smdi.Rmd' using 'UTF-8'... OK
     ERROR
    Errors in running code in vignettes:
    when running code in 'a_data_generation.Rmd'
      ...
      redémarrage de l'évaluation d'une promesse interrompue
    ...
    > library(here)
    here() starts at C:/Users/josep/AppData/Local/Temp/RtmpEfQVV1/filea85464dd75f/vignettes
    
    > library(knitr)
    
    > include_graphics(here("vignettes", "smdi_diagnose_table.png"))
    
      When sourcing 'b_routine_diagnostics.R':
    Erreur : Cannot find the file(s): "C:/Users/josep/AppData/Local/Temp/RtmpEfQVV1/filea85464dd75f/vignettes/vignettes/smdi_diagnose_table.png"
    Exécution arrêtée
    ```

*   R CMD check timed out
    

