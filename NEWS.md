# broom.helpers 1.15.0

**New supported models**

- support for `mmrm::mmrm()` models (#228)
- support for `survival::cch()` models (#242)

**New features**

- new `tidy_post_fun` argument in `tidy_plus_plus()` (#235)

**Fix**

- fix the order of the levels of categorical variables in the results of
  `tidy_marginal_predictions()` (#245)
- fix in `supported_models`
- bug fix when using `tidy_parameters()` for mixed models (#238)
- bug fix for `survey::svyglm()` models with replicate weights (#240)

# broom.helpers 1.14.0

**New features**

- support for `MASS::contr.sdif()` contrasts (#230)
- support for `pscl::zeroinfl()` and `pscl::hurdle()` models (#232)
- support for `betareg::betareg()` models (#234)

**Fix**

- input of `packageVersion()` should be a character string (#225)

# broom.helpers 1.13.0

**New features**

- `tidy_add_estimate_to_reference_rows()` now also populate p-values and
  confidence intervals for sum contrasts (#220)
- Marginal tidiers are now compatible with `nnet::multinom()`, `MASS::polr()`,
  `ordinal::clm()` and `ordinal::clmm()` models, as long as the type of models
  is supported by the corresponding package, for example, `margins` does not
  currently support `nnet::multinom()` models (#215)

**Improvements**

- Marginal predictions vignette has been updated to follow changes in
  `marginaleffects` version 0.10.0 (#216)

# broom.helpers 1.12.0

**New features**

- Set of functions to support marginal predictions, contrasts and slopes /
  effects (#202):
    - A dedicated article presenting the concepts and the different functions
      has been added to the package documentation website
    - Several tidiers are provided to tidy results in a way that it could be 
      used by `broom.helpers` functions.
    - **Marginal Predictions:** `tidy_marginal_predictions()`,
      `plot_marginal_predictions()`, `tidy_all_effects()`, and `tidy_ggpredict()`
    - **Marginal Means:** `tidy_marginal_means()`
    - **Marginal Contrasts:** `tidy_avg_comparisons()` and
      `tidy_marginal_contrasts()`
    - **Marginal Effects:** `tidy_avg_slopes()` and `tidy_margins()`
- New method `model_list_higher_order_variables()` to list the highest order
  combinations of variables (#202)
- New method `model_get_response_variable()` to get the name of the response
  variable (#202)
- New helper function `seq_range()` to generate a sequence of values between
  the minimum and the maximum of a vector (#202)
- New argument `contrasts_adjust` in `tidy_plus_plus()`,
  `tidy_add_pairwise_contrasts()` and `model_get_pairwise_contrasts()` allowing
  to change the adjustment method used to compute pairwise contrasts (#204)

# broom.helpers 1.11.0

**New features**

- New functions `tidy_add_pairwise_contrasts()` and `model_get_pairwise_contrasts()`
  to compute pairwise contrasts of categorical variables with `emmeans`,
  and corresponding new arguments in `tidy_plus_plus()` (#192)
- New tidier `tidy_margins()` to display Average Marginal Effects (#195)
- New tidier `tidy_all_effects()` to display Marginal Predictions (#195)
- New tidier `tidy_ggpredict()` to display Conditional Predictions (#195)

**Bug fixes and improvements**

- Better messages when `exponentiate` argument is not appropriate (#197)

# broom.helpers 1.10.0

**New features**

- `tidy_select_variables()` now sorts the variables according to `include` (#183)

**New supported models**

- Support for `logitr::logitr()` models (#179)
- Experimental support for `multgee::nomLORgee()` and `multgee::ordLORgee()`
  models (#185)

**Bug fixes and improvements**

- Improvement of `.get_package_dependencies()` to be more efficient. It now 
  looks only at a single package description file (#178)
- New function `.get_all_packages_dependencies()` to list all dependencies of
  all packages (#178)
- Bug fix in `.get_min_version_required()` (#181)

# broom.helpers 1.9.0

**New features**

- New function `.get_package_dependencies()` listing all dependencies, including
  minimum version required, of a package. (#171)
- Improvement of `.assert_package()` now taking into account the comparison
  operator (> or >=) when a minimum version is required (#171)
  
**Bug fixes and improvements**

- Compatibility with upcoming `tidyselect` v1.2.0 (#173)
- Avoid an unwanted warning for some `mgcv::gam()` models (#175)


# broom.helpers 1.8.0

**New supported models**

- Support for `parsnip::model_fit` objects (#161)
- Support for `biglm::bigglm()` and `biglmm::bigglm()` models (#155)
- Support for `fixest::feglm()`, `fixest::femlm()`, `fixest::feols()`
  and `fixest::feNmlm()` (requires R>=4.1) (#167)

**New features**

- Support for `dplyr::vars()` (also exported by {gtsummary}) as a selector has 
  now been deprecated. Users will be warned that support for `vars()` will
  eventually be removed from the package (#154)
- `.is_selector_scoped()`, an internal function used in generating custom
  selector functions, is now exported (#163)


# broom.helpers 1.7.0

**New features**

- The `.assert_package()` now uses `rlang::check_installed()` and 
  `rlang::is_installed()` to check whether needed packages are installed. The 
  `rlang::check_installed()` prompts user to install needed package when run 
  interactively. (#147)
- `tidy_add_n()` and `model_get_n()` support for `tidycmprsk::crr()` 
  models (#143)
- Listing of supported models is now available in `supported_models` tibble (#145)

**Bug fixes**

- Avoiding duplicating rows when applying `tidy_add_n()` to a `mgcv::gam()`
  model with smooth terms (#150)

# broom.helpers 1.6.0

**New supported models**

- Support for `plm::plm()` models (#140)

**New features**

- The `.formula_list_to_named_list()` now respects the `select_single=` 
  argument for all inputs types. Previously, named lists were ignored.
- Added new argument `.formula_list_to_named_list(null_allowed=)` argument 
  that works in conjunction with `type_check=` asserting the class/type of 
  the RHS of the formula (or the value of the named list) (#137)
- Better error message in `.formula_list_to_named_list()` (#136)
- Two additional select helpers `all_ran_pars()` and `all_ran_vals()`

**Bug fixes**

- Fix so `.formula_list_to_named_list(type_check=)` checks RHS of a formula 
  and the value of named list. (#138)

# broom.helpers 1.5.0

**New features**

- New method `model_get_coefficients_type.tidycrr()` (#128)
- Updated error messaging about using `broom.helpers::tidy_parameters()`
  to include the package prefix. This message sometimes appears while
  running `gtsummary::tbl_regression()` where some users may not be
  aware where the `tidy_paramters()` function lives. (#129)
- `.formula_list_to_named_list()` improvement: it is now possible to add
  a type check (#132)
- New functions `.assert_package()` and `.get_min_version_required()` to 
  check for a package's installation status and whether the installed
  version meets the minimum required version from the DESCRIPTION file (#134)

**Bug fixes**

- Bug fix for identifying the levels of a logical variable (#125)
- Bug fix for `nnet::multinom()` models with a binary outcome (#130)

# broom.helpers 1.4.0

**New supported models**

- Support for `glmmTMB::glmmTMB()` models (#119)

**New features**

- Function arguments that accept formula-list values now have more flexible 
  inputs. (#121)
    - The passed list may now be a combination of named lists and lists of 
      formulas, e.g. `list(trt ~ 1, all_continuous() ~ 2)`.
    - The shortcut `~ <value>` may be now used to indicate `everything() ~ <value>`

**Bug fixes**

- Bug fix for computing n for some binomial models computed 
  with `lme4::glmer()` (#116)
- Populating **effect** column when adding reference rows (#117)

# broom.helpers 1.3.0

**New supported models**

- Support of `rstanarm::stan_glm()` models
- Basic support for `VGAM::vglm()` models (#105)

**New features**

- Custom tieder `tidy_parameters()` based on `parameters::model_parameters()`
  (#104)
- Custom tieder `tidy_with_broom_or_parameters()` (#104)
- By default, `tidy_plus_plus()` now uses `tidy_with_broom_or_parameters()`
- `model_get_coefficients_type()` now returns "prop_hazard" for cloglog-binomial
  models (#106)

# broom.helpers 1.2.1

**Bug fixes**

- Better identification of term labels for interaction terms using sum 
  contrasts (#108)
- Now `tidy_add_n()` works with multinomial models when `y` is not coded as
  a factor (#109)
- `glue` added to Suggests

# broom.helpers 1.2.0

**New features**

- `model_get_coefficients_type()` now returns "relative_risk" for log-binomial
  models (#101)
- New function `tidy_disambiguate_terms()` for disambiguating random-effect 
  terms in mixed models and new options for `tidy_plus_plus()`: 
  `disambiguate_terms` (`TRUE` by default) and `disambiguate_sep` (#98)
- For mixed models, `var_type` column is now equal to `"ran_pars"` or 
  `"ran_vals"` for random-effect parameters and values, based of the 
  `effect` column returned by `broom.mixed::tidy()` (#90)
- New contrasts type ("no.contrast") returned by `model_list_contrasts`()
- New function `tidy_add_n()` to add the number of observations (and for 
  relevant models the number of events and exposure time) (#64)
- New option `add_n` in `tidy_plus_plus()` (#64)
- New functions `model_get_n()`, `model_get_weights()`, `model_get_offset()`,
  `model_get_response()` and `model_compute_terms_contributions()` (#64)

**New supported models**

- Support of `lfe::felm()` models (#79)
- Support of `brms::brm()` models (#89)
- Basic support of `cmprsk::crr()` models (#91)
- Basic support of `stats::nls()` models (#97)
- Models with categorical variable and no intercept now supported (#85)
- Added support for `mgcv::gam()` models. (#82)

**Bug fixes and other changes**

- *Minor breaking change:* `strict` argument removed from 
  `tidy_identify_variables()` (#99)
- Replaced `usethis::ui_*()` messaging with `cli::cli_*()` (#94)
- Bug fix in `tidy_add_term_labels()` for variables with non standard 
  names (#77)
- Fix in vignette for old versions of `rmarkdown` (#95)


# broom.helpers 1.1.0

* **Minor breaking change:** column `var_type` returned by 
  `tidy_identify_variables()` is now equal to `"dichotomous"` for categorical 
  variables with only 2 levels
* **Minor breaking changes:** for intercepts terms, `tidy_identify_variables()`
  now populates `variable` column by `term` content, instead of `NA` (#66)
* **Minor breaking change:** If the variables can't be identified by 
  `tidy_identify_variables()`, the `variable` column is now populated 
  with the content of the `term` column (#63)
* Exporting select helper utility functions (#65)
    - `.generic_selector()`: makes it easy to create selecting functions like 
      `all_continuous()`.  
    - `.select_to_varnames()`: converts selecting syntax into character varnames
    - `.formula_list_to_named_list()`: takes the formula selecting syntax and 
      converts it to a named list. 
* New selecting functions `all_continuous()`, `all_categorical()`,
  `all_dichotomous()`, `all_contrasts()`, `all_intercepts()` and 
  `all_interaction()` for selecting variables from models (#54)
* Added support for multiple imputation models from the {mice} 
  package. The model passed must be the un-pooled models, and the 
  pooling step included in `tidy_fun=` (#49 @ddsjoberg) 
* New function `tidy_select_variables()` to keep/drop
  selected variables in the output (#45)
* New functions `tidy_add_coefficients_type()` and 
  `model_get_coefficients_type` to get the type of coefficients
  (generic, logistic, Poisson or proportional hazard) used
  by a model (#46)
* `tidy_add_contrasts()` and `model_list_contrasts()` now return an
  additional column `contrasts_type`
* New `no_reference_row` argument for `tidy_add_reference_rows()` (#47)
* New method `model_get_nlevels` to get the number of levels of categorical 
  variables
* New column `var_nlevels` returned by `tidy_identify_variables()`,
  `model_identify_variables()` and `tidy_plus_plus()`
* Categorical terms can now be customized with a pattern taking into account
  term level, reference level and/or variable label, see 
  `model_list_terms_levels()` and `categorical_terms_pattern` in 
  `tidy_plus_plus()` and `tidy_add_term_labels` (#61)
* `model_list_terms_levels()` now returns additional columns (`level`, 
  `reference_level`, `contrasts_type`, `var_label`, `var_levels` and 
  `dichotomous`)
* `model_list_variables()` now returns an additional `var_label` column
* The `exponentiate` argument is now passed to the `tidy_*()`
  functions, as an attribute attached to the tibble, as well as custom labels
  (`variable_labels` and `term_labels`)
* `show_single_row` argument now accepts tidyselect notation (#51 @ddsjoberg)
* `tidy_add_estimate_to_reference_rows()` now relies on `emmeans` for
  sum contrasts, allowing to cover a wider range of models
* Tibbles returned by `tidy_*` functions also inherits of `"broom.helpers"` 
  class (#56)
* `interaction_sep` argument has been added to `tidy_plus_plus()`
* Better management of variables with non standard names (#67)
* `.clean_backticks()` and `.escape_regex()` are now exported
* Bug fix for non standard variable names containing
  a character that would have a special meaning in
  a regular expression (#44)
* Bug fix in `tidy_add_header_rows()` for `nnet::multinom` models:
  label for header rows was missing (#50)
* Bug fix: now `tidy_identify_variables()` correctly identify class "integer"
  for this type of variables (#57)
* Bug fix for `tidy_add_header_rows()` for continuous variables with a non 
  standard name (#70)

# broom.helpers 1.0.0

* Initial version
