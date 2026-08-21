# Changelog

## broom.helpers 1.23.0

CRAN release: 2026-08-20

**New supported models**

- support for
  [`survival::coxphms.object`](https://rdrr.io/pkg/survival/man/coxphms.object.html)
  models, see the experimental tidier
  [`tidy_coxphms()`](https://larmarange.github.io/broom.helpers/reference/tidy_coxphms.md)
  ([\#308](https://github.com/larmarange/broom.helpers/issues/308))

**New features**

- [`model_get_coefficients_type()`](https://larmarange.github.io/broom.helpers/reference/model_get_coefficients_type.md)
  for `brmsfit` models
  ([\#316](https://github.com/larmarange/broom.helpers/issues/316))

**Fixes**

- [`.clean_backticks()`](https://larmarange.github.io/broom.helpers/reference/dot-clean_backticks.md)
  fixed when variable names contain `$`
  ([\#311](https://github.com/larmarange/broom.helpers/issues/311),
  [@NourEdinDarwish](https://github.com/NourEdinDarwish))
- fix in variable identification when
  [`I()`](https://rdrr.io/r/base/AsIs.html) is used with non standard
  variable names
  ([\#313](https://github.com/larmarange/broom.helpers/issues/313),
  [@NourEdinDarwish](https://github.com/NourEdinDarwish))

## broom.helpers 1.22.0

CRAN release: 2025-09-17

**New supported models**

- support for
  [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) models,
  use
  [`tidy_with_broom_or_parameters()`](https://larmarange.github.io/broom.helpers/reference/tidy_with_broom_or_parameters.md),
  default tidier of
  [`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.md)
  ([\#299](https://github.com/larmarange/broom.helpers/issues/299))

**Fixes**

- bug fix for `fixest` models when using a subsetted data frame
  ([\#301](https://github.com/larmarange/broom.helpers/issues/301))
- bug fix for `fixest` models when computing the number of observations
  ([\#303](https://github.com/larmarange/broom.helpers/issues/303))

**Removed functions**

- `.select_to_varnames()`, `.formula_list_to_named_list()`,
  `.generic_selector()` and `.is_selector_scoped()` have now been
  removed

## broom.helpers 1.21.0

CRAN release: 2025-04-24

**New supported models**

- basic support for
  [`svyVGAM::svy_vglm()`](https://rdrr.io/pkg/svyVGAM/man/svy_vglm.html)
  models, see the experimental tidier
  [`tidy_svy_vglm()`](https://larmarange.github.io/broom.helpers/reference/tidy_svy_vglm.md)
  ([\#293](https://github.com/larmarange/broom.helpers/issues/293))

**Minor breaking changes**

- [`tidy_add_term_labels()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_term_labels.md)
  does not relabel polynomial terms (defined with
  [`stats::poly()`](https://rdrr.io/r/stats/poly.html)) by default. The
  feature is style accessible using `relabel_poly = TRUE`
  ([\#292](https://github.com/larmarange/broom.helpers/issues/292))

## broom.helpers 1.20.0

CRAN release: 2025-03-06

**New supported models**

- improved support for
  [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) and
  [`VGAM::vgam()`](https://rdrr.io/pkg/VGAM/man/vgam.html) models, see
  the experimental tidier
  [`tidy_vgam()`](https://larmarange.github.io/broom.helpers/reference/tidy_vgam.md)
  ([\#253](https://github.com/larmarange/broom.helpers/issues/253))

**New features**

- new
  [`tidy_group_by()`](https://larmarange.github.io/broom.helpers/reference/tidy_group_by.md)
  function to indicate how to group results
  ([\#288](https://github.com/larmarange/broom.helpers/issues/288))
- new arguments `group_by` and `group_labels` for
  [`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.md)
  ([\#288](https://github.com/larmarange/broom.helpers/issues/288))

**Deprecated functions**

- `.select_to_varnames()`, `.formula_list_to_named_list()`,
  `.generic_selector()` and `.is_selector_scoped()` are now hard
  deprecated.

## broom.helpers 1.19.0

CRAN release: 2025-01-29

**Deprecated function**

- [`tidy_marginal_means()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_means.md)
  is now hard deprecated
  ([\#284](https://github.com/larmarange/broom.helpers/issues/284))

## broom.helpers 1.18.0

CRAN release: 2025-01-07

**New supported models**

- support for
  [`glmtoolbox::glmgee()`](https://rdrr.io/pkg/glmtoolbox/man/glmgee.html)
  models
  ([\#274](https://github.com/larmarange/broom.helpers/issues/274))

**New features**

- support of instrumental variables for `fixest` models
  ([\#279](https://github.com/larmarange/broom.helpers/issues/279))
- new argument `instrumental_suffix` for
  [`model_list_variables()`](https://larmarange.github.io/broom.helpers/reference/model_list_variables.md),
  [`tidy_add_variable_labels()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_variable_labels.md)
  and
  [`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.md)

**Fixes**

- variable labels are now returned by
  [`model_list_variables()`](https://larmarange.github.io/broom.helpers/reference/model_list_variables.md)
  for `svycoxph` models
  ([\#275](https://github.com/larmarange/broom.helpers/issues/275))
- compatibility with R version 4.1 minimum
  ([\#276](https://github.com/larmarange/broom.helpers/issues/276))
- fix for
  [`tidy_add_n()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_n.md)
  with models with a subset argument
  ([\#278](https://github.com/larmarange/broom.helpers/issues/278))

## broom.helpers 1.17.0

CRAN release: 2024-08-28

**Deprecated functions and changes in selectors functions**

- selectors such as
  [`all_categorical()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md)
  are now compatible with `gtsummary` version ≥ 2.0.0
  ([\#270](https://github.com/larmarange/broom.helpers/issues/270))
- new function
  [`scope_tidy()`](https://larmarange.github.io/broom.helpers/reference/scope_tidy.md)
  to scope a tidy tibble allowing to tidy select
  ([\#270](https://github.com/larmarange/broom.helpers/issues/270))
- `.select_to_varnames()`, `.formula_list_to_named_list()`,
  `.generic_selector()` and `.is_selector_scoped()` are now deprecated
  and will be removed in a future release: you may consider
  [`cards::process_selectors()`](https://rdrr.io/pkg/cards/man/process_selectors.html)
  and
  [`cards::process_formula_selectors()`](https://rdrr.io/pkg/cards/man/process_selectors.html)
  as alternatives
  ([\#270](https://github.com/larmarange/broom.helpers/issues/270))

**Fixes**

- [`model_get_model_frame.coxph()`](https://larmarange.github.io/broom.helpers/reference/model_get_model_frame.md)
  has been fixed to return a correct model frame a subject identifier is
  passed to
  [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)
  ([\#268](https://github.com/larmarange/broom.helpers/issues/268))

**Documentation**

- Documentation has been improved, showing now clearly the type expected
  for each argument
  ([\#272](https://github.com/larmarange/broom.helpers/issues/272))

## broom.helpers 1.16.0

CRAN release: 2024-08-20

**New features**

- new argument `model_matrix_attr` in
  [`tidy_and_attach()`](https://larmarange.github.io/broom.helpers/reference/tidy_attach_model.md)
  and
  [`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.md)
  to attach model frame and model matrix to the model as attributes for
  saving some execution time
  ([\#254](https://github.com/larmarange/broom.helpers/issues/254))
- [`tidy_add_n()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_n.md)
  now returns `n_ind` the number of individuals, in addition to the
  number of observations
  ([\#251](https://github.com/larmarange/broom.helpers/issues/251))
- by default,
  [`tidy_parameters()`](https://larmarange.github.io/broom.helpers/reference/tidy_parameters.md)
  calls now
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  with `pretty_names = FALSE` for saving execution time
  ([\#259](https://github.com/larmarange/broom.helpers/issues/259))
- internal code now uses the native R pipe (`|>`), requiring therefore R
  \>= 4.2
  ([\#262](https://github.com/larmarange/broom.helpers/issues/262))

**Deprecated support**

- `biglmm::bigglm()` not supported anymore as `biglmm` has been removed
  from CRAN

**Deprecated functions**

- [`tidy_marginal_means()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_means.md)
  is now deprecated, following deprecation of
  `marginaleffects::marginal_means()`. Use instead
  [`tidy_marginal_predictions()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_predictions.md)
  with the option `newdata = "balanced"`.
- [`tidy_margins()`](https://larmarange.github.io/broom.helpers/reference/tidy_margins.md)
  is now indicated as superseded and may be deprecated if `margins` is
  removed from CRAN.
  [`tidy_avg_slopes()`](https://larmarange.github.io/broom.helpers/reference/tidy_avg_slopes.md)
  could be used as an alternative.
  ([\#252](https://github.com/larmarange/broom.helpers/issues/252))

**Fixes**

- [`tidy_multgee()`](https://larmarange.github.io/broom.helpers/reference/tidy_multgee.md)
  has been fixed to properly identify the different `y.levels`
  ([\#260](https://github.com/larmarange/broom.helpers/issues/260)
  [@jackmwolf](https://github.com/jackmwolf))
- [`tidy_marginal_predictions()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_predictions.md)
  has been updated to avoid the use of the deprecated function
  `marginaleffects::datagridcf()`
  ([\#256](https://github.com/larmarange/broom.helpers/issues/256))

## broom.helpers 1.15.0

CRAN release: 2024-04-05

**New supported models**

- support for
  [`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
  models
  ([\#228](https://github.com/larmarange/broom.helpers/issues/228))
- support for
  [`survival::cch()`](https://rdrr.io/pkg/survival/man/cch.html) models
  ([\#242](https://github.com/larmarange/broom.helpers/issues/242))

**New features**

- new `tidy_post_fun` argument in
  [`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.md)
  ([\#235](https://github.com/larmarange/broom.helpers/issues/235))

**Fix**

- fix the order of the levels of categorical variables in the results of
  [`tidy_marginal_predictions()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_predictions.md)
  ([\#245](https://github.com/larmarange/broom.helpers/issues/245))
- fix in `supported_models`
- bug fix when using
  [`tidy_parameters()`](https://larmarange.github.io/broom.helpers/reference/tidy_parameters.md)
  for mixed models
  ([\#238](https://github.com/larmarange/broom.helpers/issues/238))
- bug fix for
  [`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html)
  models with replicate weights
  ([\#240](https://github.com/larmarange/broom.helpers/issues/240))

## broom.helpers 1.14.0

CRAN release: 2023-08-07

**New features**

- support for
  [`MASS::contr.sdif()`](https://rdrr.io/pkg/MASS/man/contr.sdif.html)
  contrasts
  ([\#230](https://github.com/larmarange/broom.helpers/issues/230))
- support for
  [`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) and
  [`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html) models
  ([\#232](https://github.com/larmarange/broom.helpers/issues/232))
- support for
  [`betareg::betareg()`](https://rdrr.io/pkg/betareg/man/betareg.html)
  models
  ([\#234](https://github.com/larmarange/broom.helpers/issues/234))

**Fix**

- input of
  [`packageVersion()`](https://rdrr.io/r/utils/packageDescription.html)
  should be a character string
  ([\#225](https://github.com/larmarange/broom.helpers/issues/225))

## broom.helpers 1.13.0

CRAN release: 2023-03-28

**New features**

- [`tidy_add_estimate_to_reference_rows()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_estimate_to_reference_rows.md)
  now also populate p-values and confidence intervals for sum contrasts
  ([\#220](https://github.com/larmarange/broom.helpers/issues/220))
- Marginal tidiers are now compatible with
  [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html),
  [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html),
  [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html) and
  [`ordinal::clmm()`](https://rdrr.io/pkg/ordinal/man/clmm.html) models,
  as long as the type of models is supported by the corresponding
  package, for example, `margins` does not currently support
  [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html)
  models
  ([\#215](https://github.com/larmarange/broom.helpers/issues/215))

**Improvements**

- Marginal predictions vignette has been updated to follow changes in
  `marginaleffects` version 0.10.0
  ([\#216](https://github.com/larmarange/broom.helpers/issues/216))

## broom.helpers 1.12.0

CRAN release: 2023-02-09

**New features**

- Set of functions to support marginal predictions, contrasts and slopes
  / effects
  ([\#202](https://github.com/larmarange/broom.helpers/issues/202)):
  - A dedicated article presenting the concepts and the different
    functions has been added to the package documentation website
  - Several tidiers are provided to tidy results in a way that it could
    be used by `broom.helpers` functions.
  - **Marginal Predictions:**
    [`tidy_marginal_predictions()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_predictions.md),
    [`plot_marginal_predictions()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_predictions.md),
    [`tidy_all_effects()`](https://larmarange.github.io/broom.helpers/reference/tidy_all_effects.md),
    and
    [`tidy_ggpredict()`](https://larmarange.github.io/broom.helpers/reference/tidy_ggpredict.md)
  - **Marginal Means:**
    [`tidy_marginal_means()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_means.md)
  - **Marginal Contrasts:**
    [`tidy_avg_comparisons()`](https://larmarange.github.io/broom.helpers/reference/tidy_avg_comparisons.md)
    and
    [`tidy_marginal_contrasts()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_contrasts.md)
  - **Marginal Effects:**
    [`tidy_avg_slopes()`](https://larmarange.github.io/broom.helpers/reference/tidy_avg_slopes.md)
    and
    [`tidy_margins()`](https://larmarange.github.io/broom.helpers/reference/tidy_margins.md)
- New method
  [`model_list_higher_order_variables()`](https://larmarange.github.io/broom.helpers/reference/model_list_higher_order_variables.md)
  to list the highest order combinations of variables
  ([\#202](https://github.com/larmarange/broom.helpers/issues/202))
- New method
  [`model_get_response_variable()`](https://larmarange.github.io/broom.helpers/reference/model_get_response_variable.md)
  to get the name of the response variable
  ([\#202](https://github.com/larmarange/broom.helpers/issues/202))
- New helper function
  [`seq_range()`](https://larmarange.github.io/broom.helpers/reference/seq_range.md)
  to generate a sequence of values between the minimum and the maximum
  of a vector
  ([\#202](https://github.com/larmarange/broom.helpers/issues/202))
- New argument `contrasts_adjust` in
  [`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.md),
  [`tidy_add_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_pairwise_contrasts.md)
  and
  [`model_get_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/reference/model_get_pairwise_contrasts.md)
  allowing to change the adjustment method used to compute pairwise
  contrasts
  ([\#204](https://github.com/larmarange/broom.helpers/issues/204))

## broom.helpers 1.11.0

CRAN release: 2023-01-06

**New features**

- New functions
  [`tidy_add_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_pairwise_contrasts.md)
  and
  [`model_get_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/reference/model_get_pairwise_contrasts.md)
  to compute pairwise contrasts of categorical variables with `emmeans`,
  and corresponding new arguments in
  [`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.md)
  ([\#192](https://github.com/larmarange/broom.helpers/issues/192))
- New tidier
  [`tidy_margins()`](https://larmarange.github.io/broom.helpers/reference/tidy_margins.md)
  to display Average Marginal Effects
  ([\#195](https://github.com/larmarange/broom.helpers/issues/195))
- New tidier
  [`tidy_all_effects()`](https://larmarange.github.io/broom.helpers/reference/tidy_all_effects.md)
  to display Marginal Predictions
  ([\#195](https://github.com/larmarange/broom.helpers/issues/195))
- New tidier
  [`tidy_ggpredict()`](https://larmarange.github.io/broom.helpers/reference/tidy_ggpredict.md)
  to display Conditional Predictions
  ([\#195](https://github.com/larmarange/broom.helpers/issues/195))

**Bug fixes and improvements**

- Better messages when `exponentiate` argument is not appropriate
  ([\#197](https://github.com/larmarange/broom.helpers/issues/197))

## broom.helpers 1.10.0

CRAN release: 2022-11-30

**New features**

- [`tidy_select_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_select_variables.md)
  now sorts the variables according to `include`
  ([\#183](https://github.com/larmarange/broom.helpers/issues/183))

**New supported models**

- Support for
  [`logitr::logitr()`](https://jhelvy.github.io/logitr/reference/logitr.html)
  models
  ([\#179](https://github.com/larmarange/broom.helpers/issues/179))
- Experimental support for
  [`multgee::nomLORgee()`](https://rdrr.io/pkg/multgee/man/nomLORgee.html)
  and
  [`multgee::ordLORgee()`](https://rdrr.io/pkg/multgee/man/ordLORgee.html)
  models
  ([\#185](https://github.com/larmarange/broom.helpers/issues/185))

**Bug fixes and improvements**

- Improvement of
  [`.get_package_dependencies()`](https://larmarange.github.io/broom.helpers/reference/assert_package.md)
  to be more efficient. It now looks only at a single package
  description file
  ([\#178](https://github.com/larmarange/broom.helpers/issues/178))
- New function
  [`.get_all_packages_dependencies()`](https://larmarange.github.io/broom.helpers/reference/assert_package.md)
  to list all dependencies of all packages
  ([\#178](https://github.com/larmarange/broom.helpers/issues/178))
- Bug fix in
  [`.get_min_version_required()`](https://larmarange.github.io/broom.helpers/reference/assert_package.md)
  ([\#181](https://github.com/larmarange/broom.helpers/issues/181))

## broom.helpers 1.9.0

CRAN release: 2022-09-23

**New features**

- New function
  [`.get_package_dependencies()`](https://larmarange.github.io/broom.helpers/reference/assert_package.md)
  listing all dependencies, including minimum version required, of a
  package.
  ([\#171](https://github.com/larmarange/broom.helpers/issues/171))
- Improvement of
  [`.assert_package()`](https://larmarange.github.io/broom.helpers/reference/assert_package.md)
  now taking into account the comparison operator (\> or \>=) when a
  minimum version is required
  ([\#171](https://github.com/larmarange/broom.helpers/issues/171))

**Bug fixes and improvements**

- Compatibility with upcoming `tidyselect` v1.2.0
  ([\#173](https://github.com/larmarange/broom.helpers/issues/173))
- Avoid an unwanted warning for some
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) models
  ([\#175](https://github.com/larmarange/broom.helpers/issues/175))

## broom.helpers 1.8.0

CRAN release: 2022-07-05

**New supported models**

- Support for
  [`parsnip::model_fit`](https://parsnip.tidymodels.org/reference/model_fit.html)
  objects
  ([\#161](https://github.com/larmarange/broom.helpers/issues/161))
- Support for
  [`biglm::bigglm()`](https://rdrr.io/pkg/biglm/man/bigglm.html) and
  `biglmm::bigglm()` models
  ([\#155](https://github.com/larmarange/broom.helpers/issues/155))
- Support for
  [`fixest::feglm()`](https://lrberge.github.io/fixest/reference/feglm.html),
  [`fixest::femlm()`](https://lrberge.github.io/fixest/reference/femlm.html),
  [`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html)
  and
  [`fixest::feNmlm()`](https://lrberge.github.io/fixest/reference/feNmlm.html)
  (requires R\>=4.1)
  ([\#167](https://github.com/larmarange/broom.helpers/issues/167))

**New features**

- Support for
  [`dplyr::vars()`](https://dplyr.tidyverse.org/reference/vars.html)
  (also exported by {gtsummary}) as a selector has now been deprecated.
  Users will be warned that support for
  [`vars()`](https://dplyr.tidyverse.org/reference/vars.html) will
  eventually be removed from the package
  ([\#154](https://github.com/larmarange/broom.helpers/issues/154))
- `.is_selector_scoped()`, an internal function used in generating
  custom selector functions, is now exported
  ([\#163](https://github.com/larmarange/broom.helpers/issues/163))

## broom.helpers 1.7.0

CRAN release: 2022-04-22

**New features**

- The
  [`.assert_package()`](https://larmarange.github.io/broom.helpers/reference/assert_package.md)
  now uses
  [`rlang::check_installed()`](https://rlang.r-lib.org/reference/is_installed.html)
  and
  [`rlang::is_installed()`](https://rlang.r-lib.org/reference/is_installed.html)
  to check whether needed packages are installed. The
  [`rlang::check_installed()`](https://rlang.r-lib.org/reference/is_installed.html)
  prompts user to install needed package when run interactively.
  ([\#147](https://github.com/larmarange/broom.helpers/issues/147))
- [`tidy_add_n()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_n.md)
  and
  [`model_get_n()`](https://larmarange.github.io/broom.helpers/reference/model_get_n.md)
  support for
  [`tidycmprsk::crr()`](https://mskcc-epi-bio.github.io/tidycmprsk/reference/crr.html)
  models
  ([\#143](https://github.com/larmarange/broom.helpers/issues/143))
- Listing of supported models is now available in `supported_models`
  tibble
  ([\#145](https://github.com/larmarange/broom.helpers/issues/145))

**Bug fixes**

- Avoiding duplicating rows when applying
  [`tidy_add_n()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_n.md)
  to a [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) model with
  smooth terms
  ([\#150](https://github.com/larmarange/broom.helpers/issues/150))

## broom.helpers 1.6.0

CRAN release: 2022-01-12

**New supported models**

- Support for [`plm::plm()`](https://rdrr.io/pkg/plm/man/plm.html)
  models
  ([\#140](https://github.com/larmarange/broom.helpers/issues/140))

**New features**

- The `.formula_list_to_named_list()` now respects the `select_single=`
  argument for all inputs types. Previously, named lists were ignored.
- Added new argument `.formula_list_to_named_list(null_allowed=)`
  argument that works in conjunction with `type_check=` asserting the
  class/type of the RHS of the formula (or the value of the named list)
  ([\#137](https://github.com/larmarange/broom.helpers/issues/137))
- Better error message in `.formula_list_to_named_list()`
  ([\#136](https://github.com/larmarange/broom.helpers/issues/136))
- Two additional select helpers
  [`all_ran_pars()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md)
  and
  [`all_ran_vals()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md)

**Bug fixes**

- Fix so `.formula_list_to_named_list(type_check=)` checks RHS of a
  formula and the value of named list.
  ([\#138](https://github.com/larmarange/broom.helpers/issues/138))

## broom.helpers 1.5.0

CRAN release: 2021-12-07

**New features**

- New method
  [`model_get_coefficients_type.tidycrr()`](https://larmarange.github.io/broom.helpers/reference/model_get_coefficients_type.md)
  ([\#128](https://github.com/larmarange/broom.helpers/issues/128))
- Updated error messaging about using
  [`broom.helpers::tidy_parameters()`](https://larmarange.github.io/broom.helpers/reference/tidy_parameters.md)
  to include the package prefix. This message sometimes appears while
  running
  [`gtsummary::tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_regression.html)
  where some users may not be aware where the `tidy_paramters()`
  function lives.
  ([\#129](https://github.com/larmarange/broom.helpers/issues/129))
- `.formula_list_to_named_list()` improvement: it is now possible to add
  a type check
  ([\#132](https://github.com/larmarange/broom.helpers/issues/132))
- New functions
  [`.assert_package()`](https://larmarange.github.io/broom.helpers/reference/assert_package.md)
  and
  [`.get_min_version_required()`](https://larmarange.github.io/broom.helpers/reference/assert_package.md)
  to check for a package’s installation status and whether the installed
  version meets the minimum required version from the DESCRIPTION file
  ([\#134](https://github.com/larmarange/broom.helpers/issues/134))

**Bug fixes**

- Bug fix for identifying the levels of a logical variable
  ([\#125](https://github.com/larmarange/broom.helpers/issues/125))
- Bug fix for
  [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html)
  models with a binary outcome
  ([\#130](https://github.com/larmarange/broom.helpers/issues/130))

## broom.helpers 1.4.0

CRAN release: 2021-09-30

**New supported models**

- Support for
  [`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html)
  models
  ([\#119](https://github.com/larmarange/broom.helpers/issues/119))

**New features**

- Function arguments that accept formula-list values now have more
  flexible inputs.
  ([\#121](https://github.com/larmarange/broom.helpers/issues/121))
  - The passed list may now be a combination of named lists and lists of
    formulas, e.g. `list(trt ~ 1, all_continuous() ~ 2)`.
  - The shortcut `~ <value>` may be now used to indicate
    `everything() ~ <value>`

**Bug fixes**

- Bug fix for computing n for some binomial models computed with
  [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html)
  ([\#116](https://github.com/larmarange/broom.helpers/issues/116))
- Populating **effect** column when adding reference rows
  ([\#117](https://github.com/larmarange/broom.helpers/issues/117))

## broom.helpers 1.3.0

CRAN release: 2021-04-10

**New supported models**

- Support of
  [`rstanarm::stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html)
  models
- Basic support for
  [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) models
  ([\#105](https://github.com/larmarange/broom.helpers/issues/105))

**New features**

- Custom tieder
  [`tidy_parameters()`](https://larmarange.github.io/broom.helpers/reference/tidy_parameters.md)
  based on
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  ([\#104](https://github.com/larmarange/broom.helpers/issues/104))
- Custom tieder
  [`tidy_with_broom_or_parameters()`](https://larmarange.github.io/broom.helpers/reference/tidy_with_broom_or_parameters.md)
  ([\#104](https://github.com/larmarange/broom.helpers/issues/104))
- By default,
  [`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.md)
  now uses
  [`tidy_with_broom_or_parameters()`](https://larmarange.github.io/broom.helpers/reference/tidy_with_broom_or_parameters.md)
- [`model_get_coefficients_type()`](https://larmarange.github.io/broom.helpers/reference/model_get_coefficients_type.md)
  now returns “prop_hazard” for cloglog-binomial models
  ([\#106](https://github.com/larmarange/broom.helpers/issues/106))

## broom.helpers 1.2.1

CRAN release: 2021-02-26

**Bug fixes**

- Better identification of term labels for interaction terms using sum
  contrasts
  ([\#108](https://github.com/larmarange/broom.helpers/issues/108))
- Now
  [`tidy_add_n()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_n.md)
  works with multinomial models when `y` is not coded as a factor
  ([\#109](https://github.com/larmarange/broom.helpers/issues/109))
- `glue` added to Suggests

## broom.helpers 1.2.0

CRAN release: 2021-02-22

**New features**

- [`model_get_coefficients_type()`](https://larmarange.github.io/broom.helpers/reference/model_get_coefficients_type.md)
  now returns “relative_risk” for log-binomial models
  ([\#101](https://github.com/larmarange/broom.helpers/issues/101))
- New function
  [`tidy_disambiguate_terms()`](https://larmarange.github.io/broom.helpers/reference/tidy_disambiguate_terms.md)
  for disambiguating random-effect terms in mixed models and new options
  for
  [`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.md):
  `disambiguate_terms` (`TRUE` by default) and `disambiguate_sep`
  ([\#98](https://github.com/larmarange/broom.helpers/issues/98))
- For mixed models, `var_type` column is now equal to `"ran_pars"` or
  `"ran_vals"` for random-effect parameters and values, based of the
  `effect` column returned by
  [`broom.mixed::tidy()`](https://generics.r-lib.org/reference/tidy.html)
  ([\#90](https://github.com/larmarange/broom.helpers/issues/90))
- New contrasts type (“no.contrast”) returned by
  `model_list_contrasts`()
- New function
  [`tidy_add_n()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_n.md)
  to add the number of observations (and for relevant models the number
  of events and exposure time)
  ([\#64](https://github.com/larmarange/broom.helpers/issues/64))
- New option `add_n` in
  [`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.md)
  ([\#64](https://github.com/larmarange/broom.helpers/issues/64))
- New functions
  [`model_get_n()`](https://larmarange.github.io/broom.helpers/reference/model_get_n.md),
  [`model_get_weights()`](https://larmarange.github.io/broom.helpers/reference/model_get_weights.md),
  [`model_get_offset()`](https://larmarange.github.io/broom.helpers/reference/model_get_offset.md),
  [`model_get_response()`](https://larmarange.github.io/broom.helpers/reference/model_get_response.md)
  and
  [`model_compute_terms_contributions()`](https://larmarange.github.io/broom.helpers/reference/model_compute_terms_contributions.md)
  ([\#64](https://github.com/larmarange/broom.helpers/issues/64))

**New supported models**

- Support of [`lfe::felm()`](https://rdrr.io/pkg/lfe/man/felm.html)
  models ([\#79](https://github.com/larmarange/broom.helpers/issues/79))
- Support of
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)
  models ([\#89](https://github.com/larmarange/broom.helpers/issues/89))
- Basic support of
  [`cmprsk::crr()`](https://rdrr.io/pkg/cmprsk/man/crr.html) models
  ([\#91](https://github.com/larmarange/broom.helpers/issues/91))
- Basic support of [`stats::nls()`](https://rdrr.io/r/stats/nls.html)
  models ([\#97](https://github.com/larmarange/broom.helpers/issues/97))
- Models with categorical variable and no intercept now supported
  ([\#85](https://github.com/larmarange/broom.helpers/issues/85))
- Added support for
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) models.
  ([\#82](https://github.com/larmarange/broom.helpers/issues/82))

**Bug fixes and other changes**

- *Minor breaking change:* `strict` argument removed from
  [`tidy_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_identify_variables.md)
  ([\#99](https://github.com/larmarange/broom.helpers/issues/99))
- Replaced `usethis::ui_*()` messaging with `cli::cli_*()`
  ([\#94](https://github.com/larmarange/broom.helpers/issues/94))
- Bug fix in
  [`tidy_add_term_labels()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_term_labels.md)
  for variables with non standard names
  ([\#77](https://github.com/larmarange/broom.helpers/issues/77))
- Fix in vignette for old versions of `rmarkdown`
  ([\#95](https://github.com/larmarange/broom.helpers/issues/95))

## broom.helpers 1.1.0

CRAN release: 2020-11-24

- **Minor breaking change:** column `var_type` returned by
  [`tidy_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_identify_variables.md)
  is now equal to `"dichotomous"` for categorical variables with only 2
  levels
- **Minor breaking changes:** for intercepts terms,
  [`tidy_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_identify_variables.md)
  now populates `variable` column by `term` content, instead of `NA`
  ([\#66](https://github.com/larmarange/broom.helpers/issues/66))
- **Minor breaking change:** If the variables can’t be identified by
  [`tidy_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_identify_variables.md),
  the `variable` column is now populated with the content of the `term`
  column ([\#63](https://github.com/larmarange/broom.helpers/issues/63))
- Exporting select helper utility functions
  ([\#65](https://github.com/larmarange/broom.helpers/issues/65))
  - `.generic_selector()`: makes it easy to create selecting functions
    like
    [`all_continuous()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md).  
  - `.select_to_varnames()`: converts selecting syntax into character
    varnames
  - `.formula_list_to_named_list()`: takes the formula selecting syntax
    and converts it to a named list.
- New selecting functions
  [`all_continuous()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md),
  [`all_categorical()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md),
  [`all_dichotomous()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md),
  [`all_contrasts()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md),
  [`all_intercepts()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md)
  and
  [`all_interaction()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md)
  for selecting variables from models
  ([\#54](https://github.com/larmarange/broom.helpers/issues/54))
- Added support for multiple imputation models from the {mice} package.
  The model passed must be the un-pooled models, and the pooling step
  included in `tidy_fun=`
  ([\#49](https://github.com/larmarange/broom.helpers/issues/49)
  [@ddsjoberg](https://github.com/ddsjoberg))
- New function
  [`tidy_select_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_select_variables.md)
  to keep/drop selected variables in the output
  ([\#45](https://github.com/larmarange/broom.helpers/issues/45))
- New functions
  [`tidy_add_coefficients_type()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_coefficients_type.md)
  and `model_get_coefficients_type` to get the type of coefficients
  (generic, logistic, Poisson or proportional hazard) used by a model
  ([\#46](https://github.com/larmarange/broom.helpers/issues/46))
- [`tidy_add_contrasts()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_contrasts.md)
  and
  [`model_list_contrasts()`](https://larmarange.github.io/broom.helpers/reference/model_list_contrasts.md)
  now return an additional column `contrasts_type`
- New `no_reference_row` argument for
  [`tidy_add_reference_rows()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_reference_rows.md)
  ([\#47](https://github.com/larmarange/broom.helpers/issues/47))
- New method `model_get_nlevels` to get the number of levels of
  categorical variables
- New column `var_nlevels` returned by
  [`tidy_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_identify_variables.md),
  [`model_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/model_identify_variables.md)
  and
  [`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.md)
- Categorical terms can now be customized with a pattern taking into
  account term level, reference level and/or variable label, see
  [`model_list_terms_levels()`](https://larmarange.github.io/broom.helpers/reference/model_list_terms_levels.md)
  and `categorical_terms_pattern` in
  [`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.md)
  and `tidy_add_term_labels`
  ([\#61](https://github.com/larmarange/broom.helpers/issues/61))
- [`model_list_terms_levels()`](https://larmarange.github.io/broom.helpers/reference/model_list_terms_levels.md)
  now returns additional columns (`level`, `reference_level`,
  `contrasts_type`, `var_label`, `var_levels` and `dichotomous`)
- [`model_list_variables()`](https://larmarange.github.io/broom.helpers/reference/model_list_variables.md)
  now returns an additional `var_label` column
- The `exponentiate` argument is now passed to the `tidy_*()` functions,
  as an attribute attached to the tibble, as well as custom labels
  (`variable_labels` and `term_labels`)
- `show_single_row` argument now accepts tidyselect notation
  ([\#51](https://github.com/larmarange/broom.helpers/issues/51)
  [@ddsjoberg](https://github.com/ddsjoberg))
- [`tidy_add_estimate_to_reference_rows()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_estimate_to_reference_rows.md)
  now relies on `emmeans` for sum contrasts, allowing to cover a wider
  range of models
- Tibbles returned by `tidy_*` functions also inherits of
  `"broom.helpers"` class
  ([\#56](https://github.com/larmarange/broom.helpers/issues/56))
- `interaction_sep` argument has been added to
  [`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.md)
- Better management of variables with non standard names
  ([\#67](https://github.com/larmarange/broom.helpers/issues/67))
- [`.clean_backticks()`](https://larmarange.github.io/broom.helpers/reference/dot-clean_backticks.md)
  and
  [`.escape_regex()`](https://larmarange.github.io/broom.helpers/reference/dot-escape_regex.md)
  are now exported
- Bug fix for non standard variable names containing a character that
  would have a special meaning in a regular expression
  ([\#44](https://github.com/larmarange/broom.helpers/issues/44))
- Bug fix in
  [`tidy_add_header_rows()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_header_rows.md)
  for [`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html)
  models: label for header rows was missing
  ([\#50](https://github.com/larmarange/broom.helpers/issues/50))
- Bug fix: now
  [`tidy_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_identify_variables.md)
  correctly identify class “integer” for this type of variables
  ([\#57](https://github.com/larmarange/broom.helpers/issues/57))
- Bug fix for
  [`tidy_add_header_rows()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_header_rows.md)
  for continuous variables with a non standard name
  ([\#70](https://github.com/larmarange/broom.helpers/issues/70))

## broom.helpers 1.0.0

CRAN release: 2020-09-18

- Initial version
