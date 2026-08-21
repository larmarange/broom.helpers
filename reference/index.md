# Package index

## All-in-one function

- [`tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.md)
  : Tidy a model and compute additional informations
- [`supported_models`](https://larmarange.github.io/broom.helpers/reference/supported_models.md)
  : Listing of Supported Models

## Add to tidy tibbles

- [`tidy_attach_model()`](https://larmarange.github.io/broom.helpers/reference/tidy_attach_model.md)
  [`tidy_and_attach()`](https://larmarange.github.io/broom.helpers/reference/tidy_attach_model.md)
  [`tidy_get_model()`](https://larmarange.github.io/broom.helpers/reference/tidy_attach_model.md)
  [`tidy_detach_model()`](https://larmarange.github.io/broom.helpers/reference/tidy_attach_model.md)
  : Attach a full model to the tibble of model terms
- [`tidy_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_identify_variables.md)
  : Identify the variable corresponding to each model coefficient
- [`tidy_add_coefficients_type()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_coefficients_type.md)
  : Add coefficients type and label as attributes
- [`tidy_add_contrasts()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_contrasts.md)
  : Add contrasts type for categorical variables
- [`tidy_add_estimate_to_reference_rows()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_estimate_to_reference_rows.md)
  : Add an estimate value to references rows for categorical variables
- [`tidy_add_header_rows()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_header_rows.md)
  : Add header rows variables with several terms
- [`tidy_add_n()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_n.md)
  : Add the (weighted) number of observations
- [`tidy_add_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_pairwise_contrasts.md)
  : Add pairwise contrasts for categorical variables
- [`tidy_add_reference_rows()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_reference_rows.md)
  : Add references rows for categorical variables
- [`tidy_add_term_labels()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_term_labels.md)
  : Add term labels
- [`tidy_add_variable_labels()`](https://larmarange.github.io/broom.helpers/reference/tidy_add_variable_labels.md)
  : Add variable labels
- [`tidy_disambiguate_terms()`](https://larmarange.github.io/broom.helpers/reference/tidy_disambiguate_terms.md)
  : Disambiguate terms
- [`tidy_remove_intercept()`](https://larmarange.github.io/broom.helpers/reference/tidy_remove_intercept.md)
  : Remove intercept(s)
- [`tidy_select_variables()`](https://larmarange.github.io/broom.helpers/reference/tidy_select_variables.md)
  : Select variables to keep/drop
- [`tidy_group_by()`](https://larmarange.github.io/broom.helpers/reference/tidy_group_by.md)
  [`auto_group_by()`](https://larmarange.github.io/broom.helpers/reference/tidy_group_by.md)
  : Group results by selected columns

## Custom tidiers

- [`tidy_broom()`](https://larmarange.github.io/broom.helpers/reference/tidy_broom.md)
  :

  Tidy with
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) and
  checks that all arguments are used

- [`tidy_parameters()`](https://larmarange.github.io/broom.helpers/reference/tidy_parameters.md)
  : Tidy a model with parameters package

- [`tidy_with_broom_or_parameters()`](https://larmarange.github.io/broom.helpers/reference/tidy_with_broom_or_parameters.md)
  : Tidy a model with broom or parameters

- [`tidy_multgee()`](https://larmarange.github.io/broom.helpers/reference/tidy_multgee.md)
  :

  Tidy a `multgee` model

- [`tidy_zeroinfl()`](https://larmarange.github.io/broom.helpers/reference/tidy_zeroinfl.md)
  :

  Tidy a `zeroinfl` or a `hurdle` model

- [`tidy_vgam()`](https://larmarange.github.io/broom.helpers/reference/tidy_vgam.md)
  **\[experimental\]** :

  Tidy a `vglm` or a `vgam` model

- [`tidy_svy_vglm()`](https://larmarange.github.io/broom.helpers/reference/tidy_svy_vglm.md)
  **\[experimental\]** :

  Tidy a `svy_vglm` model

- [`tidy_coxphms()`](https://larmarange.github.io/broom.helpers/reference/tidy_coxphms.md)
  **\[experimental\]** : Tidy a multi-state survival model

## Tidiers for marginal predictions, contrasts, and effects

- [`tidy_marginal_predictions()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_predictions.md)
  [`variables_to_predict()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_predictions.md)
  [`plot_marginal_predictions()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_predictions.md)
  :

  Marginal Predictions with
  [`marginaleffects::avg_predictions()`](https://rdrr.io/pkg/marginaleffects/man/predictions.html)

- [`tidy_all_effects()`](https://larmarange.github.io/broom.helpers/reference/tidy_all_effects.md)
  :

  Marginal Predictions at the mean with
  [`effects::allEffects()`](https://rdrr.io/pkg/effects/man/effect.html)

- [`tidy_ggpredict()`](https://larmarange.github.io/broom.helpers/reference/tidy_ggpredict.md)
  :

  Marginal Predictions with
  [`ggeffects::ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.html)

- [`tidy_avg_comparisons()`](https://larmarange.github.io/broom.helpers/reference/tidy_avg_comparisons.md)
  :

  Marginal Contrasts with
  [`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html)

- [`tidy_marginal_contrasts()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_contrasts.md)
  [`variables_to_contrast()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_contrasts.md)
  :

  Marginal Contrasts with
  [`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html)

- [`tidy_avg_slopes()`](https://larmarange.github.io/broom.helpers/reference/tidy_avg_slopes.md)
  :

  Marginal Slopes / Effects with
  [`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)

- [`tidy_margins()`](https://larmarange.github.io/broom.helpers/reference/tidy_margins.md)
  **\[superseded\]** :

  Average Marginal Effects with
  [`margins::margins()`](https://rdrr.io/pkg/margins/man/margins.html)

## Get information from model objects

- [`model_compute_terms_contributions()`](https://larmarange.github.io/broom.helpers/reference/model_compute_terms_contributions.md)
  : Compute a matrix of terms contributions

- [`model_get_assign()`](https://larmarange.github.io/broom.helpers/reference/model_get_assign.md)
  : Get the assign attribute of model matrix of a model

- [`model_get_coefficients_type()`](https://larmarange.github.io/broom.helpers/reference/model_get_coefficients_type.md)
  : Get coefficient type

- [`model_get_contrasts()`](https://larmarange.github.io/broom.helpers/reference/model_get_contrasts.md)
  : Get contrasts used in the model

- [`model_get_model()`](https://larmarange.github.io/broom.helpers/reference/model_get_model.md)
  : Get the model from model objects

- [`model_get_model_frame()`](https://larmarange.github.io/broom.helpers/reference/model_get_model_frame.md)
  : Get the model frame of a model

- [`model_get_model_matrix()`](https://larmarange.github.io/broom.helpers/reference/model_get_model_matrix.md)
  : Get the model matrix of a model

- [`model_get_n()`](https://larmarange.github.io/broom.helpers/reference/model_get_n.md)
  : Get the number of observations

- [`model_get_nlevels()`](https://larmarange.github.io/broom.helpers/reference/model_get_nlevels.md)
  :

  Get the number of levels for each factor used in `xlevels`

- [`model_get_offset()`](https://larmarange.github.io/broom.helpers/reference/model_get_offset.md)
  : Get model offset

- [`model_get_pairwise_contrasts()`](https://larmarange.github.io/broom.helpers/reference/model_get_pairwise_contrasts.md)
  : Get pairwise comparison of the levels of a categorical variable

- [`model_get_response()`](https://larmarange.github.io/broom.helpers/reference/model_get_response.md)
  : Get model response

- [`model_get_response_variable()`](https://larmarange.github.io/broom.helpers/reference/model_get_response_variable.md)
  : Get the name of the response variable

- [`model_get_terms()`](https://larmarange.github.io/broom.helpers/reference/model_get_terms.md)
  : Get the terms of a model

- [`model_get_weights()`](https://larmarange.github.io/broom.helpers/reference/model_get_weights.md)
  : Get sampling weights used by a model

- [`model_get_xlevels()`](https://larmarange.github.io/broom.helpers/reference/model_get_xlevels.md)
  : Get xlevels used in the model

- [`model_identify_variables()`](https://larmarange.github.io/broom.helpers/reference/model_identify_variables.md)
  : Identify for each coefficient of a model the corresponding variable

- [`model_list_contrasts()`](https://larmarange.github.io/broom.helpers/reference/model_list_contrasts.md)
  : List contrasts used by a model

- [`model_list_higher_order_variables()`](https://larmarange.github.io/broom.helpers/reference/model_list_higher_order_variables.md)
  : List higher order variables of a model

- [`model_list_terms_levels()`](https://larmarange.github.io/broom.helpers/reference/model_list_terms_levels.md)
  : List levels of categorical terms

- [`model_list_variables()`](https://larmarange.github.io/broom.helpers/reference/model_list_variables.md)
  : List all the variables used in a model

## Variable type selectors

- [`all_continuous()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md)
  [`all_categorical()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md)
  [`all_dichotomous()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md)
  [`all_interaction()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md)
  [`all_ran_pars()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md)
  [`all_ran_vals()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md)
  [`all_intercepts()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md)
  [`all_contrasts()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.md)
  : Select helper functions
- [`scope_tidy()`](https://larmarange.github.io/broom.helpers/reference/scope_tidy.md)
  : Scoping a tidy tibble allowing to tidy select

## Exported utility functions

- [`.assert_package()`](https://larmarange.github.io/broom.helpers/reference/assert_package.md)
  [`.get_package_dependencies()`](https://larmarange.github.io/broom.helpers/reference/assert_package.md)
  [`.get_all_packages_dependencies()`](https://larmarange.github.io/broom.helpers/reference/assert_package.md)
  [`.get_min_version_required()`](https://larmarange.github.io/broom.helpers/reference/assert_package.md)
  : Check a package installation status or minimum required version
- [`.clean_backticks()`](https://larmarange.github.io/broom.helpers/reference/dot-clean_backticks.md)
  : Remove backticks around variable names
- [`.escape_regex()`](https://larmarange.github.io/broom.helpers/reference/dot-escape_regex.md)
  : Escapes any characters that would have special meaning in a regular
  expression
- [`seq_range()`](https://larmarange.github.io/broom.helpers/reference/seq_range.md)
  : Sequence generation between min and max

## Deprecated

- [`tidy_marginal_means()`](https://larmarange.github.io/broom.helpers/reference/tidy_marginal_means.md)
  **\[deprecated\]** :

  Marginal Means with deprecated `marginaleffects::marginal_means()`
