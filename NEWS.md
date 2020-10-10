# broom.helpers (development version)

* Breaking change: column `var_type` returned by `tidy_identify_variables`
  is now equal to `"dichotomous"` for categorical variables with only
  2 levels
* Added support for multiple imputation models from the {mice} 
  package. The model passed must be the un-pooled models, and the 
  pooling step included in `tidy_fun=` (#49 @ddsjoberg) 
* New function `tidy_select_variables()` to keep/drop
  selected variables in the output (#45)
* New functions `tidy_add_coefficients_type()` and 
  `model_get_coefficients_type` to get the type of coefficients
  (generic, logistic, Poisson or proportional hazard) used
  by a model (#46)
* New `no_reference_row` argument for `tidy_add_reference_rows()` (#47)
* The `exponentiate` argument is now passed to the `tidy_*()`
  functions, as an attribute attached to the tibble
* `show_single_row` argument now accepts tidyselect notation (#51 @ddsjoberg)
* New method `model_get_nlevels` to get the number of levels of categorical variables
* New column `var_nlevels` returned by `tidy_identify_variables()`,
  `model_identify_variables()` and `tidy_plus_plus()`
* Bug fix for non standard variable names containing
  a character that would have a special meaning in
  a regular expression (#44)
* Bug fix in `tidy_add_header_rows()` for `nnet::multinom` models:
  label for header rows was missing (#50)
* Bug fix: now `tidy_identify_variables()` correctly identify class "integer"
  for this type of variables (#57)

# broom.helpers 1.0.0

* Initial version
