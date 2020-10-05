# broom.helpers (development version)

* Added support for multiple imputation models from the {mice} 
  package. The model passed must be the un-pooled models, and the 
  pooling step included in `tidy_fun=` (#49 @ddsjoberg) 
* New function `tidy_select_variables()` to keep/drop
  selected variables in the output (#45)
* New functions `tidy_add_coefficients_type()` and 
  `model_get_coefficients_type` to get the type of coefficients
  (generic, logistic, poisson or proportional hazard) used
  by a model (#46)
* New `no_reference_row` argument for `tidy_add_reference_rows()` (#47)
* The `exponentiate` argument is now passed to the `tidy_*()`
  functions, as an attribute attached to the tibble
* Bug fix for non standard variable names containing
  a character that would have a special meaning in
  a regular expression (#44)
* Bug fix in `tidy_add_header_rows()` for `nnet::multinom` models:
  label for header rows was missing (#50)

# broom.helpers 1.0.0

* Initial version
