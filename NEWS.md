# broom.helpers (development version)

* Added support for multiple imputation models from the {mice} package. The model passed must be the un-pooled models, and the pooling step included in `tidy_fun=`. 

* Bug fix for non standard variable names containing
  a character that would have a special meaning in
  a regular expression (#44)

# broom.helpers 1.0.0

* Initial version
