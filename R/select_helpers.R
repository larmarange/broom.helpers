#' Select helper functions
#'
#' @description Set of functions to supplement the *tidyselect* set of
#' functions for selecting columns of data frames (and other items as well).
#' - `all_continuous()` selects continuous variables
#' - `all_categorical()` selects categorical (including `"dichotomous"`) variables
#' - `all_dichotomous()` selects only type `"dichotomous"`
#' - `all_interaction()` selects interaction terms from a regression model
#' - `all_intercepts()` selects intercept terms from a regression model
#' - `all_contrasts()` selects variables in regression model based on their type
#'   of contrast
#' - `all_ran_pars()` and `all_ran_vals()` for random-effect parameters and
#'   values from a mixed model
#'   (see `vignette("broom_mixed_intro", package = "broom.mixed")`)
#' @name select_helpers
#' @rdname select_helpers
#' @param dichotomous (`logical`)\cr
#' Whether to include dichotomous variables, default is `TRUE`.
#' @param contrasts_type (`string`)\cr
#' Type of contrast to select. When `NULL`, all variables with a
#' contrast will be selected. Default is `NULL`. Select among contrast types
#' `c("treatment", "sum", "poly", "helmert", "sdif", "other")`.
#' @param continuous2 (`logical`)\cr
#' Whether to include continuous2 variables, default is `TRUE`.
#' For compatibility with `{gtsummary}`), see [`gtsummary::all_continuous2()`].
#'
#' @return A character vector of column names selected.
#' @seealso [scope_tidy()]
#' @examples
#' \donttest{
#' glm(response ~ age * trt + grade, gtsummary::trial, family = binomial) |>
#'   tidy_plus_plus(exponentiate = TRUE, include = all_categorical())
#'
#' glm(response ~ age + trt + grade + stage,
#'   gtsummary::trial,
#'   family = binomial,
#'   contrasts = list(trt = contr.SAS, grade = contr.sum, stage = contr.poly)
#' ) |>
#'   tidy_plus_plus(
#'     exponentiate = TRUE,
#'     include = all_contrasts(c("treatment", "sum"))
#'   )
#' }
NULL

#' @rdname select_helpers
#' @export
all_continuous <- function(continuous2 = TRUE) {
  types <- if (continuous2) c("continuous", "continuous2") else "continuous"

  where(function(x) isTRUE(attr(x, "gtsummary.var_type") %in% types))
}

#' @rdname select_helpers
#' @export
all_categorical <- function(dichotomous = TRUE) {
  types <- if (dichotomous) c("categorical", "dichotomous") else "categorical"

  where(function(x) isTRUE(attr(x, "gtsummary.var_type") %in% types))
}

#' @rdname select_helpers
#' @export
all_dichotomous <- function() {
  where(function(x) isTRUE(attr(x, "gtsummary.var_type") %in% "dichotomous"))
}

#' @rdname select_helpers
#' @export
all_interaction <- function() {
  where(function(x) isTRUE(attr(x, "gtsummary.var_type") %in% "interaction"))
}

#' @rdname select_helpers
#' @export
all_ran_pars <- function() {
  where(function(x) isTRUE(attr(x, "gtsummary.var_type") %in% "ran_pars"))
}

#' @rdname select_helpers
#' @export
all_ran_vals <- function() {
  where(function(x) isTRUE(attr(x, "gtsummary.var_type") %in% "ran_vals"))
}

#' @rdname select_helpers
#' @export
all_intercepts <- function() {
  where(function(x) isTRUE(attr(x, "gtsummary.var_type") %in% "intercept"))
}

#' @rdname select_helpers
#' @export
all_contrasts <- function(contrasts_type = c("treatment", "sum", "poly", "helmert", "sdif", "other")) { # nolint
  contrasts_type <- rlang::arg_match(contrasts_type, multiple = TRUE)
  where(function(x) isTRUE(attr(x, "gtsummary.contrasts_type") %in% contrasts_type))
}
