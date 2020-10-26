#' Select helper functions
#'
#' @description Set of functions to supplement the {tidyselect} set of
#' functions for selecting columns of data frames (and other items as well).
#' - `all_continuous()` selects continuous variables
#' - `all_categorical()` selects categorical (including `"dichotomous"`) variables
#' - `all_dichotomous()` selects only type `"dichotomous"`
#' - `all_interaction()` selects interaction terms from a regression model
#' - `all_intercepts()` selects intercept terms from a regression model
#' - `all_contrasts()` selects variables in regression model based on their type of contrast
#' @name select_helpers
#' @rdname select_helpers
#' @param dichotomous Logical indicating whether to include dichotomous variables.
#' Default is `TRUE`
#' @param type type of contrast to select. Must be one or more of
#' `c("treatment", "sum", "poly", "helmert")`. Default is all.
#' @return A character vector of column names selected
#' @examples
#' mod <- glm(response ~ age * trt + grade, gtsummary::trial, family = binomial)
#' res <- mod %>% tidy_plus_plus(exponentiate = TRUE, keep_model = TRUE)
#' res %>% tidy_select_variables(all_continuous())
#' res %>% tidy_select_variables(all_dichotomous())
#' res %>% tidy_select_variables(all_categorical())
#' res %>% tidy_select_variables(all_categorical(dichotomous = FALSE))
#' res %>% tidy_select_variables(all_interaction())
NULL

#' @rdname select_helpers
#' @export
all_continuous <- function() {
  .generic_selector("variable", "var_type",
                    .data$var_type %in% "continuous",
                    fun_name = "all_continuous")
}

#' @rdname select_helpers
#' @export
all_dichotomous <- function() {
  .generic_selector("variable", "var_type",
                    .data$var_type %in% "dichotomous",
                    fun_name = "all_dichotomous")
}

#' @rdname select_helpers
#' @export
all_categorical <- function(dichotomous = TRUE) {
  types <- switch(dichotomous, c("categorical", "dichotomous")) %||% "categorical"

  .generic_selector("variable", "var_type",
                    .data$var_type %in% types,
                    fun_name = "all_categorical")
}

#' @rdname select_helpers
#' @export
all_interaction <- function() {
  .generic_selector("variable", "var_type",
                    .data$var_type %in% "interaction",
                    fun_name = "all_interaction")
}

#' @rdname select_helpers
#' @export
all_intercepts <- function() {
  .generic_selector("variable", "var_type",
                    .data$var_type %in% "intercept",
                    fun_name = "all_intercepts")
}

#' @rdname select_helpers
#' @export
all_contrasts <- function(type = c("treatment", "sum", "poly", "helmert")) {
  type <- match.arg(type, several.ok = TRUE)
  contr.type <-
    purrr::map_chr(type,
                   ~switch(.x,
                           "treatment" = "contr.treatment",
                           "sum" = "contr.sum",
                           "poly" = "contr.poly",
                           "helmert" = "contr.helmert")
    )

  .generic_selector("variable", "contrasts",
                    .data$contrasts %in% contr.type,
                    fun_name = "all_contrasts")
}
