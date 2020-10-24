#' Select helper functions
#'
#' Set of functions to supplement the {tidyselect} set of functions for selecting
#' columns of data frames.
#' @name select_helpers
#' @rdname select_helpers
#' @param dichotomous Logical indicating whether to include dichotomous variables.
#' Default is `TRUE`
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
all_treatment_contrasts <- function() {
  .generic_selector("variable", "contrasts",
                    .data$contrasts %in% "contr.treatment",
                    fun_name = "all_treatment_contrasts")
}

#' @rdname select_helpers
#' @export
all_sum_contrasts <- function() {
  .generic_selector("variable", "contrasts",
                    .data$contrasts %in% "contr.sum",
                    fun_name = "all_sum_contrasts")
}

#' @rdname select_helpers
#' @export
all_poly_contrasts <- function() {
  .generic_selector("variable", "contrasts",
                    .data$contrasts %in% "contr.poly",
                    fun_name = "all_poly_contrasts")
}

#' @rdname select_helpers
#' @export
all_helmert_contrasts <- function() {
  .generic_selector("variable", "contrasts",
                    .data$contrasts %in% "contr.helmert",
                    fun_name = "all_helmert_contrasts")
}



