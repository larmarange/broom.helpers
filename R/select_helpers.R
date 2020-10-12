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
#' res <- mod %>% tidy_plus_plus(exponentiate = TRUE)
#' res %>% tidy_select_variables(all_continuous())
#' res %>% tidy_select_variables(all_dichotomous())
#' res %>% tidy_select_variables(all_categorical())
#' res %>% tidy_select_variables(all_categorical(dichotomous = FALSE))
#' res %>% tidy_select_variables(all_interaction())
NULL

#' @rdname select_helpers
#' @export
all_continuous <- function() {
  env_variable_type$lst_variable_types %>%
    purrr::keep(~identical(., "continuous")) %>%
    names()
}

#' @rdname select_helpers
#' @export
all_dichotomous <- function() {
  env_variable_type$lst_variable_types %>%
    purrr::keep(~identical(., "dichotomous")) %>%
    names()
}

#' @rdname select_helpers
#' @export
all_categorical <- function(dichotomous = TRUE) {
  types <-
    switch(dichotomous, c("categorical", "dichotomous")) %||%
    "categorical"

  env_variable_type$lst_variable_types %>%
    purrr::keep(~. %in% types) %>%
    names()
}

#' @rdname select_helpers
#' @export
all_interaction <- function() {
  env_variable_type$lst_variable_types %>%
    purrr::keep(~identical(., "interaction")) %>%
    names()
}

