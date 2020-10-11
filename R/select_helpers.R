#' Select helper functions
#'
#' Set of functions to supplement the {tidyselect} set of functions for selecting
#' columns of data frames.
#' @name select_helpers
#' @rdname select_helpers
#' @param dichotomous Logical indicating whether to include dichotomous variables.
#' Default is `TRUE`
#' @return A character vector of column names selected
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

