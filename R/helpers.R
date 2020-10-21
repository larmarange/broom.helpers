#' Escapes any characters that would have special
#' meaning in a regular expression
#'
#' This functions has been adapted from `Hmisc::escapeRegex()`
#' @param string a character vector
#' @export
#' @family other_helpers
.escape_regex <- function (string)
{
  gsub(
    "([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1",
    string
  )
}



#' Remove backtips around variable names
#'
#' @param x a character vector of variable names to be cleaned
#' @param variable_names optional character vector of possible
#' variable names, for example obtained with
#' [model_list_variables(only_variable = TRUE)][model_list_variables()]
#'
#' @export
#' @family other_helpers
.clean_backticks <- function(x, variable_names = x) {
  variable_names <- variable_names %>%
    stats::na.omit() %>%
    unique() %>%
    .escape_regex()

  # cleaning existing backtips in variable_names
  variable_names <- ifelse(
    # does string starts and ends with backticks
    stringr::str_detect(variable_names, "^`.*`$"),
    # if yes remove first and last character of string
    stringr::str_sub(variable_names, 2, -2),
    # otherwise, return original string
    variable_names
  )

  # cleaning x, including interaction terms
  for (v in variable_names) {
    x <- stringr::str_replace_all(
      x,
      paste0("`", v, "`"),
      v
    )
  }
  x
}

