#' Tidy a model with parameters package
#'
#' Use `parameters::model_parameters()` to tidy a model and apply
#' `parameters::standardize_names(style = "broom")` to the output
#' @param x a model
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param ... additional parameters passed to `parameters::model_parameters()`
#' @examples
#' lm(Sepal.Length ~ Sepal.Width + Species, data = iris) %>%
#'   tidy_parameters()
#' \dontrun {
#'   lm(Sepal.Length ~ Sepal.Width + Species, data = iris) %>%
#'     tidy_parameters(bootstrap = TRUE)
#' }
#' @export
tidy_parameters <- function(x, conf.int = TRUE, conf.level = .95, ...) {
  if (!requireNamespace("parameters"))
    stop("You need to install 'parameters' to use 'tidy_parameters'.")

  if (!conf.int) conf.level <- NULL

  x %>%
    parameters::model_parameters(ci = conf.level, ...) %>%
    parameters::standardize_names(style = "broom")
}


#' Escapes any characters that would have special
#' meaning in a regular expression
#'
#' This functions has been adapted from `Hmisc::escapeRegex()`
#' @param string a character vector
#' @export
#' @family other_helpers
.escape_regex <- function(string) {
  gsub(
    "([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1",
    string
  )
}



#' Remove backticks around variable names
#'
#' @param x a character vector to be cleaned
#' @param variable_names list of variable names,
#' could be obtained with
#' [model_list_variables(only_variable = TRUE)][model_list_variables()]
#' to properly take into account interaction only terms/variables
#'
#' @export
#' @family other_helpers
.clean_backticks <- function(x, variable_names = x) {
  saved_names <- names(x)

  variable_names <- variable_names %>%
    stats::na.omit() %>%
    unique() %>%
    .escape_regex()

  # cleaning existing backticks in variable_names
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
  names(x) <- saved_names
  x
}

