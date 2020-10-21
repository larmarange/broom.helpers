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
