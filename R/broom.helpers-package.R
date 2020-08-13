## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
#' @importFrom dplyr .data
## usethis namespace: end
NULL

#' @importFrom dplyr `%>%`
#' @export
dplyr::`%>%`

# remove backtips around variable names
.clean_backtips <- function(x, variable_names = x) {
  for (i in stats::na.omit(variable_names)) {
    x <- stringr::str_replace_all(x, paste0("`", i, "`"), i)

    if (stringr::str_detect(i, "^`.*`$"))
      x <- stringr::str_replace_all(x, i, stringr::str_sub(i, 2, -2))
  }
  x
}

# update named vectors, y values overriding x values if common name
.update_vector <- function(x, y) {
  if (is.null(y))
    return(x)
  if (is.null(names(y)) || any(names(y) == ""))
    stop("All elements of y should be named.")
  for (i in names(y)) {
    if (utils::hasName(x, i)) {
      x[i] <- y[i]
    } else {
      x <- c(x, y[i])
    }
  }
  x
}
