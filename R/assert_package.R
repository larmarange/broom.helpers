#' Check a package installation status or minimum required version
#'
#' The function checks whether a package is installed and returns an error
#' or `FALSE` if not available. If a package search is provided, the function
#' will check whether a minimum version of a package is required.
#'
#' @param pkg Package required
#' @param fn Calling function from the user perspective. Used to write
#' informative error messages.
#' @param pkg_search the package the function will search for a minimum
#' required version from.
#' @param boolean logical indicating whether to return a `TRUE`/`FALSE`, rather
#' than error when package/package version not available. Default is `FALSE`,
#' which will return an error if pkg is not installed.
#'
#' @return logical or error
#' @name assert_package
#' @examples
#' .assert_package("broom", boolean = TRUE)
#'
#' .get_min_version_required("brms")
NULL

#' @rdname assert_package
#' @export
.assert_package <- function(pkg, fn = NULL, pkg_search = "broom.helpers", boolean = FALSE) {
  # check if min version is required -------------------------------------------
  version <- .get_min_version_required(pkg, pkg_search)

  # check installation TRUE/FALSE ----------------------------------------------
  if (isTRUE(boolean)) {
    return(rlang::is_installed(pkg = pkg, version = version))
  }

  # prompt user to install package ---------------------------------------------
  rlang::check_installed(
    pkg = pkg,
    version = version,
    reason = switch(!is.null(fn), stringr::str_glue("for `{fn}`"))
  )
  invisible()
}

#' @rdname assert_package
#' @export
.get_min_version_required <- function(pkg, pkg_search = "broom.helpers") {
  if (is.null(pkg_search)) return(NULL)
  # get min version required for a Suggested package in gtsummary
  utils::packageDescription(
    pkg_search,
    fields = c("Imports", "Depends", "Suggests", "Enhances", "LinkingTo")
  ) %>%
    purrr::map(
      ~stringr::str_remove_all(.x, "[\r\n]") %>%
        stringr::str_split(pattern = stringr::fixed(",")) %>%
        unlist() %>%
        {.[stringr::word(.) == pkg]} %>%
        stringr::word(start = 2, end = -1) %>%
        stringr::str_remove_all(pattern = " ") %>%
        stringr::str_remove_all(pattern = "^\\(>=|\\)$") %>%
        {switch(!rlang::is_empty(.) && !is.na(.), .)}
    ) %>%
    unlist()

}
