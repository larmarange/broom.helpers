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
#' .get_package_dependencies()
#' .get_min_version_required("brms")
NULL

#' @rdname assert_package
#' @export
.assert_package <- function(pkg, fn = NULL, pkg_search = "broom.helpers", boolean = FALSE) {
  # check if min version is required -------------------------------------------
  version <- .get_min_version_required(pkg, pkg_search)
  compare <- purrr::attr_getter("compare")(version)

  # check installation TRUE/FALSE ----------------------------------------------
  if (isTRUE(boolean)) {
    return(rlang::is_installed(pkg = pkg, version = version, compare = compare))
  }

  # prompt user to install package ---------------------------------------------
  rlang::check_installed(
    pkg = pkg,
    version = version,
    compare = compare,
    reason = switch(!is.null(fn), stringr::str_glue("for `{fn}`"))
  )
  invisible()
}

#' @rdname assert_package
#' @export
.get_package_dependencies <- function(pkg_search = "broom.helpers") {
  utils::installed.packages() %>%
    tibble::as_tibble() %>%
    dplyr::filter(.data$Package %in% .env$pkg_search) %>%
    dplyr::select(dplyr::all_of(c("Package", "Imports", "Depends", "Suggests", "Enhances", "LinkingTo"))) %>%
    dplyr::rename(pkg_search = "Package") %>%
    tidyr::pivot_longer(-.data$pkg_search, values_to = "pkg", names_to = "dependency_type") %>%
    tidyr::separate_rows(.data$pkg, sep = ",") %>%
    dplyr::mutate(pkg = stringr::str_squish(.data$pkg)) %>%
    dplyr::filter(!is.na(.data$pkg)) %>%
    tidyr::separate(.data$pkg, into = c("pkg", "version"), sep = " ", extra = "merge", fill = "right") %>%
    dplyr::mutate(
      compare = .data$version %>% stringr::str_extract(pattern = "[>=<]+"),
      version = .data$version %>% stringr::str_remove_all(pattern = "[\\(\\) >=<]")
    )
}

#' @rdname assert_package
#' @export
.get_min_version_required <- function(pkg, pkg_search = "broom.helpers") {
  if (is.null(pkg_search)) return(NULL)
  res <- .get_package_dependencies(pkg_search) %>%
    dplyr::filter(.data$pkg == .env$pkg & !is.na(.data$version))
  version <- res %>% purrr::pluck("version")
  attr(version, "compare") <- res %>% purrr::pluck("compare")
  version
}
