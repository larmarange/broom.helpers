#' Check a package installation status or minimum required version
#'
#' The function `.assert_package()` checks whether a package is installed and
#' returns an error or `FALSE` if not available. If a package search is provided,
#' the function will check whether a minimum version of a package is required.
#' The function `.get_package_dependencies()` returns a tibble with all
#' dependencies of a specific package. Finally, `.get_min_version_required()`
#' will return, if any, the minimum version of `pkg` required by `pkg_search`,
#' `NULL` if no minimum version required.
#'
#' @param pkg Package required
#' @param fn Calling function from the user perspective. Used to write
#' informative error messages.
#' @param pkg_search the package the function will search for a minimum
#' required version from.
#' @param boolean logical indicating whether to return a `TRUE`/`FALSE`, rather
#' than error when package/package version not available. Default is `FALSE`,
#' which will return an error if `pkg` is not installed.
#' @param remove_duplicates if several versions of a package are installed,
#' should only the first one be returned?
#' @param lib.loc location of `R` library trees to search through, see
#' `utils::installed.packages()`.
#' @details
#' `.get_package_dependencies()` accepts `pkg_search = NULL` and will return, in
#' that case, the list of dependencies of all installed packages.
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
.get_package_dependencies <- function(
    pkg_search = "broom.helpers",
    remove_duplicates = FALSE,
    lib.loc = NULL) {
  deps <-
    utils::installed.packages(lib.loc = lib.loc) %>%
    tibble::as_tibble() %>%
    dplyr::select(dplyr::all_of(
      c("Package", "Version", "LibPath", "Imports", "Depends", "Suggests", "Enhances", "LinkingTo")
    )) %>%
    dplyr::rename(
      pkg_search = "Package",
      pkg_search_version = "Version",
      lib_path = "LibPath"
    )

  if (!is.null(pkg_search))
    deps <- deps %>% dplyr::filter(.data$pkg_search %in% .env$pkg_search)
  if (remove_duplicates)
    deps <- deps %>% dplyr::distinct(.data$pkg_search, .keep_all = TRUE)

  deps %>%
    tidyr::pivot_longer(
      c(-.data$pkg_search, -.data$pkg_search_version, -.data$lib_path),
      values_to = "pkg",
      names_to = "dependency_type"
    ) %>%
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
  res <- .get_package_dependencies(pkg_search, remove_duplicates = TRUE) %>%
    dplyr::filter(.data$pkg == .env$pkg & !is.na(.data$version))
  version <- res %>% purrr::pluck("version")
  attr(version, "compare") <- res %>% purrr::pluck("compare")
  if (length(version) >= 1) names(version) <- res %>% purrr::pluck("dependency_type")
  version
}
