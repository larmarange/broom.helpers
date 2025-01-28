## usethis namespace: start
#' @importFrom cli cli_alert_info cli_alert_info cli_alert_danger cli_code cli_ul
#' @importFrom rlang .data .env
#' @importFrom purrr %||%
## usethis namespace: end
NULL

# because `where` is not exported by tidyselect
# cf. https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables(c("."))

# update named vectors, y values overriding x values if common name
.update_vector <- function(x, y) {
  if (is.null(y)) {
    return(x)
  }
  if (is.null(names(y)) || any(names(y) == "")) {
    cli::cli_abort("All elements of y should be named.")
  }
  for (i in names(y)) {
    if (utils::hasName(x, i)) {
      x[i] <- y[i]
    } else {
      x <- c(x, y[i])
    }
  }
  x
}

# return superscript character
.superscript_numbers <- function(x) {
  if (!is.character(x)) {
    x <- as.character(x)
  }
  x[x == "1"] <- "" # do not show when equal 1
  pattern <- c(
    "0" = "\u2070", "1" = "\u00b9", "2" = "\u00b2",
    "3" = "\u00b3", "4" = "\u2074", "5" = "\u2075",
    "6" = "\u2076", "7" = "\u2077", "8" = "\u2078",
    "9" = "\u2079"
  )
  x |> stringr::str_replace_all(pattern)
}


# for consistent column order
.order_tidy_columns <- function(x) {
  x |>
    dplyr::select(
      dplyr::any_of(
        c(
          "y.level", "component", "term", "original_term", "variable",
          "instrumental", "var_label", "var_class", "var_type",
          "var_nlevels", "header_row", "contrasts", "contrasts_type",
          "reference_row", "label", "n_obs", "n_ind", "n_event", "exposure"
        )
      ),
      dplyr::everything()
    )
}

# attributes to be saved between tidy_* functions
.save_attributes <- function(x) {
  .attributes <- attributes(x)
  .attributes_names <- intersect(
    names(.attributes),
    c(
      "exponentiate", "conf.level", "coefficients_type", "coefficients_label",
      "variable_labels", "term_labels", "N_obs", "N_ind", "N_event", "Exposure",
      "force_contr.treatment", "skip_add_reference_rows",
      "find_missing_interaction_terms", "component"
    )
  )
  .attributes[.attributes_names]
}

#' Sequence generation between min and max
#'
#' @param x (`numeric`)\cr
#' A numeric vector.
#' @param length.out (`integer`)\cr
#' Desired length of the sequence (a positive integer).
#' @details
#' `seq_range(x, length.out)` is a shortcut for
#' `seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = length.out)`
#' @return
#' a numeric vector
#' @export
#' @examples
#' seq_range(iris$Petal.Length)
seq_range <- function(x, length.out = 25) {
  seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = length.out)
}
