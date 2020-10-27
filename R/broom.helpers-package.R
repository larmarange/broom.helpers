## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
#' @importFrom dplyr .data
#' @importFrom purrr %||%
## usethis namespace: end
NULL

# because `where` is not exported by tidyselect
# cf. https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables(c(".", "where"))

# update named vectors, y values overriding x values if common name
.update_vector <- function(x, y) {
  if (is.null(y)) {
    return(x)
  }
  if (is.null(names(y)) || any(names(y) == "")) {
    stop("All elements of y should be named.")
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
# .superscript_numbers(0:20)
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
  x %>% stringr::str_replace_all(pattern)
}


# for consistent column order
.order_tidy_columns <- function(x) {
  x %>%
    dplyr::select(
      dplyr::any_of(
        c(
          "y.level", "term", "variable", "var_label", "var_class", "var_type",
          "var_nlevels", "header_row", "contrasts", "reference_row", "label"
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
      "exponentiate", "coefficients_type", "coefficients_label",
      "variable_labels", "term_labels", "N", "Nevent", "Exposure"
      )
  )
  .attributes[.attributes_names]
}

# tidyselect helper to be used in tidy_*() functions
.tidy_tidyselect <- function(x, include) {
  include <- rlang::enquo(include)

  # scoping the variable types
  .scope_variable_type(x)

  # keeping variables and class
  df_vars <-
    x %>%
    dplyr::filter(!.data$var_type %in% "intercept") %>%
    dplyr::filter(!is.na(.data$variable)) %>%
    dplyr::select(.data$variable, .data$var_class) %>%
    dplyr::distinct()

  df_empty <-
    purrr::map2_dfc(
      df_vars$variable, df_vars$var_class,
      function(var, class) {
        # assigning variable type/class so user may use
        # `where(is.character)` type selectors
        switch(
          class,
          "numeric" = data.frame(NA_real_),
          "character" = data.frame(NA_character_),
          "factor" = data.frame(factor(NA)),
          "ordered" = data.frame(factor(NA, ordered = TRUE)),
          "integer" = data.frame(NA_integer_)
        ) %||%
          data.frame(NA) %>%
          purrr::set_names(var)
      }
    )

  # determine if selecting input begins with `var()`
  select_input_starts_var <-
    !rlang::quo_is_symbol(include) && # if not a symbol (ie name)
    identical(eval(as.list(rlang::quo_get_expr(include)) %>% purrr::pluck(1)),
              dplyr::vars)

  # performing selecting
  if (select_input_starts_var) {
    # `vars()` evaluates to a list of quosures; unquoting them in `select()`
    res <- names(dplyr::select(df_empty, !!!rlang::eval_tidy(include)))
  }
  else {
    res <- names(dplyr::select(df_empty, !!include))
  }

  res
}


#' Copy of tidyselect's unexported `where()` function
#'
#' Need this function when we do checks if the select helpers are wrapped in `var()`.
#' If it is not present, users cannot use `where(is.numeric)` type selectors.
#' tidyselect maintainers have indicated they will export `where()` in a future
#' release so this will not be required
#' @noRd
where <- function(fn) {
  predicate <- rlang::as_function(fn)

  function(x, ...) {
    out <- predicate(x, ...)

    if (!rlang::is_bool(out)) {
      rlang::abort("`where()` must be used with functions that return `TRUE` or `FALSE`.")
    }

    out
  }
}

# set new environment for new tidyselect funs
env_variable_type <- rlang::new_environment()

# scoping the variable types
.scope_variable_type <- function(x) {
  # removing everything from selecting environment
  rm(list = ls(envir = env_variable_type), envir = env_variable_type)

  # if variable and var_type not in tibble, exit scoping function
  if (!all(c("variable", "var_type") %in% names(x)))
    return(invisible(NULL))

  # saving list of variable types to selecting environment
  df_var_types <-
    x %>%
    dplyr::select(.data$variable, .data$var_type) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::distinct()

  env_variable_type$lst_variable_types <-
    df_var_types$var_type %>% purrr::set_names(df_var_types$variable)

  return(invisible(NULL))
}
