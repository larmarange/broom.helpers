#' Select variables to keep/drop
#'
#' Will remove unselected variables from the results.
#' To remove the intercept, use [tidy_remove_intercept()].
#'
#' @details
#' If the `variable` column is not yet available in `x`,
#' [tidy_identify_variables()] will be automatically applied.
#' @param x a tidy tibble
#' @param keep variables to keep. Use `-` to remove a variable.
#' Default is `everything()`
#' @param model the corresponding model, if not attached to `x`
#' @export
#' @family tidy_helpers
#' @examples
#' res <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived)) %>%
#'   glm(Survived ~ Class + Age + Sex, data = ., weights = .$n, family = binomial) %>%
#'   tidy_and_attach() %>%
#'   tidy_identify_variables()
#'
#' res
#' res %>% tidy_select_variables()
#' res %>% tidy_select_variables(keep = "Class")
#' res %>% tidy_select_variables(keep = -c("Age", "Sex"))
#' res %>% tidy_select_variables(keep = starts_with("A"))

tidy_select_variables <- function(
  x, keep = everything(), model = tidy_get_model(x)
) {
  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  if (!"variable" %in% names(x)) {
    x <- x %>% tidy_identify_variables(model = model)
  }
  .attributes <- .save_attributes(x)

  # obtain character vector of selected variables
  keep <- .tidy_tidyselect(x, {{ keep }})

  x %>%
    dplyr::filter(
      .data$var_type == "intercept" |
        .data$variable %in% keep
    ) %>%
    tidy_attach_model(model = model, .attributes = .attributes)

}

.tidy_tidyselect <- function(x, keep) {
  keep <- rlang::enquo(keep)

  # keeping variables and class
  df_vars <-
    x %>%
    dplyr::filter(!.data$var_type %in% "intercept") %>%
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
    !rlang::quo_is_symbol(keep) && # if not a symbol (ie name)
    identical(eval(as.list(rlang::quo_get_expr(keep)) %>% purrr::pluck(1)),
              dplyr::vars)

  # performing selecting
  if (select_input_starts_var) {
    # `vars()` evaluates to a list of quosures; unquoting them in `select()`
    res <- names(dplyr::select(df_empty, !!!rlang::eval_tidy(keep)))
  }
  else {
    res <- names(dplyr::select(df_empty, !!keep))
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

