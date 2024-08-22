#' Scoping a tidy tibble allowing to tidy select
#'
#' This function uses the information from a model tidy tibble to generate
#' a data frame exposing the different variables of the model,
#' data frame that could be used for tidy selection. In addition, columns
#' `"var_type"`, `"var_class"` and `"contrasts_type"` are scoped and their
#' values are added as attributes to the data frame.
#' For example, if `var_type='continuous'` for variable `"age"`, then the
#' attribute `attr(.$age, 'gtsummary.var_type') <- 'continuous'` is set.
#' That attribute is then used in a selector like `all_continuous()`.
#' Note: attributes are prefixed with `"gtsummary."` to be compatible with
#' selectors provided by `{gtsummary}`.
#'
#' @param x a tidy tibble, with a `"variable"` column, as returned by
#' `tidy_identify_variables ()`
#' @param data an optional data frame the attributes will be added to
#' @return a data frame
#' @export
#' @examples
#' mod <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)
#' tt <- mod |> tidy_and_attach() |> tidy_add_contrasts()
#'
#' scope_tidy(tt) |> str()
#' scope_tidy(tt, data = model_get_model_frame(mod)) |> str()
#'
#' scope_tidy(tt) |> dplyr::select(dplyr::starts_with("Se")) |> names()
#' scope_tidy(tt) |> dplyr::select(where(is.factor)) |> names()
#' scope_tidy(tt) |> dplyr::select(all_continuous()) |> names()
#' scope_tidy(tt) |> dplyr::select(all_contrasts()) |> names()
#' scope_tidy(tt) |> dplyr::select(all_interaction()) |> names()
#' scope_tidy(tt) |> dplyr::select(all_intercepts()) |> names()
scope_tidy <- function(x, data = NULL) {
  if (!"variable" %in% names(x)) {
    cli::cli_abort(
      "The {.code .$x} data frame does not have the required {.val variable} column."
    )
  }

  # if data not passed, use table_body to construct one
  if (rlang::is_empty(data)) {
    data <- dplyr::tibble(!!!rlang::rep_named(unique(x$variable), logical(0L)))

    # if var_class available in x, convert colums
    if ("var_class" %in% names(x)) {
      df_class <- x[c("variable", "var_class")] |>
        unique() |>
        tidyr::drop_na()
      for (i in seq_len(nrow(df_class))) {
        f <- switch(
          df_class$var_class[i],
          "character" = as.character,
          "factor" = as.factor,
          "ordered" = as.ordered,
          "integer" = as.integer,
          "numeric" = as.numeric,
          "complex" = as.complex,
          "Date" = as.Date,
          "POSIXlt" = as.POSIXlt,
          "POSIXct" = as.POSIXct,
          "difftime" = as.difftime,
          as.logical
        )
        data[[df_class$variable[i]]] <- f(NA)
      }
    }
  }

  # only keeping rows that have corresponding column names in data
  x <- x |> dplyr::filter(.data$variable %in% names(data))

  # if x passed, add columns as attr to data
  base_attr_cols <- c("var_type", "var_class", "contrasts_type")
  attr_cols <- x |>
    dplyr::select(any_of(base_attr_cols)) |>
    names()

  # add attributes
  for (v in attr_cols) {
    df_attr <- x[c("variable", v)] |>
      unique() |>
      tidyr::drop_na()
    for (i in seq_len(nrow(df_attr))) {
      attr(data[[df_attr$variable[i]]], paste0("gtsummary.", v)) <- df_attr[[v]][i]
    }
  }

  # return data frame with attributes
  data
}
