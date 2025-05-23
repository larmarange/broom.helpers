#' Identify the variable corresponding to each model coefficient
#'
#' `tidy_identify_variables()` will add to the tidy tibble
#' three additional columns: `variable`, `var_class`, `var_type` and `var_nlevels`.
#'
#' It will also identify interaction terms and intercept(s).
#'
#' `var_type` could be:
#'
#' * `"continuous"`,
#' * `"dichotomous"` (categorical variable with 2 levels),
#' * `"categorical"` (categorical variable with 3 levels or more),
#' * `"intercept"`
#' * `"interaction"`
#' * `"ran_pars` (random-effect parameters for mixed models)
#' * `"ran_vals"` (random-effect values for mixed models)
#' * `"unknown"` in the rare cases where `tidy_identify_variables()`
#'   will fail to identify the list of variables
#'
#' For dichotomous and categorical variables, `var_nlevels` corresponds to the number
#' of original levels in the corresponding variables.
#'
#' For `fixest` models, a new column `instrumental` is added to indicate
#' instrumental variables.
#' @param x (`data.frame`)\cr
#' A tidy tibble as produced by `tidy_*()` functions.
#' @param model (a model object, e.g. `glm`)\cr
#' The corresponding model, if not attached to `x`.
#' @inheritParams tidy_plus_plus
#' @export
#' @seealso [model_identify_variables()]
#' @family tidy_helpers
#' @examples
#' df <- Titanic |>
#'   dplyr::as_tibble() |>
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#' glm(
#'   Survived ~ Class + Age * Sex,
#'   data = df,
#'   weights = df$n,
#'   family = binomial
#' ) |>
#'   tidy_and_attach() |>
#'   tidy_identify_variables()
#'
#' lm(
#'   Sepal.Length ~ poly(Sepal.Width, 2) + Species,
#'   data = iris,
#'   contrasts = list(Species = contr.sum)
#' ) |>
#'   tidy_and_attach(conf.int = TRUE) |>
#'   tidy_identify_variables()
tidy_identify_variables <- function(x, model = tidy_get_model(x),
                                    quiet = FALSE) {
  if (is.null(model)) {
    cli::cli_abort(c(
      "{.arg model} is not provided.",
      "You need to pass it or to use {.fn tidy_and_attach}."
    ))
  }

  if ("header_row" %in% names(x)) {
    cli::cli_abort(paste(
      "{.fn tidy_identify_variables} cannot be applied",
      "after {.fn tidy_add_header_rows}."
    ))
  }

  .attributes <- .save_attributes(x)

  # specific case for fixest models to handle instrumental variables
  if (inherits(model, "fixest")) {
    x <- x |>
      dplyr::mutate(
        original_term = .data$term,
        instrumental = .data$term |> stringr::str_starts("fit_"),
        term = dplyr::if_else(
          .data$term |> stringr::str_starts("fit_"),
          .data$term |> stringr::str_sub(5),
          .data$term
        )
      )
  }

  # specific case for marginal means / effects / predictions / contrasts
  if (
    isTRUE(
      stringr::str_starts(.attributes$coefficients_type, "marginal") &&
        "variable" %in% names(x)
    )
  ) {
    x <- x |>
      dplyr::left_join(
        model_list_variables(model, add_var_type = TRUE),
        by = "variable"
      ) |>
      tidy_attach_model(model = model, .attributes = .attributes)
    return(x)
  }

  if ("variable" %in% names(x)) {
    x <- x |> dplyr::select(
      -any_of(c("variable", "var_class", "var_type", "var_nlevels"))
    )
  }

  variables_list <- model_identify_variables(model)

  if (nrow(variables_list) > 0) {
    x <- x |>
      dplyr::left_join(variables_list, by = "term")

    # management of random parameters (mixed models)
    if ("effect" %in% names(x)) {
      x <- x |>
        dplyr::mutate(
          var_type = dplyr::if_else(
            .data$effect %in% c("ran_pars", "ran_vals", "random"),
            .data$effect,
            .data$var_type
          )
        )
    }

    x |>
      dplyr::mutate(
        var_type = dplyr::if_else(
          is.na(.data$var_type),
          "intercept",
          .data$var_type
        ),
        variable = dplyr::if_else(
          is.na(.data$variable),
          .data$term,
          .data$variable
        )
      ) |>
      tidy_attach_model(model = model, .attributes = .attributes)
  } else {
    if (!quiet) {
      cli_alert_danger(paste0(
        "Unable to identify the list of variables.\n\n",
        "This is usually due to an error calling {.code stats::model.frame(x)}",
        "or {.code stats::model.matrix(x)}.\n",
        "It could be the case if that type of model does not implement these methods.\n",
        "Rarely, this error may occur if the model object was created within\na ",
        "functional programming framework (e.g. using {.code lappy()}, ",
        "{.code purrr::map()}, etc.)."
      ))
    }

    x |>
      dplyr::mutate(
        variable = .data$term,
        var_class = NA_integer_,
        var_type = "unknown",
        var_nlevels = NA_integer_
      ) |>
      tidy_attach_model(model = model, .attributes = .attributes)
  }
}
