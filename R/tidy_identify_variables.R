#' Identify the variable corresponding to each model coefficient
#'
#' `tidy_identify_variables()` will add to the tidy tibble
#' three additional columns: `variable`, `var_class` and `var_type`.
#'
#' It will also identify interaction terms and intercept(s).
#' `var_type` could be `"continuous"`, `"categorical"`, `"intercept"`
#' or `"interaction"`.
#' @param x a tidy tibble
#' @param model the corresponding model, if not attached to `x`
#' @export
#' @seealso [model_identify_variables()]
#' @family tidy_helpers
#' @examples
#' Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) %>%
#'   glm(Survived ~ Class + Age * Sex, data = ., weights = .$n, family = binomial) %>%
#'   tidy_and_attach() %>%
#'   tidy_identify_variables()
#'
#' lm(
#'   Sepal.Length ~ poly(Sepal.Width, 2) + Species,
#'   data = iris,
#'   contrasts = list(Species = contr.sum)
#' ) %>%
#' tidy_and_attach(conf.int = TRUE) %>%
#' tidy_identify_variables()
tidy_identify_variables <- function(x, model = tidy_get_model(x)) {
  if (is.null(model))
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")

  if ("header_row" %in% names(x))
    stop("`tidy_identify_variables()` cannot be applied after `tidy_add_header_rows().`")

  if ("variable" %in% names(x))
    x <- dplyr::select(-.data$variable, -.data$var_class, -.data$var_type)

  variables_list <- model_identify_variables(model)

  # clean unconventional variable names
  x$term <- .clean_backtips(x$term, variables_list$variable)

  x %>%
    dplyr::left_join(variables_list, by = "term") %>%
    dplyr::select(
      dplyr::any_of("y.level"), # for multinom models
      .data$term, .data$variable, .data$var_class, .data$var_type,
      dplyr::everything()
    ) %>%
    dplyr::mutate(
      var_type = dplyr::if_else(
        is.na(.data$variable),
        "intercept",
        .data$var_type
      ),
      # specific case of polynomial terms defined with poly()
      variable = stringr::str_replace(.data$variable, "^poly\\((.*),(.*)\\)$", "\\1")
    ) %>%
    tidy_attach_model(model) %>%
    .order_tidy_columns()
}

