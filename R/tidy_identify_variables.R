#' Identify the variable corresponding to each model term
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
  model_identify_variables(model) %>%
    dplyr::right_join(x, by = "term") %>%
    dplyr::select(.data$term, dplyr::everything()) %>%
    tidy_attach_model(model)
}

#' Identify for each term of a model the corresponding variable
#'
#' It will also identify interaction terms and intercept(s).
#' @param model a model object
#' @return
#' A tibble with four columns:
#' * `term`: coefficients of the model
#' * `variable`: the corresponding variable
#' * `var_class`: class of the variable (cf. [stats::.MFclass()])
#' * `var_type`: `"continuous"`, `"categorical"`, `"intercept"`
#'   or `"interaction"`
#' @export
#' @family model_helpers
#' @seealso [tidy_identify_variables()]
#' @examples
#' Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) %>%
#'   glm(Survived ~ Class + Age * Sex, data = ., weights = .$n, family = binomial) %>%
#'   model_identify_variables()
#'
#' lm(
#'   Sepal.Length ~ poly(Sepal.Width, 2) + Species,
#'   data = iris,
#'   contrasts = list(Species = contr.sum)
#' ) %>%
#'   model_identify_variables()
model_identify_variables <- function(model) {
  UseMethod("model_identify_variables")
}

#' @rdname model_identify_variables
#' @export
model_identify_variables.default <- function(model) {
  assign <- attr(stats::model.matrix(model), "assign")
  assign[assign == 0] <- NA
  term.labels <- attr(stats::terms(model), "term.labels")
  dataClasses <- attr(stats::terms(model), "dataClasses")

  if (is.null(dataClasses)) {
    mf <- stats::model.frame(model)
    dataClasses <- purrr::map(mf, stats::.MFclass) %>% unlist()
  }

  tibble::tibble(
    term = rownames(summary(model)$coefficients),
    variable = term.labels[assign]
  ) %>%
    dplyr::left_join(
      tibble::tibble(
        variable = names(dataClasses),
        var_class = dataClasses
      ),
      by = "variable"
    ) %>%
    dplyr::mutate(
      var_type = dplyr::case_when(
        is.na(.data$variable) ~ "intercept",
        .data$var_class %in% c("factor", "character", "logical") ~ "categorical",
        !is.na(.data$var_class) ~ "continuous",
        is.na(.data$var_class) & stringr::str_detect(.data$variable, ":") ~ "interaction"
      )
    )
}
