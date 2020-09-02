#' List all the variables used in a model
#'
#' Including variables used only in an interaction.
#'
#' @param model a model object
#' @return
#' A tibble with three columns:
#' * `variable`: the corresponding variable
#' * `var_class`: class of the variable (cf. [stats::.MFclass()])
#' * `label_attr`: variable label defined in the original data frame
#'    with the label attribute (cf. [labelled::var_label()])
#'
#' @export
#' @family model_helpers
#' @examples
#' Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) %>%
#'   glm(
#'     Survived ~ Class + Age : Sex,
#'     data = ., weights = .$n,
#'     family = binomial
#'   ) %>%
#'   model_list_variables()
#'
#' iris %>%
#'   lm(
#'     Sepal.Length ~ poly(Sepal.Width, 2) + Species,
#'     data = .,
#'     contrasts = list(Species = contr.sum)
#'   ) %>%
#'   model_list_variables()
#'
#' if (requireNamespace("gtsummary")) {
#'   glm(
#'     response ~ poly(age, 3) + stage + grade * trt,
#'     na.omit(gtsummary::trial),
#'     family = binomial,
#'   ) %>%
#'     model_list_variables()
#' }
model_list_variables <- function(model) {
  UseMethod("model_list_variables")
}

#' @rdname model_list_variables
#' @export
model_list_variables.default <- function(model) {
  model_terms <- stats::terms(model)

  variable_names <- attr(model_terms, "term.labels") %>%
    .clean_backtips()
  dataClasses <- attr(model_terms, "dataClasses")

  if (is.null(dataClasses)) {
    model_frame <- stats::model.frame(model)
    dataClasses <- purrr::map(model_frame, stats::.MFclass) %>% unlist()
  }

  # update the list with all elements of dataClasses
  variable_names <- names(dataClasses) %>%
    c(variable_names) %>%
    unique()

  tibble::tibble(
    variable = variable_names
  ) %>%
    .add_var_class(dataClasses) %>%
    .add_label_attr(model) %>%
    # specific case of polynomial terms defined with poly()
    dplyr::mutate(
      variable = stringr::str_replace(.data$variable, "^poly\\((.*),(.*)\\)$", "\\1")
    )
}


#' @rdname model_list_variables
#' @export
model_list_variables.lavaan <- function(model) {
  tibble::tibble(
    variable = unique(model@ParTable$lhs)
  ) %>%
    dplyr::left_join(
      tibble::tibble(
        variable = model@Data@ov$name,
        var_class = model@Data@ov$type
      ),
      by = "variable"
    ) %>%
    dplyr::mutate(
      var_class = dplyr::if_else(
        .data$var_class == "ordered",
        "factor",
        .data$var_class
      )
    ) %>%
    .add_label_attr(model)
}

## model_list_variables() helpers --------------------------

.add_var_class <- function(x, dataClasses) {
  x %>%
    dplyr::left_join(
      tibble::tibble(
        variable = names(dataClasses),
        var_class = dataClasses
      ),
      by = "variable"
    )
}

.add_label_attr <- function(x, model) {
  labels <- unlist(labelled::var_label(model_get_model_frame(model)))
  if (length(labels) > 0)
    x %>%
      dplyr::left_join(
        dplyr::tibble(
          variable = names(labels),
          label_attr = labels
        ),
        by = "variable"
      )
  else
    x %>%
      dplyr::mutate(label_attr = NA)
}

