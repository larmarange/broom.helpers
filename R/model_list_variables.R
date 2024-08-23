#' List all the variables used in a model
#'
#' Including variables used only in an interaction.
#'
#' @param model (a model object, e.g. `glm`)\cr
#' A model object.
#' @param labels (`list` or `string`)\cr
#' An optional named list or named vector of
#' custom variable labels.
#' @param only_variable (`logical`)\cr
#' If `TRUE`, will return only "variable" column.
#' @param add_var_type (`logical`)\cr
#' If `TRUE`, add `var_nlevels` and `var_type` columns.
#' @return
#' A tibble with three columns:
#' * `variable`: the corresponding variable
#' * `var_class`: class of the variable (cf. [stats::.MFclass()])
#' * `label_attr`: variable label defined in the original data frame
#'    with the label attribute (cf. [labelled::var_label()])
#' * `var_label`: a variable label (by priority, `labels` if defined,
#'   `label_attr` if available, otherwise `variable`)
#'
#' If `add_var_type = TRUE`:
#' * `var_type`: `"continuous"`, `"dichotomous"` (categorical variable with 2 levels),
#'   `"categorical"` (categorical variable with 3 or more levels), `"intercept"`
#'   or `"interaction"`
#' * `var_nlevels`: number of original levels for categorical variables
#'
#' @export
#' @family model_helpers
#' @examplesIf interactive()
#' if (.assert_package("gtsummary", boolean = TRUE)) {
#'   df <- Titanic |>
#'     dplyr::as_tibble() |>
#'     dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#'   glm(
#'     Survived ~ Class + Age:Sex,
#'     data = df, weights = df$n,
#'     family = binomial
#'   ) |>
#'   model_list_variables()
#'
#'   iris |>
#'     lm(
#'       Sepal.Length ~ poly(Sepal.Width, 2) + Species,
#'       data = _,
#'       contrasts = list(Species = contr.sum)
#'     ) |>
#'     model_list_variables()
#'
#'   glm(
#'     response ~ poly(age, 3) + stage + grade * trt,
#'     na.omit(gtsummary::trial),
#'     family = binomial,
#'   ) |>
#'     model_list_variables()
#' }
model_list_variables <- function(model, labels = NULL,
                                 only_variable = FALSE, add_var_type = FALSE) {
  UseMethod("model_list_variables")
}

#' @rdname model_list_variables
#' @export
model_list_variables.default <- function(model, labels = NULL,
                                         only_variable = FALSE, add_var_type = FALSE) {
  model_frame <- model_get_model_frame(model)
  model_terms <- model_get_terms(model)

  if (!is.null(model_terms) && inherits(model_terms, "terms")) {
    variable_names <- attr(model_terms, "term.labels")
    dataClasses <- purrr::map(model_frame, .MFclass2) |> unlist()

    if (is.null(dataClasses)) {
      dataClasses <- attr(model_terms, "dataClasses")
    }
  } else {
    dataClasses <- model_frame |>
      lapply(.MFclass2) |>
      unlist()
    variable_names <- names(dataClasses)
  }

  if (is.null(variable_names)) {
    return(NULL)
  }

  # update the list with all elements of dataClasses
  variable_names <- names(dataClasses) |>
    c(variable_names) |>
    .clean_backticks() |>
    unique()

  res <- tibble::tibble(
    variable = variable_names
  ) |>
    .add_var_class(dataClasses) |>
    .add_label_attr(model) |>
    # specific case of polynomial terms defined with poly()
    dplyr::mutate(
      variable = stringr::str_replace(.data$variable, "^poly\\((.*),(.*)\\)$", "\\1")
    ) |>
    .compute_var_label(labels)

  if (only_variable) {
    return(res$variable)
  }

  if (add_var_type) {
    return(.add_var_type(res, model))
  }

  res
}


#' @rdname model_list_variables
#' @export
model_list_variables.lavaan <- function(model, labels = NULL,
                                        only_variable = FALSE, add_var_type = FALSE) {
  res <- tibble::tibble(
    variable = .clean_backticks(unique(model@ParTable$lhs))
  ) |>
    dplyr::left_join(
      tibble::tibble(
        variable = .clean_backticks(model@Data@ov$name),
        var_class = model@Data@ov$type
      ),
      by = "variable"
    ) |>
    dplyr::mutate(
      var_class = dplyr::if_else(
        .data$var_class == "ordered",
        "factor",
        .data$var_class
      )
    ) |>
    .add_label_attr(model) |>
    .compute_var_label(labels)

  if (only_variable) {
    return(res$variable)
  }

  if (add_var_type) {
    return(.add_var_type(res, model))
  }

  res
}

#' @rdname model_list_variables
#' @export
model_list_variables.logitr <- function(model, labels = NULL,
                                        only_variable = FALSE, add_var_type = FALSE) {
  res <- model_list_variables.default(model, labels, FALSE)

  if (!is.null(model$data$scalePar)) {
    label_scalePar <- labels |> purrr::pluck("scalePar")
    res <- res |>
      dplyr::add_row(
        variable = "scalePar",
        var_class = "numeric",
        label_attr = ifelse(
          is.null(label_scalePar),
          NA,
          label_scalePar
        ),
        var_label = ifelse(
          is.null(label_scalePar),
          "scalePar",
          label_scalePar
        )
      )
  }


  if (only_variable) {
    return(res$variable)
  }

  if (add_var_type) {
    return(.add_var_type(res, model))
  }

  res
}

## model_list_variables() helpers --------------------------

.add_var_class <- function(x, dataClasses) {
  x |>
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
  if (length(labels) > 0) {
    x |>
      dplyr::left_join(
        dplyr::tibble(
          variable = names(labels),
          label_attr = labels
        ),
        by = "variable"
      )
  } else {
    x |>
      dplyr::mutate(label_attr = NA)
  }
}

# stats::.MFclass do not distinct integer and numeric
.MFclass2 <- function(x) {
  if (is.logical(x)) {
    return("logical")
  }
  if (is.ordered(x)) {
    return("ordered")
  }
  if (is.factor(x)) {
    return("factor")
  }
  if (is.character(x)) {
    return("character")
  }
  if (is.matrix(x) && is.numeric(x)) {
    return(paste0("nmatrix.", ncol(x)))
  }
  if (is.integer(x)) {
    return("integer")
  }
  if (is.numeric(x)) {
    return("numeric")
  }
  return("other")
}

.compute_var_label <- function(x, labels = NULL) {
  if (is.list(labels)) {
    labels <- unlist(labels)
  }
  if (is.null(labels)) {
    x$var_custom_label <- NA_character_
  } else {
    x <- x |>
      dplyr::left_join(
        dplyr::tibble(
          variable = names(labels),
          var_custom_label = labels
        ),
        by = "variable"
      )
  }
  x |>
    dplyr::mutate(
      label_attr = as.character(.data$label_attr),
      var_label = dplyr::case_when(
        !is.na(.data$var_custom_label) ~ .data$var_custom_label,
        !is.na(.data$label_attr) ~ .data$label_attr,
        TRUE ~ .data$variable
      )
    ) |>
    dplyr::select(-dplyr::all_of("var_custom_label"))
}

.add_var_type <- function(x, model) {
  x <- x |>
    dplyr::left_join(
      model_get_nlevels(model),
      by = "variable"
    )
  x |> .compute_var_type()
}
