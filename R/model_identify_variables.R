#' Identify for each coefficient of a model the corresponding variable
#'
#' It will also identify interaction terms and intercept(s).
#' @param model (a model object, e.g. `glm`)\cr
#' A model object.
#' @return
#' A tibble with four columns:
#' * `term`: coefficients of the model
#' * `variable`: the corresponding variable
#' * `var_class`: class of the variable (cf. [stats::.MFclass()])
#' * `var_type`: `"continuous"`, `"dichotomous"` (categorical variable with 2 levels),
#'   `"categorical"` (categorical variable with 3 or more levels), `"intercept"`
#'   or `"interaction"`
#' * `var_nlevels`: number of original levels for categorical variables
#' @export
#' @family model_helpers
#' @seealso [tidy_identify_variables()]
#' @examples
#' df <- Titanic |>
#'   dplyr::as_tibble() |>
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#' glm(
#'   Survived ~ Class + Age * Sex,
#'   data = df, weights = df$n,
#'   family = binomial
#' ) |>
#'   model_identify_variables()
#'
#' lm(
#'   Sepal.Length ~ poly(Sepal.Width, 2) + Species,
#'   data = iris,
#'   contrasts = list(Species = contr.sum)
#' ) |>
#'   model_identify_variables()
model_identify_variables <- function(model) {
  UseMethod("model_identify_variables")
}

#' @rdname model_identify_variables
#' @export
model_identify_variables.default <- function(model) {
  assign <- model |> model_get_assign()
  model_matrix <- attr(assign, "model_matrix")

  if (is.null(model_matrix) || is.null(assign)) {
    # return an empty tibble
    return(
      dplyr::tibble(
        variable = NA_character_,
        var_class = NA_character_,
        var_type = NA_character_,
        var_nlevels = NA_integer_
      ) |>
        dplyr::filter(FALSE)
    )
  }

  assign[assign == 0] <- NA
  model_terms <- model_get_terms(model)
  variable_names <- model |> model_list_variables(only_variable = TRUE)
  variables <- attr(model_terms, "term.labels") |>
    .clean_backticks(variable_names = variable_names)

  tibble::tibble(
    term = colnames(model_matrix),
    variable = variables[assign]
  ) |>
    # specific case of polynomial terms defined with poly()
    dplyr::mutate(
      variable = stringr::str_replace(.data$variable, "^poly\\((.*),(.*)\\)$", "\\1")
    ) |>
    dplyr::left_join(
      model_list_variables(model) |>
        dplyr::select("variable", "var_class"),
      by = "variable"
    ) |>
    dplyr::left_join(
      model_get_nlevels(model),
      by = "variable"
    ) |>
    .compute_var_type()
}


#' @rdname model_identify_variables
#' @export
model_identify_variables.lavaan <- function(model) {
  tibble::tibble(
    term = paste(model@ParTable$lhs, model@ParTable$op, model@ParTable$rhs),
    variable = .clean_backticks(model@ParTable$lhs)
  ) |>
    dplyr::left_join(
      tibble::tibble(
        variable = .clean_backticks(model@Data@ov$name),
        var_class = model@Data@ov$type,
        var_nlevels = model@Data@ov$nlev
      ),
      by = "variable"
    ) |>
    dplyr::mutate(
      var_nlevels = dplyr::if_else(
        .data$var_nlevels == 0,
        NA_integer_,
        .data$var_nlevels
      ),
      var_class = dplyr::if_else(
        .data$var_class == "ordered",
        "factor",
        .data$var_class
      )
    ) |>
    .compute_var_type()
}

# for stats::aov(), variable is equal to term
#' @rdname model_identify_variables
#' @export
model_identify_variables.aov <- function(model) {
  model |>
    model_list_variables() |>
    dplyr::mutate(term = .data$variable) |>
    dplyr::select(dplyr::all_of(c("term", "variable", "var_class"))) |>
    dplyr::left_join(
      model |> model_get_nlevels(),
      by = "variable"
    ) |>
    .compute_var_type()
}


#' @rdname model_identify_variables
#' @export
model_identify_variables.clm <- function(model) {
  res <- model_identify_variables.default(model)
  if (is.null(model$alpha.mat)) {
    res <- dplyr::bind_rows(
      res |>
        dplyr::filter(.data$term != "(Intercept)"),
      dplyr::tibble(
        term = names(model$alpha),
        var_type = "intercept"
      )
    )
  } else {
    y.levels <- colnames(model$alpha.mat)
    nominal_terms <- rownames(model$alpha.mat)
    res <- dplyr::bind_rows(
      res |>
        dplyr::filter(!.data$term %in% nominal_terms),
      res |>
        dplyr::filter(.data$term %in% nominal_terms) |>
        tidyr::crossing(y.level = y.levels) |>
        dplyr::mutate(term = paste(.data$y.level, .data$term, sep = "."))
    )
  }
  res
}


#' @rdname model_identify_variables
#' @export
model_identify_variables.clmm <- model_identify_variables.clm

#' @rdname model_identify_variables
#' @export
model_identify_variables.gam <- function(model) {
  model_identify_variables.default(model) |>
    dplyr::bind_rows(
      # suppressWarnings to avoid a warning when the result is an empty tibble
      suppressWarnings(broom::tidy(model, parametric = FALSE)) |>
        dplyr::bind_rows(tibble::tibble(term = character(0))) |>
        dplyr::select(dplyr::all_of("term")) |>
        dplyr::mutate(variable = .data$term, var_type = "continuous")
    )
}

#' @export
#' @rdname model_identify_variables
model_identify_variables.model_fit <- function(model) {
  model_identify_variables(model$fit)
}

#' @rdname model_identify_variables
#' @importFrom dplyr add_row
#' @export
model_identify_variables.logitr <- function(model) {
  res <- model_identify_variables.default(model)
  if (!is.null(model$data$scalePar)) {
    res <- res |>
      dplyr::add_row(
        term = "scalePar",
        variable = "scalePar",
        var_class = "numeric",
        var_nlevels = NA,
        var_type = "continuous"
      )
  }
  res
}


## model_identify_variables() helpers --------------------------

.compute_var_type <- function(x) {
  cat_classes <- c("factor", "character", "logical")
  x |>
    dplyr::mutate(
      var_type = dplyr::case_when(
        is.na(.data$variable) ~ "intercept",
        .data$var_class %in% cat_classes & .data$var_nlevels <= 2 ~ "dichotomous",
        .data$var_class %in% cat_classes ~ "categorical",
        !is.na(.data$var_class) ~ "continuous",
        is.na(.data$var_class) & stringr::str_detect(.data$variable, ":") ~ "interaction"
      )
    )
}

#' @export
#' @rdname model_identify_variables
model_identify_variables.svy_vglm <- function(model) {
  model_identify_variables(model$fit)
}
