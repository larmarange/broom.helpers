#' List higher order variables of a model
#'
#' @param model (a model object, e.g. `glm`)\cr
#' A model object.
#' @export
#' @family model_helpers
#' @examples
#' lm(hp ~ mpg + factor(cyl) + disp:hp, mtcars) |>
#'   model_list_higher_order_variables()
#'
#' mod <- glm(
#'   response ~ stage * grade + trt:stage,
#'   gtsummary::trial,
#'   family = binomial
#' )
#' mod |> model_list_higher_order_variables()
#'
#' mod <- glm(
#'   Survived ~ Class * Age + Sex,
#'   data = Titanic |> as.data.frame(),
#'   weights = Freq,
#'   family = binomial
#' )
#' mod |> model_list_higher_order_variables()
model_list_higher_order_variables <- function(model) {
  UseMethod("model_list_higher_order_variables")
}

#' @export
#' @rdname model_list_higher_order_variables
model_list_higher_order_variables.default <- function(model) {
  variables <- model |>
    model_list_variables(only_variable = TRUE)

  # exclude response variable
  response_variable <- model |> model_get_response_variable()
  if (!is.null(response_variable)) {
    variables <- variables[!variables %in% response_variable]
  }
  # exclude (weights)
  variables <- variables[variables != "(weights)"]

  terms <- strsplit(variables, ":")
  # count the number of times a combination of terms appear
  .count_combination <- function(i) {
    lapply(
      terms,
      function(x) {
        all(i %in% x)
      }
    ) |>
      unlist() |>
      sum()
  }
  count <- lapply(terms, .count_combination) |> unlist()

  # keep combinations appearing only once
  variables[count == 1]
}
