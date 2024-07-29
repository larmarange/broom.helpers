#' Get the name of the response variable
#'
#' @param model a model object
#' @export
#' @family model_helpers
#' @examples
#' lm(hp ~ mpg + factor(cyl) + disp:hp, mtcars) |>
#'   model_get_response_variable()
#'
#' mod <- glm(
#'   response ~ stage * grade + trt,
#'   gtsummary::trial,
#'   family = binomial
#' )
#' mod |> model_get_response_variable()
#'
#' mod <- glm(
#'   Survived ~ Class * Age + Sex,
#'   data = Titanic |> as.data.frame(),
#'   weights = Freq,
#'   family = binomial
#' )
#' mod |> model_get_response_variable()
model_get_response_variable <- function(model) {
  UseMethod("model_get_response_variable")
}

#' @export
#' @rdname model_get_response_variable
model_get_response_variable.default <- function(model) {
  model_frame <- model |> model_get_model_frame()
  model_terms <- model |> model_get_terms()
  if (!is.null(model_terms) && inherits(model_terms, "terms")) {
    return(names(model_frame)[attr(model_terms, "response")])
  } else {
    return(NULL)
  }
}
