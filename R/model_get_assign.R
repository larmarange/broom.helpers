#' Get the assign attribute of model matrix of a model
#'
#' Return the assign attribute attached to the object returned by
#' [stats::model.matrix()].
#'
#' @param model (a model object, e.g. `glm`)\cr
#' A model object.
#' @export
#' @family model_helpers
#' @seealso [stats::model.matrix()]
#' @examples
#' lm(hp ~ mpg + factor(cyl), mtcars) |>
#'   model_get_assign()
model_get_assign <- function(model) {
  UseMethod("model_get_assign")
}

#' @export
#' @rdname model_get_assign
model_get_assign.default <- function(model) {
  model_matrix <- model_get_model_matrix(model)
  get_assign <- purrr::attr_getter("assign")
  assign <- model_matrix |> get_assign()

  if (is.null(assign)) {
    # an alternative generic way to compute assign
    # (e.g. for felm models)
    model_matrix <- tryCatch(
      stats::model.matrix(stats::terms(model), stats::model.frame(model)),
      error = function(e) {
        NULL # nocov
      }
    )
    assign <- model_matrix |> get_assign()
  }

  if (!is.atomic(assign)) {
    return(NULL)
  } # nocov

  attr(assign, "model_matrix") <- model_matrix
  assign
}

#' @export
#' @rdname model_get_assign
model_get_assign.vglm <- function(model) {
  model_matrix <- model_get_model_matrix(model)
  get_assign <- purrr::attr_getter("orig.assign.lm")
  assign <- model_matrix |> get_assign()
  attr(assign, "model_matrix") <- model_matrix
  assign
}

#' @export
#' @rdname model_get_assign
model_get_assign.svy_vglm <- function(model) {
  model_get_assign(model$fit)
}

#' @export
#' @rdname model_get_assign
model_get_assign.model_fit <- function(model) {
  model_get_assign(model$fit)
}

#' @export
#' @rdname model_get_assign
model_get_assign.fixest <- function(model) {
  model_matrix <- stats::model.matrix.default(
    model_get_terms(model),
    data = eval(model$call$data, model$call_env)
  )
  get_assign <- purrr::attr_getter("assign")
  assign <- model_matrix |> get_assign()
  attr(assign, "model_matrix") <- model_matrix
  assign
}
