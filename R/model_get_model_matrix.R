#' Get the model matrix of a model
#'
#' The structure of the object returned by [stats::model.matrix()]
#' could slightly differ for certain types of models.
#' `model_get_model_matrix()` will always return an object
#' with the same structure as [stats::model.matrix.default()].
#'
#' @param model a model object
#' @export
#' @family model_helpers
#' @seealso [stats::model.matrix()]
model_get_model_matrix <- function(model) {
  UseMethod("model_get_model_matrix")
}

#' @export
#' @rdname model_get_model_matrix
model_get_model_matrix.default <- function(model) {
  stats::model.matrix(model)
}

#' @export
#' @rdname model_get_model_matrix
model_get_model_matrix.clm <- function(model) {
  stats::model.matrix(model)[[1]]
}

#' @export
#' @rdname model_get_model_matrix
model_get_model_matrix.clmm <- function(model) {
  stats::model.matrix(stats::terms(model), model$model)
}
