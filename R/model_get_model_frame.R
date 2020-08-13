#' Get the model frame of a model
#'
#' The structure of the object returned by [stats::model.frame()]
#' could slightly differ for certain types of models.
#' `model_get_model_frame()` will always return an object
#' with the same data structure or `NULL` if it is not possible
#' to compute model frame from `model`.
#'
#' @param model a model object
#' @export
#' @family model_helpers
#' @seealso [stats::model.frame()]
model_get_model_frame <- function(model) {
  UseMethod("model_get_model_frame")
}

#' @export
#' @rdname model_get_model_frame
model_get_model_frame.default <- function(model) {
  stats::model.frame(model)
}


# It does not seems possible to get it from the model object
#' @export
#' @rdname model_get_model_frame
model_get_model_frame.lavaan <- function(model) {
  NULL
}
