#' Get contrasts used in the model
#'
#' @param model a model object
#' @export
#' @family model_helpers
model_get_contrasts <- function(model) {
  UseMethod("model_get_contrasts")
}

#' @export
#' @rdname model_get_contrasts
model_get_contrasts.default <- function(model) {
  model$contrasts
}


#' @export
#' @rdname model_get_contrasts
model_get_contrasts.lmerMod <- function(model) {
  attr(model.matrix(model), "contrasts")
}


#' @export
#' @rdname model_get_contrasts
model_get_contrasts.glmerMod <- model_get_contrasts.lmerMod

#' @export
#' @rdname model_get_contrasts
model_get_contrasts.lavaan <- function(model) {
  NULL
}
