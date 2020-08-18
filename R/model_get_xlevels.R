#' Get xlevels used in the model
#'
#' @param model a model object
#' @export
#' @family model_helpers
model_get_xlevels <- function(model) {
  UseMethod("model_get_xlevels")
}

#' @export
#' @rdname model_get_xlevels
model_get_xlevels.default <- function(model) {
  tryCatch(
    model$xlevels,
    error = function(e) {NULL}
  )
}


#' @export
#' @rdname model_get_xlevels
model_get_xlevels.lmerMod <- function(model) {
  xlevels <- stats::model.frame(model) %>% lapply(levels)
  selection <- ! (xlevels %>% lapply(is.null) %>% unlist())
  xlevels[selection] # keep only not null
}


#' @export
#' @rdname model_get_xlevels
model_get_xlevels.glmerMod <- model_get_xlevels.lmerMod

#' @export
#' @rdname model_get_xlevels
model_get_xlevels.mipo <- function(model) {
    get_model_mipo(model) %>% model_get_xlevels()
}
