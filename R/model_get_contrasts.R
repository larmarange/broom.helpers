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
  tryCatch(
    model$contrasts,
    error = function(e) {
      # if first approach is not working, try second one
      tryCatch(
        attr(model_get_model_matrix(model), "contrasts"),
        error = function(e) {
          NULL
        }
      )
    }
  )
}
