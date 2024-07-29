#' Get the terms of a model
#'
#' Return the result of [stats::terms()] applied to the model
#' or `NULL` if it is not possible to get terms from `model`.
#'
#' @param model a model object
#' @export
#' @family model_helpers
#' @seealso [stats::terms()]
#' @examples
#' lm(hp ~ mpg + factor(cyl), mtcars) |>
#'   model_get_terms()
model_get_terms <- function(model) {
  UseMethod("model_get_terms")
}

#' @export
#' @rdname model_get_terms
model_get_terms.default <- function(model) {
  tryCatch(
    stats::terms(model),
    error = function(e) {
      NULL
    }
  )
}

#' @export
#' @rdname model_get_terms
model_get_terms.brmsfit <- function(model) {
  model$formula |>
    brms::brmsterms(resp_rhs_all = FALSE) |>
    purrr::pluck("allvars") |>
    stats::terms()
}

#' @export
#' @rdname model_get_terms
#' @details
#' For models fitted with `glmmTMB::glmmTMB()`, it will return a terms object
#' taking into account all components ("cond" and "zi"). For a more
#' restricted terms object, please refer to `glmmTMB::terms.glmmTMB()`.
model_get_terms.glmmTMB <- function(model) {
  model$modelInfo$allForm$combForm |> stats::terms()
}

#' @export
#' @rdname model_get_terms
model_get_terms.model_fit <- function(model) {
  model_get_terms(model$fit)
}

#' @export
#' @rdname model_get_terms
model_get_terms.betareg <- function(model) {
  model_get_terms(model$terms$full)
}

#' @export
#' @rdname model_get_terms
model_get_terms.betareg <- function(model) {
  model_get_terms(model$terms$full)
}

#' @export
#' @rdname model_get_model_matrix
model_get_terms.cch <- function(model, ...) {
  stats::terms.formula(
    model$call$formula |> stats::formula(),
    data = model |> model_get_model_frame()
  )
}
