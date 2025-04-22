#' Get the model frame of a model
#'
#' The structure of the object returned by [stats::model.frame()]
#' could slightly differ for certain types of models.
#' `model_get_model_frame()` will always return an object
#' with the same data structure or `NULL` if it is not possible
#' to compute model frame from `model`.
#'
#' @param model (a model object, e.g. `glm`)\cr
#' A model object.
#' @export
#' @family model_helpers
#' @seealso [stats::model.frame()]
#' @examples
#' lm(hp ~ mpg + factor(cyl), mtcars) |>
#'   model_get_model_frame() |>
#'   head()
model_get_model_frame <- function(model) {
  if (!is.null(attr(model, "model_frame")))
    return(attr(model, "model_frame"))
  UseMethod("model_get_model_frame")
}

#' @export
#' @rdname model_get_model_frame
model_get_model_frame.default <- function(model) {
  tryCatch(
    stats::model.frame(model),
    error = function(e) {
      NULL
    }
  )
}

#' @export
#' @rdname model_get_model_frame
model_get_model_frame.coxph <- function(model) {
  # variable labels not available, but accessible through model.frame.default()
  # however, model.frame.default() does not return (id) and the correct number
  # of lines
  res <- tryCatch(
    stats::model.frame(model),
    error = function(e) {
      NULL
    }
  )

  if (!is.null(res)) {
    res <- res |>
      labelled::copy_labels_from(
        stats::model.frame.default(model),
        .strict = FALSE
      )
  }

  res
}

#' @export
#' @rdname model_get_model_frame
model_get_model_frame.svycoxph <- model_get_model_frame.default

#' @export
#' @rdname model_get_model_frame
model_get_model_frame.survreg <- function(model) {
  tryCatch(
    stats::model.frame.default(model),
    error = function(e) {
      NULL # nocov
    }
  )
}


#' @export
#' @rdname model_get_model_frame
model_get_model_frame.biglm <- function(model) {
  stats::model.frame(
    stats::formula(model),
    data = stats::model.frame.default(model)
  )
}

#' @export
#' @rdname model_get_model_frame
model_get_model_frame.model_fit <- function(model) {
  model_get_model_frame(model$fit)
}

#' @export
#' @rdname model_get_model_frame
model_get_model_frame.fixest <- function(model) {
  stats::model.frame.default(
    model_get_terms(model),
    data = get(model$call$data, model$call_env)
  )
}

#' @export
#' @rdname model_get_model_frame
model_get_model_frame.svy_vglm <- function(model) {
  stats::model.frame.default(
    model |> model_get_terms(),
    data = model$design$variables
  )
}



