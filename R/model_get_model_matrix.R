#' Get the model matrix of a model
#'
#' The structure of the object returned by [stats::model.matrix()]
#' could slightly differ for certain types of models.
#' `model_get_model_matrix()` will always return an object
#' with the same structure as [stats::model.matrix.default()].
#'
#' @param model (a model object, e.g. `glm`)\cr
#' A model object.
#' @param ... Additional arguments passed to [stats::model.matrix()].
#' @export
#' @family model_helpers
#' @seealso [stats::model.matrix()]
#' @examples
#' lm(hp ~ mpg + factor(cyl), mtcars) |>
#'   model_get_model_matrix() |>
#'   head()
model_get_model_matrix <- function(model, ...) {
  if (!is.null(attr(model, "model_matrix")))
    return(attr(model, "model_matrix"))
  UseMethod("model_get_model_matrix")
}

#' @export
#' @rdname model_get_model_matrix
model_get_model_matrix.default <- function(model, ...) {
  tryCatch(
    stats::model.matrix(model, ...),
    error = function(e) {
      tryCatch( # test second approach
        stats::model.matrix(stats::terms(model), model$model, ...),
        error = function(e) {
          NULL
        }
      )
    }
  )
}

#' @export
#' @rdname model_get_model_matrix
# For multinom models, names of the model matrix are not
# consistent with the terms names when contrasts other
# than treatment are used, resulting in an issue for
# the identification of variables
model_get_model_matrix.multinom <- function(model, ...) {
  mm <- stats::model.matrix(model, ...)
  co <- stats::coef(model)
  if (is.matrix(co)) {
    colnames(mm) <- colnames(co)
  } else {
    colnames(mm) <- names(co)
  }
  mm
}

#' @export
#' @rdname model_get_model_matrix
model_get_model_matrix.clm <- function(model, ...) {
  stats::model.matrix(model, ...)[[1]]
}

#' @export
#' @rdname model_get_model_matrix
model_get_model_matrix.brmsfit <- function(model, ...) {
  model |>
    brms::standata() |>
    purrr::pluck("X")
}

#' @export
#' @rdname model_get_model_matrix
#' @details
#' For models fitted with `glmmTMB::glmmTMB()`, it will return a model matrix
#' taking into account all components ("cond", "zi" and "disp"). For a more
#' restricted model matrix, please refer to `glmmTMB::model.matrix.glmmTMB()`.
model_get_model_matrix.glmmTMB <- function(model, ...) {
  # load lme4 if available
  .assert_package("lme4", fn = "broom.helpers::model_get_model_matrix.glmmTMB()")

  stats::model.matrix(
    lme4::nobars(model$modelInfo$allForm$combForm),
    stats::model.frame(model, ...),
    contrasts.arg = model$modelInfo$contrasts
  )
}


#' @export
#' @rdname model_get_model_matrix
#' @details
#' For [plm::plm()] models, constant columns are not removed.
model_get_model_matrix.plm <- function(model, ...) {
  stats::model.matrix(model, cstcovar.rm = "none", ...)
}


#' @export
#' @rdname model_get_model_matrix
model_get_model_matrix.biglm <- function(model, ...) {
  stats::model.matrix(
    model,
    data = stats::model.frame.default(model)
  )
}

#' @export
#' @rdname model_get_model_matrix
model_get_model_matrix.model_fit <- function(model, ...) {
  model_get_model_matrix(model$fit, ...)
}

#' @export
#' @rdname model_get_model_matrix
#' @details
#' For `fixest` models, will recreate a model matrix with both main variables
#' and instrumental variables. For more options, see
#' [fixest::model.matrix.fixest].
model_get_model_matrix.fixest <- function(model, ...) {
  stats::model.matrix.default(
    model_get_terms(model),
    data = get(model$call$data, model$call_env),
    ...
  )
}

#' @export
#' @rdname model_get_model_matrix
model_get_model_matrix.LORgee <- function(model, ...) {
  stats::model.matrix.default(
    model,
    data = stats::model.frame(model)
  )
}

#' @export
#' @rdname model_get_model_matrix
model_get_model_matrix.betareg <- function(model, ...) {
  stats::model.matrix.default(
    model |> model_get_terms(),
    data = model |> model_get_model_frame()
  )
}

#' @export
#' @rdname model_get_model_matrix
model_get_model_matrix.cch <- function(model, ...) {
  stats::model.matrix.default(
    model$call$formula |> stats::formula(),
    data = model |> model_get_model_frame()
  )
}

#' @export
#' @rdname model_get_model_matrix
model_get_model_matrix.vglm <- function(model, ...) {
  stats::model.matrix(model, ..., type = "lm")
}

#' @export
#' @rdname model_get_model_matrix
model_get_model_matrix.vgam <- function(model, ...) {
  stats::model.matrix(model, ..., type = "lm")
}

#' @export
#' @rdname model_get_model_matrix
model_get_model_matrix.svy_vglm <- function(model) {
  model_get_model_matrix(model$fit)
}
