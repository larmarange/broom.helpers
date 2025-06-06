#' Get contrasts used in the model
#'
#' @param model (a model object, e.g. `glm`)\cr
#' A model object.
#' @export
#' @family model_helpers
#' @examples
#' glm(
#'   am ~ mpg + factor(cyl),
#'   data = mtcars,
#'   family = binomial,
#'   contrasts = list(`factor(cyl)` = contr.sum)
#' ) |>
#'   model_get_contrasts()
model_get_contrasts <- function(model) {
  UseMethod("model_get_contrasts")
}

#' @export
model_get_contrasts.default <- function(model) {
  # we try 3 different approaches in a row
  mc <- model_get_contrasts_1(model)
  if (is.null(mc)) {
    mc <- model_get_contrasts_2(model)
  }
  if (is.null(mc)) {
    mc <- model_get_contrasts_3(model)
  }
  mc
}

model_get_contrasts_1 <- function(model) {
  tryCatch(
    purrr::chuck(model, "contrasts"),
    error = function(e) {
      NULL
    }
  )
}

model_get_contrasts_2 <- function(model) {
  tryCatch(
    attr(model_get_model_matrix(model), "contrasts"),
    error = function(e) {
      NULL
    }
  )
}

model_get_contrasts_3 <- function(model) {
  tryCatch(
    attr(stats::model.matrix(stats::terms(model), stats::model.frame(model)), "contrasts"),
    error = function(e) {
      NULL
    }
  )
}

#' @export
#' @rdname model_get_contrasts
model_get_contrasts.model_fit <- function(model) {
  model_get_contrasts(model$fit)
}

#' @export
#' @rdname model_get_contrasts
model_get_contrasts.zeroinfl <- function(model) {
  mc <- model_get_contrasts_1(model)
  res <- mc$count
  # merging/combining the two lists
  for (v in names(mc$zero)) res[[v]] <- mc$zero[[v]]
  res
}

#' @export
#' @rdname model_get_contrasts
model_get_contrasts.hurdle <- model_get_contrasts.zeroinfl

#' @export
#' @rdname model_get_contrasts
model_get_contrasts.betareg <- function(model) {
  mc <- model_get_contrasts_1(model)
  res <- mc$mean
  # merging/combining the two lists
  for (v in names(mc$precision)) res[[v]] <- mc$precision[[v]]
  res
}

#' @export
#' @rdname model_get_contrasts
model_get_contrasts.svy_vglm <- function(model) {
  model_get_contrasts(model$fit)
}
