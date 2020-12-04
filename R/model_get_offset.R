#' Get model offset
#'
#' \lifecycle{maturing}
#'
#' This function does not cover `lavaan` models (`NULL` is returned).
#'
#' @param model a model object
#' @export
#' @family model_helpers
#' @examples
#' mod <- glm(
#'   response ~ trt + offset(log(ttdeath)),
#'   gtsummary::trial,
#'   family = poisson
#' )
#' mod %>% model_get_offset()
model_get_offset <- function(model) {
  UseMethod("model_get_offset")
}

#' @export
#' @rdname model_get_offset
model_get_offset.default <- function(model) {
  tryCatch(
    model %>%
      model_get_model_frame() %>%
      stats::model.offset(),
    error = function(e) {
      NULL #nocov
    }
  )
}
