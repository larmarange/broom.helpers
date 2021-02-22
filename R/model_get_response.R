#' Get model response
#'
#' This function does not cover `lavaan` models (`NULL` is returned).
#'
#' @param model a model object
#' @export
#' @family model_helpers
#' @examples
#' lm(hp ~ mpg + factor(cyl) + disp:hp, mtcars) %>%
#'   model_get_response()
#'
#' mod <- glm(
#'   response ~ stage * grade + trt,
#'   gtsummary::trial,
#'   family = binomial,
#'   contrasts = list(stage = contr.sum, grade = contr.treatment(3, 2), trt = "contr.SAS")
#' )
#' mod %>% model_get_response()
#'
#' mod <- glm(
#'   Survived ~ Class * Age + Sex,
#'   data = Titanic %>% as.data.frame(),
#'   weights = Freq,
#'   family = binomial
#' )
#' mod %>% model_get_response()
#'
#' d <- dplyr::as_tibble(Titanic) %>%
#'   dplyr::group_by(Class, Sex, Age) %>%
#'   dplyr::summarise(
#'     n_survived = sum(n * (Survived == "Yes")),
#'     n_dead = sum(n * (Survived == "No"))
#'   )
#' mod <- glm(cbind(n_survived, n_dead) ~ Class * Age + Sex, data = d, family = binomial, y = FALSE)
#' mod %>% model_get_response()
model_get_response <- function(model) {
  UseMethod("model_get_response")
}

#' @export
#' @rdname model_get_response
model_get_response.default <- function(model) {
  tryCatch(
    model %>%
      model_get_model_frame() %>%
      stats::model.response(),
    error = function(e) {
      NULL
    }
  )
}

#' @export
#' @rdname model_get_response
model_get_response.glm <- function(model) {
  y <- model %>% purrr::pluck("y")
  if (is.null(y))
    y <- model %>%
      model_get_model_frame() %>%
      stats::model.response()

  # model defined with cbind
  if (is.matrix(y) && ncol(y) == 2) {
    y <- y[,1] / rowSums(y)
    y[is.nan(y)] <- 0
  }
  y
}

#' @export
#' @rdname model_get_response
model_get_response.glmerMod <- model_get_response.glm
