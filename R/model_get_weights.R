#' Get sampling weights used by a model
#'
#' This function does not cover `lavaan` models (`NULL` is returned).
#'
#' @param model a model object
#' @export
#' @family model_helpers
#' @examples
#' mod <- lm(Sepal.Length ~ Sepal.Width, iris)
#' mod %>% model_get_weights()
#'
#' mod <- lm(hp ~ mpg + factor(cyl) + disp:hp, mtcars, weights = mtcars$gear)
#' mod %>% model_get_weights()
#'
#' mod <- glm(
#'   response ~ stage * grade + trt,
#'   gtsummary::trial,
#'   family = binomial
#' )
#' mod %>% model_get_weights()
#'
#' mod <- glm(
#'   Survived ~ Class * Age + Sex,
#'   data = Titanic %>% as.data.frame(),
#'   weights = Freq,
#'   family = binomial
#' )
#' mod %>% model_get_weights()
#'
#' d <- dplyr::as_tibble(Titanic) %>%
#'   dplyr::group_by(Class, Sex, Age) %>%
#'   dplyr::summarise(
#'     n_survived = sum(n * (Survived == "Yes")),
#'     n_dead = sum(n * (Survived == "No"))
#'   )
#' mod <- glm(cbind(n_survived, n_dead) ~ Class * Age + Sex, data = d, family = binomial)
#' mod %>% model_get_weights()
model_get_weights <- function(model) {
  UseMethod("model_get_weights")
}

#' @export
#' @rdname model_get_weights
model_get_weights.default <- function(model) {
  w <- tryCatch(
    stats::weights(model),
    error = function(e) {
      NULL
    }
  )
  if (is.null(w) || length(w) == 0) {
    mf <- model %>% model_get_model_frame()
    if (!is.null(mf)) {
      if ("(weights)" %in% names(mf)) {
        w <- mf %>% purrr::pluck("(weights)")
      } else {
        w <- rep_len(1L, mf %>% nrow())
      }
    }
  }
  # matrix case => transform to vector
  if (is.matrix(w)) w <- c(w)
  w
}

#' @export
#' @rdname model_get_weights
model_get_weights.svyglm <- function(model) {
  stats::weights(model$survey.design)
}

#' @export
#' @rdname model_get_weights
model_get_weights.model_fit <- function(model) {
  model_get_weights(model$fit)
}

#' @export
#' @rdname model_get_weights
model_get_weights.mmrm <- function(model) {
  w <- stats::weights(model)
  mf <- model %>% model_get_model_frame()
  w[as.integer(row.names(mf))]
}
