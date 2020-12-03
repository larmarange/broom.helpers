#' Add the number of observations
#'
#' @description
#' \lifecycle{maturing}
#'
#' Add the number of observations in a new column `n` taking into account
#' interaction terms and the different types of contrasts.
#'
#' @details
#' For binomial and multinomial logistic models, will also return
#' the number of events (`nevent`).
#'
#' For Poisson models, will return the number of events (`nevent`) and
#' exposure time defined with [stats::offset()] (`exposure`).
#'
#' For Cox models ([survival::coxph()]), will return the number of
#' events (`nevent`) and exposure time (`exposure`).
#' @param x a tidy tibble
#' @param model the corresponding model, if not attached to `x`
#' @export
#' @family tidy_helpers
#' @examples
#' lm(Petal.Length ~ ., data = iris) %>%
#'   tidy_and_attach() %>%
#'   tidy_add_n()
#'
#' lm(Petal.Length ~ ., data = iris, contrasts = list(Species = contr.sum)) %>%
#'   tidy_and_attach() %>%
#'   tidy_add_n()
#'
#' lm(Petal.Length ~ ., data = iris, contrasts = list(Species = contr.poly)) %>%
#'   tidy_and_attach() %>%
#'   tidy_add_n()
#'
#' lm(Petal.Length ~ poly(Sepal.Length, 2), data = iris) %>%
#'   tidy_and_attach() %>%
#'   tidy_add_n()
#'
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#'
#' df %>%
#'   glm(
#'     Survived ~ Class + Age + Sex,
#'     data = ., weights = .$n, family = binomial,
#'     contrasts = list(Age = contr.sum, Class = "contr.helmert")
#'   ) %>%
#'   tidy_and_attach() %>%
#'   tidy_add_n()
#'
#' df %>%
#'   glm(
#'     Survived ~ Class * (Age : Sex),
#'     data = ., weights = .$n, family = binomial,
#'     contrasts = list(Age = contr.sum, Class = "contr.helmert")
#'   ) %>%
#'   tidy_and_attach() %>%
#'   tidy_add_n()
#'
#' glm(response ~ age + grade * trt, gtsummary::trial, family = poisson) %>%
#'   tidy_and_attach() %>%
#'   tidy_add_n()
#'
#' glm(
#'   response ~ trt * grade + offset(ttdeath),
#'   gtsummary::trial,
#'   family = poisson,
#'   weights = rep_len(1:2, 200)
#' ) %>%
#'   tidy_and_attach() %>%
#'   tidy_add_n()
tidy_add_n <- function(x, model = tidy_get_model(x)) {
  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  .attributes <- .save_attributes(x)

  if (any(c("n", "nevent", "exposure") %in% names(x))) {
    x <- x %>% dplyr::select(-dplyr::any_of(c("n", "nevent", "exposure")))
  }

  n <- model %>% model_get_n()
  if (is.null(n)) {
    x$n <- NA_real_
  } else {
    if ("y.level" %in% names(n)) {
      x <- x %>%
        dplyr::left_join(n, by = c("y.level", "term"))
    } else {
      x <- x %>%
        dplyr::left_join(n, by = "term")
    }

  }

  x %>%
    tidy_attach_model(model = model, .attributes = .attributes)
}
