#' Get the number of observations
#'
#' For binomial and multinomial logistic models, will also return
#' the number of events.
#'
#' For Poisson models, will return the number of events and exposure time
#' (defined with [stats::offset()]).
#'
#' For Cox models ([survival::coxph()]), will return the number of events and
#' exposure time.
#'
#' For competing risk regression models ([tidycmprsk::crr()]), `n_event` takes
#' into account only the event of interest defined by `failcode.`
#'
#' See [tidy_add_n()] for more details.
#'
#' The total number of observations (`N_obs`), of events (`N_event`) and of
#' exposure time (`Exposure`) are stored as attributes of the returned tibble.
#'
#' This function does not cover `lavaan` models (`NULL` is returned).
#'
#' @param model a model object
#' @export
#' @family model_helpers
#' @examples
#' lm(hp ~ mpg + factor(cyl) + disp:hp, mtcars) %>%
#'   model_get_n()
#'
#' mod <- glm(
#'   response ~ stage * grade + trt,
#'   gtsummary::trial,
#'   family = binomial,
#'   contrasts = list(stage = contr.sum, grade = contr.treatment(3, 2), trt = "contr.SAS")
#' )
#' mod %>% model_get_n()
#'
#' \dontrun{
#' mod <- glm(
#'   Survived ~ Class * Age + Sex, data = Titanic %>% as.data.frame(),
#'   weights = Freq, family = binomial
#' )
#' mod %>% model_get_n()
#'
#' d <- dplyr::as_tibble(Titanic) %>%
#'   dplyr::group_by(Class, Sex, Age) %>%
#'   dplyr::summarise(
#'     n_survived = sum(n * (Survived == "Yes")),
#'     n_dead = sum(n * (Survived == "No"))
#'   )
#' mod <- glm(cbind(n_survived, n_dead) ~ Class * Age + Sex, data = d, family = binomial)
#' mod %>% model_get_n()
#'
#' mod <- glm(response ~ age + grade * trt, gtsummary::trial, family = poisson)
#' mod %>% model_get_n()
#'
#' mod <- glm(
#'   response ~ trt * grade + offset(ttdeath),
#'   gtsummary::trial,
#'   family = poisson
#' )
#' mod %>% model_get_n()
#'
#' dont
#' df <- survival::lung %>% dplyr::mutate(sex = factor(sex))
#' mod <- survival::coxph(survival::Surv(time, status) ~ ph.ecog + age + sex, data = df)
#' mod %>% model_get_n()
#'
#' mod <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
#' mod %>% model_get_n()
#'
#' mod <- lme4::glmer(response ~ trt * grade + (1 | stage),
#'   family = binomial, data = gtsummary::trial
#' )
#' mod %>% model_get_n()
#'
#' mod <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
#'   family = binomial, data = lme4::cbpp
#' )
#' mod %>% model_get_n()
#' }
model_get_n <- function(model) {
  UseMethod("model_get_n")
}

#' @export
#' @rdname model_get_n
model_get_n.default <- function(model) {
  tcm <- model %>% model_compute_terms_contributions()
  if (is.null(tcm)) return(NULL)

  w <- model %>% model_get_weights()
  n <- dplyr::tibble(
    term = colnames(tcm),
    n_obs = colSums(tcm * w)
  )
  attr(n, "N_obs") <- sum(w)

  n
}

#' @export
#' @rdname model_get_n
model_get_n.glm <- function(model) {
  tcm <- model %>% model_compute_terms_contributions()
  if (is.null(tcm)) return(NULL) # nocov

  w <- model %>% model_get_weights()
  n <- dplyr::tibble(
    term = colnames(tcm),
    n_obs = colSums(tcm * w)
  )
  attr(n, "N_obs") <- sum(w)

  ct <- model %>% model_get_coefficients_type()

  if (ct %in% c("logistic", "poisson")) {
    y <- model %>% model_get_response()
    if (is.factor(y)) {
      # the first level denotes failure and all others success
      y <- as.integer(y != levels(y)[1])
    }
    n$n_event <- colSums(tcm * y * w)
    attr(n, "N_event") <- sum(y * w)
  }

  if (ct == "poisson") {
    off <- model %>% model_get_offset()
    if (is.null(off)) off <- 0L
    n$exposure <- colSums(tcm * exp(off) * w)
    attr(n, "Exposure") <- sum(exp(off) * w)
  }

  n
}

#' @export
#' @rdname model_get_n
model_get_n.glmerMod <- model_get_n.glm

#' @export
#' @rdname model_get_n
model_get_n.multinom <- function(model) {
  tcm <- model %>% model_compute_terms_contributions()
  if (is.null(tcm)) return(NULL) # nocov

  w <- model %>% model_get_weights()
  y <- model %>% model_get_response()
  if (!is.factor(y)) y <- factor(y)

  n <- purrr::map_df(
    levels(y)[-1],
    ~ dplyr::tibble(
      y.level = .x,
      term = colnames(tcm),
      n_obs = colSums(tcm * w),
      n_event = colSums((y == .x) * tcm * w)
    )
  )
  attr(n, "N_obs") <- sum(w)
  attr(n, "N_event") <- sum((y != levels(y)[1]) * w)

  n
}
#' @export
#' @rdname model_get_n
model_get_n.LORgee <- function(model) {
  if (stringr::str_detect(model$title, "NOMINAL")) {
    model_get_n.multinom(model)
  } else {
    model_get_n.default(model)
  }
}

#' @export
#' @rdname model_get_n
model_get_n.coxph <- function(model) {
  tcm <- model %>% model_compute_terms_contributions()
  if (is.null(tcm)) return(NULL) # nocov

  w <- model %>% model_get_weights()
  n <- dplyr::tibble(
    term = colnames(tcm),
    n_obs = colSums(tcm * w)
  )
  attr(n, "N_obs") <- sum(w)

  y <- model %>% model_get_response()
  status <- y[, ncol(y)]
  if (ncol(y) == 3) {
    time <- y[, 2] - y[, 1]
  } else {
    time <- y[, 1]
  }

  n$n_event <- colSums(tcm * status * w)
  attr(n, "N_event") <- sum(status * w)
  n$exposure <- colSums(tcm * time * w)
  attr(n, "Exposure") <- sum(time * w)

  n
}

#' @export
#' @rdname model_get_n
model_get_n.survreg <- model_get_n.coxph

#' @export
#' @rdname model_get_n
model_get_n.model_fit <- function(model) {
  model_get_n(model$fit)
}

#' @export
#' @rdname model_get_n
model_get_n.tidycrr <- function(model) {
  tcm <- model %>% model_compute_terms_contributions()
  if (is.null(tcm)) return(NULL) # nocov

  w <- model %>% model_get_weights()
  n <- dplyr::tibble(
    term = colnames(tcm),
    n_obs = colSums(tcm * w)
  )
  attr(n, "N_obs") <- sum(w)

  y <- model %>% model_get_response()
  time <- y[, 1]
  status <- as.integer(y[, 2] == model$failcode)

  n$n_event <- colSums(tcm * status * w)
  attr(n, "N_event") <- sum(status * w)
  n$exposure <- colSums(tcm * time * w)
  attr(n, "Exposure") <- sum(time * w)

  n
}
