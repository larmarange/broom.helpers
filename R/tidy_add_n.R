#' Add the (weighted) number of observations
#'
#' Add the number of observations in a new column `n_obs`, taking into account any
#' weights if they have been defined.
#'
#' For continuous variables, it corresponds to all valid observations
#' contributing to the model.
#'
#' For categorical variables coded with treatment or sum contrasts,
#' each model term could be associated to only one level of the original
#' categorical variable. Therefore, `n_obs` will correspond to the number of
#' observations associated with that level. `n_obs` will also be computed for
#' reference rows. For polynomial contrasts (defined with [stats::contr.poly()]),
#' all levels will contribute to the computation of each model term. Therefore,
#' `n_obs` will be equal to the total number of observations. For Helmert and custom
#' contrasts, only rows contributing positively (i.e. with a positive contrast)
#' to the computation of a term will be considered for estimating `n_obs`. The
#' result could therefore be difficult to interpret. For a better understanding
#' of which observations are taken into account to compute `n_obs` values, you
#' could look at [model_compute_terms_contributions()].
#'
#' For interaction terms, only rows contributing to all the terms of the
#' interaction will be considered to compute `n_obs`.
#'
#' For binomial logistic models, `tidy_add_n()` will also return the
#' corresponding number of events (`n_event`) for each term, taking into account
#' any defined weights. Observed proportions could be obtained as `n_obs / n_event`.
#'
#' Similarly, a number of events will be computed for multinomial logistic
#' models (`nnet::multinom()`) for each level of the outcome (`y.level`),
#' corresponding to the number of observations equal to that outcome level.
#'
#' For Poisson models, `n_event` will be equal to the number of counts per term.
#' In addition, a third column `exposure` will be computed. If no offset is
#' defined, exposure is assumed to be equal to 1 (eventually multiplied by
#' weights) per observation. If an offset is defined, `exposure` will be equal
#' to the (weighted) sum of the exponential of the offset (as a reminder, to
#' model the effect of `x` on the ratio `y / z`, a Poisson model will be defined
#' as `glm(y ~ x + offset(log(z)), family = poisson)`). Observed rates could be
#' obtained with `n_event / exposure`.
#'
#' For Cox models ([survival::coxph()]), an individual could be coded
#' with several observations (several rows). `n_obs` will correspond to the weighted
#' number of observations which could be different from the number of
#' individuals. `tidy_add_n()` will also compute a (weighted) number of events
#' (`n_event`) according to the definition of the [survival::Surv()] object.
#' Exposure time is also returned in `exposure` column. It is equal to the
#' (weighted) sum of the time variable if only one variable time is passed to
#' [survival::Surv()], and to the (weighted) sum of `time2 - time` if two time
#' variables are defined in [survival::Surv()].
#'
#' For competing risk regression models ([tidycmprsk::crr()]), `n_event` takes
#' into account only the event of interest defined by `failcode.`
#'
#' The (weighted) total number of observations (`N_obs`), of events (`N_event`) and
#' of exposure time (`Exposure`) are stored as attributes of the returned
#' tibble.
#'
#' @param x a tidy tibble
#' @param model the corresponding model, if not attached to `x`
#' @export
#' @family tidy_helpers
#' @examplesIf interactive()
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
#'   response ~ trt * grade + offset(log(ttdeath)),
#'   gtsummary::trial,
#'   family = poisson
#' ) %>%
#'   tidy_and_attach() %>%
#'   tidy_add_n()
tidy_add_n <- function(x, model = tidy_get_model(x)) {
  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  .attributes <- .save_attributes(x)

  if (any(c("n_obs", "n_event", "exposure") %in% names(x))) {
    x <- x %>% dplyr::select(-dplyr::any_of(c("n_obs", "n_event", "exposure")))
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

  if (!is.null(attr(n, "N_obs")))
    .attributes$N_obs <- attr(n, "N_obs")
  if (!is.null(attr(n, "N_event")))
    .attributes$N_event <- attr(n, "N_event")
  if (!is.null(attr(n, "Exposure")))
    .attributes$Exposure <- attr(n, "Exposure")

  x %>%
    tidy_attach_model(model = model, .attributes = .attributes)
}
