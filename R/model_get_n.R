#' Get the number of observations
#'
#' \lifecycle{experimental}
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
#' mod <- glm(Survived ~ Class * Age + Sex, data = Titanic %>% as.data.frame(), weights = Freq, family = binomial)
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
model_get_n <- function(model) {
  UseMethod("model_get_n")
}

#' @export
#' @rdname model_get_n
model_get_n.default <- function(model) {
  # add a warning if contr.poly or custom contrasts

  # computation will be done using a term matrix (tm)
  # adapted from model matrix
  # only positive terms are taken into account
  mf <- model %>% model_get_model_frame()
  tryCatch({
    mf2 <- mf %>%
      dplyr::mutate(
        dplyr::across(
          c(where(is.numeric), -dplyr::any_of("(weights)")),
          ~ 1L
        )
      )
    tm <- model %>%
      model_get_model_matrix(data = mf2)
  },
  error = function(e) {
    tm <- NULL
  })
  if (is.null(tm)) {
    tryCatch({
      model2 <- model
      model2$model <- model2$model %>%
        dplyr::mutate(
          dplyr::across(
            c(where(is.numeric), -dplyr::any_of("(weights)")),
            ~ 1L
          )
        )
      tm <- model2 %>%
        model_get_model_matrix()
    },
    error = function(e) {
      tm <- NULL
    })
  }
  if (is.null(tm)) {
    return(NULL)
  }

  # getting weights
  if ("prior.weights" %in% names(model)) {
    weights <- model$prior.weights
  } else {
    if ("(weights)" %in% colnames(mf)) {
      weights <- mf[["(weights)"]]
    } else {
      weights <- rep.int(1L, nrow(mf))
    }
  }

  # adding reference terms
  # for treatment and sum contrasts
  tl <- model %>%
    model_list_terms_levels()
  for (v in unique(tl$variable)) {
    ct <- tl %>%
      dplyr::filter(.data$variable == v) %>%
      purrr::chuck("contrasts_type") %>%
      dplyr::first()
    ref_term <- tl %>%
      dplyr::filter(.data$variable == v & reference) %>%
      purrr::chuck("term")
    nonref_terms <- tl %>%
      dplyr::filter(.data$variable == v & !reference) %>%
      purrr::chuck("term")

    if (ct == "treatment" & all(nonref_terms %in% colnames(tm))) {
      tm <- cbind(
        tm,
        matrix(
          as.integer(
            rowSums(tm[, nonref_terms, drop = FALSE] == 0L) ==
              length(nonref_terms)
          ),
          ncol = 1,
          dimnames = list(NULL, ref_term)
        )
      )
    }
    if (ct == "sum" & all(nonref_terms %in% colnames(tm))) {
      tm <- cbind(
        tm,
        matrix(
          as.integer(
            rowSums(tm[, nonref_terms, drop = FALSE] == -1L) ==
              length(nonref_terms)
          ),
          ncol = 1,
          dimnames = list(NULL, ref_term)
        )
      )
    }
  }

  res <- dplyr::tibble(
    term = colnames(tm),
    n = colSums((tm > 0) * weights)
  )
  attr(res, "N") <- sum(weights)

  # adding n_events
  # look at model$y (* model$prior.weights) if exists
  if (model %>% model_get_coefficients_type() == "logistic") {
    y <- model$y
    res$nevent <- colSums((tm > 0) * y * weights)
    attr(res, "Nevent") <- sum(y * weights)
  }

  res
}
