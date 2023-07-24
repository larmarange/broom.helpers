#' Compute a matrix of terms contributions
#'
#' @description
#'
#' Used for [model_get_n()]. For each row and term, equal 1 if this row should
#' be taken into account in the estimate of the number of observations,
#' 0 otherwise.
#'
#' @details
#' This function does not cover `lavaan` models (`NULL` is returned).
#'
#' @param model a model object
#' @export
#' @family model_helpers
#' @examplesIf interactive()
#' mod <- lm(Sepal.Length ~ Sepal.Width, iris)
#' mod %>% model_compute_terms_contributions()
#'
#' mod <- lm(hp ~ mpg + factor(cyl) + disp:hp, mtcars)
#' mod %>% model_compute_terms_contributions()
#'
#' mod <- glm(
#'   response ~ stage * grade + trt,
#'   gtsummary::trial,
#'   family = binomial,
#'   contrasts = list(
#'     stage = contr.sum,
#'     grade = contr.treatment(3, 2),
#'     trt = "contr.SAS"
#'   )
#' )
#' mod %>% model_compute_terms_contributions()
#'
#' mod <- glm(
#'   response ~ stage * trt,
#'   gtsummary::trial,
#'   family = binomial,
#'   contrasts = list(stage = contr.poly)
#' )
#' mod %>% model_compute_terms_contributions()
#'
#' mod <- glm(
#'   Survived ~ Class * Age + Sex,
#'   data = Titanic %>% as.data.frame(),
#'   weights = Freq, family = binomial
#' )
#' mod %>% model_compute_terms_contributions()
#'
#' d <- dplyr::as_tibble(Titanic) %>%
#'   dplyr::group_by(Class, Sex, Age) %>%
#'   dplyr::summarise(
#'     n_survived = sum(n * (Survived == "Yes")),
#'     n_dead = sum(n * (Survived == "No"))
#'   )
#' mod <- glm(cbind(n_survived, n_dead) ~ Class * Age + Sex, data = d, family = binomial)
#' mod %>% model_compute_terms_contributions()
model_compute_terms_contributions <- function(model) {
  UseMethod("model_compute_terms_contributions")
}

#' @export
#' @rdname model_compute_terms_contributions
model_compute_terms_contributions.default <- function(model) {
  contr <- model %>% model_get_contrasts()

  # check poly contrasts
  # we change the contrasts arguments to force positive values
  if (!is.null(contr)) {
    list.contr.poly <- model %>%
      model_list_contrasts() %>%
      dplyr::filter(.data$contrasts == "contr.poly") %>%
      purrr::pluck("variable")
    for (v in list.contr.poly) {
      contr[[v]] <- contr.poly.abs
    }
  }

  tcm <- tryCatch(
    {
      formula <- model_get_terms(model)
      if (is.null(formula)) {
        return(NULL)
      } # stop

      # continuous variables converted to 1 to force positive values
      d <- model %>% purrr::pluck("data")
      if (is.null(d)) d <- model %>% model_get_model_frame()

      if (is.null(d)) {
        return(NULL)
      } # stop

      d <- d %>%
        dplyr::mutate(
          dplyr::across(
            where(~ is.numeric(.x) & (
              # check is.matrix for cbind variables
              # but include polynomial terms
              !is.matrix(.x) | inherits(.x, "poly")
            )),
            ~ abs(.x) + 1 # force positive value
          )
        )
      stats::model.matrix(formula, data = d, contrasts.arg = contr)
    },
    error = function(e) {
      NULL # nocov
    }
  )

  if (is.null(tcm)) {
    return(NULL) # nocov
  }

  tcm <- .add_ref_terms_to_tcm(model, tcm)

  # keep only positive terms
  tcm <- tcm > 0
  storage.mode(tcm) <- "integer"
  tcm
}

contr.poly.abs <- function(...) {
  stats::contr.poly(...) %>% abs()
}

.add_ref_terms_to_tcm <- function(model, tcm) {
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
      dplyr::filter(.data$variable == v & .data$reference) %>%
      purrr::chuck("term")
    nonref_terms <- tl %>%
      dplyr::filter(.data$variable == v & !.data$reference) %>%
      purrr::chuck("term")

    if (ct == "treatment" && all(nonref_terms %in% colnames(tcm))) {
      tcm <- cbind(
        tcm,
        matrix(
          as.integer(
            rowSums(tcm[, nonref_terms, drop = FALSE] == 0L) ==
              length(nonref_terms)
          ),
          ncol = 1,
          dimnames = list(NULL, ref_term)
        )
      )
    }
    if (ct == "sum" && all(nonref_terms %in% colnames(tcm))) {
      tcm <- cbind(
        tcm,
        matrix(
          as.integer(
            rowSums(tcm[, nonref_terms, drop = FALSE] == -1L) ==
              length(nonref_terms)
          ),
          ncol = 1,
          dimnames = list(NULL, ref_term)
        )
      )
    }
  }
  tcm
}
