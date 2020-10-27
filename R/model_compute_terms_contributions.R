#' Compute a matrix of terms contributions
#'
#' Used for [model_add_n()]. For each row and term, equal 1 if this row should
#' be taken into account in the estimate of the number of observations, 0 otherwise.
#'
#' \lifecycle{experimental}
#'
#' @param model a model object
#' @export
#' @family model_helpers
#' @examples
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
#'   contrasts = list(stage = contr.sum, grade = contr.treatment(3, 2), trt = "contr.SAS")
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
#' mod <- glm(Survived ~ Class * Age + Sex, data = Titanic %>% as.data.frame(), weights = Freq, family = binomial)
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

  tcm <- tryCatch({
    formula <- stats::terms(model)
    # continuous variables converted to 1 to force positive values
    d <- model %>% purrr::pluck("data")
    if (is.null(d)) d <- model %>% model_get_model_frame()
    d <- d %>%
      dplyr::mutate(
        dplyr::across(
          where(~ is.numeric(.x) & !is.matrix(.x)), # check is.matrix for cbind variables
          ~ dplyr::if_else(is.na(.x), NA_integer_, 1L)
        )
      )
    stats::model.matrix(formula, data = d, contrasts.arg = contr)
  },
  error = function(e) {
    NULL
  })

  if (is.null(tcm)) {
    return(NULL)
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
      dplyr::filter(.data$variable == v & reference) %>%
      purrr::chuck("term")
    nonref_terms <- tl %>%
      dplyr::filter(.data$variable == v & !reference) %>%
      purrr::chuck("term")

    if (ct == "treatment" & all(nonref_terms %in% colnames(tcm))) {
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
    if (ct == "sum" & all(nonref_terms %in% colnames(tcm))) {
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

