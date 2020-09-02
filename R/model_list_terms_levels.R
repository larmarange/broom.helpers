#' List levels of categorical terms
#'
#' Only for categorical variables with a treatment,
#' a SAS or a sum contrasts.
#'
#' @param model a model object
#' @return
#' A tibble with four columns:
#' * `variable`: variable
#' * `term`: term name
#' * `level`: term level
#' * `reference`: logical indicating which term is the reference level
#' @export
#' @family model_helpers
#' @examples
#' glm(
#'   am ~ mpg + factor(cyl),
#'   data = mtcars,
#'   family = binomial,
#'   contrasts = list(`factor(cyl)` = contr.sum)
#' ) %>%
#'   model_list_terms_levels()
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
#'   model_list_terms_levels()
model_list_terms_levels <- function(model) {
  UseMethod("model_list_terms_levels")
}

#' @export
#' @rdname model_list_terms_levels
model_list_terms_levels.default <- function(model) {
  contrasts_list <- model_list_contrasts(model)
  if (is.null(contrasts_list))
    return(NULL)

  contrasts_list <- contrasts_list %>%
    # keep only treatment, SAS and sum contrasts
    dplyr::filter(
      .data$contrasts %>%
        stringr::str_starts("contr.treatment|contr.SAS|contr.sum")
    )
  xlevels <- model_get_xlevels(model)

  if (nrow(contrasts_list) == 0 | length(xlevels) == 0)
    return(NULL)

  model_terms <- model_identify_variables(model) %>%
    dplyr::filter(!is.na(.data$variable))

  res <- dplyr::tibble()

  for (v in contrasts_list$variable) {
    if (v %in% names(xlevels)) {
      term_levels <- xlevels[[v]]
      # terms could be named according to two approaches
      terms_names1 <- paste0(v, term_levels)
      terms_names2 <- paste0(v, seq(1, length(term_levels)))

      observed_terms <- model_terms$term[model_terms$variable == v]
      ref <- contrasts_list$reference[contrasts_list$variable == v]
      # observed terms correspond to first case
      if (all(observed_terms %in% terms_names1[-ref])) {
        res <- dplyr::bind_rows(
          res,
          dplyr::tibble(
            variable = v,
            term = terms_names1,
            label = term_levels,
            reference = seq(1, length(term_levels)) == ref
          )
        )
      } else {
        # observed terms correspond to second case
        if (all(observed_terms %in% terms_names2[-ref])) {
          res <- dplyr::bind_rows(
            res,
            dplyr::tibble(
              variable = v,
              term = terms_names2,
              label = term_levels,
              reference = seq(1, length(term_levels)) == ref
            )
          )
        } else {
          # it could be an interaction term only
          # we check what is the most frequent
          n1 <- .count_term(model_terms$term, terms_names1[-ref])
          n2 <- .count_term(model_terms$term, terms_names2[-ref])
          if (n1 >= n2) {
            res <- dplyr::bind_rows(
              res,
              dplyr::tibble(
                variable = v,
                term = terms_names1,
                label = term_levels,
                reference = seq(1, length(term_levels)) == ref
              )
            )
          } else {
            res <- dplyr::bind_rows(
              res,
              dplyr::tibble(
                variable = v,
                term = terms_names2,
                label = term_levels,
                reference = seq(1, length(term_levels)) == ref
              )
            )
          }
        }
      }
    }
  }

  res
}

# count the total number of times where elements of searched
# are found in observed terms
.count_term <- function(observed, searched) {
  total <- 0
  for (i in searched) {
    total <- total +
      stringr::str_count(observed, paste0("(^|:)", i, "(:|$)")) %>%
      sum()
  }
  total
}
