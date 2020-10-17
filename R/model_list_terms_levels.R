#' List levels of categorical terms
#'
#' Only for categorical variables with treatment,
#' SAS or sum contrasts.
#'
#' @param model a model object
#' @param label_pattern a [glue pattern][glue::glue()] for term labels (see examples)
#' @param variable_labels an optional named list or named vector of
#' custom variable labels passed to [model_list_variables()]
#' @return
#' A tibble with height columns:
#' * `variable`: variable
#' * `contrasts_type`: type of contrasts ("sum" or "treatment")
#' * `term`: term name
#' * `level`: term level
#' * `reference`: logical indicating which term is the reference level
#' * `reference_level`: level of the reference term
#' * `var_label`: variable label obtained with [model_list_variables()]
#' * `label`: term label (by default equal to term level)
#' The first seven columns can be used in `label_pattern`.
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
#' mod <- df %>%
#'   glm(
#'     Survived ~ Class + Age + Sex,
#'     data = ., weights = .$n, family = binomial,
#'     contrasts = list(Age = contr.sum, Class = "contr.helmert")
#'   )
#' mod %>% model_list_terms_levels()
#' mod %>% model_list_terms_levels("{level} vs {reference_level}")
#' mod %>% model_list_terms_levels("{variable} [{level} - {reference_level}]")
#' mod %>% model_list_terms_levels(
#'   "{ifelse(reference, level, paste(level, '-', reference_level))}"
#' )
model_list_terms_levels <- function(
  model,
  label_pattern = "{level}",
  variable_labels = NULL
) {
  UseMethod("model_list_terms_levels")
}

#' @export
#' @rdname model_list_terms_levels
model_list_terms_levels.default <- function(
  model, label_pattern = "{level}",
  variable_labels = NULL
) {
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

  if (nrow(model_terms) == 0)
    return(NULL)

  res <- dplyr::tibble()

  for (v in contrasts_list$variable) {
    if (v %in% names(xlevels)) {
      contrasts_type <- ifelse(
        contrasts_list$contrasts[contrasts_list$variable == v] == "contr.sum",
        "sum",
        "treatment"
      )
      term_levels <- xlevels[[v]]
      # terms could be named according to two approaches
      terms_names1 <- paste0(v, term_levels)
      terms_names2 <- paste0(v, seq(1, length(term_levels)))

      observed_terms <- model_terms$term[model_terms$variable == v]
      ref <- contrasts_list$reference[contrasts_list$variable == v]
      # observed terms correspond to first case
      if (length(observed_terms) > 0 & all(observed_terms %in% terms_names1[-ref])) {
        approach <- 1
      } else {
        # observed terms correspond to second case
        if (length(observed_terms) > 0 & all(observed_terms %in% terms_names2[-ref])) {
          approach <- 2
        } else {
          # it could be an interaction term only
          # we check what is the most frequent
          n1 <- .count_term(model_terms$term, terms_names1)
          n2 <- .count_term(model_terms$term, terms_names2)
          approach <- dplyr::if_else(n1 >= n2, 1, 2)
        }
      }

      if (approach == 1) {
        res <- dplyr::bind_rows(
          res,
          dplyr::tibble(
            variable = v,
            contrasts_type = contrasts_type,
            term = terms_names1,
            level = term_levels,
            reference = seq(1, length(term_levels)) == ref,
            reference_level = term_levels[ref]
          )
        )
      } else {
        res <- dplyr::bind_rows(
          res,
          dplyr::tibble(
            variable = v,
            contrasts_type = contrasts_type,
            term = terms_names2,
            level = term_levels,
            reference = seq(1, length(term_levels)) == ref,
            reference_level = term_levels[ref]
          )
        )
      }

    }
  }

  res %>%
    dplyr::left_join(
      model %>%
        model_list_variables(labels = variable_labels) %>%
        dplyr::select(all_of(c("variable", "var_label"))),
      by = "variable"
    ) %>%
    dplyr::mutate(label = stringr::str_glue_data(res, label_pattern))
}

# count the total number of times where elements of searched
# are found in observed terms
.count_term <- function(observed, searched) {
  total <- 0
  for (i in searched) {
    total <- total +
      stringr::str_count(observed, paste0("(^|:)", .escape_regex(i), "(:|$)")) %>%
      sum()
  }
  total
}

