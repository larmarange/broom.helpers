#' Tidy a model and compute additional informations
#'
#' This function will apply sequentially:
#' * [tidy_and_attach()]
#' * [tidy_disambiguate_terms()]
#' * [tidy_identify_variables()]
#' * [tidy_add_contrasts()]
#' * [tidy_add_reference_rows()]
#' * [tidy_add_pairwise_contrasts()]
#' * [tidy_add_estimate_to_reference_rows()]
#' * [tidy_add_variable_labels()]
#' * [tidy_add_term_labels()]
#' * [tidy_add_header_rows()]
#' * [tidy_add_n()]
#' * [tidy_remove_intercept()]
#' * [tidy_select_variables()]
#' * [tidy_add_coefficients_type()]
#' * [tidy_detach_model()]
#'
#' @param model a model to be attached/tidied
#' @param tidy_fun option to specify a custom tidier function
#' @param conf.int should confidence intervals be computed? (see [broom::tidy()])
#' @param conf.level level of confidence for confidence intervals (default: 95%)
#' @param exponentiate logical indicating whether or not to exponentiate the
#' coefficient estimates. This is typical for logistic, Poisson and Cox models,
#' but a bad idea if there is no log or logit link; defaults to `FALSE`.
#' @param model_matrix_attr logical indicating whether model frame and model
#' matrix should be added as attributes of `model` (respectively named
#' `"model_frame"` and `"model_matrix"`) and passed through
#' @param variable_labels a named list or a named vector of custom variable labels
#' @param term_labels a named list or a named vector of custom term labels
#' @param interaction_sep separator for interaction terms
#' @param categorical_terms_pattern a [glue pattern][glue::glue()] for
#' labels of categorical terms with treatment or sum contrasts
#' (see [model_list_terms_levels()])
#' @param disambiguate_terms should terms be disambiguated with
#' [tidy_disambiguate_terms()]? (default `TRUE`)
#' @param disambiguate_sep separator for [tidy_disambiguate_terms()]
#' @param add_reference_rows should reference rows be added?
#' @param no_reference_row variables (accepts [tidyselect][dplyr::select] notation)
#' for those no reference row should be added, when `add_reference_rows = TRUE`
#' @param add_pairwise_contrasts apply [tidy_add_pairwise_contrasts()]?
#' `r lifecycle::badge("experimental")`
#' @param pairwise_variables variables to add pairwise contrasts
#' (accepts [tidyselect][dplyr::select] notation)
#' @param keep_model_terms keep original model terms for variables where
#' pairwise contrasts are added? (default is `FALSE`)
#' @param pairwise_reverse determines whether to use `"pairwise"` (if `TRUE`)
#' or `"revpairwise"` (if `FALSE`), see [emmeans::contrast()]
#' @param contrasts_adjust optional adjustment method when computing contrasts,
#' see [emmeans::contrast()] (if `NULL`, use `emmeans` default)
#' @param emmeans_args list of additional parameter to pass to
#' [emmeans::emmeans()] when computing pairwise contrasts
#' @param add_estimate_to_reference_rows should an estimate value be added
#' to reference rows?
#' @param add_header_rows should header rows be added?
#' @param show_single_row variables that should be displayed
#' on a single row (accepts [tidyselect][dplyr::select] notation), when
#' `add_header_rows` is `TRUE`
#' @param add_n should the number of observations be added?
#' @param intercept should the intercept(s) be included?
#' @inheritParams tidy_select_variables
#' @param keep_model should the model be kept as an attribute of the final
#' result?
#' @param tidy_post_fun custom function applied to the results at the end of
#' `tidy_plus_plus()` (see note)
#' @param quiet logical argument whether broom.helpers should not return
#' a message when requested output cannot be generated. Default is `FALSE`
#' @param strict logical argument whether broom.helpers should return an error
#' when requested output cannot be generated. Default is `FALSE`
#' @param ... other arguments passed to `tidy_fun()`
#' @note
#' `tidy_post_fun` is applied to the result at the end of `tidy_plus_plus()`
#' and receive only one argument (the result of `tidy_plus_plus()`). However,
#' if needed, the model is still attached to the tibble as an attribute, even
#' if `keep_model = FALSE`. Therefore, it is possible to use [tidy_get_model()]
#' within `tidy_fun` if, for any reason, you need to access the source model.
#' @family tidy_helpers
#' @examplesIf interactive()
#' ex1 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris) |>
#'   tidy_plus_plus()
#' ex1
#'
#' df <- Titanic |>
#'   dplyr::as_tibble() |>
#'   dplyr::mutate(
#'     Survived = factor(Survived, c("No", "Yes"))
#'   ) |>
#'   labelled::set_variable_labels(
#'     Class = "Passenger's class",
#'     Sex = "Gender"
#'   )
#' ex2 <- glm(
#'   Survived ~ Class + Age * Sex,
#'   data = df, weights = df$n,
#'   family = binomial
#' ) |>
#'   tidy_plus_plus(
#'     exponentiate = TRUE,
#'     add_reference_rows = FALSE,
#'     categorical_terms_pattern = "{level} / {reference_level}",
#'     add_n = TRUE
#'   )
#' ex2
#' if (.assert_package("gtsummary", boolean = TRUE)) {
#'   ex3 <-
#'     glm(
#'       response ~ poly(age, 3) + stage + grade * trt,
#'       na.omit(gtsummary::trial),
#'       family = binomial,
#'       contrasts = list(
#'         stage = contr.treatment(4, base = 3),
#'         grade = contr.sum
#'       )
#'     ) |>
#'     tidy_plus_plus(
#'       exponentiate = TRUE,
#'       variable_labels = c(age = "Age (in years)"),
#'       add_header_rows = TRUE,
#'       show_single_row = all_dichotomous(),
#'       term_labels = c("poly(age, 3)3" = "Cubic age"),
#'       keep_model = TRUE
#'     )
#'   ex3
#' }
#' @export
tidy_plus_plus <- function(model,
                           tidy_fun = tidy_with_broom_or_parameters,
                           conf.int = TRUE,
                           conf.level = .95,
                           exponentiate = FALSE,
                           model_matrix_attr = TRUE,
                           variable_labels = NULL,
                           term_labels = NULL,
                           interaction_sep = " * ",
                           categorical_terms_pattern = "{level}",
                           disambiguate_terms = TRUE,
                           disambiguate_sep = ".",
                           add_reference_rows = TRUE,
                           no_reference_row = NULL,
                           add_pairwise_contrasts = FALSE,
                           pairwise_variables = all_categorical(),
                           keep_model_terms = FALSE,
                           pairwise_reverse = TRUE,
                           contrasts_adjust = NULL,
                           emmeans_args = list(),
                           add_estimate_to_reference_rows = TRUE,
                           add_header_rows = FALSE,
                           show_single_row = NULL,
                           add_n = TRUE,
                           intercept = FALSE,
                           include = everything(),
                           keep_model = FALSE,
                           tidy_post_fun = NULL,
                           quiet = FALSE,
                           strict = FALSE,
                           ...) {
  res <- model |>
    tidy_and_attach(
      tidy_fun = tidy_fun,
      conf.int = conf.int,
      conf.level = conf.level,
      exponentiate = exponentiate,
      model_matrix_attr = model_matrix_attr,
      ...
    )

  if (disambiguate_terms) {
    res <- res |>
      tidy_disambiguate_terms(sep = disambiguate_sep, quiet = quiet)
  }

  res <- res |>
    tidy_identify_variables(quiet = quiet) |>
    tidy_add_contrasts()

  if (add_reference_rows) {
    res <- res |> tidy_add_reference_rows(
      no_reference_row = {{ no_reference_row }},
      quiet = quiet
    )
  }

  if (add_pairwise_contrasts) {
    res <- res |>
      tidy_add_pairwise_contrasts(
        variables = {{ pairwise_variables }},
        keep_model_terms = keep_model_terms,
        pairwise_reverse = pairwise_reverse,
        contrasts_adjust = contrasts_adjust,
        emmeans_args = emmeans_args
      )
  }

  if (add_reference_rows && add_estimate_to_reference_rows) {
    res <- res |>
      tidy_add_estimate_to_reference_rows(exponentiate = exponentiate, quiet = quiet)
  }

  res <- res |>
    tidy_add_variable_labels(
      labels = variable_labels,
      interaction_sep = interaction_sep,
      quiet = quiet
    ) |>
    tidy_add_term_labels(
      labels = term_labels,
      interaction_sep = interaction_sep,
      categorical_terms_pattern = categorical_terms_pattern,
      quiet = quiet
    )

  if (add_header_rows) {
    res <- res |>
      tidy_add_header_rows(
        show_single_row = {{ show_single_row }},
        strict = strict, quiet = quiet
      )
  }

  if (add_n) {
    res <- res |> tidy_add_n()
  }

  if (!intercept) {
    res <- res |> tidy_remove_intercept()
  }

  res <- res |>
    tidy_select_variables(
      include = {{ include }},
    ) |>
    tidy_add_coefficients_type()

  if (!is.null(tidy_post_fun))
    res <- res |> tidy_post_fun()

  if (!keep_model) {
    res <- res |> tidy_detach_model()
  }

  res
}
