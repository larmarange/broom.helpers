#' Tidy a model and compute additional informations
#'
#' This function will apply sequentially:
#' * [tidy_and_attach()]
#' * [tidy_identify_variables()]
#' * [tidy_add_contrasts()]
#' * [tidy_add_reference_rows()]
#' * [tidy_add_estimate_to_reference_rows()]
#' * [tidy_add_variable_labels()]
#' * [tidy_add_term_labels()]
#' * [tidy_add_header_rows()]
#' * [tidy_remove_intercept()]
#' * [tidy_select_variables()]
#' * [tidy_detach_model()]
#'
#' @param model a model to be attached/tidied
#' @param tidy_fun option to specify a custom tidier function
#' @param conf.int should confidence intervals be computed? (see [broom::tidy()])
#' @param exponentiate logical indicating whether or not to exponentiate the
#' coefficient estimates. This is typical for logistic and multinomial regressions,
#' but a bad idea if there is no log or logit link. Defaults to `FALSE`.
#' @param variable_labels a named list or a named vector of custom variable labels
#' @param term_labels a named list or a named vector of custom term labels
#' @param add_reference_rows should reference rows be added?
#' @param add_estimate_to_reference_rows should an estimate value be added to reference rows?
#' @param add_header_rows should header rows be added?
#' @param show_single_row a vector indicating the names of binary
#' variables that should be displayed on a single row, when
#' `add_header_rows` is `TRUE`
#' @param intercept should the intercept(s) be included?
#' @param keep variables to keep
#' @param drop variables to drop
#' @param keep_model should the model be kept as an attribute of the final result?
#' @param quiet logical argument whether broom.helpers should not return a message
#' when requested output cannot be generated. Default is FALSE
#' @param strict logical argument whether broom.helpers should return an error
#' when requested output cannot be generated. Default is FALSE
#' @param ... other arguments passed to `tidy_fun()`
#' @family tidy_helpers
#' @examples
#' ex1 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris) %>%
#'   tidy_plus_plus()
#' ex1
#'
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(
#'     Survived = factor(Survived, c("No", "Yes"))
#'   ) %>%
#'   labelled::set_variable_labels(
#'     Class = "Passenger's class",
#'     Sex = "Gender"
#'   )
#'
#' ex2 <- glm(
#'   Survived ~ Class + Age * Sex,
#'   data = df, weights = df$n,
#'   family = binomial
#' ) %>%
#'   tidy_plus_plus(exponentiate = TRUE)
#' ex2
#'
#' if (requireNamespace("gtsummary")) {
#'   ex3 <- glm(
#'     response ~ poly(age, 3) + stage + grade * trt,
#'     na.omit(gtsummary::trial),
#'     family = binomial,
#'     contrasts = list(
#'       stage = contr.treatment(4, base = 3),
#'       grade = contr.sum
#'     )
#'   ) %>%
#'     tidy_plus_plus(
#'       exponentiate = TRUE,
#'       variable_labels = c(age = "Age (in years)"),
#'       add_header_rows = TRUE,
#'       show_single_row = "trt",
#'       term_labels = c("poly(age, 3)3" = "Cubic age"),
#'       keep_model = TRUE
#'     )
#'   ex3
#' }
#' @export
tidy_plus_plus <- function(
                           model,
                           tidy_fun = broom::tidy,
                           conf.int = TRUE,
                           exponentiate = FALSE,
                           variable_labels = NULL,
                           term_labels = NULL,
                           add_reference_rows = TRUE,
                           add_estimate_to_reference_rows = TRUE,
                           add_header_rows = FALSE,
                           show_single_row = NULL,
                           intercept = FALSE,
                           keep = NULL,
                           drop = NULL,
                           keep_model = FALSE,
                           quiet = FALSE,
                           strict = FALSE,
                           ...) {
  res <- model %>%
    tidy_and_attach(
      tidy_fun = tidy_fun,
      conf.int = conf.int,
      exponentiate = exponentiate,
      ...
    ) %>%
    tidy_identify_variables(strict = strict, quiet = quiet) %>%
    tidy_add_contrasts()
  if (add_reference_rows) {
    res <- res %>% tidy_add_reference_rows(quiet = quiet)
  }
  if (add_reference_rows & add_estimate_to_reference_rows) {
    res <- res %>%
      tidy_add_estimate_to_reference_rows(exponentiate = exponentiate, quiet = quiet)
  }
  res <- res %>%
    tidy_add_variable_labels(labels = variable_labels, quiet = quiet) %>%
    tidy_add_term_labels(labels = term_labels, quiet = quiet)
  if (add_header_rows) {
    res <- res %>%
      tidy_add_header_rows(show_single_row = show_single_row,
                           strict = strict, quiet = quiet)
  }
  if (!intercept) {
    res <- res %>% tidy_remove_intercept()
  }
  res <- res %>% tidy_select_variables(
    keep = keep,
    drop = drop,
    quiet = quiet,
    strict = strict
  )
  if (!keep_model) {
    res <- res %>% tidy_detach_model()
  }
  res %>%
    .order_tidy_columns()
}
