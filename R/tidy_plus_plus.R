#' Tidy a model and add compute additional informations
#'
#' This function will apply sequentially:
#' * [tidy_and_attach()]
#' * [tidy_identify_variables()]
#' * [tidy_add_contrasts()]
#' * [tidy_add_reference_rows()]
#' * [tidy_add_variable_labels()]
#' * [tidy_add_term_labels()]
#' * [tidy_remove_intercept()]
#' * [tidy_detach_model()]
#'
#' @param model a model to be attached/tidied
#' @param tidy_fun option to specify a custom tidier function
#' @param conf.int should confidence intervals be computed? (see [broom::tidy()])
#' @param variable_labels a named list or a named vector of custom variable labels
#' @param term_labels a named list or a named vector of custom term labels
#' @param add_reference_rows should reference rows be added?
#' @param intercept should the intercept(s) be included?
#' @param keep_model should the model be kept as an attribute of the final result?
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
#'   tidy_plus_plus()
#' ex2
#'
#' if(requireNamespace("gtsummary")) {
#'   ex3 <- glm(
#'     response ~ poly(age, 3) + stage + grade * trt,
#'     na.omit(gtsummary::trial),
#'     family = binomial,
#'     contrasts = list(stage = contr.treatment(4, base = 3),
#'                      grade = contr.sum)
#'   ) %>%
#'     tidy_plus_plus(variable_labels = c(age = "Age (in years)"))
#'   ex3
#' }
#' @export
tidy_plus_plus <- function(
  model,
  tidy_fun = broom::tidy,
  conf.int = TRUE,
  variable_labels = NULL,
  term_labels = NULL,
  intercept = FALSE,
  add_reference_rows = TRUE,
  keep_model = FALSE,
  ...
) {
  res <- model %>%
    tidy_and_attach(tidy_fun = tidy_fun, conf.int = conf.int, ...) %>%
    tidy_identify_variables() %>%
    tidy_add_contrasts()
  if (add_reference_rows)
    res <- res %>% tidy_add_reference_rows()
  res <- res %>%
    tidy_add_variable_labels(labels = variable_labels) %>%
    tidy_add_term_labels(labels = term_labels)
  if(!intercept)
    res <- res %>% tidy_remove_intercept()
  if (!keep_model)
    res <- res %>% tidy_detach_model()
  res
}
