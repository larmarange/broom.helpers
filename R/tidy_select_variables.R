#' Select variables to keep/drop
#'
#' Will remove unselected variables from the results.
#' To remove the intercept, use [tidy_remove_intercept()].
#'
#' @details
#' If the `variable` column is not yet available in `x`,
#' [tidy_identify_variables()] will be automatically applied.
#' @param x a tidy tibble
#' @param include variables to include. Accepts [tidyselect][dplyr::select]
#' syntax. Use `-` to remove a variable. Default is `everything()`.
#' See also [all_continuous()], [all_categorical()], [all_dichotomous()]
#' and [all_interaction()]
#' @return
#' The `x` tibble limited to the included variables (and eventually the intercept),
#' sorted according to the `include` parameter.
#'
#' @param model the corresponding model, if not attached to `x`
#' @export
#' @family tidy_helpers
#' @examples
#' res <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived)) %>%
#'   glm(Survived ~ Class + Age * Sex, data = ., weights = .$n, family = binomial) %>%
#'   tidy_and_attach() %>%
#'   tidy_identify_variables()
#'
#' res
#' res %>% tidy_select_variables()
#' res %>% tidy_select_variables(include = "Class")
#' res %>% tidy_select_variables(include = -c("Age", "Sex"))
#' res %>% tidy_select_variables(include = starts_with("A"))
#' res %>% tidy_select_variables(include = all_categorical())
#' res %>% tidy_select_variables(include = all_dichotomous())
#' res %>% tidy_select_variables(include = all_interaction())
#' res %>% tidy_select_variables(
#'   include = c("Age", all_categorical(dichotomous = FALSE), all_interaction())
#' )

tidy_select_variables <- function(
  x, include = everything(), model = tidy_get_model(x)
) {
  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  if (!"variable" %in% names(x)) {
    x <- x %>% tidy_identify_variables(model = model)
  }
  .attributes <- .save_attributes(x)

  # obtain character vector of selected variables
  include <- .select_to_varnames({{ include }}, var_info = x, arg_name = "include")

  # order result, intercept first then by the order of include
  if ("y.level" %in% names(x))
    x$group_order <- factor(x$y.level) %>% forcats::fct_inorder()
  else
    x$group_order <- 1

  x %>%
    dplyr::filter(
      .data$var_type == "intercept" |
        .data$variable %in% include
    ) %>%
    dplyr::mutate(
      log_intercept = .data$var_type == "intercept",
      fct_variable = factor(.data$variable, levels = include)
    ) %>%
    dplyr::arrange(
      .data$group_order,
      dplyr::desc(.data$log_intercept),
      .data$fct_variable
    ) %>%
    dplyr::select(
      -.data$group_order,
      -.data$log_intercept,
      -.data$fct_variable
    ) %>%
    tidy_attach_model(model = model, .attributes = .attributes)
}
