#' Select variables to keep/drop
#'
#' Will remove unselected variables from the results.
#' To remove the intercept, use [tidy_remove_intercept()].
#'
#' @details
#' If the `variable` column is not yet available in `x`,
#' [tidy_identify_variables()] will be automatically applied.
#' @param x a tidy tibble
#' @param keep variables to keep. Use `-` to remove a variable.
#' Default is `everything()`
#' @param model the corresponding model, if not attached to `x`
#' @export
#' @family tidy_helpers
#' @examples
#' res <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived)) %>%
#'   glm(Survived ~ Class + Age + Sex, data = ., weights = .$n, family = binomial) %>%
#'   tidy_and_attach() %>%
#'   tidy_identify_variables()
#'
#' res
#' res %>% tidy_select_variables()
#' res %>% tidy_select_variables(keep = "Class")
#' res %>% tidy_select_variables(keep = -c("Age", "Sex"))
#' res %>% tidy_select_variables(keep = starts_with("A"))

tidy_select_variables <- function(
  x, keep = everything(), model = tidy_get_model(x)
) {
  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  if (!"variable" %in% names(x)) {
    x <- x %>% tidy_identify_variables(model = model)
  }
  .attributes <- .save_attributes(x)

  # obtain character vector of selected variables
  keep <- .tidy_tidyselect(x, {{ keep }})

  x %>%
    dplyr::filter(
      .data$var_type == "intercept" |
        .data$variable %in% keep
    ) %>%
    tidy_attach_model(model = model, .attributes = .attributes)

}
