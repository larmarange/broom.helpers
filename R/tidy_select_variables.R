#' Select variables to keep/drop
#'
#' Will remove unselected variables from the results.
#' To remove the intercept, use [tidy_remove_intercept()].
#'
#' @details
#' If the `variable` column is not yet available in `x`,
#' [tidy_identify_variables()] will be automatically applied.
#' @param x a tidy tibble
#' @param keep variables to keep
#' @param drop variables to drop
#' @param model the corresponding model, if not attached to `x`
#' @param quiet logical argument whether a message should not be
#' returned when requesting to keep/drop a variable not
#' existing in `x`
#' @param strict logical argument whether an error should be
#' returned when requesting to keep/drop a variable not
#' existing in `x`
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
#' res %>% tidy_select_variables(drop = "Class")
#' res %>% tidy_select_variables(keep = c("Age", "Sex"))
#'
tidy_select_variables <- function(
  x, keep = NULL, drop = NULL,
  model = tidy_get_model(x), quiet = FALSE, strict = FALSE
) {
  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  if (!"variable" %in% names(x)) {
    x <- x %>% tidy_identify_variables(model = model)
  }

  .attributes <- .save_attributes(x)

  current_variables <- na.omit(unique(x$variable))

  if (is.null(keep))
    keep <- current_variables

  not_found <- setdiff(keep, current_variables)
  if (length(not_found) > 0 && !quiet) {
    usethis::ui_oops(paste0(
      usethis::ui_code(not_found),
      " terms in 'keep' have not been found in ",
      usethis::ui_code("x"),
      "."
    ))
  }
  if (length(not_found) > 0 && strict) {
    stop("Incorrect call with `keep =`. Quitting execution.", call. = FALSE)
  }

  not_found <- setdiff(drop, current_variables)
  if (length(not_found) > 0 && !quiet) {
    usethis::ui_oops(paste0(
      usethis::ui_code(not_found),
      " terms in 'drop' have not been found in ",
      usethis::ui_code("x"),
      "."
    ))
  }
  if (length(not_found) > 0 && strict) {
    stop("Incorrect call with `drop =`. Quitting execution.", call. = FALSE)
  }

  selected_variables <- setdiff(keep, drop)

  x %>%
    dplyr::filter(
      .data$var_type == "intercept" |
        .data$variable %in% selected_variables
    ) %>%
    .order_tidy_columns() %>%
    tidy_attach_model(model = model, .attributes = .attributes)

}
