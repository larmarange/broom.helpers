#' Add contrasts type for categorical variables
#'
#' Add a `contrasts` column corcontrasts_listponding to the type of contrasts
#'
#' @details
#' If the `variable` column is not yet available in `x`,
#' [tidy_identify_variables()] will be automatically applied.
#'
#' @param x a tidy tibble
#' @param model the corresponding model, if not attached to `x`
#' @export
#' @family tidy_helpers
#' @examples
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
#'   tidy_and_attach() %>%
#'   tidy_add_contrasts()
tidy_add_contrasts <- function(x, model = tidy_get_model(x)) {
  if (is.null(model))
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")

  if (!"variable" %in% names(x))
    x <- x %>% tidy_identify_variables(model)

  model_contrasts <- model_get_contrasts(model)

  if (length(model_contrasts) == 0) {
    x$contrasts <- NA_character_
  } else {
    contrasts_list <- tibble::tibble(
      variable = names(model_contrasts),
      contrasts = NA_character_
    )
    xlevels <- model_get_xlevels(model)
    for (i in 1:nrow(contrasts_list)) {
      n_levels <- length(xlevels[[contrasts_list$variable[i]]])

      if (is.character(model_contrasts[[i]]) & length(is.character(model_contrasts[[i]]) == 1))
        contrasts_list$contrasts[[i]] <- model_contrasts[[i]]
      else if (all(model_contrasts[[i]] == stats::contr.treatment(n_levels)))
        contrasts_list$contrasts[[i]] <- "contr.treatment"
      else if (all(model_contrasts[[i]] == stats::contr.sum(n_levels)))
        contrasts_list$contrasts[[i]] <- "contr.sum"
      else if (all(model_contrasts[[i]] == stats::contr.helmert(n_levels)))
        contrasts_list$contrasts[[i]] <- "contr.helmert"
      else if (all(model_contrasts[[i]] == stats::contr.poly(n_levels)))
        contrasts_list$contrasts[[i]] <- "contr.poly"
      else if (all(model_contrasts[[i]] == stats::contr.SAS(n_levels)))
        contrasts_list$contrasts[[i]] <- "contr.SAS"
      else
        contrasts_list$contrasts[[i]] <- "custom"
    }
    x <- x %>%
      dplyr::left_join(contrasts_list, by = "variable")
  }
  x %>%
    tidy_attach_model(model)
}


