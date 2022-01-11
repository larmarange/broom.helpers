#' Get xlevels used in the model
#'
#' @param model a model object
#' @export
#' @family model_helpers
#' @examples
#' lm(hp ~ mpg + factor(cyl), mtcars) %>%
#'   model_get_xlevels()
model_get_xlevels <- function(model) {
  UseMethod("model_get_xlevels")
}

#' @export
#' @rdname model_get_xlevels
model_get_xlevels.default <- function(model) {
  xlevels <- tryCatch(
    model %>% purrr::chuck("xlevels"),
    error = function(e) {
      NULL # nocov
    }
  )
  xlevels %>% .add_xlevels_for_logical_variables(model)
}

.add_xlevels_for_logical_variables <- function(xlevels, model) {
  log_vars <- model %>%
    model_list_variables() %>%
    dplyr::filter(.data$var_class == "logical") %>%
    purrr::pluck("variable")

  for (v in setdiff(log_vars, names(xlevels)))
    xlevels[[v]] <- c("FALSE", "TRUE")

  xlevels
}

#' @export
#' @rdname model_get_xlevels
model_get_xlevels.lmerMod <- function(model) {
  stats::model.frame(model) %>%
    lapply(levels) %>%
    purrr::compact() %>% # keep only not null
    .add_xlevels_for_logical_variables(model)
}


#' @export
#' @rdname model_get_xlevels
model_get_xlevels.glmerMod <- model_get_xlevels.lmerMod

#' @export
#' @rdname model_get_xlevels
model_get_xlevels.felm <- model_get_xlevels.lmerMod

#' @export
#' @rdname model_get_xlevels
model_get_xlevels.brmsfit <- model_get_xlevels.lmerMod

#' @export
#' @rdname model_get_xlevels
model_get_xlevels.glmmTMB <- model_get_xlevels.lmerMod

#' @export
#' @rdname model_get_xlevels
model_get_xlevels.plm <- model_get_xlevels.lmerMod
