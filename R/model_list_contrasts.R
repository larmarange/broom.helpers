#' List contrasts used by a model
#'
#' @param model a model object
#' @return
#' A tibble with three columns:
#' * `variable`: variable name
#' * `contrasts`: type of contrasts
#' @export
#' @family model_helpers
#' @examples
#' glm(
#'   am ~ mpg + factor(cyl),
#'   data = mtcars,
#'   family = binomial,
#'   contrasts = list(`factor(cyl)` = contr.sum)
#' ) %>%
#'   model_list_contrasts()
model_list_contrasts <- function(model) {
  UseMethod("model_list_contrasts")
}

#' @export
#' @rdname model_list_contrasts
model_list_contrasts.default <- function(model) {
  model_contrasts <- model_get_contrasts(model)

  if (length(model_contrasts) == 0) {
    return(NULL)
  }

  contrasts_list <- tibble::tibble(
    variable = names(model_contrasts),
    contrasts = NA_character_
  )
  xlevels <- model_get_xlevels(model)
  for (i in seq_len(nrow(contrasts_list))) {
    n_levels <- length(xlevels[[contrasts_list$variable[i]]])

    if (is.character(model_contrasts[[i]]) & length(is.character(model_contrasts[[i]]) == 1)) {
      contrasts_list$contrasts[[i]] <- model_contrasts[[i]]
    } else if (all(model_contrasts[[i]] == stats::contr.treatment(n_levels))) {
      contrasts_list$contrasts[[i]] <- "contr.treatment"
    } else if (all(model_contrasts[[i]] == stats::contr.sum(n_levels))) {
      contrasts_list$contrasts[[i]] <- "contr.sum"
    } else if (all(model_contrasts[[i]] == stats::contr.helmert(n_levels))) {
      contrasts_list$contrasts[[i]] <- "contr.helmert"
    } else if (all(model_contrasts[[i]] == stats::contr.poly(n_levels))) {
      contrasts_list$contrasts[[i]] <- "contr.poly"
    } else if (all(model_contrasts[[i]] == stats::contr.SAS(n_levels))) {
      contrasts_list$contrasts[[i]] <- "contr.SAS"
    } else {
      for (j in 2:n_levels) { # testing treatment coding width different value for base variable
        if (all(model_contrasts[[i]] == stats::contr.treatment(n_levels, base = j))) {
          contrasts_list$contrasts[[i]] <- paste0("contr.treatment(base=", j, ")")
        }
      }
    }

    # if still not found, just indicate custom contrast
    if (is.na(contrasts_list$contrasts[[i]])) {
      contrasts_list$contrasts[[i]] <- "custom"
    }
  }
  contrasts_list
}

