#' Disambiguate terms
#'
#' For mixed models, the `term` column returned by `broom.mixed` may have
#' duplicated values for random-effect parameters and random-effect values.
#' In such case, the terms could be disambiguated be prefixing them with the
#' value of the `group` column. `tidy_disambiguate_terms()` will not change
#' any term if there is no `group` column in `x`. The original term value
#' is kept in a new column `original_term`.
#'
#'
#' @param x a tidy tibble
#' @param sep character, separator added between group name and term
#' @param model the corresponding model, if not attached to `x`
#' @export
#' @family tidy_helpers
#' @examples
#' if (require(lme4) & require(broom.mixed) & require(gtsummary)) {
#'   mod <- lme4::lmer(marker ~ stage + (1|grade) + (death|response), trial)
#'   mod %>% tidy_and_attach() %>% tidy_disambiguate_terms()
#' }
tidy_disambiguate_terms <- function(x, sep = ".", model = tidy_get_model(x), quiet = FALSE) {
  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  if ("original_term" %in% names(x)) {
    if (!quiet) {
      cli_alert_danger("{.code tidy_disambiguate_terms()} has already been applied. x has been returned unchanged.")
    }
    return(x)
  }

  .attributes <- .save_attributes(x)

  if ("group" %in% names(x)) {
    x <- x %>%
      dplyr::mutate(
        term = dplyr::if_else(
          is.na(.data$group),
          term,
          paste(.data$group, .data$term, sep = sep)
        ),
        original_term = term
      )
  }

  x %>%
    tidy_attach_model(model = model, .attributes = .attributes)
}
