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
#' @inheritParams tidy_plus_plus
#' @export
#' @family tidy_helpers
#' @examplesIf interactive()
#' if (.assert_package("lme4", boolean = TRUE) && .assert_package("broom.mixed", boolean = TRUE) && .assert_package("gtsummary", boolean = TRUE)) {
#' mod <- lme4::lmer(marker ~ stage + (1|grade) + (death|response), gtsummary::trial)
#' mod %>% tidy_and_attach() %>% tidy_disambiguate_terms()
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
          .data$term,
          paste(.data$group, .data$term, sep = sep)
        ),
        original_term = .data$term
      )
  }

  x %>%
    tidy_attach_model(model = model, .attributes = .attributes)
}
