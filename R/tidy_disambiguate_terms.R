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
#' @param x (`data.frame`)\cr
#' A tidy tibble as produced by `tidy_*()` functions.
#' @param sep (`string`)\cr
#' Separator added between group name and term.
#' @param model (a model object, e.g. `glm`)\cr
#' The corresponding model, if not attached to `x`.
#' @inheritParams tidy_plus_plus
#' @export
#' @family tidy_helpers
#' @examples
#' \donttest{
#' if (
#'   .assert_package("lme4", boolean = TRUE) &&
#'     .assert_package("broom.mixed", boolean = TRUE) &&
#'     .assert_package("gtsummary", boolean = TRUE)
#' ) {
#'   mod <- lme4::lmer(marker ~ stage + (1 | grade) + (death | response), gtsummary::trial)
#'   mod |>
#'     tidy_and_attach() |>
#'     tidy_disambiguate_terms()
#' }
#' }
tidy_disambiguate_terms <- function(x, sep = ".", model = tidy_get_model(x), quiet = FALSE) {
  if (is.null(model)) {
    cli::cli_abort(c(
      "{.arg model} is not provided.",
      "You need to pass it or to use {.fn tidy_and_attach}."
    ))
  }
  if ("original_term" %in% names(x)) {
    if (
      !quiet &&
        !inherits(model, "LORgee") && # no alert for multgee models
        !inherits(model, "zeroinfl") && # or zeroninfl/hurdle
        !inherits(model, "hurdle") &&
        !inherits(model, "vgam") && # vgam models
        !inherits(model, "vglm") &&
        !inherits(model, "svy_vglm")
    ) {
      cli_alert_danger(paste(
        "{.code tidy_disambiguate_terms()} has already been applied.",
        "x has been returned unchanged."
      ))
    }
    return(x)
  }

  .attributes <- .save_attributes(x)

  if ("group" %in% names(x)) {
    x <- x |>
      dplyr::mutate(
        original_term = .data$term,
        term = dplyr::if_else(
          is.na(.data$group) | .data$group == "",
          .data$term,
          paste(.data$group, .data$term, sep = sep)
        )
      )
  }

  x |>
    tidy_attach_model(model = model, .attributes = .attributes)
}
