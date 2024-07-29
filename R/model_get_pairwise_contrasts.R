#' Get pairwise comparison of the levels of a categorical variable
#'
#' It is computed with [emmeans::emmeans()].
#'
#' @param model a model object
#' @param variables names of variables to add pairwise contrasts
#' @param pairwise_reverse determines whether to use `"pairwise"` (if `TRUE`)
#' or `"revpairwise"` (if `FALSE`), see [emmeans::contrast()]
#' @param contrasts_adjust optional adjustment method when computing contrasts,
#' see [emmeans::contrast()] (if `NULL`, use `emmeans` default)
#' @param conf.level level of confidence for confidence intervals
#' @param emmeans_args list of additional parameter to pass to
#' [emmeans::emmeans()] when computing pairwise contrasts
#' @details
#' `r lifecycle::badge("experimental")`
#' For `pscl::zeroinfl()` and `pscl::hurdle()` models, pairwise contrasts are
#' computed separately for each component, using `mode = "count"` and
#' `mode = "zero"` (see documentation of `emmeans`) and a component column
#' is added to the results. This support is still experimental.
#' @family model_helpers
#' @export
#' @examplesIf interactive()
#' if (.assert_package("emmeans", boolean = TRUE)) {
#'   mod <- lm(Sepal.Length ~ Species, data = iris)
#'   mod |> model_get_pairwise_contrasts(variables = "Species")
#'   mod |>
#'     model_get_pairwise_contrasts(
#'       variables = "Species",
#'       contrasts_adjust = "none"
#'     )
#' }
model_get_pairwise_contrasts <- function(
    model,
    variables,
    pairwise_reverse = TRUE,
    contrasts_adjust = NULL,
    conf.level = .95,
    emmeans_args = list()) {
  UseMethod("model_get_pairwise_contrasts")
}

#' @export
model_get_pairwise_contrasts.default <- function(
    model,
    variables,
    pairwise_reverse = TRUE,
    contrasts_adjust = NULL,
    conf.level = .95,
    emmeans_args = list()) {
  purrr::map_df(
    variables,
    .get_pairwise_contrasts_one_var,
    model = model,
    pairwise_reverse = pairwise_reverse,
    contrasts_adjust = contrasts_adjust,
    conf.level = conf.level,
    emmeans_args = emmeans_args
  )
}

.get_pairwise_contrasts_one_var <- function(
    model,
    variable,
    pairwise_reverse = TRUE,
    contrasts_adjust = NULL,
    conf.level = .95,
    emmeans_args = list()) {
  .assert_package(
    "emmeans",
    fn = "broom.helpers::model_get_pairwise_contrasts()"
  )
  emmeans_args$object <- model
  emmeans_args$specs <- variable
  e <- do.call(emmeans::emmeans, emmeans_args)

  if (is.null(contrasts_adjust)) {
    e <- e |>
      graphics::pairs(reverse = pairwise_reverse)
  } else {
    e <- e |>
      graphics::pairs(reverse = pairwise_reverse, adjust = contrasts_adjust)
  }

  r <- e |>
    dplyr::as_tibble()
  if (!is.numeric(r[[2]])) { # if by
    r <- r |>
      tidyr::unite("term", 1:2, sep = " | ")
  }
  r <- r[, c(1:3, ncol(r) - 1, ncol(r))]
  colnames(r) <- c(
    "term", "estimate", "std.error",
    "statistic", "p.value"
  )

  ci <- stats::confint(e, level = conf.level) |>
    dplyr::as_tibble()
  if (!is.numeric(ci[[2]])) { # if by
    ci <- ci |>
      tidyr::unite("term", 1:2, sep = " | ")
  }
  ci <- ci[, c(1, ncol(ci) - 1, ncol(ci))]
  colnames(ci) <- c("term", "conf.low", "conf.high")
  r <- dplyr::left_join(r, ci, by = "term")
  r$variable <- variable
  r$contrasts <- ifelse(pairwise_reverse, "pairwise", "revpairwise")
  r$contrasts_type <- "pairwise"
  r |> dplyr::relocate(dplyr::all_of("variable"))
}

#' @export
model_get_pairwise_contrasts.zeroinfl <- function(model, ...) {
  cli::cli_abort(c(
    "Pairwise contrasts are not supported for multi-components model.",
    "Use directly {.fn emmeans::emmeans}."
  ))
}

#' @export
model_get_pairwise_contrasts.hurdle <- model_get_pairwise_contrasts.zeroinfl

#' @export
model_get_pairwise_contrasts.betareg <- model_get_pairwise_contrasts.zeroinfl
