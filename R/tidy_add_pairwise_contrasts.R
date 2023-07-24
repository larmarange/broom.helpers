#' Add pairwise contrasts for categorical variables
#'
#' `r lifecycle::badge("experimental")`
#' Computes pairwise contrasts with [emmeans::emmeans()] and add them to the
#' results tibble. Works only with models supported by `emmeans`, see
#' `vignette("models", package = "emmeans")`.
#'
#' @note
#' If the `contrasts` column is not yet available in `x`,
#' [tidy_add_contrasts()] will be automatically applied.
#'
#' @param x a tidy tibble
#' @param variables a vector indicating the name of variables
#' for those pairwise contrasts should be added.
#' Accepts [tidyselect][dplyr::select] syntax. Default is  [all_categorical()]
#' @param keep_model_terms keep terms from the model?
#' @param pairwise_reverse determines whether to use `"pairwise"` (if `TRUE`)
#' or `"revpairwise"` (if `FALSE`), see [emmeans::contrast()]
#' @param contrasts_adjust optional adjustment method when computing contrasts,
#' see [emmeans::contrast()] (if `NULL`, use `emmeans` default)
#' @param conf.level confidence level, by default use the value indicated
#' previously in [tidy_and_attach()]
#' @param model the corresponding model, if not attached to `x`
#' @inheritParams tidy_plus_plus
#' @export
#' @family tidy_helpers
#' @examplesIf interactive()
#' if (.assert_package("emmeans", boolean = TRUE)) {
#'   mod1 <- lm(Sepal.Length ~ Species, data = iris)
#'   mod1 %>%
#'     tidy_and_attach() %>%
#'     tidy_add_pairwise_contrasts()
#'
#'   mod1 %>%
#'     tidy_and_attach() %>%
#'     tidy_add_pairwise_contrasts(pairwise_reverse = FALSE)
#'
#'   mod1 %>%
#'     tidy_and_attach() %>%
#'     tidy_add_pairwise_contrasts(keep_model_terms = TRUE)
#'
#'   mod1 %>%
#'     tidy_and_attach() %>%
#'     tidy_add_pairwise_contrasts(contrasts_adjust = "none")
#'
#'   if (.assert_package("gtsummary", boolean = TRUE)) {
#'     mod2 <- glm(
#'       response ~ age + trt + grade,
#'       data = gtsummary::trial,
#'       family = binomial
#'     )
#'     mod2 %>%
#'       tidy_and_attach(exponentiate = TRUE) %>%
#'       tidy_add_pairwise_contrasts()
#'   }
#' }
tidy_add_pairwise_contrasts <- function(
    x,
    variables = all_categorical(),
    keep_model_terms = FALSE,
    pairwise_reverse = TRUE,
    contrasts_adjust = NULL,
    conf.level = attr(x, "conf.level"),
    emmeans_args = list(),
    model = tidy_get_model(x),
    quiet = FALSE) {
  if (is.null(model)) {
    cli::cli_abort(c(
      "{.arg model} is not provided.",
      "You need to pass it or to use {.fn tidy_and_attach}."
    ))
  }

  if (is.null(conf.level) || !is.numeric(conf.level)) {
    cli::cli_abort("{.arg conf.level} is not provided. You need to pass it explicitely.")
  }

  if (!"contrasts" %in% names(x)) {
    x <- x %>% tidy_add_contrasts(model = model)
  }

  .attributes <- .save_attributes(x)

  if (isTRUE(stringr::str_starts(.attributes$coefficients_type, "marginal"))) {
    cli::cli_abort("Pairwise contrasts are not compatible with marginal effects / contrasts / means / predictions.")
  } # nolint

  if (is.null(conf.level)) {
    cli::cli_abort("Please specify {.arg conf.level}")
  }

  # obtain character vector of selected variables
  variables <- .select_to_varnames(
    {{ variables }},
    var_info = x,
    arg_name = "variables"
  )

  if (isTRUE(.attributes$exponentiate) && is.null(emmeans_args$type)) {
    emmeans_args$type <- "response"
  }

  pc <- model_get_pairwise_contrasts(
    model = model,
    variables = variables,
    pairwise_reverse = pairwise_reverse,
    contrasts_adjust = contrasts_adjust,
    conf.level = conf.level,
    emmeans_args = emmeans_args
  )

  x <- dplyr::bind_rows(x, pc) %>%
    dplyr::mutate(variableF = forcats::fct_inorder(.data$variable)) %>%
    dplyr::arrange(.data$variableF) %>%
    tidyr::fill(all_of(c("var_class", "var_type", "var_nlevels"))) %>%
    dplyr::select(-all_of("variableF"))

  if (!keep_model_terms) {
    x <- x %>%
      dplyr::filter(
        !(.data$variable %in% variables) | .data$contrasts_type == "pairwise"
      )
  }

  x %>%
    tidy_attach_model(model = model, .attributes = .attributes)
}
