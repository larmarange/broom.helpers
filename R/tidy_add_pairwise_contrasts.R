#' Add pairwise contrasts for categorical variables
#'
#'
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
#' @param conf.level confidence level, if `NULL` use the value indicated
#' previously in [tidy_and_attach()]
#' @param model the corresponding model, if not attached to `x`
#' @inheritParams tidy_plus_plus
#' @export
#' @family tidy_helpers
tidy_add_pairwise_contrasts <- function(
  x,
  variables = all_categorical(),
  keep_model_terms = FALSE,
  pairwise_reverse = TRUE,
  conf.level = NULL,
  emmeans_args = list(),
  model = tidy_get_model(x),
  quiet = FALSE
) {
  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  if (!"contrasts" %in% names(x)) {
    x <- x %>% tidy_add_contrasts(model = model)
  }

  .attributes <- .save_attributes(x)

  if (is.null(conf.level))
    conf.level <- .attributes$conf.level

  if (is.null(conf.level))
    stop("Please specify conf.level")

  # obtain character vector of selected variables
  variables <- .select_to_varnames(
    {{ variables }},
    var_info = x,
    arg_name = "variables"
  )

  if (isTRUE(.attributes$exponentiate) && is.null(emmeans_args$type))
    emmeans_args$type <- "response"

  pc <- model_get_pairwise_contrasts(
    model = model,
    variables = variables,
    pairwise_reverse = pairwise_reverse,
    conf.level = conf.level,
    emmeans_args = emmeans_args
  )

  x <- dplyr::bind_rows(x, pc) %>%
    dplyr::mutate(variableF = forcats::fct_inorder(.data$variable)) %>%
    dplyr::arrange(.data$variableF) %>%
    tidyr::fill(all_of(c("var_class", "var_type", "var_nlevels"))) %>%
    dplyr::select(-all_of("variableF"))

  if (!keep_model_terms)
    x <- x %>%
      dplyr::filter(
        !(.data$variable %in% variables) | .data$contrasts_type == "pairwise"
      )

  x %>%
    tidy_attach_model(model = model, .attributes = .attributes)
}
