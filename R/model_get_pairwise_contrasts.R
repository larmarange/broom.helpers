#' Get pairwise comparison of the levels of a categorical variable
#'
#' It is computed with [emmeans::emmeans()].
#'
#' @param model a model object
#' @param variables variables to add pairwise contrasts
#' (accepts [tidyselect][dplyr::select] notation)
#' @param pairwise_reverse determines whether to use `"pairwise"` (if `TRUE`)
#' or `"revpairwise"` (if `FALSE`), see [emmeans::contrast()]
#' @param conf.level level of confidence for confidence intervals
#' @param emmeans_args list of additional parameter to pass to
#' [emmeans::emmeans()] when computing pairwise contrasts
#' @family model_helpers
#' @export
model_get_pairwise_contrasts <- function(
  model,
  variables,
  pairwise_reverse = TRUE,
  conf.level = .95,
  emmeans_args = list()
) {
  UseMethod("model_get_pairwise_contrasts")
}

#' @export
model_get_pairwise_contrasts.default <- function(
  model,
  variables,
  pairwise_reverse = TRUE,
  conf.level = .95,
  emmeans_args = list()
) {
  purrr::map_df(
    variables,
    .get_pairwise_contrasts_one_var,
    model = model,
    pairwise_reverse = pairwise_reverse,
    conf.level = conf.level,
    emmeans_args = emmeans_args
  )
}

.get_pairwise_contrasts_one_var <- function(
  model,
  variable,
  pairwise_reverse = TRUE,
  conf.level = .95,
  emmeans_args = list()
){
  .assert_package(
    "emmeans",
    fn = "broom.helpers::model_get_pairwise_contrasts()"
  )
  emmeans_args$object <- model
  emmeans_args$specs <- variable
  e <- do.call(emmeans::emmeans, emmeans_args) %>%
    graphics::pairs(reverse = pairwise_reverse)
  r <- e %>%
    dplyr::as_tibble()
  r <- r[, c(1:3, ncol(r) - 1, ncol(r))]
  colnames(r) <- c(
    "term", "estimate", "std.error",
    "statistic", "p.value"
  )
  ci <- stats::confint(e, level = conf.level) %>%
    dplyr::as_tibble()
  ci <- ci[, c(1, ncol(ci) - 1, ncol(ci))]
  colnames(ci) <- c("term", "conf.low", "conf.high")
  r <- dplyr::left_join(r, ci, by = "term")
  r$variable <- variable
  r$contrasts <- ifelse(pairwise_reverse, "pairwise", "revpairwise")
  r$contrasts_type <- "pairwise"
  r %>% dplyr::relocate(dplyr::all_of("variable"))
}
