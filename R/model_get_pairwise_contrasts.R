#' Get pairwise comparison of the levels of a categorical variable
#'
#' It is computed with [emmeans::emmeans()].
#'
#' @param model a model object
#' @family model_helpers
#' @export
model_get_pairwise_contrasts <- function(
  model,
  variables,
  keep_model_terms = TRUE,
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
    .get_pairwise_contrats_one_var,
    model = model,
    pairwise_reverse = pairwise_reverse,
    conf.level = conf.level,
    emmeans_args = emmeans_args
  )
}

.get_pairwise_contrats_one_var <- function(
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
    pairs(reverse = pairwise_reverse)
  r <- e %>%
    dplyr::as_tibble()
  r <- r[, c(1:3, ncol(r) - 1, ncol(r))]
  colnames(r) <- c(
    "term", "estimate", "std.error",
    "statistic", "p.value"
  )
  ci <- confint(e, level = conf.level) %>%
    dplyr::as_tibble()
  ci <- ci[, c(1, ncol(ci) - 1, ncol(ci))]
  colnames(ci) <- c("term", "conf.low", "conf.high")
  r <- dplyr::left_join(r, ci, by = "term")
  r$variable <- variable
  r$contrasts <- ifelse(pairwise_reverse, "pairwise", "revpairwise")
  r$contrasts_type <- "pairwise"
  r %>% dplyr::relocate(dplyr::all_of("variable"))
}
