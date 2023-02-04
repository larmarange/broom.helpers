#' Average Marginal Effects with `margins::margins()`
#'
#' `r lifecycle::badge("experimental")`
#' Use `margins::margins()` to estimate average marginal effects (AME) and
#' return a tibble tidied in a way that it could be used by `broom.helpers`
#' functions. See `margins::margins()` for a list of supported models.
#' @details
#' By default, `margins::margins()` estimate average marginal effects (AME): an
#' effect is computed for each observed value in the original dataset before
#' being averaged.
#'
#' For more information, see `vignette("marginal_tidiers", "broom.helpers")`.
#' @note When applying `margins::margins()`, custom contrasts are ignored.
#' Treatment contrasts (`stats::contr.treatment()`) are applied to all
#' categorical variables. Interactions are also ignored.
#' @param x a model
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param ... additional parameters passed to `margins::margins()`
#' @family marginal_tieders
#' @seealso `margins::margins()`
#' @export
#' @examplesIf interactive()
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   tidyr::uncount(n) %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#' mod <- glm(
#'   Survived ~ Class + Age + Sex,
#'   data = df, family = binomial
#' )
#' tidy_margins(mod)
#' tidy_plus_plus(mod, tidy_fun = tidy_margins)
tidy_margins <- function(x, conf.int = TRUE, conf.level = 0.95, ...) {
  .assert_package("margins")

  dots <- rlang::dots_list(...)
  if (isTRUE(dots$exponentiate))
    cli::cli_abort("{.arg exponentiate = TRUE} is not relevant for {.fun broom.helpers::tidy_margins}.") # nolint

  res <- broom::tidy(
    margins::margins(x, ...),
    conf.int = conf.int,
    conf.level = conf.level
  )
  attr(res, "coefficients_type") <- "marginal_effects_average"
  attr(res, "force_contr.treatment") <- TRUE
  res
}

#' Marginal Predictions at the mean with `effects::allEffects()`
#'
#' `r lifecycle::badge("experimental")`
#' Use `effects::allEffects()` to estimate marginal predictions and
#' return a tibble tidied in a way that it could be used by `broom.helpers`
#' functions.
#' See `vignette("functions-supported-by-effects", package = "effects")` for
#' a list of supported models.
#' @details
#' By default, `effects::allEffects()` estimate marginal predictions at the mean
#' at the observed means for continuous variables and weighting modalities
#' of categorical variables according to their observed distribution in the
#' original dataset. Marginal predictions are therefore computed at
#' a sort of averaged situation / typical values for the other variables fixed
#' in the model.
#'
#' For more information, see `vignette("marginal_tidiers", "broom.helpers")`.
#' @note
#' If the model contains interactions, `effects::allEffects()` will return
#' marginal predictions for the different levels of the interactions.
#' @param x a model
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param ... additional parameters passed to `effects::allEffects()`
#' @family marginal_tieders
#' @seealso `effects::allEffects()`
#' @export
#' @examplesIf interactive()
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   tidyr::uncount(n) %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#' mod <- glm(
#'   Survived ~ Class + Age + Sex,
#'   data = df, family = binomial
#' )
#' tidy_all_effects(mod)
#' tidy_plus_plus(mod, tidy_fun = tidy_all_effects)
tidy_all_effects <- function(x, conf.int = TRUE, conf.level = .95, ...) {
  .assert_package("effects")

  dots <- rlang::dots_list(...)
  if (isTRUE(dots$exponentiate))
    cli::cli_abort("{.arg exponentiate = TRUE} is not relevant for {.fun broom.helpers::tidy_all_effects}.") # nolint

  .clean <- function(x) {
    # merge first columns if interaction
    x <- tidyr::unite(x, "term", 1:(ncol(x) - 4), sep = ":")
    names(x) <- c("term", "estimate", "std.error", "conf.low", "conf.high")
    x$term <- as.character(x$term)
    rownames(x) <- NULL
    x
  }
  res <- x %>%
    effects::allEffects(se = conf.int, level = conf.level, ...) %>%
    as.data.frame() %>%
    purrr::map(.clean) %>%
    dplyr::bind_rows(.id = "variable") %>%
    dplyr::relocate("variable", "term")
  attr(res, "coefficients_type") <- "marginal_predictions_at_mean"
  attr(res, "skip_add_reference_rows") <- TRUE
  attr(res, "find_missing_interaction_terms") <- TRUE
  res
}

#' Marginal Predictions with `ggeffects::ggpredict()`
#'
#' `r lifecycle::badge("experimental")`
#' Use `ggeffects::ggpredict()` to estimate marginal predictions
#' and return a tibble tidied in a way that it could be used by `broom.helpers`
#' functions.
#' See <https://strengejacke.github.io/ggeffects/> for a list of supported
#' models.
#' @details
#' By default, `ggeffects::ggpredict()` estimate marginal predictions at the
#' observed mean of continuous variables and at the first modality of categorical
#' variables (regardless of the type of contrasts used in the model).
#'
#' For more information, see `vignette("marginal_tidiers", "broom.helpers")`.
#' @note
#' By default, `ggeffects::ggpredict()` estimates marginal predictions for each
#' individual variable, regardless of eventual interactions.
#' @param x a model
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param ... additional parameters passed to `ggeffects::ggpredict()`
#' @family marginal_tieders
#' @seealso `ggeffects::ggpredict()`
#' @export
#' @examplesIf interactive()
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   tidyr::uncount(n) %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#' mod <- glm(
#'   Survived ~ Class + Age + Sex,
#'   data = df, family = binomial
#' )
#' tidy_ggpredict(mod)
#' tidy_plus_plus(mod, tidy_fun = tidy_ggpredict)
tidy_ggpredict <- function(x, conf.int = TRUE, conf.level = .95, ...) {
  .assert_package("ggeffects")

  dots <- rlang::dots_list(...)
  if (isTRUE(dots$exponentiate))
    cli::cli_abort("{.arg exponentiate = TRUE} is not relevant for {.fun broom.helpers::tidy_ggpredict}.") # nolint

  if (isFALSE(conf.int)) conf.level <- NA
  res <- x %>%
    ggeffects::ggpredict(ci.lvl = conf.level, ...) %>%
    purrr::map(
      ~ .x %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(x = as.character(.data$x))
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::rename(
      variable = "group",
      term = "x",
      estimate = "predicted"
    ) %>%
    dplyr::relocate("variable", "term")
  attr(res, "coefficients_type") <- "marginal_predictions"
  attr(res, "skip_add_reference_rows") <- TRUE
  res
}

#' Marginal Slopes / Effects with `marginaleffects::avg_slopes()`
#'
#' `r lifecycle::badge("experimental")`
#' Use `marginaleffects::avg_slopes()` to estimate marginal slopes / effects and
#' return a tibble tidied in a way that it could be used by `broom.helpers`
#' functions. See `marginaleffects::avg_slopes()` for a list of supported
#' models.
#' @details
#' By default, `marginaleffects::avg_slopes()` estimate average marginal
#' effects (AME): an effect is computed for each observed value in the original
#' dataset before being averaged. Marginal Effects at the Mean (MEM) could be
#' computed by specifying `newdata = "mean"`. Other types of marginal effects
#' could be computed. Please refer to the documentation page of
#' `marginaleffects::avg_slopes()`.
#'
#' For more information, see `vignette("marginal_tidiers", "broom.helpers")`.
#' @param x a model
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param ... additional parameters passed to
#' `marginaleffects::avg_slopes()`
#' @family marginal_tieders
#' @seealso `marginaleffects::avg_slopes()`
#' @export
#' @examplesIf interactive()
#' # Average Marginal Effects (AME)
#'
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   tidyr::uncount(n) %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#' mod <- glm(
#'   Survived ~ Class + Age + Sex,
#'   data = df, family = binomial
#' )
#' tidy_avg_slopes(mod)
#' tidy_plus_plus(mod, tidy_fun = tidy_avg_slopes)
#'
#' mod2 <- lm(Petal.Length ~ poly(Petal.Width, 2) + Species, data = iris)
#' tidy_avg_slopes(mod2)
#'
#' # Marginal Effects at the Mean (MEM)
#' tidy_avg_slopes(mod, newdata = "mean")
#' tidy_plus_plus(mod, tidy_fun = tidy_avg_slopes, newdata = "mean")
tidy_avg_slopes <- function(x, conf.int = TRUE, conf.level = 0.95, ...) {
  .assert_package("marginaleffects")

  dots <- rlang::dots_list(...)
  if (isTRUE(dots$exponentiate))
    cli::cli_abort("{.arg exponentiate = TRUE} is not relevant for {.fun broom.helpers::tidy_avg_slopes}.") # nolint
  dots$exponentiate <- NULL
  dots$conf_level <- conf.level
  dots$model <- x

  res <- do.call(marginaleffects::avg_slopes, dots) %>%
    dplyr::rename(variable = "term")
  if ("contrast" %in% names(res))
    res <- res %>% dplyr::rename(term = "contrast")
  else
    res <- res %>% dplyr::mutate(term = .data$variable)

  res <- res %>%
    dplyr::relocate("variable", "term")

  attr(res, "coefficients_type") <- dplyr::case_when(
    is.null(dots$newdata) ~ "marginal_effects_average",
    isTRUE(dots$newdata == "mean") ~ "marginal_effects_at_mean",
    isTRUE(dots$newdata == "marginalmeans") ~ "marginal_effects_at_marginalmeans",
    TRUE ~ "marginal_effects"
  )
  attr(res, "skip_add_reference_rows") <- TRUE
  res
}

#' Marginal Contrasts with `marginaleffects::avg_comparisons()`
#'
#' `r lifecycle::badge("experimental")`
#' Use `marginaleffects::avg_comparisons()` to estimate marginal contrasts and
#' return a tibble tidied in a way that it could be used by `broom.helpers`
#' functions. See `marginaleffects::avg_comparisons()` for a list of supported
#' models.
#' @details
#' By default, `marginaleffects::avg_comparisons()` estimate average marginal
#' contrasts: a contrast is computed for each observed value in the original
#' dataset (counterfactual approach) before being averaged.
#' Marginal Contrasts at the Mean could be computed by specifying
#' `newdata = "mean"`. The `variables` argument can be used to select the
#' contrasts to be computed. Please refer to the documentation page of
#' `marginaleffects::avg_comparisons()`.
#'
#' See also `tidy_marginal_contrasts()` for taking into account interactions.
#' For more information, see `vignette("marginal_tidiers", "broom.helpers")`.
#' @param x a model
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param ... additional parameters passed to
#' `marginaleffects::avg_comparisons()`
#' @family marginal_tieders
#' @seealso `marginaleffects::avg_comparisons()`
#' @export
#' @examplesIf interactive()
#' # Average Marginal Contrasts
#'
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   tidyr::uncount(n) %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#' mod <- glm(
#'   Survived ~ Class + Age + Sex,
#'   data = df, family = binomial
#' )
#' tidy_avg_comparisons(mod)
#' tidy_plus_plus(mod, tidy_fun = tidy_avg_comparisons)
#'
#' mod2 <- lm(Petal.Length ~ poly(Petal.Width, 2) + Species, data = iris)
#' tidy_avg_comparisons(mod2)
#'
#' # Custumizing the type of contrasts
#' tidy_avg_comparisons(
#'   mod2,
#'   variables = list(Petal.Width = 2, Species = "pairwise")
#' )
#'
#' # Marginal Contrasts at the Mean
#' tidy_avg_comparisons(mod, newdata = "mean")
#' tidy_plus_plus(mod, tidy_fun = tidy_avg_comparisons, newdata = "mean")
tidy_avg_comparisons <- function(x, conf.int = TRUE, conf.level = 0.95, ...) {
  .assert_package("marginaleffects")

  dots <- rlang::dots_list(...)
  if (isTRUE(dots$exponentiate))
    cli::cli_abort("{.arg exponentiate = TRUE} is not relevant for {.fun broom.helpers::tidy_avg_comparisons}.") # nolint
  dots$exponentiate <- NULL
  dots$conf_level <- conf.level
  dots$model <- x

  res <- do.call(marginaleffects::avg_comparisons, dots) %>%
    dplyr::rename(variable = "term")
  if ("contrast" %in% names(res))
    res <- res %>% dplyr::rename(term = "contrast")
  else
    res <- res %>% dplyr::mutate(term = .data$variable)

  res <- res %>%
    dplyr::relocate("variable", "term")

  attr(res, "coefficients_type") <- dplyr::case_when(
    is.null(dots$newdata) ~ "marginal_contrasts_average",
    isTRUE(dots$newdata == "mean") ~ "marginal_contrasts_at_mean",
    isTRUE(dots$newdata == "marginalmeans") ~ "marginal_contrasts_at_marginalmeans",
    TRUE ~ "marginal_contrasts"
  )
  attr(res, "skip_add_reference_rows") <- TRUE
  res
}

#' Marginal Means with `marginaleffects::marginal_means()`
#'
#' `r lifecycle::badge("experimental")`
#' Use `marginaleffects::marginal_means()` to estimate marginal means and
#' return a tibble tidied in a way that it could be used by `broom.helpers`
#' functions. See `marginaleffects::marginal_means()()` for a list of supported
#' models.
#' @details
#' `marginaleffects::marginal_means()` estimate marginal means:
#' adjusted predictions, averaged across a grid of categorical predictors,
#' holding other numeric predictors at their means. Please refer to the
#' documentation page of `marginaleffects::marginal_means()`. Marginal means
#' are defined only for categorical variables.
#'
#' For more information, see `vignette("marginal_tidiers", "broom.helpers")`.
#' @param x a model
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param ... additional parameters passed to
#' `marginaleffects::marginal_means()`
#' @family marginal_tieders
#' @seealso `marginaleffects::marginal_means()`
#' @export
#' @examplesIf interactive()
#' # Average Marginal Means
#'
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   tidyr::uncount(n) %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#' mod <- glm(
#'   Survived ~ Class + Age + Sex,
#'   data = df, family = binomial
#' )
#' tidy_marginal_means(mod)
#' tidy_plus_plus(mod, tidy_fun = tidy_marginal_means)
#'
#' mod2 <- lm(Petal.Length ~ poly(Petal.Width, 2) + Species, data = iris)
#' tidy_marginal_means(mod2)
tidy_marginal_means <- function(x, conf.int = TRUE, conf.level = 0.95, ...) {
  .assert_package("marginaleffects")

  dots <- rlang::dots_list(...)
  if (isTRUE(dots$exponentiate))
    cli::cli_abort("{.arg exponentiate = TRUE} is not relevant for {.fun broom.helpers::tidy_marginal_means}.") # nolint
  dots$exponentiate <- NULL
  dots$conf_level <- conf.level
  dots$model <- x

  res <- do.call(marginaleffects::marginal_means, dots) %>%
    dplyr::rename(
      variable = "term",
      term = "value"
    ) %>%
    dplyr::mutate(term = as.character(.data$term))


  attr(res, "coefficients_type") <- "marginal_means"
  attr(res, "skip_add_reference_rows") <- TRUE
  res
}





#' Marginal Predictions with `marginaleffects::avg_predictions()`
#'
#' `r lifecycle::badge("experimental")`
#' Use `marginaleffects::avg_predictions()` to estimate marginal predictions for
#' each variable of a model and return a tibble tidied in a way that it could
#' be used by `broom.helpers` functions.
#' See `marginaleffects::avg_predictions()` for a list of supported models.
#' @details
#' Marginal predictions are obtained by calling, for each variable,
#' `marginaleffects::avg_predictions()` with the same variable being used for
#' the `variables` and the `by` argument.
#'
#' Considering a categorical variable named `cat`, `tidy_marginal_predictions()`
#' will call `avg_predictions(model, variables = list(cat = unique), by = "cat")`
#' to obtain average marginal predictions for this variable.
#'
#' Considering a continuous variable named `cont`, `tidy_marginal_predictions()`
#' will call `avg_predictions(model, variables = list(cont = "fivenum"), by = "cont")`
#' to obtain average marginal predictions for this variable at the minimum, the
#' first quartile, the median, the third quartile and the maximum of the observed
#' values of `cont`.
#'
#' By default, *average marginal predictions* are computed: predictions are made
#' using a counterfactual grid for each value of the variable of interest,
#' before averaging the results. *Marginal predictions at the mean* could be
#' obtained by indicating `newdata = "mean"`. Other assumptions are possible,
#' see the help file of `marginaleffects::avg_predictions()`.
#'
#' `tidy_marginal_predictions()` will compute marginal predictions for each
#' variable or combination of variables, before stacking the results in a unique
#' tibble. This is why `tidy_marginal_predictions()` has a `variables_list`
#' argument consisting of a list of specifications that will be passed
#' sequentially to the `variables` argument of `marginaleffects::avg_predictions()`.
#'
#' The helper function `variables_to_predict()` could be used to automatically
#' generate a suitable list to be used with `variables_list`. By default, all
#' unique values are retained for categorical variables and `"fivenum"` (i.e.
#' Tukey's five numbers, minimum, quartiles and maximum) for continuous variables.
#' When `interactions = FALSE`, `variables_to_predict()` will return a list of
#' all individual variables used in the model. If `interactions = FALSE`, it
#' will search for higher order combinations of variables (see
#' `model_list_higher_order_variables()`).
#'
#' `variables_list`'s default value, `"auto"`, calls
#' `variables_to_predict(interactions = TRUE)` while `"no_interaction"` is a
#' shortcut for `variables_to_predict(interactions = FALSE)`.
#'
#' You can also provide custom specifications (see examples).
#'
#' `plot_marginal_predictions()` works in a similar way and returns a list of
#' plots that could be combined with `patchwork::wrap_plots()` (see examples).
#'
#' For more information, see `vignette("marginal_tidiers", "broom.helpers")`.
#' @param x a model
#' @param variables_list a list whose elements will be sequentially passed to
#' `variables` in `marginaleffects::avg_predictions()` (see details below);
#' alternatively, it could also be the string `"auto"` (default) or
#' `"no_interaction"`
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param ... additional parameters passed to
#' `marginaleffects::avg_predictions()`
#' @family marginal_tieders
#' @seealso `marginaleffects::avg_predictions()`
#' @export
#' @examplesIf interactive()
#' # Average Marginal Predictions
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   tidyr::uncount(n) %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#' mod <- glm(
#'   Survived ~ Class + Age + Sex,
#'   data = df, family = binomial
#' )
#' tidy_marginal_predictions(mod)
#' tidy_plus_plus(mod, tidy_fun = tidy_marginal_predictions)
#' if (require("patchwork")) {
#'   plot_marginal_predictions(mod) %>% patchwork::wrap_plots()
#'   plot_marginal_predictions(mod) %>%
#'     patchwork::wrap_plots() &
#'     ggplot2::scale_y_continuous(limits = c(0, 1), label = scales::percent)
#' }
#'
#' mod2 <- lm(Petal.Length ~ poly(Petal.Width, 2) + Species, data = iris)
#' tidy_marginal_predictions(mod2)
#' if (require("patchwork")) {
#'   plot_marginal_predictions(mod2) %>% patchwork::wrap_plots()
#' }
#' tidy_marginal_predictions(
#'   mod2,
#'   variables_list = variables_to_predict(mod2, continuous = "threenum")
#' )
#' tidy_marginal_predictions(
#'   mod2,
#'   variables_list = list(
#'     list(Petal.Width = c(0, 1, 2, 3)),
#'     list(Species = unique)
#'   )
#' )
#' tidy_marginal_predictions(
#'   mod2,
#'   variables_list = list(list(Species = unique, Petal.Width = 1:3))
#' )
#'
#' # Model with interactions
#' mod3 <- glm(
#'   Survived ~ Sex * Age + Class,
#'   data = df, family = binomial
#' )
#' tidy_marginal_predictions(mod3)
#' tidy_marginal_predictions(mod3, "no_interaction")
#' if (require("patchwork")) {
#'   plot_marginal_predictions(mod3) %>%
#'     patchwork::wrap_plots()
#'   plot_marginal_predictions(mod3, "no_interaction") %>%
#'     patchwork::wrap_plots()
#' }
#' tidy_marginal_predictions(
#'   mod3,
#'   variables_list = list(
#'     list(Class = unique, Sex = "Female"),
#'     list(Age = unique)
#'   )
#' )
#'
#' # Marginal Predictions at the Mean
#' tidy_marginal_predictions(mod, newdata = "mean")
#' if (require("patchwork")) {
#'   plot_marginal_predictions(mod, newdata = "mean") %>%
#'     patchwork::wrap_plots()
#' }
tidy_marginal_predictions <- function(x, variables_list = "auto",
                                     conf.int = TRUE, conf.level = 0.95, ...) {
  .assert_package("marginaleffects")

  dots <- rlang::dots_list(...)
  if (isTRUE(dots$exponentiate))
    cli::cli_abort("{.arg exponentiate = TRUE} is not relevant for {.fun broom.helpers::tidy_marginal_predictions}.") # nolint
  dots$exponentiate <- NULL
  dots$conf_level <- conf.level
  dots$model <- x

  if (is.character(variables_list) && variables_list == "auto")
    variables_list <- variables_to_predict(x, interactions = TRUE)
  if (is.character(variables_list) && variables_list == "no_interaction")
    variables_list <- variables_to_predict(x, interactions = FALSE)
  if (!is.list(variables_list))
    cli::cli_abort("{.arg variables_list} should be a list or \"auto\" or \"no_interaction\".")

  res <- purrr::map_df(variables_list, .tidy_one_marginal_prediction, dots)

  attr(res, "coefficients_type") <- dplyr::case_when(
    is.null(dots$newdata) ~ "marginal_predictions_average",
    isTRUE(dots$newdata == "mean") ~ "marginal_predictions_at_mean",
    isTRUE(dots$newdata == "marginalmeans") ~ "marginal_predictions_at_marginalmeans",
    TRUE ~ "marginal_predictions"
  )
  attr(res, "skip_add_reference_rows") <- TRUE
  res
}

.tidy_one_marginal_prediction <- function(variables, dots) {
  dots$variables <- variables
  dots$by <- names(variables)
  do.call(marginaleffects::avg_predictions, dots) %>%
    dplyr::mutate(variable = paste(names(variables), collapse = ":")) %>%
    tidyr::unite(col = "term", sep = " * ", dplyr::all_of(names(variables))) %>%
    dplyr::relocate("variable", "term")
}

#' @export
#' @param model a model
#' @param interactions should combinations of variables corresponding to
#' interactions be returned?
#' @param categorical default value for categorical variables
#' @param continuous default value for continuous variables
#' @rdname tidy_marginal_predictions
variables_to_predict <- function(model, interactions = TRUE,
                                 categorical = unique, continuous = "fivenum") {
  variables <- model %>%
    model_list_variables(add_var_type = TRUE)

  if (interactions) {
    keep <- model_list_higher_order_variables(model)
  } else {
    keep <- variables[variables$var_type != "interaction", ]$variable
  }

  response_variable <- model %>% model_get_response_variable()
  if (!is.null(response_variable))
    keep <- keep[keep != response_variable]

  ret <- list(
    categorical = categorical,
    dichotomous = categorical,
    continuous = continuous
  )
  variables <- variables %>%
    tibble::column_to_rownames("variable")

  one_element <- function(v) {
    v <- strsplit(v, ":") %>% unlist()
    one <- variables[v, "var_type"]
    one <- ret[one]
    names(one) <- v
    one
  }
  lapply(keep, one_element)
}

#' @export
#' @rdname tidy_marginal_predictions
plot_marginal_predictions <- function(x, variables_list = "auto",
                                     conf.level = 0.95, ...) {
  .assert_package("marginaleffects")
  .assert_package("ggplot2")

  dots <- rlang::dots_list(...)
  dots$conf_level <- conf.level
  dots$model <- x

  if (is.character(variables_list) && variables_list == "auto")
    variables_list <- variables_to_predict(x, interactions = TRUE) %>%
      purrr::map(rev)
  if (is.character(variables_list) && variables_list == "no_interaction")
    variables_list <- variables_to_predict(x, interactions = FALSE) %>%
      purrr::map(rev)
  if (!is.list(variables_list))
    cli::cli_abort("{.arg variables_list} should be a list or \"auto\" or \"no_interaction\".")

  purrr::map(variables_list, .plot_one_marginal_prediction, dots)
}

.plot_one_marginal_prediction <- function(variables, dots) {
  if (length(variables) >= 4)
    cli::cli_abort(paste(
      "Combination of 4 or more variables. {.fun plot_marginal_predictions} can",
      "manage only combinations of 3 variables or less."
    ))

  list_variables <- dots$model %>% model_list_variables(add_var_type = TRUE)
  x_variable <- names(variables[1])
  x_type <- list_variables %>%
    dplyr::filter(.data$variable == x_variable) %>%
    dplyr::pull("var_type")
  if (x_type == "dichotomous") x_type <- "categorical"
  x_label <- list_variables %>%
    dplyr::filter(.data$variable == x_variable) %>%
    dplyr::pull("var_label")

  if (is.character(variables[[1]]) && variables[[1]] == "fivenum")
    variables[[1]] <- broom.helpers::seq_range
  dots$variables <- variables
  dots$by <- names(variables)
  d <- do.call(marginaleffects::avg_predictions, dots)

  mapping <- ggplot2::aes(
    x = .data[[x_variable]],
    y = .data[["estimate"]],
    ymin = .data[["conf.low"]],
    ymax = .data[["conf.high"]]
  )
  if (x_type == "continuous")
    mapping$group <- ggplot2::aes(group = 1L)$group

  if (length(variables) >= 2) {
    colour_variable <- names(variables[2])
    d[[colour_variable]] <- factor(d[[colour_variable]])
    colour_label <- list_variables %>%
      dplyr::filter(.data$variable == colour_variable) %>%
      dplyr::pull("var_label")
    mapping$colour <- ggplot2::aes(colour = .data[[colour_variable]])$colour
    if (x_type == "continuous") {
      mapping$fill <- ggplot2::aes(fill = .data[[colour_variable]])$fill
      mapping$group <- ggplot2::aes(group = .data[[colour_variable]])$group
    }
  }

  if (x_type == "continuous") {
    p <- ggplot2::ggplot(d, mapping = mapping) +
      ggplot2::geom_ribbon(
        mapping = ggplot2::aes(colour = NULL),
        alpha = 0.1,
        show.legend = FALSE
      ) +
      ggplot2::geom_line()
  } else {
      p <- ggplot2::ggplot(d, mapping = mapping) +
        ggplot2::geom_pointrange(position = ggplot2::position_dodge(.5))
  }

  if (length(variables) >= 2)
    p <- p +
      ggplot2::labs(colour = colour_label, fill = colour_label)

  if (length(variables) >= 3) {
    facet_variable <- names(variables[3])
    p <- p +
      ggplot2::facet_wrap(facet_variable)
  }

  p +
    ggplot2::xlab(x_label) +
    ggplot2::ylab(NULL) +
    ggplot2::theme_light() +
    ggplot2::theme(legend.position = "bottom")
}

#' Marginal Contrasts with `marginaleffects::avg_comparisons()`
#'
#' `r lifecycle::badge("experimental")`
#' Use `marginaleffects::avg_comparisons()` to estimate marginal contrasts for
#' each variable of a model and return a tibble tidied in a way that it could
#' be used by `broom.helpers` functions.
#' See `marginaleffects::avg_comparisons()` for a list of supported models.
#' @details
#' Marginal contrasts are obtained by calling, for each variable or combination
#' of variables, `marginaleffects::avg_comparisons()` with the option `cross = TRUE`.
#'
#' Considering a categorical variable named `cat`, `tidy_marginal_contrasts()`
#' will call `avg_comparisons(model, variables = list(cat = "reference"))`
#' to obtain average marginal contrasts for this variable.
#'
#' Considering a continuous variable named `cont`, `tidy_marginalcontrasts()`
#' will call `avg_comparisons(model, variables = list(cont = 1))`
#' to obtain average marginal contrasts for an increase of one unit.
#'
#' By default, *average marginal contrasts* are computed: contrasts are computed
#' using a counterfactual grid for each value of the variable of interest,
#' before averaging the results. *Marginal contrasts at the mean* could be
#' obtained by indicating `newdata = "mean"`. Other assumptions are possible,
#' see the help file of `marginaleffects::avg_comparisons()`.
#'
#' `tidy_marginal_contrasts()` will compute marginal contrasts for each
#' variable or combination of variables, before stacking the results in a unique
#' tibble. This is why `tidy_marginal_contrasts()` has a `variables_list`
#' argument consisting of a list of specifications that will be passed
#' sequentially to the `variables` argument of `marginaleffects::avg_comparisons()`.
#'
#' The helper function `variables_to_predict()` could be used to automatically
#' generate a suitable list to be used with `variables_list`.
#'
#' `variables_list`'s default value, `"auto"`, calls
#' `variables_to_predict(interactions = TRUE)` while `"no_interaction"` is a
#' shortcut for `variables_to_predict(interactions = FALSE)`. `"reference"` is
#' used for categorical variables and `1` for continuous variables.
#'
#' You can also provide custom specifications (see examples).
#'
#' For more information, see `vignette("marginal_tidiers", "broom.helpers")`.
#' @param x a model
#' @param variables_list a list whose elements will be sequentially passed to
#' `variables` in `marginaleffects::avg_comparisons()` (see details below);
#' alternatively, it could also be the string `"auto"` (default) or
#' `"no_interaction"`
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param ... additional parameters passed to
#' `marginaleffects::avg_comparisons()`
#' @family marginal_tieders
#' @seealso `marginaleffects::avg_comparisons()`, `tidy_avg_comparisons()`
#' @export
#' @examplesIf interactive()
#' # Average Marginal Contrasts
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   tidyr::uncount(n) %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#' mod <- glm(
#'   Survived ~ Class + Age + Sex,
#'   data = df, family = binomial
#' )
#' tidy_marginal_contrasts(mod)
#' tidy_plus_plus(mod, tidy_fun = tidy_marginal_contrasts)
#'
#' mod2 <- lm(Petal.Length ~ poly(Petal.Width, 2) + Species, data = iris)
#' tidy_marginal_contrasts(mod2)
#' tidy_marginal_contrasts(
#'   mod2,
#'   variables_list = variables_to_predict(
#'     mod2,
#'     continuous = 3,
#'     categorical = "pairwise"
#'   )
#' )
#'
#' # Model with interactions
#' mod3 <- glm(
#'   Survived ~ Sex * Age + Class,
#'   data = df, family = binomial
#' )
#' tidy_marginal_contrasts(mod3)
#' tidy_marginal_contrasts(mod3, "no_interaction")
#'
#' # Marginal Contrasts at the Mean
#' tidy_marginal_contrasts(mod, newdata = "mean")
tidy_marginal_contrasts <- function(x, variables_list = "auto",
                                     conf.int = TRUE, conf.level = 0.95, ...) {
  .assert_package("marginaleffects")

  dots <- rlang::dots_list(...)
  if (isTRUE(dots$exponentiate))
    cli::cli_abort("{.arg exponentiate = TRUE} is not relevant for {.fun broom.helpers::tidy_marginal_contrasts}.") # nolint
  dots$exponentiate <- NULL
  dots$conf_level <- conf.level
  dots$model <- x

  if (is.character(variables_list) && variables_list == "auto")
    variables_list <- variables_to_predict(
      x,
      interactions = TRUE,
      categorical = "reference",
      continuous = 1
    )
  if (is.character(variables_list) && variables_list == "no_interaction")
    variables_list <- variables_to_predict(
      x,
      interactions = FALSE,
      categorical = "reference",
      continuous = 1
    )
  if (!is.list(variables_list))
    cli::cli_abort("{.arg variables_list} should be a list or \"auto\" or \"no_interaction\".")

  res <- purrr::map_df(variables_list, .tidy_one_marginal_contrast, dots)

  attr(res, "coefficients_type") <- dplyr::case_when(
    is.null(dots$newdata) ~ "marginal_contrasts_average",
    isTRUE(dots$newdata == "mean") ~ "marginal_contrasts_at_mean",
    isTRUE(dots$newdata == "marginalmeans") ~ "marginal_contrasts_at_marginalmeans",
    TRUE ~ "marginal_contrasts"
  )
  attr(res, "skip_add_reference_rows") <- TRUE
  res
}

.tidy_one_marginal_contrast <- function(variables, dots) {
  dots$variables <- variables
  do.call(marginaleffects::avg_comparisons, dots) %>%
    dplyr::select(-dplyr::any_of("term")) %>%
    dplyr::mutate(variable = paste(names(variables), collapse = ":")) %>%
    tidyr::unite(col = "term", sep = " * ", dplyr::starts_with("contrast")) %>%
    dplyr::relocate("variable", "term")
}
