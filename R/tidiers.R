#' Tidy a model with parameters package
#'
#' Use `parameters::model_parameters()` to tidy a model and apply
#' `parameters::standardize_names(style = "broom")` to the output
#' @param x a model
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param ... additional parameters passed to `parameters::model_parameters()`
#' @examplesIf interactive()
#' if (.assert_package("parameters", boolean = TRUE)) {
#' lm(Sepal.Length ~ Sepal.Width + Species, data = iris) %>%
#'   tidy_parameters()
#' }
#' @export
#' @family custom_tieders
tidy_parameters <- function(x, conf.int = TRUE, conf.level = .95, ...) {
  .assert_package("parameters", fn = "broom.helpers::tidy_parameters()")

  if (!conf.int) conf.level <- NULL

  res <- x %>%
    parameters::model_parameters(ci = conf.level, ...) %>%
    parameters::standardize_names(style = "broom")

  if (inherits(x, "multinom")) {
    if ("response" %in% colnames(res)) {
      res <- res %>%
        dplyr::rename(y.level = "response")
    } else {
      # binary
      res$y.level <- x$lev %>% utils::tail(n = 1)
    }
  }

  res
}

#' Tidy a model with broom or parameters
#'
#' Try to tidy a model with `broom::tidy()`. If it fails, will try to tidy the
#' model using `parameters::model_parameters()` through `tidy_parameters()`.
#' @param x a model
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param ... additional parameters passed to `broom::tidy()` or
#' `parameters::model_parameters()`
#' @export
#' @family custom_tieders
tidy_with_broom_or_parameters <- function(x, conf.int = TRUE, conf.level = .95, ...) {
  # load broom.mixed if available
  if (any(c("glmerMod", "lmerMod", "glmmTMB", "glmmadmb", "stanreg", "brmsfit") %in% class(x))) {
    .assert_package("broom.mixed", fn = "broom.helpers::tidy_with_broom_or_parameters()")
  }

  if (inherits(x, "LORgee")) {
    cli::cli_alert_info("{.pkg multgee} model detected.")
    cli::cli_alert_success("{.code tidy_multgee()} used instead.")
    cli::cli_alert_info(
      "Add {.code tidy_fun = broom.helpers::tidy_multgee} to quiet these messages."
    )
    return(tidy_multgee(x, conf.int = conf.int, conf.level = conf.level, ...))
  }

  tidy_args <- list(...)
  tidy_args$x <- x
  tidy_args$conf.int <- conf.int
  if (conf.int) tidy_args$conf.level <- conf.level

  res <- tryCatch(
    do.call(tidy_broom, tidy_args),
    error = function(e) {
      NULL
    }
  )

  # trying without exponentiate
  if (is.null(res)) {
    tidy_args2 <- tidy_args
    tidy_args2$exponentiate <- NULL
    res <- tryCatch(
      do.call(tidy_broom, tidy_args2),
      error = function(e) {
        NULL
      }
    )
    if (!is.null(res) && !is.null(tidy_args$exponentiate) && tidy_args$exponentiate) {
      # changing to FALSE is managed by tidy_and_attch()
      cli::cli_abort("'exponentiate = TRUE' is not valid for this type of model.")
    }
  }

  if (is.null(res)) {
    cli::cli_alert_warning("{.code broom::tidy()} failed to tidy the model.")
    res <- tryCatch(
      do.call(tidy_parameters, tidy_args),
      error = function(e) {
        cli::cli_alert_warning("{.code tidy_parameters()} also failed.")
        cli::cli_alert_danger(e)
        NULL
      }
    )
    if (is.null(res)) {
      stop("Unable to tidy `x`.")
    } else {
      # success of parameters
      cli::cli_alert_success("{.code tidy_parameters()} used instead.")
      cli::cli_alert_info(
        "Add {.code tidy_fun = broom.helpers::tidy_parameters} to quiet these messages."
      )
    }
  }
  res
}

#' Tidy with `broom::tidy()` and checks that all arguments are used
#'
#' @param x a model to tidy
#' @param ... additional parameters passed to `broom::tidy()`
#' @family custom_tieders
#' @export
tidy_broom <- function(x, ...) {
  rlang::check_dots_used()
  broom::tidy(x, ...)
}

#' Tidy a `multgee` model
#'
#' `r lifecycle::badge("experimental")`
#' A tidier for models generated with `multgee::nomLORgee()` or `multgee::ordLORgee()`.
#' Term names will be updated to be consistent with generic models. The original
#' term names are preserved in an `"original_term"` column.
#' @param x a `multgee::nomLORgee()` or a `multgee::ordLORgee()` model
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param ... additional parameters passed to `parameters::model_parameters()`
#' @export
#' @family custom_tieders
#' @examplesIf interactive()
#' if (.assert_package("multgee", boolean = TRUE)) {
#'   library(multgee)
#'
#'   mod <- multgee::nomLORgee(
#'     y ~ factor(time) * sec,
#'     data = multgee::housing,
#'     id = id,
#'     repeated = time,
#'   )
#'   mod %>% tidy_multgee()
#'
#'   mod2 <- ordLORgee(
#'     formula = y ~ factor(time) + factor(trt) + factor(baseline),
#'     data = multgee::arthritis,
#'     id = id,
#'     repeated = time,
#'     LORstr = "uniform"
#'   )
#'   mod2 %>% tidy_multgee()
#' }
tidy_multgee <- function(x, conf.int = TRUE, conf.level = .95, ...) {
  if (!inherits(x, "LORgee"))
    cli::cli_abort(paste(
      "Only {.fn multgee::nomLORgee} and {.fn multgee::ordLORgee} models",
      "are supported."
    ))

  res <- tidy_parameters(x, conf.int = conf.int, conf.level = conf.level, ...)
  res$original_term <- res$term

  # multinomial model
  if (stringr::str_detect(x$title, "NOMINAL")) {
    mf <- x %>% model_get_model_frame()
    if (!is.factor(mf[[1]]))
      mf[[1]] <- factor(mf[[1]])
    y.levels <- levels(mf[[1]])[-1]

    mm <- x %>% model_get_model_matrix()
    t <- colnames(mm)

    res$term <- rep.int(t, times = length(y.levels))
    res$y.level <- rep(y.levels, each = length(t))

    return(res)
  } else {
    mm <- x %>% model_get_model_matrix()
    t <- colnames(mm)
    t <- t[t != "(Intercept)"]
    b <- res$term[stringr::str_starts(res$term, "beta")]
    res$term <- c(b, t)
    return(res)
  }
}

#' Average Marginal Effects Estimation
#'
#' Use `margins::margins()` to estimate "average marginal effects" and return a
#' tibble tidied in a way that could be used by `broom.helpers`functions. See
#' `margins::margins()` for a list of supported models.
#' @note When applying `margins::margins()`, custom contrasts are ignored.
#' Treatment contrasts (`stats::contr.treatment()`) are applied to all
#' categorical variables. Interactions are also ignored.
#' @param x a model
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param ... additional parameters passed to `margins::margins()`
#' @family custom_tieders
#' @seealso `margins::margins()`
#' @export
#' @examples
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#' mod <- glm(
#'   Survived ~ Class + Age + Sex,
#'   data = df, weights = df$n, family = binomial
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
  attr(res, "coefficients_type") <- "average_marginal_effects"
  res
}

#' Marginal Effects Estimation
#'
#' Use `effects::allEffects()` to estimate "marginal effects" and return a
#' tibble tidied in a way that could be used by `broom.helpers`functions.
#' See `vignette("functions-supported-by-effects", package = "effects")` for
#' a list of supported models.
#' @param x a model
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param ... additional parameters passed to `effects::allEffects()`
#' @family custom_tieders
#' @seealso `effects::allEffects()`
#' @export
#' @examples
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#' mod <- glm(
#'   Survived ~ Class + Age + Sex,
#'   data = df, weights = df$n, family = binomial
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
    dplyr::bind_rows(.id = "variable")
  attr(res, "coefficients_type") <- "marginal_effects"
  res
}

#' Conditional Effects Estimation
#'
#' Use `ggeffects::ggpredict()` to estimate "conditional effects" and return a
#' tibble tidied in a way that could be used by `broom.helpers`functions.
#' See <https://strengejacke.github.io/ggeffects/> for a list of supported
#' models.
#' @param x a model
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param ... additional parameters passed to `ggeffects::ggpredict()`
#' @family custom_tieders
#' @seealso `ggeffects::ggpredict()`
#' @export
#' @examples
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#' mod <- glm(
#'   Survived ~ Class + Age + Sex,
#'   data = df, weights = df$n, family = binomial
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
    )
  attr(res, "coefficients_type") <- "conditional_effects"
  res
}
