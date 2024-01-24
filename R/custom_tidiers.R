#' Tidy a model with parameters package
#'
#' Use [parameters::model_parameters()] to tidy a model and apply
#' `parameters::standardize_names(style = "broom")` to the output
#' @param x a model
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param ... additional parameters passed to [parameters::model_parameters()]
#' @note
#' For [betareg::betareg()], the component column in the results is standardized
#' with [broom::tidy()], using `"mean"` and `"precision"` values.
#' @examplesIf interactive()
#' if (.assert_package("parameters", boolean = TRUE)) {
#'   lm(Sepal.Length ~ Sepal.Width + Species, data = iris) %>%
#'     tidy_parameters()
#' }
#' @export
#' @family custom_tieders
tidy_parameters <- function(x, conf.int = TRUE, conf.level = .95, ...) {
  .assert_package("parameters", fn = "broom.helpers::tidy_parameters()")
  args <- list(...)
  if (!conf.int) conf.level <- NULL
  args$ci <- conf.level
  args$model <- x

  if (
    inherits(x, "betareg") &&
      !is.null(args$component) &&
      args$component == "mean"
  ) {
    args$component <- "conditional"
  }

  res <-
    do.call(parameters::model_parameters, args) %>%
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

  if (!is.null(args$component)) {
    attr(res, "component") <- args$component
  }

  # for betareg, need to standardize component with tidy::broom()
  if (inherits(x, "betareg")) {
    if (is.null(args$component) || args$component == "conditional") {
      res$component <- "mean"
    }
    if (!is.null(args$component) && args$component == "precision") {
      res$component <- "precision"
    }
    if (!is.null(args$component) && args$component == "all") {
      res$component[res$component == "conditional"] <- "mean"
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
    cli::cli_alert_success("{.fn tidy_multgee} used instead.")
    cli::cli_alert_info(
      "Add {.code tidy_fun = broom.helpers::tidy_multgee} to quiet these messages."
    )
    return(tidy_multgee(x, conf.int = conf.int, conf.level = conf.level, ...))
  }

  if (inherits(x, "zeroinfl")) {
    cli::cli_alert_info("{.cls zeroinfl} model detected.")
    cli::cli_alert_success("{.fn tidy_zeroinfl} used instead.")
    cli::cli_alert_info(
      "Add {.code tidy_fun = broom.helpers::tidy_zeroinfl} to quiet these messages."
    )
    return(tidy_zeroinfl(x, conf.int = conf.int, conf.level = conf.level, ...))
  }

  if (inherits(x, "hurdle")) {
    cli::cli_alert_info("{.cls hurdle} model detected.")
    cli::cli_alert_success("{.fn tidy_zeroinfl} used instead.")
    cli::cli_alert_info(
      "Add {.code tidy_fun = broom.helpers::tidy_zeroinfl} to quiet these messages."
    )
    return(tidy_zeroinfl(x, conf.int = conf.int, conf.level = conf.level, ...))
  }

  tidy_args <- list(...)
  tidy_args$x <- x
  tidy_args$conf.int <- conf.int
  if (conf.int) tidy_args$conf.level <- conf.level

  # class of models known for tidy() not supporting exponentiate argument
  # and for ignoring it
  if (any(c("fixest", "plm", "felm", "lavaan", "nls", "survreg", "cch") %in% class(x))) {
    if (isFALSE(tidy_args$exponentiate)) {
      tidy_args$exponentiate <- NULL
    } else if (isTRUE(tidy_args$exponentiate)) {
      cli::cli_abort("'exponentiate = TRUE' is not valid for this type of model.")
    }
  }

  # class of models known for tidy() not supporting conf.int argument
  # and for ignoring it in tidy_args
  if (any(c("cch") %in% class(x))) {
    tidy_args$conf.int <- NULL
  }

  # for betareg, if exponentiate = TRUE, forcing tidy_parameters,
  # by adding `component = "all" to the arguments`
  if (inherits(x, "betareg")) {
    if (isFALSE(tidy_args$exponentiate)) {
      tidy_args$exponentiate <- NULL
    } else if (isTRUE(tidy_args$exponentiate)) {
      component <- tidy_args$component
      cli::cli_alert_info(
        "{.code exponentiate = TRUE} not valid for {.cl betareg} with {.fn broom::tidy()}."
      )
      if (is.null(component)) {
        cli::cli_alert_success("{.code tidy_parameters(component = \"all\")} used instead.")
        cli::cli_alert_info(
          "Add {.code tidy_fun = broom.helpers::tidy_parameters} to quiet these messages."
        )
        return(
          tidy_parameters(
            x,
            conf.int = conf.int,
            conf.level = conf.level,
            component = "all",
            ...
          )
        )
      } else {
        cli::cli_alert_success("{.code tidy_parameters()} used instead.")
        cli::cli_alert_info(
          "Add {.code tidy_fun = broom.helpers::tidy_parameters} to quiet these messages."
        )
        return(
          tidy_parameters(
            x,
            conf.int = conf.int,
            conf.level = conf.level,
            ...
          )
        )
      }
    }
  }

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
      # changing to FALSE is managed by tidy_and_attach()
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
      cli::cli_abort("Unable to tidy {.arg x}.")
    } else {
      # success of parameters
      cli::cli_alert_success("{.code tidy_parameters()} used instead.")
      cli::cli_alert_info(
        "Add {.code tidy_fun = broom.helpers::tidy_parameters} to quiet these messages."
      )
    }
  }

  # cleaning in conf.int = FALSE
  if (isFALSE(conf.int)) {
    res <- res %>%
      dplyr::select(-dplyr::any_of(c("conf.low", "conf.high")))
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
  if (!inherits(x, "LORgee")) {
    cli::cli_abort(paste(
      "Only {.fn multgee::nomLORgee} and {.fn multgee::ordLORgee} models",
      "are supported."
    ))
  }

  res <- tidy_parameters(x, conf.int = conf.int, conf.level = conf.level, ...)
  res$original_term <- res$term

  # multinomial model
  if (stringr::str_detect(x$title, "NOMINAL")) {
    mf <- x %>% model_get_model_frame()
    if (!is.factor(mf[[1]])) {
      mf[[1]] <- factor(mf[[1]])
    }
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

#' Tidy a `zeroinfl` or a `hurdle` model
#'
#' `r lifecycle::badge("experimental")`
#' A tidier for models generated with `pscl::zeroinfl()` or `pscl::hurdle()`.
#' Term names will be updated to be consistent with generic models. The original
#' term names are preserved in an `"original_term"` column.
#' @param x a `pscl::zeroinfl()` or a `pscl::hurdle()` model
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param component `NULL` or one of `"all"`, `"conditional"`, `"zi"`, or
#' `"zero_inflated"`
#' @param ... additional parameters passed to `parameters::model_parameters()`
#' @export
#' @family custom_tieders
#' @examplesIf interactive()
#' if (.assert_package("pscl", boolean = TRUE)) {
#'   library(pscl)
#'   mod <- zeroinfl(
#'     art ~ fem + mar + phd,
#'     data = pscl::bioChemists
#'   )
#'
#'   mod %>% tidy_zeroinfl(exponentiate = TRUE)
#' }
tidy_zeroinfl <- function(
    x,
    conf.int = TRUE,
    conf.level = .95,
    component = NULL,
    ...) {
  if (!inherits(x, "zeroinfl") && !inherits(x, "hurdle")) {
    cli::cli_abort("{.arg x} should be of class {.cls zeroinfl} or {.cls hurdle}")
  } # nolint

  res <- tidy_parameters(
    x,
    conf.int = conf.int,
    conf.level = conf.level,
    component = component,
    ...
  )
  res$original_term <- res$term
  starts_zero <- stringr::str_starts(res$term, "zero_")
  res$term[starts_zero] <- stringr::str_sub(res$term[starts_zero], 6)
  starts_count <- stringr::str_starts(res$term, "count_")
  res$term[starts_count] <- stringr::str_sub(res$term[starts_count], 7)

  if (!is.null(component) && component %in% c("conditional", "zero_inflated")) {
    res$component <- component
  }
  if (!is.null(component) && component == "zi") {
    res$component <- "zero_inflated"
  }

  attr(res, "component") <- component
  res
}
