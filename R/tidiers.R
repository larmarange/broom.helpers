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

  x %>%
    parameters::model_parameters(ci = conf.level, ...) %>%
    parameters::standardize_names(style = "broom")
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
  if (any(c("glmerMod", "lmerMod") %in% class(x))) {
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
  tidy_args$conf.level <- conf.level

  res <- tryCatch(
    do.call(broom::tidy, tidy_args),
    error = function(e) {
      NULL
    }
  )

  # trying without exponentiate
  if (is.null(res)) {
    tidy_args2 <- tidy_args
    tidy_args2$exponentiate <- NULL
    res <- tryCatch(
      do.call(broom::tidy, tidy_args2),
      error = function(e) {
        cli::cli_alert_warning("{.code broom::tidy()} failed to tidy the model.")
        cli::cli_alert_danger(e)
        NULL
      }
    )
    if (!is.null(res) && !is.null(tidy_args$exponentiate) && tidy_args$exponentiate) {
      # changing to FALSE is managed by tidy_and_attch()
      stop("'exponentiate = TRUE' is not valid for this type of model.")
    }
  }

  if (is.null(res)) {
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

#' Tidy a `multgee` model
#'
#' `r lifecycle::badge("experimental")`
#' @param x a `multgee::nomLORgee()` or a `multgee::ordLORgee()` model
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param ... additional parameters passed to [parameters::model_parameters()]
#' @export
#' @family custom_tieders
tidy_multgee <- function(x, conf.int = TRUE, conf.level = .95, ...) {
  if (!inherits(x, "LORgee"))
    cli::cli_abort(paste(
      "Only {.fn multgee::nomLORgee} and {.fn multgee::ordLORgee} models",
      "are supported."
    ))

  res <- tidy_parameters(x, conf.int = conf.int, conf.level = conf.level, ...)
  res$original_term <- res$term

  # multinomial model
  if (x$call[[1]] == "nomLORgee") {
    mf <- x %>% model_get_model_frame()
    if (!is.factor(mf[[1]]))
      mf[[1]] <- factor(mf[[1]])
    y.levels <- levels(mf[[1]])[-1]

    mm <- x %>% model_get_model_matrix()
    t <- colnames(mm)

    res$term <- rep.int(t, times = length(y.levels))
    res$y.level <- rep(y.levels, each = length(t))

    return(res)
  }

  if (x$call[[1]] == "ordLORgee") {
    mm <- x %>% model_get_model_matrix()
    t <- colnames(mm)
    t <- t[t != "(Intercept)"]
    b <- res$term[stringr::str_starts(res$term, "beta")]
    res$term <- c(b, t)
    return(res)
  }
  cli::cli_abort("Type of model not covered")
}
