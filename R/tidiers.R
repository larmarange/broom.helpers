#' Tidy a model with parameters package
#'
#' Use `parameters::model_parameters()` to tidy a model and apply
#' `parameters::standardize_names(style = "broom")` to the output
#' @param x a model
#' @param conf.int logical indicating whether or not to include a confidence
#' interval in the tidied output
#' @param conf.level the confidence level to use for the confidence interval
#' @param ... additional parameters passed to `parameters::model_parameters()`
#' @examples
#' if (require(parameters)) {
#'   lm(Sepal.Length ~ Sepal.Width + Species, data = iris) %>%
#'     tidy_parameters()
#' }
#' @export
#' @family custom_tieders
tidy_parameters <- function(x, conf.int = TRUE, conf.level = .95, ...) {
  if (!requireNamespace("parameters", quietly = TRUE))
    stop("You need to install 'parameters' to use 'tidy_parameters'.") #nocov

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
  requireNamespace("broom.mixed") # load broom.mixed if available
  res <- tryCatch(
    broom::tidy(x, conf.int = conf.int, conf.level, ...),
    error = function(e) {
      cli::cli_alert_warning("{.code broom::tidy()} failed to tidy the model.")
      cli::cli_alert_danger(e)
      NULL
    }
  )
  if (is.null(res)) {
    res <- tryCatch(
      tidy_parameters(x, conf.int = conf.int, conf.level, ...),
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
    }
  } else {
    # success of broom
    cli::cli_alert_success("Model tidied with {.code broom::tidy()}.")
  }
  res
}
