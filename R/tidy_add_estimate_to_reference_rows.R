#' Add an estimate value to references rows for categorical variables
#'
#' For categorical variables with a treatment contrast
#' ([stats::contr.treatment()]) or a SAS contrast ([stats::contr.SAS()])
#' will add an estimate equal to `0` (or `1` if `exponentiate = TRUE`)
#' to the reference row.
#'
#' For categorical variables with a sum contrast ([stats::contr.sum()]),
#' the estimate value of the reference row will be equal to the sum of
#' all other coefficients multiplied by `-1` (eventually exponentiated if
#' `exponentiate = TRUE`), and obtained with `emmeans::emmeans()`.
#' The `emmeans` package should therefore be installed.
#' For sum contrasts, the model coefficient corresponds
#' to the difference of each level with the grand mean.
#'
#' For other variables, no change will be made.
#'
#' @details
#' If the `reference_row` column is not yet available in `x`,
#' [tidy_add_reference_rows()] will be automatically applied.
#'
#' @param x a tidy tibble
#' @param exponentiate logical indicating whether or not to exponentiate the
#' coefficient estimates. It should be consistent with the original call to
#' [broom::tidy()]
#' @param model the corresponding model, if not attached to `x`
#' @inheritParams tidy_plus_plus
#' @export
#' @family tidy_helpers
#' @examplesIf interactive()
#' if (.assert_package("gtsummary", boolean = TRUE) && .assert_package("emmeans", boolean = TRUE)) {
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(dplyr::across(where(is.character), factor))
#'
#' df %>%
#'   glm(
#'     Survived ~ Class + Age + Sex,
#'     data = ., weights = .$n, family = binomial,
#'     contrasts = list(Age = contr.sum, Class = "contr.SAS")
#'   ) %>%
#'   tidy_and_attach(exponentiate = TRUE) %>%
#'   tidy_add_reference_rows() %>%
#'   tidy_add_estimate_to_reference_rows()
#'
#' glm(
#'   response ~ stage + grade * trt,
#'   gtsummary::trial,
#'   family = binomial,
#'   contrasts = list(
#'     stage = contr.treatment(4, base = 3),
#'     grade = contr.treatment(3, base = 2),
#'     trt = contr.treatment(2, base = 2)
#'   )
#' ) %>%
#'   tidy_and_attach() %>%
#'   tidy_add_reference_rows() %>%
#'   tidy_add_estimate_to_reference_rows()
#' }
tidy_add_estimate_to_reference_rows <- function(
  x, exponentiate = attr(x, "exponentiate"),
  model = tidy_get_model(x),
  quiet = FALSE
) {
  if (is.null(exponentiate) || !is.logical(exponentiate))
    cli::cli_abort("{.arg exponentiate} is not provided. You need to pass it explicitely.")

  if (is.null(model)) {
    cli::cli_abort(c(
      "{.arg model} is not provided.",
      "You need to pass it or to use {.fn tidy_and_attach}."
    ))
  }

  .attributes <- .save_attributes(x)
  .attributes$exponentiate <- exponentiate

  if (!"reference_row" %in% names(x)) {
    x <- x %>% tidy_add_reference_rows(model = model)
  }

  if (!"estimate" %in% names(x)) { # to avoid a problem with certain types of model (e.g. gam)
    return(x %>% tidy_attach_model(model))
  }

  # treatment contrasts
  x <- x %>%
    dplyr::mutate(
      estimate = dplyr::if_else(
        !is.na(.data$reference_row) &
          .data$reference_row &
          stringr::str_starts(.data$contrasts, "contr.treatment|contr.SAS"),
        dplyr::if_else(exponentiate, 1, 0),
        .data$estimate
      )
    )

  # sum contrasts
  ref_rows_sum <- which(x$reference_row & x$contrasts == "contr.sum")
  if (length(ref_rows_sum) > 0) {
    for (i in ref_rows_sum) {
      est <- .get_ref_row_estimate_contr_sum(
        x$variable[i],
        model = model,
        exponentiate = exponentiate,
        quiet = quiet
      )
      x$estimate[i] <- est$estimate
      x$std.error[i] <- est$std.error
      x$p.value[i] <- est$p.value
      if (all(c("conf.low", "conf.high") %in% names(x))) {
        x$conf.low[i] <- est$conf.low
        x$conf.high[i] <- est$conf.high
      }
    }
  }

  x %>%
    tidy_attach_model(model = model, .attributes = .attributes)
}


.get_ref_row_estimate_contr_sum <- function(variable, model, exponentiate = FALSE,
                                            quiet = FALSE) {

  if (inherits(model, "multinom")) {
    dc <- NULL
    if (!quiet)
      cli_alert_info(paste0(
        "Sum contrasts are not supported for 'multinom' models.\n",
        "Reference row of variable '", variable, "' remained unchanged."
      ))
  } else if (inherits(model, "LORgee")) {
    dc <- NULL
    if (!quiet)
      cli_alert_info(paste0(
        "Sum contrasts are not supported for {.pkg multgee} models.\n",
        "Reference row of variable '", variable, "' remained unchanged."
      ))
  } else {
    .assert_package("emmeans", fn = "broom.helpers::tidy_add_estimate_to_reference_rows()")

    dc <- tryCatch(
      suppressMessages(
        emmeans::emmeans(model, specs = variable, contr = "eff")
      ),
      error = function(e) {
        if (!quiet)
          cli_alert_info(paste0(
            "No emmeans() method for this type of model.\n",
            "Reference row of variable '", variable, "' remained unchanged."
          ))
        NULL
      }
    )
  }

  if (is.null(dc)) {
    res <- data.frame(
      estimate = NA_real_,
      std.error = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_
    )
  } else {
    res <- dc$contrasts %>%
      as.data.frame() %>%
      dplyr::last() %>%
      dplyr::select("estimate", std.error = "SE", "p.value")
    ci <- dc %>%
      emmeans::contrast() %>%
      confint() %>%
      dplyr::last()
    res$conf.low <- ci$asymp.LCL
    res$conf.high <- ci$asymp.UCL
  }

  if (exponentiate) {
    res$estimate <- exp(res$estimate)
    res$conf.low <- exp(res$conf.low)
    res$conf.high <- exp(res$conf.high)
  }
  res
}
