#' Add references rows for categorical variables
#'
#' For categorical variables with a treatment contrast
#' ([stats::contr.treatment()]), a SAS contrast ([stats::contr.SAS()])
#' or a sum contrast ([stats::contr.sum()]), add a reference row.
#'
#' The added `reference_row` column will be equal to:
#'
#' * `TRUE` for a reference row;
#' * `FALSE` for a normal row of a variable with a reference row;
#' * `NA` for variables without a reference row.
#'
#' If the `contrasts` column is not yet available in `x`,
#' [tidy_add_contrasts()] will be automatically applied.
#'
#' `tidy_add_reference_rows()` will not populate the label
#' of the reference term. It is therefore better to apply
#' [tidy_add_term_labels()] after `tidy_add_reference_rows()`
#' rather than before.
#' @param x a tidy tibble
#' @param model the corresponding model, if not attached to `x`
#' @inheritParams tidy_plus_plus
#' @export
#' @family tidy_helpers
#' @examples
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#'
#' df %>%
#'   glm(
#'     Survived ~ Class + Age + Sex,
#'     data = ., weights = .$n, family = binomial,
#'     contrasts = list(Age = contr.sum, Class = "contr.SAS")
#'   ) %>%
#'   tidy_and_attach() %>%
#'   tidy_add_reference_rows()
#'
#' if (requireNamespace("gtsummary")) {
#'   glm(
#'     response ~ stage + grade * trt,
#'     gtsummary::trial,
#'     family = binomial,
#'     contrasts = list(
#'       stage = contr.treatment(4, base = 3),
#'       grade = contr.treatment(3, base = 2),
#'       trt = contr.treatment(2, base = 2)
#'     )
#'   ) %>%
#'     tidy_and_attach() %>%
#'     tidy_add_reference_rows()
#' }
tidy_add_reference_rows <- function(x, model = tidy_get_model(x), quiet = FALSE) {
  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  if ("header_row" %in% names(x)) {
    stop("`tidy_add_reference_rows()` cannot be applied after `tidy_add_header_rows().`")
  }

  if ("reference_row" %in% names(x)) {
    if (!quiet)
      message("tidy_add_reference_rows() has already been applied. x has been returned unchanged.")
    return(x)
  }

  if ("label" %in% names(x)) {
    if (!quiet)
      message(paste0(
        "tidy_add_reference_rows() has been applied after tidy_add_term_labels().\n",
        "You should consider applying tidy_add_reference_rows() first."
      ))
  }

  if (!"contrasts" %in% names(x)) {
    x <- x %>% tidy_add_contrasts(model = model)
  }

  has_var_label <- "var_label" %in% names(x)
  if (!has_var_label) {
    x$var_label <- NA_character_
  } # temporary populate it

  x <- x %>%
    dplyr::mutate(
      reference_row = dplyr::if_else(
        .data$contrasts %in% c("contr.treatment", "contr.SAS", "contr.sum"),
        FALSE,
        NA
      ),
      reference_row = dplyr::if_else(
        .data$contrasts %>% stringr::str_starts("contr.treatment"),
        FALSE,
        .data$reference_row
      ),
      rank = 1:dplyr::n() # for sorting table at the end
    )

  if ("y.level" %in% names(x)) { # specific case for nnet::multinom
    # contr.treatment -> add reference row before
    # base term needs to be taken into account
    if (any(!is.na(x$contrasts) & stringr::str_starts(x$contrasts, "contr.treatment"))) {
      xlevels <- model_get_xlevels(model)
      ref_rows_before <- x %>%
        dplyr::filter(.data$contrasts %>% stringr::str_starts("contr.treatment")) %>%
        dplyr::group_by(.data$variable, .data$y.level) %>%
        dplyr::summarise(
          var_class = dplyr::first(.data$var_class),
          var_type = dplyr::first(.data$var_type),
          var_label = dplyr::last(.data$var_label),
          contrasts = dplyr::first(.data$contrasts),
          rank = min(.data$rank) - .25,
          .groups = "drop_last"
        ) %>%
        dplyr::mutate(
          contr_base = stringr::str_replace(.data$contrasts, "contr.treatment\\(base=([0-9]+)\\)", "\\1"),
          contr_base = stringr::str_replace(.data$contr_base, "contr.treatment", "1"),
          contr_base = as.integer(.data$contr_base),
          rank = .data$rank + .data$contr_base - 1, # update position based on rank
          # term = paste0(.data$variable, "_ref"),
          reference_row = TRUE
        ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(term = paste0(.data$variable, xlevels[[.data$variable]][.data$contr_base])) %>%
        dplyr::select(-.data$contr_base)
      x <- x %>%
        dplyr::bind_rows(ref_rows_before)
    }

    # contr.SAS & contr.sum -> add reference row after
    if (any(!is.na(x$contrasts) & x$contrasts %in% c("contr.sum", "contr.SAS"))) {
      ref_rows_after <- x %>%
        dplyr::filter(.data$contrasts %in% c("contr.sum", "contr.SAS")) %>%
        dplyr::group_by(.data$variable, .data$y.level) %>%
        dplyr::summarise(
          var_class = dplyr::last(.data$var_class),
          var_type = dplyr::last(.data$var_type),
          var_label = dplyr::last(.data$var_label),
          contrasts = dplyr::last(.data$contrasts),
          rank = max(.data$rank) + .25,
          n_levels = dplyr::n(),
          .groups = "drop_last"
        ) %>%
        dplyr::mutate(
          term = paste0(.data$variable, .data$n_levels),
          reference_row = TRUE
        ) %>%
        dplyr::select(-.data$n_levels)
      x <- x %>%
        dplyr::bind_rows(ref_rows_after)
    }
  } else {
    # contr.treatment -> add reference row before
    # base term needs to be taken into account
    if (any(!is.na(x$contrasts) & stringr::str_starts(x$contrasts, "contr.treatment"))) {
      xlevels <- model_get_xlevels(model)
      ref_rows_before <- x %>%
        dplyr::filter(.data$contrasts %>% stringr::str_starts("contr.treatment")) %>%
        dplyr::group_by(.data$variable) %>%
        dplyr::summarise(
          var_class = dplyr::first(.data$var_class),
          var_type = dplyr::first(.data$var_type),
          var_label = dplyr::last(.data$var_label),
          contrasts = dplyr::first(.data$contrasts),
          rank = min(.data$rank) - .25,
          .groups = "drop_last"
        ) %>%
        dplyr::mutate(
          contr_base = stringr::str_replace(.data$contrasts, "contr.treatment\\(base=([0-9]+)\\)", "\\1"),
          contr_base = stringr::str_replace(.data$contr_base, "contr.treatment", "1"),
          contr_base = as.integer(.data$contr_base),
          rank = .data$rank + .data$contr_base - 1, # update position based on rank
          # term = paste0(.data$variable, "_ref"),
          reference_row = TRUE
        ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(term = paste0(.data$variable, xlevels[[.data$variable]][.data$contr_base])) %>%
        dplyr::select(-.data$contr_base)
      x <- x %>%
        dplyr::bind_rows(ref_rows_before)
    }

    # contr.SAS & contr.sum -> add reference row after
    if (any(!is.na(x$contrasts) & x$contrasts %in% c("contr.sum", "contr.SAS"))) {
      ref_rows_after <- x %>%
        dplyr::filter(.data$contrasts %in% c("contr.sum", "contr.SAS")) %>%
        dplyr::group_by(.data$variable) %>%
        dplyr::summarise(
          var_class = dplyr::last(.data$var_class),
          var_type = dplyr::last(.data$var_type),
          var_label = dplyr::last(.data$var_label),
          contrasts = dplyr::last(.data$contrasts),
          rank = max(.data$rank) + .25,
          n_levels = dplyr::n(),
          .groups = "drop_last"
        ) %>%
        dplyr::mutate(
          term = paste0(.data$variable, .data$n_levels + 1),
          reference_row = TRUE
        ) %>%
        dplyr::select(-.data$n_levels)
      x <- x %>%
        dplyr::bind_rows(ref_rows_after)
    }
  }

  if (!has_var_label) {
    x <- x %>% dplyr::select(-.data$var_label)
  }

  x %>%
    dplyr::arrange(.data$rank) %>%
    dplyr::select(-.data$rank) %>%
    tidy_attach_model(model = model) %>%
    .order_tidy_columns()
}
