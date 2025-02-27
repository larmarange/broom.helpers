#' Add header rows variables with several terms
#'
#' For variables with several terms (usually categorical variables but
#' could also be the case of continuous variables with polynomial terms
#' or splines), `tidy_add_header_rows()` will add an additional row
#' per variable, where `label` will be equal to `var_label`.
#' These additional rows could be identified with `header_row` column.
#'
#' The `show_single_row` argument allows to specify a list
#' of dichotomous variables that should be displayed on a single row
#' instead of two rows.
#'
#' The added `header_row` column will be equal to:
#'
#' * `TRUE` for an header row;
#' * `FALSE` for a normal row of a variable with an header row;
#' * `NA` for variables without an header row.
#'
#' If the `label` column is not yet available in `x`,
#' [tidy_add_term_labels()] will be automatically applied.
#' @param x (`data.frame`)\cr
#' A tidy tibble as produced by `tidy_*()` functions.
#' @param show_single_row ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#' Names of dichotomous variables that should be displayed on a single row.
#' See also [all_dichotomous()].
#' @param model (a model object, e.g. `glm`)\cr
#' The corresponding model, if not attached to `x`.
#' @inheritParams tidy_plus_plus
#' @export
#' @family tidy_helpers
#' @examplesIf interactive()
#' if (.assert_package("gtsummary", boolean = TRUE)) {
#'   df <- Titanic |>
#'     dplyr::as_tibble() |>
#'     dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#'
#'   res <-
#'     glm(
#'       Survived ~ Class + Age + Sex,
#'       data = df, weights = df$n, family = binomial,
#'       contrasts = list(Age = contr.sum, Class = "contr.SAS")
#'     ) |>
#'     tidy_and_attach() |>
#'     tidy_add_variable_labels(labels = list(Class = "Custom label for Class")) |>
#'     tidy_add_reference_rows()
#'   res |> tidy_add_header_rows()
#'   res |> tidy_add_header_rows(show_single_row = all_dichotomous())
#'
#'   glm(
#'     response ~ stage + grade * trt,
#'     gtsummary::trial,
#'     family = binomial,
#'     contrasts = list(
#'       stage = contr.treatment(4, base = 3),
#'       grade = contr.treatment(3, base = 2),
#'       trt = contr.treatment(2, base = 2)
#'     )
#'   ) |>
#'     tidy_and_attach() |>
#'     tidy_add_reference_rows() |>
#'     tidy_add_header_rows()
#' }
tidy_add_header_rows <- function(x,
                                 show_single_row = NULL,
                                 model = tidy_get_model(x),
                                 quiet = FALSE,
                                 strict = FALSE) {
  if (is.null(model)) {
    cli::cli_abort(c(
      "{.arg model} is not provided.",
      "You need to pass it or to use {.fn tidy_and_attach}."
    ))
  }

  if ("header_row" %in% names(x)) {
    if (!quiet) {
      cli_alert_danger(paste(
        "{.code tidy_add_header_rows()} has already been applied.",
        "x has been returned unchanged."
      ))
    }
    return(x)
  }

  .attributes <- .save_attributes(x)

  if (!"label" %in% names(x)) {
    x <- x |> tidy_add_term_labels(model = model)
  }

  # management of show_single_row --------------
  # if reference_rows have been defined, removal of reference row
  variables_to_simplify <- NULL
  # obtain character vector of selected variables
  cards::process_selectors(
    data = scope_tidy(x),
    show_single_row = {{ show_single_row }}
  )

  has_reference_row <- "reference_row" %in% names(x)
  if (!has_reference_row) {
    x$reference_row <- FALSE
  }

  xx <- x
  if ("y.level" %in% names(x)) {
    xx <- xx |>
      dplyr::filter(.data$y.level == x$y.level[1])
  }

  # checking if variables incorrectly requested for single row summary
  if ("component" %in% colnames(xx)) {
    bad_single_row <- xx |>
      dplyr::filter(
        !is.na(.data$variable),
        is.na(.data$reference_row) | !.data$reference_row,
        .data$variable %in% show_single_row
      ) |>
      dplyr::group_by(.data$component, .data$variable) |>
      dplyr::count() |>
      dplyr::filter(.data$n > 1) |>
      dplyr::pull(.data$variable)
  } else {
    bad_single_row <- xx |>
      dplyr::filter(
        !is.na(.data$variable),
        is.na(.data$reference_row) | !.data$reference_row,
        .data$variable %in% show_single_row
      ) |>
      dplyr::group_by(.data$variable) |>
      dplyr::count() |>
      dplyr::filter(.data$n > 1) |>
      dplyr::pull(.data$variable)
  }
  if (length(bad_single_row) > 0) {
    if (!quiet) {
      paste(
        "Variable(s) {paste(shQuote(bad_single_row), collapse = \", \")} were",
        "incorrectly requested to be printed on a single row."
      ) |>
        cli_alert_danger()
    }
    if (strict) {
      cli::cli_abort(
        "Incorrect call with `show_single_row=`. Quitting execution.",
        call = NULL
      )
    }
    show_single_row <- setdiff(show_single_row, bad_single_row)
  }

  if (
    length(show_single_row) > 0 &&
      any(x$variable %in% show_single_row)
  ) {
    if ("component" %in% colnames(xx)) {
      variables_to_simplify <- xx |>
        dplyr::filter(
          .data$variable %in% show_single_row & !.data$reference_row
        ) |>
        dplyr::count(.data$component, .data$variable) |>
        dplyr::filter(.data$n == 1) |>
        purrr::pluck("variable") |>
        unique()
    } else {
      variables_to_simplify <- xx |>
        dplyr::filter(
          .data$variable %in% show_single_row & !.data$reference_row
        ) |>
        dplyr::count(.data$variable) |>
        dplyr::filter(.data$n == 1) |>
        purrr::pluck("variable")
    }

    # removing reference rows of those variables
    if (length(variables_to_simplify) > 0) {
      x <- x |>
        dplyr::filter(
          is.na(.data$variable) |
            !.data$variable %in% variables_to_simplify |
            (.data$variable %in% variables_to_simplify & !.data$reference_row)
        )
    }

    # for variables in show_single_row
    # label should be equal to var_label
    x <- x |>
      dplyr::mutate(
        label = dplyr::if_else(
          .data$variable %in% show_single_row,
          .data$var_label,
          .data$label
        )
      )
  }

  if (!has_reference_row) {
    x <- x |> dplyr::select(-dplyr::all_of("reference_row"))
  }

  # computing header rows ---------------

  x <- x |>
    dplyr::mutate(
      rank = seq_len(dplyr::n()) # for sorting table at the end
    )

  if ("y.level" %in% names(x)) {
    header_rows <- x |>
      dplyr::filter(!is.na(.data$variable) & !.data$variable %in% show_single_row)

    if (nrow(header_rows) > 0) {
      header_rows <- header_rows |>
        dplyr::mutate(term_cleaned = .clean_backticks(.data$term, .data$variable)) |>
        dplyr::group_by(.data$variable, .data$y.level) |>
        dplyr::summarise(
          var_class = dplyr::first(.data$var_class),
          var_type = dplyr::first(.data$var_type),
          var_label = dplyr::first(.data$var_label),
          var_nlevels = dplyr::first(.data$var_nlevels),
          contrasts = dplyr::first(.data$contrasts),
          contrasts_type = dplyr::first(.data$contrasts_type),
          var_nrow = dplyr::n(),
          var_test = sum(.data$term_cleaned != .data$variable),
          rank = min(.data$rank) - .25,
          .groups = "drop_last"
        ) |>
        dplyr::filter(.data$var_nrow >= 2 | .data$var_test > 0) |>
        dplyr::select(-dplyr::all_of(c("var_nrow", "var_test"))) |>
        dplyr::mutate(
          header_row = TRUE,
          label = .data$var_label
        )
    }
  } else if ("component" %in% names(x)) {
    header_rows <- x |>
      dplyr::filter(!is.na(.data$variable) & !.data$variable %in% show_single_row)

    if (nrow(header_rows) > 0) {
      header_rows <- header_rows |>
        dplyr::mutate(term_cleaned = .clean_backticks(.data$term, .data$variable)) |>
        dplyr::group_by(.data$variable, .data$component) |>
        dplyr::summarise(
          var_class = dplyr::first(.data$var_class),
          var_type = dplyr::first(.data$var_type),
          var_label = dplyr::first(.data$var_label),
          var_nlevels = dplyr::first(.data$var_nlevels),
          contrasts = dplyr::first(.data$contrasts),
          contrasts_type = dplyr::first(.data$contrasts_type),
          var_nrow = dplyr::n(),
          var_test = sum(.data$term_cleaned != .data$variable),
          rank = min(.data$rank) - .25,
          .groups = "drop_last"
        ) |>
        dplyr::filter(.data$var_nrow >= 2 | .data$var_test > 0) |>
        dplyr::select(-dplyr::all_of(c("var_nrow", "var_test"))) |>
        dplyr::mutate(
          header_row = TRUE,
          label = .data$var_label
        )
    }
  } else {
    header_rows <- x |>
      dplyr::filter(
        !is.na(.data$variable) &
          !.data$variable %in% show_single_row &
          !.data$var_type %in% c("ran_pars", "ran_vals")
      )

    if (nrow(header_rows) > 0) {
      header_rows <- header_rows |>
        dplyr::mutate(term_cleaned = .clean_backticks(.data$term, .data$variable)) |>
        dplyr::group_by(.data$variable) |>
        dplyr::summarise(
          var_class = dplyr::first(.data$var_class),
          var_type = dplyr::first(.data$var_type),
          var_label = dplyr::first(.data$var_label),
          var_nlevels = dplyr::first(.data$var_nlevels),
          contrasts = dplyr::first(.data$contrasts),
          contrasts_type = dplyr::first(.data$contrasts_type),
          var_nrow = dplyr::n(),
          # for dichotomous variables with no reference row
          var_test = sum(.data$term_cleaned != .data$variable),
          rank = min(.data$rank) - .25,
          .groups = "drop_last"
        ) |>
        dplyr::filter(.data$var_nrow >= 2 | .data$var_test > 0) |>
        dplyr::select(-dplyr::all_of(c("var_nrow", "var_test"))) |>
        dplyr::mutate(
          header_row = TRUE,
          label = .data$var_label
        )
    }
  }

  x <- x |>
    dplyr::mutate(
      header_row = dplyr::if_else(.data$variable %in% header_rows$variable, FALSE, NA)
    ) |>
    dplyr::bind_rows(header_rows) |>
    dplyr::arrange(.data$rank) |>
    dplyr::select(-dplyr::all_of("rank"))

  x |>
    tidy_attach_model(model = model, .attributes = .attributes)
}
