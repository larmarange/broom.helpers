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
#' @param x a tidy tibble
#' @param show_single_row a vector indicating the names of binary
#' variables that should be displayed on a single row
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
#'   tidy_add_variable_labels(labels = list(Class = "Custom label for Class")) %>%
#'   tidy_add_reference_rows() %>%
#'   tidy_add_header_rows()
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
#'     tidy_add_reference_rows() %>%
#'     tidy_add_header_rows()
#' }
tidy_add_header_rows <- function(x,
                                 show_single_row = NULL,
                                 model = tidy_get_model(x),
                                 quiet = FALSE,
                                 strict = FALSE) {
  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  if ("header_row" %in% names(x)) {
    if (!quiet)
      usethis::ui_oops("tidy_add_header_rows() has already been applied. x has been returned unchanged.")
    return(x)
  }

  if (!"label" %in% names(x)) {
    x <- x %>% tidy_add_term_labels(model = model)
  }

  # management of show_single_row --------------
  # if reference_rows have been defined, removal of reference row
  variables_to_simplify <- NULL
  show_single_row <- stats::na.omit(unique(show_single_row))

  has_reference_row <- "reference_row" %in% names(x)
  if (!has_reference_row)
    x$reference_row <- FALSE

  # checking if variables incorrectly requested for single row summary
  bad_single_row <- x %>%
    dplyr::filter(!is.na(.data$variable),
                  is.na(.data$reference_row) | !.data$reference_row,
                  .data$variable %in% show_single_row) %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::count() %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::pull(.data$variable)
  if (length(bad_single_row) > 0) {
    if (!quiet)
      paste("Variable(s) {paste(shQuote(bad_single_row), collapse = ", ")} were",
            "incorrectly requested to be printed on a single row.") %>%
      usethis::ui_oops()
    if (strict) stop("Incorrect call with `show_single_row=`. Quitting execution.", call. = FALSE)
  }

  if (
    length(show_single_row) > 0 &&
      any(x$variable %in% show_single_row)
  ) {
    xx <- x
    if ("y.level" %in% names(x)) { # specific case for multinom
      xx <- xx %>%
        dplyr::filter(.data$y.level == x$y.level[1])
    }

    variables_to_simplify <- xx %>%
      dplyr::filter(
        .data$variable %in% show_single_row & !.data$reference_row
      ) %>%
      dplyr::count(.data$variable) %>%
      dplyr::filter(.data$n == 1) %>%
      purrr::pluck("variable")

    # removing reference rows of those variables
    if (length(variables_to_simplify) > 0) {
      x <- x %>%
        dplyr::filter(
          is.na(.data$variable) |
            !.data$variable %in% variables_to_simplify |
            (.data$variable %in% variables_to_simplify & !.data$reference_row)
        ) %>%
        dplyr::mutate(
          label = dplyr::if_else(
            .data$variable %in% variables_to_simplify,
            .data$var_label,
            .data$label
          )
        )
    }
  }

  if (!has_reference_row)
    x <- x %>% dplyr::select(-.data$reference_row)

  # computing header rows ---------------

  x <- x %>%
    dplyr::mutate(
      rank = 1:dplyr::n() # for sorting table at the end
    )

  if ("y.level" %in% names(x)) { # specific case for nnet::multinom
    header_rows <- x %>%
      dplyr::filter(!is.na(.data$variable) & !.data$variable %in% variables_to_simplify)

    if (nrow(header_rows) > 0) {
      header_rows <- header_rows %>%
        dplyr::group_by(.data$variable, .data$y.level) %>%
        dplyr::summarise(
          var_class = dplyr::first(.data$var_class),
          var_type = dplyr::first(.data$var_type),
          var_label = dplyr::first(.data$var_label),
          contrasts = dplyr::first(.data$contrasts),
          var_nrow = dplyr::n(),
          var_test = sum(.data$term != .data$variable),
          rank = min(.data$rank) - .25,
          .groups = "drop_last"
        ) %>%
        dplyr::filter(.data$var_nrow >= 2 | .data$var_test > 0) %>%
        dplyr::select(-.data$var_nrow, -.data$var_test) %>%
        dplyr::mutate(header_row = TRUE)
    }
  } else {
    header_rows <- x %>%
      dplyr::filter(!is.na(.data$variable) & !.data$variable %in% variables_to_simplify)

    if (nrow(header_rows) > 0)
      header_rows <- header_rows %>%
        dplyr::group_by(.data$variable) %>%
        dplyr::summarise(
          var_class = dplyr::first(.data$var_class),
          var_type = dplyr::first(.data$var_type),
          var_label = dplyr::first(.data$var_label),
          contrasts = dplyr::first(.data$contrasts),
          var_nrow = dplyr::n(),
          var_test = sum(.data$term != .data$variable),
          rank = min(.data$rank) - .25,
          .groups = "drop_last"
        ) %>%
        dplyr::filter(.data$var_nrow >= 2 | .data$var_test > 0) %>%
        dplyr::select(-.data$var_nrow, -.data$var_test) %>%
        dplyr::mutate(
          header_row = TRUE,
          label = .data$var_label
        )
  }

  x <- x %>%
    dplyr::mutate(
      header_row = dplyr::if_else(.data$variable %in% header_rows$variable, FALSE, NA)
    ) %>%
    dplyr::bind_rows(header_rows) %>%
    dplyr::arrange(.data$rank) %>%
    dplyr::select(-.data$rank)

  x %>%
    tidy_attach_model(model = model) %>%
    .order_tidy_columns()
}
