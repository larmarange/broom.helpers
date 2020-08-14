#' Add header rows variables with several terms
#'
#' For variables with several terms (usually categorical variables but
#' could also be the case of continuous variables with polynomial terms
#' or splines), `tidy_add_header_rows()` will add an additional row
#' per variable, where `label` will be equal to `var_label`.
#' These additional rows could be identified with `header_row` column.
#'
#' In addition, the `show_single_row` argument allows to specify a list
#' of dichotomous variables that should be displayed on a single row
#' instead of two rows. This argument will have no effect if reference
#' rows have not been added to the tibble (cf. [tidy_add_reference_rows()]).
#'
#' @details
#' If the `label` column is not yet available in `x`,
#' [tidy_add_term_labels()] will be automatically applied.
#' @param x a tidy tibble
#' @param show_single_row a vector indicating the names of binary
#' variables that should be displayed on a single row
#' @param model the corresponding model, if not attached to `x`
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
#'   tidy_add_reference_rows() %>%
#'   tidy_add_header_rows()
#'
#' if(requireNamespace("gtsummary")) {
#'   glm(
#'     response ~ stage + grade * trt,
#'     gtsummary::trial,
#'     family = binomial,
#'     contrasts = list(stage = contr.treatment(4, base = 3),
#'                      grade = contr.treatment(3, base = 2),
#'                      trt = contr.treatment(2, base = 2))
#'   ) %>%
#'     tidy_and_attach() %>%
#'     tidy_add_reference_rows() %>%
#'     tidy_add_header_rows()
#' }
tidy_add_header_rows <- function(x, show_single_row = NULL, model = tidy_get_model(x)) {
  if (is.null(model))
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  x <- x %>% tidy_attach_model(model) # in case not already attached

  if ("header_row" %in% names(x)) {
    warning("tidy_add_header_rows() has already been applied. x has been returned unchanged.")
    return(x)
  }

  if (!"label" %in% names(x))
    x <- x %>% tidy_add_term_labels()

  # management of show_single_row --------------
  # only if reference_rows have been defined
  show_single_row <- stats::na.omit(unique(show_single_row))
  if (
    length(show_single_row) > 0 &&
    "reference_row" %in% names(x) &&
    any(x$variable %in% show_single_row)
  ) {
    xx <- x
    if ("y.level" %in% names(x)) { # specific case for multinom
      xx <- xx %>%
        dplyr::filter(.data$y.level == x$y.level[1])
    }

    variables_to_simplify <- xx %>%
      dplyr::filter(.data$variable %in% show_single_row) %>%
      dplyr::count(.data$variable) %>%
      dplyr::filter(.data$n == 2) %>%
      `[[`("variable")
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

  # computing header rows ---------------

  x <- x %>%
    dplyr::mutate(
      header_row = FALSE,
      rank = 1:dplyr::n() # for sorting table at the end
    )

  if ("y.level" %in% names(x)) { # specific case for nnet::multinom
    header_rows <- x %>%
      dplyr::filter(!is.na(.data$variable)) %>%
      dplyr::group_by(.data$variable, .data$y.level) %>%
      dplyr::summarise(
        var_class = dplyr::first(.data$var_class),
        var_type = dplyr::first(.data$var_type),
        var_label = dplyr::first(.data$var_label),
        var_nrow = dplyr::n(),
        rank = min(.data$rank) - .25,
        .groups = "drop_last"
      ) %>%
      dplyr::filter(.data$var_nrow >= 2) %>%
      dplyr::select(-.data$var_nrow) %>%
      dplyr::mutate(header_row = TRUE)
    x <- x %>%
      dplyr::bind_rows(header_rows)
  } else {
    header_rows <- x %>%
      dplyr::filter(!is.na(.data$variable)) %>%
      dplyr::group_by(.data$variable) %>%
      dplyr::summarise(
        var_class = dplyr::first(.data$var_class),
        var_type = dplyr::first(.data$var_type),
        var_label = dplyr::first(.data$var_label),
        var_nrow = dplyr::n(),
        rank = min(.data$rank) - .25,
        .groups = "drop_last"
      ) %>%
      dplyr::filter(.data$var_nrow >= 2) %>%
      dplyr::select(-.data$var_nrow) %>%
      dplyr::mutate(
        header_row = TRUE,
        label = .data$var_label
      )
    x <- x %>%
      dplyr::bind_rows(header_rows)
  }

  x %>%
    dplyr::arrange(.data$rank) %>%
    dplyr::select(-.data$rank)
}


