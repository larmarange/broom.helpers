#' Add term labels
#'
#' Will add term labels in a `label` column, based on:
#' 1. labels provided in `labels` argument if provided;
#' 2. factor levels for categorical variables coded with
#'    treatment, SAS or sum contrasts (the label could be
#'    customized with `categorical_terms_pattern` argument);
#' 3. variable labels when there is only one term per variable;
#' 4. term name otherwise.
#'
#' @details
#' If the `variable_label` column is not yet available in `x`,
#' [tidy_add_variable_labels()] will be automatically applied.
#' If the `contrasts` column is not yet available in `x`,
#' [tidy_add_contrasts()] will be automatically applied.
#'
#' It is possible to pass a custom label for any term in `labels`,
#' including interaction terms.
#' @param x (`data.frame`)\cr
#' A tidy tibble as produced by `tidy_*()` functions.
#' @param labels (`list` or `string`)\cr
#' An optional named list or named vector of custom term labels.
#' @param interaction_sep  (`string`)\cr
#' Separator for interaction terms.
#' @param categorical_terms_pattern ([`glue pattern`][glue::glue()])\cr
#' A [glue pattern][glue::glue()] for labels of categorical terms with treatment
#' or sum contrasts (see examples and [model_list_terms_levels()]).
#' @param relabel_poly Should terms generated with [stats::poly()] be relabeled?
#' @param model (a model object, e.g. `glm`)\cr
#' The corresponding model, if not attached to `x`.
#' @inheritParams tidy_plus_plus
#' @export
#' @family tidy_helpers
#' @examples
#' \donttest{
#' df <- Titanic |>
#'   dplyr::as_tibble() |>
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) |>
#'   labelled::set_variable_labels(
#'     Class = "Passenger's class",
#'     Sex = "Sex"
#'   )
#'
#' mod <-
#'   glm(Survived ~ Class * Age * Sex, data = df, weights = df$n, family = binomial)
#' mod |>
#'   tidy_and_attach() |>
#'   tidy_add_term_labels()
#' mod |>
#'   tidy_and_attach() |>
#'   tidy_add_term_labels(
#'     interaction_sep = " x ",
#'     categorical_terms_pattern = "{level} / {reference_level}"
#'   )
#' }
tidy_add_term_labels <- function(x,
                                 labels = NULL,
                                 interaction_sep = " * ",
                                 categorical_terms_pattern = "{level}",
                                 relabel_poly = FALSE,
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
    cli::cli_abort("{.fn tidy_add_term_labels} cannot be applied after {.fn tidy_add_header_rows}.")
  }

  .attributes <- .save_attributes(x)

  if ("label" %in% names(x)) {
    x <- x |> dplyr::select(-dplyr::all_of("label"))
  }

  if (is.list(labels)) {
    labels <- unlist(labels)
  }

  if (!"var_label" %in% names(x)) {
    x <- x |> tidy_add_variable_labels(model = model)
  }
  if (!"contrasts" %in% names(x)) {
    x <- x |> tidy_add_contrasts(model = model)
  }

  # specific case for nnet::multinom
  # keeping only one level for computing term_labels
  if ("y.level" %in% names(x)) {
    xx <- x |>
      dplyr::distinct(.data$term, .keep_all = TRUE)
  } else {
    xx <- x
  }

  # start with term names
  term_labels <- unique(stats::na.omit(xx$term))
  names(term_labels) <- term_labels

  # add categorical terms levels
  sdif_term_level <- "diff"
  if (.attributes$exponentiate) sdif_term_level <- "ratio"

  terms_levels <- model |> model_list_terms_levels(
    label_pattern = categorical_terms_pattern,
    variable_labels = .attributes$variable_labels,
    sdif_term_level = sdif_term_level
  )
  if (!is.null(terms_levels)) {
    additional_term_labels <- terms_levels$label
    names(additional_term_labels) <- terms_levels$term
    term_labels <- term_labels |>
      .update_vector(additional_term_labels)

    # also consider "variablelevel" notation
    # when not already used (e.g. for sum contrasts)
    terms_levels2 <- terms_levels |>
      dplyr::mutate(term2 = paste0(.data$variable, .data$level)) |>
      dplyr::filter(.data$term2 != .data$term)
    if (nrow(terms_levels2) > 0) {
      additional_term_labels <- terms_levels2$label
      names(additional_term_labels) <- terms_levels2$term2
      term_labels <- term_labels |>
        .update_vector(additional_term_labels)
    }
    # also consider "variablelevel_rank" notation
    # for no intercept model (because type of interaction unknown)
    terms_levels3 <- terms_levels |>
      dplyr::mutate(term3 = paste0(.data$variable, .data$level_rank)) |>
      dplyr::filter(.data$term3 != .data$term & .data$contrasts_type == "no.contrast")
    if (nrow(terms_levels3) > 0) {
      additional_term_labels <- terms_levels3$label
      names(additional_term_labels) <- terms_levels3$term3
      term_labels <- term_labels |>
        .update_vector(additional_term_labels)
    }
  }

  # add variable labels
  # first variable list (for interaction only terms)
  # then current variable labels in x
  variables_list <- model_list_variables(model)
  if (!is.null(variables_list)) {
    variables_list <- variables_list |>
      dplyr::mutate(
        label = dplyr::if_else(
          is.na(.data$label_attr),
          .data$variable,
          as.character(.data$label_attr)
        ),
      )
    additional_term_labels <- variables_list$label
    names(additional_term_labels) <- variables_list$variable
    term_labels <- term_labels |>
      .update_vector(additional_term_labels)
    # add version with backtips for variables with non standard names
    names(additional_term_labels) <- paste0(
      "`", names(additional_term_labels), "`"
    )
    term_labels <- term_labels |>
      .update_vector(additional_term_labels)
  }

  x_var_labels <- xx |>
    dplyr::mutate(
      variable = dplyr::if_else(
        is.na(.data$variable), # for intercept
        .data$term,
        .data$variable
      )
    ) |>
    dplyr::group_by(.data$variable) |>
    dplyr::summarise(
      var_label = dplyr::first(.data$var_label),
      .groups = "drop_last"
    )
  additional_term_labels <- x_var_labels$var_label
  names(additional_term_labels) <- x_var_labels$variable
  term_labels <- term_labels |>
    .update_vector(additional_term_labels)
  # add version with backtips for variables with non standard names
  names(additional_term_labels) <- paste0(
    "`", names(additional_term_labels), "`"
  )
  term_labels <- term_labels |>
    .update_vector(additional_term_labels)


  # check if all elements of labels are in x
  # show a message otherwise
  not_found <- setdiff(names(labels), names(term_labels))
  if (length(not_found) > 0 && !quiet) {
    cli_alert_danger("{.code {not_found}} terms have not been found in {.code x}.")
  }
  if (length(not_found) > 0 && strict) {
    cli::cli_abort("Incorrect call with `labels=`. Quitting execution.", call = NULL)
  }

  # labels for polynomial terms
  if (relabel_poly) {
    poly_terms <- xx |>
      dplyr::filter(
        .data$term |> stringr::str_starts("poly\\(")
      ) |>
      dplyr::mutate(
        degree = .data$term |> stringr::str_replace("poly\\(.+\\)([0-9]+)", "\\1"),
        label = paste0(.data$var_label, .superscript_numbers(.data$degree))
      )
    poly_labels <- poly_terms$label
    names(poly_labels) <- poly_terms$term
    term_labels <- term_labels |>
      .update_vector(poly_labels)
  }

  # labels argument
  term_labels <- term_labels |>
    .update_vector(labels)
  # save custom labels
  .attributes$term_labels <- labels

  # management of interaction terms
  interaction_terms <- xx$term[!is.na(xx$var_type) & xx$var_type == "interaction"]
  # do not treat those specified in labels
  interaction_terms <- setdiff(interaction_terms, names(labels))
  names(interaction_terms) <- interaction_terms
  interaction_terms <-
    interaction_terms |>
    strsplit(":")

  # in some cases (e.g. marginal predictions)
  # interaction terms are not prefixed by variable names
  # => need to identify them from interaction_terms directly
  if (isTRUE(.attributes$find_missing_interaction_terms)) {
    it <- unname(unlist(interaction_terms))
    missing_terms <- setdiff(it[it != ""], names(term_labels))
    if (length(missing_terms) > 0) {
      names(missing_terms) <- missing_terms
      term_labels <- term_labels |>
        .update_vector(missing_terms)
    }
  }

  interaction_terms <- interaction_terms |>
    lapply(function(x) {
      paste(term_labels[x], collapse = interaction_sep)
    }) |>
    unlist()
  term_labels <- term_labels |>
    .update_vector(interaction_terms)

  x |>
    dplyr::left_join(
      tibble::tibble(
        term = names(term_labels),
        label = term_labels
      ),
      by = "term"
    ) |>
    tidy_attach_model(model = model, .attributes = .attributes)
}
