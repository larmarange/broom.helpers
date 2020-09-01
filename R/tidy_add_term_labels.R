#' Add term labels
#'
#' Will add term labels in a `label` column, based on:
#' 1. labels provided in `labels` argument if provided;
#' 2. factor levels for categorical variables coded with
#'    treatment, SAS or sum contrasts;
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
#' @param x a tidy tibble
#' @param labels an optional named list or named vector of
#' custom term labels
#' @param interaction_sep separator for interaction terms
#' @param model the corresponding model, if not attached to `x`
#' @inheritParams tidy_plus_plus
#' @export
#' @family tidy_helpers
#' @examples
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) %>%
#'   labelled::set_variable_labels(
#'     Class = "Passenger's class",
#'     Sex = "Sex"
#'   )
#'
#' df %>%
#'   glm(Survived ~ Class * Age * Sex, data = ., weights = .$n, family = binomial) %>%
#'   tidy_and_attach() %>%
#'   tidy_add_term_labels()
tidy_add_term_labels <- function(x,
                                 labels = NULL,
                                 interaction_sep = " * ",
                                 model = tidy_get_model(x),
                                 quiet = FALSE) {
  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  if ("header_row" %in% names(x)) {
    stop("`tidy_add_term_labels()` cannot be applied after `tidy_add_header_rows().`")
  }

  if ("label" %in% names(x)) {
    x <- x %>% dplyr::select(-.data$label)
  }

  if (is.list(labels)) {
    labels <- unlist(labels)
  }

  if (!"var_label" %in% names(x)) {
    x <- x %>% tidy_add_variable_labels(model = model, quiet = quiet)
  }
  if (!"contrasts" %in% names(x)) {
    x <- x %>% tidy_add_contrasts(model = model)
  }

  # reference rows required for naming correctly categorical variables
  # will be removed eventually at the end
  if ("reference_row" %in% names(x)) {
    xx <- x
  } else {
    xx <- x %>% tidy_add_reference_rows(model = model)
  }

  # specific case for nnet::multinom
  # keeping only one level for computing term_labels
  if ("y.level" %in% names(x)) {
    xx <- xx %>%
      dplyr::filter(.data$y.level == x$y.level[1])
  }

  # start with term names
  term_labels <- unique(stats::na.omit(xx$term))
  names(term_labels) <- term_labels

  # check if all elements of labels are in x
  # show a message otherwise
  not_found <- setdiff(names(labels), names(term_labels))
  if (length(not_found) > 0 && !quiet) {
    usethis::ui_oops(paste0(
      usethis::ui_code(not_found),
      " terms have not been found in ",
      usethis::ui_code("x"),
      "."
    ))
  }

  # factor levels for categorical variables
  xlevels <- model_get_xlevels(model)
  for (v in names(xlevels)) {
    if (v %in% unique(xx$variable)) {
      var_contrasts <- xx$contrasts[!is.na(xx$variable) & xx$variable == v][1]
      # add xlevels only if treatment or sum contrasts
      if (var_contrasts %>% stringr::str_starts("contr.treatment|contr.SAS|contr.sum")) {
        additional_term_labels <- xlevels[[v]]
        names(additional_term_labels) <- xx$term[!is.na(xx$variable) & xx$variable == v]
        term_labels <- term_labels %>%
          .update_vector(additional_term_labels)
      }
    }
  }

  # variable label if term is equal to the variable
  additional_term_labels <-
    xx$var_label[!is.na(xx$term) & !is.na(xx$variable) & xx$term == xx$variable]
  if (length(additional_term_labels) > 0) {
    names(additional_term_labels) <-
      xx$term[!is.na(xx$term) & !is.na(xx$variable) & xx$term == xx$variable]
    term_labels <- term_labels %>%
      .update_vector(additional_term_labels)
  }

  # labels for polynomial terms
  poly_terms <- xx %>%
    dplyr::filter(.data$term %>% stringr::str_starts("poly\\(")) %>%
    dplyr::mutate(
      degree = .data$term %>% stringr::str_replace("poly\\(.+\\)([0-9]+)", "\\1"),
      label = paste0(.data$var_label, .superscript_numbers(.data$degree))
    )
  poly_labels <- poly_terms$label
  names(poly_labels) <- poly_terms$term
  term_labels <- term_labels %>%
    .update_vector(poly_labels)

  # labels argument
  term_labels <- term_labels %>%
    .update_vector(labels)

  # management of interaction terms
  interaction_terms <- xx$term[!is.na(xx$var_type) & xx$var_type == "interaction"]
  # do not treat those specified in labels
  interaction_terms <- setdiff(interaction_terms, names(labels))
  names(interaction_terms) <- interaction_terms
  interaction_terms <-
    interaction_terms %>%
    strsplit(":") %>%
    lapply(function(x) {
      paste(term_labels[x], collapse = interaction_sep)
    }) %>%
    unlist()
  term_labels <- term_labels %>%
    .update_vector(interaction_terms)

  x %>%
    dplyr::left_join(
      tibble::tibble(
        term = names(term_labels),
        label = term_labels
      ),
      by = "term"
    ) %>%
    tidy_attach_model(model = model) %>%
    .order_tidy_columns()
}
