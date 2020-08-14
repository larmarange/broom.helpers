#' Add term labels
#'
#' Will add term labels in a `var_label` column, based on:
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
                                     model = tidy_get_model(x)
                                     ) {
  if (is.null(model))
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")

  if ("label" %in% names(x))
    x <- x %>% dplyr::select(-.data$label)

  if (is.list(labels))
    labels <- unlist(labels)

  if (!"var_label" %in% names(x))
    x <- x %>% tidy_add_variable_labels(model = model)
  if (!"contrasts" %in% names(x))
    x <- x %>% tidy_add_contrasts(model = model)

  # reference rows required for naming correctly categorical variables
  # will be removed eventually at the end
  with_reference_rows <- "reference_row" %in% names(x)
  if (!with_reference_rows)
    x <- x %>% tidy_add_reference_rows(model = model)

  # start with term names
  term_labels <- unique(stats::na.omit(x$term))
  names(term_labels) <- term_labels

  # check if all elements of labels are in x
  # show a message otherwise
  not_found <- setdiff(names(labels), names(term_labels))
  if (length(not_found) > 0)
    message(paste0(
      paste0('"', not_found, '"', collapse = ", "),
      " terms have not been found in \"x\"."
    ))

  # factor levels for categorical variables
  xlevels <- model_get_xlevels(model)
  for (v in names(xlevels)) {
    additional_term_labels <- xlevels[[v]]
    names(additional_term_labels) <- x$term[!is.na(x$variable) & x$variable == v]
    term_labels <- term_labels %>%
      .update_vector(additional_term_labels)
  }

  # variable label if term is equal to the variable
  additional_term_labels <-
    x$var_label[!is.na(x$term) & !is.na(x$variable) & x$term == x$variable]
  if (length(additional_term_labels) > 0) {
    names(additional_term_labels) <-
      x$term[!is.na(x$term) & !is.na(x$variable) & x$term == x$variable]
    term_labels <- term_labels %>%
      .update_vector(additional_term_labels)
  }

  # labels for polynomial terms
  poly_terms <- x %>%
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
  interaction_terms <- x$term[!is.na(x$var_type) & x$var_type == "interaction"]
  # do not treat those specified in labels
  interaction_terms <- setdiff(interaction_terms, names(labels))
  names(interaction_terms) <- interaction_terms
  interaction_terms <-
    interaction_terms %>%
    strsplit(":") %>%
    lapply(function(x){paste(term_labels[x], collapse = interaction_sep)}) %>%
    unlist()
  term_labels <- term_labels %>%
    .update_vector(interaction_terms)

  x <- x %>%
    dplyr::left_join(
      tibble::tibble(
        term = names(term_labels),
        label = term_labels
      ),
      by = "term"
    )

  if (!with_reference_rows)
    x <- x %>%
      dplyr::filter(is.na(.data$reference_row) | !.data$reference_row) %>%
      dplyr::select(-.data$reference_row)

  x %>%
    tidy_attach_model(model)
}


