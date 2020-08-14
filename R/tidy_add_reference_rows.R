#' Add references rows for categorical variables
#'
#' For categorical variables with a treatment contrast
#' ([stats::contr.treatment()]), a SAS contrast ([stats::contr.SAS()])
#' or a sum contrast ([stats::contr.sum()]), add a reference row.
#'
#' @details
#' If the `contrasts` column is not yet available in `x`,
#' [tidy_add_contrasts()] will be automatically applied.
#' @param x a tidy tibble
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
#'   tidy_add_reference_rows()
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
#'     tidy_add_reference_rows()
#' }
tidy_add_reference_rows <- function(x, model = tidy_get_model(x)) {
  if (is.null(model))
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  x <- x %>% tidy_attach_model(model) # in case not already attached

  if ("reference_row" %in% names(x)) {
    warning("tidy_add_reference_rows() has already been applied. x has been returned unchanged.")
    return(x)
  }

  if (!"contrasts" %in% names(x))
    x <- x %>% tidy_add_contrasts()

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

  # contr.treatment -> add reference row before
  # base term needs to be taken into account
  if (any(!is.na(x$contrasts) & stringr::str_starts(x$contrasts, "contr.treatment"))) {
    ref_rows_before <- x %>%
      dplyr::filter(.data$contrasts %>% stringr::str_starts("contr.treatment")) %>%
      dplyr::group_by(.data$variable) %>%
      dplyr::summarise(
        var_class = dplyr::first(.data$var_class),
        var_type = dplyr::first(.data$var_type),
        contrasts = dplyr::first(.data$contrasts),
        rank = min(.data$rank) - .25,
        .groups = "drop_last"
      ) %>%
      dplyr::mutate(
        contr_base = stringr::str_replace(.data$contrasts, "contr.treatment\\(base=([0-9]+)\\)", "\\1"),
        contr_base = stringr::str_replace(.data$contr_base, "contr.treatment", "1"),
        contr_base = as.integer(.data$contr_base),
        rank = .data$rank + .data$contr_base - 1, # update position based on rank
        term = paste0(.data$variable, "_ref"),
        reference_row = TRUE
      ) %>%
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
        contrasts = dplyr::last(.data$contrasts),
        rank = max(.data$rank) + .25,
        .groups = "drop_last"
      ) %>%
      dplyr::mutate(
        term = paste0(.data$variable, "_ref"),
        reference_row = TRUE
      )
    x <- x %>%
      dplyr::bind_rows(ref_rows_after)
  }

  x %>%
    dplyr::arrange(.data$rank) %>%
    dplyr::select(-.data$rank)
}


