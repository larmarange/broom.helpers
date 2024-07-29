#' List levels of categorical terms
#'
#' Only for categorical variables with treatment,
#' SAS, sum or successive differences contrasts (cf. [MASS::contr.sdif()]), and
#' categorical variables with no contrast.
#'
#' @param model a model object
#' @param label_pattern a [glue pattern][glue::glue()] for term labels (see examples)
#' @param variable_labels an optional named list or named vector of
#' custom variable labels passed to [model_list_variables()]
#' @param sdif_term_level for successive differences contrasts, how should term
#' levels be named? `"diff"` for `"B - A"` (default), `"ratio"` for `"B / A"`
#' @return
#' A tibble with ten columns:
#' * `variable`: variable
#' * `contrasts_type`: type of contrasts ("sum" or "treatment")
#' * `term`: term name
#' * `level`: term level
#' * `level_rank`: rank of the level
#' * `reference`: logical indicating which term is the reference level
#' * `reference_level`: level of the reference term
#' * `var_label`: variable label obtained with [model_list_variables()]
#' * `var_nlevels`: number of levels in this variable
#' * `dichotomous`: logical indicating if the variable is dichotomous
#' * `label`: term label (by default equal to term level)
#' The first nine columns can be used in `label_pattern`.
#' @export
#' @family model_helpers
#' @examples
#' glm(
#'   am ~ mpg + factor(cyl),
#'   data = mtcars,
#'   family = binomial,
#'   contrasts = list(`factor(cyl)` = contr.sum)
#' ) |>
#'   model_list_terms_levels()
#'
#' df <- Titanic |>
#'   dplyr::as_tibble() |>
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#'
#' mod <- df |>
#'   glm(
#'     Survived ~ Class + Age + Sex,
#'     data = ., weights = .$n, family = binomial,
#'     contrasts = list(Age = contr.sum, Class = "contr.helmert")
#'   )
#' mod |> model_list_terms_levels()
#' mod |> model_list_terms_levels("{level} vs {reference_level}")
#' mod |> model_list_terms_levels("{variable} [{level} - {reference_level}]")
#' mod |> model_list_terms_levels(
#'   "{ifelse(reference, level, paste(level, '-', reference_level))}"
#' )
model_list_terms_levels <- function(
    model,
    label_pattern = "{level}",
    variable_labels = NULL,
    sdif_term_level = c("diff", "ratio")) {
  UseMethod("model_list_terms_levels")
}

#' @export
#' @rdname model_list_terms_levels
model_list_terms_levels.default <- function(
    model, label_pattern = "{level}",
    variable_labels = NULL,
    sdif_term_level = c("diff", "ratio")) {
  contrasts_list <- model_list_contrasts(model)
  if (is.null(contrasts_list)) {
    return(NULL)
  }

  sdif_term_level <- match.arg(sdif_term_level)

  contrasts_list <- contrasts_list |>
    # keep only treatment, SAS and sum contrasts
    dplyr::filter(
      .data$contrasts |>
        stringr::str_starts("contr.treatment|contr.SAS|contr.sum|no.contrast|contr.sdif")
    )
  xlevels <- model_get_xlevels(model)

  if (nrow(contrasts_list) == 0 || length(xlevels) == 0) {
    return(NULL)
  }

  model_terms <- model_identify_variables(model) |>
    dplyr::filter(!is.na(.data$variable))

  if (nrow(model_terms) == 0) {
    return(NULL)
  }

  res <- dplyr::tibble()

  for (v in contrasts_list$variable) {
    if (v %in% names(xlevels)) {
      contrasts_type <- contrasts_list$contrasts_type[contrasts_list$variable == v]
      terms_levels <- xlevels[[v]]

      observed_terms <- model_terms$term[model_terms$variable == v]
      ref <- contrasts_list$reference[contrasts_list$variable == v]

      # terms could be named according to two approaches
      # plus variations with backticks
      s <- seq(1, length(terms_levels))
      terms_names1 <- paste0(v, terms_levels)
      terms_names2 <- paste0(v, s)
      terms_names1b <- paste0("`", v, "`", terms_levels)
      terms_names2b <- paste0("`", v, "`", s)
      # naming approach for contr.sdif
      terms_names3 <- paste0(v, terms_levels, "-", dplyr::lag(terms_levels))
      terms_names3 <- terms_names3[-1]
      terms_names3b <- paste0("`", v, "`", terms_levels, "-", dplyr::lag(terms_levels))
      terms_names3b <- terms_names3b[-1]
      terms_names4 <- paste0(v, s, "-", dplyr::lag(s))
      terms_names4 <- terms_names4[-1]
      terms_names4b <- paste0("`", v, "`", s, "-", dplyr::lag(s))
      terms_names4b <- terms_names4b[-1]

      # identification of the naming approach
      approach <- NA
      if (length(observed_terms) && !is.na(ref)) {
        approach <- dplyr::case_when(
          all(observed_terms %in% terms_names1[-ref]) ~ "1",
          all(observed_terms %in% terms_names2[-ref]) ~ "2",
          all(observed_terms %in% terms_names3[-ref]) ~ "3",
          all(observed_terms %in% terms_names4[-ref]) ~ "4",
          all(observed_terms %in% terms_names1b[-ref]) ~ "1b",
          all(observed_terms %in% terms_names2b[-ref]) ~ "2b",
          all(observed_terms %in% terms_names3b[-ref]) ~ "3b",
          all(observed_terms %in% terms_names4b[-ref]) ~ "4b"
        )
      }
      if (length(observed_terms) && is.na(ref)) {
        approach <- dplyr::case_when(
          all(observed_terms %in% terms_names1) ~ "1",
          all(observed_terms %in% terms_names2) ~ "2",
          all(observed_terms %in% terms_names3) ~ "3",
          all(observed_terms %in% terms_names4) ~ "4",
          all(observed_terms %in% terms_names1b) ~ "1b",
          all(observed_terms %in% terms_names2b) ~ "2b",
          all(observed_terms %in% terms_names3b) ~ "3b",
          all(observed_terms %in% terms_names4b) ~ "4b"
        )
      }
      # case of an interaction term only
      if (is.na(approach)) {
        n1 <- .count_term(model_terms$term, terms_names1)
        n2 <- .count_term(model_terms$term, terms_names2)
        n1b <- .count_term(model_terms$term, terms_names1b)
        n2b <- .count_term(model_terms$term, terms_names2b)
        approach <- dplyr::case_when(
          (n1b + n2b) > (n1 + n2) & n1b >= n2b ~ "1b",
          (n1b + n2b) > (n1 + n2) & n1b < n2b ~ "2b",
          n2 > n1 ~ "2",
          TRUE ~ "1"
        )
      }

      terms_names <- switch(
        approach,
        "1" = terms_names1,
        "2" = terms_names2,
        "3" = terms_names3,
        "4" = terms_names4,
        "1b" = terms_names1b,
        "2b" = terms_names2b,
        "3b" = terms_names3b,
        "4b" = terms_names4b
      )

      if (approach %in% c("3", "3b", "4", "4b")) {
        sep <- "-"
        if (sdif_term_level == "ratio") sep <- "/"
        tl <- terms_levels
        terms_levels <- paste(tl, sep, dplyr::lag(tl))
        terms_levels <- terms_levels[-1]
      }

      res <- dplyr::bind_rows(
        res,
        dplyr::tibble(
          variable = v,
          contrasts_type = contrasts_type,
          term = terms_names,
          level = terms_levels,
          level_rank = seq(1, length(terms_levels)),
          reference = seq(1, length(terms_levels)) == ref,
          reference_level = terms_levels[ref]
        )
      )
    }
  }

  res |>
    dplyr::left_join(
      model |>
        model_list_variables(labels = variable_labels) |>
        dplyr::select(all_of(c("variable", "var_label"))),
      by = "variable"
    ) |>
    dplyr::left_join(
      model |>
        model_get_nlevels() |>
        dplyr::select(all_of(c("variable", "var_nlevels"))),
      by = "variable"
    ) |>
    dplyr::mutate(
      dichotomous = .data$var_nlevels == 2,
      label = stringr::str_glue_data(res, label_pattern)
    )
}

# count the total number of times where elements of searched
# are found in observed terms
.count_term <- function(observed, searched) {
  total <- 0
  for (i in searched) {
    total <-
      total +
      stringr::str_count(
        observed,
        paste0("(^|:)", .escape_regex(i), "(:|$)")
      ) |>
      sum()
  }
  total
}
