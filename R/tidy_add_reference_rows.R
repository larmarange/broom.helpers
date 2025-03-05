#' Add references rows for categorical variables
#'
#' For categorical variables with a treatment contrast
#' ([stats::contr.treatment()]), a SAS contrast ([stats::contr.SAS()])
#' a sum contrast ([stats::contr.sum()]), or successive differences contrast
#' ([MASS::contr.sdif()]) add a reference row.
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
#' rather than before. Similarly, it is better to apply
#' `tidy_add_reference_rows()` before [tidy_add_n()].
#' @param x (`data.frame`)\cr
#' A tidy tibble as produced by `tidy_*()` functions.
#' @param no_reference_row ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#' Variables for those no reference row should be added.
#' See also [all_categorical()] and [all_dichotomous()].
#' @param model (a model object, e.g. `glm`)\cr
#' The corresponding model, if not attached to `x`.
#' @inheritParams tidy_plus_plus
#' @export
#' @family tidy_helpers
#' @examplesIf .assert_package("gtsummary", boolean = TRUE)
#' \donttest{
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
#'     tidy_and_attach()
#'   res |> tidy_add_reference_rows()
#'   res |> tidy_add_reference_rows(no_reference_row = all_dichotomous())
#'   res |> tidy_add_reference_rows(no_reference_row = "Class")
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
#'     tidy_add_reference_rows()
#' }
tidy_add_reference_rows <- function(
    x, no_reference_row = NULL,
    model = tidy_get_model(x),
    quiet = FALSE) {
  if (is.null(model)) {
    cli::cli_abort(c(
      "{.arg model} is not provided.",
      "You need to pass it or to use {.fn tidy_and_attach}."
    ))
  }

  .attributes <- .save_attributes(x)

  # adding reference rows is not meaningful for stats::aov
  if (inherits(model, "aov")) {
    return(x |> dplyr::mutate(reference_row = NA))
  }
  # checking cases where adding reference rows is not meaningful
  if (isTRUE(.attributes$skip_add_reference_rows)) {
    return(x |> dplyr::mutate(reference_row = NA))
  }

  if ("header_row" %in% names(x)) {
    cli::cli_abort(paste(
      "{.fn tidy_add_reference_rows} cannot be applied",
      "after {.fn tidy_add_header_rows}."
    ))
  }

  if ("reference_row" %in% names(x)) {
    if (!quiet) {
      cli_alert_danger(paste(
        "{.code tidy_add_reference_rows()} has already been applied.",
        "x has been returned unchanged."
      ))
    }
    return(x)
  }

  if ("label" %in% names(x)) {
    if (!quiet) {
      cli_alert_info(paste0(
        "tidy_add_reference_rows() has been applied after tidy_add_term_labels().\n",
        "You should consider applying tidy_add_reference_rows() first."
      ))
    }
  }

  if ("n_obs" %in% names(x)) {
    if (!quiet) {
      cli_alert_info(paste0(
        "{.code tidy_add_reference_rows()} has been applied after {.code tidy_add_n()}.\n",
        "You should consider applying {.code tidy_add_reference_rows()} first."
      ))
    }
  }

  if (!"contrasts" %in% names(x)) {
    x <- x |> tidy_add_contrasts(model = model)
  }

  # obtain character vector of selected variables
  cards::process_selectors(
    data = scope_tidy(x),
    no_reference_row = {{ no_reference_row }}
  )

  terms_levels <- model_list_terms_levels(model)

  if (!is.null(terms_levels)) {
    terms_levels <- terms_levels |>
      # keep only terms corresponding to variable in x
      # (e.g. to exclude interaction only variables)
      dplyr::filter(
        .data$variable %in% unique(stats::na.omit(x$variable)) &
          # and exclude variables in no_reference_row
          !.data$variable %in% no_reference_row
      )
  }

  if (is.null(terms_levels) || nrow(terms_levels) == 0) {
    return(
      x |>
        dplyr::mutate(reference_row = NA) |>
        tidy_attach_model(model)
    )
  }

  terms_levels <- terms_levels |>
    dplyr::group_by(.data$variable) |>
    dplyr::mutate(rank = seq_len(dplyr::n()))

  has_var_label <- "var_label" %in% names(x)
  if (!has_var_label) {
    x$var_label <- NA_character_
  } # temporary populate it

  has_instrumental <- "instrumental" %in% names(x)
  if (!has_instrumental) {
    x$instrumental <- NA
  } # temporary populate it

  x <- x |>
    dplyr::mutate(
      reference_row = dplyr::if_else(
        .data$variable %in% unique(terms_levels$variable),
        FALSE,
        NA
      ),
      rank = seq_len(dplyr::n()) # for sorting table at the end
    )

  group <- NULL
  if ("component" %in% names(x)) {
    group <- "component"
  }
  if ("y.level" %in% names(x)) {
    group <- "y.level"
  }
  if (!is.null(group)) {
    x$.group_by_var <- x[[group]]
  } else {
    x$.group_by_var <- ""
  }

  ref_rows <- terms_levels |>
    dplyr::filter(.data$reference) |>
    dplyr::mutate(reference_row = TRUE) |>
    dplyr::select(
      dplyr::all_of(
        c("term", "variable", "label", "reference_row", "rank")
      )
    )

  if (!"label" %in% names(x)) {
    ref_rows <- ref_rows |> dplyr::select(-all_of("label"))
  }

  # populate effect column for mixed models
  tmp <- x
  if (!"effect" %in% names(x)) {
    tmp$effect <- NA_character_
  }

  var_summary <- tmp |>
    dplyr::group_by(.data$.group_by_var, .data$variable) |>
    dplyr::summarise(
      var_class = dplyr::first(.data$var_class),
      var_type = dplyr::first(.data$var_type),
      var_label = dplyr::first(.data$var_label),
      instrumental = dplyr::first(.data$instrumental),
      var_nlevels = dplyr::first(.data$var_nlevels),
      effect = dplyr::first(.data$effect),
      contrasts = dplyr::first(.data$contrasts),
      contrasts_type = dplyr::first(.data$contrasts_type),
      var_min_rank = min(.data$rank),
      var_max_rank = min(.data$rank),
      .groups = "drop_last"
    )

  ref_rows <- ref_rows |>
    dplyr::left_join(
      var_summary,
      by = "variable"
    ) |>
    dplyr::mutate(
      rank = .data$var_min_rank - 1.25 + .data$rank,
      # if last, reduce by .5 to avoid overlap with next variable
      rank = dplyr::if_else(
        .data$rank > .data$var_max_rank,
        .data$rank - .5,
        .data$rank
      )
    ) |>
    dplyr::select(-dplyr::all_of(c("var_min_rank", "var_max_rank")))

  if (!"effect" %in% names(x)) {
    ref_rows <- ref_rows |> dplyr::select(-dplyr::all_of("effect"))
  }

  x <- x |>
    dplyr::bind_rows(ref_rows)
  if (!is.null(group)) {
    x[[group]] <- x$.group_by_var
  }
  x <- x |> dplyr::select(-dplyr::all_of(".group_by_var"))

  if (!has_var_label) {
    x <- x |> dplyr::select(-dplyr::all_of("var_label"))
  }

  if (!has_instrumental) {
    x <- x |> dplyr::select(-dplyr::all_of("instrumental"))
  }

  x |>
    dplyr::arrange(.data$rank) |>
    dplyr::select(-dplyr::all_of("rank")) |>
    tidy_attach_model(model = model, .attributes = .attributes)
}
