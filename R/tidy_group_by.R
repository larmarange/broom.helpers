#' Group results by selected columns
#'
#' Indicates that results should be grouped. By default
#' (`group_by = auto_group_by()`), results will be grouped according to the
#' `y.level` column (for multinomial models) or the `component` column
#' (multi-components models) if any.
#' @param x (`data.frame`)\cr
#' A tidy tibble as produced by `tidy_*()` functions.
#' @param group_by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#' One or several variables to group by. Default is `auto_group_by()`.
#' Use `NULL` to force ungrouping.
#' @param group_labels (`string`)\cr
#' An optional named vector of custom term labels.
#' @param model (a model object, e.g. `glm`)\cr
#' The corresponding model, if not attached to `x`.
#' @return
#' The `x` tibble with, if relevant, an additional `group_by` column.
#' @export
#' @examplesIf require("nnet")
#' mod <- multinom(Species ~ Petal.Width + Petal.Length, data = iris)
#' mod |> tidy_and_attach() |> tidy_group_by()
#'
#' mod |>
#'   tidy_and_attach() |>
#'   tidy_group_by(group_labels = c(versicolor = "harlequin blueflag"))
#'
#' mod |> tidy_and_attach() |> tidy_group_by(group_by = NULL)
#'
#' mod |>
#'   tidy_and_attach() |>
#'   tidy_identify_variables() |>
#'   tidy_group_by(group_by = variable)
#' @family tidy_helpers
tidy_group_by <- function(
    x,
    group_by = auto_group_by(),
    group_labels = NULL,
    model = tidy_get_model(x)) {
  if (is.null(model)) {
    cli::cli_abort(c(
      "{.arg model} is not provided.",
      "You need to pass it or to use {.fn tidy_and_attach}."
    ))
  }
  .attributes <- .save_attributes(x)

  # obtain character vector of selected variables
  group_vars <- x |> dplyr::select({{ group_by }}) |> colnames()

  # compute groups
  if (length(group_vars) > 0) {
    x <- x |>
      tidyr::unite(col = "group_by", dplyr::all_of(group_vars), remove = FALSE)
    groups <- unique(x$group_by)
    x$group_by <- factor(x$group_by, levels = groups)
    x <- x |> dplyr::arrange(group_by)

    # group labels
    if (!is.null(group_labels)) {
      if (is.null(names(group_labels)) || any(names(group_labels) == ""))
        cli::cli_abort("All elements of {.arg group_labels} should be named.")
      keep <- names(group_labels) %in% levels(x$group_by)
      drop <- group_labels[!keep]
      if (length(drop) > 0) {
        cli::cli_alert_warning(c(
          "Problem in {.arg group_labels}:\n",
          "value{?s} {.strong {drop}} not found in the data and ignored."
        ))
      }
      group_labels <- group_labels[keep]
      l <- levels(x$group_by)
      names(l) <- l
      l[names(group_labels)] <- group_labels
      levels(x$group_by) <- l
    }
  }
  if (length(group_vars) == 0 && "group_by" %in% names(x))
    x <- x |> dplyr::select(-.data$group_by)
  # sometimes, group_by not relevant after tidy_select_variable
  if ("group_by" %in% names(x) && all(x$group_by == ""))
    x <- x |> dplyr::select(-.data$group_by)
  x |>
    tidy_attach_model(model = model, .attributes = .attributes)
}

#' @rdname tidy_group_by
#' @export
auto_group_by <- function() {
  vars <- tidyselect::peek_vars()
  if ("group_by" %in% vars) # if already grouped, we keep it
    return("group_by")
  if ("y.level" %in% vars)
    return("y.level")
  if ("component" %in% vars)
    return("component")
  NULL
}
