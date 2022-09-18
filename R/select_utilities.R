#' Convert formula selector to a named list
#'
#' Functions takes a list of formulas, a named list, or a combination of named
#' elements with formula elements and returns a named list.
#' For example, `list(age = 1, starts_with("stage") ~ 2)`.
#'
#' @section Shortcuts:
#' A shortcut for specifying an option be applied to all columns/variables
#' is omitting the LHS of the formula.
#' For example, `list(~ 1)` is equivalent to passing `list(everything() ~ 1)`.
#'
#' Additionally, a single formula may be passed instead of placing a single
#' formula in a list; e.g. `everything() ~ 1` is equivalent to
#' passing `list(everything() ~ 1)`
#'
#' @param x list of selecting formulas
#' @param type_check A predicate function that checks the elements passed on
#' the RHS of the formulas in `x=` (or the element in a named list)
#' satisfy the function.
#' @param type_check_msg When the `type_check=` fails, the string provided
#' here will be printed as the error message. When `NULL`, a generic
#' error message will be printed.
#' @param null_allowed Are `NULL` values accepted for the right hand side of
#' formulas?
#' @inheritParams .select_to_varnames
#'
#' @export
.formula_list_to_named_list <- function(x, data = NULL, var_info = NULL,
                                        arg_name = NULL, select_single = FALSE,
                                        type_check = NULL, type_check_msg = NULL,
                                        null_allowed = TRUE) {
  # if NULL provided, return NULL ----------------------------------------------
  if (is.null(x)) {
    return(NULL)
  }

  # converting to list if single element passed --------------------------------
  if (inherits(x, "formula")) {
    x <- list(x)
  }

  # checking the input is valid ------------------------------------------------
  .check_valid_input(x = x, arg_name = arg_name, type_check = type_check)

  # convert to a named list ----------------------------------------------------
  len_x <- length(x)
  named_list <- vector(mode = "list", length = len_x)
  for (i in seq_len(len_x)) {
    if (rlang::is_named(x[i])) {
      named_list[i] <- list(x[i])
    }
    else if (rlang::is_formula(x[[i]])) {
      named_list[i] <-
        .single_formula_to_list(x[[i]],
                                data = data,
                                var_info = var_info,
                                arg_name = arg_name,
                                select_single = select_single,
                                type_check = type_check,
                                type_check_msg = type_check_msg,
                                null_allowed = null_allowed) %>%
        list()
    }
    else {
      .formula_select_error(arg_name = arg_name)
    }

    .rhs_checks(x = named_list[i][[1]], arg_name = arg_name, type_check = type_check,
                type_check_msg = type_check_msg, null_allowed = null_allowed)
  }
  named_list <- purrr::flatten(named_list)

  # removing duplicates (using the last one listed if variable occurs more than once)
  tokeep <- names(named_list) %>% rev() %>% {!duplicated(.)} %>% rev()
  result <- named_list[tokeep]

  if (isTRUE(select_single) && length(result) > 1) {
    .select_single_error_msg(names(result), arg_name = arg_name)
  }
  result
}

.select_single_error_msg <- function(selected, arg_name) {
  if (!rlang::is_empty(arg_name)) {
    stringr::str_glue(
      "Error in `{arg_name}=` argument--select only a single column. ",
      "The following columns were selected, ",
      "{paste(sQuote(selected), collapse = ', ')}") %>%
      stop(call. = FALSE)
  }
  stringr::str_glue(
    "Error in selector--select only a single column. ",
    "The following columns were selected, ",
    "{paste(sQuote(selected), collapse = ', ')}") %>%
    stop(call. = FALSE)
}

.check_valid_input <- function(x, arg_name, type_check) {
  if (!rlang::is_list(x) &&
      !(rlang::is_vector(x) && rlang::is_named(x))) {
    err_msg <-
      stringr::str_glue(
        "Error processing the `{arg_name %||% ''}` argument. ",
        "Expecting a list or formula.\n",
        "Review syntax details at",
        "'https://www.danieldsjoberg.com/gtsummary/reference/syntax.html'")
    if (tryCatch(do.call(type_check, list(x)), error = function(e) FALSE)) {
      x_string <-
        suppressWarnings(tryCatch(
          switch(rlang::is_string(x)) %||% as.character(deparse(x)),
          error = function(e) NULL
        ))
      if (!is.null(x_string) && length(x_string) == 1 && nchar(x_string) <= 50) {
        err_msg <-
          paste(
            err_msg,
            stringr::str_glue("Did you mean `everything() ~ {x_string}`?"),
            sep = "\n\n"
          )
      }
    }

    stop(err_msg, call. = FALSE)
  }

  return(invisible())
}

# checking the type/class/NULL of the RHS of formula
.rhs_checks <- function(x, arg_name,
                        type_check, type_check_msg,
                        null_allowed) {
  purrr::imap(
    x,
    function(rhs, lhs) {
      if (!null_allowed && is.null(rhs)){
        stringr::str_glue(
          "Error processing `{arg_name %||% ''}` argument for element '{lhs[[1]]}'. ",
          "A NULL value is not allowed."
        ) %>%
          stop(call. = FALSE)
      }

      # check the type of RHS ------------------------------------------------------
      if (!is.null(type_check) && !is.null(rhs) && !type_check(rhs)) {
        stringr::str_glue(
          "Error processing `{arg_name %||% ''}` argument for element '{lhs[[1]]}'. ",
          type_check_msg %||% "The value passed is not the expected type/class."
        ) %>%
          stop(call. = FALSE)
      }
    }
  )

  return(invisible())
}

.single_formula_to_list <- function(x, data, var_info, arg_name,
                                    select_single, type_check, type_check_msg,
                                    null_allowed) {
  # for each formula extract lhs and rhs ---------------------------------------
  # checking the LHS is not empty
  f_lhs_quo <- .f_side_as_quo(x, "lhs")
  if (rlang::quo_is_null(f_lhs_quo)) f_lhs_quo <- rlang::expr(everything())
  # extract LHS of formula
  lhs <- .select_to_varnames(select = !!f_lhs_quo,
                             data = data,
                             var_info = var_info,
                             arg_name = arg_name,
                             select_single = select_single)

  # evaluate RHS of formula in the original formula environment
  rhs <- .f_side_as_quo(x, "rhs") %>% rlang::eval_tidy()

  # checking if RHS is NULL ----------------------------------------------------


  # converting rhs and lhs into a named list
  purrr::map(lhs, ~ list(rhs) %>% rlang::set_names(.x)) %>%
    purrr::flatten()
}


#' Variable selector
#'
#' Function takes `select()`-like inputs and converts the selector to
#' a character vector of variable names. Functions accepts tidyselect syntax,
#' and additional selector functions defined within the package
#'
#' @param select A single object selecting variables, e.g. `c(age, stage)`,
#' `starts_with("age")`
#' @param data A data frame to select columns from. Default is NULL
#' @param var_info A data frame of variable names and attributes. May also pass
#' a character vector of variable names. Default is NULL
#' @param arg_name Optional string indicating the source argument name. This
#' helps in the error messaging. Default is NULL.
#' @param select_single Logical indicating whether the result must be a single
#' variable. Default is `FALSE`
#'
#' @return A character vector of variable names
#' @export
.select_to_varnames <- function(select, data = NULL, var_info = NULL,
                                arg_name = NULL, select_single = FALSE) {
  if (is.null(data) && is.null(var_info))
    stop("At least one of `data=` and `var_info=` must be specified.")

  select <- rlang::enquo(select)

  # if NULL passed, return NULL
  if (rlang::quo_is_null(select)) return(NULL)

  # convert var_info to data frame if data not provided ------------------------
  if (is.null(data)) data <- .var_info_to_df(var_info)

  if (!is.null(var_info)) {
    # scoping the variable types
    .scope_var_info(var_info)
    # un-scoping on exit
    on.exit(rm(list = ls(envir = env_variable_type), envir = env_variable_type))
  }

  # determine if selecting input begins with `var()`
  select_input_starts_var <-
    !rlang::quo_is_symbol(select) && # if not a symbol (ie name)
    tryCatch(identical(
      eval(as.list(rlang::quo_get_expr(select)) %>% purrr::pluck(1)),
      dplyr::vars),
      error = function(e) FALSE)

  # performing selecting
  res <-
    tryCatch({
      if (select_input_starts_var) {
        # `vars()` was deprecated on June 6, 2022, gtsummary will stop
        # exporting `vars()` at some point as well.
        paste("Use of {.code vars()} is now {.strong deprecated} and support will soon be removed.",
              "Please replace calls to {.code vars()} with {.code c()}.") %>%
          cli::cli_alert_warning()

        # `vars()` evaluates to a list of quosures; unquoting them in `select()`
        names(dplyr::select(data, !!!rlang::eval_tidy(select)))
      }
      else {
        names(dplyr::select(data, !!select))
      }
    },
    error = function(e) {
      if (!is.null(arg_name))
        error_msg <- stringr::str_glue("Error in `{arg_name}=` argument input. Select from ",
                                       "{paste(sQuote(names(data)), collapse = ', ')}")
      else error_msg <- as.character(e) # nocov
      stop(error_msg, call. = FALSE)
    })

  # assuring only a single column is selected
  if (select_single == TRUE && length(res) > 1) {
    .select_single_error_msg(res, arg_name = arg_name)
  }

  # if nothing is selected, return a NULL
  if (length(res) == 0) return(NULL)

  res
}


#' Generate a custom selector function
#'
#' @param variable_column string indicating column variable names are stored
#' @param select_column character vector of columns used in the `select_expr=` argument
#' @param select_expr unquoted predicate command to subset a data frame to select variables
#' @param fun_name quoted name of function where `.generic_selector()` is being used.
#' This helps with error messaging.
#'
#' @details
#' `.is_selector_scoped()` checks if a selector has been properly registered
#' in `env_variable_type$df_var_info`.
#'
#' @return custom selector functions
#' @export
.generic_selector <- function(variable_column, select_column, select_expr, fun_name) {
  # ensuring the proper data has been scoped to use this function
  if (!.is_selector_scoped(variable_column, select_column)) {
    cli_alert_danger("Cannot use selector '{fun_name}()' in this context.")
    stop("Invalid syntax", call. = FALSE)
  }

  # selecting the variable from the variable information data frame
  env_variable_type$df_var_info %>%
    dplyr::select(all_of(c(variable_column, select_column))) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::filter({{ select_expr }}) %>%
    dplyr::pull(dplyr::all_of(variable_column)) %>%
    unique()
}

#' @rdname dot-generic_selector
#' @export
.is_selector_scoped <- function(variable_column, select_column) {
  exists("df_var_info", envir = env_variable_type) &&
    all(c(variable_column, select_column) %in% names(env_variable_type$df_var_info))
}

# scoping the variable characteristics
.scope_var_info <- function(x) {
  # removing everything from selecting environment
  rm(list = ls(envir = env_variable_type), envir = env_variable_type)
  if (!inherits(x, "data.frame")) return(invisible(NULL))

  # saving var_info to selecting environment, where it may be utilized by selecting fns
  env_variable_type$df_var_info <- x

  return(invisible(NULL))
}

# function that converts a meta_data tibble to a tibble of variable names (to be used in selecting)
.var_info_to_df <- function(x) {
  # converting variable name and class into data frame so users can use `where(predicate)`-types
  if (inherits(x, "data.frame") && all(c("variable", "var_class") %in% names(x))) {
    # keep unique var names
    x <-
      dplyr::select(x, all_of(c("variable", "var_class"))) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(.data$variable))
    df <-
      purrr::map2_dfc(
        x$variable, x$var_class,
        function(var, class) {
          switch(
            class,
            "numeric" = data.frame(pi),
            "character" = data.frame(letters[1]),
            "factor" = data.frame(datasets::iris$Species[1]),
            "ordered" = data.frame(factor(datasets::iris$Species[1], ordered = TRUE)),
            "integer" = data.frame(1L),
            "Date" = data.frame(Sys.Date()),
            "POSIXlt" = data.frame(as.POSIXlt(Sys.Date())),
            "POSIXct" = data.frame(as.POSIXct(Sys.Date())),
            "difftime" = data.frame(Sys.Date() - Sys.Date())
          ) %||%
            data.frame(NA) %>%
            purrr::set_names(var)
        }
      )
  }
  # if a data.frame
  else if (inherits(x, "data.frame") && "variable" %in% names(x)) {
    df <- purrr::map_dfc(unique(x$variable), ~data.frame(NA) %>% purrr::set_names(.x))
  }
  # if only a vector of names were passed, converting them to a data frame
  else if (rlang::is_vector(x) && !is.list(x)) {
    df <- purrr::map_dfc(unique(x), ~data.frame(NA) %>% purrr::set_names(.x))
  }
  # return data frame with variables as column names
  df
}

# extract LHS/RHS of formula as quosure. attached env will be the formula env
.f_side_as_quo <- function(x, side = c("lhs", "rhs")) {
  side <- match.arg(side)
  f_expr <-
    switch(side,
           "lhs" = rlang::f_lhs(x),
           "rhs" = rlang::f_rhs(x))
  f_quo <- rlang::quo(!!f_expr)
  attr(f_quo, ".Environment") <- rlang::f_env(x)
  f_quo
}

# there are a couple of places the users input may result in an error.
# this function prints an informative error msg with correct syntax example
.formula_select_error <- function(arg_name) {
  example_text <- formula_select_examples[[arg_name %||% "not_an_arg"]] %||%
    paste(c("label = list(age ~ \"Age, years\")",
            "statistic = list(all_continuous() ~ \"{mean} ({sd})\")",
            "type = list(c(response, death) ~ \"categorical\")"))

  # printing error for argument input
  if (!is.null(arg_name))
    cli_alert_danger(
      "There was a problem with the {.code {arg_name}=} argument input.")
  else
    cli_alert_danger("There was a problem with one of the function argument inputs.")
  cli_alert_info("Below is an example of correct syntax.")
  cli_code(example_text)
  stop("Invalid argument syntax", call. = FALSE)
}

formula_select_examples <- list(
  labels = "labels = list(age ~ \"Age, years\", response ~ \"Tumor Response\")",
  label = "label = list(age ~ \"Age, years\", response ~ \"Tumor Response\")",
  type = "type = list(age ~ \"continuous\", where(is.integer) ~ \"categorical\")",
  statistic = c("statistic = list(all_continuous() ~ \"{mean} ({sd})\", all_categorical() ~ \"{n} / {N} ({p}%)\")",
                "statistic = list(age ~ \"{median}\")"),
  digits = c("digits = list(age ~ 2)", "digits = list(all_continuous() ~ 2)"),
  value = c("value = list(grade ~ \"III\")", "value = list(all_logical() ~ FALSE)"),
  test = c("test = list(all_continuous() ~ \"t.test\")", "test = list(age ~ \"kruskal.test\")")
)

# set new environment for new tidyselect funs
env_variable_type <- rlang::new_environment()

