test_that("select_helpers: .select_to_varnames", {
  expect_error(
    .select_to_varnames(mpg)
  )

  expect_equal(
    .select_to_varnames(select = vars(hp, mpg), data = mtcars),
    dplyr::select(mtcars, hp, mpg) |> colnames()
  )

  expect_equal(
    .select_to_varnames(select = mpg, data = mtcars),
    dplyr::select(mtcars, mpg) |> colnames()
  )

  expect_equal(
    .select_to_varnames(select = "mpg", data = mtcars),
    dplyr::select(mtcars, "mpg") |> colnames()
  )

  expect_equal(
    .select_to_varnames(select = c("hp", "mpg"), data = mtcars),
    dplyr::select(mtcars, c("hp", "mpg")) |> colnames()
  )

  expect_equal(
    .select_to_varnames(select = c(hp, mpg), data = mtcars),
    dplyr::select(mtcars, c(hp, mpg)) |> colnames()
  )

  expect_equal(
    .select_to_varnames(select = NULL, data = mtcars),
    NULL
  )

  expect_equal(
    .select_to_varnames(select = vars(dplyr::everything(), -mpg), data = mtcars),
    dplyr::select(mtcars, dplyr::everything(), -mpg) |> colnames()
  )
})

test_that("select_helpers: all_*()", {
  mod <- glm(response ~ age * trt + grade, gtsummary::trial, family = binomial)
  mod_tidy <- tidy_and_attach(mod)

  expect_equal(
    tidy_select_variables(mod_tidy, include = all_categorical())$variable |>
      na.omit() |>
      unique(),
    c("(Intercept)", "trt", "grade")
  )

  expect_equal(
    tidy_select_variables(mod_tidy, include = all_categorical(dichotomous = FALSE))$variable |>
      na.omit() |>
      unique(),
    c("(Intercept)", "grade")
  )

  expect_equal(
    tidy_select_variables(mod_tidy, include = all_continuous())$variable |>
      na.omit() |>
      unique(),
    c("(Intercept)", "age")
  )

  expect_equal(
    tidy_select_variables(mod_tidy, include = all_dichotomous())$variable |>
      na.omit() |>
      unique(),
    c("(Intercept)", "trt")
  )

  expect_equal(
    tidy_select_variables(mod_tidy, include = all_interaction())$variable |>
      na.omit() |>
      unique(),
    c("(Intercept)", "age:trt")
  )
})

test_that("select_helpers: tidy_plus_plus", {
  skip_on_cran()

  mod <- glm(response ~ age * trt + grade, gtsummary::trial, family = binomial)
  mod2 <- glm(response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(
      stage = contr.sum,
      grade = contr.poly,
      trt = contr.helmert
    )
  )
  mod3 <- glm(
    response ~ stage + grade + trt + factor(death),
    gtsummary::trial,
    family = binomial,
    contrasts = list(
      stage = contr.treatment(4, 3), grade = contr.treatment(3, 2),
      trt = contr.treatment(2, 2), "factor(death)" = matrix(c(-3, 2))
    )
  )

  expect_equal(
    tidy_plus_plus(mod3, include = all_contrasts("treatment"))$variable |>
      na.omit() |>
      unique(),
    c("stage", "grade", "trt")
  )

  expect_equal(
    tidy_plus_plus(mod3, include = all_contrasts("other"))$variable |>
      na.omit() |>
      unique(),
    c("factor(death)")
  )

  expect_equal(
    tidy_plus_plus(mod, include = all_contrasts())$variable |>
      na.omit() |>
      unique(),
    c("trt", "grade")
  )

  expect_equal(
    tidy_plus_plus(mod, include = all_categorical())$variable |>
      na.omit() |>
      unique(),
    c("trt", "grade")
  )

  expect_equal(
    tidy_plus_plus(mod, include = all_contrasts("treatment"))$variable |>
      na.omit() |>
      unique(),
    c("trt", "grade")
  )

  expect_equal(
    tidy_plus_plus(mod, include = all_continuous())$variable |>
      na.omit() |>
      unique(),
    c("age")
  )

  expect_equal(
    tidy_plus_plus(mod, include = all_dichotomous())$variable |>
      na.omit() |>
      unique(),
    c("trt")
  )

  expect_equal(
    tidy_plus_plus(mod, include = all_interaction())$variable |>
      na.omit() |>
      unique(),
    c("age:trt")
  )

  expect_equal(
    tidy_plus_plus(mod, include = all_intercepts(), intercept = TRUE)$variable |>
      na.omit() |>
      unique(),
    c("(Intercept)")
  )

  expect_equal(
    tidy_plus_plus(mod,
      add_header_rows = TRUE,
      show_single_row = all_dichotomous()
    )$variable %in% "trt" |>
      sum(),
    1L
  )

  skip_if_not_installed("emmeans")
  expect_equal(
    tidy_plus_plus(mod2, include = all_contrasts("sum"))$variable |>
      na.omit() |>
      unique(),
    c("stage")
  )

  expect_equal(
    tidy_plus_plus(mod2, include = all_contrasts("poly"))$variable |>
      na.omit() |>
      unique(),
    c("grade")
  )

  expect_equal(
    tidy_plus_plus(mod2, include = all_contrasts("helmert"))$variable |>
      na.omit() |>
      unique(),
    c("trt")
  )

  skip_on_cran()
  skip_if_not_installed("lme4")
  mod3 <- lme4::lmer(age ~ stage + (stage | grade) + (1 | grade), gtsummary::trial)
  res <- mod3 |> tidy_plus_plus(
    tidy_fun = broom.mixed::tidy,
    include = all_ran_pars()
  )
  expect_equal(
    res$term,
    c(
      "grade.sd__(Intercept)", "grade.cor__(Intercept).stageT2",
      "grade.cor__(Intercept).stageT3", "grade.cor__(Intercept).stageT4",
      "grade.sd__stageT2", "grade.cor__stageT2.stageT3", "grade.cor__stageT2.stageT4",
      "grade.sd__stageT3", "grade.cor__stageT3.stageT4", "grade.sd__stageT4",
      "grade.1.sd__(Intercept)", "Residual.sd__Observation"
    )
  )
  res <- mod3 |> tidy_plus_plus(
    tidy_fun = broom.mixed::tidy,
    include = all_ran_vals()
  )
  expect_equal(res |> nrow(), 0L)
})

test_that("select_helpers: tidy_add_header_rows", {
  mod <- glm(response ~ age * trt + grade, gtsummary::trial, family = binomial)
  mod_tidy <- tidy_and_attach(mod)

  expect_equal(
    tidy_add_header_rows(mod_tidy, show_single_row = all_dichotomous())$variable %in% "trt" |>
      sum(),
    1L
  )
})

test_that("select_helpers: tidy_add_variable_labels", {
  mod <- glm(response ~ age * trt + grade, gtsummary::trial, family = binomial)
  mod_tidy <- tidy_and_attach(mod)

  expect_error(
    tidy_add_variable_labels(mod_tidy, labels = where(is.numeric) ~ "NUMERIC"),
    NA
  )

  expect_equal(
    tidy_add_variable_labels(mod_tidy,
      labels = list(
        `(Intercept)` ~ "b0",
        age ~ "AGE",
        trt ~ "Drug",
        "grade" ~ "Grade",
        contains("age:") ~ "Interaction"
      )
    ) |>
      dplyr::pull(var_label) |>
      unique(),
    c("b0", "AGE", "Drug", "Grade", "Interaction")
  )
})

test_that("select_helpers: .select_to_varnames", {
  expect_error(
    .select_to_varnames(c(mpg, hp), data = mtcars, select_single = TRUE)
  )
})

test_that("select_helpers: .generic_selector ", {
  mod <- glm(response ~ age * trt + grade, gtsummary::trial, family = binomial)

  expect_error(
    tidy_and_attach(mod) |>
      tidy_identify_variables() |>
      tidy_add_variable_labels(labels = all_contrasts("helmert") ~ "HELMERT!")
  )

  expect_error(
    all_continuous()
  )

  expect_equal(
    .var_info_to_df(letters) |> names(),
    letters
  )
})

test_that("select_helpers: .formula_list_to_named_list ", {
  mod <- glm(response ~ age * trt + grade, gtsummary::trial, family = binomial)
  tidy_mod <- tidy_plus_plus(mod)

  expect_error(
    .formula_list_to_named_list(list(age ~ "Age", TRUE), var_info = tidy_mod)
  )

  expect_error(
    .formula_list_to_named_list(list(age ~ "Age"),
      var_info = tidy_mod,
      type_check = is.character
    ),
    NA
  )
  expect_error(
    .formula_list_to_named_list(list(age ~ "Age"),
      var_info = tidy_mod,
      type_check = is.logical
    )
  )
  expect_error(
    .formula_list_to_named_list(letters,
      var_info = tidy_mod,
      type_check = is.logical
    )
  )

  expect_equal(
    .formula_list_to_named_list(age ~ "Age", var_info = tidy_mod),
    list(age = "Age")
  )

  expect_equal(
    .formula_list_to_named_list(~"Age", var_info = tidy_mod),
    list(age = "Age", trt = "Age", grade = "Age", `age:trt` = "Age")
  )
  expect_equal(
    .formula_list_to_named_list(list(~"Age", `age:trt` = "interact"), var_info = tidy_mod),
    list(age = "Age", trt = "Age", grade = "Age", `age:trt` = "interact")
  )

  expect_error(
    .formula_list_to_named_list(list(age ~ "Age"),
      var_info = tidy_mod, type_check = is.logical,
      arg_name = "label"
    )
  )
  expect_error(
    .formula_list_to_named_list(list(age ~ "Age"),
      var_info = tidy_mod,
      type_check = is.logical, arg_name = "label",
      type_check_msg = "Age msg error"
    ),
    "Age msg error"
  )
  expect_error(
    .formula_list_to_named_list("Age",
      var_info = tidy_mod,
      type_check = rlang::is_string,
      arg_name = "label"
    ),
    "Did you mean `everything"
  )
  expect_error(
    .formula_list_to_named_list("Age",
      var_info = tidy_mod,
      type_check = rlang::is_string,
      arg_name = "label"
    ),
    "Age"
  )
  expect_error(
    .formula_list_to_named_list(~"Age",
      var_info = tidy_mod,
      type_check = rlang::is_string,
      arg_name = "label"
    ),
    NA
  )

  expect_error(
    .formula_list_to_named_list(list(age ~ NULL),
      var_info = tidy_mod,
      type_check = is.logical
    ),
    NA
  )
  expect_error(
    .formula_list_to_named_list(list(age ~ NULL),
      var_info = tidy_mod,
      type_check = is.logical, null_allowed = FALSE
    )
  )
  expect_error(
    select_test <-
      .formula_list_to_named_list(
        list(response = "Response", dplyr::contains("age") ~ "?AGE?", trt = NULL),
        data = gtsummary::trial,
        arg_name = "label",
        type_check = rlang::is_string,
        type_check_msg = NULL,
        null_allowed = TRUE
      ),
    NA
  )
  expect_equal(
    select_test,
    list(response = "Response", age = "?AGE?", stage = "?AGE?", trt = NULL)
  )

  expect_error(
    .formula_list_to_named_list(
      list(response = "Response", dplyr::contains("age") ~ "?AGE?", trt = NULL),
      data = gtsummary::trial,
      arg_name = "label",
      type_check = rlang::is_string,
      type_check_msg = NULL,
      null_allowed = FALSE
    )
  )

  expect_error(
    .formula_list_to_named_list(
      list(response = "Response", dplyr::contains("age") ~ "?AGE?", trt = NULL),
      data = gtsummary::trial,
      arg_name = "label",
      select_single = TRUE
    )
  )
  expect_error(
    .formula_list_to_named_list(
      list(response = "Response", dplyr::contains("age") ~ "?AGE?", trt = NULL),
      data = gtsummary::trial,
      select_single = TRUE
    )
  )
})


test_that("select_helpers: .scope_var_info", {
  mod_tidy <- lm(mpg ~ hp, mtcars) |> tidy_and_attach()

  # can scope a data frame with no variable
  expect_error(
    .scope_var_info(mod_tidy |> tidy_identify_variables()), NA
  )

  # no error when non-data frame is scoped
  expect_error(
    .scope_var_info(mod_tidy$term), NA
  )
})


test_that("select_helpers: .var_info_to_df ", {
  mod_tidy <- lm(mpg ~ hp, mtcars) |> tidy_and_attach()

  # can convert a tibble without a var_class column
  expect_error(
    .var_info_to_df(mod_tidy |> tidy_identify_variables() |> dplyr::select(-var_class)), NA
  )
})
