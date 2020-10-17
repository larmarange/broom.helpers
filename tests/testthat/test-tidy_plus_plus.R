test_that("tidy_plus_plus() works for basic models", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  expect_error(
    mod %>% tidy_plus_plus(add_header_rows = TRUE, include = c(stage, grade)),
    NA
  )

  # combining custom variable labels with categorical_terms_pattern
  # check that the custom variable labels are passed to model_list_terms_levels()
  res <- mod %>%
    tidy_plus_plus(
      variable_labels = c(grade = "custom"),
      add_reference_rows = FALSE,
      categorical_terms_pattern = "{var_label}:{level}/{reference_level}"
    )
  expect_equivalent(
    res$label,
    c("T Stage:T2/T1", "T Stage:T3/T1", "T Stage:T4/T1", "custom:II/I",
      "custom:III/I", "Chemotherapy Treatment:Drug B/Drug A")
  )
})


test_that("tidy_plus_plus() and functionnal programming", {
  # works with glm
  expect_error(
    res <- dplyr::tibble(grade = c("I", "II", "III")) %>%
      dplyr::mutate(df_model = purrr:::map(grade, ~ gtsummary::trial %>% dplyr::filter(grade == ..1))) %>%
      dplyr::mutate(
        mv_formula_char = "response ~ trt + age + marker",
        mv_formula = purrr::map(mv_formula_char, ~ as.formula(.x)),
        mv_model_form =
          purrr::map2(
            mv_formula, df_model,
            ~ glm(..1, data = ..2)
          ),
        mv_tbl_form =
          purrr::map(
            mv_model_form,
            ~ tidy_plus_plus(..1, exponentiate = TRUE, add_header_rows = TRUE)
          )
      ),
    NA
  )

  # for coxph, identification of variables will not work
  # will display a message
  # but a result should be returned
  expect_message(
    res <- dplyr::tibble(grade = c("I", "II", "III")) %>%
      dplyr::mutate(df_model = purrr:::map(grade, ~ gtsummary::trial %>% dplyr::filter(grade == ..1))) %>%
      dplyr::mutate(
        mv_formula_char = "survival::Surv(ttdeath, death) ~ trt + age + marker",
        mv_formula = purrr::map(mv_formula_char, ~ as.formula(.x)),
        mv_model_form =
          purrr::map2(
            mv_formula, df_model,
            ~ survival::coxph(..1, data = ..2)
          ),
        mv_tbl_form =
          purrr::map(
            mv_model_form,
            ~ tidy_plus_plus(..1, exponentiate = TRUE)
          )
      )
  )
})


test_that("tidy_plus_plus() with mice objects", {
  # impute missing values
  imputed_trial <-
    suppressWarnings(mice::mice(gtsummary::trial, maxit = 2, m = 2))
  # build regression model
  mod <- with(imputed_trial, lm(age ~ marker + grade))

  # testing pre-pooled results
  expect_error(
    tidy_plus_plus(
      mod,
      exponentiate = FALSE,
      tidy_fun = function(x, ...) mice::pool(x) %>% mice::tidy(...)
    ),
    NA
  )
})


test_that("tidy_plus_plus() with tidyselect", {
  # build regression model
  mod <- lm(age ~ trt + marker + grade, gtsummary::trial)

  expect_error(
    tidy_plus_plus(
      mod,
      add_header_rows = TRUE,
      show_single_row = trt,
      no_reference_row = grade
    ),
    NA
  )

  expect_equal(
    tidy_plus_plus(
      mod,
      add_header_rows = TRUE,
      show_single_row = "trt",
      no_reference_row = "grade"
    ),
    tidy_plus_plus(
      mod,
      add_header_rows = TRUE,
      show_single_row = trt,
      no_reference_row = grade
    )
  )
})
