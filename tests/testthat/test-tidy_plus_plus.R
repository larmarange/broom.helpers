test_that("tidy_plus_plus() works for basic models", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  expect_error(
    mod %>% tidy_plus_plus(add_header_rows = TRUE),
    NA
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
