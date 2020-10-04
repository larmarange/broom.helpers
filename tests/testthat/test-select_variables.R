test_that("tidy_select_variables() works for basic models", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_identify_variables()

  # no change by default
  res2 <- res %>% tidy_select_variables()
  expect_equivalent(res, res2)

  # keep
  res2 <- res %>% tidy_select_variables(keep = "stage")
  expect_equivalent(
    res2$variable,
    c(NA, "stage", "stage", "stage")
  )
  res2 <- res %>% tidy_select_variables(keep = c("grade", "trt"))
  expect_equivalent(
    res2$variable,
    c(NA, "grade", "grade", "trt")
  )

  # select and de-select
  expect_equivalent(
    res %>% tidy_select_variables(keep = stage),
    res %>% tidy_select_variables(keep = -c(grade, trt))
  )

  # tidyselect fns
  expect_equivalent(
    res %>% tidy_select_variables(keep = contains("tage")),
    res %>% tidy_select_variables(keep = stage)
  )

  # no error when none selected
  expect_error(
    res %>% tidy_select_variables(keep = starts_with("zzzzzzz")),
    NA
  )
  expect_error(
    res %>% tidy_select_variables(keep = -everything()),
    NA
  )
})


test_that("test tidy_select_variables() checks", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  # expect an error if no model attached
  expect_error(mod %>% broom::tidy() %>% tidy_select_variables())

  # could be apply twice (no error)
  expect_error(
    mod %>% tidy_and_attach() %>% tidy_select_variables() %>% tidy_select_variables(),
    NA
  )
})
