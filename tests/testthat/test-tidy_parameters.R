test_that("tidy_parameters() works for basic models", {
  skip_if_not_installed("parameters")
  mod <- lm(Petal.Length ~ Petal.Width, iris)
  expect_error(
    mod %>% tidy_parameters(),
    NA
  )
  expect_error(
    mod %>% tidy_plus_plus(tidy_fun = tidy_parameters),
    NA
  )

  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  expect_error(
    mod %>% tidy_parameters(),
    NA
  )
  expect_error(
    mod %>% tidy_plus_plus(tidy_fun = tidy_parameters),
    NA
  )
  expect_error(
    mod %>% tidy_plus_plus(tidy_fun = tidy_parameters, conf.int = .80),
    NA
  )
})
