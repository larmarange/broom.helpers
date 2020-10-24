test_that("select_helpers: tidy_select_variables", {
  mod <- glm(response ~ age * trt + grade, gtsummary::trial, family = binomial)
  mod_tidy <- tidy_and_attach(mod)

  expect_equal(
    tidy_select_variables(mod_tidy, include = all_categorical())$variable %>%
      na.omit() %>%
      unique(),
    c("(Intercept)", "trt", "grade")
  )

  expect_equal(
    tidy_select_variables(mod_tidy, include = all_categorical(dichotomous = FALSE))$variable %>%
      na.omit() %>%
      unique(),
    c("(Intercept)", "grade")
  )

  expect_equal(
    tidy_select_variables(mod_tidy, include = all_continuous())$variable %>%
      na.omit() %>%
      unique(),
    c("(Intercept)", "age")
  )

  expect_equal(
    tidy_select_variables(mod_tidy, include = all_dichotomous())$variable %>%
      na.omit() %>%
      unique(),
    c("(Intercept)", "trt")
  )

  expect_equal(
    tidy_select_variables(mod_tidy, include = all_interaction())$variable %>%
      na.omit() %>%
      unique(),
    c("(Intercept)", "age:trt")
  )
})

test_that("select_helpers: tidy_plus_plus", {
  mod <- glm(response ~ age * trt + grade, gtsummary::trial, family = binomial)

  expect_equal(
    tidy_plus_plus(mod, include = all_categorical())$variable %>%
      na.omit() %>%
      unique(),
    c("trt", "grade")
  )

  expect_equal(
    tidy_plus_plus(mod, include = all_continuous())$variable %>%
      na.omit() %>%
      unique(),
    c("age")
  )

  expect_equal(
    tidy_plus_plus(mod, include = all_dichotomous())$variable %>%
      na.omit() %>%
      unique(),
    c("trt")
  )

  expect_equal(
    tidy_plus_plus(mod, include = all_interaction())$variable %>%
      na.omit() %>%
      unique(),
    c("age:trt")
  )

  expect_equal(
    tidy_plus_plus(mod, add_header_rows = TRUE,
                   show_single_row = all_dichotomous())$variable %in% "trt" %>%
      sum(),
    1L
  )
})

test_that("select_helpers: tidy_add_header_rows", {
  mod <- glm(response ~ age * trt + grade, gtsummary::trial, family = binomial)
  mod_tidy <- tidy_and_attach(mod)

  expect_equal(
    tidy_add_header_rows(mod_tidy, show_single_row = all_dichotomous())$variable %in% "trt" %>%
      sum(),
    1L
  )
})
