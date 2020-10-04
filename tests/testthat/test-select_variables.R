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

  # drop
  res2 <- res %>% tidy_select_variables(drop = "stage")
  expect_equivalent(
    res2$variable,
    c(NA, "grade", "grade", "trt")
  )
  res2 <- res %>% tidy_select_variables(drop = c("grade", "trt"))
  expect_equivalent(
    res2$variable,
    c(NA, "stage", "stage", "stage")
  )

  # keep and drop
  res2 <- res %>% tidy_select_variables(
    keep = c("grade", "trt"),
    drop = "trt"
  )
  expect_equivalent(
    res2$variable,
    c(NA, "grade", "grade")
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

  # message if keep/drop unexisting variables and quiet=FALSE
  expect_message(
    mod %>% tidy_and_attach() %>% tidy_select_variables(keep = "unk")
  )
  expect_message(
    mod %>% tidy_and_attach() %>% tidy_select_variables(drop = "unk")
  )

  # no message if keep/drop unexisting variables and quiet=TRUE
  expect_message(
    mod %>% tidy_and_attach() %>% tidy_select_variables(keep = "unk", quiet = TRUE),
    NA
  )
  expect_message(
    mod %>% tidy_and_attach() %>% tidy_select_variables(drop = "unk", quiet = TRUE),
    NA
  )

  # error if keep/drop unexisting variables and strict=TRUE
  expect_error(
    mod %>% tidy_and_attach() %>% tidy_select_variables(keep = "unk", strict = TRUE)
  )
  expect_error(
    mod %>% tidy_and_attach() %>% tidy_select_variables(drop = "unk", strict = TRUE)
  )
})
