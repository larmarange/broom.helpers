test_that("tidy_add_reference_rows() works as expected", {
  mod <- glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.treatment, grade = contr.SAS, trt = contr.sum)
  )
  res <- mod %>% tidy_and_attach() %>% tidy_add_reference_rows()
  expect_equivalent(
    res$term,
    c("(Intercept)", "stageT1", "stage2", "stage3", "stage4", "grade1",
      "grade2", "grade3", "trt1", "trt2", "grade1:trt1", "grade2:trt1")
  )
  expect_equivalent(
    res$reference_row,
    c(NA, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE,
      NA, NA)
  )

  # no reference row added if other contrasts are used
  mod <- glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.poly, grade = contr.helmert, trt = matrix(c(2, 3)))
  )
  res <- mod %>% tidy_and_attach() %>% tidy_add_reference_rows()
  expect_true(all(is.na(res$reference_row)))
})


test_that("tidy_add_reference_rows() works with different values of base in contr.treatment()", {
  mod <- glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.treatment(4, base = 3),
                     grade = contr.treatment(3, base = 2),
                     trt = contr.treatment(2, base = 2))
  )
  res <- mod %>% tidy_and_attach() %>% tidy_add_reference_rows()
  expect_equivalent(
    res$term,
    c("(Intercept)", "stage1", "stage2", "stageT3", "stage4", "grade1",
      "gradeII", "grade3", "trt1", "trt2", "grade1:trt1", "grade3:trt1")
  )
  expect_equivalent(
    res$reference_row,
    c(NA, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE,
      NA, NA)
  )
})

test_that("tidy_add_reference_rows() use var_label if available", {
  mod <- glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_variable_labels() %>%
    tidy_add_reference_rows()

  expect_equivalent(
    res$var_label,
    c("(Intercept)", "T Stage", "T Stage", "T Stage", "T Stage",
      "Grade", "Grade", "Grade", "Chemotherapy Treatment", "Chemotherapy Treatment",
      "Grade * Chemotherapy Treatment", "Grade * Chemotherapy Treatment")
  )
})

test_that("tidy_add_reference_rows() works with nnet::multinom", {
  mod <- nnet::multinom(grade ~ stage + marker + age, data = gtsummary::trial, trace = FALSE)
  res <- mod %>% tidy_and_attach() %>% tidy_add_reference_rows()
  expect_equivalent(
    res$reference_row,
    c(NA, TRUE, FALSE, FALSE, FALSE, NA, NA, NA, TRUE, FALSE, FALSE,
      FALSE, NA, NA)
  )
})