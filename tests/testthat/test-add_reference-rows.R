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
    c("(Intercept)", "stage_ref", "stage2", "stage3", "stage4", "grade1",
      "grade2", "grade_ref", "trt1", "trt_ref", "grade1:trt1", "grade2:trt1")
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
