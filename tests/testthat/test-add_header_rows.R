test_that("tidy_add_header_rows() works as expected", {
  mod <- glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.treatment, grade = contr.SAS, trt = contr.sum)
  )
  res <- mod %>% tidy_and_attach() %>% tidy_add_header_rows()
  expect_equivalent(
    res$label,
    c("(Intercept)", "T Stage", "T2", "T3", "T4", "Grade", "I", "II",
      "Drug A", "Grade * Chemotherapy Treatment", "I * Drug A", "II * Drug A")
  )
  expect_equivalent(
    res$term,
    c("(Intercept)", NA, "stage2", "stage3", "stage4", NA, "grade1",
      "grade2", "trt1", NA, "grade1:trt1", "grade2:trt1")
  )
  expect_equivalent(
    res$header_row,
    c(NA, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, NA, TRUE,
      FALSE, FALSE)
  )

  # no effect of show_single_row when no reference row
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_identify_variables() %>%
    tidy_add_header_rows(show_single_row = .$variable)
  expect_equivalent(
    res$label,
    c("(Intercept)", "T Stage", "T2", "T3", "T4", "Grade", "I", "II",
      "Drug A", "Grade * Chemotherapy Treatment", "I * Drug A", "II * Drug A")
  )
  expect_equivalent(
    res$term,
    c("(Intercept)", NA, "stage2", "stage3", "stage4", NA, "grade1",
      "grade2", "trt1", NA, "grade1:trt1", "grade2:trt1")
  )
  expect_equivalent(
    res$header_row,
    c(NA, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, NA, TRUE,
      FALSE, FALSE)
  )

  # with reference rows
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_reference_rows() %>%
    tidy_add_header_rows()
  expect_equivalent(
    res$label,
    c("(Intercept)", "T Stage", "T1", "T2", "T3", "T4", "Grade",
      "I", "II", "III", "Chemotherapy Treatment", "Drug A", "Drug B",
      "Grade * Chemotherapy Treatment", "I * Drug A", "II * Drug A")
  )
  expect_equivalent(
    res$term,
    c("(Intercept)", NA, "stageT1", "stage2", "stage3", "stage4",
      NA, "grade1", "grade2", "grade3", NA, "trt1", "trt2", NA, "grade1:trt1",
      "grade2:trt1")
  )
  expect_equivalent(
    res$header_row,
    c(NA, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
      TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
  )

  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_reference_rows() %>%
    tidy_add_header_rows(show_single_row = c("trt", "unexist", "stage"))
  expect_equivalent(
    res$label,
    c("(Intercept)", "T Stage", "T1", "T2", "T3", "T4", "Grade",
      "I", "II", "III", "Chemotherapy Treatment", "Grade * Chemotherapy Treatment",
      "I * Drug A", "II * Drug A")
  )
  expect_equivalent(
    res$term,
    c("(Intercept)", NA, "stageT1", "stage2", "stage3", "stage4",
      NA, "grade1", "grade2", "grade3", "trt1", NA, "grade1:trt1",
      "grade2:trt1")
  )
  expect_equivalent(
    res$header_row,
    c(NA, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
      NA, TRUE, FALSE, FALSE)
  )
})


test_that("tidy_add_header_rows() works with nnet::multinom", {
  mod <- nnet::multinom(grade ~ stage + marker + age + trt, data = gtsummary::trial, trace = FALSE)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_reference_rows() %>%
    tidy_add_header_rows()
  expect_equivalent(
    res$header_row,
    c(NA, TRUE, FALSE, FALSE, FALSE, FALSE, NA, NA, TRUE, FALSE,
      FALSE, NA, TRUE, FALSE, FALSE, FALSE, FALSE, NA, NA, TRUE, FALSE,
      FALSE)
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_reference_rows() %>%
    tidy_add_header_rows(show_single_row = .$variable)
  expect_equivalent(
    res$header_row,
    c(NA, TRUE, FALSE, FALSE, FALSE, FALSE, NA, NA, NA, NA, TRUE,
      FALSE, FALSE, FALSE, FALSE, NA, NA, NA)
  )
})
