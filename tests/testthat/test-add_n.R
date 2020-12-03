test_that("tidy_add_n() works for basic models", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_n()
  expect_equivalent(
    res$n,
    c(193, 52, 40, 49, 63, 63, 98)
  )
  expect_equivalent(
    res$nevent,
    c(61, 13, 15, 15, 19, 21, 33)
  )

  mod <- glm(response ~ stage + grade + trt, gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.sum, grade = contr.helmert, trt = contr.SAS)
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_n()
  expect_equivalent(
    res$n,
    c(193, 52, 52, 40, 63, 63, 95)
  )

  mod <- glm(response ~ stage + grade + trt, gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.poly, grade = contr.treatment, trt = matrix(c(-3, 2)))
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_n()
  expect_equivalent(
    res$n,
    c(193, 193, 193, 193, 63, 63, 98)
  )

  mod <- glm(
    response ~ stage + grade + trt + factor(death),
    gtsummary::trial,
    family = binomial,
    contrasts = list(
      stage = contr.treatment(4, 3), grade = contr.treatment(3, 2),
      trt = contr.treatment(2, 2), "factor(death)" = matrix(c(-3, 2))
    )
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_n()
  expect_equivalent(
    res$n,
    c(193, 52, 52, 49, 67, 63, 95, 107)
  )

  mod <- glm(response ~ stage + grade + trt, gtsummary::trial,
    family = binomial,
    contrasts = list(stage = "contr.sum", grade = "contr.helmert", trt = "contr.SAS")
  )
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_n()
  expect_equivalent(
    res$n,
    c(193, 52, 52, 40, 63, 63, 95)
  )
})

test_that("test tidy_add_n() checks", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  # expect an error if no model attached
  expect_error(mod %>% broom::tidy() %>% tidy_add_n())

  # could be apply twice (no error)
  expect_error(
    mod %>% tidy_and_attach() %>% tidy_add_n() %>% tidy_add_n(),
    NA
  )
})


test_that("tidy_add_n() works with variables having non standard name", {
  df <- gtsummary::trial %>% dplyr::mutate(`grade of kids` = grade)
  mod <- glm(response ~ stage + `grade of kids` + trt, df, family = binomial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_n()
  expect_equivalent(
    res$n,
    c(193, 52, 40, 49, 63, 63, 98)
  )
})


test_that("tidy_add_n() works with lme4::lmer", {
  df <- gtsummary::trial
  df$stage <- as.character(df$stage)
  df$group <- rep.int(1:2, 100)
  mod <- lme4::lmer(marker ~ stage + grade + (1 | group), df)
  expect_error(mod %>% tidy_and_attach(tidy_fun = broom.mixed::tidy) %>% tidy_add_n(), NA)
})


test_that("tidy_add_n() works with lme4::glmer", {
  df <- gtsummary::trial
  df$stage <- as.character(df$stage)
  df$group <- rep.int(1:2, 100)
  suppressMessages(
    mod <- lme4::glmer(response ~ stage + grade + (1 | group), df, family = binomial)
  )
  expect_error(mod %>% tidy_and_attach(tidy_fun = broom.mixed::tidy) %>% tidy_add_n(), NA)
})


test_that("tidy_add_n() works with survival::coxph", {
  df <- survival::lung %>% dplyr::mutate(sex = factor(sex))
  mod <- survival::coxph(survival::Surv(time, status) ~ ph.ecog + age + sex, data = df)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_n(), NA)
})

test_that("tidy_add_n() works with survival::survreg", {
  mod <- survival::survreg(
    survival::Surv(futime, fustat) ~ factor(ecog.ps) + rx,
    survival::ovarian,
    dist = "exponential"
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_n(), NA)
})

test_that("tidy_add_n() works with nnet::multinom", {
  mod <- nnet::multinom(grade ~ stage + marker + age, data = gtsummary::trial, trace = FALSE)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_n(), NA)

  mod <- nnet::multinom(
    grade ~ stage + marker + age,
    data = gtsummary::trial, trace = FALSE,
    contrasts = list(stage = contr.sum)
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_n(), NA)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_n()
  expect_equivalent(
    res$n,
    c(179, 47, 52, 37, 179, 179, 179, 47, 52, 37, 179, 179)
  )
  expect_equivalent(
    res$nevent,
    c(57, 21, 16, 8, 57, 57, 58, 12, 18, 12, 58, 58)
  )
})

test_that("tidy_add_n() works with survey::svyglm", {
  df <- survey::svydesign(~1, weights = ~1, data = gtsummary::trial)
  mod <- survey::svyglm(response ~ age + grade * trt, df, family = quasibinomial)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_n(), NA)
})

test_that("tidy_add_n() works with ordinal::clm", {
  mod <- ordinal::clm(rating ~ temp * contact, data = ordinal::wine, nominal = ~contact)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_n(), NA)
})


test_that("tidy_add_n() works with ordinal::clmm", {
  mod <- ordinal::clmm(rating ~ temp * contact + (1 | judge), data = ordinal::wine)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_n(), NA)
})


test_that("tidy_add_n() works with MASS::polr", {
  mod <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_n(), NA)
})


test_that("tidy_add_n() works with geepack::geeglm", {
  df <- geepack::dietox
  df$Cu <- as.factor(df$Cu)
  mf <- formula(Weight ~ Cu * Time)
  suppressWarnings(
    mod <- geepack::geeglm(mf, data = df, id = Pig, family = poisson("identity"), corstr = "ar1")
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_n(), NA)
})


test_that("tidy_add_n() works with gam::gam", {
  data(kyphosis, package = "gam")
  mod <- gam::gam(Kyphosis ~ gam::s(Age, 4) + Number, family = binomial, data = kyphosis)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_n(), NA)
})


test_that("tidy_add_n() works with lavaan::lavaan", {
  df <- lavaan::HolzingerSwineford1939
  df$grade <- factor(df$grade, ordered = TRUE)
  HS.model <- "visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6 + grade
               speed   =~ x7 + x8 + x9 "
  mod <- lavaan::lavaan(HS.model,
    data = df,
    auto.var = TRUE, auto.fix.first = TRUE,
    auto.cov.lv.x = TRUE
  )
  expect_error(res <- mod %>% tidy_and_attach() %>% tidy_add_n(), NA)
  expect_true(all(is.na(res$n)))
})
