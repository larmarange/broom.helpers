test_that("model_get_n() works for basic models", {
  mod <- lm(Sepal.Length ~ ., iris)
  res <- mod %>% model_get_n()
  expect_equivalent(
    res$n,
    c(150, 150, 150, 150, 50, 50, 50)
  )

  # logistic model
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  res <- mod %>% model_get_n()
  expect_equivalent(
    res$n,
    c(193, 52, 40, 49, 63, 63, 98, 52, 67, 95)
  )
  expect_equivalent(
    res$nevent,
    c(61, 13, 15, 15, 19, 21, 33, 18, 21, 28)
  )

  # works with cbind syntax
  mod <- glm(
    Survived ~ Class * Age + Sex, data = Titanic %>% as.data.frame(),
    weights = Freq, family = binomial
  )
  res <- mod %>% model_get_n()
  expect_equivalent(
    res$n,
    c(2201, 285, 706, 885, 2092, 470, 261, 627, 885, 325, 109, 1731)
  )
  expect_equivalent(
    res$nevent,
    c(711, 118, 178, 212, 654, 344, 94, 151, 212, 203, 57, 367)
  )

  # Poisson without offset
  mod <- glm(response ~ age + grade * trt, gtsummary::trial, family = poisson)
  res <- mod %>% model_get_n()
  expect_equivalent(
    res$n,
    c(183, 183, 58, 60, 94, 29, 33, 65, 89)
  )
  expect_equivalent(
    res$nevent,
    c(58, 58, 17, 20, 31, 10, 8, 21, 27)
  )
  expect_equivalent(
    res$exposure,
    c(183, 183, 58, 60, 94, 29, 33, 65, 89)
  )

  # Poisson with offset
  mod <- glm(
    response ~ trt * grade + offset(ttdeath),
    gtsummary::trial,
    family = poisson,
    weights = rep_len(1:2, 200)
  )
  res <- mod %>% model_get_n()
  expect_equivalent(
    res$n,
    c(292, 151, 94, 92, 49, 49, 141, 106)
  )
  expect_equivalent(
    res$nevent,
    c(96, 53, 28, 31, 19, 12, 43, 37)
  )
  expect_equivalent(
    res$exposure %>% round(),
    c(5819, 2914, 1826, 1766, 887, 916, 2905, 2227)
  )
})


test_that("model_get_n() handles variables having non standard name", {
  df <- gtsummary::trial %>% dplyr::mutate(`grade of kids` = grade)
  mod <- glm(response ~ stage + `grade of kids` + trt, df,
    family = binomial,
    contrasts = list(`grade of kids` = contr.sum)
  )
  expect_message(
    res <- mod %>% model_get_n(),
    NA
  )
})

test_that("model_get_n() works with lme4::lmer", {
  df <- gtsummary::trial
  df$stage <- as.character(df$stage)
  df$group <- rep.int(1:2, 100)
  mod <- lme4::lmer(marker ~ stage + grade + (1 | group), df)
  expect_error(res <- mod %>% model_get_n(), NA)
  expect_equivalent(names(res), c("term", "n"))
})


test_that("model_get_n() works with lme4::glmer", {
  df <- gtsummary::trial
  df$stage <- as.character(df$stage)
  df$group <- rep.int(1:2, 100)
  suppressMessages(
    mod <- lme4::glmer(response ~ stage + grade + (1 | group), df, family = binomial)
  )
  expect_error(res <- mod %>% model_get_n(), NA)
  expect_equivalent(names(res), c("term", "n", "nevent"))
})


test_that("model_get_n() works with survival::coxph", {
  df <- survival::lung %>% dplyr::mutate(sex = factor(sex))
  mod <- survival::coxph(survival::Surv(time, status) ~ ph.ecog + age + sex, data = df)
  expect_error(res <- mod %>% model_get_n(), NA)
  expect_equivalent(names(res), c("term", "n", "nevent", "exposure"))
})

test_that("model_get_n() works with survival::survreg", {
  mod <- survival::survreg(
    survival::Surv(futime, fustat) ~ factor(ecog.ps) + rx,
    survival::ovarian,
    dist = "exponential"
  )
  expect_error(res <- mod %>% model_get_n(), NA)
  expect_equivalent(names(res), c("term", "n", "nevent", "exposure"))
})

test_that("model_get_n() works with nnet::multinom", {
  mod <- nnet::multinom(grade ~ stage + marker + age, data = gtsummary::trial, trace = FALSE)
  expect_error(res <- mod %>% model_get_n(), NA)
  expect_equivalent(names(res), c("y.level", "term", "n", "nevent"))
  expect_equivalent(
    res$y.level,
    c("II", "II", "II", "II", "II", "II", "II", "III", "III", "III",
      "III", "III", "III", "III")
  )
  expect_equivalent(
    res$n,
    c(179, 52, 37, 43, 179, 179, 47, 179, 52, 37, 43, 179, 179, 47)
  )
  expect_equivalent(
    res$nevent,
    c(57, 16, 8, 12, 57, 57, 21, 58, 18, 12, 16, 58, 58, 12)
  )
})

test_that("model_get_n() works with survey::svyglm", {
  df <- survey::svydesign(~1, weights = ~1, data = gtsummary::trial)
  mod <- survey::svyglm(response ~ age + grade * trt, df, family = quasibinomial)
  expect_error(res <- mod %>% model_get_n(), NA)
  expect_equivalent(names(res), c("term", "n", "nevent"))

  mod <- survey::svyglm(response ~ age + grade + offset(ttdeath), df, family = quasipoisson)
  expect_error(res <- mod %>% model_get_n(), NA)
  expect_equivalent(names(res), c("term", "n", "nevent", "exposure"))

  df <- survey::svydesign(
    ~ 1, weights = ~ Freq,
    data = as.data.frame(Titanic) %>% dplyr::filter(Freq > 0)
  )
  mod <- survey::svyglm(Survived ~ Class + Age * Sex, df, family = quasibinomial)
  expect_error(res <- mod %>% model_get_n(), NA)
  expect_equivalent(names(res), c("term", "n", "nevent"))
  expect_equivalent(
    res$n,
    c(2201, 285, 706, 885, 2092, 470, 425, 325, 109, 1731)
  )
})

test_that("model_get_n() works with ordinal::clm", {
  mod <- ordinal::clm(rating ~ temp * contact, data = ordinal::wine, nominal = ~contact)
  expect_error(res <- mod %>% model_get_n(), NA)
  expect_equivalent(names(res), c("term", "n"))
  # note: no nevent computed for ordinal models
})


test_that("model_get_n() works with ordinal::clmm", {
  mod <- ordinal::clmm(rating ~ temp * contact + (1 | judge), data = ordinal::wine)
  expect_error(res <- mod %>% model_get_n(), NA)
  expect_equivalent(names(res), c("term", "n"))
})


test_that("model_get_n() works with MASS::polr", {
  mod <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing)
  expect_error(res <- mod %>% model_get_n(), NA)
  expect_equivalent(names(res), c("term", "n"))
})


test_that("model_get_n() works with geepack::geeglm", {
  df <- geepack::dietox
  df$Cu <- as.factor(df$Cu)
  mf <- formula(Weight ~ Cu * Time)
  suppressWarnings(
    mod <- geepack::geeglm(mf, data = df, id = Pig, family = poisson("identity"), corstr = "ar1")
  )
  expect_error(res <- mod %>% model_get_n(), NA)
  expect_equivalent(names(res), c("term", "n"))

  suppressWarnings(
    mod <- geepack::geeglm(mf, data = df, id = Pig, family = poisson(), corstr = "ar1")
  )
  expect_error(res <- mod %>% model_get_n(), NA)
  expect_equivalent(names(res), c("term", "n", "nevent", "exposure"))
})


test_that("model_get_n() works with gam::gam", {
  data(kyphosis, package = "gam")
  mod <- gam::gam(Kyphosis ~ gam::s(Age, 4) + Number, family = binomial, data = kyphosis)
  expect_error(res <- mod %>% model_get_n(), NA)
  expect_equivalent(names(res), c("term", "n", "nevent"))
})


test_that("model_get_n() works with lavaan::lavaan", {
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
  expect_error(res <- mod %>% model_get_n(), NA)
  expect_null(res)
})

