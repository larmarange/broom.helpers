test_that("tidy_add_term_labels() works for basic models", {
  df <- gtsummary::trial
  mod <- glm(response ~ age + grade + trt, df, family = binomial)
  res <- mod %>% tidy_and_attach() %>% tidy_add_term_labels()
  expect_equivalent(
    res$label,
    c("(Intercept)", "Age", "II", "III", "Drug B")
  )

  df <- gtsummary::trial
  mod <- glm(response ~ age + grade + trt, df, family = binomial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_reference_rows() %>%
    tidy_add_term_labels()
  expect_equivalent(
    res$label,
    c("(Intercept)", "Age", "I", "II", "III", "Drug A", "Drug B")
  )

  # if labels provided in `labels`, taken into account
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_reference_rows() %>%
    tidy_add_term_labels(
      labels = list(
        "(Intercept)" = "the intercept",
        "trtDrug A" = "the reference term",
        gradeIII = "third grade"
      )
    )
  expect_equivalent(
    res$label,
    c("the intercept", "Age", "I", "II", "third grade", "the reference term",
      "Drug B")
  )
  # no error if providing labels not corresponding to an existing variable
  # but display a message
  expect_error(
    mod %>%
      tidy_and_attach() %>%
      tidy_add_term_labels(
        labels = list(aaa = "aaa", bbb = "bbb", ccc = 44)
      ),
    NA
  )
  expect_message(
    mod %>%
      tidy_and_attach() %>%
      tidy_add_term_labels(
        labels = list(aaa = "aaa", bbb = "bbb", ccc = 44)
      )
  )
})


test_that("tidy_add_term_labels() correctly manages interaction terms", {
  df <- gtsummary::trial
  mod <- glm(response ~ age * grade * trt, df, family = binomial)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_reference_rows() %>%
    tidy_add_term_labels()
  expect_equivalent(
    res$label,
    c("(Intercept)", "Age", "I", "II", "III", "Drug A", "Drug B",
      "Age * II", "Age * III", "Age * Drug B", "II * Drug B", "III * Drug B",
      "Age * II * Drug B", "Age * III * Drug B")
  )

  # custom separator and custom labels for certain interaction terms
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_reference_rows() %>%
    tidy_add_term_labels(
      interaction_sep = ":::",
      labels = c(
        "age:gradeII" = "custom interaction label",
        "gradeII:trtDrug B" = "a second custom label"
      )
    )
  expect_equivalent(
    res$label,
    c("(Intercept)", "Age", "I", "II", "III", "Drug A", "Drug B",
      "custom interaction label", "Age:::III", "Age:::Drug B", "a second custom label",
      "III:::Drug B", "Age:::II:::Drug B", "Age:::III:::Drug B")
  )
})


test_that("tidy_add_term_labels() works with variables having non standard name", {
  df <- gtsummary::trial %>% dplyr::mutate(`grade of kids` = grade)
  mod <- lm(age ~ marker * `grade of kids`, df)
  res <- mod %>%
    tidy_and_attach() %>%
    tidy_add_reference_rows() %>%
    tidy_add_term_labels()
  expect_equivalent(
    res$label,
    c("(Intercept)", "Marker Level (ng/mL)", "I", "II", "III", "Marker Level (ng/mL) * II",
      "Marker Level (ng/mL) * III")
  )
})


test_that("tidy_add_term_labels() works with stats::poly()", {
  df <- iris %>% labelled::set_variable_labels(Petal.Length = "Length of petal")
  mod <- lm(Sepal.Length ~ poly(Sepal.Width, 3) + poly(Petal.Length, 2), df)
  res <- mod %>% tidy_and_attach() %>%
    tidy_add_term_labels()
  expect_equivalent(
    res$label,
    c("(Intercept)", "Sepal.Width", "Sepal.Width²", "Sepal.Width³",
      "Petal.Length", "Petal.Length²")
  )
})


test_that("tidy_add_term_labels() works with lme4::lmer", {
  mod <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  expect_error(mod %>% tidy_and_attach(tidy_fun = broom.mixed::tidy) %>% tidy_add_term_labels(), NA)
})


test_that("tidy_add_term_labels() works with lme4::glmer", {
  mod <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                     family = binomial, data = lme4::cbpp)
  expect_error(mod %>% tidy_and_attach(tidy_fun = broom.mixed::tidy) %>% tidy_add_term_labels(), NA)
})


test_that("tidy_add_term_labels() works with survival::coxph", {
  df <- survival::lung %>% dplyr::mutate(sex = factor(sex))
  mod <- survival::coxph(survival::Surv(time, status) ~ ph.ecog + age + sex, data = df)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_term_labels(), NA)
})

test_that("tidy_add_term_labels() works with survival::survreg", {
  mod <- survival::survreg(
    survival::Surv(futime, fustat) ~ ecog.ps + rx,
    survival::ovarian, dist="exponential"
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_term_labels(), NA)
})

test_that("tidy_add_term_labels() works with nnet::multinom", {
  mod <- nnet::multinom(grade ~ stage + marker + age, data = gtsummary::trial, trace = FALSE)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_term_labels(), NA)
})

test_that("tidy_add_term_labels() works with survey::svyglm", {
  df <- survey::svydesign(~ 1, weights = ~1, data = gtsummary::trial)
  mod <- survey::svyglm(response ~ age + grade * trt, df, family = quasibinomial)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_term_labels(), NA)
})

test_that("tidy_add_term_labels() works with ordinal::clm", {
  mod <- ordinal::clm(rating ~ temp * contact, data = ordinal::wine, nominal = ~ contact)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_term_labels(), NA)
})


test_that("tidy_add_term_labels() works with ordinal::clmm", {
  mod <- ordinal::clmm(rating ~ temp * contact + (1|judge), data = ordinal::wine)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_term_labels(), NA)
})


test_that("tidy_add_term_labels() works with MASS::polr", {
  mod <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_term_labels(), NA)
})


test_that("tidy_add_term_labels() works with geepack::geeglm", {
  df <- geepack::dietox
  df$Cu     <- as.factor(df$Cu)
  mf <- formula(Weight ~ Cu * Time)
  suppressWarnings(
    mod <- geepack::geeglm(mf, data = df, id = Pig, family = poisson("identity"), corstr = "ar1")
  )
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_term_labels(), NA)
})


test_that("tidy_add_term_labels() works with gam::gam", {
  data(kyphosis, package = "gam")
  mod <- gam::gam(Kyphosis ~ gam::s(Age,4) + Number, family = binomial, data = kyphosis)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_term_labels(), NA)

  mod <- suppressWarnings(gam::gam(
    Ozone^(1/3) ~ gam::lo(Solar.R) + gam::lo(Wind, Temp),
    data = datasets::airquality, na = gam::na.gam.replace
  ))
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_term_labels(), NA)
})


test_that("tidy_add_term_labels() works with lavaan::lavaan", {
  df <- lavaan::HolzingerSwineford1939
  df$grade <- factor(df$grade, ordered = TRUE)
  HS.model <- 'visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6 + grade
               speed   =~ x7 + x8 + x9 '
  mod <- lavaan::lavaan(HS.model, data = df,
                        auto.var = TRUE, auto.fix.first = TRUE,
                        auto.cov.lv.x = TRUE)
  expect_error(mod %>% tidy_and_attach() %>% tidy_add_term_labels(), NA)
})

