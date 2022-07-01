test_that("tidy_plus_plus() works for basic models", {
  mod <- lm(Petal.Length ~ Petal.Width, iris)
  expect_error(
    mod %>% tidy_plus_plus(),
    NA
  )

  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  expect_error(
    mod %>% tidy_plus_plus(add_header_rows = TRUE, include = c(stage, grade)),
    NA
  )

  # combining custom variable labels with categorical_terms_pattern
  # check that the custom variable labels are passed to model_list_terms_levels()
  res <- mod %>%
    tidy_plus_plus(
      variable_labels = c(grade = "custom"),
      add_reference_rows = FALSE,
      categorical_terms_pattern = "{var_label}:{level}/{reference_level}"
    )
  expect_equivalent(
    res$label,
    c("T Stage:T2/T1", "T Stage:T3/T1", "T Stage:T4/T1", "custom:II/I",
      "custom:III/I", "Chemotherapy Treatment:Drug B/Drug A")
  )

  # works with add_n
  res <- mod %>% tidy_plus_plus(add_n = TRUE)
  expect_true(all(c("n_obs", "n_event") %in% names(res)))
})

test_that("tidy_plus_plus() works with no intercept models", {
  mod <- glm(response ~ stage + grade - 1, data = gtsummary::trial, family = binomial)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
  expect_equivalent(
    res$variable,
    c("stage", "stage", "stage", "stage", "grade", "grade", "grade")
  )
  expect_equivalent(
    res$label,
    c("T1", "T2", "T3", "T4", "I", "II", "III")
  )
  expect_equivalent(
    res$contrasts_type,
    c("no.contrast", "no.contrast", "no.contrast", "no.contrast",
      "treatment", "treatment", "treatment")
  )
})

test_that("tidy_plus_plus() and functionnal programming", {
  # works with glm
  expect_error(
    res <- dplyr::tibble(grade = c("I", "II", "III")) %>%
      dplyr::mutate(df_model = purrr::map(grade, ~ gtsummary::trial %>% dplyr::filter(grade == ..1))) %>%
      dplyr::mutate(
        mv_formula_char = "response ~ trt + age + marker",
        mv_formula = purrr::map(mv_formula_char, ~ as.formula(.x)),
        mv_model_form =
          purrr::map2(
            mv_formula, df_model,
            ~ glm(..1, data = ..2)
          ),
        mv_tbl_form =
          purrr::map(
            mv_model_form,
            ~ tidy_plus_plus(..1, exponentiate = TRUE, add_header_rows = TRUE)
          )
      ),
    NA
  )

  # for coxph, identification of variables will not work
  # will display a message
  # but a result should be returned
  expect_message(
    suppressWarnings(
      res <- dplyr::tibble(grade = c("I", "II", "III")) %>%
        dplyr::mutate(df_model = purrr::map(grade, ~ gtsummary::trial %>% dplyr::filter(grade == ..1))) %>%
        dplyr::mutate(
          mv_formula_char = "survival::Surv(ttdeath, death) ~ trt + age + marker",
          mv_formula = purrr::map(mv_formula_char, ~ as.formula(.x)),
          mv_model_form =
            purrr::map2(
              mv_formula, df_model,
              ~ survival::coxph(..1, data = ..2)
            ),
          mv_tbl_form =
            purrr::map(
              mv_model_form,
              ~ tidy_plus_plus(..1, exponentiate = TRUE)
            )
        )
    )
  )

})


test_that("tidy_plus_plus() with mice objects", {
  skip_on_cran()
  skip_if(packageVersion("mice") < "3.12.0")
  # impute missing values
  imputed_trial <-
    suppressWarnings(mice::mice(gtsummary::trial, maxit = 2, m = 2, print = FALSE))
  # build regression model
  mod <- with(imputed_trial, lm(age ~ marker + grade))

  # testing pre-pooled results
  expect_error(
    tidy_plus_plus(
      mod,
      exponentiate = FALSE,
      tidy_fun = function(x, ...) mice::pool(x) %>% mice::tidy(...)
    ),
    NA
  )
})


test_that("tidy_plus_plus() with tidyselect", {
  # build regression model
  mod <- lm(age ~ trt + marker + grade, gtsummary::trial)

  expect_error(
    tidy_plus_plus(
      mod,
      add_header_rows = TRUE,
      show_single_row = trt,
      no_reference_row = grade
    ),
    NA
  )

  expect_equal(
    tidy_plus_plus(
      mod,
      add_header_rows = TRUE,
      show_single_row = "trt",
      no_reference_row = "grade"
    ),
    tidy_plus_plus(
      mod,
      add_header_rows = TRUE,
      show_single_row = trt,
      no_reference_row = grade
    )
  )
})

test_that("tidy_plus_plus() works with stats::aov", {
  mod <- aov(yield ~ block + N*P*K, npk)
  expect_error(
    res <- tidy_plus_plus(mod),
    NA
  )
  expect_equivalent(
    res$variable,
    c("block", "N", "P", "K", "N:P", "N:K", "P:K")
  )
})

test_that("tidy_plus_plus() works with lme4::lmer", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  skip_if_not_installed("broom.mixed")
  mod <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with lme4::glmer", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  mod <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                     family = binomial, data = lme4::cbpp
  )
  skip_if_not_installed("broom.mixed")
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
  mod <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                     family = binomial("probit"), data = lme4::cbpp
  )
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with lme4::glmer.nb", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  skip_if_not_installed("MASS")
  library(lme4)
  suppressMessages(
    mod <- lme4::glmer.nb(Days ~ Age + Eth + (1|Sex), data = MASS::quine)
  )
  skip_if_not_installed("broom.mixed")
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})

test_that("tidy_plus_plus() works with survival::coxph", {
  df <- survival::lung %>% dplyr::mutate(sex = factor(sex))
  mod <- survival::coxph(survival::Surv(time, status) ~ ph.ecog + age + sex, data = df)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})

test_that("tidy_plus_plus() works with survival::survreg", {
  mod <- survival::survreg(
    survival::Surv(futime, fustat) ~ ecog.ps + rx,
    survival::ovarian,
    dist = "exponential"
  )
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with survival::clogit", {
  skip_on_cran()
  library(survival)
  resp <- levels(survival::logan$occupation)
  n <- nrow(survival::logan)
  indx <- rep(1:n, length(resp))
  logan2 <- data.frame(survival::logan[indx,],
                       id = indx,
                       tocc = factor(rep(resp, each=n)))
  logan2$case <- (logan2$occupation == logan2$tocc)
  mod <- survival::clogit(case ~ tocc + tocc:education + strata(id), logan2)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with nnet::multinom", {
  skip_on_cran()
  suppressMessages(
    mod <- nnet::multinom(
      grade ~ stage + marker + age, data = gtsummary::trial,
      trace = FALSE
    )
  )
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )

  # multinom model with binary outcome
  suppressMessages(
    mod <- nnet::multinom(
      response ~ stage + marker + age, data = gtsummary::trial,
      trace = FALSE
    )
  )
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )

})

test_that("tidy_plus_plus() works with survey::svyglm", {
  skip_if_not_installed("survey")
  skip_on_cran()
  df <- survey::svydesign(~1, weights = ~1, data = gtsummary::trial)
  mod <- survey::svyglm(response ~ age + grade * trt, df, family = quasibinomial)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})

test_that("tidy_plus_plus() works with survey::svycoxph", {
  skip_if_not_installed("survey")
  skip_on_cran()
  dpbc <- survey::svydesign(id = ~ 1, prob = ~ 1, strata = ~ edema, data = survival::pbc)
  mod <- survey::svycoxph(Surv(time, status>0) ~ log(bili) + protime + albumin, design = dpbc)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})

test_that("tidy_plus_plus() works with survey::svyolr", {
  skip_if_not_installed("survey")
  skip_on_cran()
  data(api, package = "survey")
  fpc <- survey::svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
  fpc <- update(fpc, mealcat=cut(meals,c(0,25,50,75,100)))
  mod <- survey::svyolr(mealcat~avg.ed+mobility+stype, design = fpc)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})

test_that("tidy_plus_plus() works with ordinal::clm", {
  skip_on_cran()
  mod <- ordinal::clm(rating ~ temp * contact, data = ordinal::wine)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with ordinal::clmm", {
  skip_on_cran()
  mod <- ordinal::clmm(rating ~ temp * contact + (1 | judge), data = ordinal::wine)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with MASS::polr", {
  mod <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with MASS::glm.nb", {
  skip_on_cran()
  mod <- MASS::glm.nb(Days ~ Sex / (Age + Eth * Lrn), data = MASS::quine)
  expect_error(
    suppressWarnings(res <- mod %>% tidy_plus_plus()),
    NA
  )
})

test_that("tidy_plus_plus() works with geepack::geeglm", {
  skip_on_cran()
  skip_if(packageVersion("geepack") < 1.3)

  df <- geepack::dietox
  df$Cu <- as.factor(df$Cu)
  mf <- formula(Weight ~ Cu * Time)
  suppressWarnings(
    mod <- geepack::geeglm(mf, data = df, id = Pig, family = poisson("log"), corstr = "ar1")
  )
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with gam::gam", {
  skip_on_cran()
  data(kyphosis, package = "gam")
  mod <- gam::gam(Kyphosis ~ gam::s(Age, 4) + Number, family = binomial, data = kyphosis)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with brms::brm", {
  skip_on_cran()
  skip_if_not_installed("broom.mixed")
  skip_if_not_installed("brms")
  skip_if(packageVersion("brms") < 2.13)
  skip_if_not_installed("rstanarm")

  load(system.file("extdata", "brms_example.rda", package="broom.mixed"))
  mod <- brms_crossedRE
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})

test_that("tidy_plus_plus() works with rstanarm::stan_glm", {
  skip_on_cran()
  skip_if_not_installed("broom.mixed")
  skip_if_not_installed("rstanarm")

  mod <- rstanarm::stan_glm(
    response ~ age + grade,
    data = gtsummary::trial,
    refresh = 0,
    family = binomial
  )
  expect_error(
    res <- mod %>% tidy_plus_plus(tidy_fun = broom.mixed::tidy),
    NA
  )
})

test_that("tidy_plus_plus() works with cmprsk::crr", {
  skip_on_cran()
  skip_if_not_installed("cmprsk")
  skip_if(packageVersion("broom") < "0.7.4")

  ftime <- rexp(200)
  fstatus <- sample(0:2,200,replace=TRUE)
  cov <- matrix(runif(600),nrow=200)
  dimnames(cov)[[2]] <- c('x1','x2','x3')
  mod <- cmprsk::crr(ftime,fstatus,cov)
  expect_error(
    res <- mod %>% tidy_plus_plus(quiet = TRUE),
    NA
  )
})

test_that("tidy_plus_plus() works with tidycmprsk::crr", {
  skip_on_cran()
  skip_if_not_installed("tidycmprsk")

  mod <- tidycmprsk::crr(Surv(ttdeath, death_cr) ~ age + grade, tidycmprsk::trial)
  expect_error(
    res <- mod %>% tidy_plus_plus(quiet = TRUE),
    NA
  )
})


test_that("tidy_plus_plus() works with stats::nls", {
  mod <- stats::nls(
    Petal.Width ~ a * Petal.Length - (Sepal.Width + Sepal.Length) / b + a^2,
    data = iris,
    start = list(a = 1, b = 1)
  )
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with lavaan::lavaan", {
  skip_on_cran()
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
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with lfe::felm", {
  skip_on_cran()
  skip_if_not_installed("lfe")
  mod <- lfe::felm(marker ~ age + grade | stage | 0, gtsummary::trial)
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() error messaging", {
  # does not allow for exponentiate, conf.inf, conf.level arguments
  bad_tidy <- function(x) {
    broom::tidy
  }

  expect_error(
    lm(mpg ~ cyl, mtcars) %>%
      tidy_plus_plus(tidy_fun = bad_tidy)
  )
})


test_that("tidy_plus_plus() works with mgcv::gam", {
  skip_on_cran()
  skip_if_not_installed("mgcv")
  tidy_gam <- function(x, conf.int = FALSE, exponentiate = FALSE, ...) {
    broom::tidy(x,
                conf.int = conf.int,
                exponentiate = exponentiate,
                parametric = TRUE,  ...) %>%
      dplyr::mutate(parametric = TRUE) %>%
      dplyr::bind_rows(
        broom::tidy(x, parametric = FALSE, ...) %>%
          dplyr::mutate(parametric = FALSE)
      ) %>%
      dplyr::relocate(.data$parametric, .after = dplyr::last_col())
  }

  gam_logistic <- mgcv::gam(response ~ s(marker, ttdeath) + grade + age, data = gtsummary::trial, family = binomial)
  gam_linear <- mgcv::gam(response ~ s(marker, ttdeath) + grade, data = gtsummary::trial)
  gam_smooth_only <- mgcv::gam(response ~ s(marker, ttdeath), data = gtsummary::trial)
  gam_param_only <- mgcv::gam(response ~ grade, data = gtsummary::trial)

  expect_error(tbl_gam_logistic <- gam_logistic %>% tidy_plus_plus(tidy_fun = tidy_gam), NA)
  expect_error(gam_logistic %>% tidy_plus_plus(), NA)

  expect_error(tbl_gam_linear <- gam_linear %>% tidy_plus_plus(tidy_fun = tidy_gam), NA)
  expect_error(gam_linear %>% tidy_plus_plus(), NA)

  expect_error(tbl_gam_smooth_only <- gam_smooth_only %>% tidy_plus_plus(tidy_fun = tidy_gam), NA)
  expect_error(gam_smooth_only %>% tidy_plus_plus(), NA)

  expect_error(tbl_gam_param_only <- gam_param_only %>% tidy_plus_plus(tidy_fun = tidy_gam), NA)
  # the default tidier return a df with no columns and no rows...it fails.
})


test_that("tidy_plus_plus() works with VGAM::vglm", {
  skip_on_cran()
  skip_if_not_installed("VGAM")
  skip_if_not_installed("parameters")

  df <- data.frame(
    treatment = gl(3, 3),
    outcome = gl(3, 1, 9),
    counts = c(18,17,15,20,10,20,25,13,12)
  )
  mod <- VGAM::vglm(
    counts ~ outcome + treatment,
    family = VGAM::poissonff,
    data = df,
    trace = FALSE
  )
  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with plm::plm", {
  skip_on_cran()
  skip_if_not_installed("plm")

  data("Grunfeld", package = "plm")
  mod <- plm::plm(
    inv ~ value + capital,
    data = Grunfeld,
    model = "within",
    index = c("firm", "year")
  )

  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )
})


test_that("tidy_plus_plus() works with biglm::bigglm", {
  skip_on_cran()
  skip_if_not_installed("biglm")
  skip_if(compareVersion(as.character(getRversion()), "3.6") < 0)

  mod <- biglm::bigglm(
    response ~ age + trt,
    data = as.data.frame(gtsummary::trial),
    family = binomial()
  )

  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )

  # check that reference rows are properly added
  expect_equal(
    res %>% dplyr::filter(variable == "trt") %>% purrr::pluck("reference_row"),
    c(TRUE, FALSE)
  )
})

test_that("tidy_plus_plus() works with biglmm::bigglm", {
  skip_on_cran()
  skip_if_not_installed("biglmm")
  skip_if(compareVersion(as.character(getRversion()), "3.6") < 0)

  mod <- biglmm::bigglm(
    response ~ age + trt,
    data = as.data.frame(gtsummary::trial),
    family = binomial()
  )

  expect_error(
    res <- mod %>% tidy_plus_plus(),
    NA
  )

  # check that reference rows are properly added
  expect_equal(
    res %>% dplyr::filter(variable == "trt") %>% purrr::pluck("reference_row"),
    c(TRUE, FALSE)
  )
})

test_that("tidy_plus_plus() works with parsnip::model_fit object", {
  skip_on_cran()
  skip_if_not_installed("parsnip")

  d <- gtsummary::trial
  d$response <- as.factor(d$response)
  mod1 <- glm(response ~ stage + grade + trt, d, family = binomial)
  mod2 <- parsnip::logistic_reg() %>%
    parsnip::set_engine("glm") %>%
    parsnip::fit(response ~ stage + grade + trt, data = d)

  res1 <- mod1 %>% tidy_plus_plus(exponentiate = TRUE)
  expect_error(
    res2 <- mod2 %>% tidy_plus_plus(exponentiate = TRUE),
    NA
  )
  expect_equivalent(res1, res2)
})
