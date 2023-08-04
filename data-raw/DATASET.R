supported_models <-
  tibble::tribble(
    ~model, ~notes,
    "`stats::lm()`", "",
    "`stats::glm()`", "",
    "`stats::aov()`", "Reference rows are not relevant for such models.",
    "`ordinal::clm()`", "Limited support for models with nominal predictors.",
    "`ordinal::clmm()`", "Limited support for models with nominal predictors.",
    "`survival::coxph()`", "",
    "`survival::survreg()`", "",
    "`survival::clogit()`", "",
    "`lme4::lmer()`", "`broom.mixed` package required",
    "`lme4::glmer()`", "`broom.mixed` package required",
    "`lme4::glmer.nb()`", "`broom.mixed` package required",
    "`brms::brm()`", "`broom.mixed` package required",
    "`geepack::geeglm()`", "",
    "`gam::gam()`", "",
    "`glmmTMB::glmmTMB()`", "`broom.mixed` package required",
    "`mgcv::gam()`", paste(
      "Use default tidier `broom::tidy()` for smooth terms only,",
      "or `gtsummary::tidy_gam()` to include parametric terms"
    ),
    "`nnet::multinom()`", "",
    "`survey::svyglm()`", "",
    "`survey::svycoxph()`", "",
    "`survey::svyolr()`", "",
    "`MASS::polr()`", "",
    "`MASS::glm.nb()`", "",
    "`mice::mira`", paste(
      "Limited support. If `mod` is a `mira` object, use",
      "`tidy_plus_plus(mod, tidy_fun = function(x, ...) mice::pool(x) %>% mice::tidy(...))`"
    ),
    "`lavaan::lavaan()`", "Limited support for categorical variables",
    "`stats::nls()`", "Limited support",
    "`lfe::felm()`", "",
    "`rstanarm::stan_glm()`", "`broom.mixed` package required",
    "`VGAM::vglm()`",
    "Limited support. It is recommended to use `tidy_parameters()` as `tidy_fun`.",
    "`cmprsk::crr()`",
    "Limited support. It is recommended to use `tidycmprsk::crr()` instead.",
    "`tidycmprsk::crr()`", "",
    "`plm::plm()`", "",
    "`biglm::bigglm()`", "",
    "`biglmm::bigglm()`", "",
    "`parsnip::model_fit`", "Supported as long as the type of model and the engine is supported.",
    "`fixest::feglm()`", "May fail with R <= 4.0.",
    "`fixest::femlm()`", "May fail with R <= 4.0.",
    "`fixest::feols()`", "May fail with R <= 4.0.",
    "`fixest::feNmlm()`", "May fail with R <= 4.0.",
    "`logitr::logitr()`", "Requires logitr >= 0.8.0",
    "`multgee::nomLORgee()`", "Experimental support. Use `tidy_multgee()` as `tidy_fun`.",
    "`multgee::ordLORgee()`", "Experimental support. Use `tidy_multgee()` as `tidy_fun`.",
    "`pscl::zeroinfl()`", "Experimental support. Use `tidy_zeroinfl()` as `tidy_fun`.",
    "`pscl::hurdle()`", "Experimental support. Use `tidy_zeroinfl()` as `tidy_fun`."
  ) %>%
  dplyr::arrange(.data$model, .locale = "en")

usethis::use_data(supported_models, overwrite = TRUE)
