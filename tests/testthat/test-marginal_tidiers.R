test_that("tidy_margins()", {
  skip_on_cran()
  skip_if_not_installed("margins")

  mod <- lm(Petal.Length ~ Petal.Width + Species, data = iris)
  expect_error(
    t <- tidy_margins(mod),
    NA
  )
  expect_error(
    res <- tidy_plus_plus(mod, tidy_fun = tidy_margins),
    NA
  )
  expect_equal(
    nrow(res),
    nrow(t) + 1 # due to adding ref row
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Average Marginal Effects"
  )
  expect_error(
    tidy_plus_plus(
      mod,
      tidy_fun = tidy_margins,
      add_pairwise_contrasts = TRUE
    )
  )
})

test_that("tidy_all_effects()", {
  skip_on_cran()
  skip_if_not_installed("effects")

  mod <- lm(Petal.Length ~ Petal.Width + Species, data = iris)
  expect_error(
    t <- tidy_all_effects(mod),
    NA
  )
  expect_error(
    res <- tidy_plus_plus(mod, tidy_fun = tidy_all_effects),
    NA
  )
  expect_equal(
    nrow(res),
    nrow(t)
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Marginal Predictions at the Mean"
  )
  expect_error(
    tidy_plus_plus(
      mod,
      tidy_fun = tidy_all_effects,
      add_pairwise_contrasts = TRUE
    )
  )
})

test_that("tidy_ggpredict()", {
  skip_on_cran()
  skip_if_not_installed("ggeffects")

  mod <- lm(Petal.Length ~ Petal.Width + Species, data = iris)
  expect_error(
    t <- tidy_ggpredict(mod),
    NA
  )
  expect_error(
    res <- tidy_plus_plus(mod, tidy_fun = tidy_ggpredict),
    NA
  )
  expect_equal(
    nrow(res),
    nrow(t)
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Marginal Predictions"
  )
  expect_error(
    tidy_plus_plus(
      mod,
      tidy_fun = tidy_ggpredict,
      add_pairwise_contrasts = TRUE
    )
  )
})

test_that("tidy_marginalpredictions()", {
  skip_on_cran()
  skip_if_not_installed("marginaleffects")

  mod <- lm(Petal.Length ~ Petal.Width * Species + Sepal.Length, data = iris)
  expect_error(
    t <- tidy_marginalpredictions(mod),
    NA
  )
  expect_error(
    res <- tidy_plus_plus(mod, tidy_fun = tidy_marginalpredictions),
    NA
  )
  expect_equal(
    nrow(res),
    nrow(t)
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Average Marginal Predictions"
  )
  expect_true(any(res$var_type == "interaction"))
  expect_error(
    tidy_plus_plus(
      mod,
      tidy_fun = tidy_marginalpredictions,
      add_pairwise_contrasts = TRUE
    )
  )

  expect_error(
    t <- tidy_marginalpredictions(mod, "no_interaction"),
    NA
  )
  expect_error(
    res <- tidy_plus_plus(
      mod,
      tidy_fun = tidy_marginalpredictions,
      variables_list = "no_interaction"
    ),
    NA
  )
  expect_equal(
    nrow(res),
    nrow(t)
  )
  expect_false(any(res$var_type == "interaction"))

  expect_error(
    t <- tidy_marginalpredictions(mod, newdata = "mean"),
    NA
  )
  expect_error(
    res <- tidy_plus_plus(
      mod,
      tidy_fun = tidy_marginalpredictions,
      newdata = "mean"
    ),
    NA
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Marginal Predictions at the Mean"
  )

  expect_error(
    res <- tidy_plus_plus(
      mod,
      tidy_fun = tidy_marginalpredictions,
      newdata = "marginalmeans"
    ),
    NA
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Marginal Predictions at Marginal Means"
  )
})
