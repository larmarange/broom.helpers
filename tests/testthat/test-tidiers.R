test_that("tidy_margins()", {
  skip_on_cran()
  skip_if_not_installed("margins")

  mod <- lm(Petal.Length ~ Petal.Width + Species, data = iris)
  expect_error(
    tidy_margins(mod),
    NA
  )
  expect_error(
    res <- tidy_plus_plus(mod, tidy_fun = tidy_margins),
    NA
  )
})

test_that("tidy_all_effects()", {
  skip_on_cran()
  skip_if_not_installed("effects")

  mod <- lm(Petal.Length ~ Petal.Width + Species, data = iris)
  expect_error(
    tidy_all_effects(mod),
    NA
  )
  expect_error(
    res <- tidy_plus_plus(mod, tidy_fun = tidy_all_effects),
    NA
  )
})

test_that("tidy_ggpredict()", {
  skip_on_cran()
  skip_if_not_installed("ggeffects")

  mod <- lm(Petal.Length ~ Petal.Width + Species, data = iris)
  expect_error(
    tidy_ggpredict(mod),
    NA
  )
  expect_error(
    res <- tidy_plus_plus(mod, tidy_fun = tidy_ggpredict),
    NA
  )
})
