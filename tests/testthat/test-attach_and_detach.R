test_that("Attach and Detach models works", {
  mod <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
  expect_identical(
    mod,
    mod %>% tidy_and_attach() %>% tidy_get_model()
  )

  tb <- broom::tidy(mod)
  expect_identical(
    tb,
    tb %>% tidy_attach_model(mod) %>% tidy_detach_model()
  )
})
