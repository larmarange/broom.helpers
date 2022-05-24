test_that(".assert_package() works", {
  # broom will always be installed with broom.helpers
  expect_error(
    .assert_package("broom"),
    NA
  )
  expect_true(.assert_package("broom", boolean = TRUE))

  # expect an error msg for pkg that doesn't exist
  expect_error(
    .assert_package("br000000m")
  )
  expect_error(
    .assert_package("br000000m", fn = "test_fun()")
  )
  expect_false(.assert_package("br000000m", boolean = TRUE))

  expect_equal(
    rlang::inject(!!utils::packageDescription(
      "broom.helpers",
      fields = c("Imports", "Depends", "Suggests", "Enhances", "LinkingTo")
    )),
    list(Imports = NA, Depends = NA, Suggests = NA, Enhances = NA, LinkingTo = NA)
  )

  expect_equal(
    .get_min_version_required("lme4"),
    c(Suggests  = "1.1.28")
  )
  expect_equal(
    .get_min_version_required("brms", pkg_search = NULL),
    NULL
  )
})
