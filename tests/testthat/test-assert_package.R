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
    .get_min_version_required("brms"),
    c(Suggests  = "2.13.0")
  )
  expect_equal(
    .get_min_version_required("brms", pkg_search = NULL),
    NULL
  )
})
