test_that(".assert_package() works", {
  # broom will always be installed with broom.helpers
  expect_error(
    .assert_package("broom"),
    NA
  )
  expect_true(.assert_package("broom", boolean = TRUE))

  expect_false(.assert_package("br000000m", boolean = TRUE))

  mv <- c(Suggests  = "1.1.28")
  attr(mv, "compare") <- ">="
  expect_equal(
    .get_min_version_required("lme4"),
    mv
  )
  expect_equal(
    .get_min_version_required("brms", pkg_search = NULL),
    NULL
  )

  skip_if(interactive())
  # expect an error msg for pkg that doesn't exist
  # note: if interactive(), user will be invited to install the missing pkg
  expect_error(
    .assert_package("br000000m")
  )
  expect_error(
    .assert_package("br000000m", fn = "test_fun()")
  )
})
