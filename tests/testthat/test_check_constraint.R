context("Test check_constraint() returns deprecated")

test_that("check_constraint() reports it is deprecated", {
  expect_error(  # expect an error AND a warning
    expect_warning(
      check_constraint(), "is deprecated"
    )
  )

})
