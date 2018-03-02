context("Test check_*() return deprecated")

test_that("check_constraint() reports it is deprecated", {
  expect_error(  # expect an error AND a warning
    expect_warning(
      check_constraint(), "is deprecated"
    )
  )

})

test_that("check_ind() reports it is deprecated", {
  expect_error(  # expect error AND warning
    expect_warning(
      check_ind(), "is deprecated"
    )
  )
})
