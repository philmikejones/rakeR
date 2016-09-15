#' check_constraint
#'
#' @param constraint_var The constraint table to check, usually a data frame
#' @param num_zones The number of zones that should be present in the table
#'
#' @return An error status. 0 (silent) if no errors; 1 if errors detected
#' @export
#'
#' @examples
#' # not run
check_constraint <- function(constraint_var, num_zones) {

  testthat::context("Check constraint_var")

  testthat::test_that("Each zone code is unique", {
    testthat::expect_equal(
      constraint_var[["code"]],
      unique(constraint_var[["code"]]))
  })
  testthat::test_that("Number of zones is correct", {
    testthat::expect_equal(nrow(constraint_var), num_zones)
  })
  testthat::test_that("All columns numeric", {
    testthat::expect_true(
      all(apply(constraint_var[, 2:ncol(constraint_var)], 2, is.numeric)))
  })
}
