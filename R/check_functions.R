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

  context("Check constraint_var")

  test_that("Each zone code is unique", {
    expect_equal(constraint_var[["code"]], unique(constraint_var[["code"]]))
  })
  test_that("Number of zones is correct", {
    expect_equal(nrow(constraint_var), num_zones)
  })
  test_that("All columns numeric", {
    expect_true(all(apply(census_age[, 2:ncol(census_age)], 2, is.numeric)))
  })
}
