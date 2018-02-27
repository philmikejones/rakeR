context("Test integerise function")

cons <- readr::read_csv("../cakemap_cons.csv")
inds <- readr::read_csv("../cakemap_inds.csv")
vars <- c("Car", "NSSEC8", "ageband4")

weights <- weight(cons = cons, inds = inds, vars = vars)

test_that("Error if num of observations don't match", {
  inds_10 <- inds[1:10, ]
  expect_error(
    integerise(weights, inds_10),
    "Number of observations in weights does not match inds"
  )
})

weights_int <- integerise(weights, inds)

test_that("Sum of input weights should equal number of integerised cases", {
  expect_equal(sum(weights), nrow(weights_int))
})

test_that(
  "Number of zones in integerised data set should match that in constraints", {
    expect_equal(length(unique(weights_int[["zone"]])), nrow(cons))
  })

test_that("integerised weights should add up to cons population", {
  expect_equal(nrow(weights_int), (sum(cons[, -1] / length(vars))))
})
