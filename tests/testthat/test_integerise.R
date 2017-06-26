context("Test integerise function")

cons <- readr::read_csv("../cakemap_cons.csv")
inds <- readr::read_csv("../cakemap_inds.csv")
vars <- c("Car", "NSSEC8", "ageband4")

weights <- weight(cons = cons, inds = inds, vars = vars, iterations = 10)
weights_int <- integerise(weights)

test_that("integerised weights should add up to cons population", {
  expect_equal(sum(weights_int), sum(weights))
  expect_equal(sum(weights_int), (sum(cons[, -1] / length(vars))))
})

test_that("integerised weights should have one column per zone", {
  expect_equal(ncol(weights_int), nrow(cons))
})

test_that("integerised weights are integers", {
  expect_true(all(apply(weights_int, 2, is.integer)))
})
