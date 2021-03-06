context("Test integerise function")

cons <- readr::read_csv("../cakemap_cons.csv")
inds <- readr::read_csv("../cakemap_inds.csv")
vars <- c("Car", "NSSEC8", "ageband4")

# Check zeros are handled correctly by making one observation 0
cons[19, "n_1_1"] <- 0  # lowest count (35)
cons[19, "n_1_2"] <- 283  # add these so populations still match

weights <- rk_weight(cons = cons, inds = inds, vars = vars)

test_that("Error if num of observations don't match", {
  inds_10 <- inds[1:10, ]
  expect_error(
    rk_integerise(weights, inds_10),
    "Number of observations in weights does not match inds"
  )
})

test_that("Error if inds isn't a data frame", {
  inds <- unlist(inds)
  expect_error(rk_integerise(weights, inds), "inds is not a data frame")
})

weights_int <- rk_integerise(weights, inds)

test_that("Num of cols in weights_int should be num cols of inds + 1", {
  expect_equal(ncol(weights_int), (ncol(inds) + 1))
})

test_that("Num zones in weights_int matches num zones in cons", {
  expect_equal(nrow(unique(weights_int[, "zone"])), nrow(cons))
})

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

test_that("No missing values in integerised output", {
  expect_false(any(is.na(weights_int)))
})

test_that("method other than 'trs' fails", {
  expect_error(
    rk_integerise(weights, inds, method = "not_trs"),
    "only supports the truncate, replicate"
  )
})

test_that("Return integer weights unmodified", {
  weights <- floor(weights)
  expect_message(rk_integerise(weights, inds), "weights already integers")
})
