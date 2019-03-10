context("Test rk_weight() function produces correct output")

cons <- readr::read_csv("../cakemap_cons.csv")
inds <- readr::read_csv("../cakemap_inds.csv")
vars <- c("Car", "NSSEC8", "ageband4")

weights <- rk_weight(cons = cons, inds = inds, vars = vars)

test_that("Error if vars is not a vector", {
  vars <- function(x) {}
  expect_error(rk_weight(cons, inds, vars = vars), "vector")
})

test_that("Ncols should equal number of zones in cons", {
  expect_equal(ncol(weights), nrow(cons))
})

test_that("Nrows should equal number of individuals in survey", {
  expect_equal(nrow(weights), nrow(inds))
})

test_that("weights matrix is numeric", {
  lapply(weights, function(x) {
    expect_is(x, "numeric")
  })
})

test_that("No missing values", {
  expect_false(any(is.na(weights)))
})

test_that("Populations match (i.e. sum weights == (sum cons / n vars))", {
  expect_equal(sum(weights), (sum(cons[, -1]) / length(vars)))
  # Drop first column because it contains zone numbers
})

test_that("individual IDs stored in rownames of weights", {
  expect_equal(rownames(weights), inds[[1]])
})

test_that("Check for data frame errors correctly", {
  cons_notdf <- unlist(cons)
  inds_notdf <- unlist(inds)
  expect_error(rk_weight(cons_notdf, inds, vars), "cons is not a data frame")
  expect_error(rk_weight(cons, inds_notdf, vars), "inds is not a data frame")
})

test_that("Check duplicate cons or inds IDs are picked up", {
  expect_false(any(duplicated(cons[, 1])))
  if (nrow(cons > 1)) {
    cons[1, 1] <- cons[2, 1]  # duplicate zone id
    expect_true(any(duplicated(cons[, 1])))
  }

})

test_that("Errors if NAs present", {
  # test inds first: if cons was first it would never get to inds
  inds[1, 2] <- NA
  expect_error(rk_weight(cons, inds, vars), "NA")

  cons[1, 2] <- NA
  expect_error(rk_weight(cons, inds, vars), "NA")
})

test_that("Duplicated zone codes produces an error", {
  cons[2, 1] <- cons[1, 1]
  expect_error(rk_weight(cons, inds, vars), "zone codes")
})

test_that("Duplicated ID codes produces an error", {
  inds[2, 1] <- inds[1, 1]
  expect_error(rk_weight(cons, inds, vars), "individual IDs")
})

test_that("Error if column names (ind/cons) don't match", {
  inds$Car[inds$Car == "car_no"] <- "car_maybe"
  expect_error(rk_weight(cons, inds, vars), "do not match")
})

test_that("Error if any zone completely empty", {
  # Make one observation 0 to ensure this is being handled correctly
  cons[19, "n_1_1"] <- 0  # lowest count (35)
  cons[19, "n_1_2"] <- 283  # add these so populations still match
  stopifnot(any(cons == 0))

  cons[1, 2:ncol(cons)] <- 0
  expect_error(
    rk_weight(cons, inds, vars),
    "0 population")
})
