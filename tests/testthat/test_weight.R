context("Test weight() function produces correct output")

cons <- readr::read_csv("../cakemap_cons.csv")
inds <- readr::read_csv("../cakemap_inds.csv")
vars <- c("Car", "NSSEC8", "ageband4")

# Make one observation 0 to ensure this is being handled correctly
cons[19, "n_1_1"] <- 0  # lowest count (35)
cons[19, "n_1_2"] <- 283  # add these so populations still match
stopifnot(any(cons == 0))

weights <- weight(cons = cons, inds = inds, vars = vars)

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
  expect_error(weight(cons_notdf, inds, vars), "cons is not a data frame")
  expect_error(weight(cons, inds_notdf, vars), "inds is not a data frame")
})
