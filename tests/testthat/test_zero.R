context("Test zeros (0) are handled correctly")

cons <- readr::read_csv("../cakemap_cons.csv")
inds <- readr::read_csv("../cakemap_inds.csv")
vars <- c("Car", "NSSEC8", "ageband4")

cons[19, "n_1_1"] <- 0  # lowest count (35)
cons[19, "n_1_2"] <- 283  # add these so populations still match
stopifnot(any(cons == 0))

weights <- weight(cons = cons, inds = inds, vars = vars)

test_that("Populations match (i.e. sum weights == (sum cons / n vars))", {
  expect_equal(sum(weights), (sum(cons[, -1]) / length(vars)))
  # Drop first column because it contains zone numbers
})

test_that("weights matrix is numeric", {
  lapply(weights, function(x) {
    expect_is(x, "numeric")
  })
})

test_that("No missing values", {
  expect_false(any(is.na(weights)))
})
