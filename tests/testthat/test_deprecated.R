context("Check extract() function")

cons <- readr::read_csv("../cakemap_cons.csv")
inds <- readr::read_csv("../cakemap_inds.csv")
vars <- c("Car", "NSSEC8", "ageband4")

# Check zeros are handled correctly by making one observation 0
cons[19, "n_1_1"] <- 0  # lowest count (35)
cons[19, "n_1_2"] <- 283  # add these so populations still match

weights <- weight(cons = cons, inds = inds, vars = vars, iterations = 10)

stopifnot(
  any(cons == 0),
  any(weights == 0)
)

test_that("extract() errors with a numeric variable", {
  inds$income <- sample(2000:4000, size = nrow(inds), replace = TRUE)
  expect_error(
    extract(weights = weights, inds = inds, id = "code"),
               regexp = "cannot work with numeric")
})

test_that("extract() should work when income is binned", {
  inds$income <- sample(2000:4000, size = nrow(inds), replace = TRUE)
  inds[["income"]] <- cut(inds[["income"]],
                          breaks = 2,
                          labels = c("low", "high"),
                          include.lowest = TRUE)
  # expect_error as NA is effectively pass with no error
  expect_error(extract(weights = weights, inds = inds, id = "code"), NA)
})

extd_weights <- extract(weights = weights, inds = inds, id = "code")
test_that("Number of zones is correct", {
  expect_equal(length(extd_weights$code), length(cons$code))
})

test_that("Zone codes of extd_weights match cons zone codes", {
  expect_equal(extd_weights$code, cons$code)
})

test_that("Simulated population total matches real population total", {
  expect_equal(sum(cons[, 2:3]), sum(extd_weights$total))
})

test_that(
  "Simulated extd_weights population matches simulated weights population", {
    expect_equal(sum(extd_weights$total), sum(weights))
  })

test_that("extracted weights are numeric (except zone code)", {
  lapply(extd_weights[, 2:ncol(extd_weights)], function(x) {
    expect_is(x, "numeric")
  })
})

test_that("No missing values", {
  expect_false(any(is.na(extd_weights)))
})


context("Test integerise function")

cons <- readr::read_csv("../cakemap_cons.csv")
inds <- readr::read_csv("../cakemap_inds.csv")
vars <- c("Car", "NSSEC8", "ageband4")

# Check zeros are handled correctly by making one observation 0
cons[19, "n_1_1"] <- 0  # lowest count (35)
cons[19, "n_1_2"] <- 283  # add these so populations still match

weights <- weight(cons = cons, inds = inds, vars = vars)

test_that("Error if num of observations don't match", {
  inds_10 <- inds[1:10, ]
  expect_error(
    integerise(weights, inds_10),
    "Number of observations in weights does not match inds"
  )
})

test_that("Error if inds isn't a data frame", {
  inds <- unlist(inds)
  expect_error(integerise(weights, inds), "inds is not a data frame")
})

weights_int <- integerise(weights, inds)

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
    integerise(weights, inds, method = "not_trs"),
    "only supports the truncate, replicate"
  )
})

test_that("Return integer weights unmodified", {
  weights <- floor(weights)
  expect_message(integerise(weights, inds), "weights already integers")
})


context("Test rake() function produces correct output for extract")

cons <- readr::read_csv("../cakemap_cons.csv")
inds <- readr::read_csv("../cakemap_inds.csv")
vars <- c("Car", "NSSEC8", "ageband4")

test_that("Check fraction", {
  frac_weights <- rake(cons = cons, inds = inds, vars = vars,
                          output = "fraction", id = "code")

  lapply(frac_weights[, 3:ncol(frac_weights)], function(x) {
    expect_is(x, "numeric")
  })
})


context("Test rake() function produces correct output for integer")

test_that("Check integer", {
  int_weights <- rake(cons = cons, inds = inds, vars = vars,
                         output = "integer",
                         seed = 42, method = "trs")

  expect_equal(nrow(int_weights), sum(cons[, c("car_no", "car_yes")]))
})


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
  expect_error(weight(cons, inds, vars), "NA")

  cons[1, 2] <- NA
  expect_error(weight(cons, inds, vars), "NA")
})

test_that("Duplicated zone codes produces an error", {
  cons[2, 1] <- cons[1, 1]
  expect_error(weight(cons, inds, vars), "zone codes")
})

test_that("Duplicated ID codes produces an error", {
  inds[2, 1] <- inds[1, 1]
  expect_error(weight(cons, inds, vars), "individual IDs")
})

test_that("Error if column names (ind/cons) don't match", {
  inds$Car[inds$Car == "car_no"] <- "car_maybe"
  expect_error(weight(cons, inds, vars), "Column names don't match")
})

test_that("Error if any zone completely empty", {
  cons[1, 2:ncol(cons)] <- 0
  expect_error(
    weight(cons, inds, vars),
    "0 population")
})


context("extract_weights()")
test_that("extract_weights() errors", {
  expect_error(extract_weights(), "extract_weights() is deprecated.")
})
