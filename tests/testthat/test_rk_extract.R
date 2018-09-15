context("Check rk_extract() function")

cons <- readr::read_csv("../cakemap_cons.csv")
inds <- readr::read_csv("../cakemap_inds.csv")
vars <- c("Car", "NSSEC8", "ageband4")

# Check zeros are handled correctly by making one observation 0
cons[19, "n_1_1"] <- 0  # lowest count (35)
cons[19, "n_1_2"] <- 283  # add these so populations still match

weights <- rk_weight(cons = cons, inds = inds, vars = vars, iterations = 10)

stopifnot(
  any(cons == 0),
  any(weights == 0)
)

test_that("rk_extract() errors with a numeric variable", {
  inds$income <- sample(2000:4000, size = nrow(inds), replace = TRUE)
  expect_error(
    rk_extract(weights = weights, inds = inds, id = "code"),
               regexp = "cannot work with numeric")
})

test_that("rk_extract() should work when income is binned", {
  inds$income <- sample(2000:4000, size = nrow(inds), replace = TRUE)
  inds[["income"]] <- cut(inds[["income"]],
                          breaks = 2,
                          labels = c("low", "high"),
                          include.lowest = TRUE)
  # expect_error as NA is effectively pass with no error
  expect_error(rk_extract(weights = weights, inds = inds, id = "code"), NA)
})

extd_weights <- rk_extract(weights = weights, inds = inds, id = "code")
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
