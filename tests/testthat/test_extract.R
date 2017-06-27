context("Test extract() function")

cons <- readr::read_csv("../cakemap_cons.csv")
inds <- readr::read_csv("../cakemap_inds.csv")
vars <- c("Car", "NSSEC8", "ageband4")

inds$income <- sample(2000:4000, size = nrow(inds), replace = TRUE)
weights <- weight(cons = cons, inds = inds, vars = vars, iterations = 10)

test_that("extract() errors with a numeric variable", {
  expect_error(rakeR::extract(weights = weights, inds = inds, id = "code"),
               regexp = "cannot work with numeric")
})

inds[["income"]] <- cut(inds[["income"]],
                        breaks = 2,
                        labels = c("low", "high"),
                        include.lowest = TRUE)

extd_weights <- extract_weights(weights, inds, "id")

test_that("Number of zones is correct", {
  expect_equal(length(extd_weights$code), length(cons$zone))
})

test_that("Simulated population total matched real population total", {
  expect_equal(sum(cons[, 2:3]), sum(extd_weights$total))
})

test_that("Simulated income weights match zone total", {
  expect_equal(rowSums(extd_weights[, c("f", "m")]), extd_weights$total)
})
