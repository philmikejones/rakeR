context("Test weight function")

cons <- data.frame(
  "zone"  = letters[1:3],
  "a0_49" = c(8, 2, 7),
  "a_gt50" = c(4, 8, 4),
  "f"    = c(6, 6, 8),
  "m"    = c(6, 4, 3)
)

inds <- data.frame(
  "id"     = LETTERS[1:5],
  "age"    = c("a_gt50", "a_gt50", "a0_49", "a_gt50", "a0_49"),
  "sex"    = c("m", "m", "m", "f", "f"),
  "income" = c(2868, 2474, 2231, 3152, 2473),
  stringsAsFactors = FALSE
)

vars <- c("age", "sex")

weights <- weight(cons = cons, inds = inds, vars = vars)

test_that("Ncols should equal number of zones in cons", {
  expect_equal(ncol(weights), nrow(cons))
})
test_that("Nrows should equal number of individuals in survey", {
  expect_equal(nrow(weights), nrow(inds))
})
test_that("Populations match (i.e. sum weights == (sum cons / n vars))", {
  expect_equal(sum(weights), (sum(cons[, -1]) / length(vars)))
  # Drop first column because it contains zone numbers
})
test_that("individual IDs stored in dimnames", {
  expect_equal(rownames(weights), inds[, 1])
})

# Compare my calculated weights with those given by Robin in his book (p. 26)
# First, convert weights to matrix and drop attributes
# Create book_wt matrix
weights <- round(weights, digits = 2)
weights <- as.vector(as.matrix(weights))

# I've rounded these by hand because round() goes to the nearest even integer
# see man page
book_wt <- as.vector(matrix(
  c(1.23, 1.73, 0.73,
    1.23, 1.73, 0.73,
    3.54, 0.55, 1.55,
    1.54, 4.55, 2.55,
    4.46, 1.45, 5.45),
  nrow = 5, ncol = 3, byrow = TRUE))

test_that("Weights match weights given in Robin's book (p. 26)", {
  expect_equal(weights, book_wt)
})


# CakeMap example
load("cakemap_book_weights.RData")
load("cakemap.RData")
vars <- c("Car", "NSSEC8", "ageband4")

weights <- weight(cons, inds, vars)

test_that("weight() gives similar results to Robin's weights", {
  testthat::expect_lt(
    sum(as.vector(cakemap_book_weights)) - sum(as.vector(weights)),
    sum(as.vector(cakemap_book_weights)) / 10000
  )
})
