context("Test rk_weight()")


cons <- data.frame(
 "zone"      = letters[1:3],
 "age_0_49"  = c(8, 2, 7),
 "age_gt_50" = c(4, 8, 4),
 "sex_f"     = c(6, 6, 8),
 "sex_m"     = c(6, 4, 3),
 stringsAsFactors = FALSE
)
inds <- data.frame(
 "id"     = LETTERS[1:5],
 "age"    = c("age_gt_50", "age_gt_50", "age_0_49", "age_gt_50", "age_0_49"),
 "sex"    = c("sex_m", "sex_m", "sex_m", "sex_f", "sex_f"),
 "income" = c(2868, 2474, 2231, 3152, 2473),
 stringsAsFactors = FALSE
)
vars <- c("age", "sex")
weights <- rk_weight(cons, inds, vars)


# Test inputs
test_that("Error if cons is not a data frame", {
  cons_notdf <- unlist(cons)
  expect_error(rk_weight(cons_notdf, inds, vars), "cons is not a data frame")
})

test_that("Error if inds is not a data frame", {
  inds_notdf <- unlist(inds)
  expect_error(rk_weight(cons, inds_notdf, vars), "inds is not a data frame")
})

test_that("Error if vars is not a vector", {
  vars <- function(x) {}
  expect_error(rk_weight(cons, inds, vars = vars), "vars is not a vector")
})

test_that("Error if NAs present in cons", {
  cons[1, 2] <- NA
  expect_error(rk_weight(cons, inds, vars), "NA")
})

test_that("Error if NAs present in inds", {
  inds[1, 2] <- NA
  expect_error(rk_weight(cons, inds, vars), "NA")
})

test_that("Error if duplicate cons zone codes", {
  cons[2, 1] <- cons[1, 1]
  expect_error(rk_weight(cons, inds, vars), "duplicate zone codes")
})

test_that("Error if duplicate inds IDs", {
  inds[2, 1] <- inds[1, 1]
  expect_error(rk_weight(cons, inds, vars), "duplicate ids")
})

test_that("Error if any cons zones are 0", {
  cons[1, 2:ncol(cons)] <- 0L
  expect_error(rk_weight(cons, inds, vars), "contain 0")
})

test_that("Error if column names (ind/cons) don't match", {
  inds$age[inds$age == "age_0_49"] <- "not_age_0_49"
  expect_error(rk_weight(cons, inds, vars), "do not match")
})


# Test outputs
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
})

test_that("individual IDs stored in rownames of weights", {
  expect_equal(rownames(weights), inds[[1]])
})
