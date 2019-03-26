library("testthat")
context("Check rk_extract() function")

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

test_that("Error if attempt to extract() numeric variable", {
  expect_error(rk_extract(weights, inds, "id"), "numeric")
})

inds[["income"]] <- cut(
  inds[["income"]],
  breaks = 2,
  labels = c("low", "high"),
  include.lowest = TRUE
)
extd_weights <- rk_extract(weights, inds, id = "id")

test_that("Number of zones is correct", {
  expect_equal(nrow(extd_weights), length(cons$zone))
})

test_that("Zone codes of extd_weights match cons zone codes", {
  expect_equal(extd_weights$code, cons$zone)
})

test_that("Simulated population total matches real population total", {
  expect_equal(sum(cons[, 2:3]), sum(extd_weights$total))
})

test_that("Simulated population matches simulated population", {
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
