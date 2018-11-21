context("Check deprecated functions")

test_that("extract_weights() errors", {
  expect_error(extract_weights(), regexp = "extract_weights()")
})


# SimpleWorld: input for weight()
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

test_that("weight() returns deprecated", {
  expect_warning(weight(cons, inds, vars), regexp = "rk_weight")
})


# Input for extract()
weights <- rk_weight(cons, inds, vars)
inds$income <- cut(
  inds$income, breaks = 2, include.lowest = TRUE, labels = c("low", "high")
)

test_that("extract() returns deprecated", {
  expect_warning(extract(weights, inds, id = "id"), regexp = "rk_extract")
})


test_that("integerise() returns deprecated", {
  expect_warning(integerise(weights, inds), regexp = "^'integerise")
})

test_that("rake() errors", {
  expect_error(
    rake(cons, inds[, c("id", "age", "sex")], vars = vars, id = "id")
  )
})
