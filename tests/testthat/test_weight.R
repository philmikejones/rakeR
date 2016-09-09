context("Check SimpleWorld simulation")

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
