context("Test simulate()")

cons <- data.frame(

  "zone"   = letters[1:3],
  "a0_49"  = c(8, 2, 7),
  "a_gt50" = c(4, 8, 4),
  "f"      = c(6, 6, 8),
  "m"      = c(6, 4, 3)

)

inds <- data.frame(

  "id"     = LETTERS[1:5],
  "age"    = c("a_gt50", "a_gt50", "a0_49", "a_gt50", "a0_49"),
  "sex"    = c("m", "m", "m", "f", "f"),
  "income" = c(2868, 2474, 2231, 3152, 2473),
  stringsAsFactors = FALSE

)

vars <- c("age", "sex")

weights     <- weight(cons = cons, inds = inds, vars = vars)
weights_int <- integerise(weights)
sim_df      <- simulate(weights_int, inds = inds)


test_that("nrow sim_df == census population", {
  expect_equal(nrow(sim_df), sum(weights))
})
test_that("correct number of zones in sim_df", {
  expect_equal(length(unique(sim_df[["zone"]])), nrow(cons))
})
