context("Test weight function")

cons <- data.frame(
  "zone"  = letters[1:3],
  "a0_49" = c(8, 2, 7),
  "a_gt50" = c(4, 8, 4),
  "f"    = c(6, 6, 8),
  "m"    = c(6, 4, 3)
)

inds <- data.frame(
  "id"         = LETTERS[1:5],
  "age"        = c("a_gt50", "a_gt50", "a0_49", "a_gt50", "a0_49"),
  "sex"        = c("m", "m", "m", "f", "f"),
  "income"     = c(868, 2474, 2231, 3152, 473),
  stringsAsFactors = FALSE
)

vars <- c("age", "sex")
weights <- weight(cons = cons, inds = inds, vars = vars, iterations = 10)

test_that("extract() errors with a numeric variable", {
  expect_error(extract(weights = weights, inds = inds, id = "id"))
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
