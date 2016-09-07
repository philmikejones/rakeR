context("Check SimpleWorld simulation")

con_age <- data.frame(

  "zone"  = 1:3,
  "a0_49" = c(8, 2, 7),
  "a_gt50" = c(4, 8, 4)

)

con_sex <- data.frame(

  "zone" = 1:3,
  "m"    = c(6, 4, 3),
  "f"    = c(6, 6, 8)

)

ind <- data.frame(

  "id"     = 1:5,
  "age"    = c("a_gt50", "a_gt50", "a0_49", "a_gt50", "a0_49"),
  "sex"    = c("m", "m", "m", "f", "f"),
  "income" = c(2868, 2474, 2231, 3152, 2473),
  stringsAsFactors = FALSE

)

# cons variables MUST be supplied in alphabetical order to the function
# Currently the function DOES NOT check the order of variables so it's
# easiest to prepare the constraints up front
# If necessary add a prefix (e.g. 'a_') if you need to group variables
con_sex <- con_sex[, c(1, 3, 2)]


# Create a cons object will all con_ vars
cons <- merge(con_age, con_sex, by = "zone")

vars <- c("age", "sex")

weights <- rake(cons = cons, ind = ind, vars = vars)

test_that("Ncols should equal number of zones in cons", {
  expect_equal(ncol(weights), nrow(cons))
})
test_that("Nrows should equal number of individuals in ind", {
  expect_equal(nrow(weights), nrow(ind))
})
test_that("Populations match (i.e. sum weights == (sum cons / n vars))", {
  expect_equal(sum(weights), (sum(cons[, -1]) / length(vars)))
  # Drop first column because it contains zone numbers
})
